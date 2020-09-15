

#' Bicycle paved width factor (links)
#' 
#' @param link Data.table of link data.
#' @param int  Data.table of subject intersection data.
#' @return Numeric value, unitless.
#' @examples
#' ogbike.F_w.link(link)
#' @export
ogbike.F_w.link <- function(link, int) {

  #Midsegment flow per lane in direction of travel
  v_vm = int[ traf_dir == link$link_dir, sum(v_rt + v_lt + v_th)] / link$N_mth
  
  #Adjusted width of outside shoulder, if curb present
  if(link$curb) {
    W_osstar = link$W_os - 1.5
    W_osstar = ifelse(W_osstar >= 0, W_osstar, 0)
  } else {
    W_osstar = link$W_os
  }
  
  #Total width of outside thru lane, bike lane, and paved shoulder/parking
  if(link$p_pk == 0) {
    W_t = link[ , W_ol + W_bl] + W_osstar
  } else {
    W_t = link[ , W_ol + W_bl]
  }
  
  #Effective total width of outside through lane
  if(v_vm > 160 | link$div > 0) {
    W_v = W_t
  } else {
    W_v = W_t*(2 - 0.005*v_vm)
  }
  
  #Effective width of outside through lane
  if(link$W_bl + W_osstar < 4) {
    W_e = W_v - 10*link$p_pk
    W_e = ifelse(W_e >= 0, W_e, 0)
  } else {
    W_e = W_v + link$W_bl + W_osstar - 20*link$p_pk
    W_e = ifelse(W_e >= 0, W_e, 0)
  }
  
  F_w = -0.005*W_e^2
  
  return(F_w)
}


#' Bicycle traffic speed factor (links)
#' 
#' @param link Data.table of subject link data.
#' @param int  Data.table of subject intersection data.
#' @param dat Data.table of entire data set.
#' @return Numeric value, unitless.
#' @examples
#' ogbike.F_s.link(link)
#' @export
ogbike.F_s.link <- function(link, int, dat) {
  
  #Vehicle running speed
  S_R = auto.S_R(link, int, dat)
  
  #Adjusted motorized vehicle link running speed
  S_Ra = ifelse(S_R < 21, 21, S_R)
  
  #Midsegment flow per lane in direction of travel
  v_vm = int[ traf_dir == link$link_dir, sum(v_rt + v_lt + v_th)] / link$N_mth
  
  #Adjusted heavy vehicle percent
  P_HVa = ifelse(v_vm*(1 - 0.01*link$P_HV) < 200 & link$P_HV > 0.5, 
                 0.5, 
                 link$P_HV)
  
  #Motorized vehicle speed adjustment factor
  F_s = 0.199*(1.1199*log(S_Ra - 20) + 0.8103)*(1 + 0.1038*P_HVa)^2
  
  return(F_s)
}


#' Bicycle level of service score for links
#' 
#' @param link Data.table of link data.
#' @param control String containing the boundary intersection control type (Signalized", "AWSC - Stop", "TWSC - Stop", "Uncontrolled", "Yield")
#' @return A numeric LOS score
#' @examples
#' ogbike.I_link(link, control)
#' @export
ogbike.I_link <- function(link, int, dat) {
  
  #Cross-section adjustment factor
  F_w = ogbike.F_w.link(link, int)
  
  #Midsegment flow per lane in direction of travel
  v_vm = int[ traf_dir == link$link_dir, sum(v_rt + v_lt + v_th)] / link$N_mth
  
  #Motorized vehicle volume adjustment factor
  v_ma = ifelse(v_vm > 4*link$N_th, v_vm, 4*link$N_th)
  
  F_v = 0.507*log(v_ma / (4*link$N_th))
  
  #Motorized vehicle speed adjustment factor
  F_s = ogbike.F_s.link(link, int, dat)
  
  #Pavement condition factor
  F_p = 7.066 / link$P_c^2
  
  #### LOS Score
  I_link = 0.760 + F_w + F_v + F_s + F_p
  
  return(I_link)
}


#' Bicycle LOS score for intersections
#' 
#' @param link Data.table of subject link data.
#' @param int  Data.table of subject intersection data.
#' @param dat Data.table of entire data set.
#' @return A numeric LOS score
#' @examples
#' ogbike.I_int(int, dir)
#' @export
ogbike.I_int <- function(link, int) {
  
  #Traffic direction
  dir = link$link_dir
  
  #The traffic direction being crossed
  xdir = switch(dir,
                "NB" = "WB",
                "SB" = "EB", 
                "EB" = "NB",
                "WB" = "SB")
  
  #Opposite cross street dir
  odir = switch(xdir,
                "NB" = "SB",
                "SB" = "NB",
                "EB" = "WB",
                "WB" = "EB")

  #Number of traffic lanes crossed
  N_dc = int[traf_dir == xdir, N_dc]
  N_dc = ifelse(is.na(N_dc), int[traf_dir == odir, N_dc], N_dc)
  
  #Curb to curb width of cross street
  W_cd = int[traf_dir == xdir, W_cd]
  
  W_cd = ifelse(is.na(W_cd), 0, W_cd)
  
  #Adjusted width of paved outside shoulder
  #W_osstar = int[ traf_dir == dir, ifelse(curb & W_os - 1.5 >= 0, W_os - 1.5, W_os)]
  W_osstar = link[ , ifelse(curb & W_os - 1.5 >= 0, W_os - 1.5, W_os)]
  
  #Total width of outside thru lane
  #W_t = int[ traf_dir == dir, W_ol + W_bl + ifelse(p_pk > 0, 0, 1)*W_osstar]
  W_t = link[ , W_ol + W_bl + ifelse(p_pk > 0, 0, 1)*W_osstar]
  
  #Cross-section factor
  F_w = 0.0153*W_cd - 0.2144*W_t
  
  #Veh volume factor
  F_v = int[traf_dir == dir, 0.0066*((v_lt+v_th+v_rt)/(4*N_th))]
  
  I_int = 4.1324 + F_w + F_v
  
  return(I_int)
}


#' Bicycle LOS score for segment
#' 
#' @param link Data.table of link data.
#' @param int Data.table of intersection data.
#' @return A data.table with numeric and letter grade LOS scores
#' @examples
#' ogbike.I_seg(link, int)
#' @export
ogbike.I_seg <- function(link, int, dat) {
  #Put LOS scores for link and intersection into table
  scores = data.table(
    segment_id = link$link_id,
    direction = link$link_dir,
    mode = "bicycle",
    I_link = ogbike.I_link(link, int, dat),
    I_int = ogbike.I_int(link, int)
  )
  
  #Calculate segment LOS
  LL = link$LL
  N_aps = link$N_aps
  
  F_bi = ifelse(int[traf_dir == link$link_dir, control] == "Uncontrolled", 0, 1)
  
  
  #Calculate score
  scores[ , I_seg := 0.160*I_link + 0.011*F_bi*exp(I_int) + 0.035*(N_aps/(LL/5280)) + 2.85]
  
  #Get grade from score
  scores = cbind(scores,
                 setNames(scores[ , lapply(.SD, score2LOS), .SDcols = c("I_link","I_int","I_seg"),], c("link_LOS","int_LOS","seg_LOS")))
  
  
  
  return(scores)
}


