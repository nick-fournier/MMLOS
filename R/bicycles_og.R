
#1. DETERMINE BICYCLE RUNNING SPEED
#2. DETERMINE Bicycle DELAY AT INTERSECTION
#3. DETERMINE BICYCLE TRAVEL SPEED
#4. DETERMINE BICYCLE LOS SCORE FOR INTERSECTION
#6. DETERMINE BICYCLE LOS SCORE FOR LINK
#7. DETERMINE LINK LOS
#9. DETERMINE BICYCLE LOS SCORE FOR SEGMENT

#Bicycle paved width factor (links)
ogbike.F_w.link <- function(link) {

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
  if(link$v_m > 160 | link$div) {
    W_v = W_t
  } else {
    W_v = W_t*(2 - 0.005*link$v_m)
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


#Bicycle traffic speed factor (links)
ogbike.F_s.link <- function(link, int) {
  
  #Vehicle running speed
  S_R = auto.S_R(link, control = int[traf_dir == link$link_dir, control])
  
  #Adjusted motorized vehicle link running speed
  S_Ra = ifelse(S_R < 21, 21, S_R)
  
  
  #Adjusted heavy vehicle percent
  P_HVa = ifelse(link$v_m*(1 - 0.01*link$P_HV) < 200 & link$P_HV > 0.5, 
                 0.5, 
                 link$P_HV)
  
  #Motorized vehicle speed adjustment factor
  F_s = 0.199*(1.1199*log(S_Ra - 20) + 0.8103)*(1 + 0.1038*P_HVa)^2
  
  return(F_s)
}

#Bicycle level of service score for links
ogbike.I_link <- function(link, int) {
  
  #Cross-section adjustment factor
  F_w = ogbike.F_w.link(link)
  
  #Motorized vehicle volume adjustment factor
  v_ma = ifelse(link$v_m > 4*link$N_th, link$v_m, 4*link$N_th)
  
  F_v = 0.507*log(v_ma / (4*link$N_th))
  
  #Motorized vehicle speed adjustment factor
  F_s = ogbike.F_s.link(link, int)
  
  #Pavement condition factor
  F_p = 7.066 / link$P_c^2
  
  #### LOS Score
  I_link = 0.760 + F_w + F_v + F_s + F_p
  
  return(I_link)
}

#Bicycle LOS score for intersections
ogbike.I_int <- function(int, dir) {
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
  N_d = int[traf_dir == xdir, N_d]
  
  N_d = ifelse(is.na(N_d), int[traf_dir == odir, N_d], N_d)
  
  #Curb to curb width of cross street
  W_cd = int[traf_dir == xdir, W_cd]
  
  W_cd = ifelse(is.na(W_cd), 0, W_cd)
  
  #Adjusted width of paved outside shoulder
  W_osstar = int[ traf_dir == dir, ifelse(curb & W_os - 1.5 >= 0, W_os - 1.5, W_os)]
  
  #Total width of outside thru lane
  W_t = int[ traf_dir == dir, W_ol + W_bl + ifelse(p_pk > 0, 0, 1)*W_osstar]
  
  #Cross-section factor
  F_w = 0.0153*W_cd - 0.2144*W_t
  
  #Veh volume factor
  F_v = int[traf_dir == dir, 0.0066*((v_lt+v_th+v_rt)/(4*N_th))]
  
  I_int = 4.1324 + F_w + F_v
  
  return(I_int)
}

#Bicycle LOS score for segment
ogbike.I_seg <- function(link, int) {
  #Put LOS scores for link and intersection into table
  scores = data.table(
    segment_id = link$link_id,
    direction = link$link_dir,
    mode = "bicycle",
    I_link = ogbike.I_link(link, int),
    I_int = ogbike.I_int(int, link$link_dir)
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


