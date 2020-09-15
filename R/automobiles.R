#' Determine the segment running time
#' 
#' @param link Data.table of link data.
#' @param int  Data.table of subject intersection data.
#' @return Numeric value (decimal).
#' @examples
#' auto.S_R(link, control)
#' @export
auto.t_R <- function(link, int, dat) {
  
  #Free flow speed
  S_f = auto.S_f(link, int, dat)
  
  #Signalization delay factor
  f_x = switch(int[traf_dir == link$link_dir, as.character(control)],
               "Signalized" = 1,
               "AWSC - Stop" = 1,
               "TWSC - Stop" = 1, 
               "Uncontrolled" = 0, 
               "Yield" = min(v_v*(1 - p_rt - p_lt)/c_th, 1))
  #Midsegment flow per lane in direction of travel
  v_vm = int[ traf_dir == link$link_dir, sum(v_rt + v_lt + v_th)]
  
  #Proximity adjustment factor
  f_v = 2 / (1 + (1 - v_vm / (52.8*link$N_mth*S_f))^0.21)
  
  #Startup lost time
  l1 = switch(int[traf_dir == link$link_dir, as.character(control)],
              "Signalized" = 2, 
              "AWSC - Stop" = 2.5,
              "TWSC - Stop" = 2.5, 
              "Uncontrolled" = 0,
              "Yield" = 2.5)
  
  
  #Hardcoded table of vehicle turn delay for each approach, including the boundary
  turndelay <- data.table( "v_m" = c(0,200,300,400,500,600,700),
                           "1" = c(0.00,0.04,0.08,0.12,0.18,0.27,0.39),
                           "2" = c(0.00,0.04,0.08,0.15,0.25,0.41,0.72),
                           "3" = c(0.00,0.05,0.09,0.15,0.15,0.15,0.15))
  #Melt into long for convenience.
  turndelay <- melt(turndelay, id.vars = "v_m")
  
  
  #Interpolate turn delay
  d_ap = (1+link$N_aps)*turndelay[variable==link$N_mth, approx(x = v_m, y = value, xout = v_vm / link$N_mth, rule = 2)$y]
  
  #Other delay
  d_other = 0
  
  #Motorized vehicle midsegment running time
  t_R = f_x * ((6.0 - l1) / (0.0025*link$LL)) + ((3600*link$LL)/(5280*S_f)) * f_v + d_ap + d_other
  
  return(t_R)
}


#' Determine segment running speed for automobiles
#' 
#' @param link Data.table of subject link data.
#' @param int  Data.table of subject intersection data.
#' @param dat Data.table of entire data set.
#' @return Numeric value in mi/hr.
#' @examples
#' auto.S_R(link, control)
#' @export
auto.S_R <- function(link, int, dat) {
  
  #Segment running time
  t_R = auto.t_R(link, int, dat)
  #Link running speed
  S_R = (3600*link$LL) / (5280*t_R)
  
  return(S_R)
}


#' Determine base free-flow speed
#' 
#' @param link Data.table of subject link data.
#' @param int  Data.table of subject intersection data.
#' @param dat Data.table of entire data set.
#' @return Numeric value in mi/hr.
#' @examples
#' auto.S_f(link, int, dat)
#' @export
#' 
auto.S_f <- function(link, int, dat) {

  #Opposite street dir
  odir = switch(link$link_dir,
                "NB" = "SB",
                "SB" = "NB",
                "EB" = "WB",
                "WB" = "EB")
  
  #Opposite direction
  olink <- dat$links[link_id == link$link_id & link_dir != link$link_dir, ]
  
  #Speed constant
  s0 = 25.6 + 0.47*link$S_lim
  
  #Cross-section factor
  f_cs = 1.5*link$div - 0.47*link$curb
  
  #Access point density
  D_a = 5280*(link$N_aps + olink$N_aps)/(link$LL - int[traf_dir==odir, W_cd])
  
  #Access point density factor
  f_A = -0.078*D_a/link$N_mth
  
  #Base free-flow speed
  S_fo = s0  + f_cs + f_A
  
  #Adjustment for signal spacing
  f_L = 1.02 - 4.7*(S_fo - 19.5)/(max(link$LL, 400))
  f_L = ifelse(f_L > 1, 1, f_L)
  
  #Free-flow speed
  S_f = S_fo*f_L

  return(S_f)  
}


#' Determine the adjusted saturation flow rate
#' 
#' @param link Data.table of subject link data.
#' @param int  Data.table of subject intersection data.
#' @param dat Data.table of entire data set.
#' @return Numeric value in veh/hr.
#' @examples
#' auto.satflow(link, int)
#' @export
auto.satflow <- function(link, int) {
  
  #Constants
  #Opposite street dir
  odir = switch(link$link_dir,
                "NB" = "SB",
                "SB" = "NB",
                "EB" = "WB",
                "WB" = "EB")
  
  s0 = 1900 #Baseline saturation flow rate (1750 outside of major metro area)
  E_T = 2 #Car equivalent of trucks
  N_m = 20 #Parking maneuver rate
  E_R = 1.18 #Equivalent car for right turns
  E_L = 1.05 #Equivalent car for left turns
  v0 = int[traf_dir == odir, v_v] #Opposiding demand flow
  
  ##Adjustment factors
  #Lane width
  if(link$W_ol < 10) {
    f_w = 0.96
  } else if(link$W_ol >= 10 & link$W_ol <= 12.9) {
    f_w = 1
  } else if(link$W_ol > 12.9) {
    f_w = 1.04
  }
  
  #Heavy vehicles
  f_HV = 100 / (100 + link$P_HV*(E_T - 1))
  #Slope grade
  f_g = 1 - (link$p_g/200)
  #Parking
  f_p = ((link$N_mth - 0.1 - (18*N_m/3600))/link$N_mth)
  f_p = ifelse(f_p < 0.05, 0.05, f_p)
  #bus blocking
  f_bb = (link$N_mth - (14.4*link$n_bus/3600))/link$N_mth
  f_bb = ifelse(f_bb < 0.05, 0.05, f_bb)
  #Area of type
  f_a = 0.9
  #Lane utilization
  f_LU = 1
  #Right turns adjustment
  f_RT = 1 / E_R
  #Left turn adjustment
  f_LT = 1 / E_L
  
  ### pedestrian adjustment factor for right turns
  #ped flow rate
  v_pedg = int[traf_dir == link$link_dir, v_p*(C/gped)]
  v_pedg = ifelse(v_pedg > 5000, 5000, v_pedg)
  
  #ped occupancy
  if(int[traf_dir == link$link_dir, v_p] <= 2000) {
    OCC_pedg = v_pedg/2000
  } else {
    OCC_pedg = 0.4 + v_pedg/10000
    OCC_pedg = ifelse(OCC_pedg > 0.9, 0.9, OCC_pedg)
  }
  
  #bicycle flow rate
  v_bicg = int[traf_dir == link$link_dir, v_b*(C/gped)]
  v_bicg = ifelse(v_bicg > 1900, 1900, v_bicg)

  #bike occupancy
  OCC_bicg = 0.02 + v_bicg/2700
  
  #conflict zone occupancy
  OCC_rR = int[traf_dir == link$link_dir, gped/g*OCC_pedg + OCC_bicg - gped/g*OCC_pedg*OCC_bicg]
  
  #Unoccupied time
  A_RpbT = 1 - OCC_rR
  
  #Saturation flow rate adjustment factor for right turns
  f_Rpb = A_RpbT
  
  #Ped occupancy after queue clears
  #Effective permitted green time
  g_p = int[traf_dir == link$link_dir, g - l]
  g_p = ifelse(g_p < 0, 0, g_p)
  
  #Effective permitted green time unblocked by opposite queue
  g_u = int[traf_dir == odir, g - l]
  
  #Opposing queue service time
  g_q = g_p - g_u 
  
  #Ped occupancy after queue clears
  if( g_p < int[traf_dir == link$link_dir, gped]) {
    OCC_pedu = int[traf_dir == link$link_dir, OCC_pedg*(1 - 0.5*g_q/gped)]
  } else {
    OCC_pedu = 0
  }
  
  #Determine relevant conflict zone occupancy
  OCC_rL = int[traf_dir == link$link_dir, ((gped - g_q) / (g_p - g_q)) * (OCC_pedu)*exp(-5*v0/3600)]
  
  #Unoccupied time
  A_LpbT = 1 - OCC_rL
  
  #Saturation flow rate adjustment factor for left turns
  f_Lpb = A_LpbT
  
  #Adjusted saturation flow rate
  s = s0*f_w*f_HV*f_g*f_p*f_bb*f_a*f_LU*f_LT*f_RT*f_Lpb*f_Rpb
  
  return(s)
}


#' Determine the volume to capacity ratio for critical intersection
#' 
#' @param link Data.table of link data.
#' @param int  Data.table of subject intersection data.
#' @return Numeric value (decimal).
#' @examples
#' auto.VCratio(link, int)
#' @export
auto.VCratio <- function(link, int) {
  #Adjusted saturation flow rate
  s = auto.satflow(link, int)
  
  #Lane group capacity
  c = link$N_mth*s*int[traf_dir == link$link_dir, g/C]
  
  #V/C Ratio
  X = int[traf_dir == link$link_dir, v_v] / c
  
  return(X)
}


#' Determine the intersection delay
#' 
#' @param link Data.table of subject link data.
#' @param int  Data.table of subject intersection data.
#' @param dat Data.table of entire data set.
#' @return Numeric value (decimal).
#' @examples
#' auto.S_R(link, control)
#' @export
auto.delay <- function(link,int) {
  
  #Volume/Capacity Ratio
  X = auto.VCratio(link, int)
  
  #Control delay
  if(int[traf_dir == link$link_dir, as.character(control)] == "Signalized") {
    #Uniform delay
    d1 = int[traf_dir == link$link_dir, (0.5*C*(1-g/C)^2) / (1-(min(1,X)*g/C))]
    ### Incremental delay
    
    
    ### Initial delay
    #Initial queue length
    Qb = 0
    
    if(v >= cA) {
      Qeo = TT*(v-cA)
      tA=TT
    } else {
      Qeo = 0
      tA = Qb/(cA-v)
      tA = ifelse(tA > TT, TT, tA)
    }
    
    Qe = Qb + t_A*(v-cA)
    
    d3 = (3600/(v*TT))*(tA*((Qb+Qe-Qeo)/2) + (Qe^2 - Qeo^2)/(2*cA) - (Qb^2)/(2*cA))
    
    ### Control delay
    d = d1 + d2 + d3
  }
  
  
  d = switch(int[traf_dir == link$link_dir, as.character(control)],
                "Signalized" = int[traf_dir == link$link_dir, (0.5*C*(1-g/C)^2) / (1-(min(1,X)*g/C))],
                "AWSC - Stop" = 1,
                "TWSC - Stop" = 1, 
                "Uncontrolled" = 0, 
                "Yield" = min(v_th/c_th,1))
}


#' Determine the segment travel speed
#' 
#' @param link Data.table of link data.
#' @param int  Data.table of subject intersection data.
#' @return Numeric value (decimal).
#' @examples
#' auto.S_R(link, control)
#' @export
auto.S_Tseg <- function(link, int) {
  
  

  
  #Through delay
  d_t = 
  
  
  #Segment running time
  t_R = auto.t_R(link,int)
  
  #Segment travel speed
  S_Tseg = 3600*link$LL / (5280 * (t_R + d_t))
  
  return(S_Tseg)
}














