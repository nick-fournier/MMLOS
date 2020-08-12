#' Determine segment running speed for automobiles
#' 
#' @param link Data.table of link data.
#' @param control String containing the boundary intersection control type
#' (Signalized", "AWSC - Stop", "TWSC - Stop", "Uncontrolled", "Yield")
#' @return Numeric value in mi/hr.
#' @examples
#' auto.S_R(link, control)
#' @export
auto.S_R <- function(link, control) {
  
  #Signalization delay factor
  f_x = switch(control,
               "Signalized" = 1,
               "AWSC - Stop" = 1,
               "TWSC - Stop" = 1, 
               "Uncontrolled" = 0, 
               "Yield" = min(v_th/c_th,1))
  
  #Proximity adjustment factor
  f_v = 2 / (1 + (1 - link$v_m / (52.8*link$N_th*link$S_f))^0.21)
  
  #Startup lost time
  l1 = switch(control,
              "Signalized" = 2, 
              "AWSC - Stop" = 2.5,
              "TWSC - Stop" = 2.5, 
              "Uncontrolled" = 0,
              "Yield" = 2.5)
  
  
  #Hardcoded table of vehicle turn delay for each approach, including the boundary
  turndelay <- data.table( v_m = c(0,200,300,400,500,600,700),
                           "1" = c(0.00,0.04,0.08,0.12,0.18,0.27,0.39),
                           "2" = c(0.00,0.04,0.08,0.15,0.25,0.41,0.72),
                           "3" = c(0.00,0.05,0.09,0.15,0.15,0.15,0.15))
  #Melt into long for convenience.
  turndelay <- melt(turndelay, id.vars = "v_m")
  
  #Interpolate turn delay
  d_ap = (1+link$N_aps)*turndelay[variable==link$N_th, approx(x = v_m, y = value, xout = link$v_m, rule = 2)$y]
  
  #Other delay
  d_other = 0
  
  #Motorized vehicle midsegment running time
  t_R = f_x * ((6.0 - l1) / (0.0025*link$LL)) + ((3600*link$LL)/(5280*link$S_f)) * f_v + d_ap + d_other
  
  #Link running speed
  S_R = (3600*link$LL) / (5280*t_R)
  
  return(S_R)
}


#' Determine the adjusted saturation flow rate
#' 
#' @param link Data.table of link data.
#' @param control String containing the boundary intersection control type
#' (Signalized", "AWSC - Stop", "TWSC - Stop", "Uncontrolled", "Yield")
#' @return Numeric value in mi/hr.
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
  f_p = ((link$N_th - 0.1 - (18*N_m/3600))/link$N_th)
  f_p = ifelse(f_p < 0.05, 0.05, f_p)
  #bus blocking
  f_bb = (link$N_th - (14.4*link$n_bus/3600))/link$N_th
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
#' @param control String containing the boundary intersection control type
#' (Signalized", "AWSC - Stop", "TWSC - Stop", "Uncontrolled", "Yield")
#' @return Numeric value in mi/hr.
#' @examples
#' auto.S_R(link, control)
#' @export
auto.VCratio <- function(link, int) {
  #Adjusted saturation flow rate
  s = auto.satflow(link, int)
  
  #Lane group capacity
  c = link$N_th*s*int[traf_dir == link$link_dir, g/C]
  
  #V/C Ratio
  X = int[traf_dir == link$link_dir, v_v] / c
  
  return(X)
}




