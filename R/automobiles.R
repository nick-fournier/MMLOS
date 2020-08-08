#Segment running time
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


