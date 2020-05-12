






#Segment running time
func.t_R <- function(dat) {
  
  
  tab_dap = fread("./data/turning_vehicle_delay.csv")
  
  
  #Split by intersection
  int.splt <- split(dat$INT, by = "int_id")  
  
  
  f_x = switch(control, "Signalized" = 1, "Stop" = 1, "Uncontrolled" = 0, "Yield" = min(v_th/c_th,1))
  l1 = switch(control, "Signalized" = 2, "Stop" = 2.5, "Uncontrolled" = 0, "Yield" = 2.5)
  
  
  N_th
  
  tab_dap[v_m = v_m]
  
  
  t_R = f_x * ((6.0 - l1) / (0.0025*L)) + ((3600*L)/(5280*S_f)) * f_v + sum(d_api) + d_other
  
  return(t_R)
}


