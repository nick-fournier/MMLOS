
#1. DETERMINE BICYCLE RUNNING SPEED
#2. DETERMINE Bicycle DELAY AT INTERSECTION
#3. DETERMINE BICYCLE TRAVEL SPEED
#4. DETERMINE BICYCLE LOS SCORE FOR INTERSECTION
#6. DETERMINE BICYCLE LOS SCORE FOR LINK
#7. DETERMINE LINK LOS
#9. DETERMINE BICYCLE LOS SCORE FOR SEGMENT


#Bicycle paved width factor (links)
func.bike.F_w.link <- function(link) {
  #Effective buffer width
  #W_bufstar = link[ , sqrt(W_blbuf^2 + 16*H_blbuf^0.5)]
  W_bufstar = link[ , 4*(W_blbuf^2 + (24*H_blbuf))^(1/4)]
  
  #Adjusted width of outside shoulder, if curb present
  if(link$curb) {
    W_osstar = link$W_os - 1.5
    W_osstar = ifelse(W_osstar >= 0, W_osstar, 0)
  } else {
    W_osstar = link$W_os
  }
  
  #Is protected?
  if(link$protected) {
    P_pkstar = 1
  } else {
    P_pkstar = link$p_pk
  }
  
  #Total width of outside thru lane, bike lane, and paved shoulder/parking
  if(link$p_pk == 0) {
    W_t = link[ , W_ol + W_bl] + W_osstar + W_bufstar
  } else {
    W_t = link[ , W_ol + W_bl] + W_bufstar
  }
  
  #Effective total width of outside through lane
  if(link$v_m > 160 | link$div) {
    W_v = W_t
  } else {
    W_v = W_t*(1.8 - 0.005*link$v_m)
  }
  
  #Effective width of outside through lane
  if(link$W_bl + W_osstar < 4) {
    W_e = W_v - 10*P_pkstar
    W_e = ifelse(W_e >= 0, W_e, 0)
  } else {
    W_e = W_v + link$W_bl + W_osstar - 20*P_pkstar
    W_e = ifelse(W_e >= 0, W_e, 0)
  }
  
  F_w = -0.005*W_e^2
  
  return(F_w)
}

#Bicycle traffic speed factor (links)
func.bike.F_s.link <- function(link, control) {
  

  #Vehicle running speed
  S_R = func.auto.S_R(link, control)
  
  #Adjusted motorized vehicle link running speed
  S_Ra = ifelse(S_R < 21, 21, S_R)
  
  #Adjusted heavy vehicle percent
  P_HVa = ifelse(link$v_m*(1 - 0.01*link$P_HV) < 200 & link$P_HV > 50, 
                 50, 
                 link$P_HV)
  
  #Motorized vehicle speed adjustment factor
  F_s = 0.199*(1.1199*log(S_Ra - 20) + 0.8103)*(1 + 0.1038*P_HVa)^2
  
  return(F_s)
}

#Average bicycle delay for intersection
func.bike.d_bd <- function(int, dir) {
  switch(int[traf_dir == dir, as.character(control)],
         "Signalized" = func.bike.d_signal(int, dir), 
         "AWSC - Stop" = 0,
         "TWSC - Stop" = func.bike.d_twsc(int, dir), 
         "Uncontrolled" = func.bike.d_1stageleft(int,dir),
         "Yield" = 0)
}

#Bicycle control delay
func.bike.d_signal <- function(int, dir) {
  #Delay from signal and right-turning vehicle encroachment
  d_bS = func.bike.d_bS(int, dir)
  
  #Proportion of overall left turning bicycles
  P_L = int[traf_dir == dir, v_bl/v_b]
  
  #Proportion of two-stage left turns
  P_L2 = int[traf_dir == dir, P_bl2]
  
  #One-stage left turn delay
  d_bL1 = func.bike.d_1stageleft(int, dir)
  
  #Two-stage left turn delay
  d_bL2 = func.bike.d_2stageleft(int, dir)
  
  #Average total bicycle delay at intersections
  d_bd = d_bS + P_L*((1 - P_L2)*d_bL1 + P_L2*d_bL2)
  
  return(d_bd)
}

#One-stage left turn bicycle delay
func.bike.d_1stageleft <- function(int, dir, tol = 1e-8) {
  
  #Opposite cross street dir
  odir = switch(dir,
                "NB" = "SB",
                "SB" = "NB",
                "EB" = "WB",
                "WB" = "EB")
  
  #Yield rate
  M_y = int[traf_dir == dir, M_yb]
  
  #Volume of bicycles and vehicles in veh per sec
  v_b = int[traf_dir == dir, v_b/3600]
  v_v = int[traf_dir == dir, v_v/3600]
  #v_v = int[traf_dir == dir, (v_v/N_d)/3600]
  
  #Number of thru lanes (inclusive of opposite direction)
  N_th = int[traf_dir == dir, N_th]
    
  #Total thru lanes
  #int[traf_dir %in% c(dir,odir), sum(N_th)]
  
  #Intersection crossing length
  L = int[traf_dir == dir, W_cd]
  
  #Critical gap headway
  t_cb = int[traf_dir == dir, L/S_b + t_sb]
  
  #Total numbre of platooned bicycles
  N_c = (v_b*exp(v_b*t_cb) + v_v*exp(-v_v*t_cb)) / ( (v_b + v_v)*exp((v_b - v_v)*t_cb) )
  
  #Distribution of platooned bicycles
  W_bl = int[traf_dir == dir, ifelse(W_bl < 2.5, 2.5, W_bl)]
  N_b = 2.5*N_c/W_bl
  N_b = max(2.5*N_c/W_bl, 1.0)
  
  #Critical group headway
  t_cbG = t_cb + 2*(N_b - 1)
  
  #Probability of blocked lane
  P_b = 1 - exp(-t_cbG*v_v/L)
  
  #Probability of delayed crossing
  P_d = 1 - (1 - P_b)^L
  
  #Average bicycle delay for gap acceptance
  d_bg = (1/v_v)*(exp(v_v*t_cbG) - v_v*t_cbG - 1)
  
  #Average Biycle delay
  d_bgd = d_bg / P_d
  
  #Headway
  h = ( (1/v_v) - (t_cbG + (1/v_v))*exp(-v_v*t_cbG) ) / (1 - exp(-v_v*t_cbG))
  
  #Average number of crossing events
  n = round(1/exp(-v_v*t_cbG), 0)
  
  #Initial sum of PYi
  sumPY = 0
  term1 = 0
  i = 0
  prev = 0
  diff = 1
  #Delay accounting for motorist yield probability
  if(N_th == 1) {
    #One lane
    while(i < n & diff > tol) {
      PY = ifelse(i == 0, 0, P_d*M_y*(1 - M_y)^(i-1))
      sumPY = sumPY + PY
      
      #Calculates and sums up the first term
      term1 = h*(i - 0.5)*PY + term1
      
      #Calculates the second term
      term2 = (P_d - sumPY)*d_bgd
      
      #Calculate convergence value
      val = term1 + term2
      diff = abs(val - prev)
      
      #Move onto next iteration
      prev = val
      i <- i + 1
    }
    d_bL2 <- term1 + term2
    
  } else if(N_th == 2) {
    #Two lane
    while(i < n & diff > tol) {
      PY = ifelse(i == 0, 0, (P_d - sumPY)*( ((2*P_b*(1-P_b)*M_y) + P_b^2 * M_y^2) / P_d) )
      sumPY = sumPY + PY
      
      #Calculates and sums up the first term
      term1 = h*(i - 0.5)*PY + term1
      
      #Calculates the second term
      term2 = (P_d - sumPY)*d_bgd
      
      #Calculate convergence value
      val = term1 + term2
      diff = abs(val - prev)
      
      #Move onto next iteration
      prev = val
      i <- i + 1
    }
    d_bL2 <- term1 + term2
    
  } else if(N_th == 3) {
    #Three lane
    while(i < n & diff > tol) {
      PY = ifelse(i == 0, 0, (P_d - sumPY)*((P_b^3 * M_y^3 + 3*P_b^2 * (1-P_b)*M_y^2 + 3*P_b*(1-P_b)^2 * M_y) / P_d))
      sumPY = sumPY + PY
      
      #Calculates and sums up the first term
      term1 = h*(i - 0.5)*PY + term1
      
      #Calculates the second term
      term2 = (P_d - sumPY)*d_bgd
      
      #Calculate convergence value
      val = term1 + term2
      diff = abs(val - prev)
      
      #Move onto next iteration
      prev = val
      i <- i + 1
    }
    d_bL2 <- term1 + term2
    
  } else if(N_th == 4) {
    #Four lane
    while(i < n & diff > tol) {
      PY = ifelse(i == 0, 0, (P_d - sumPY)*((P_b^4 * M_y^4 + 4*P_b^3 * (1-P_b)*M_y^3 + 6*P_b^2 *(1-P_b)^2 * M_y^2 + 4*P_b*(1-P_b)^3 * M_y) / P_d))
      sumPY = sumPY + PY
      
      #Calculates and sums up the first term
      term1 = h*(i - 0.5)*PY + term1
      
      #Calculates the second term
      term2 = (P_d - sumPY)*d_bgd
      
      #Calculate convergence value
      val = term1 + term2
      diff = abs(val - prev)
      
      #Move onto next iteration
      prev = val
      i <- i + 1
    }
    d_bL2 <- term1 + term2
  } else {
    stop(paste0("Invalid number of lanes: ", N_th))
  }
  
  l = int[traf_dir == dir, l]
  C = int[traf_dir == dir, C]
  g = int[traf_dir == dir, g]
  t_sb = int[traf_dir == dir, t_sb]
  
  #Arrive on red delay
  d_R = ((C-g)/C)*( (C-g)/2) + l + t_sb
  
  #
  d_bL2 = d_bL2 + d_R
  
  return(d_bL2)
}

#Two-stage left turn bicycle delay
func.bike.d_2stageleft <- function(int, dir) {
  
  #Picking the crosswalk data from the travel direction (it is perpendicular to vehicles)
  xdir = switch(dir,
                "NB" = "WB",
                "SB" = "EB",
                "EB" = "NB",
                "WB" = "SB")
  
  #Cycle time
  C = int[traf_dir == dir, C]
  
  #Clearance time for approach direction
  l1 = int[traf_dir == dir, l]
  
  #Green time for approach direction
  g1 = int[traf_dir == dir, g]
  
  #Bike startup time
  t_sb = int[traf_dir == dir, t_sb]
  
  #Average delay for arrival on green
  d_bL2G = (g1/2) + l1 + t_sb
  
  #Average delay for arrival on red
  d_bL2R = ((C - g1)/2) + g1 + l1 + 2*t_sb
  
  #Average total delay
  d_bL2 = (g1/C)*d_bL2G + ((C-g1)/C)*d_bL2R
  
  return(d_bL2)
  
}

#Bicycle delay from signal and right turning vehicles
func.bike.d_bS <- function(int, dir) {
  
  #Critical gap time
  t_c = 5
  
  #Saturation flow rate
  s_b = max(1500*floor(int[traf_dir == dir, W_bl]/2.5), 1500)
  
  #Right turning vehicle volume per second
  v_RTV = int[traf_dir == dir, v_rt]/3600
  
  #Right turning vehicle capacity reduction factor
  f_RTV = exp(-v_RTV*t_c)
  
  #Effective bike lane capacity
  c_be = s_b*f_RTV*int[traf_dir == dir, g/C]
  
  #HCM bike lane capacity
  c_b = int[traf_dir == dir, 2000*g/C]
  
  #Calculate delay from signal, including right-turn vehicle enroachment
  d_bS = int[traf_dir == dir, (0.5*C*(1-(g/C))^2) / (1 - min(v_b/c_be, 1)*(g/C)) ]

  
  return(d_bS)
}

#Bicycle delay for uncontrolled (e.g., TWSC)
func.bike.d_twsc <- function(int, dir) {
  
  #Picking the crosswalk data from the travel direction (it is perpendicular to vehicles)
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
  
  #Yield rate
  M_y = int[traf_dir == xdir, M_yb]
  
  #Number of thru lanes (inclusive of opposite direction)
  N_th = int[traf_dir == xdir, N_th]
  
  #Total thru lanes
  #int[traf_dir %in% c(xdir,odir), sum(N_th)]
  
  #Vehicle flow rate (veh/s)
  v_v = int[traf_dir == xdir, (v_v/N_d)/3600]
  
  #Bicycle flow rate (bike/s)
  v_p = int[traf_dir == xdir, v_b/3600]
  
  #Critical headway
  t_c = int[traf_dir == xdir, (W_cd/S_b) + t_sb]
  
  #Average number of bikes waiting to cross
  N_c = (v_p*exp(v_p*t_c) + v_v*exp(-v_v*t_c)) / ((v_p + v_v)*exp(v_p-v_v)*t_c)
  
  #Number of rows of bikes
  N_b = max( ((2.5*N_c) / int[traf_dir == xdir, W_bl]), 1 )
  
  #Critical group headway
  t_cg = t_c + 2*(N_b - 1)
  
  #Probability of blocked lane
  P_b = 1 - exp(-t_cg * v_v / N_th)
  
  #Probability of delayed crossing
  P_d = 1 - (1 - P_b)^N_th
  
  #Average gap waiting delay per bicycle
  d_gd = (1/v_v)*(exp(v_v*t_cg) - v_v*t_cg - 1)
  
  #Average delay for any bicycle
  d_bgd = d_gd / P_d
  
  #Average headway
  h = ((1/v_v) - (t_cg + (1/v_v))*exp(-v_v*t_cg)) / (1 - exp(-v_v*t_cg))
  
  #Average number of crossing events
  n = round(1/exp(-v_v*t_cg), 0)
  
  #Initial sum of PYi
  sumPY = 0 
  term1 = 0
  
  #Delay accounting for motorist yield probability
  if(N_th == 1) {
    #One lane
    for(i in 1:n) {
      PY = P_d*M_y*(1 - M_y)^(i-1)
      sumPY = sumPY + PY
      
      #Calculates and sums up the first term
      term1 = h*(i - 0.5)*PY + term1
      
      #Calculates the second term
      term2 = (P_d - sumPY)*d_bgd
    }
    d_bd <- term1 + term2
    
  } else if(N_th == 2) {
    #Two lane
    for(i in 1:n) {
      PY = (P_d - sumPY)*( ((2*P_b*(1-P_b)*M_y) + P_b^2 * M_y^2) / P_d)
      sumPY = sumPY + PY
      
      #Calculates and sums up the first term
      term1 = h*(i - 0.5)*PY + term1
      
      #Calculates the second term
      term2 = (P_d - sumPY)*d_bgd
    }
    d_bd <- term1 + term2
    
  } else if(N_th == 3) {
    #Three lane
    for(i in 1:n) {
      PY = (P_d - sumPY)*((P_b^3 * M_y^3 + 3*P_b^2 * (1-P_b)*M_y^2 + 3*P_b*(1-P_b)^2 * M_y) / P_d)
      sumPY = sumPY + PY
      
      #Calculates and sums up the first term
      term1 = h*(i - 0.5)*PY + term1
      
      #Calculates the second term
      term2 = (P_d - sumPY)*d_bgd
    }
    d_bd <- term1 + term2
    
  } else if(N_th == 4) {
    #Four lane
    for(i in 1:n) {
      PY = (P_d - sumPY)*((P_b^4 * M_y^4 + 4*P_b^3 * (1-P_b)*M_y^3 + 6*P_b*(1-P_b)^2 * M_y^2 + 4*P_b*(1-P_b)^3 * M_y) / P_d)
      sumPY = sumPY + PY
      
      #Calculates and sums up the first term
      term1 = h*(i - 0.5)*PY + term1
      
      #Calculates the second term
      term2 = (P_d - sumPY)*d_bgd
    }
    d_bd <- term1 + term2
  } else {
    stop(paste0("Invalid number of lanes: ", N_th))
  }
  
  return(d_bd)
  
}

#Bicycle LOS score for intersections
func.bike.I_int <- function(int, dir) {
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
  
  #Vehicle count traveling on major street during 15-min period
  n_15mj = (0.25 / N_d)*sum(int$v_v, na.rm = T)
  
  #Cross-section factor
  F_w = 0.0153*W_cd - 0.2144*W_t
  
  #Veh volume factor
  F_v = int[traf_dir == dir, 0.0066*(v_lt + v_th + v_rt)/(4*N_th)]
  
  #Bike delay at intersection
  d_bd = func.bike.d_bd(int, dir)
  
  #Delay factor
  F_delay = 0.0401*ifelse(d_bd == 0, 0, log(d_bd))
  
  #Veh speed factor
  #F_s = 0.00013*n_15mj*int[traf_dir == dir, S_85mj]
  F_s = (sqrt(n_15mj)*int[traf_dir == dir, S_85mj])/400
  
  #LOS Score
  I_int = 4.1324 + F_w + F_v + F_s + F_delay
  
  return(I_int)
}

#Bicycle level of service score for links
func.bike.I_link <- function(link, control) {
  
  #### Caclulate final factors for LOS score
  
  #Cross-section adjustment factor
  F_w = func.bike.F_w.link(link)
  
  #Motorized vehicle volume adjustment factor
  v_ma = ifelse(link$v_m > 4*link$N_th, link$v_m, 4*link$N_th)
  
  F_v = 0.507*log(v_ma / (4*link$N_th))
  
  #Motorized vehicle speed adjustment factor
  F_s = func.bike.F_s.link(link, control)
  
  #Pavement condition factor
  F_p = 7.066 / link$P_c^2
  
  #### LOS Score
  I_link = 0.760 + F_w + F_v + F_s + F_p
  
  return(I_link)
}

#Bicycle LOS score for segment
func.bike.I_seg <- function(link, int) {
  #Put LOS scores for link and intersection into table
  scores = data.table(
    segment_id = link$link_id,
    direction = link$link_dir,
    mode = "bicycle",
    I_link = func.bike.I_link(link, int[traf_dir == link$link_dir, as.character(control)]),
    I_int = func.bike.I_int(int, link$link_dir)
  )
  
  #Calculate segment LOS
  LL = link$LL
  N_aps = link$N_aps
  
  #Boundary indicator factor
  F_bi = 1
  
  #Calculate score
  scores[ , I_seg := 0.160*I_link + 0.011*F_bi*exp(I_int) + 0.035*(N_aps/(LL/5280)) + 2.85]
  
  #Get grade from score
  scores = cbind(scores,
                 setNames(scores[ , lapply(.SD, func.score2LOS), .SDcols = c("I_link","I_int","I_seg"),], c("link_LOS","int_LOS","seg_LOS")))
  
  
  
  return(scores)
}


