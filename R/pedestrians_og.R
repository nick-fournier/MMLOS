
#1. DETERMINE FREE-FLOW WALKING SPEED
#2. DETERMINE AVERAGE PEDESTRIAN SPACE
#3. DETERMINE PEDESTRIAN DELAY AT INTERSECTION
#4. DETERMINE PEDESTRIAN TRAVEL SPEED
#5. DETERMINE PEDESTRIAN LOS SCORE FOR INTERSECTION
#6. DETERMINE PEDESTRIAN LOS SCORE FOR LINK
#7. DETERMINE LINK LOS
#8. DETERMINE ROADWAY CROSSING DIFFICULTY FACTOR
#9. DETERMINE PEDESTRIAN LOS SCORE FOR SEGMENT
#10. DETERMINE SEGMENT LOS


#Pedestrian paved width factor (links)
func.ogped.F_w.link  <- function(link) {
  
  #Total width of outside thru lane, bike lane, and paved shoulder/parking
  W_t = link[, W_ol + W_bl + W_buf + W_os]
  
  #Adjusted width of paved outside shoulder
  W_osstar = link$W_os - 1.5
  w_osstar = ifelse(W_osstar >= 0, W_osstar, link$W_os)
  
  #Effective total width
  if(link$v_m > 160 | link$div) {
    W_v = W_t
  } else {
    W_v = W_t*(2 - 0.005*link$v_m)
  }
    
  #Total width shoulder, bike lane, and parking lane
  W_l = link$W_bl + W_osstar + link$W_pk
  
  if(link$p_pk < 0.25 | link$p_strp) {
    W_l = link$W_bl + W_osstar
  } else {
    W_l = 10
  }
  
  #Available sidewalk width
  W_A = ifelse(link$W_T > 0, 0, link$W_T - W_buf)
  
  #Adjusted available sidewalk
  W_aA = min(W_A, 10)
  
  #Buffer coeffient
  f_b = ifelse(link$h_blsw > 3, 5.37, 1)
  
  #Sidewalk width coefficient
  f_sw = 6.0 - 0.3*W_aA
  
  #Cross-section adjustment factor
  F_w = -1.2276*log(W_v + 0.5*W_l + 50*link$p_pk + link$W_buf*f_b + W_aA*f_sw)
  
  return(F_w)
}

#Pedestrian level of service score for links
func.ogped.I_link <- function(link, int) {
    
  #### Caclulate final factors for LOS score
  F_w = func.ogped.F_w.link(link) #Cross-section adjustment factor
  
  #Motorized vehicle volume adjustment factor
  F_v = 0.0091*link$v_m/(4*link$N_th) 
  
  #Vehicle running speed
  S_R = func.auto.S_R(link, int)
  
  #Motorized vehicle speed adjustment factor
  F_s = 4*(S_R/100)^2 
  
  #### LOS Score
  I_link = 6.0468 + F_w + F_v + F_s
  
  return(I_link)
}

#Pedestrian control delay
func.ogped.d_pd <- function(int, dir) {
  switch(int[traf_dir == dir, control],
         "Signalized" = func.ogped.d_signal(int, dir), 
         "AWSC - Stop" = 0,
         "TWSC - Stop" = func.ogped.d_twsc(int, dir), 
         "Uncontrolled" = 0,
         "Yield" = 0)
  }

#Pedestrian control delay from signal
func.ogped.d_signal <- function(int, dir) {
  with(int[traf_dir == dir, ], ((C - g)^2)/(2*C) )
}

#Current HCM Pedestrian delay for uncontrolled (e.g., TWSC)
func.ogped.d_twsc <- function(int, dir) {
  
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
  M_y = int[traf_dir == xdir, M_yp]
  
  #Number of thru lanes (inclusive of opposite direction)
  N_th = int[traf_dir == xdir, N_th]
  
  #Length of crosswalk
  L = int[traf_dir == xdir, W_cd]
  
  #Total thru lanes
  #int[traf_dir %in% c(xdir,odir), sum(N_th)]
  
  #Vehicle flow rate (veh/s)
  v_v = int[traf_dir == xdir, v_v/3600]
  
  #Pedestrian flow rate (ped/s)
  v_p = int[traf_dir == xdir, v_p/3600]
  
  #Critical headway
  t_c = int[traf_dir == xdir, (W_cd/S_p) + t_sp]
  
  #Average number of peds waiting to cross
  N_c = (v_p*exp(v_p*t_c) + v_v*exp(-v_v*t_c)) / ((v_p + v_v)*exp(v_p-v_v)*t_c)
  
  #Number of rows of peds
  N_p = int[traf_dir == xdir, round( 8*(N_c - 1)/W_c)] + 1
  
  #Critical group headway
  t_cg = t_c + 2*(N_p - 1)
  
  #Probability of blocked lane
  P_b = 1 - exp(-t_cg * v_v / N_th)
  
  #Probability of delayed crossing
  P_d = 1 - (1 - P_b)^N_th
  
  #Average gap waiting delay per pedestrian
  d_g = (1/v_v)*(exp(v_v*t_cg) - v_v*t_cg - 1)
  
  #Average delay for any pedestrian
  d_gd = d_g / P_d
  
  #Average headway
  h = N_th/v_v
  
  #Average number of crossing events
  n = round(d_gd/h)
  
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
      term2 = (P_d - sumPY)*d_gd
    }
    d_pd <- term1 + term2
    
  } else if(N_th == 2) {
    #Two lane
    for(i in 1:n) {
      PY = (P_d - sumPY)*( ((2*P_b*(1-P_b)*M_y) + P_b^2 * M_y^2) / P_d)
      sumPY = sumPY + PY
      
      #Calculates and sums up the first term
      term1 = h*(i - 0.5)*PY + term1
      
      #Calculates the second term
      term2 = (P_d - sumPY)*d_gd
    }
    d_pd <- term1 + term2
    
  } else if(N_th == 3) {
    #Three lane
    for(i in 1:n) {
      PY = (P_d - sumPY)*((P_b^3 * M_y^3 + 3*P_b^2 * (1-P_b)*M_y^2 + 3*P_b*(1-P_b)^2 * M_y) / P_d)
      sumPY = sumPY + PY
      
      #Calculates and sums up the first term
      term1 = h*(i - 0.5)*PY + term1
      
      #Calculates the second term
      term2 = (P_d - sumPY)*d_gd
    }
    d_pd <- term1 + term2
    
  } else if(N_th == 4) {
    #Four lane
    for(i in 1:n) {
      PY = (P_d - sumPY)*((P_b^4 * M_y^4 + 4*P_b^3 * (1-P_b)*M_y^3 + 6*P_b*(1-P_b)^2 * M_y^2 + 4*P_b*(1-P_b)^3 * M_y) / P_d)
      sumPY = sumPY + PY
      
      #Calculates and sums up the first term
      term1 = h*(i - 0.5)*PY + term1
      
      #Calculates the second term
      term2 = (P_d - sumPY)*d_gd
    }
    d_pd <- term1 + term2
  } else {
    stop(paste0("Invalid number of lanes: ", N_th))
  }
  
  return(d_pd)
  
}

#Pedestrian crossing difficulty factor
func.ogped.F_cd <- function(link, int, Ilink, Iint) {
  
  #Crossing direction for diversion
  mxdir = switch(link$link_dir,
                 "NB" = "EB",
                 "SB" = "WB",
                 "EB" = "SB",
                 "WB" = "NB")
  
  #If missing approach, just choose opposite equivalent
  if( is.na(int[traf_dir == mxdir, N_d]) ) {
    mxdir = switch(mxdir,
                   "NB" = "SB",
                   "SB" = "NB",
                   "EB" = "WB",
                   "WB" = "EB")
  }
  
  #Calculate average diversion delay from midsegment point
  D_c = link$LL / 3
  D_d = 2 * D_c
  
  #Calculate average diversion delay from midsegment point
  d_pd = (D_d / link$S_pf) + func.ogped.d_pd(int, mxdir)
  
  #Calculate wait delay from control delay
  d_pw = func.ogped.d_twsc(int, mxdir)
  
  #Cossing delay
  d_px = min(d_pd, d_pw, 60)
  
  
  #
  
  #Crossing difficulty factor
  F_cd = 1 + (0.10*d_px - (0.318*Ilink + 0.220*Iint + 1.606))/7.5
  
  return(F_cd)
}

#Pedestrian LOS score for intersections
func.ogped.I_int <- function(link, int) {
  
  #The traffic direction being crossed
  xdir = switch(link$link_dir,
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
  
  #Intersection delay
  d_pd = func.ogped.d_pd(int,link$link_dir)
  
  #Number of traffic lanes crossed
  N_d = int[traf_dir == xdir, N_d]
  
  N_d = ifelse(is.na(N_d), int[traf_dir == odir, N_d], N_d)
  
  #Vehicle count traveling on major street during 15-min period
  n_15mj = (0.25 / N_d)*sum(int$v_v, na.rm = T)
  
  #Left and right turns with ped movement
  v_rtlt = int[traf_dir == link$link_dir, v_rtor + v_ltperm]
  
  #Number of right turn channel islands
  N_rtcid = int[traf_dir == link$link_dir, N_rtcid]
  
  #Delay factor
  F_delay = 0.0401*ifelse(d_pd == 0, 0, log(d_pd))
  
  #Veh speed factor
  F_s = 0.00013*n_15mj*int[traf_dir == link$link_dir, S_85mj]
  
  #Veh volume factor
  F_v = 0.00569*(v_rtlt / 4) - N_rtcid*(0.0027*n_15mj - 0.1946)
  
  #Cross-section factor
  F_w = 0.681*(N_d)^0.514
  
  #LOS score
  I_int = 0.5997 + F_w + F_v + F_s + F_delay
  
  return(I_int)
}

#Pedestrian LOS score for segment
func.ogped.I_seg <- function(link, int) {
  #Put LOS scores for link and intersection into table
  scores = data.table(
    segment_id = link$link_id,
    direction = link$link_dir,
    mode = "pedestrian",
    I_link = func.ogped.I_link(link, int),
    I_int = func.ogped.I_int(link, int)
  )
  
  F_cd = func.ogped.F_cd(link,int, scores$I_link, scores$I_int)
  
  #Calculate score
  scores[ , I_seg := F_cd*(0.318*I_link + 0.220*I_int + 1.606)]
  
  #Get grade from score
  scores = cbind(scores,
                 setNames(scores[ , lapply(.SD, func.score2LOS), .SDcols = c("I_link","I_int","I_seg"),], c("link_LOS","int_LOS","seg_LOS")))
  
  return(scores)
}



