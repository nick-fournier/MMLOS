
#These functions are for calculating level of service at intersections


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


#Load data in, defaults to template
dat <- func.loaddat("template")

#Load approx turn delay table
tab_turndelay = fread("./data/turning_vehicle_delay.csv")
tab_turndelay = melt(tab_turndelay, id.vars = "v_m")

#Load approx delay equivalent LOS 
tab_equivlos = fread("./data/ped_equiv_delay_los.csv")



#Pedestrian paved width factor (links)
func.F_w.ped  <- function(link) {
  
  #Adjusted width of paved outside shoulder
  W_osstar = link$W_os - 1.5
  w_osstar = ifelse(link$W_os - 1.5 >= 0, W_osstar, link$W_os)
  
  #Available sidewalk width
  W_A = ifelse(link$W_T > 0, 0, W_T - W_buf)
  
  #Effective total width
  W_v = link$W_ol + link$W_bl + W_osstar + link$W_pk
  w_v = ifelse(link$v_m > 160 | W_A > 0, W_v, W_V * (2 - 0.005*v_m))
  
  #Total width shoulder, bike lane, and parking lane
  W_l = link$W_bl + W_osstar + link$W_pk
  
  #Buffer coeffient
  f_b = ifelse(link$bar_blsw, 5.37, 1)
  
  #Adjusted available sidewalk
  W_aA = min(W_A, 10)
  
  #Sidewalk width coefficient
  f_sw = 6.0 - 0.3*W_aA
  
  #Cross-section adjustment factor
  F_w = -1.2276*log(W_v + 0.5*W_l + 50*link$p_pk + link$W_buf*f_b + W_aA*f_sw)
  
  return(F_w)
}

#Pedestrian traffic speed factor (links)
func.F_s.ped <- function(link, int) {
  #Signalization delay factor
  f_x = switch(int[traf_dir == link$link_dir, control],
               "Signalized" = 1,
               "AWSC - Stop" = 1,
               "TWSC - Stop" = 1, 
               "Uncontrolled" = 0, 
               "Yield" = min(v_th/c_th,1))
  
  #Proximity adjustment factor
  f_v = 2 / (1 + (1 - link$v_m / (52.8*link$N_th*link$S_f))^0.21)
  
  #Startup lost time
  l1 = switch(int[traf_dir == link$link_dir, control],
              "Signalized" = 2, 
              "AWSC - Stop" = 2.5,
              "TWSC - Stop" = 2.5, 
              "Uncontrolled" = 0,
              "Yield" = 2.5)

  
  #Table of vehicle turn delay for each approach, including the boundary
  d_ap = (1+link$N_aps)*tab_turndelay[variable==link$N_th, approx(x = v_m, y = value, xout = link$v_m)$y]
  
  #Other delay
  d_other = 0
  
  #Motorized vehicle midsegment running time
  t_R = f_x * ((6.0 - l1) / (0.0025*link$LL)) + ((3600*link$LL)/(5280*link$S_f)) * f_v + d_ap + d_other
  
  #Link running speed
  S_R = (3600*link$LL) / (5280*t_R)
  
  #Link running speed
  S_R = (3600*link$LL) / (5280*t_R)
  
  F_s = 4*(S_R/100)^2 #Motorized vehicle speed adjustment factor

  return(F_s)
}

#Pedestrian level of service score for links
func.ped.I_link <- function(link, int) {
    
  #### Caclulate final factors for LOS score
  F_w = func.F_w.ped(link) #Cross-section adjustment factor
  
  F_v = 0.0091*link$v_m/(4*link$N_th) #Motorized vehicle volume adjustment factor
  
  F_s = func.F_s.ped(link, int) #Motorized vehicle speed adjustment factor
  
  #### LOS Score
  I_link = 6.0468 + F_w + F_v + F_s
  
  return(I_link)
}

#Pedestrian control delay
func.ped.d_pd <- function(int, dir) {
  switch(int[traf_dir == dir, control],
         "Signalized" = func.ped.d_signal(int, dir), 
         "AWSC - Stop" = 0,
         "TWSC - Stop" = func.ped.d_twsc(int, dir), 
         "Uncontrolled" = 0,
         "Yield" = 0)
  }

func.ped.d_signal <- function(int, dir) {
  with(int[traf_dir == dir, ], ((C - g)^2)/(2*C) )
}

#Pedestrian delay for uncontrolled (e.g., TWSC)
func.ped.d_twsc <- function(int, dir) {
  
  #Picking the crosswalk data from the travel direction (it is perpendicular to vehicles)
  int_xing = switch(dir,
         "NB" = int[traf_dir == "WB", ],
         "SB" = int[traf_dir == "EB", ],
         "EB" = int[traf_dir == "NB", ],
         "WB" = int[traf_dir == "SB", ])
  
  #Yield rate
  M_y = int_xing$M_yp
  
  #Number of thru lanes
  N_th = int_xing$N_th
  
  #Vehicle flow rate (veh/s)
  v_v = int_xing$v_v/3600
  
  #Pedestrian flow rate (ped/s)
  v_p = int_xing$v_p/3600
  
  #Critical headway
  t_c = (int_xing$W_cd / int_xing$S_p) + int_xing$t_sp
  
  #Average number of peds waiting to cross
  N_c = (v_p*exp(v_p*t_c) + v_v*exp(-v_v*t_c)) / ((v_p + v_v)*exp(v_p-v_v)*t_c)
  
  #Number of rows of peds
  N_p = max( ((8*N_c) / int_xing$W_c), 1 )
  
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

#Pedestrian level of service score for midsegment crossings
func.ped.I_mx <- function(link, int) {
  
  #Calculate average diversion delay from midsegment point
  D_c = link$LL / 3
  D_d = 2 * D_c
  d_pd = (0.084*2*D_d / link$S_pf) + func.ped.d_pd(int, link$link_dir)
  
  #Calculate wait delay from control delay
  #Using the effective crosswalk movement direction to get wait delay
  mxdir = switch(link$link_dir,
                 "NB" = "EB",
                 "SB" = "WB",
                 "EB" = "SB",
                 "WB" = "NB")
  
  d_pw = func.ped.d_pd(int, mxdir)
  
  #Convert to LOS score
  I_pd = approx(x = tab_equivlos$delay, y = tab_equivlos$LOS, xout = d_pd)$y
  I_pw = approx(x = tab_equivlos$delay, y = tab_equivlos$LOS, xout = d_pw)$y
  
  return(min(I_pd, I_pw, 6))
}

#Pedestrian LOS score for intersections
func.ped.I_int <- function(link, int) {
  
  xdir = switch(link$link_dir,
                    "NB" = "WB",
                    "SB" = "EB", 
                    "EB" = "NB",
                    "WB" = "SB")
  
  #Intersection delay
  d_pd = func.ped.d_pd(int,link$link_dir)
  
  #Number of traffic lanes crossed
  N_d = int[traf_dir == xdir, N_d]
  
  #Vehicle count traveling on major street during 15-min period
  n_15mj = (0.25 / N_d)*sum(int$v_v)
  
  #Delay factor
  F_delay = 0.0401*log(d_pd)
  
  #Veh speed factor
  F_s = 0.00013*n_15mj*int[traf_dir == link$link_dir, S_85mj]
  
  #Veh volume factor
  F_v = 0.00569*(int[traf_dir == "NB", v_rtor + v_rt] / 4) - int[traf_dir == "NB", N_rtcid]*(0.0027*n_15mj - 0.1946)
  
  #Cross-section factor
  F_w = 0.681*(N_d)^0.514
  
  #LOS score
  I_int = 0.5997 + F_w + F_v + F_s + F_delay
  
  return(I_int)
}

#Pedestrian LOS
func.ped.LOS <- function(score) {
  
  #Split by intersection
  int.split <- split(dat$INT, by = "int_id")  
  link.split <- split(dat$LINK, by = c("link_id","link_dir")) 
  
  #Getting LOS score for each link, intersection, and segment.
  lapply(link.split, function(link) {
    int = int.split[[link$boundary_id]]
    
    #Put LOS scores for link and intersection into table
    LOS = data.table(
      segment_id = link$link_id,
      direction = link$link_dir,
      mode = "pedestrian",
      I_link = func.ped.I_link(link, int),
      I_mx = func.ped.I_mx(link, int),
      I_int = func.ped.I_int(link, int)
    )
    
    #Calculate segment LOS
    p_mx = link$p_mx
    S_pf = link$S_pf
    LL = link$LL
    d_pp = func.ped.d_pd(int, link$link_dir)
    
    LOS[ , I_seg := ( ((I_link*(1-p_mx) + I_mx*p_mx)^3 * (LL/S_pf) + I_int^3 *d_pp) / ((LL/S_pf) + d_pp) )^(1/3)]
    print(LOS)
    return(LOS)
    })
}



