#' Transit LOS score for segment
#' 
#' @param link Data.table of link data.
#' @param int Data.table of intersection data.
#' @return A data.table with numeric and letter grade LOS scores
#' @examples
#' transit.I_seg(link, int)
#' @export
transit.I_seg <- function(link, int) {
  
  ### Constants
  e = -0.4 #Ridership elasticity
  t_d = 12 #Average dwell time
  
  #Base travel time rate
  if(link$pop) {
    T_btt = 6
  } else {
    T_btt = 4
  }
  
  r_at = 4.0 #Transit acceleration rate
  r_dt = 4.0 #Transit deceleration rate
  t_l = 3 #Transit running time loss 
  #(Exhibit 17-23, using "Typical Mixed traffic" as default)
  
  s = auto.satflow(link, int) #Saturation flow rate
  X = auto.VCratio(link, int) #Volume-to-capacity ratio
  
  #Acceleration delay function intersection control
  if(link$nearside) {
    f_ad = switch(int[traf_dir == link$link_dir, control],
           "Signalized" = int[traf_dir == link$link_dir, g/C], 
           "Roundabout" = 1 - X,
           "AWSC - Stop" = 0,
           "TWSC - Stop" = 0, 
           "Uncontrolled" = 0,
           "Yield" = 0)
  } else {
    f_ad = 1
  }
  
  #Proportion of dwell time occuring at green
  if(int[traf_dir == link$link_dir, control] == "Signalized") {
    f_dt = int[traf_dir == link$link_dir, g/C]
  } else {
    f_dt = 1
  }
  
  ### 
  
  #Midsegment flow per lane in direction of travel
  v_m = int[ traf_dir == link$link_dir, sum(v_rt + v_lt + v_th) / N_th]
  
  #Motorized vehicle running speed along segment
  S_R = auto.S_R(link, int[traf_dir == link$link_dir, control])
  
  #Transit vehicle running speed
  S_Rt = min(S_R, 61/(1+exp(-1+(1185*link$N_th)/link$LL)))
  
  #Acceleration
  d_ad = (5280/3600)*(S_Rt/2)*((1/r_at) + (1/r_dt))*f_ad
  
  #Delay due to passengers
  d_ps = t_d*f_dt
  
  #Reentry delay
  d_re = int[traf_dir == link$link_dir, g] / (s - v_m)
  
  #Delay due to a stop
  d_ts = d_ad + d_ps + d_re
  
  #Segment running time
  t_Rt = ((3600*link$LL)/(5280*S_Rt)) + d_ts*link$n_bus
  
  #Delay at intersection
  d_t = t_l*60*(link$LL/5280)
  
  #Transit travel speed
  S_Ttseg = (3600*link$LL) / (5280*(t_Rt + d_t))
  
  # Headway factor
  F_h = 4*exp(-1.434/(link$n_bus))
  
  
  #Passenger load weighting factor
  if(link$F_l <= 0.80) {
    a_l = 1.00
  } else if(0.80 < link$F_l & link$F_l <= 1) {
    a_l = 1 + (4*(link$F_l - 0.80)) / 4.2
  } else if( link$F_l > 1) {
    a_l = 1 + (4*(link$F_l - 0.80) + (F_l - 1)*(0.65 + 5*(F_l-1))) / (4.2*link$F_l)
  }
  
  #Amenity time rate
  T_at = (1.3*link$p_sh + 0.2*link$p_be)/link$L_pt
  
  #Excess wait time due to late arrivals
  t_ex = (link$t_late*(1-link$p_ot))^2
  
  #Excess wait time rate due to late arrivals
  T_ex = t_ex/link$L_pt
  
  #Perceived travel time rate
  T_ptt = (a_l*(60/S_Ttseg)) + 2*T_ex - T_at
    
  #Perceived travel time factor
  F_tt = ((e-1)*T_btt - (e+1)*T_ptt) / ((e-1)*T_ptt - (e+1)*T_btt)
  
  #Wait-Ride score
  s_wr = F_h*F_tt
  
  #Pedestrian LOS for link
  I_plink = ped.I_link(link, int)
  
  #Segment LOS score
  I_seg = 6 + 1.5*s_wr + 0.15*I_plink
  
  return(I_seg)
}