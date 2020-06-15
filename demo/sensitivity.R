
library(data.table)
library(ggplot2)
library(MMLOS)
library(gridExtra)
library(pbapply)
library(extrafont)
library(ggpubr)

#### LOS Analysis of speed vs volume at intersections ####
#Intersections vary speed and volume
int.spdvol <- expand.grid(
  int_id   = NA,
  app_dir  = NA,
  traf_dir = NA,
  N_d      = 1,
  N_th     = 1,
  S_85mj   = seq(5, 50, by = 5), #links$S_f and int$S_85mj
  v_rt     = NA,
  v_lt     = NA,
  v_th     = NA,
  v_v      = seq(50, 1200, by = 50), #Vehicle volume at intersection
  v_rtor   = NA,
  v_ltperm = NA,
  v_p      = 100,
  v_b      = 250,
  v_bl     = 250/4,
  v_br     = 250/4,
  P_bl2    = 0.5,
  N_rtcid  = 0,
  g        = 31.7,
  l        = 3.3,
  C        = 90,
  Walkmi   = 31.7,
  W_c      = 12,
  W_cd     = 24,
  L        = 24,
  W_bl     = 5,
  W_ol     = 12,
  p_pk     = 0.8,
  W_os     = 8,
  M_yp     = 0.5,
  M_yb     = 0.05,
  S_p      = 3.5,
  S_b      = 10,
  t_sb     = 3,
  t_sp     = 3,
  control  = "Signalized",
  curb     = TRUE)

#Calculate subsequent dummy vars
int.spdvol <- as.data.table(int.spdvol)
int.spdvol[ , v_rt := v_v/4]
int.spdvol[ , v_lt := v_v/4]
int.spdvol[ , v_th := v_v/2]
int.spdvol[ , v_rtor := v_v/2]
int.spdvol[ , v_ltperm := v_v/4]
int.spdvol[ , int_id := 1:nrow(int.spdvol)]
int.spdvol <- rbindlist(lapply(c("NB","SB","EB","WB"), function(x) {
  tmp = int.spdvol
  tmp$traf_dir <- rep(x, nrow(int.spdvol))
  tmp$app_dir <- rep(switch(x,
                        "NB" = "S",
                        "SB" = "N",
                        "EB" = "W",
                        "WB" = "E"),
                     nrow(int.spdvol))
  return(tmp)
}))

#Calculate LOS
LOS <- rbindlist(pblapply(unique(int.spdvol$int_id), function(x) {
  int = int.spdvol[int_id == x, ]
  data.table("v_v" = int$v_v[1], "S_85mj" = int$S_85mj[1],
             I_int.rev = func.bike.I_int(int, "EB"),
             I_int.og = func.ogbike.I_int(int, "EB"))
}))

#Melt into longform
LOS <- melt(LOS, c("v_v","S_85mj"))

#Score
LOS$LOS <- sapply(LOS$value, MMLOS::func.score2LOS)



#### Right turn delay ####
#Intersections vary right turn volume
int.rtbikevol <- expand.grid(
  int_id   = NA,
  app_dir  = NA,
  traf_dir = NA,
  N_d      = 1,
  N_th     = 1,
  S_85mj   = 25, #links$S_f and int$S_85mj
  v_rt     = seq(100, 1000, by = 50),
  v_lt     = NA,
  v_th     = NA,
  v_v      = NA, #Vehicle volume at intersection
  v_rtor   = NA,
  v_ltperm = NA,
  v_p      = 100,
  v_b      = 250, #seq(0, 1000, by = 150),
  v_bl     = NA,
  v_br     = NA,
  P_bl2    = 0.5,
  N_rtcid  = 0,
  g        = 31.7,
  l        = 3.3,
  C        = 90,
  Walkmi   = 31.7,
  W_c      = 12,
  W_cd     = 24,
  L        = 24,
  W_bl     = 5,
  W_ol     = 12,
  p_pk     = 0.8,
  W_os     = 8,
  M_yp     = 0.5,
  M_yb     = 0.1,
  S_p      = 3.5,
  S_b      = 10,
  t_sb     = 3,
  t_sp     = 3,
  control  = "Signalized",
  curb     = TRUE)

#Calculate subsequent dummy vars
int.rtbikevol <- as.data.table(int.rtbikevol)
int.rtbikevol[ , v_lt := v_rt/4]
int.rtbikevol[ , v_th := v_rt]
int.rtbikevol[ , v_rtor := v_v/2]
int.rtbikevol[ , v_ltperm := v_rt/4]
int.rtbikevol[ , v_v := v_rt + v_lt + v_th]
int.rtbikevol[ , v_bl := v_b/4]
int.rtbikevol[ , v_br := v_b/4]
int.rtbikevol[ , int_id := 1:nrow(int.rtbikevol)]
int.rtbikevol <- rbindlist(lapply(c("NB","SB","EB","WB"), function(x) {
  tmp = int.rtbikevol
  tmp$traf_dir <- rep(x, nrow(int.rtbikevol))
  tmp$app_dir <- rep(switch(x,
                            "NB" = "S",
                            "SB" = "N",
                            "EB" = "W",
                            "WB" = "E"),
                     nrow(int.rtbikevol))
  return(tmp)
}))

#### Right turning vehicle delay
rtdelay <- rbindlist(lapply(unique(int.rtbikevol$int_id), function(x) {
  int = int.rtbikevol[int_id == x, ]
  data.table("v_rt" = int$v_rt[1], "v_b" = int$v_b[1], "Proposed revisions" = func.bike.d_bS(int, "EB"))
}))

rtdelay <- merge(rtdelay, 
      rbindlist(lapply(unique(int.rtbikevol$int_id), function(x) {
        int = int.rtbikevol[int_id == x, ]
        dir = "EB"
        #HCM bike lane capacity
        c_b = int[traf_dir == dir, 2000*g/C]
        #Calculate delay from signal, including right-turn vehicle encroachment
        d_bS = int[traf_dir == dir, (0.5*C*(1-(g/C))^2) / (1 - min(v_b/c_b, 1)*(g/C)) ]
        
        data.table("v_rt" = int$v_rt[1], "v_b" = int$v_b[1], "Current HCM" = d_bS)
      })),
    by = c("v_rt","v_b"))

#Convert to veh per cycle
rtdelay[ , v_rt := int.rtbikevol$C[1]*v_rt/3600]
rtdelay[ , v_b := int.rtbikevol$C[1]*v_b/3600]
rtdelay[ , (c("Proposed revisions","Current HCM")) := lapply(.SD, function(x) x/v_b),
         .SDcols = c("Proposed revisions","Current HCM")]






#### Traffic speed exposure factor ####
#Intersections vary speed and volume
int.spd <- expand.grid(
  int_id   = NA,
  app_dir  = NA,
  traf_dir = NA,
  N_d      = 1,
  N_th     = 1,
  S_85mj   = seq(5, 50, by = 5), #links$S_f and int$S_85mj
  v_rt     = NA,
  v_lt     = NA,
  v_th     = NA,
  v_v      = seq(50, 1200, by = 50), #Vehicle volume at intersection
  v_rtor   = NA,
  v_ltperm = NA,
  v_p      = 100,
  v_b      = 100,
  v_bl     = 100/4,
  v_br     = 100/4,
  P_bl2    = 0.5,
  N_rtcid  = 0,
  g        = 31.7,
  l        = 3.3,
  C        = 90,
  Walkmi   = 31.7,
  W_c      = 12,
  W_cd     = 24,
  L        = 24,
  W_bl     = 5,
  W_ol     = 12,
  p_pk     = 0.8,
  W_os     = 8,
  M_yp     = 0.5,
  M_yb     = 0.1,
  S_p      = 3.5,
  S_b      = 10,
  t_sb     = 3,
  t_sp     = 3,
  control  = "Signalized",
  curb     = TRUE)

#Calculate subsequent dummy vars
int.spd <- as.data.table(int.spd)
int.spd[ , v_rt := v_v/4]
int.spd[ , v_lt := v_v/4]
int.spd[ , v_th := v_v/2]
int.spd[ , v_rtor := v_v/2]
int.spd[ , v_ltperm := v_v/4]
int.spd[ , int_id := 1:nrow(int.spd)]
int.spd <- rbindlist(lapply(c("NB","SB","EB","WB"), function(x) {
  tmp = int.spd
  tmp$traf_dir <- rep(x, nrow(int.spd))
  tmp$app_dir <- rep(switch(x,
                            "NB" = "S",
                            "SB" = "N",
                            "EB" = "W",
                            "WB" = "E"),
                     nrow(int.spd))
  return(tmp)
}))

#Calculate speed factor
spdfact <- rbindlist(pblapply(unique(int.spd$int_id), function(x) {
  int = int.spd[int_id == x, ]
  #The traffic direction being crossed
  xdir = "NB"
  #Opposite cross street dir
  odir ="SB"
  N_d = int[traf_dir == xdir, N_d]
  N_d = ifelse(is.na(N_d), int[traf_dir == odir, N_d], N_d)
  n_15mj = (0.25 / N_d)*sum(int$v_v, na.rm = T)
  
  data.table("v_v" = int$v_v[1], "S_85mj" = int$S_85mj[1],
             F_s = (sqrt(n_15mj)*int[traf_dir == "EB", S_85mj])/200)
}))

  
  
#### Left turn delay from car volume vs proportion of 1 and 2 stage left turns ####
#Intersections vary right turn volume
int.ltbike <- expand.grid(
  int_id   = NA,
  app_dir  = NA,
  traf_dir = NA,
  N_d      = 1:4, #Number of lanes crossed
  N_th     = 1,
  S_85mj   = 25,
  v_rt     = NA,
  v_lt     = NA,
  v_th     = NA,
  v_v      = seq(100, 2000, by = 25), #Vehicle volume at intersection
  v_rtor   = NA,
  v_ltperm = NA,
  v_p      = 100,
  v_b      = 250,
  v_bl     = NA,
  v_br     = NA,
  P_bl2    = 0.5,
  N_rtcid  = 0,
  g        = 31.7,
  l        = 3.3,
  C        = 90,
  Walkmi   = 31.7,
  W_c      = 12,
  W_cd     = 24,
  L        = 24,
  W_bl     = 5,
  W_ol     = 12,
  p_pk     = 0.8,
  W_os     = 8,
  M_yp     = 0.5,
  M_yb     = 0.05,
  S_p      = 3.5,
  S_b      = 10,
  t_sb     = 3,
  t_sp     = 3,
  control  = "Signalized",
  curb     = TRUE)

#Calculate subsequent dummy vars
int.ltbike <- as.data.table(int.ltbike)
int.ltbike[ , N_th := N_d]
int.ltbike[ , v_v := v_v*N_d]
int.ltbike <- int.ltbike[v_v < 5000, ]
int.ltbike[ , v_rt := v_v/4]
int.ltbike[ , v_lt := v_v/4]
int.ltbike[ , v_th := v_v/2]
int.ltbike[ , v_rtor := v_v/2]
int.ltbike[ , v_ltperm := v_v/4]
int.ltbike[ , int_id := 1:nrow(int.ltbike)]
int.ltbike <- rbindlist(lapply(c("NB","SB","EB","WB"), function(x) {
  tmp = int.ltbike
  tmp$traf_dir <- rep(x, nrow(int.ltbike))
  tmp$app_dir <- rep(switch(x,
                            "NB" = "S",
                            "SB" = "N",
                            "EB" = "W",
                            "WB" = "E"),
                     nrow(int.ltbike))
  return(tmp)
}))
#Sort just for processing sake
int.ltbike <- int.ltbike[order(-v_v), ]

#Calculate delay
ltdelay <- rbindlist(pblapply(unique(int.ltbike$int_id), function(x) {
  int = int.ltbike[int_id == x, ]
  data.table("v_v" = int$v_v[1], "v_b" = int$v_b[1], "N_d" = int$N_d[1],
             "One-stage turn delay" = func.bike.d_1stageleft(int, "EB"), #One-stage left turn delay
             "Two-stage turn delay" = func.bike.d_2stageleft(int, "EB")  #Two-stage left turn delay
             )
}))

#Adjust volume per lane
ltdelay[ , v_vpl := v_v/N_d]

#Convert to veh per cycle
ltdelay[ , v_v := int.ltbike$C[1]*v_v/3600]
ltdelay[ , v_vpl := int.ltbike$C[1]*v_vpl/3600]
ltdelay[ , v_b := int.ltbike$C[1]*v_b/3600]
ltdelay[ , (c("One-stage turn delay","Two-stage turn delay")) := lapply(.SD, function(x) x/v_b),
         .SDcols = c("One-stage turn delay","Two-stage turn delay")]

#### Plots ####
#### #LOS plot ####
plot.LOS = lapply(c("I_int.rev","I_int.og"), function(x) {
  ggplot(data = LOS[variable == x, ], aes(x = v_v, y = S_85mj, z = value)) + 
    # geom_tile() +
    # scale_fill_distiller("LOS", palette = "RdYlGn", limits = range(LOS$value)) +
    geom_contour_filled(breaks = c(-Inf,2.00,2.75,3.50,4.25,5.00,Inf)) +
    scale_fill_manual("LOS score", values = rev(RColorBrewer::brewer.pal(n = 6, name = "RdYlGn")),
                      labels = c("A","B","C","D","E","F")) +
    scale_x_continuous("Automobile traffic volume (veh/hr)", expand = c(0,0)) +
    scale_y_continuous("Automobile traffic speed (mph)", expand = c(0,0)) +
    coord_fixed(ratio = 25) +
    theme_bw() +
    theme(text = element_text(family = "Times New Roman"), legend.position = "bottom", )
})
names(plot.LOS) <- c("rev","og")

ggarrange(plot.LOS$rev, plot.LOS$og, ncol = 2, common.legend = T, legend = "right")

#### #Speed exposure plot ####
ggplot(data = spdfact, aes(x = v_v, y = S_85mj, z = F_s)) + 
  geom_contour_filled(breaks = 0:ceiling(max(spdfact$F_s))) +
  scale_fill_manual(expression("Traffic speed\nexposure factor,"~F[s]), values = rev(RColorBrewer::brewer.pal(n = 10, name = "RdYlGn")),
                    labels = paste(0:floor(max(spdfact$F_s)),
                                   1:ceiling(max(spdfact$F_s)), sep="-")) +
  scale_x_continuous("Automobile traffic volume (veh/hr)", expand = c(0,0)) +
  scale_y_continuous("Automobile traffic speed (mph)", expand = c(0,0)) +
  coord_fixed(ratio = 25) +
  theme_bw() +
  theme(text = element_text(family = "Times New Roman"), legend.position = "right", )

#### #Right turn delay plot ####
ggplot(data = melt(rtdelay, id.vars = c("v_rt","v_b")),
       aes(x = v_rt, y = value, color = variable, linetype = variable)) +
  geom_line() + 
  scale_x_continuous("Number of right turning vehicles\nper 90 second signal cycle", expand = c(0,0)) +
  scale_y_continuous("Average icycle delay (s)", expand = c(0,0), limits = c(3,5)) +
  scale_color_brewer(NULL, palette = "Set1", direction = -1) +
  scale_linetype(NULL) +
  theme_bw() +
  theme(text = element_text(family = "Times New Roman"), 
        legend.position = "right",
        legend.background = element_blank(),
        legend.box.background = element_blank())

#### #Left turn delay plot ####
ggplot(data = melt(ltdelay, id.vars = c("v_v","v_b","v_vpl","N_d")),
       aes(x = v_vpl, y  = value, color = factor(N_d), linetype = variable)) +
  geom_line() + 
  scale_x_continuous("Number of vehicles per lane\nper 90 second signal cycle", expand = c(0,0)) +
  scale_y_continuous("Average bicycle delay (s)", expand = c(0,0)) +
  scale_color_brewer("Number of lanes crossed", palette = "Set1",
                     labels = c("One lane", "Two lanes", "Three lanes", "Four lanes")) +
  scale_linetype(NULL) +
  coord_cartesian(ylim = c(0,20)) +
  theme_bw() +
  theme(text = element_text(family = "Times New Roman"),
        plot.subtitle = element_text(face="bold", hjust=0.5))




