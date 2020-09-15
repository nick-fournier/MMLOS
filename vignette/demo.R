

#library(MMLOS)
library(data.table)
library(ggplot2)
library(gridExtra)
library(pbapply)
library(extrafont)
library(ggpubr)




# #Load data in from files.
# dat <- loaddat()
# 
# #
# Current <- calcMMLOSdat, revs = F)
# 
# Revisions <- calcMMLOSdat, revs = T)
# 
# #Display
# Current$bike
# Revisions$bike



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
             I_int.rev = bike.I_int(int, "EB"),
             I_int.og = ogbike.I_int(int, "EB"))
}))

#Melt into longform
LOS <- melt(LOS, c("v_v","S_85mj"))

#Score
LOS$LOS <- sapply(LOS$value, MMLOS::score2LOS)


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
