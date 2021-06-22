
library(MMLOS)
library(knitr)
library(data.table)
library(ggplot2)
library(ggpubr)



#### Run MMLOS on data ####
berk.ex <- MMLOScalc("berkeley", revs = F)
berk.rev <- MMLOScalc("berkeley", revs = T)

pasa.ex <- MMLOScalc("pasadena", revs = F)
pasa.rev <- MMLOScalc("pasadena", revs = T)


#### Combine the data tables horizontally (add columns). ####

formatdata <- function(existing, revised) {
  #Add labels
  existing[['bike']][ , method := "Existing"]
  revised[['bike']][ , method := "Revised"]

  # Add row number, help us to sort it later
  existing[['bike']]$rn <- 1:nrow(existing[['bike']])
  revised[['bike']]$rn <- 1:nrow(revised[['bike']])
  
  # Combine the data tables vertically (stacking rows). Vertical format makes plotting easier
  scores <- rbind(existing[['bike']], revised[['bike']])
  # Let's create a new unique ID column that contains the segment ID, direction, and methodology version
  scores[ , id := paste(segment_id, direction, method)]
  #Re-shape the data from wide into long format. This is for the numeric scores
  scores <- melt(scores[ , .(id, rn, method, I_int, I_link, I_seg)], id.vars = c("id","rn","method"))
  #Sort data
  scores <- scores[order(rn), ]
  
  #Labels
  scores[variable == "I_int", variable := "Intersection"]
  scores[variable == "I_link", variable := "Link"]
  scores[variable == "I_seg", variable := "Segment"]
  
  #Reshape for another plot
  scores.xy <- dcast(scores, rn+variable~method, value.var = "value")
  
  return( list("wide" = scores, "long" = scores.xy) )
}


berk.scores = formatdata(berk.ex, berk.rev)
pasa.scores = formatdata(pasa.ex, pasa.rev)


#### Plotting ####
#Berk Bar Plot
berk.bar <- ggplot(data = berk.scores[['wide']], aes(x = id, y = value, fill = variable, alpha = method)) + 
  geom_col(position = "dodge") +
  scale_fill_brewer("LOS component", palette = "Set1") +
  scale_alpha_discrete("Methodology", range = c(0.5,1)) +
  labs(x = NULL, y = "LOS Score") +
  coord_fixed(ylim = c(0,5)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Pasadena Bar Plot
pasa.bar <- ggplot(data = pasa.scores[['wide']], aes(x = id, y = value, fill = variable, alpha = method)) + 
  geom_col(position = "dodge") +
  scale_fill_brewer("LOS component", palette = "Set1") +
  scale_alpha_discrete("Methodology", range = c(0.5,1)) +
  labs(x = NULL, y = "LOS Score") +
  coord_fixed(ylim = c(0,5)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


#Berk Scat Plot
berk.scat <- ggplot(data = berk.scores[['long']], aes(x = Existing, y = Revised, color = variable)) + 
  geom_point() +
  geom_abline(slope = 1) +
  scale_color_brewer("LOS component", palette = "Set1") +
  labs(x = "Existing HCM LOS score", y = "Revised HCM LOS Score") +
  coord_fixed(xlim = c(0,5), ylim = c(0,5)) +
  ggtitle("Hearst Avenue, Berkeley, CA") +
  theme_bw() + theme(legend.position = 'none')
  

#Pasadena Scat Plot
pasa.scat <- ggplot(data = pasa.scores[['long']], aes(x = Existing, y = Revised, color = variable)) + 
  geom_point() +
  geom_abline(slope = 1) +
  scale_color_brewer("LOS component", palette = "Set1") +
  labs(x = "Existing HCM LOS score", y = "Revised HCM LOS Score") +
  coord_fixed(xlim = c(0,6), ylim = c(0,6)) +
  ggtitle("Colorado Boulevard, Pasadena, CA") +
  theme_bw() + 
  theme(legend.position = c(0.7,0.25),
        legend.box.background = element_rect(fill = "transparent"),
        legend.key = element_rect(fill = "transparent"),
        legend.background = element_rect(fill = "transparent"))  


#
ggarrange(plotlist = list(berk.scat, pasa.scat), ncol = 2, common.legend = T, legend = "right")
ggarrange(plotlist = list(berk.bar, pasa.bar), ncol = 2, common.legend = T, legend = "right")


ggsave(filename = "C:\\Users\\nichf\\Desktop\\berk_scat.png", plot = berk.scat, width = 3.5, height = 3.5, units = "in")
ggsave(filename = "C:\\Users\\nichf\\Desktop\\pasa_scat.png", plot = pasa.scat, width = 3.5, height = 3.5, units = "in")

ggsave(filename = "C:\\Users\\nichf\\Desktop\\berk_bar.png", plot = berk.bar, width = 6, height = 5, units = "in")
ggsave(filename = "C:\\Users\\nichf\\Desktop\\pasa_bar.png", plot = pasa.bar, width = 6, height = 5, units = "in")





