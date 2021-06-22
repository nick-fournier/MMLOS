library(ggplot2)
library(gridExtra)
library(extrafont)
library(directlabels)
library(metR)

W_ol = 12
W_bl = 6
W_os = 0
A = 0.5
B = 1
wmax = 12

wbufold <- function(x,y) 4*(x^2 + 24*y)^(1/4)
wbuf <- function(x,y,a,b) wmax*(1-exp(-a*x-b*y))
# Fw <- function(W_buf) -0.005*(W_ol+W_bl+W_os+W_buf)^2
Fw <- function(W_buf) -0.005*(W_bl + W_buf)^2

binlabel <- function(x) {
  x = scales::percent(x, accuracy=1)
  x = paste(x[-length(x)], x[-1], sep = "-")
  return(x)
}


###
data <- expand.grid(x = seq(0, 10, by = 0.1), y = seq(0, 10, by = 0.1))
data$wbuf <- mapply(wbuf, data$x, data$y, A, B)
data$Fw <- mapply(Fw, data$wbuf)

data$wbufold <- mapply(wbufold, data$x, data$y)
data$Fwold <- mapply(Fw, data$wbufold)

expression(paste(alpha=1.5,beta=3))

#### BUFFER FUNC FORM ####
plot.bufferform <- ggplot(data.frame(x=c(0,12)), aes(x)) +
  stat_function(fun = function(x) wbuf(x,0, a=1.5, b=3)/wmax, aes(linetype = "Horizontal only", color="high"), size=1) +
  stat_function(fun = function(x) wbuf(0,x, a=1.5, b=3)/wmax, aes(linetype = "Vertical only", color="high"), size=1) +
  stat_function(fun = function(x) wbuf(x,0, a=0.5, b=1)/wmax, aes(linetype = "Horizontal only", color="mid"), size=1) +
  stat_function(fun = function(x) wbuf(0,x, a=0.5, b=1)/wmax, aes(linetype = "Vertical only", color="mid"), size=1) +
  stat_function(fun = function(x) wbuf(x,0, a=0.1, b=0.2)/wmax, aes(linetype = "Horizontal only", color="low"), size=1) +
  stat_function(fun = function(x) wbuf(0,x, a=0.1, b=0.2)/wmax, aes(linetype = "Vertical only", color="low"), size=1) +
  scale_x_continuous('Physical buffer distance (ft)', expand = c(0,0)) +
  scale_y_continuous(expression('Effective buffer ratio'~(W[buf]^"*" / W[max])), labels = scales::percent, expand = c(0,0)) +
  scale_linetype(NULL) +
  scale_color_brewer(NULL, palette = 'Dark2', limits = c('high','mid','low'),
                     labels = c('high' = expression('High:'~alpha==1.5*","~beta==3),
                                'mid' = expression('Mid:'~alpha==0.5*","~beta==1),
                                'low' = expression('Low:'~alpha==0.1*","~beta==0.2))) +
  coord_fixed(ratio = 12, xlim = c(0, 12), ylim = c(0, 1)) +
  theme_classic() +
  theme(text = element_text(family = "Times New Roman"), 
        legend.position = c(0.75,0.2),
        legend.margin = margin(-2.5,0,0,0, unit="pt"),
        legend.key.height = unit(1, "pt"),
        legend.text.align = 0, 
        legend.background = element_rect(colour = "transparent", fill = "transparent"),
        legend.key = element_rect(fill = "transparent", colour = "transparent"))
# plot.bufferform

#### BUFFER FUNC DENSITY FORM ####
binsize = 0.1
plot.bufferdensity <- ggplot(data, aes(x, y, z=wbuf/wmax)) +
  geom_contour_filled(binwidth = binsize) +
  geom_contour(binwidth=binsize, aes(color=..level..), size=0.05) + 
  # geom_dl(binwidth=binsize, aes(label = scales::percent(..level..)),
  #         stat = "contour", color = "black",
  #         method = list("last.points", vjust=-0.5, cex=0.7,
  #                       fontfamily="Times New Roman")) +
  scale_color_continuous(low="black", high="black", guide=F) +
  scale_x_continuous('Horizontal buffer distance (ft)', expand = c(0,0)) +
  scale_y_continuous('Vertical buffer distance (ft)', expand = c(0,0)) +
  scale_fill_manual(expression('Effective buffer,'~W[buf]^"*"),
                    values = colorRampPalette(colors=c("#000000", "#FFFFFF"))(1/binsize),
                    labels = binlabel(seq(0,1,by=binsize))) +
  coord_fixed(xlim = c(0, 5), ylim = c(0, 3)) +
  theme_bw() +
  theme(text = element_text(family = "Times New Roman"),
        legend.key = element_rect(fill = 'transparent', color='black'),
        legend.position = "bottom",
        legend.margin = margin(-5,0,0,0, unit="pt"),
        legend.text.align = 0,
        legend.text=element_text(size=10, angle = 90, hjust=-1, vjust=0.5)) +
  guides(fill = guide_legend(label.position = "bottom",
                             title.position = "top",
                             nrow=1, byrow=TRUE)) 
# plot.bufferdensity

#### CROSS SECTION LOS FACTOR ####
labs <- paste(sprintf('%.2f', seq(floor(min(data$Fw)),-1, length.out=8)),
              sprintf('%.2f', seq(ceiling(min(data$Fw)),0, length.out=8)), sep=" to ")

plot.LOSfactor <- ggplot(data, aes(x, y,  z = Fw)) + 
  geom_contour_filled(bins = 8) +
  scale_fill_brewer(expression("Cross-section\nadjustment factor, "~F[w]),
                    palette = "YlGnBu", direction = -1, drop = T) +
  scale_x_continuous(expression("Buffer width, "~italic(W[buf])~" (ft)"), expand = c(0,0)) +
  scale_y_continuous(expression("Buffer height, "~italic(H[buf])~" (ft)"), expand = c(0,0)) +
  coord_fixed(xlim = c(0,6), ylim = c(0,3)) +
  theme_bw() +
  theme(text = element_text(family = "Times New Roman"), legend.position = "right")
plot.LOSfactor


ggsave(filename = "C:\\Users\\nichf\\Desktop\\buffer_form.png", plot = plot.bufferform, width = 3.5, height = 3.5, units = "in")
ggsave(filename = "C:\\Users\\nichf\\Desktop\\buffer_density.png", plot = plot.bufferdensity, width = 3.5, height = 3.5, units = "in")
ggsave(filename = "C:\\Users\\nichf\\Desktop\\buffer_factor.png", plot = plot.LOSfactor, width = 7, height = 3, units = "in")


