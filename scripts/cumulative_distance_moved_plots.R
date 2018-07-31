#import data
dat2017 <- read.csv("data/cleaned_data_2017.csv", na="NA")
dat2016 <- read.csv("data/cleaned_data_2016.csv", na="NA")
dat2016 <- dat2016[!is.na(dat2016$treat_id),]
dat2016 <- dat2016[!is.na(dat2016$dist_sum),]

#subset data


#load libraries

library(ggplot2)
library(cowplot)

par(mfrow=c(2,1))
#create plot
ggplot(dat2017, aes(x=release_time, y=dist_sum, colour=treat_id, group=qbtag))+
  geom_line()+
  theme(panel.background=element_rect(fill=NA), panel.border = element_rect(colour = "black", fill=NA, size=1),
        legend.position="none", axis.text = element_text(size=18, colour="black"), axis.title=element_text(size=20)) +
  #facet_grid(treat_id~treat_fed) + 
  geom_hline(yintercept=c(100, 200, 300), linetype = "dashed") +
  #ggtitle("2017") + 
  labs(x="Time since release (hours)", y="Distance moved (ft)") +
  scale_color_manual(values=c("black", "black")) + 
  scale_y_continuous(breaks = seq(0, 300, 100), labels = c("0", "100", "200", "300"), limits=c(0,350)) +
  scale_x_continuous(breaks = seq(0, 600, 60), labels=c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10"))

#ggplot(dat2017, aes(x=release_time, y=dist_sum, colour=treat_fed, group=qbtag))+geom_line()+
  theme(panel.background=element_rect(fill=NA), panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  facet_grid(treat_fed~.) + geom_hline(yintercept=c(100, 200, 300), linetype = "dashed")+ ggtitle("Distance Moved Over Time 2017")

ggplot(dat2016, aes(x=release_time, y=dist_sum, colour=treat_id, group=qbtag))+
  geom_line()+
  theme(panel.background=element_rect(fill=NA), panel.border = element_rect(colour = "black", fill=NA, size=1),
        legend.position="none", axis.text = element_text(size=18, colour="black"), axis.title=element_text(size=20, colour="black")) +
  geom_hline(yintercept=c(100, 200, 300), linetype = "dashed") +
  #ggtitle("2016") +
  labs(x="Time Since Release (hours)", y="Distance Moved (ft)") +
  scale_color_manual(values=c("black", "black")) + 
  scale_y_continuous(breaks = seq(0, 300, 100), labels = c("0", "100", "200", "300"), limits=c(0, 350)) +
  scale_x_continuous(breaks = seq(0, 600, 60), labels=c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10"))

#' These graphs are useful to see how quickly the larvae move certain distances. It doesn't show anything I haven't
#' found so far, although it does kind of show it wouldn't take much longer than 10 hours for a lot of these larvae
#' to move further than 100 feet.  


#' Plot for publication

tiff(filename = "figures/cumdist.tiff", width = 3.2, height = 5, units = "in", type = "cairo", res = 600)

p1 <- ggplot(dat2017, aes(x=release_time, y=dist_sum, colour=treat_id, group=qbtag))+
  geom_line(size = 0.25, position = "jitter")+
  theme(panel.background=element_rect(fill=NA), panel.border = element_rect(colour = "black", fill=NA, size=1),
        legend.position="none", axis.text = element_text(size=10, colour="black"), axis.title=element_text(size=12)) +
  #facet_grid(treat_id~treat_fed) + 
  geom_hline(yintercept=c(100, 200, 300), linetype = "dashed") +
  #ggtitle("2017") + 
  labs(x="Time since release (hours)", y="Distance moved (ft)") +
  scale_color_manual(values=c("black", "black")) + 
  scale_y_continuous(breaks = seq(0, 300, 100), labels = c("0", "100", "200", "300"), limits=c(0,350)) +
  scale_x_continuous(breaks = seq(0, 600, 60), labels=c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10")) 
#ggsave(file="figures/cumdist2017.tiff", units="in", heigh=8, width=8, dpi=600) #save plot for poster

p2 <-ggplot(dat2016, aes(x=release_time, y=dist_sum, colour=treat_id, group=qbtag))+
  geom_line(size = 0.25, position = "jitter")+
  theme(panel.background=element_rect(fill=NA), panel.border = element_rect(colour = "black", fill=NA, size=1),
        legend.position="none", axis.text = element_text(size=10, colour="black"), axis.title=element_text(size=12, colour="black")) +
  geom_hline(yintercept=c(100, 200, 300), linetype = "dashed") +
  #ggtitle("2016") +
  labs(x="Time Since release (hours)", y="Distance moved (ft)") +
  scale_color_manual(values=c("black", "black")) + 
  scale_y_continuous(breaks = seq(0, 300, 100), labels = c("0", "100", "200", "300"), limits=c(0, 350)) +
  scale_x_continuous(breaks = seq(0, 600, 60), labels=c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10"))
#ggsave(file="figures/cumdist2016.tiff", units="in", heigh=8, width=8, dpi=600) #save plot for poster

plot_grid(p2, p1, ncol= 1, align="v", labels="AUTO")
ggsave(file = "figures/cumdist.tiff", units = "in", height = 5, width = 3.2, dpi = 1200)
dev.off()


#' Spun with ezspin("scripts/cumulative_distance_moved_plots.R", out_dir="spin_output", fig_dir="figures", keep_md=F)
#' 