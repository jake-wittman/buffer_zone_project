#' This script is to compare distances moved between different treatments (absence vs presence, 
#' fed vs starved). There is also code for making histograms.

rm(list=ls())
#'Library
library(car)
library(ggplot2)
library(nlme)
library(knitr)
library(ezknitr)
library(effects)
library(cowplot)

options(width=150)
#' Import data
dat2016 <-read.csv("data/summary_data_2016.csv", na="NA")
dat2017 <-read.csv("data/summary_data_2017.csv", na="NA")
op <- par()

#' ### Distance parameters
ggplot(data=dat2016, aes(m_total_path)) + geom_histogram(fill="white", colour="black") + facet_grid(treat_id ~ .) +
  theme_bw() + theme(panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) + ggtitle("Total Path Distance") +
  labs(x="Total Path Distance (m)", y="# of Larvae")

ggplot(data=dat2016, aes(m_displacement)) + geom_histogram(fill="white", colour="black") + facet_grid(treat_id ~ .) +
  theme_bw() + theme(panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) + ggtitle("Displacement") +
  labs(x="Displacement (m)", y="# of Larvae")

ggplot(data=dat2016, aes(mvmt_index)) + geom_histogram(fill="white", colour="black", breaks=seq(0,16,0.5), closed="left")+
  facet_grid(treat_id ~ .) +
  theme_bw() + theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(), 
                     axis.line = element_line(colour = "black")) +
  scale_x_continuous(breaks=seq(0,16,1)) + ggtitle("Tortuosity") + labs(x="Tortuosity", y="# of Larvae")

ggplot(data=dat2016, aes(x=treat_id, y=mvmt_index)) + geom_boxplot() +  
  theme_bw() + theme(panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) + ggtitle("Tortuosity by Treatment") +
  labs(x="Vegetation Treatment", y="Tortuosity")

ggplot(data=dat2016, aes(alt_index)) + geom_histogram(fill="white", colour="black", breaks=seq(0,1,0.05)) + facet_grid(treat_id ~ .) +
  theme_bw() + theme(panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  ggtitle("Tortuosity (reciprocal)") + labs(x="Tortuosity (reciprocal)", y="# of Larvae")

#' There are very few non-0s in the Presence factor (this will be the same for displacement and movement index).It looks like
#' the majority of movement index values are less than 2.5. Distance and displacement look pretty similar. Interesting
#' that displacement is very rarely > 35. 

flm_2016_totdist1 <- lm(m_total_path ~ treat_id, data=dat2016)
par(mfrow=c(2,2))
plot(flm_2016_totdist1)


flm_2016_totdist3 <- lm(sqrt(m_total_path+1) ~ treat_id, data=dat2016)
par(mfrow=c(2,2))
plot(flm_2016_totdist3)

#' The residuals are not super normally distributed and the variance is a bit wonky. A sqrt transformation
#' could work, but Brian suggested doing a binomial GLM with 0 = no movement, 1 = movement and
#' seeing if there is a difference in treatments.  






#' ## Probability of moving 2016

#' Make a new variable for movement
dat2016$move <- ifelse(dat2016$m_total_path > 0, 1, 0) #create a binary variable for moving: did the larva move or not
dat2016$treat_veg <- ifelse(dat2016$treat_id=="Absence", 0, 1)
#' Binomial GLM - probability of moving
#' Pr(moving) ~ Binomial(p), $logit(p_i) = \alpha + \beta*vegetation$

glm_movement_2016 <- glm(move ~ treat_veg, data=dat2016, family="binomial")
par(mfrow=c(2,2))
plot(glm_movement_2016)
summary(glm_movement_2016)
Anova(glm_movement_2016)
plogis(glm_movement_2016$coefficients[1] + 
         glm_movement_2016$coefficients[2]*unique(dat2016$treat_veg))

#' There is a significant difference in the probability of moving if there is food available.
#' When food is not available, the probabillity of moving is 0.98. If there is food (larvae were placed
#' in a tree) the probability of moving is 0.26. It's important to note that movement happened  in 2016 mostly
#' becuase the larvae were blown out of a tree.  










#' ### ANOVA comparing distance moved and displacement
#' Once we account for non-movers, we can compare distance moved and displacement between the two groups.
dat2016_movers <- subset(dat2016, move==1) #only use data from larvae that moved
lm1 <- lm(m_total_path ~ treat_id, data=dat2016_movers)
par(mfrow=c(2,2))
plot(lm1)
sqrtlm1 <- lm(sqrt(m_total_path) ~ treat_id, data=dat2016_movers)
plot(sqrtlm1)
#' The square root transformed residuals look better. Let's use them.
plot(effect("treat_id", sqrtlm1), type="response")
summary(sqrtlm1)
Anova(sqrtlm1)
#' There is a significant difference between the two vegetation treatments in terms of total distance moved.


lm2 <- lm(m_displacement ~ treat_id, data=dat2016_movers)
par(mfrow=c(2,2))
plot(lm2)
sqrtlm2 <- lm(sqrt(m_displacement) ~ treat_id, data=dat2016_movers)
plot(sqrtlm2)
#' The square root transformed residuals look better.
plot(effect("treat_id", sqrtlm2), type="response")
summary(sqrtlm2)
Anova(sqrtlm2)
#' There is a significant difference between the two vegetation treatments in terms of displacement.  

#' We don't need to account for the non-movers with movement index, as their values are NA here when movement is 0.
lm3 <- lm(mvmt_index ~ treat_id, data=dat2016)
par(mfrow=c(2,2))
plot(lm3)
#' These residuals don't look bad, plus the sqrt and log transformations make them worse.
plot(effect("treat_id", lm3), type="response")
summary(lm3)
Anova(lm3)

#' According to this, there is no difference in the movement indeces between vegetation treatments. There shouldn't
#' be either for the inverse of the ratio, but I'm checking anyway. Use a binomial GLM for the alt-index as it is a proportion.



#' To do a binomial regression with the tortuosity alt index, I have to first convert the distance measurements used to c alculate
#' the ratio to integer units (so convert from feet to cm).
dat2016$cm_total_path <- round(dat2016$m_total_path,2) * 100 #first multiply the number of feet moved
#times how many meters are in a foot (0.3048). Round this to 2 digits so when you multiply by 100 to get the number of cm, it is an integer
dat2016$cm_displacement <- round(dat2016$m_displacement,2) * 100 #same as above
response2016 <- cbind(dat2016$cm_displacement, dat2016$cm_total_path) #need to bind together the two columns used to generate the proprotion (successes/failures)
treat_veg2016 <- as.factor(dat2016$treat_veg)
lm4 <- glm(response2016 ~ treat_veg2016, family=binomial(link=logit))
par(mfrow=c(2,2))
plot(lm4) #check assumptions
#Residuals aren't great, definitely a few outliers. Not really sure how to deal with them though

par(op)
plot(alt_index ~ treat_veg, data=dat2016) #visualize model
lines(dat2016$treat_veg, predict(lm4, type="response")) #add fitted line
plot(effect("treat_veg2016", lm4), type="response") #another visualization
summary(effect("treat_veg2016", lm4)) #gives probability and CI
summary(lm4)
Anova(lm4)

#' According to this, there is a significant difference in the tortuositys between the presence and absence groups in 2016.

#' ### Bar plots for ANOVA

mean_avg_dist2016 <- as.data.frame(as.list(aggregate(m_total_path ~ treat_id, data=dat2016_movers, 
                                                     FUN = function(x) c(mean = mean(x), n = length(x), se = sd(x)/sqrt(length(x))))))
colnames(mean_avg_dist2016) <- c("treat_id", "mean", "n", "se")

# 2016 plot ---------------------------------------------------------------



p1<-ggplot(mean_avg_dist2016, aes(x=treat_id, y=mean)) +
  geom_col(fill="white", color="black") +
  geom_errorbar(aes(x = treat_id, ymin = mean - se, ymax=mean + se), width=.2) +
  theme(panel.background=element_rect(fill=NA), panel.border = element_rect(colour = "black", fill=NA, size=1),
        legend.position="none", axis.text = element_text(size=10, colour="black"), 
        axis.title=element_text(size=12)) +
  labs(y=expression(paste("Mean (" %+-% " SE) path distance (ft)")), x="Host vegetation") +
  scale_x_discrete(labels=c("Absent", "Present")) + 
  scale_y_continuous(limits=c(0,31.5), breaks=seq(0, 30, 3),
                     labels=c("0", "10", "20", "30", "40", "50", "60", "70", "80", "90", "100"),
                     expand=c(0,0))
    #+
  #annotate("text", x=2, y=28.5, label="F['1,62'] == 14.67", parse=T, size=5) +
  #annotate("text", x=2, y=26, label="p < 0.0001", size=5) #+
#theme(plot.margin=unit(c(0.5, 0.5, 0.5, 0.5), "in")) #for plotting for poster

median_tort2016 <- aggregate(alt_index ~ treat_id, data=dat2016, FUN=function(x) median(x, na.rm=T))
#ggsave(file="figures/anova2016.tiff", dpi=600, width = 8, height = 8, units="in") #to save image

#############################################################################################################################
#
#                         2017
#
#####################################################################################################################



#' ### Probability of moving 2017
#' There were two treatments this year: presence/absence of tree, and fed/starved. A subanalysis could be done as well
#' looking at the actual movement parameters.
#Data cleaning
dat2017$move <- ifelse(dat2017$m_total_path > 0, 1, 0) #create a binary variable: did the larva move or not?
dat2017$treat_veg <- ifelse(dat2017$treat_id=="Absence", 0, 1) #dummy indicator for factor
dat2017$treat_fed_num <- ifelse(dat2017$treat_fed=="Starved",0,1) #dummy indicator for factor
dat2017$instar <- as.factor(dat2017$instar)
dat2017$instar_num <- ifelse(dat2017$instar == 5, 0, 1) #dummy indicator for instar factor
dat2017 <- dat2017[!is.na(dat2017$treat_id),] #remove NAs for plotting

#' Looking at the data
ggplot(data=dat2017, aes(m_total_path)) + geom_histogram(fill="white", colour="black") + facet_grid(treat_id ~ .) +
  theme_bw() + theme(panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) + 
  ggtitle("Total Path Distance by Vegetation Treatment") + labs(x="Total Path Distance (m)", y="# of Larvae")

ggplot(data=dat2017, aes(m_total_path)) + geom_histogram(fill="white", colour="black") + facet_grid(treat_fed ~ .) +
  theme_bw() + theme(panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  ggtitle("Total Path Distance by Feeding Treatment") + labs(x="Total Path Distance (m)", y="# of Larvae")

ggplot(data=dat2017, aes(m_total_path)) + geom_histogram(fill="white", colour="black") + facet_grid(treat_id ~ treat_fed) +
  theme_bw() + theme(panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  ggtitle("Total Path Distance by Feeding and Vegetation Treatments") + labs(x="Total Path Distance (m)", y="# of Larvae")

ggplot(data=dat2017, aes(m_displacement)) + geom_histogram(fill="white", colour="black") + facet_grid(treat_id ~ .) +
  theme_bw() + theme(panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  ggtitle("Displacement by Vegetation Treatment") + labs(x="Displacement(m)", y="# of Larvae")

ggplot(data=dat2017, aes(m_displacement)) + geom_histogram(fill="white", colour="black") + facet_grid(treat_fed ~ .) +
  theme_bw() + theme(panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  ggtitle("Displacement by Feeding  Treatment") + labs(x="Displacement(m)", y="# of Larvae")

ggplot(data=dat2017, aes(m_displacement)) + geom_histogram(fill="white", colour="black") + facet_grid(treat_id ~ treat_fed) +
  theme_bw() + theme(panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  ggtitle("Displacement by Feeding and Vegetation Treatments") + labs(x="Displacement(m)", y="# of Larvae")

ggplot(data=dat2017, aes(mvmt_index)) + geom_histogram(fill="white", colour="black", breaks=seq(0,11,0.5), closed="left")+
  facet_grid(treat_id ~ treat_fed) +
  theme_bw() + theme(panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) + 
  ggtitle("Tortuosity by Feeding and Vegetation Treatments") + labs(x="Tortuosity", y="# of Larvae")

ggplot(data=dat2017, aes(alt_index)) + geom_histogram(fill="white", colour="black", breaks=seq(0,1,0.05)) + facet_grid(treat_id ~ .) +
  theme_bw() + theme(panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  ggtitle("Tortuosity (reciprocal) by Vegetation Treatment") + labs(x="Tortuosity (reciprocal)", y="# of Larvae")

ggplot(data=dat2017, aes(alt_index)) + geom_histogram(fill="white", colour="black", breaks=seq(0,1,0.05)) + facet_grid(treat_fed ~ .) +
  theme_bw() + theme(panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  ggtitle("Tortuosity (reciprocal) by Feeding Treatment") + labs(x="Tortuosity (reciprocal)", y="# of Larvae")

ggplot(data=dat2017, aes(alt_index)) + geom_histogram(fill="white", colour="black", breaks=seq(0,1,0.05)) + facet_grid(treat_id ~ treat_fed) +
  theme_bw() + theme(panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  ggtitle("Tortuosity (reciprocal) by Vegetation and Feeding Treatments") + labs(x="Tortuosity (reciprocal)", y="# of Larvae")

ggplot(data=dat2017, aes(x=treat_fed, y=alt_index)) + geom_boxplot() + facet_grid(. ~ treat_id) +
  theme_bw() + theme(panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  ggtitle("Tortuosity (reciprocal) by Vegetation and Feeding Treatments") + labs(x="Tortuosity (reciprocal)", y="# of Larvae")


#' Percentage of larvae moving greater than x feet in 2017
nrow(dat2017[dat2017$total_path_distance > 100,])/nrow(dat2017)
nrow(dat2017[dat2017$total_path_distance > 100,])
nrow(dat2017[dat2017$total_path_distance > 200,])/nrow(dat2017)
nrow(dat2017[dat2017$total_path_distance > 200,])
nrow(dat2017[dat2017$total_path_distance > 300,])/nrow(dat2017)
nrow(dat2017[dat2017$total_path_distance > 300,])

#' It looks like there is not really a difference in the fed and starved treatments, the difference is primarily
#' in the absence/presence of the host vegetation.We'll start with just looking at probability of movement for these treatments.
#' The model for this analysis is Pr(movement) ~ Binom(p), $p_i = \alpha + \beta_1*vegetation + \beta_2*fed + \beta_3*fed*vegetation
#' + \beta_4*instar$

glm_movement_2017 <- glm(move ~ treat_veg, data=dat2017, family="binomial")
par(mfrow=c(2,2))
plot(glm_movement_2017)
summary(glm_movement_2017)
Anova(glm_movement_2017)
unique(plogis(glm_movement_2017$coefficients[1] + glm_movement_2017$coefficients[2]*dat2017$treat_veg))
#glm_movement_2017$coefficients[3]*dat2017$treat_fed_num)) #+
#glm_movement_2017$coefficients[4]*dat2017$treat_fed_num*dat2017$treat_veg ))

#' There does not appear to be an interaction between presence/absence of vegetation and starvation, and there is no
#' difference between fed and starved larvae for the probability of moving. The only difference is in the probability of moving
#' depending on if they were placed in a tree or not. There is no effect of instar on the probability of moving.

#' ### Anova comparing distance moved and displacement. 
#' Need to subset and remove the non-movers
dat2017_movers <- subset(dat2017, move==1) #only use data from larvae that moved
dat2017_movers$instar <- as.factor(dat2017_movers$instar)
lm5 <- lm(m_total_path ~ treat_id*treat_fed + instar, data=dat2017_movers)
par(mfrow=c(2,2))
plot(lm5)
sqrtlm5 <- lm(sqrt(m_total_path) ~ treat_id*treat_fed, data=dat2017_movers)
plot(sqrtlm5)
#' Residuals look better for the sqrt transformation.
plot(effect("treat_id", sqrtlm5), type="response")
plot(effect("treat_fed", sqrtlm5), type="response")
plot(effect("instar", sqrtlm5), type="response")
plot(effect("treat_id*treat_fed", sqrtlm5), type="response")
plot(effect("treat_id*instar", sqrtlm5), type="response")
plot(effect("treat_fed*instar", sqrtlm5), type="response")
summary(sqrtlm5)
Anova(sqrtlm5)
#' Again, no difference in how far the larvae moved except for the presence/absence of host vegetation (treat_id) 
#' treatment and instar

lm6 <- lm(m_displacement ~ treat_id*treat_fed + instar, data=dat2017_movers)
par(mfrow=c(2,2))
plot(lm6)
sqrtlm6 <- lm(sqrt(m_displacement) ~ treat_id*treat_fed + instar, data=dat2017_movers)
par(mfrow=c(2,2))
plot(sqrtlm6)
#Residuals look better for the sqrt transformation
plot(effect("treat_id", sqrtlm6), type="response")
plot(effect("treat_fed", sqrtlm6), type="response")
plot(effect("instar", sqrtlm6), type="response")
plot(effect("treat_id*treat_fed", sqrtlm6), type="response")
plot(effect("treat_id*instar", sqrtlm6), type="response")
plot(effect("treat_fed*instar", sqrtlm6), type="response")
summary(sqrtlm6)
Anova(sqrtlm6)
#' Similar result, significant difference only for presence/absence of host vegetation and instar

#' Logistic regression comparing tortuosity. See above 2016 section for rationale.
dat2017$cm_total_path <- round(dat2017$m_total_path,2) * 100 #first multiply the number of feet moved
#times how many meters are in a foot (0.3048). Round this to 2 digits so when you multiply by 100 to get the number of cm, it is an integer
dat2017$cm_displacement <- round(dat2017$m_displacement,2) * 100 #same as above
response2017 <- cbind(dat2017$cm_displacement, dat2017$cm_total_path) #bind columns used to create proportion
treat_veg2017 <- as.factor(dat2017$treat_veg)
treat_fed2017 <- as.factor(as.integer(dat2017$treat_fed_num))
instar2017 <- as.factor(dat2017$instar)
lm7 <- glm(response2017 ~ treat_veg2017*treat_fed2017 + instar2017, family=binomial(link=logit))
par(mfrow=c(2,2))
plot(lm7)
#' Residuals aren't great, definitely a few outliers. Including instar drastically improves the residuals

#' Visualize the model
par(op)
plot(effect("treat_veg2017", lm7), type="response") #visualize model by vegetation treatment, holding feeding status constant
plot(effect("treat_fed2017", lm7), type="response") #visualize model by fed treatment, holding vegetation constant
plot(effect("instar2017", lm7), type="response")
plot(effect("treat_veg2017*treat_fed2017", lm7), type="response") #visualize all combinations
plot(effect("treat_fed2017*treat_veg2017", lm7), type="response") #visualize all combinations

#' Text output of results
summary(lm7)
Anova(lm7)
summary(effect("treat_veg2017", lm7))
summary(effect("treat_fed2017", lm7))
summary(effect("instar2017", lm7))
summary(effect("treat_veg2017*treat_fed2017", lm7))
summary(effect("treat_veg2017*treat_fed2017+instar2017", lm7))
#' So, all factors and combinations were significant. Vegetation, feeding status, and instar all seem to influence these
#' movement parameters.

#' ### Bar plots for ANOVA
mean_avg_dist2017 <- as.data.frame(as.list(aggregate(m_total_path ~ treat_id + treat_fed, data=dat2017_movers, 
                                                     FUN = function(x) c(mean = mean(x), n = length(x), se = sd(x)/sqrt(length(x))))))
colnames(mean_avg_dist2017) <- c("treat_id", "treat_fed", "mean", "n", "se")

# 2017 plot ---------------------------------------------------------------

mean_avg_dist2017$posthoc <- ifelse(mean_avg_dist2017$treat_id == "Absence", "a", "b")
p2 <- ggplot(mean_avg_dist2017, aes(x=treat_id, y=mean, fill=treat_fed)) +
  geom_bar(stat="identity", position=position_dodge(), colour="black") +
  geom_errorbar(aes(x = treat_id, ymin = mean - se, ymax=mean + se), width=.2, position=position_dodge(0.9)) +
  theme(panel.background=element_rect(fill=NA), panel.border = element_rect(colour = "black", fill=NA, size=1),
        axis.text = element_text(size=10, colour="black"), axis.title=element_text(size=12),
        legend.position = c(0.55, 0.6), legend.text=element_text(size=10), legend.title=element_text(size=12)) + 
  labs(y= expression(paste("Mean (" %+-% " SE) path distance (ft)")), x="Host vegetation") +
  scale_x_discrete(labels=c("Absent", "Present")) +
  scale_y_continuous(limits=c(0,31.5), breaks=seq(0, 30, 3), 
                     labels=c("0", "10", "20", "30", "40", "50", "60", "70", "80", "90", "100"),
                     expand = c(0, 0)) +
  scale_fill_manual(values=c("white", "grey"), name="Feeding status") +
  annotate("text", x = 0.775, y = 28.25, label = "a", size = 6, parse = T) +
  annotate("text", x = 1.225, y = 30.5, label = "a", size = 6, parse = T) +
  annotate("text", x = 1.775, y = 9.1, label = "b", size = 6, parse = T) +
  annotate("text", x = 2.225, y = 11.5, label = "b", size = 6, parse = T)
#+


  #annotate("text", x=2.2, y=28.5, label="F['1, 111'] == 110.34", parse=T, size=7) +
  #annotate("text", x=2.2, y=27, label="p < 0.0001", size=7) +
  #theme(plot.margin=unit(c(0.5, 0.5, 0.5, 0.5), "in")) #for poster plotting
plot_grid(p1, p2, nrow = 1, align="h", labels="AUTO")
ggsave(file="figures/anova.tiff", dpi=1200, width=6.7, height=4, units="in") #for saving the file
median_tort2017 <- aggregate(alt_index ~ treat_id + treat_fed, 
                             data=dat2017, FUN=function(x) median(x, na.rm=T))

plot_grid(p1, p2, align="h", labels="auto")
#' 
#' 
#' Spun with ezspin("scripts/anova_histograms.R", out_dir="spin_output", fig_dir="figures", keep_md=F)


# ANOVA presentation plot -------------------------------------------------

p3 <-ggplot(mean_avg_dist2016, aes(x=treat_id, y=mean)) +
  geom_col(fill="white", color="black") +
  geom_errorbar(aes(x = treat_id, ymin = mean - se, ymax=mean + se), width=.2) +
  theme(panel.background=element_rect(fill=NA), panel.border = element_rect(colour = "black", fill=NA, size=1),
        legend.position="none", axis.text = element_text(size=18, colour="black"), 
        axis.title=element_text(size=20)) +
  labs(y=expression(paste("Mean (" %+-% " SE) path distance (ft)")), x="Host vegetation") +
  scale_x_discrete(labels=c("Absent", "Present")) + 
  scale_y_continuous(limits=c(0,31.5), breaks=seq(0, 30, 3),
                     labels=c("0", "10", "20", "30", "40", "50", "60", "70", "80", "90", "100"),
                     expand=c(0,0)) +
 annotate("text", x=2, y=28.5, label="F['1,62'] == 14.67", parse=T, size=6) +
 annotate("text", x=2, y=26, label="p < 0.0001", size=6) +
 theme(plot.margin=unit(c(0, 0.5, 0, 0), "in")) #for plotting for poster


p4 <- ggplot(mean_avg_dist2017, aes(x=treat_id, y=mean, fill=treat_fed)) +
  geom_bar(stat="identity", position=position_dodge(), colour="black") +
  geom_errorbar(aes(x = treat_id, ymin = mean - se, ymax=mean + se), width=.2, position=position_dodge(0.9)) +
  theme(panel.background=element_rect(fill=NA), panel.border = element_rect(colour = "black", fill=NA, size=1),
        axis.text = element_text(size=18, colour="black"), axis.title=element_text(size=18),
        legend.position = c(0.55, 0.6), legend.text=element_text(size=16), legend.title=element_text(size=18)) + 
  labs(y= expression(paste("Mean (" %+-% " SE) path distance (ft)")), x="Host vegetation") +
  scale_x_discrete(labels=c("Absent", "Present")) +
  scale_y_continuous(limits=c(0,31.5), breaks=seq(0, 30, 3), 
                     labels=c("0", "10", "20", "30", "40", "50", "60", "70", "80", "90", "100"),
                     expand = c(0, 0)) +
  scale_fill_manual(values=c("white", "grey"), name="Feeding status") +
  annotate("text", x = 0.775, y = 28.25, label = "a", size = 6, parse = T) +
  annotate("text", x = 1.225, y = 30.5, label = "a", size = 6, parse = T) +
  annotate("text", x = 1.775, y = 9.1, label = "b", size = 6, parse = T) +
  annotate("text", x = 2.225, y = 11.5, label = "b", size = 6, parse = T) +
  annotate("text", x=2.2, y=28.5, label="F['1, 111'] == 110.34", parse=T, size=6) +
  annotate("text", x=2.2, y=26, label="p < 0.0001", size=6) +
  theme(plot.margin=unit(c(0, 0.5, 0, 0), "in")) #for poster plotting

ggsave(plot = p3, file="figures/anovapres2016.tiff", dpi=1200, width=5, height=6.4, units="in") #for saving the file
ggsave(plot = p4, file="figures/anovapres2017.tiff", dpi=1200, width=5, height=6.4, units="in") #for saving the file

