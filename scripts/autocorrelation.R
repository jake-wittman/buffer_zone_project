#Clear list
rm(list=ls())

#libraries
library(ggplot2)
library(plyr)
library(knitr)
library(ezknitr)

#Custom functions
mean_NA <- function(x){ #Take means without getting errors due to NAs
  val_out <-mean(x,na.rm=T)
  return(val_out)
}

sum_NA <- function(x){
  val_out <- sum(x, na.rm=T)
  return(val_out)
}

sem <- function(x){
  val_out <- sd(x, na.rm=T)/sqrt(length(x))
  return(val_out)
}

r <- function(x){
  val_out <- x*(pi/180)
  return(val_out)
}

se <- function(x){
  val_out <- sd(x, na.rm=T)/length(which(!is.na(x)))
  return(val_out)
}

#' Import data
data <- read.csv("data/cleaned_data_2017.csv", na.strings="NA")

#' Data cleaning
data$x_m <- as.numeric(data$x_m)
data$y_m <- as.numeric(data$y_m)
data <- data[data$qbtag != 30.1, ]  #Larva 30.1 had only 1 observation so cannot be used for this
data <- data[data$qbtag != 43, ]  #Larva 43 had only 1 observation so cannot be used for this
data <- data[data$qbtag != 59, ]  #Larva 59 had only 1 observation so cannot be used for this
data <- data[data$qbtag != 37, ]  #Larva 37 had only 1 observation so cannot be used for this
data <- data[data$qbtag != 44.2, ]  #Larva 44.2 had only 1 observation so cannot be used for this
#' Calculate average autocorrelation for moving larvae
data <- data[data$treat_id == "Absence",]
data <- data[data$open_env == 0,]
my_tags <- unique(data$qbtag)
auto_cor_dist <- as.data.frame(matrix(NA, nrow=length(my_tags), ncol=20))

colnames(auto_cor_dist) <- c("lag0", "lag1", "lag2", "lag3", "lag4", "lag5", "lag6", "lag7",
                             "lag8", "lag9", "lag10", "lag11", "lag12", "lag13", "lag14",
                             "lag15", "lag16", "lag17", "lag18", "qbtag")

auto_cor_dist$qbtag <- my_tags
auto_cor_ta$qbtag <- my_tags

for(i in my_tags){
  
  dat_temp <- data[data$qbtag==i,] #seperate data set into individual data frames for each larvae
  
  if(nrow(dat_temp) >= 3 ){ #can't calculate ACF for data sets with less than 3 observations.
    
    acf_dist <- acf(dat_temp$m_segment_dist, na.action=na.pass, plot=F) #calculate ACF for step distance
    acf_ta <- acf(dat_temp$ta, na.action = na.pass, plot=F) #calculate ACF for turn angle
    
    for(j in 1:length(acf_dist$acf)){
      
     auto_cor_dist[auto_cor_dist$qbtag == i,j] <- acf_dist$acf[j]
     
   }
   
  }
  
else{}
}

acf_dist_mean <- colMeans(auto_cor_dist[,1:19], na.rm=T) #get mean ACF for each step distance
acf_dist_sd <- apply(auto_cor_dist[,1:19], 2, sd, na.rm=T) #get SD for each average ACF
acf_dist_se <- apply(auto_cor_dist[,1:19], 2, se)


#need to remove NA values to plot
acf_dist_mean <- acf_dist_mean[!is.na(acf_dist_mean)]
acf_dist_sd <- acf_dist_sd[!is.na(acf_dist_sd)]
acf_dist_se <- acf_dist

acf_dist <- as.data.frame(cbind(acf_dist_mean, acf_dist_sd,
                                1:length(which(!is.na(acf_dist_mean)==T)))) #make data frame for graphing
colnames(acf_dist) <- c("mean", "sd", "lag")

colnames(acf_ta) <- c("mean", "sd", "lag")

#' To get confidence intervals for a correlation value, use a Fisher transfomartion. Transform
#' the correlation coeffecient to z then plus/minus 1.96 (95% CI) * the SE (sqrt(1/n-3)). Then
#' back transform those values back.

counts_dist <- apply(auto_cor_dist[,1:14], 2, function(x) length(which(!is.na(x))))
sd_dist<-apply(auto_cor_dist[,1:14], 2, sd, na.rm=T)
sigma_dist <- sqrt(1/(counts_dist-3))
lag_dist <- seq(0, 13, 1)
z_dist <- 0.5*log((1+acf_dist_mean)/(1-acf_dist_mean))
l_z_dist <- z_dist - (1.96*sigma_dist)
u_z_dist <- z_dist + (1.96*sigma_dist)
l_CI_dist <- ((exp(2*l_z_dist)-1)/(exp(2*l_z_dist)+1))
u_CI_dist <- ((exp(2*u_z_dist)-1)/(exp(2*u_z_dist)+1))
dist_cor <- as.data.frame(cbind(acf_dist_mean, lag_dist, l_CI_dist, u_CI_dist))

ggplot(dist_cor, aes(y=acf_dist_mean, x=lag_dist)) + #plot mean ACF results
  geom_point() +
  #ggtitle("ACF DIST") +
  geom_errorbar(aes(ymin=l_CI_dist, ymax=u_CI_dist)) +
  labs(x="Lag", y="Average Autocorrelation") +
  theme(panel.background=element_rect(fill=NA), panel.border = element_rect(colour = "black", fill=NA, size=1),
        legend.position="none", axis.text = element_text(size=16, colour="black"), 
        axis.title=element_text(size=20)) +
  geom_hline(yintercept=0, linetype = "dashed") 




#' The below code does what I did above but for pacf.
pauto_cor_dist <- as.data.frame(matrix(NA, nrow=length(my_tags), ncol=20))

colnames(pauto_cor_dist) <- c("lag0", "lag1", "lag2", "lag3", "lag4", "lag5", "lag6", "lag7",
                             "lag8", "lag9", "lag10", "lag11", "lag12", "lag13", "lag14",
                             "lag15", "lag16", "lag17", "lag18", "qbtag")

pauto_cor_dist$qbtag <- my_tags


for(i in unique(data$qbtag)){
  
  dat_temp <- data[data$qbtag==i,]
  
  if(nrow(dat_temp) >= 3 ){
    
    pacf_dist <- pacf(dat_temp$m_segment_dist, na.action=na.pass, plot=F)
    pacf_ta <- pacf(dat_temp$ta, na.action = na.pass, plot=F)
    
    for(j in 1:length(pacf_dist$acf)){
      
      pauto_cor_dist[pauto_cor_dist$qbtag == i,j] <- pacf_dist$acf[j]
      
    }
   
  }
  else{}
}
pacf_dist_mean <- colMeans(pauto_cor_dist[,1:19], na.rm=T)
pacf_dist_sd <- apply(pauto_cor_dist[,1:19], 2, sd, na.rm=T) #get SD for each average PACF

#need to remove NA values to plot
pacf_dist_mean <- pacf_dist_mean[!is.na(pacf_dist_mean)]
pacf_dist_sd <- pacf_dist_sd[!is.na(pacf_dist_sd)]
pacf_dist <- as.data.frame(cbind(pacf_dist_mean, pacf_dist_sd,
                                1:length(which(!is.na(pacf_dist_mean)==T)))) #make data frame for graphing
colnames(pacf_dist) <- c("mean", "sd", "lag")

ggplot(pacf_dist, aes(y=mean, x=lag)) + geom_point() + 
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd)) + ggtitle("PACF Dist")#plot mean ACF results


#' Looking at these autocorrelations, it appears that there is a lot of variabliity in the auto-
#' correlation of step distances. The intervals overlap 0, so as a population it seems unlikely
#' that there is significant autocorrelation in move distance.  
#' You can't rely on these autocorrelation values for turning angle because turn angle is 
#' a circular variable. Turchin recommends looking at auto correlation in bearing, rather
#' than turn angle.
#' We'll calculate the average cos for the difference between the ith bearing
#' and the ith bearing + j, where j is the lag. (pg 252 Turchin)
dat_sub <- data[,c("qbtag","bearing")]
dat_sub$lag0 <- dat_sub$bearing - dat_sub$bearing
dat_sub$lag1 <- NA
dat_sub$lag2 <- NA
dat_sub$lag3 <- NA
dat_sub$lag4 <- NA
dat_sub$lag5 <- NA
dat_sub$lag6 <- NA
dat_sub$lag7 <- NA
dat_sub$lag8 <- NA
dat_sub$lag9 <- NA
dat_sub$lag10 <- NA
dat_sub$lag11 <- NA
dat_sub$lag12 <- NA
dat_sub$lag13 <- NA
dat_sub$lag14 <- NA
dat_sub$lag15 <- NA

for(j in 2:nrow(dat_sub)){
  if(dat_sub$qbtag[j]==dat_sub$qbtag[j-1]){
    dat_sub$lag1[j] <-dat_sub$bearing[j] - dat_sub$bearing[j-1]
  }
  else{
    dat_sub$lag1[j] <- NA
  }
}

for(j in 3:nrow(dat_sub)){
  if(dat_sub$qbtag[j]==dat_sub$qbtag[j-2]){
    dat_sub$lag2[j] <-dat_sub$bearing[j] - dat_sub$bearing[j-2]
  }
  else{
    dat_sub$lag2[j] <- NA
  }
}

for(j in 4:nrow(dat_sub)){
  if(dat_sub$qbtag[j]==dat_sub$qbtag[j-3]){
    dat_sub$lag3[j] <-dat_sub$bearing[j] - dat_sub$bearing[j-3]
  }
  else{
    dat_sub$lag3[j] <- NA
  }
}

for(j in 5:nrow(dat_sub)){
  if(dat_sub$qbtag[j]==dat_sub$qbtag[j-4]){
    dat_sub$lag4[j] <-dat_sub$bearing[j] - dat_sub$bearing[j-4]
  }
  else{
    dat_sub$lag4[j] <- NA
  }
}

for(j in 6:nrow(dat_sub)){
  if(dat_sub$qbtag[j]==dat_sub$qbtag[j-5]){
    dat_sub$lag5[j] <-dat_sub$bearing[j] - dat_sub$bearing[j-5]
  }
  else{
    dat_sub$lag5[j] <- NA
  }
}

for(j in 7:nrow(dat_sub)){
  if(dat_sub$qbtag[j]==dat_sub$qbtag[j-6]){
    dat_sub$lag6[j] <-dat_sub$bearing[j] - dat_sub$bearing[j-6]
  }
  else{
    dat_sub$lag6[j] <- NA
  }
}

for(j in 8:nrow(dat_sub)){
  if(dat_sub$qbtag[j]==dat_sub$qbtag[j-7]){
    dat_sub$lag7[j] <-dat_sub$bearing[j] - dat_sub$bearing[j-7]
  }
  else{
    dat_sub$lag7[j] <- NA
  }
}

for(j in 9:nrow(dat_sub)){
  if(dat_sub$qbtag[j]==dat_sub$qbtag[j-8]){
    dat_sub$lag8[j] <-dat_sub$bearing[j] - dat_sub$bearing[j-8]
  }
  else{
    dat_sub$lag8[j] <- NA
  }
}

for(j in 10:nrow(dat_sub)){
  if(dat_sub$qbtag[j]==dat_sub$qbtag[j-9]){
    dat_sub$lag9[j] <-dat_sub$bearing[j] - dat_sub$bearing[j-9]
  }
  else{
    dat_sub$lag9[j] <- NA
  }
}

for(j in 11:nrow(dat_sub)){
  if(dat_sub$qbtag[j]==dat_sub$qbtag[j-10]){
    dat_sub$lag10[j] <-dat_sub$bearing[j] - dat_sub$bearing[j-10]
  }
  else{
    dat_sub$lag10[j] <- NA
  }
}
  
for(j in 12:nrow(dat_sub)){
  if(dat_sub$qbtag[j]==dat_sub$qbtag[j-11]){
    dat_sub$lag11[j] <-dat_sub$bearing[j] - dat_sub$bearing[j-11]
  }
  else{
    dat_sub$lag11[j] <- NA
  }
}  
  
for(j in 13:nrow(dat_sub)){
  if(dat_sub$qbtag[j]==dat_sub$qbtag[j-12]){
    dat_sub$lag12[j] <-dat_sub$bearing[j] - dat_sub$bearing[j-12]
  }
  else{
    dat_sub$lag11[j] <- NA
  }
}

for(j in 14:nrow(dat_sub)){
  if(dat_sub$qbtag[j]==dat_sub$qbtag[j-13]){
    dat_sub$lag13[j] <-dat_sub$bearing[j] - dat_sub$bearing[j-13]
  }
  else{
    dat_sub$lag13[j] <- NA
  }
}

for(j in 15:nrow(dat_sub)){
  if(dat_sub$qbtag[j]==dat_sub$qbtag[j-14]){
    dat_sub$lag14[j] <-dat_sub$bearing[j] - dat_sub$bearing[j-14]
  }
  else{
    dat_sub$lag14[j] <- NA
  }
}

for(j in 16:nrow(dat_sub)){
  if(dat_sub$qbtag[j]==dat_sub$qbtag[j-15]){
    dat_sub$lag15[j] <-dat_sub$bearing[j] - dat_sub$bearing[j-15]
  }
  else{
    dat_sub$lag15[j] <- NA
  }
}

#' To get confidence intervals for a correlation value, use a Fisher transfomartion. Transform
#' the correlation coeffecient to z then plus/minus 1.96 (95% CI) * the SE (sqrt(1/n-3)). Then
#' back transform those values back.
avg_cor <- colMeans(cos(r(dat_sub[,3:18])), na.rm=T)
counts <- apply(dat_sub[,3:18], 2, function(x) length(which(!is.na(x))))
sd<-apply(dat_sub[,3:18], 2, sd, na.rm=T)
sigma <- sqrt(1/(counts-3))
lag <- seq(0, 15, 1)
z <- 0.5*log((1+avg_cor)/(1-avg_cor))
l_z <- z - (1.96*sigma)
u_z <- z + (1.96*sigma)
l_CI <- ((exp(2*l_z)-1)/(exp(2*l_z)+1))
u_CI <- ((exp(2*u_z)-1)/(exp(2*u_z)+1))
bearing_cor <- as.data.frame(cbind(avg_cor, lag, l_CI, u_CI))

ggplot(bearing_cor, aes(y=avg_cor, x=lag)) + #plot mean ACF results
  geom_point() + 
  geom_errorbar(aes(ymin=l_CI, ymax=u_CI)) +
  #ggtitle("ACF Bearing") +
  labs(x="Lag", y="Average autocorrelation in bearing") +
  theme(panel.background=element_rect(fill=NA),
        axis.line = element_line(colour = "black"),
        panel.border = element_blank(),
        legend.position="none",
        axis.text = element_text(size=10, colour="black"), 
        axis.title=element_text(size=12)) +
  geom_hline(yintercept=0, linetype = "dashed") +
  scale_y_continuous(limits=c(-1,1)) +
  scale_x_continuous(breaks=seq(1, 15, 2), labels=seq(1, 15, 2)) +
  coord_cartesian(xlim = c(0.8, 15.2))
ggsave("figures/autocorrelation.tiff", units = "in", height = 3.2, width = 3.2, dpi = 1200)
  
#' Spun with ezspin("scripts/autocorrelation.R", out_dir="spin_output", fig_dir="figures", keep_md=F)
#if(j == my_tags[1]){my_new_d <- dat_temp} else { my_new_d <- rbind.data.frame(my_new_d, dat_temp)} 