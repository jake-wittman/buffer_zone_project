#' Clear list
rm(list=ls())

#' Load libraries
library(circular)
library(knitr)
library(ezknitr)

#' Import data
disp_dat2017 <- read.csv("data/summary_data_2017.csv", na="NA") #displacement data 
dat2017 <- read.csv("data/cleaned_data_2017.csv", na="NA") #all movement data
dat2017 <- dat2017[!is.na(dat2017$bearing),] #need to remove NA for some plotting things
disp_dat2017 <- disp_dat2017[!is.na(disp_dat2017$displacement_bearing),]
disp_dat2017 <- disp_dat2017[!is.na(disp_dat2017$displacement),]
dat2017$time <- factor(dat2017$time, levels=c("8:00", "8:30", "9:00", "9:30", "10:00", "10:30",
                                              "11:00", "11:30", "12:00", "12:30", "13:00",
                                              "13:30", "14:00", "14:30", "15:00", "15:30",
                                              "16:00", "16:30", "17:00"))

dat2016 <- read.csv("data/cleaned_data_2016.csv", na="NA") #all movement data
disp_dat2016 <- read.csv("data/summary_data_2016.csv", na="NA")
dat2016 <- dat2016[!is.na(dat2016$bearing),] #need to remove NA for some plotting things
disp_dat2016 <- disp_dat2016[!is.na(disp_dat2016$displacement_bearing),]
disp_dat2016 <- disp_dat2016[!is.na(disp_dat2016$displacement),]
#' Set initial plotting parameters
default <- par(mfrow=c(1,1))

#################################################################################################
##################################2017 Plots#####################################################
#################################################################################################

#' Data cleaning
#sets bearing (degrees) of movement direction as circular data object, template sets 0 at positive vertical axis with clockwise rotation
dat2017$bearing <- circular(dat2017$bearing, type="angles", units="degrees", template="geographics")
disp_dat2017$displacement_bearing <-circular(disp_dat2017$displacement_bearing, type="angles", units="degrees", template="geographics")

#' Want to compare the average bearing for each larva at each starting location, curious to see how these compare to
#' displacement
for(j in unique(dat2017$qbtag)){
  dat_temp <- dat2017[dat2017$qbtag==j, "bearing"] #pull out the bearing data for each individual larva
  dat_temp <- circular(dat_temp, type="angles", units="degrees", template="geographics") #set as circular data type
  disp_dat2017$average_bearing[disp_dat2017$qbtag==j] <- mean(dat_temp, na.rm=T) #calculate mean direction
  disp_dat2017$rho_avg_bearing[disp_dat2017$qbtag==j] <- rho.circular(dat_temp) #calculate mean resultant length
}

disp_dat2017$average_bearing <- ifelse(disp_dat2017$average_bearing < 0, 
                                       disp_dat2017$average_bearing + 360, disp_dat2017$average_bearing + 0 )
disp_dat2017$average_bearing <-circular(disp_dat2017$average_bearing, type="angles", units="degrees", template="geographics")

#' Plot bearing for all data
plot(dat2017$bearing, cex = 0.8, bin = 72, stack = TRUE, sep = 0.045, shrink = 1.4, main = "Frequency of Direction of Travel, 2017")
axis.circular(at = circular(seq(pi/4,7*pi/4,pi/2)), zero = pi/2, rotation="clock", 
              labels = c("NE","NW","SW","SE"), cex = 1.1) #add intercardinal direction labels
ticks.circular(circular(seq(0,2*pi,pi/8)), zero=pi/2, rotation ='clock', tcl=0.075) #add more ticks
rose.diag(dat2017$bearing, bins=16, col="darkgrey", cex=1.0, prop=1.8, add=TRUE) #add rose diagram 
lines(density.circular(dat2017$bearing, bw = 40), lwd=2, lty=3) #add density kernel

#' This rose diagram shows that there seems to be a strong quad-modal distribution to the direction of travel
#' for the larvae I released, all in the intercardinal directions.

#' Make rose diagrams for each release point
for(i in unique(dat2017$starting_quad)){
  dat_temp <- dat2017[dat2017$starting_quad==i,] #subset data by starting quadrant
  plot(dat_temp$bearing, stack=T, bin=120, shrink=1.3) 
  title(paste("Bearing", toString(i), "Release 2017"),line=0)
  axis.circular(at = circular(seq(pi/4,7*pi/4,pi/2)), zero = pi/2, rotation="clock", 
                labels = c("NE","NW","SW","SE"), cex = 1.1) #add intercardinal direction labels
  ticks.circular(circular(seq(0,2*pi,pi/8)), zero=pi/2, rotation ='clock', tcl=0.075) #add extra acess ticks
  rose.diag(dat_temp$bearing, bins=24, col="darkgrey", cex=1.0, prop=1.8, add=TRUE) #add rose diagram
}
#' When broken down by release point, other patterns can be seen. The SW release point larvae
#' moved strongly in the NW and SE directions, with some moving NE and SW as well. The NW release
#' location were strongly bimodal, mostly NE and SW. There also appears to be a secondary peak SE and NW.
#' The NE release larvae almost all moved NW (towards the hill). The SE release larvae also moved strongly
#' NW, with a secondary peak NE and SE and a few moving SW.


#' Add mean and median arrows for each release point
rho2017 <- as.data.frame(matrix(NA, nrow=4, ncol=4)) #dataframe to save basic descriptive stats
colnames(rho2017) <- c("release_loc", "mean", "rho", "sd")
rho2017[,1] <- c("NE", "SE", "SW", "NW")

for(i in unique(dat2017$starting_quad)){ # same plots as above chunk but w/o rose diagram and with mean &/or median arrows
  dat_temp <- dat2017[dat2017$starting_quad==i,]
  rho2017[rho2017$release_loc ==i, 2] <- mean(dat_temp$bearing) #save mean angle of movement
  rho2017[rho2017$release_loc ==i, 3] <- rho.circular(dat_temp$bearing) #save measure of concentration for mean angle
  rho2017[rho2017$release_loc ==i, 4] <- sd.circular(dat_temp$bearing) #save SD for mean angle of movement
  plot(dat_temp$bearing, stack=T, bin=120, shrink=1.3)
  title(paste("Bearing", toString(i), "Release 2017"),line=0)
  axis.circular(at = circular(seq(pi/4,7*pi/4,pi/2)), zero = pi/2, rotation="clock", 
                labels = c("NE","NW","SW","SE"), cex = 1.1)
  ticks.circular(circular(seq(0,2*pi,pi/8)), zero=pi/2, rotation ='clock', tcl=0.075)
  #arrows.circular(median(dat_temp$bearing), lwd=3, lty=6)
  if(rho.circular(dat_temp$bearing) > 0.1){
  arrows.circular(mean(dat_temp$bearing), y=rho.circular(dat_temp$bearing), lwd=3)
  text(0,0.65, c("r =              ", format(round(rho.circular(dat_temp$bearing),2), nsmall = 2)))
    }
  else{}
}

rho2017
#' Sample variance for a circular variable is $1-r$, which constrains variance between 0 and 1.
#' Standard deviation for a circular variable is $(-2log(1-V))^(1/2) = (-2log(r)^(1/2)$. This 
#' parameter can take a value anywhere from 0 to positive infinity.  
#' The parameter of concentration, r, is pretty small for these, except the NE release. 

#' Rose diagrams for displacement - all and by release point
plot(disp_dat2017$displacement_bearing, cex = 0.8, bin = 72, stack = TRUE, 
    sep = 0.045, shrink = 1.1, main = "Bearing Displacement 2017")
axis.circular(at = circular(seq(pi/4,7*pi/4,pi/2)), zero = pi/2, rotation="clock", 
              labels = c("NE","NW","SW","SE"), cex = 1.1)
ticks.circular(circular(seq(0,2*pi,pi/8)), zero=pi/2, rotation ='clock', tcl=0.075)
rose.diag(disp_dat2017$displacement_bearing, bins=16, col="darkgrey", cex=1.0, prop=1.8, add=TRUE)
lines(density.circular(disp_dat2017$displacement_bearing, bw = 40), lwd=2, lty=3)


#' The displacement trends are similar what was seen earlier, with peaks in the NW, NE, SW, and SE.
#' I think the NW dominates so much in this graph because so many of the larvae at the NE and SE release
#' points ended their movement in the NW direction.
par(mfrow=c(2,2))
dat_temp_1 <- disp_dat2017[disp_dat2017$treat_id == "Absence",]
for(i in unique(disp_dat2017$starting_quad)){
  dat_temp <- dat_temp_1[dat_temp_1$starting_quad==i,]
  plot(dat_temp$displacement_bearing, stack=T, bin=120, shrink=1.1)
  title(paste("Bearing Displacement:", toString(i), "Release, 2017"),line=1)
  axis.circular(at = circular(seq(pi/4,7*pi/4,pi/2)), zero = pi/2, rotation="clock", 
                labels = c("NE","NW","SW","SE"), cex = 1.1)
  ticks.circular(circular(seq(0,2*pi,pi/8)), zero=pi/2, rotation ='clock', tcl=0.075)
  print(i)
  print(as.numeric(mean(dat_temp$displacement_bearing)))
  print(kuiper.test(dat_temp$displacement_bearing))
  print(nrow(dat_temp))
  #arrows.circular(median(dat_temp$bearing), lwd=3, lty=6)
  if(rho.circular(dat_temp$displacement_bearing) > 0.1){
    arrows.circular(mean(dat_temp$displacement_bearing), y=rho.circular(dat_temp$displacement_bearing), lwd=3)
    text(0,0.65, c("r =              ", format(round(rho.circular(dat_temp$displacement_bearing),2), nsmall = 2)))    }
  else{}
}
par(default)

#' There are not enough data points from the presence group to justify using them in
#' this analysis so I'll only use data from the absence group.

par(mfrow=c(2,2))

dat_temp_1 <- disp_dat2017[disp_dat2017$treat_id == "Absence",]
  for(i in unique(disp_dat2017$starting_quad)){
    dat_temp <- dat_temp_1[dat_temp_1$starting_quad==i,]
    plot(dat_temp$displacement_bearing, stack=T, bin=120, shrink=1.1)
    title(paste("Bearing Displacement:", toString(i), "Release, 2017"),line=1)
    axis.circular(at = circular(seq(pi/4,7*pi/4,pi/2)), zero = pi/2, rotation="clock", 
                  labels = c("NE","NW","SW","SE"), cex = 1.1)
    ticks.circular(circular(seq(0,2*pi,pi/8)), zero=pi/2, rotation ='clock', tcl=0.075)
    print(i)
    print(kuiper.test(dat_temp$displacement_bearing))
    #arrows.circular(median(dat_temp$bearing), lwd=3, lty=6)
  }


par(default)

#' These show similar trends as the all-data graphs above, albeit less strongly.

#' Rose diagrams for average bearing for each larvae, all together and based on release site
plot(disp_dat2017$average_bearing, cex = 0.8, bin = 72, stack = TRUE, 
     sep = 0.045, shrink = 1.1, main = "Average Bearing 2017")
axis.circular(at = circular(seq(pi/4,7*pi/4,pi/2)), zero = pi/2, rotation="clock", 
              labels = c("NE","NW","SW","SE"), cex = 1.1)
ticks.circular(circular(seq(0,2*pi,pi/8)), zero=pi/2, rotation ='clock', tcl=0.075)
rose.diag(disp_dat2017$average_bearing, bins=16, col="darkgrey", cex=1.0, prop=1.8, add=TRUE)
lines(density.circular(disp_dat2017$average_bearing, bw = 40), lwd=2, lty=3)
start_loc <- c("NW", "NE", "SW", "SE")

par(mfrow=c(2,2),
    oma = c(1,1,1,1) + 0.1,
    mar=c(0,0,1,1) + 0.1)
for(i in start_loc){
  dat_temp <- disp_dat2017[disp_dat2017$starting_quad==i,]
  plot(dat_temp$average_bearing, stack=T, bin=120, shrink=1)
  title(paste("Average Bearing:", toString(i), "Release 2017"),line=0.2)
  axis.circular(at = circular(seq(pi/4,7*pi/4,pi/2)), zero = pi/2, rotation="clock", 
                labels = c("NE","NW","SW","SE"), cex = 1.1)
  ticks.circular(circular(seq(0,2*pi,pi/8)), zero=pi/2, rotation ='clock', tcl=0.075)
  rose.diag(dat_temp$average_bearing, bins=24, col="darkgrey", cex=1.0, prop=1.1, add=TRUE)
}
plot(disp_dat2017[disp_dat2017$starting_quad=="SE","average_bearing"],
     stack=T, bin=120, shrink=1.1, cex=1.2)
title(paste("Average Bearing: SE Release 2017"), line=0.2)
axis.circular(at = circular(seq(pi/4, 7*pi/4, pi/2)), zero = pi/2, rotation="clock",
              labels = c("NE", "NW", "SW", "SE"), cex=1.1)
ticks.circular(circular(seq(0,2*pi,pi/8)), zero=pi/2, rotation="clock", tcl=0.075)
rose.diag(disp_dat2017[disp_dat2017$starting_quad=="SE","average_bearing"], bins=24, col="darkgrey", cex=1.0, prop=1.1, add=T)

for(i in start_loc){
  print(i)
  print(kuiper.test(disp_dat2017[disp_dat2017$starting_quad==i, "average_bearing"]))
}
par(default)
#' All the average bearings except NW are significantly different from being uniform according
#' to a kuiper test.
#' Average bearing doesn't appear that different from the bearing for the displacements
plot(as.numeric(disp_dat2017$displacement_bearing) ~ as.numeric(disp_dat2017$average_bearing))
#' There'sa pretty strong correlation between displacement bearing and average bearing.


#' Make rose diagrams for each sampling time.
for(i in sort(unique(dat2017$time))){
  dat_temp <- dat2017[dat2017$time==i,]
  plot(dat_temp$bearing, stack=T, bin=120, shrink=1.3)
  title(paste("Bearing at", toString(i), "2017"),line=1)
  axis.circular(at = circular(seq(pi/4,7*pi/4,pi/2)), zero = pi/2, rotation="clock", 
                labels = c("NE","NW","SW","SE"), cex = 1.1)
  ticks.circular(circular(seq(0,2*pi,pi/8)), zero=pi/2, rotation ='clock', tcl=0.075)
  rose.diag(dat_temp$bearing, bins=24, col="darkgrey", cex=1.0, prop=1.8, add=TRUE)
}
#' It's hard to say if there is any consistent pattern in these plots.

#' Throw in the calculations for rho and add some mean arrows too, might help see if there are patterns.
#' Will also perform Kuiper's test to test for differences from randomness. This is essentially a goodness-of-fit test
#' to see if a uniform distribution is a good fit for the data. It calculates deviations of my data from a uniform distribution
#' and uses those to find critical values.
kuiper2017 <- as.data.frame(matrix(NA, nrow=length(unique(dat2017$time)), ncol=3)) #Store Kuiper test results
colnames(kuiper2017) <- c("time", "test_stat", "n")
kuiper2017$time <- c("8:00", "8:30", "9:00", "9:30", "10:00", "10:30",
                     "11:00", "11:30", "12:00", "12:30", "13:00",
                     "13:30", "14:00", "14:30", "15:00", "15:30",
                     "16:00", "16:30", "17:00")
par(mfrow=c(2,2))
for(i in sort(unique(dat2017$time))){
  dat_temp <- dat2017[dat2017$time==i,]
  plot(dat_temp$bearing, stack=T, bin=120, shrink=1.3)
  title(paste("Bearing at", toString(i), "2017"),line=0)
  axis.circular(at = circular(seq(pi/4,7*pi/4,pi/2)), zero = pi/2, rotation="clock", 
                labels = c("NE","NW","SW","SE"), cex = 1.1)
  ticks.circular(circular(seq(0,2*pi,pi/8)), zero=pi/2, rotation ='clock', tcl=0.075)
  arrows.circular(mean(dat_temp$bearing), y=rho.circular(dat_temp$bearing), lwd=3)
  text(0,0.65, c("r =              ", format(round(rho.circular(dat_temp$bearing),2), nsmall = 2)))
  test_temp<-kuiper.test((dat_temp$bearing*(pi/180))) #data must be in radians for Kuiper test
  kuiper2017[kuiper2017$time==i,"test_stat"] <- test_temp$statistic
  kuiper2017[kuiper2017$time==i, "n"] <- length(!is.na(dat_temp$bearing))
  }
#' It doesn't seem like there is any pattern among the rho values. The Kuiper test rejects the null
#' hypothesis that the uniform distribution accurately describes these data at all sampling times. 
#' This rejection is at least at an alpha of 0.05 and often lower for most if the test statistics. 
#' Have to run the test individually (outside the loop) to get the p-value because the makers of the 
#' function didn't think it was important enough to store after running... Or check Batschelet 1981.







#################################################################################################
##################################2016 Plots#####################################################
#################################################################################################
#' ### Data cleaning
#sets bearing (degrees) of movement direction as circular data object, template sets 0 at positive vertical axis with clockwise rotation
dat2016$bearing <- circular(dat2016$bearing, type="angles", units="degrees", template="geographics")
disp_dat2016$displacement_bearing <-circular(disp_dat2016$displacement_bearing, type="angles", units="degrees", template="geographics")

#' ### Plot bearing for all data
plot(dat2016$bearing, cex = 0.8, bin = 72, stack = TRUE, sep = 0.045, shrink = 1.4, main = "Frequency of Direction of Travel, 2016")
axis.circular(at = circular(seq(pi/4,7*pi/4,pi/2)), zero = pi/2, rotation="clock", 
              labels = c("NE","NW","SW","SE"), cex = 1.1) #add intercardinal direction labels
ticks.circular(circular(seq(0,2*pi,pi/8)), zero=pi/2, rotation ='clock', tcl=0.075) #add more ticks
rose.diag(dat2016$bearing, bins=16, col="darkgrey", cex=1.0, prop=1.8, add=TRUE) #add rose diagram 
lines(density.circular(dat2016$bearing, bw = 40), lwd=2, lty=3) #add density kernel


#' This plot appears similar to the 2017 plot, with most movement appearing in the intercardinal directions 
#' (although not really the NE).  

#' Want to compare the average bearing for each larva at each starting location, curious to see how these compare to
#' displacement
for(j in unique(dat2016$qbtag)){
  dat_temp <- dat2016[dat2016$qbtag==j, "bearing"] #pull out the bearing data for each individual larva
  dat_temp <- circular(dat_temp, type="angles", units="degrees", template="geographics") #set as circular data type
  disp_dat2016$average_bearing[disp_dat2016$qbtag==j] <- mean(dat_temp, na.rm=T) #calculate mean direction
  disp_dat2016$rho_avg_bearing[disp_dat2016$qbtag==j] <- rho.circular(dat_temp) #calculate mean resultant length
}


disp_dat2016$average_bearing <- ifelse(disp_dat2016$average_bearing < 0, 
                                       disp_dat2016$average_bearing + 360, disp_dat2016$average_bearing + 0 )
disp_dat2016$average_bearing <-circular(disp_dat2016$average_bearing, type="angles", units="degrees", template="geographics")

#' ### Make rose diagrams for each release point
for(i in unique(dat2016$starting_quad)){
  dat_temp <- dat2016[dat2016$starting_quad==i,] #subset data by starting quadrant
  plot(dat_temp$bearing, stack=T, bin=120, shrink=1.3) 
  title(paste("Bearing", toString(i), "Release 2016"),line=1)
  axis.circular(at = circular(seq(pi/4,7*pi/4,pi/2)), zero = pi/2, rotation="clock", 
                labels = c("NE","NW","SW","SE"), cex = 1.1) #add intercardinal direction labels
  ticks.circular(circular(seq(0,2*pi,pi/8)), zero=pi/2, rotation ='clock', tcl=0.075) #add extra acess ticks
  rose.diag(dat_temp$bearing, bins=24, col="darkgrey", cex=1.0, prop=1.8, add=TRUE) #add rose diagram
}

#' Larvae at the SE release location moved mostly SW. Larvae at the SW location moved kind of 
#' all over the place. The NE release location has a lot of the larvae moving S-SE and NW-N. 
#' The NW release point has larvae moving mostly between SW and NW.


#' Add mean and median arrows for each release point
rho2016 <- as.data.frame(matrix(NA, nrow=4, ncol=4)) #dataframe to save basic descriptive stats
colnames(rho2016) <- c("release_loc", "mean", "rho", "sd")
rho2016[,1] <- c("NE", "SE", "SW", "NW")

for(i in unique(dat2016$starting_quad)){ # same plots as above chunk but w/o rose diagram and with mean &/or median arrows
  dat_temp <- dat2016[dat2016$starting_quad==i,]
  rho2016[rho2016$release_loc ==i, 2] <- mean(dat_temp$bearing)
  rho2016[rho2016$release_loc ==i, 3] <- rho.circular(dat_temp$bearing)
  rho2016[rho2016$release_loc ==i, 4] <- sd.circular(dat_temp$bearing)
  plot(dat_temp$bearing, stack=T, bin=120, shrink=1.3)
  title(paste("Bearing", toString(i), "Release 2016"),line=1)
  axis.circular(at = circular(seq(pi/4,7*pi/4,pi/2)), zero = pi/2, rotation="clock", 
                labels = c("NE","NW","SW","SE"), cex = 1.1)
  ticks.circular(circular(seq(0,2*pi,pi/8)), zero=pi/2, rotation ='clock', tcl=0.075)
  #arrows.circular(median(dat_temp$bearing), lwd=3, lty=6)
  if(rho.circular(dat_temp$bearing) > 0.1){
    arrows.circular(mean(dat_temp$bearing), y=rho.circular(dat_temp$bearing), lwd=3)
    text(0,0.65, c("r =              ", format(round(rho.circular(dat_temp$bearing),2), nsmall = 2)))    }
  else{}
}
rho2016

#' The measures of concentration here are not super strong




#' ### Rose diagrams for displacement - all and by release point
plot(disp_dat2016$displacement_bearing, cex = 0.8, bin = 72, stack = TRUE, 
     sep = 0.045, shrink = 1.1, main = "Bearing Displacement, 2016")
axis.circular(at = circular(seq(pi/4,7*pi/4,pi/2)), zero = pi/2, rotation="clock", 
              labels = c("NE","NW","SW","SE"), cex = 1.1)
ticks.circular(circular(seq(0,2*pi,pi/8)), zero=pi/2, rotation ='clock', tcl=0.075)
rose.diag(dat2016$bearing, bins=16, col="darkgrey", cex=1.0, prop=1.8, add=TRUE)
lines(density.circular(dat2016$bearing, bw = 40), lwd=2, lty=3)

#' The displacement plot here shows similar trends as the all-data plot.

par(mfrow=c(2,2))
dat_temp_1 <- disp_dat2016[disp_dat2016$treat_id == "Absence",]
for(i in unique(disp_dat2016$starting_quad)){
  dat_temp <- dat_temp_1[dat_temp_1$starting_quad==i,]
  plot(dat_temp$displacement_bearing, stack=T, bin=120, shrink=1.1)
  title(paste("Bearing Displacement:", toString(i), "Release 2016"),line=1)
  axis.circular(at = circular(seq(pi/4,7*pi/4,pi/2)), zero = pi/2, rotation="clock", 
                labels = c("NE","NW","SW","SE"), cex = 1.1)
  ticks.circular(circular(seq(0,2*pi,pi/8)), zero=pi/2, rotation ='clock', tcl=0.075)
  print(i)
  print(as.numeric(mean(dat_temp$displacement_bearing)))
  print(kuiper.test(dat_temp$displacement_bearing))
  
    #arrows.circular(median(dat_temp$bearing), lwd=3, lty=6)
  if(rho.circular(dat_temp$displacement_bearing) > 0.1){
    arrows.circular(mean(dat_temp$displacement_bearing), y=rho.circular(dat_temp$displacement_bearing), lwd=3)
    text(0,0.65, c("r =              ", format(round(rho.circular(dat_temp$displacement_bearing),2), nsmall = 2)))    }
  else{}
}
par(default)

print(kuiper.test(disp_dat2016$displacement_bearing))
#' These plots by release point do not show strong trends in any direction.



#' ### Rose diagrams for average bearing for each larvae, all together and based on release site
plot(disp_dat2016$average_bearing, cex = 0.8, bin = 72, stack = TRUE, 
     sep = 0.045, shrink = 1.1, main = "Average Bearing 2016")
axis.circular(at = circular(seq(pi/4,7*pi/4,pi/2)), zero = pi/2, rotation="clock", 
              labels = c("NE","NW","SW","SE"), cex = 1.1)
ticks.circular(circular(seq(0,2*pi,pi/8)), zero=pi/2, rotation ='clock', tcl=0.075)
rose.diag(disp_dat2016$average_bearing, bins=16, col="darkgrey", cex=1.0, prop=1.8, add=TRUE)
lines(density.circular(disp_dat2016$average_bearing, bw = 40), lwd=2, lty=3)

par(mfrow=c(2,2))
for(i in unique(disp_dat2016$starting_quad)){
  dat_temp <- disp_dat2016[disp_dat2016$starting_quad==i,]
  plot(dat_temp$average_bearing, stack=T, bin=120, shrink=1.1)
  title(paste("Average Bearing:", toString(i), "Release 2016"),line=1)
  axis.circular(at = circular(seq(pi/4,7*pi/4,pi/2)), zero = pi/2, rotation="clock", 
                labels = c("NE","NW","SW","SE"), cex = 1.1)
  ticks.circular(circular(seq(0,2*pi,pi/8)), zero=pi/2, rotation ='clock', tcl=0.075)
  rose.diag(dat_temp$average_bearing, bins=24, col="darkgrey", cex=1.0, prop=1.1, add=TRUE)
}
par(default)
#' Average bearing doesn't appear that different from the bearing for the displacements
plot(as.numeric(disp_dat2016$displacement_bearing) ~ as.numeric(disp_dat2016$average_bearing))

#' There's a pretty strong correlation between displacement bearing and average bearing.



#' Make rose diagrams for each sampling time.


for(i in sort(unique(dat2016$hour))){
  dat_temp <- dat2016[dat2016$hour==i,]
  plot(dat_temp$bearing, stack=T, bin=120, shrink=1.3)
  title(paste("Bearing at", toString(i), "2016"), line=1)
  axis.circular(at = circular(seq(pi/4,7*pi/4,pi/2)), zero = pi/2, rotation="clock", 
                labels = c("NE","NW","SW","SE"), cex = 1.1)
  ticks.circular(circular(seq(0,2*pi,pi/8)), zero=pi/2, rotation ='clock', tcl=0.075)
  rose.diag(dat_temp$bearing, bins=24, col="darkgrey", cex=1.0, prop=1.8, add=TRUE)
}

#' Throw in the calculations for rho and add some mean arrows too.
kuiper2016 <- as.data.frame(matrix(NA, nrow=length(unique(dat2016$hour)), ncol=3)) #Store Kuiper test results
colnames(kuiper2016) <- c("hour", "test_stat", "n")
kuiper2016$hour <- c(seq(7, 17, 1), 19)
for(i in sort(unique(dat2016$hour))){
  dat_temp <- dat2016[dat2016$hour==i,]
  plot(dat_temp$bearing, stack=T, bin=120, shrink=1.3)
  title(paste("Bearing at", toString(i), "2017"),line=1)
  axis.circular(at = circular(seq(pi/4,7*pi/4,pi/2)), zero = pi/2, rotation="clock", 
                labels = c("NE","NW","SW","SE"), cex = 1.1)
  ticks.circular(circular(seq(0,2*pi,pi/8)), zero=pi/2, rotation ='clock', tcl=0.075)
  arrows.circular(mean(dat_temp$bearing), y=rho.circular(dat_temp$bearing), lwd=3)
  text(0,0.65, c("r =              ", format(round(rho.circular(dat_temp$bearing),2), nsmall = 2)))
  test_temp<-kuiper.test((dat_temp$bearing*(pi/180))) #for the Kuiper test, data must be in radians. See 2017 for explanation of test
  kuiper2016[kuiper2016$hour==i,"test_stat"] <- test_temp$statistic
  kuiper2016[kuiper2016$hour==i, "n"] <- length(!is.na(dat_temp$bearing))
  }

kuiper2016 <- subset(kuiper2016, n > 4) #no critical values given for n < 5
kuiper2016
#' The results of the kuiper test for this sampling period reject the null hypothesis that the uniform distribution
#' describes our data well for all sampling times with n > 5 (cannot do test for n <5). These tests
#' are all significant at at least alpha = 0.05, with many of them significant at even lower alpha levels.

#' Bearing at each sampling point does not appear to show strong patterns after about 1400. Before that
#' it appears that movement *might* be more random approaching mid-day. The rho values are
#' kind of all over the place though.



#' Spun with ezspin("scripts/rose_diagrams.R", out_dir="spin_output", fig_dir="figures", keep_md=F)
#' 