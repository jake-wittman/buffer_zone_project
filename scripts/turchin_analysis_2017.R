#libraries
library(ggplot2)
library(plyr)
library(MASS)
library(fitdistrplus)
library(tidyr)
library(doParallel)
library(beepr)
library(lme4)

#' Clear out stuff
rm(list=ls())
#Custom functions
mean_NA <- function(x){ #Take means without getting errors due to NAs
  val_out <-mean(x,na.rm=T)
  return(val_out)
}

sum_NA <- function(x){
  val_out <- sum(x, na.rm=T)
  return(val_out)
}

r <- function(degrees){ #This function is for properly calculating the (x,y) after a move distance and angle
  degrees * (pi/180) #trig functions in r use radians, not degrees, so we need to convert them
}

#' Import data
dat2017 <- read.csv("data/cleaned_data_2017.csv", na.strings="NA")

#' Data cleaning
dat2017$x_m <- as.numeric(dat2017$x_m)
dat2017$y_m <- as.numeric(dat2017$y_m)
dat2017 <- dat2017[dat2017$qbtag != 30.1,]
dat2017 <- dat2017[dat2017$qbtag != 43,]
dat2017 <- dat2017[dat2017$qbtag != 59,]

##############Calculate Independence Between Steps ################################

#' To calculate the independence between steps follow Turchin 1998 pg 250.
#dat2017 <- data[data$status == 0 & !is.na(data$status),]

#' Create empty matrix to store steps

head(dat2017)
my_tags <- unique(dat2017$qbtag) #creates a vector of each unique QB tag identifier
my_avgs <- as.data.frame(matrix(NA, nrow=length(my_tags), ncol=3)) #matrix to store average displacements
colnames(my_avgs) <- c("tag_n","average_x","average_y") #name columns of matrix
my_avgs$tag_n <- my_tags #fill out column of tag label

dat2017$x_dis <- NA #creates empty columns for displacement
dat2017$y_dis <- NA
dat2017$xi_min_xbar <- NA #creates empty columns for Xi - Xbar
dat2017$yi_min_ybar <- NA #creates empty columns for yi - ybar
my_avgs$s2 <- NA #empty column to store each larval s^2
my_avgs$d2 <- NA #empty column to store each larval d^2
my_avgs$num_pairs <- NA #empty column to store the number of pairs of successive observations for each caterpillar
my_avgs$treatment <- NA #empty column to store the treatment of each caterpillar

#' These loops calculate s^2 and d^2 for each larva
for(j in my_tags){#This for loop goes through the dataset and creates a temporary data set containing only the observations for larvae j
  dat_temp <- dat2017[dat2017$qbtag==j,]
  dat_temp$x_dis <- NA #creates empty columns for displacement
  dat_temp$y_dis <- NA
  dat_temp$xi_min_xbar <- NA #creates empty columns for Xi - Xbar
  dat_temp$yi_min_ybar <- NA #creates empty columns for yi - ybar
  dat_temp$xii_min_xi <- NA #creates empty columns for Xi+1 - Xi
  dat_temp$yii_min_yi <- NA #creates empty columns for Yi+1 - Yi
  
  for(i in 2:nrow(dat_temp)){ #this for loop calculates the difference in sequential time points x and y values to get displacements
    dat_temp$x_dis[i] <- dat_temp[i,"x_m"] - dat_temp[i-1,"x_m"] #calculates xi displacement
    dat_temp$y_dis[i] <- dat_temp[i,"y_m"] - dat_temp[i-1,"y_m"] #calculates yi displacement
    
    if(i == nrow(dat_temp)){#once the for loop reaches the last observation in temp dat, it averages the observations and puts them in a df
      my_avgs[my_avgs$tag_n==j,"average_x"] <- mean_NA(dat_temp$x_dis) #averages xi displacements
      my_avgs[my_avgs$tag_n==j,"average_y"] <- mean_NA(dat_temp$y_dis) #averages yi displacements
    }
    
  }
  dat2017[dat2017$qbtag==j,"x_dis"] <-dat_temp$x_dis #stores the displacements in the dataset, not necessary to store but whatevs
  dat2017[dat2017$qbtag==j,"y_dis"] <-dat_temp$y_dis
  
  for(i in 2:nrow(dat_temp)){ #This loop calculates the deviation for each displacement
    dat_temp$xi_min_xbar[i] <- dat_temp[i,"x_dis"] - my_avgs[my_avgs$tag_n==j,"average_x"] #Xi - Xbar
    dat_temp$yi_min_ybar[i] <- dat_temp[i,"y_dis"] - my_avgs[my_avgs$tag_n==j,"average_y"] #yi - ybar
  }
  
  
  for(i in 2:nrow(dat_temp)){
    dat_temp$xii_min_xi[i] <- dat_temp[i+1, "x_dis"] - dat_temp[i, "x_dis"]
    dat_temp$yii_min_yi[i] <-dat_temp[i+1, "y_dis"] - dat_temp[i, "y_dis"]
  }
  
  
  #dat2017[dat2017$qbtag==j,"xi_min_xbar"] <-dat_temp$xi_min_xbar #stores displacement deviations
  #dat2017[dat2017$qbtag==j,"yi_min_ybar"] <-dat_temp$yi_min_ybar #stores displacement deviations, not necessary for calculations
  
  sum_sq_dev <- (sum_NA(dat_temp$xi_min_xbar^2 + dat_temp$yi_min_ybar^2)) #sums all sqaured deviations
  sum_sq_dist <- (sum_NA(dat_temp$xii_min_xi^2 + dat_temp$yii_min_yi^2))
  my_avgs[my_avgs$tag_n==j, "s2"] <- (1/(nrow(dat_temp)-2))*sum_sq_dev #calculates s^2 for each larva
  my_avgs[my_avgs$tag_n==j, "d2"] <- (1/(nrow(dat_temp)-1))*sum_sq_dist #calculates d^2 for each larvae
  my_avgs[my_avgs$tag_n==j, "num_pairs"] <- nrow(dat_temp)-1 #stores the number of pairs of successive observations for each larvae
  my_avgs[my_avgs$tag_n==j, "treat_id"] <- dat_temp$treat_id[i] #stores treatment for each caterpillar, 1 = absence, 2 = presence
}
my_avgs$test_indp <- my_avgs$d2/my_avgs$s2 #calculate test statistic for independence in subsequent displacements
my_avgs$treat_id <- as.factor(my_avgs$treat_id)
my_avgs$treat_id <- mapvalues(my_avgs$treat_id, from=c("1","2"), to =c("Absence", "Presence")) #rename factor levels
sum_NA(my_avgs$test_indp > 2) # The number of caterpillars with a ratio of s^2/d^2 > 2 - values significantly greater than 2 ipmly negative autocorrelation
sum_NA(my_avgs$test_indp < 2) #Number of caterpillars with ratio < 2 - values significantly less than 2 imply positive autocorrelation
my_avgs$n <- seq(1, nrow(my_avgs), 1) #give each caterpillar a number from 1- the number of caterpillars used

#' Plotting results
hist(my_avgs$test_indp)
ggplot(my_avgs, aes(x=n, y=test_indp, colour=treat_id)) + geom_point()
#' There are a range of values for this test statistic, from less than 0.75 to above 2.5 but most are between 1 and 2.5, suggesting some possible positive
#' autocorrelation between subsequent displacements in at least a few of the larvae. Interestingly, most of the larvae placed in trees have a 
#' test statistic of about 2.0. 
ggplot(my_avgs, aes(x=num_pairs, y=test_indp, colour=treat_id)) + geom_point() + scale_x_continuous(breaks=0:20)

#' I need to compare these values to a null-distribution to interpret. Turchin recommends simulating a bunch of paths 
#' by randomly sampling from my own data, then calculating the statistic to generate the null.




##################### Simulating Data for Null Distribution of Step Independence Test Statistic ##############################



#' I want to work with only the absence larvae, since they did most of the moving.
sim_dist <- dat2017[dat2017$treat_id == "Absence",] #create the dataset to be sampled from for simulation
sim_dist <- sim_dist[!is.na(sim_dist$segment_dist),]
x<- seq(0, 35, 0.01)

#' To fit a gamma distribution, first need to scale the parameter as the numbers are too large
m_segment_dist <- sim_dist$m_segment_dist #save segment distance as it's own vector
scale_segment_dist <- scale(m_segment_dist, center=F, scale=T) #scale the segment distance
str(scale_segment_dist) #The numbers were scaled by dividing by 3.68
scale_segment_dist <- as.numeric(scale_segment_dist) #set as numeric to remove the scaling factor attribute (apparently, this needs to happen before the next line)
scale_segment_dist <- ifelse(scale_segment_dist==0, scale_segment_dist + 10^-15, scale_segment_dist) #add a small value to the 0s so a gamma can be fit
fit <- fitdist(scale_segment_dist, "gamma")
hist(scale_segment_dist, prob=T, breaks=10)
curve(dgamma(x, shape=fit$estimate[1], rate=fit$estimate[2]), add=T)


hist(sim_dist$bearing)
dist_travel_sample <- sim_dist$segment_dist #need vector of quantities to sample from
ta_sample <- sim_dist$bearing #vector for sampling


nsims <- 1000
avgs_temp <- as.data.frame(matrix(NA, nrow=1, ncol=2)) #temperorary data frame to get average values
colnames(avgs_temp) <- c("xbar", "ybar")
sim_dat <- as.data.frame(matrix(NA, nrow=nsims, ncol=4)) #data frame to hold s2 and d2 values from each simulated path
colnames(sim_dat) <- c("s2", "d2", "stat", "nmoves")
num_pairs <- my_avgs[my_avgs$num_pairs > 2, "num_pairs"] #sample from my data - the number of points within a path, better simulates the fact that not all my paths are the same length
set.seed(1)
for(j in 1:nsims){ #This loop is for creating multiple simulated paths
  nmoves <- sample(num_pairs, replace=T, 1) #sample the number points in a path
  move_temp <- as.data.frame(matrix(NA, nrow=nmoves, ncol=2)) #create an appropriately sized data frame to hold the simulated movement data
  colnames(move_temp) <- c("x","y")
  move_temp$x_dis <- NA #creates empty columns for displacement
  move_temp$y_dis <- NA
  move_temp$xi_min_xbar <- NA #creates empty columns for Xi - Xbar
  move_temp$yi_min_ybar <- NA #creates empty columns for yi - ybar
  move_temp$xii_min_xi <- NA #creates empty columns for Xi+1 - Xi
  move_temp$yii_min_yi <- NA #creates empty columns for Yi+1 - Yi
  
  for(i in 2:nmoves){
    move_temp[1,1:4] <- 0 #set starting points as (0,0)
    #dist_moved <- sample(dist_travel_sample, replace = T, 1)#sample from distribution of displacements
    #ta <- sample(ta_sample, replace=T, 1) #sample from distribution of bearings
    #The below commented out lines are an alternative way to generate the random distance moved and turning angles using a normal and uniform distribution, respectively
    #The normal distribution parameters selected based on maximum likelihood estimation from the data I collected. Would've preferred a gamma but I couldn't get it to work
    dist_moved <- (rgamma(1,  fit$estimate[1], fit$estimate[2])) * 3.68 #uses fit for gamma dist from above and multiplies by the scaling value used in finding fit
    ta <- runif(1, 0, 360)
    #if(dist_moved < 0){
    # dist_moved <- rnorm(1,  mle$estimate[1], mle$estimate[2])
    #}
    move_temp[i, 1] <- dist_moved*sin(r(ta)) + move_temp[i-1,1] #calculate the x coordinate of move i
    move_temp[i, 2]<- dist_moved*cos(r(ta)) + move_temp[i-1,2] #calculatae the y coordinate of move i
    move_temp$x_dis[i] <- dist_moved*sin(r(ta)) #calculate displacement in x direction
    move_temp$y_dis[i] <- dist_moved*cos(r(ta)) #calculate displacement in y direction
    move_temp$xii_min_xi[i] <- move_temp[i, "x_dis"] - move_temp[i-1, "x_dis"] #calculate the difference in Xi + 1 and Xi
    move_temp$yii_min_yi[i] <-move_temp[i, "y_dis"] - move_temp[i-1, "y_dis"] #Calculate difference in yi +1 and yi
    
    if(i == nmoves){ #when reaching last move in the simulated path, calculate the average x and y displacements
      avgs_temp[,1] <- mean_NA(move_temp$x_dis)
      avgs_temp[,2] <- mean_NA(move_temp$y_dis)
    }
  }
  for(i in 2:nmoves){ #calculate the difference in each ith displacement and the average (both x and y)
    move_temp$xi_min_xbar[i] <- move_temp$x_dis[i] - avgs_temp[1,1]
    move_temp$yi_min_ybar[i] <- move_temp$y_dis[i] - avgs_temp[1,2]
  }
  sum_sq_dev <- (1/(nrow(move_temp)-1))*sum_NA((move_temp$xi_min_xbar)^2 + (move_temp$yi_min_ybar)^2)
  sum_sq_dist <- (1/(nrow(move_temp)-1))*sum_NA((move_temp$xii_min_xi)^2 + (move_temp$yii_min_yi)^2)
  sim_dat[j,"s2"] <- sum_sq_dev
  sim_dat[j, "d2"] <- sum_sq_dist
  if(sim_dat[j, "d2"] == 0){
    sim_dat[j,"stat"] <- NA #some of the simulations give a d2 of 0 which causes problems, so turn these to NA instead
  } else{
    sim_dat[j,"stat"] <- sum_sq_dist/sum_sq_dev #calculate test statistic
  }
  sim_dat[j, "nmoves"] <- nmoves
  if(j%%100 ==0) {print(sprintf("Iteration: %d", j))
    flush.console()
    
  }
}  
mean(sim_dat$stat, na.rm=T)
sd(sim_dat$stat, na.rm=T)
crit_values<-quantile(sim_dat$stat, probs=c(0.025, 0.975), na.rm=T) #used to find critical (alpha = 0.05) values for the test stat under our null
hist(sim_dat$stat, prob=T, breaks=30)
abline(v=quantile(sim_dat$stat, probs=c(0.025, 0.975), na.rm=T))
sim_dat$n <- seq(1, nrow(sim_dat), 1)
ggplot(sim_dat, aes(x=n, y=stat)) + geom_point()

#' So the above chunk of code summarizes the restults of the sim, let's look at our actual data with these critical values
hist(my_avgs$test_indp, breaks=30)
abline(v=quantile(sim_dat$stat, probs=c(0.025, 0.975), na.rm=T))
which(my_avgs$test_indp < crit_values[1] | my_avgs$test_indp > crit_values[2])
length(which(my_avgs$test_indp < crit_values[1] | my_avgs$test_indp > crit_values[2]))
#' 16 of the larvae have a test statistic value that is significant indicating some autocorrelation.
#' This is primarily from test stat values that are less than 1 implying negative autocorrelation.


###############################################################################################
#
#                                       Net Squared Displacement
#
###############################################################################################
#' Net squared displacement can be used to check whether a correlated random walk model is appropriate
#' for modeling movement.
#' To calculate it, you need the mean move length, mean move length squared, average cos of the turn angle
#' and the average sin of the turn angle.



#' I SAVED THE OUTPUT OF THESE SIMULATIONS AS A CSV FILE CALLED net_r2.csv. USE THIS RATHER 
#' THAN RERUNNING SIMULATIONS


dat2017_abs <- subset(dat2017, treat_id=="Absence") #Work only with larvae on ground
dat2017_abs <- subset(dat2017, open_env==0) #remove data from larvae moving through a different environment
#Need to reorder the time factor
dat2017_abs$time <- factor(dat2017_abs$time, levels=c("7:30", "8:00", "8:30", "9:00",
                                                      "9:30", "10:00", "10:30", "11:00",
                                                      "11:30", "12:00", "12:30", "13:00", 
                                                      "13:30", "14:00", "14:30", "15:00",
                                                      "15:30", "16:00", "16:30", "17:00"))
#dat2017_abs <- dat2017_abs[dat2017_abs$time != "7:30",] #7:30 is the start of the observations, so not used in calculated net sq disp.
dat2017_abs$step_n <- ifelse(dat2017_abs$time=="7:30", 0, ifelse(dat2017_abs$time == "8:00", 1, ifelse(dat2017_abs$time =="8:30", 2, ifelse(dat2017_abs$time =="9:00", 3,
                             ifelse(dat2017_abs$time == "9:30", 4, ifelse(dat2017_abs$time == "10:00", 5 , ifelse(dat2017_abs$time =="10:30", 6,
                            ifelse(dat2017_abs$time =="11:00", 7, ifelse(dat2017_abs$time =="11:30", 8, ifelse(dat2017_abs$time =="12:00",9,
                            ifelse(dat2017_abs$time =="12:30", 10, ifelse(dat2017_abs$time =="13:00", 11, ifelse(dat2017_abs$time =="13:30", 12,
                            ifelse(dat2017_abs$time =="14:00", 13, ifelse(dat2017_abs$time =="14:30", 14, ifelse(dat2017_abs$time =="15:00", 15,
                            ifelse(dat2017_abs$time =="15:30", 16, ifelse(dat2017_abs$time =="16:00", 17, 
                            ifelse(dat2017_abs$time =="16:30", 18, 19)))))))))))))))))))
m1 <- mean(dat2017_abs$m_segment_dist, na.rm=T) #calculate mean move length
m2 <- mean((dat2017_abs$m_segment_dist^2), na.rm=T) #mean move length^2
phi <- mean(cos(dat2017_abs$ta[!is.na(dat2017_abs$ta)]*(pi/180))) #calculate average cosin of turn angle (in radians, not degrees)
s <- mean(sin(dat2017_abs$ta[!is.na(dat2017_abs$ta)]*(pi/180)))  #calculate average sign of turn angle (in radians) - this should be close to 0
n <- seq(1, 19, 1) #The number of moves measured. In this case, I took 20 location points so 19 moves
alpha <- atan(s/phi) #used for calculating net sq displacement
gamma <- (((1-phi)^2-s^2)*cos((n+1)*alpha))-(2*s*(1-phi)*sin((n+1)*alpha))
expected_r2 <- (n*m2) + (2 * m1) * (phi/(1-phi)) * (n-((1-phi^n)/(1-phi))) #calculate exp. r2 for a correlation random walk
expected_r2_urw <- (n*m2) #calculate expected r2 for an uncorrelated random walk
test <- (n*m2) + (2*m1^2)*((((phi-phi^2-s^2)*(n-phi))/((1-phi)^2+s^2))+((2*s^2+(phi+s^2)^((n+1)/2))/((1-phi)^2+s^2)^2)*gamma)

#Calculate observed net R2 by calculating R2 for each organism at each step n and then averaging R2 across steps
for(j in unique(dat2017_abs$qbtag)){
  dat_temp <- dat2017_abs[dat2017_abs$qbtag==j,]
  for(i in 1:nrow(dat_temp)){
    dat_temp$net_r2[i] <- (sqrt((dat_temp$x_m[i] - dat_temp$x_m[1])^2 + 
                                 (dat_temp$y_m[i] - dat_temp$y_m[1])^2))^2 #calculate net R
  }
  dat2017_abs$net_r2[dat2017_abs$qbtag==j] <- dat_temp$net_r2 #store net R
}


observed_r2 <- aggregate(dat2017_abs[,c("net_r2")], by=list(time=dat2017_abs$time), 
                  function(x) c(mean = mean(x, na.rm=T), n=length(x), sd=sd(x, na.rm=T))) #calculate observed net r2
observed_r2 <- observed_r2[-1,]
net_r2 <- data.frame(exp_r2 = expected_r2, obs_r2 = (observed_r2$x[,1]), step=n, n=observed_r2$x[,2], exp_r2_ucw=expected_r2_urw) #create data frame for data
net_r2$step <- as.integer(net_r2$step)
net_r2_long <- net_r2 %>% gather(key=r2, value=exp_r2, exp_r2, obs_r2)
net_r2_long$r2 <- as.factor(net_r2_long$r2)
obs_l_CI <- net_r2$obs_r2 - (qt(0.975, df=net_r2$n-1) * (observed_r2$x[,"sd"]/sqrt(net_r2$n)))
obs_u_CI <- net_r2$obs_r2 + (qt(0.975, df=net_r2$n-1) * (observed_r2$x[,"sd"]/sqrt(net_r2$n)))
#' It looks like the insects don't move according to a correlated random walk, but I should test this by using a bootstrap procedure.
#' To do this, I need to simulate a bunch of paths from my parameter distributions (turn angle and step distance). I also need to 
#' do so in such a way that there are paths with a different number of steps.  
#' The first step is to generate my new data. I'm going to save the (x,y) coordinates after each move, the net displacement, and the step number of each move. Then I'll want to
#' summarize that data set like I did in calculating my observed net displacement above. I'll need to calculate confidence intervals from these summaries.
npaths <- 10000
sim_dat <- matrix(NA, ncol=8, nrow=0) #empty data frame to hold simulated data. Nrows is equal to the number of paths * the maximum path length. Not all paths will be that long though
colnames(sim_dat) <- c("number", "x", "y", "net_r2", "step", "m_segment_dist", "ta", "path_length")
step_sample <- subset(my_avgs, tag_n %in% dat2017_abs$qbtag) #subset the my_avgs dataframe to include
#the same larvae that are in the dat2017_abs dataframe, used to get distribution of path lengths
set.seed(1)
for(j in 1:npaths){
  nmoves <- sample(step_sample$num_pairs, replace=T, 1) #determine how many steps are in the simulatedp ath
  dat_temp <- matrix(NA, ncol=8, nrow=(nmoves + 1)) #data frame to hold data
  colnames(dat_temp) <- c("number", "x", "y", "net_r2", "step", "m_segment_dist", "ta", "path_length")
  dat_temp[1, c("x", "y", "net_r2", "step")] <- c(0, 0, 0, 0) #set starting location
  dat_temp[,"step"] <- seq(0, nmoves, 1) #label the step # in each temp data frame
  for(i in 2:nrow(dat_temp)){
    if(i==2){
      ta <- runif(1, 0, 360)
    }
    else{ta <- sample(dat2017_abs$ta[!is.na(dat2017_abs$ta)], replace=T, 1) #sample TA from my distribution of turn angles
    }
    dist <-  sample(dat2017_abs$m_segment_dist[!is.na(dat2017_abs$m_segment_dist)], replace=T, 1) #sample distance moved from my data
    dat_temp[i, c("ta", "m_segment_dist")] <- c(ta, dist) #store ta and the movement distance for each move, just for quality assurance
    dat_temp[i, "x"] <- dist*sin(r(ta)) + dat_temp[i-1,"x"] #calculate the x coordinate of move i
    dat_temp[i, "y"]<- dist*cos(r(ta)) + dat_temp[i-1,"y"] #calculatae the y coordinate of move i
    dat_temp[i, "net_r2"] <- (sqrt((dat_temp[i, "x"] - dat_temp[1,"x"])^2 + (dat_temp[i, "y"] - dat_temp[1, "y"])^2))^2 #need to square whole thing, as it is net *squared* displacement
    dat_temp[1:i, "number"] <- j #the id for each movement path
    dat_temp[1:i, "path_length"] <- nmoves #how long each movement path is
     }
  sim_dat <- rbind(sim_dat, dat_temp) #merge the temporary data into an overall dataset
  if(j%%100 ==0) {print(sprintf("Iteration: %d", j)) #print progress
    flush.console()
  }
}

#' The CI produced by the above simulations are very asymmetical, so I'm going to try going 
#' about it another way as I don't think they should be as asymmetical as they are.
num_steps <- net_r2$n #how many paths were used to calculate the obs net_r2 value at each step, used for bootstrapping
sim_dat <- as.data.frame(sim_dat) #convert simulated data to a dataframe for easier sampling
nsims <- 1000
boot_dat <- matrix(NA, nrow=nsims, ncol=max(net_r2$step))

set.seed(1)
registerDoParallel(cores=(detectCores() - 1 )) #Need to set the # of cores each time you run the below loop
boot_dat<-foreach(j = 1:nrow(boot_dat), .combine=rbind) %:% #the %:% operator nests the foreach loops
  #print(sprintf("Row: %d", j)) #prints which step net_r2 is being bootstrapped
  foreach(i = 1:max(net_r2$step), .combine=c) %dopar%{
    dat_temp <- subset(sim_dat, path_length >= i) #subset data to only include paths with i # of steps
    temp_id <-sample(unique(dat_temp$number), replace=T, num_steps[i]) #randomly select paths from this subset of data
    random_sample <- subset(dat_temp, number %in% temp_id) #create data frame of randomly selected paths
    m1 <- mean(random_sample$m_segment_dist, na.rm=T) #calculate mean move length
    m2 <- mean((random_sample$m_segment_dist^2), na.rm=T) #mean move length^2
    phi <- mean(cos(random_sample$ta[!is.na(random_sample$ta)]*(pi/180))) #calculate average cosin of turn angle (in radians, not degrees)
    s <- mean(sin(random_sample$ta[!is.na(random_sample$ta)]*(pi/180)))  #calculate average sign of turn angle (in radians) - this should be close to 0
    n <- seq(1, 19, 1) #The number of moves measured. In this case, I took 20 location points so 19 moves
    alpha <- atan(s/phi) #used for calculating net sq displacement
    gamma <- (((1-phi)^2-s^2)*cos((n+1)*alpha))-(2*s*(1-phi)*sin((n+1)*alpha))
    sim_r2 <- (n*m2) + (2 * m1) * (phi/(1-phi)) * (n-((1-phi^n)/(1-phi)))
    #boot_dat[j, i] <- sim_r2[i]
    sim_r2[i]
  }
beep(8)

boot_dat <- as.matrix(boot_dat) #set as a matrix for apply function below
CI<-apply(boot_dat, 2, quantile, probs=c(0.025, 0.975), name=F) #get CI for each step of the expected r2
net_r2$exp_l_CI <- CI[1,] #save exp lower CI
net_r2$exp_u_CI <- CI[2,] #save exp upper CI
net_r2$obs_l_CI <- obs_l_CI
net_r2$obs_u_CI <- obs_u_CI

net_r2 <- read.csv("data/net_r2.csv") #Bring in previously saved simulation outcomes
ggplot() + geom_line(data=net_r2_long, aes(x=step, y=exp_r2, group=r2, linetype=r2)) + 
  geom_point(data=net_r2, aes(x=step, y=exp_r2)) +
  #geom_errorbar(data=net_r2, aes(x=step, ymin=exp_l_CI, ymax=exp_u_CI, colour="blue")) +
  geom_ribbon(data=net_r2, aes(x=step, ymin=exp_l_CI, ymax=exp_u_CI), alpha=0.2)+
  #geom_line(data=net_r2, aes(x=step, y=obs_r2, colour="black", linetype="dashed")) +
  geom_point(data=net_r2, aes(x=step, y=obs_r2)) +
  #geom_ribbon(data=net_r2, aes(x=step, ymin=obs_l_CI, ymax=obs_u_CI), alpha=0.2) +
  #geom_line(data=net_r2, aes(x=step, y=exp_r2_ucw, colour="green"))
  labs(x="Number of consecutive moves", y=expression(Squared~displacement~" "~ R[n]^{2}~(ft)),
       linetype=" ", colour=" ") +
  scale_linetype_manual(labels=c(expression(Pred.~R[n]^{2}),
                              expression(Obs.~R[n]^{2})), values=c("solid", "dashed")) +
  #scale_color_manual(labels = "",values=c("black", "black")) +
  scale_y_continuous(limits = c(0, 762), breaks = c(0, 152.4, 304.8, 457.2, 609.5, 762), 
                     labels=c("0", "500", "1000", "1500", "2000", "2500") ) +
  scale_x_continuous(limits = c(0, 20), breaks=seq(0, 20, 2)) +
  #theme_bw() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        axis.line = element_line(colour = "black"),
        axis.title = element_text(size = 12, colour = "black"),
        axis.text = element_text(size = 10, colour = "black"),
        legend.key = element_rect(size = 1, fill = "white"),
        legend.key.size = unit(1.9, "lines"),
        legend.position = c(0.05, 0.7))

  
  
ggsave(filename = "figures/netrsq.tiff", width = 3.2, height = 4, dpi = 1200, units = "in")

net_r2 <- net_r2[c("step", "obs_r2", "exp_l_CI", "exp_r2", "exp_u_CI", "n")]
#' net_r2 saved as csv with write.csv(net_r2, file="data/net_r2.csv")
#' So, most of the observed R2 do not fall within the CI for the predicted R2. This suggests
#' that the insects do not move according to a correlate random walk. Because it's lower than 
#' predicted, perhaps the insects on average have some directional persistence to their 
#' movement. I could look at autocorrelations in movement direction (as opposed to turning angle).
#' 

##########################################################################################################################
#
#           Mixed Effects Model for seeing if/how temperature impacts movement distance in each step
#
##########################################################################################################################
#' Want to use a mixed effects model to see if temperature has a significant effect on distance moved at a certain time.
#' Because I'm looking at distance moved every 30 minutes, there will be correlation within-subject (individual larvae).
#' I want to account for that by using a random slope + intercept model (this is more conservative than just a random int. model).
#' I'll also be using time as an offset in the model, so I can model it as a rate.
dat2017_abs$qbtag <- as.factor(dat2017_abs$qbtag)
dat2017_abs$net_r <- sqrt(dat2017_abs$net_r2) #displacement at each step rather than disp^2
avg_temp <- aggregate(g_temp ~ time, data=dat2017_abs, FUN = function(x) mean(x))
avg_dist <- aggregate(m_segment_dist ~ time, data=dat2017_abs, FUN = function(x) mean(x))
avg_r <- aggregate(net_r ~ time, data=dat2017_abs, FUN = function(x) mean(x))
avg_ta <- aggregate(ta ~ time, data=dat2017_abs, 
                    FUN = function(x) c(mean = mean(x),
                                         n = length(x), se = sd(x)/sqrt(length(x))))
ggplot(dat2017_abs, aes(ta)) + geom_histogram() + facet_grid(time~.)
plot(ta ~ as.numeric(time), data=dat2017_abs)
plot(m_segment_dist ~ g_temp, data=dat2017_abs)
plot(m_segment_dist ~ as.numeric(time), data=dat2017_abs)
plot(avg_dist$m_segment_dist ~ avg_temp$g_temp)
plot(avg_ta$ta[,1] ~ avg_ta$time)
plot(m_segment_dist ~ air_temp, data=dat2017_abs)

lme <- lmer(log(net_r2+1) ~ g_temp + (g_temp|qbtag), data=dat2017_abs)
glme <- glmer((round(sqrt(net_r2), 0)) ~ g_temp + (g_temp|qbtag), data=dat2017_abs,
              family = poisson())


#' 
#' ezspin("scripts/turchin_analysis_2017.R", out_dir="spin_output", fig_dir="figures", keep_md=F)
