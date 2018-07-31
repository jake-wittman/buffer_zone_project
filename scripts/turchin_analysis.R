#libraries
library(ggplot2)
library(plyr)
library(MASS)

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
data <- read.csv("data/cleaned_data.csv", na.strings="NA")

#' Data cleaning
data$x <- as.numeric(data$x)
data$y <- as.numeric(data$y)

##############Calculate Independence Between Steps ################################

#' To calculate the independence between steps follow Turchin 1998 pg 250.
alive_data <- data[data$status == 0 & !is.na(data$status),]

#' Create empty matrix to store steps

head(alive_data)
my_tags <- unique(alive_data$qbtag) #creates a vector of each unique QB tag identifier
my_avgs <- as.data.frame(matrix(NA, nrow=length(my_tags), ncol=3)) #matrix to store average displacements
colnames(my_avgs) <- c("tag_n","average_x","average_y") #name columns of matrix
my_avgs$tag_n <- my_tags #fill out column of tag label

alive_data$x_dis <- NA #creates empty columns for displacement
alive_data$y_dis <- NA
alive_data$xi_min_xbar <- NA #creates empty columns for Xi - Xbar
alive_data$yi_min_ybar <- NA #creates empty columns for yi - ybar
my_avgs$s2 <- NA #empty column to store each larval s^2
my_avgs$d2 <- NA #empty column to store each larval d^2
my_avgs$num_pairs <- NA #empty column to store the number of pairs of successive observations for each caterpillar
my_avgs$treatment <- NA #empty column to store the treatment of each caterpillar

#' These loops calculate s^2 and d^2 for each larva
for(j in my_tags){#This for loop goes through the dataset and creates a temporary data set containing only the observations for larvae j
  dat_temp <- alive_data[alive_data$qbtag==j,]
  dat_temp$x_dis <- NA #creates empty columns for displacement
  dat_temp$y_dis <- NA
  dat_temp$xi_min_xbar <- NA #creates empty columns for Xi - Xbar
  dat_temp$yi_min_ybar <- NA #creates empty columns for yi - ybar
  dat_temp$xii_min_xi <- NA #creates empty columns for Xi+1 - Xi
  dat_temp$yii_min_yi <- NA #creates empty columns for Yi+1 - Yi
  
  for(i in 2:nrow(dat_temp)){ #this for loop calculates the difference in sequential time points x and y values to get displacements
    dat_temp$x_dis[i] <- dat_temp[i,"x"] - dat_temp[i-1,"x"] #calculates xi displacement
    dat_temp$y_dis[i] <- dat_temp[i,"y"] - dat_temp[i-1,"y"] #calculates yi displacement
    
    if(i == nrow(dat_temp)){#once the for loop reaches the last observation in temp dat, it averages the observations and puts them in a df
      my_avgs[my_avgs$tag_n==j,"average_x"] <- mean_NA(dat_temp$x_dis) #averages xi displacements
      my_avgs[my_avgs$tag_n==j,"average_y"] <- mean_NA(dat_temp$y_dis) #averages yi displacements
    }
    
  }
  alive_data[alive_data$qbtag==j,"x_dis"] <-dat_temp$x_dis #stores the displacements in the dataset, not necessary to store but whatevs
  alive_data[alive_data$qbtag==j,"y_dis"] <-dat_temp$y_dis
  
  for(i in 2:nrow(dat_temp)){ #This loop calculates the deviation for each displacement
    dat_temp$xi_min_xbar[i] <- dat_temp[i,"x_dis"] - my_avgs[my_avgs$tag_n==j,"average_x"] #Xi - Xbar
    dat_temp$yi_min_ybar[i] <- dat_temp[i,"y_dis"] - my_avgs[my_avgs$tag_n==j,"average_y"] #yi - ybar
  }
  
  
  for(i in 2:nrow(dat_temp)){
    dat_temp$xii_min_xi[i] <- dat_temp[i+1, "x_dis"] - dat_temp[i, "x_dis"]
    dat_temp$yii_min_yi[i] <-dat_temp[i+1, "y_dis"] - dat_temp[i, "y_dis"]
  }
  
  
  #alive_data[alive_data$qbtag==j,"xi_min_xbar"] <-dat_temp$xi_min_xbar #stores displacement deviations
  #alive_data[alive_data$qbtag==j,"yi_min_ybar"] <-dat_temp$yi_min_ybar #stores displacement deviations, not necessary for calculations
  
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

#' Plotting results
hist(my_avgs$test_indp)
ggplot(my_avgs, aes(x=tag_n, y=test_indp, colour=treat_id)) + geom_point()
#' There are a range of values for this test statistic, from less than 0.75 to above 2.5 but most are between 1 and 2, suggesting some possible positive
#' autocorrelation between subsequent displacements in at least a few of the larvae. Interestingly, most of the larvae placed in trees have a 
#' test statistic of about 2.0
ggplot(my_avgs, aes(x=num_pairs, y=test_indp, colour=treat_id)) + geom_point() + scale_x_continuous(breaks=0:10)

#' I need to compare these values to a null-distribution to interpret. Turchin recommends simulating a bunch of paths 
#' by randomly sampling from my own data, then calculating the statistic to generate the null.




##################### Simulating Data for Null Distribution of Step Independence Test Statistic ##############################



#' I want to work with only the absence larvae, since they did most of the moving.
sim_dist <- alive_data[alive_data$treat_id == "Absence",] #create the dataset to be sampled from for simulation
sim_dist <- sim_dist[!is.na(sim_dist$segment_dist),]
x<- seq(0, 35, 0.1)
hist(sim_dist$segment_dist, prob=T)
curve(dnorm(x, mean=4.2215087, sd=5.9548053), add=T)
hist(sim_dist$bearing)
dist_travel_sample <- sim_dist$segment_dist #need vector of quantities to sample from
ta_sample <- sim_dist$bearing #vector for sampling
#MLE to find parameters for random sampling
#' Assume the segment distances come from a gamma distribution, althought I couldn't get that to work so I went with a normal.

mle<-fitdistr(sim_dist[!is.na(sim_dist$segment_dist), "segment_dist"], "normal")


nsims <- 50000
avgs_temp <- as.data.frame(matrix(NA, nrow=1, ncol=2)) #temperorary data frame to get average values
colnames(avgs_temp) <- c("xbar", "ybar")
sim_dat <- as.data.frame(matrix(NA, nrow=nsims, ncol=4)) #data frame to hold s2 and d2 values from each simulated path
colnames(sim_dat) <- c("s2", "d2", "stat", "nmoves")
num_pairs <- my_avgs[my_avgs$num_pairs > 2, "num_pairs"] #sample from my data - the number of points within a path, better simulates the fact that not all my paths are the same length
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
    dist_moved <- sample(dist_travel_sample, replace = T, 1)#sample from distribution of displacements
    ta <- sample(ta_sample, replace=T, 1) #sample from distribution of bearings
    #The below commented out lines are an alternative way to generate the random distance moved and turning angles using a normal and uniform distribution, respectively
    #The normal distribution parameters selected based on maximum likelihood estimation from the data I collected. Would've preferred a gamma but I couldn't get it to work
    #dist_moved <- rnorm(1,  mle$estimate[1], mle$estimate[2])
    #ta <- runif(1, 0, 360)
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
quantile(sim_dat$stat, probs=c(0.025, 0.975), na.rm=T)
hist(sim_dat$stat, prob=T, breaks=40)
abline(v=quantile(sim_dat$stat, probs=c(0.025, 0.975), na.rm=T))

###########Simulation by Cluster Sampling instead of individual#############
#' I don't think this is doing what I want it to do
cluster <- sample(my_avgs$test_indp, replace=T, size=nsims)
hist(cluster)

