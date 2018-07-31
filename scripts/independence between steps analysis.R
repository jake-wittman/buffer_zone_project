#libraries
library(ggplot2)

#clear out old stuff
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

#' Import data
data <- read.csv("data/9.23.16_gypsymothdispersal_1.5.csv", na.strings=".")

#' Data cleaning
data$x <- as.numeric(data$x)
data$y <- as.numeric(data$y)

##############Calculate Independence Between Steps ################################

#' To calculate the independence between steps follow Turchin 1998 pg 250.
alive_data <- data[data$living.status.log != 0 & !is.na(data$living.status.log),]

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
  #alive_data[alive_data$qbtag==j,"x_dis"] <-dat_temp$x_dis #stores the displacements in the dataset
  #alive_data[alive_data$qbtag==j,"y_dis"] <-dat_temp$y_dis
  
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
  my_avgs[my_avgs$tag_n==j, "d2"] <- (1/(nrow(dat_temp)-2))*sum_sq_dist #calculates d^2 for each larvae
  
}
my_avgs$test.indp <- my_avgs$d2/my_avgs$s2

hist(my_avgs$test.indp)

