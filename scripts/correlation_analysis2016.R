#Clear list
rm(list=ls())

#libraries
library(ggplot2)
library(plyr)

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
data <- read.csv("data/cleaned_data_2016.csv", na.strings="NA")

#' Data cleaning
data$x_m <- as.numeric(data$x_m)
data$y_m <- as.numeric(data$y_m)

#' I want to calculate the ith order correlations between the jth distance/turning angle and successive distance/turning angle i moves away (so
#' the 2nd order correlation is the correlation between distance moved at time 1 and time 2, time 2 and time 3, etc. This can be distance vs distance,
#' time vs time, or distance vs time). These are serial correlations.
#' I also want to calculate the correlation between the ith distance moved and the ith turning angle. This is a first order correlation.

##############More Data Cleaning################################

#' Following Andow and Kiritani 1984. 
alive_data <- data[data$living_status_log != 0 & !is.na(data$living_status_log),] #only work with data from living larvae

#' Create empty matrix to store data
my_tags <- unique(alive_data$qbtag) #creates a vector of each unique QB tag identifier
cor_data <- as.data.frame(matrix(NA, nrow=length(my_tags), ncol=21)) #matrix to store average displacements
colnames(cor_data) <- c("qbtag","dist1","dist2", "dist3", "dist4", "dist5", "dist6", "dist7", "dist8", "dist9", "dist10", "ta1",
                       "ta2", "ta3", "ta4", "ta5", "ta6", "ta7", "ta8", "ta9", "treatment") #name columns of matrix
cor_data$qbtag <- my_tags #fill out column of tag label

#' These loops store the distance and turning angle values for each larvae
for(j in my_tags){#This for loop goes through the dataset and creates a temporary data set containing only the observations for larvae j
  dat_temp <- alive_data[alive_data$qbtag==j,]
  cor_data[cor_data$qbtag==j,"treatment"] <- dat_temp$treat_id[1] #store treatment variable, 1 = Absent, 2 = Presence
  for(i in 2:nrow(dat_temp)){ #this for loop stores the distance moved and turning angle of each move
    cor_data[cor_data$qbtag==j,i] <- dat_temp$m_segment_dist[i] #store the distance moved in move i in the appropriate column
    cor_data[cor_data$qbtag==j,i+9] <- dat_temp$ta[i] #store the turning angle for move i in the appropriate column
    #The line adding ta to the cor_data data frame is a little weird. Technically the turning angle at i=2 will
    #always be NA because you need 3 moves to make a turning angle. When i=2, i+9 = 11 which is the column
    #for the distance moved at step 10 (column 11). So at i = 2, an NA value gets placed in column 11. When
    # the loop keeps going at gets to i=11 (the last row in the dat_temp frame), it overwrites the NA in 
    # column 11 with the distance moved at step 10. So ta1 is truly the first turn angle available.  
    }
  }

cor_data_absent <- subset(cor_data, treatment==1) #subset by those on the ground, the ones in the trees hardly moved

for(i in 1:ncol(cor_data)){ #This tells me how many observations I have for each ith move
  print(sum(!is.na(cor_data_absent[,i])))
}

for(i in 1:ncol(cor_data)){ #this tells me how many observations I have that aren't 0 or NA for each ith move
  print(sum_NA(cor_data_absent[,i] != 0))
}
#' So one problem with this is that I have a lot of dead larvae data points. If the larvae moved 0 because it was dead and I didn't properly
#' record it as dead (or it was resting) then it will have an NA for turning angle. I'm going to remove the 0's because in most cases the insect is dead.
cor_data_absent[cor_data_absent == 0] <- NA #Turns 0s to NA

#' Steps 8 and 9 have only 2 and 1 observations for distance, respectively so I'll remove those columns as well.
#' I should also remove ta8 because it only has 1 observation.
cor_data_absent <- cor_data_absent[,-c(1,9,10,19,21)]

cor_results <- as.data.frame(matrix(NA, ncol=ncol(cor_data_absent), nrow=ncol(cor_data_absent))) #set up data frame to store results
colnames(cor_results) <- c("dist1","dist2", "dist3", "dist4", "dist5", "dist6", "dist7", "dist10", "ta1",
                        "ta2", "ta3", "ta4", "ta5", "ta6", "ta7", "ta9" ) #name columns of matrix
rownames(cor_results) <- c("dist1","dist2", "dist3", "dist4", "dist5", "dist6", "dist7", "dist10", "ta1",
                           "ta2", "ta3", "ta4", "ta5", "ta6", "ta7", "ta9" ) #name rows of matrix

##################### Pairwise Correlation Matrix ###################################
#' This for loop calculates all the pairwise correlations between dist & dist, dist& ta, and ta & ta. I don't think this is what I wanted, necessarily.
for(j in 1:ncol(cor_data_absent)){
  tempj <- cor_data_absent[,j] #seperate the column used to calculate the correlation temporarily
  for(i in 1:ncol(cor_data_absent)){
    tempi <- cor_data_absent[,i] #seperate the other column used to calculate correlation
    if(sum(!is.na(tempj)) <6 || sum(!is.na(tempi)) <6){ #prevent NAs from exiting loop if there are fewer than 6 observations
      cor_results[j,i] <- NA
      }
    else{
    temp <- cor.test(tempj, tempi) #calculate correlation
    cor_results[j,i] <- temp$estimate #store correlation estimate
      }
    }
}

################### ith Order Correlation#################
#' Distance v TA
#first_dta <- matrix(NA, ncol=2, nrow=(ncol(cor_data_absent) * nrow(cor_data_absent))/2) #create empty matrix to store data
dist_cor <- cor_data_absent[,1:8] #seperate the correlation data to make the data wrangling a bit easier
ta_cor <- cor_data_absent[,9:16] #same as above comment

#Make the objects that'll be used for all the 1st order events (Comparing step i to step i + 1)
{dist_order1 <- NA
  ta_order1 <- NA
  for(i in 1:ncol(dist_cor)){
    dist_order1 <- c(dist_order1, dist_cor[,i])
    ta_order1 <- c(ta_order1, ta_cor[,i])
  }
  dist_order1 <- dist_order1[2:length(dist_order1)]
  ta_order1 <- ta_order1[2:length(ta_order1)]
  
  
  #Make 2nd order events (comparing step i to step i+2)
  dist_order2 <- NA
  ta_order2 <- NA
  for(i in 2:ncol(dist_cor)){
    dist_order2 <- c(dist_order2, dist_cor[,i])
    ta_order2 <- c(ta_order2, ta_cor[,i])
  }
  dist_order2 <- dist_order2[2:length(dist_order2)]
  ta_order2 <- ta_order2[2:length(ta_order2)]
  
  #Make 3rd order events (comparing step i to step i+3)
  dist_order3 <- NA
  ta_order3 <- NA
  for(i in 3:ncol(dist_cor)){
    dist_order3 <- c(dist_order3, dist_cor[,i])
    ta_order3 <- c(ta_order3, ta_cor[,i])
  }
  dist_order3 <- dist_order3[2:length(dist_order3)]
  ta_order3 <- ta_order3[2:length(ta_order3)]
  
  #Make 4th order events (comparing step i to step i+4)
  dist_order4 <- NA
  ta_order4 <- NA
  for(i in 4:ncol(dist_cor)){
    dist_order4 <- c(dist_order4, dist_cor[,i])
    ta_order4 <- c(ta_order4, ta_cor[,i])
  }
  dist_order4 <- dist_order4[2:length(dist_order4)]
  ta_order4 <- ta_order4[2:length(ta_order4)]
  
  #Make 5th order events (comparing step i to step i+5)
  dist_order5 <- NA
  ta_order5 <- NA
  for(i in 5:ncol(dist_cor)){
    dist_order5 <- c(dist_order5, dist_cor[,i])
    ta_order5 <- c(ta_order5, ta_cor[,i])
  }
  dist_order5 <- dist_order5[2:length(dist_order5)]
  ta_order5 <- ta_order5[2:length(ta_order5)]
  
  #Make 6th order events (comparing step i to step i+6)
  dist_order6 <- NA
  ta_order6 <- NA
  for(i in 6:ncol(dist_cor)){
    dist_order6 <- c(dist_order6, dist_cor[,i])
    ta_order6 <- c(ta_order6, ta_cor[,i])
  }
  dist_order6 <- dist_order6[2:length(dist_order6)]
  ta_order6 <- ta_order6[2:length(ta_order6)]
  
  #Make 7th order events (comparing step i to step i+7)
  dist_order7 <- NA
  ta_order7 <- NA
  for(i in 7:ncol(dist_cor)){
    dist_order7 <- c(dist_order7, dist_cor[,i])
    ta_order7 <- c(ta_order7, ta_cor[,i])
  }
  dist_order7 <- dist_order7[2:length(dist_order7)]
  ta_order7 <- ta_order7[2:length(ta_order7)]
  
  #Make 8th order events (comparing step i to step i+8) although really it's for ta at step 9 and dist at step 10
  dist_order8 <- NA
  ta_order8 <- NA
  for(i in 8:ncol(dist_cor)){
    dist_order8 <- c(dist_order8, dist_cor[,i])
    ta_order8 <- c(ta_order8, ta_cor[,i])
  }
  dist_order8 <- dist_order8[2:length(dist_order8)]
  ta_order8 <- ta_order8[2:length(ta_order8)]
} #The commands in this bracketed section make all the vectors used to do the ith order correlations.

i_order_cor<- as.data.frame(matrix(NA, ncol=2, nrow=16)) #set up data frame to store results
colnames(i_order_cor) <- c("dist1","ta1") #name columns of matrix
rownames(i_order_cor) <- c("dist1","dist2", "dist3", "dist4", "dist5", "dist6", "dist7", "dist10", "ta1",
                           "ta2", "ta3", "ta4", "ta5", "ta6", "ta7", "ta9" ) #name rows of matrix

#'First order correlations
dt1<-cor.test(dist_order1[1:length(ta_order1)], ta_order1)
td1 <- cor.test(ta_order1[1:length(dist_order1)], dist_order1)
i_order_cor[9,1] <- paste("r=", round(dt1$estimate, 3), "p=", round(dt1$p.value, 3), "(",dt1$parameter,")")
i_order_cor[1,2] <- paste("r=", round(td1$estimate, 3), "p=", round(td1$p.value, 3), "(",td1$parameter,")")

#' Second order correlations
dd2<-cor.test(dist_order1[1:length(dist_order2)], dist_order2)
dt2<-cor.test(dist_order1[1:length(ta_order2)], ta_order2)
tt2<-cor.test(ta_order1[1:length(ta_order2)], ta_order2)
td2 <- cor.test(ta_order1[1:length(dist_order2)], dist_order2)
i_order_cor[2,1] <- paste("r=", round(dd2$estimate, 3), "p=", round(dd2$p.value, 3), "(",dd2$parameter,")")
i_order_cor[10,1] <- paste("r=", round(dt2$estimate, 3), "p=", round(dt2$p.value, 3), "(",dt2$parameter,")")
i_order_cor[10,2] <- paste("r=", round(tt2$estimate, 3), "p=", round(tt2$p.value, 3), "(",tt2$parameter,")")
i_order_cor[2,2] <- paste("r=", round(td2$estimate, 3), "p=", round(td2$p.value, 3), "(",td2$parameter,")")

#' Third order correlations
dd3<-cor.test(dist_order1[1:length(dist_order3)], dist_order3)
dt3<-cor.test(dist_order1[1:length(ta_order3)], ta_order3)
tt3<-cor.test(ta_order1[1:length(ta_order3)], ta_order3)
td3 <- cor.test(ta_order1[1:length(dist_order3)], dist_order3)
i_order_cor[3,1] <- paste("r=", round(dd3$estimate, 3), "p=", round(dd3$p.value, 3), "(",dd3$parameter,")")
i_order_cor[11,1] <- paste("r=", round(dt3$estimate, 3), "p=", round(dt3$p.value, 3), "(",dt3$parameter,")")
i_order_cor[11,2] <- paste("r=", round(tt3$estimate, 3), "p=", round(tt3$p.value, 3), "(",tt3$parameter,")")
i_order_cor[3,2] <- paste("r=", round(td3$estimate, 3), "p=", round(td3$p.value, 3), "(",td3$parameter,")")

#' Fourth order correlations
dd4<-cor.test(dist_order1[1:length(dist_order4)], dist_order4)
dt4<-cor.test(dist_order1[1:length(ta_order4)], ta_order4)
tt4<-cor.test(ta_order1[1:length(ta_order4)], ta_order4)
td4 <- cor.test(ta_order1[1:length(dist_order4)], dist_order4)
i_order_cor[4,1] <- paste("r=", round(dd4$estimate, 3), "p=", round(dd4$p.value, 3), "(",dd4$parameter,")")
i_order_cor[12,1] <- paste("r=", round(dt4$estimate, 3), "p=", round(dt4$p.value, 3), "(",dt4$parameter,")")
i_order_cor[12,2] <- paste("r=", round(tt4$estimate, 3), "p=", round(tt4$p.value, 3), "(",tt4$parameter,")")
i_order_cor[4,2] <- paste("r=", round(td4$estimate, 3), "p=", round(td4$p.value, 3), "(",td4$parameter,")")

#' Fifth order correlations
dd5<-cor.test(dist_order1[1:length(dist_order5)], dist_order5)
dt5<-cor.test(dist_order1[1:length(ta_order5)], ta_order5)
tt5<-cor.test(ta_order1[1:length(ta_order5)], ta_order5)
td5 <- cor.test(ta_order1[1:length(dist_order5)], dist_order5)
i_order_cor[5,1] <- paste("r=", round(dd5$estimate, 3), "p=", round(dd5$p.value, 3), "(",dd5$parameter,")")
i_order_cor[13,1] <- paste("r=", round(dt5$estimate, 3), "p=", round(dt5$p.value, 3), "(",dt5$parameter,")")
i_order_cor[13,2] <- paste("r=", round(tt5$estimate, 3), "p=", round(tt5$p.value, 3), "(",tt5$parameter,")")
i_order_cor[5,2] <- paste("r=", round(td5$estimate, 3), "p=", round(td5$p.value, 3), "(",td5$parameter,")")

#' Sixth order correlations
dd6<-cor.test(dist_order1[1:length(dist_order6)], dist_order6)
dt6<-cor.test(dist_order1[1:length(ta_order6)], ta_order6)
tt6<-cor.test(ta_order1[1:length(ta_order6)], ta_order6)
td6 <- cor.test(ta_order1[1:length(dist_order6)], dist_order6)
i_order_cor[6,1] <- paste("r=", round(dd6$estimate, 3), "p=", round(dd6$p.value, 3), "(",dd6$parameter,")")
i_order_cor[14,1] <- paste("r=", round(dt6$estimate, 3), "p=", round(dt6$p.value, 3), "(",dt6$parameter,")")
i_order_cor[14,2] <- paste("r=", round(tt6$estimate, 3), "p=", round(tt6$p.value, 3), "(",tt6$parameter,")")
i_order_cor[6,2] <- paste("r=", round(td6$estimate, 3), "p=", round(td6$p.value, 3), "(",td6$parameter,")")

#' Seventh order correlations
dd7<-cor.test(dist_order1[1:length(dist_order7)], dist_order7)
dt7<-cor.test(dist_order1[1:length(ta_order7)], ta_order7)
tt7<-cor.test(ta_order1[1:length(ta_order7)], ta_order7)
td7 <- cor.test(ta_order1[1:length(dist_order7)], dist_order7)
i_order_cor[7,1] <- paste("r=", round(dd7$estimate, 3), "p=", round(dd7$p.value, 3), "(",dd7$parameter,")")
i_order_cor[15,1] <- paste("r=", round(dt7$estimate, 3), "p=", round(dt7$p.value, 3), "(",dt7$parameter,")")
i_order_cor[15,2] <- paste("r=", round(tt7$estimate, 3), "p=", round(tt7$p.value, 3), "(",tt7$parameter,")")
i_order_cor[7,2] <- paste("r=", round(td7$estimate, 3), "p=", round(td7$p.value, 3), "(",td7$parameter,")")

#' Eighth order correlations (except not really because I removed dist 8 and 9 and ta8 from the analysis due to small n)
dd8<-cor.test(dist_order1[1:length(dist_order8)], dist_order8)
dt8<-cor.test(dist_order1[1:length(ta_order8)], ta_order8)
tt8<-cor.test(ta_order1[1:length(ta_order8)], ta_order8)
td8 <- cor.test(ta_order1[1:length(dist_order8)], dist_order8)
i_order_cor[8,1] <- paste("r=", round(dd8$estimate, 3), "p=", round(dd8$p.value, 3), "(",dd8$parameter,")")
i_order_cor[16,1] <- paste("r=", round(dt8$estimate, 3), "p=", round(dt8$p.value, 3), "(",dt8$parameter,")")
i_order_cor[16,2] <- paste("r=", round(tt8$estimate, 3), "p=", round(tt8$p.value, 3), "(",tt8$parameter,")")
i_order_cor[8,2] <- paste("r=", round(td8$estimate, 3), "p=", round(td8$p.value, 3), "(",td8$parameter,")")


i_order_cor

#' Plot significant correlations
plot(ta_order1[1:length(ta_order2)], ta_order2, main="TA1 and TA2")

#' Ta 1 and ta2 are the only parameters that are correlated in this data set, but looking at the scatterplot
#' you wouldn't really know it. It's a negative association witha smallish r value.
