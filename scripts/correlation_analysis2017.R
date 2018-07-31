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

#' Import data
data <- read.csv("data/cleaned_data_2017.csv", na.strings="NA")

#' Data cleaning
data$x_m <- as.numeric(data$x_m)
data$y_m <- as.numeric(data$y_m)
data <- data[data$qbtag != 30.1, ]  #Larva 30.1 had only 1 observation so cannot be used for this
data <- data[data$qbtag != 43, ]  #Larva 30.1 had only 1 observation so cannot be used for this
data <- data[data$qbtag != 59, ]  #Larva 30.1 had only 1 observation so cannot be used for this

#' I want to calculate the ith order correlations between the jth distance/turning angle and successive distance/turning angle i moves away (so
#' the 2nd order correlation is the correlation between distance moved at time 1 and time 2, time 2 and time 3, etc. This can be distance vs distance,
#' time vs time, or distance vs time). These are serial correlations.
#' I also want to calculate the correlation between the ith distance moved and the ith turning angle. This is a first order correlation.
#' This is all from the Andow and Kiritani 1984 paper "Fine Structure of Trivial Movement in the Green Rice Leafhopper"
##############More Data Cleaning################################

#' Following Andow and Kiritani 1984. 
#' Create empty matrix to store data
my_tags <- unique(data$qbtag) #creates a vector of each unique QB tag identifier
cor_data <- as.data.frame(matrix(NA, nrow=length(my_tags), ncol=41)) #matrix to store average displacements
colnames(cor_data) <- c("qbtag","dist1","dist2", "dist3", "dist4", "dist5", "dist6", "dist7", "dist8", "dist9", "dist10",
                        "dist 11", "dist 12", "dist 13", "dist 14", "dist 15", "dist 16", "dist 17", "dist 18", "dist 19", 
                        "ta1","ta2", "ta3", "ta4", "ta5", "ta6", "ta7", "ta8", "ta9", "ta10",
                        "ta11", "ta12", "ta13", "ta14", "ta15", "ta16", "ta17", "ta18",
                        "veg_treatment", "fed_treatment", "instar") #name columns of matrix
cor_data$qbtag <- my_tags #fill out column of tag label
#data$ta <- ifelse(data$ta < 0, data$ta + 360, data$ta + 0) #change turning angle to be between (0, 360) for correlation later

#' These loops store the distance and turning angle values for each larvae
for(j in my_tags){#This for loop goes through the dataset and creates a temporary data set containing only the observations for larvae j
  dat_temp <- data[data$qbtag==j,]
  cor_data[cor_data$qbtag==j,"veg_treatment"] <- dat_temp$treat_id[1] #store vegetation treatment, 1 = Absent, 2 = Presence
  cor_data[cor_data$qbtag==j,"fed_treatment"] <- dat_temp$treat_fed[1] #store fed treatment, 1=Fed, 2=Starved
  cor_data[cor_data$qbtag==j,"instar"] <- dat_temp$instar[1]
  for(i in 2:nrow(dat_temp)){ #this for loop stores the distance moved and turning angle of each move
    cor_data[cor_data$qbtag==j,i] <- dat_temp$m_segment_dist[i] #store the distance moved in move i in the appropriate column
    cor_data[cor_data$qbtag==j,i+18] <- dat_temp$ta[i] #store the turning angle for move i in the appropriate column
    #The line adding ta to the cor_data data frame is a little weird. Technically the turning angle at i=2 will
    #always be NA because you need 3 moves to make a turning angle. When i=2, i+18 = 20 which is the column
    #for the distance moved at step 19 (column 20). So at i = 2, an NA value gets placed in column 20. When
    # the loop keeps going at gets to i=20 (the last row in the dat_temp frame), it overwrites the NA with the
    #distance moved at step 19. So ta1 is truly the first turn angle available.
    }
  }

cor_data_absent <- subset(cor_data, veg_treatment==1) #subset by those on the ground, the ones in the trees hardly moved

for(i in 1:ncol(cor_data)){ #This tells me how many observations I have for each ith move
  print(sum(!is.na(cor_data_absent[,i])))
}

for(i in 1:ncol(cor_data)){ #this tells me how many observations I have that aren't 0 or NA for each ith move
  print(sum_NA(cor_data_absent[,i] != 0))
}


#' There should be enough observations in each step that I don't have to remove any. I think.

cor_results <- as.data.frame(matrix(NA, ncol=(ncol(cor_data_absent)-4), nrow=(ncol(cor_data_absent)-4))) #set up data frame to store results
colnames(cor_results) <- c("dist1","dist2", "dist3", "dist4", "dist5", "dist6", "dist7","dist8","dist9", "dist10", 
                           "dist11", "dist12", "dist13", "dist14", "dist15", "dist16", "dist17", "dist18", "dist19",
                           "ta1", "ta2", "ta3", "ta4", "ta5", "ta6", "ta7", "ta8", "ta9", "ta10", "ta11",
                           "ta12", "ta13", "ta14", "ta15", "ta16", "ta17", "ta18") #name columns of matrix
rownames(cor_results) <- c("dist1","dist2", "dist3", "dist4", "dist5", "dist6", "dist7","dist8","dist9", "dist10", 
                           "dist11", "dist12", "dist13", "dist14", "dist15", "dist16", "dist17", "dist18", "dist19",
                           "ta1", "ta2", "ta3", "ta4", "ta5", "ta6", "ta7", "ta8", "ta9", "ta10", "ta11",
                           "ta12", "ta13", "ta14", "ta15", "ta16", "ta17", "ta18") #name rows of matrix

##################### Pairwise Correlation Matrix ###################################
#' This for loop calculates all the pairwise correlations between dist & dist, dist& ta, and ta & ta. I don't think this is what I wanted, necessarily.
for(j in 1:(ncol(cor_data_absent)-4)){
  tempj <- cor_data_absent[,j] #seperate the column used to calculate the correlation temporarily
  for(i in 1:(ncol(cor_data_absent)-4)){
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
dist_cor <- cor_data_absent[,2:19] #seperate the correlation data to make the data wrangling a bit easier
ta_cor <- cor_data_absent[,20:37] #same as above comment


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

#Make 8th order events (comparing step i to step i+8)
dist_order8 <- NA
ta_order8 <- NA
for(i in 8:ncol(dist_cor)){
  dist_order8 <- c(dist_order8, dist_cor[,i])
  ta_order8 <- c(ta_order8, ta_cor[,i])
}
dist_order8 <- dist_order8[2:length(dist_order8)]
ta_order8 <- ta_order8[2:length(ta_order8)]

#Make 9th order events (comparing step i to step i+9)
dist_order9 <- NA
ta_order9 <- NA
for(i in 9:ncol(dist_cor)){
  dist_order9 <- c(dist_order9, dist_cor[,i])
  ta_order9 <- c(ta_order9, ta_cor[,i])
}
dist_order9 <- dist_order9[2:length(dist_order9)]
ta_order9 <- ta_order9[2:length(ta_order9)]

#Make 10th order events (comparing step i to step i+10)
dist_order10 <- NA
ta_order10 <- NA
for(i in 10:ncol(dist_cor)){
  dist_order10 <- c(dist_order10, dist_cor[,i])
  ta_order10 <- c(ta_order10, ta_cor[,i])
}
dist_order10 <- dist_order10[2:length(dist_order10)]
ta_order10 <- ta_order10[2:length(ta_order10)]

#Make 11th order events (comparing step i to step i+11)
dist_order11 <- NA
ta_order11 <- NA
for(i in 11:ncol(dist_cor)){
  dist_order11 <- c(dist_order11, dist_cor[,i])
  ta_order11 <- c(ta_order11, ta_cor[,i])
}
dist_order11 <- dist_order11[2:length(dist_order11)]
ta_order11 <- ta_order11[2:length(ta_order11)]

#Make 12th order events (comparing step i to step i+12)
dist_order12 <- NA
ta_order12 <- NA
for(i in 12:ncol(dist_cor)){
  dist_order12 <- c(dist_order12, dist_cor[,i])
  ta_order12 <- c(ta_order12, ta_cor[,i])
}
dist_order12 <- dist_order12[2:length(dist_order12)]
ta_order12 <- ta_order12[2:length(ta_order12)]

#Make 13th order events (comparing step i to step i+13)
dist_order13 <- NA
ta_order13 <- NA
for(i in 13:ncol(dist_cor)){
  dist_order13 <- c(dist_order13, dist_cor[,i])
  ta_order13 <- c(ta_order13, ta_cor[,i])
}
dist_order13 <- dist_order13[2:length(dist_order13)]
ta_order13 <- ta_order13[2:length(ta_order13)]

#Make 14th order events (comparing step i to step i+14)
dist_order14 <- NA
ta_order14 <- NA
for(i in 14:ncol(dist_cor)){
  dist_order14 <- c(dist_order14, dist_cor[,i])
  ta_order14 <- c(ta_order14, ta_cor[,i])
}
dist_order14 <- dist_order14[2:length(dist_order14)]
ta_order14 <- ta_order14[2:length(ta_order14)]

#Make 15th order events (comparing step i to step i+15)
dist_order15 <- NA
ta_order15 <- NA
for(i in 15:ncol(dist_cor)){
  dist_order15 <- c(dist_order15, dist_cor[,i])
  ta_order15 <- c(ta_order15, ta_cor[,i])
}
dist_order15 <- dist_order15[2:length(dist_order15)]
ta_order15 <- ta_order15[2:length(ta_order15)]

#Make 16th order events (comparing step i to step i+16)
dist_order16 <- NA
ta_order16 <- NA
for(i in 16:ncol(dist_cor)){
  dist_order16 <- c(dist_order16, dist_cor[,i])
  ta_order16 <- c(ta_order16, ta_cor[,i])
}
dist_order16 <- dist_order16[2:length(dist_order16)]
ta_order16 <- ta_order16[2:length(ta_order16)]

#Make 17th order events (comparing step i to step i+17)
dist_order17 <- NA
ta_order17 <- NA
for(i in 17:ncol(dist_cor)){
  dist_order17 <- c(dist_order17, dist_cor[,i])
  ta_order17 <- c(ta_order17, ta_cor[,i])
}
dist_order17 <- dist_order17[2:length(dist_order17)]
ta_order17 <- ta_order17[2:length(ta_order17)]

#Make 18th order events (comparing step i to step i+18)
dist_order18 <- NA
ta_order18 <- NA
for(i in 18:ncol(dist_cor)){
  dist_order18 <- c(dist_order18, dist_cor[,i])
  ta_order18 <- c(ta_order18, ta_cor[,i])
}
dist_order18 <- dist_order18[2:length(dist_order18)]
ta_order18 <- ta_order18[2:length(ta_order18)]

} #The commands in this bracketed section make all the vectors used to do the ith order correlations.

i_order_cor<- as.data.frame(matrix(NA, ncol=2, nrow=18*2)) #set up data frame to store results
colnames(i_order_cor) <- c("dist1","ta1") #name columns of matrix
rownames(i_order_cor) <- c("dist1","dist2", "dist3", "dist4", "dist5", "dist6", "dist7","dist8","dist9", "dist10", 
                           "dist11", "dist12", "dist13", "dist14", "dist15", "dist16", "dist17", "dist18", 
                           "ta1", "ta2", "ta3", "ta4", "ta5", "ta6", "ta7", "ta8", "ta9", "ta10", "ta11",
                           "ta12", "ta13", "ta14", "ta15", "ta16", "ta17", "ta18" ) #name rows of matrix

#'First order correlations
dt1<-cor.test(dist_order1[1:length(ta_order1)], ta_order1)
td1 <- cor.test(ta_order1[1:length(dist_order1)], dist_order1)
i_order_cor[19,1] <- paste("r=", round(dt1$estimate, 3), "p=", round(dt1$p.value, 3), "(",dt1$parameter,")")
i_order_cor[1,2] <- paste("r=", round(td1$estimate, 3), "p=", round(td1$p.value, 3), "(",td1$parameter,")")

#' Second order correlations
dd2<-cor.test(dist_order1[1:length(dist_order2)], dist_order2)
dt2<-cor.test(dist_order1[1:length(ta_order2)], ta_order2)
tt2<-cor.test(ta_order1[1:length(ta_order2)], ta_order2)
td2 <- cor.test(ta_order1[1:length(dist_order2)], dist_order2)
i_order_cor[2,1] <- paste("r=", round(dd2$estimate, 3), "p=", round(dd2$p.value, 3), "(",dd2$parameter,")")
i_order_cor[20,1] <- paste("r=", round(dt2$estimate, 3), "p=", round(dt2$p.value, 3), "(",dt2$parameter,")")
i_order_cor[20,2] <- paste("r=", round(tt2$estimate, 3), "p=", round(tt2$p.value, 3), "(",tt2$parameter,")")
i_order_cor[2,2] <- paste("r=", round(td2$estimate, 3), "p=", round(td2$p.value, 3), "(",td2$parameter,")")

#' Third order correlations
dd3<-cor.test(dist_order1[1:length(dist_order3)], dist_order3)
dt3<-cor.test(dist_order1[1:length(ta_order3)], ta_order3)
tt3<-cor.test(ta_order1[1:length(ta_order3)], ta_order3)
td3 <- cor.test(ta_order1[1:length(dist_order3)], dist_order3)
i_order_cor[3,1] <- paste("r=", round(dd3$estimate, 3), "p=", round(dd3$p.value, 3), "(",dd3$parameter,")")
i_order_cor[21,1] <- paste("r=", round(dt3$estimate, 3), "p=", round(dt3$p.value, 3), "(",dt3$parameter,")")
i_order_cor[21,2] <- paste("r=", round(tt3$estimate, 3), "p=", round(tt3$p.value, 3), "(",tt3$parameter,")")
i_order_cor[3,2] <- paste("r=", round(td3$estimate, 3), "p=", round(td3$p.value, 3), "(",td3$parameter,")")

#' Fourth order correlations
dd4<-cor.test(dist_order1[1:length(dist_order4)], dist_order4)
dt4<-cor.test(dist_order1[1:length(ta_order4)], ta_order4)
tt4<-cor.test(ta_order1[1:length(ta_order4)], ta_order4)
td4 <- cor.test(ta_order1[1:length(dist_order4)], dist_order4)
i_order_cor[4,1] <- paste("r=", round(dd4$estimate, 3), "p=", round(dd4$p.value, 3), "(",dd4$parameter,")")
i_order_cor[22,1] <- paste("r=", round(dt4$estimate, 3), "p=", round(dt4$p.value, 3), "(",dt4$parameter,")")
i_order_cor[22,2] <- paste("r=", round(tt4$estimate, 3), "p=", round(tt4$p.value, 3), "(",tt4$parameter,")")
i_order_cor[4,2] <- paste("r=", round(td4$estimate, 3), "p=", round(td4$p.value, 3), "(",td4$parameter,")")

#' Fifth order correlations
dd5<-cor.test(dist_order1[1:length(dist_order5)], dist_order5)
dt5<-cor.test(dist_order1[1:length(ta_order5)], ta_order5)
tt5<-cor.test(ta_order1[1:length(ta_order5)], ta_order5)
td5 <- cor.test(ta_order1[1:length(dist_order5)], dist_order5)
i_order_cor[5,1] <- paste("r=", round(dd5$estimate, 3), "p=", round(dd5$p.value, 3), "(",dd5$parameter,")")
i_order_cor[23,1] <- paste("r=", round(dt5$estimate, 3), "p=", round(dt5$p.value, 3), "(",dt5$parameter,")")
i_order_cor[23,2] <- paste("r=", round(tt5$estimate, 3), "p=", round(tt5$p.value, 3), "(",tt5$parameter,")")
i_order_cor[5,2] <- paste("r=", round(td5$estimate, 3), "p=", round(td5$p.value, 3), "(",td5$parameter,")")

#' Sixth order correlations
dd6<-cor.test(dist_order1[1:length(dist_order6)], dist_order6)
dt6<-cor.test(dist_order1[1:length(ta_order6)], ta_order6)
tt6<-cor.test(ta_order1[1:length(ta_order6)], ta_order6)
td6 <- cor.test(ta_order1[1:length(dist_order6)], dist_order6)
i_order_cor[6,1] <- paste("r=", round(dd6$estimate, 3), "p=", round(dd6$p.value, 3), "(",dd6$parameter,")")
i_order_cor[24,1] <- paste("r=", round(dt6$estimate, 3), "p=", round(dt6$p.value, 3), "(",dt6$parameter,")")
i_order_cor[24,2] <- paste("r=", round(tt6$estimate, 3), "p=", round(tt6$p.value, 3), "(",tt6$parameter,")")
i_order_cor[6,2] <- paste("r=", round(td6$estimate, 3), "p=", round(td6$p.value, 3), "(",td6$parameter,")")

#' Seventh order correlations
dd7<-cor.test(dist_order1[1:length(dist_order7)], dist_order7)
dt7<-cor.test(dist_order1[1:length(ta_order7)], ta_order7)
tt7<-cor.test(ta_order1[1:length(ta_order7)], ta_order7)
td7 <- cor.test(ta_order1[1:length(dist_order7)], dist_order7)
i_order_cor[7,1] <- paste("r=", round(dd7$estimate, 3), "p=", round(dd7$p.value, 3), "(",dd7$parameter,")")
i_order_cor[25,1] <- paste("r=", round(dt7$estimate, 3), "p=", round(dt7$p.value, 3), "(",dt7$parameter,")")
i_order_cor[25,2] <- paste("r=", round(tt7$estimate, 3), "p=", round(tt7$p.value, 3), "(",tt7$parameter,")")
i_order_cor[7,2] <- paste("r=", round(td7$estimate, 3), "p=", round(td7$p.value, 3), "(",td7$parameter,")")

#' Eighth order correlations 
dd8<-cor.test(dist_order1[1:length(dist_order8)], dist_order8)
dt8<-cor.test(dist_order1[1:length(ta_order8)], ta_order8)
tt8<-cor.test(ta_order1[1:length(ta_order8)], ta_order8)
td8 <- cor.test(ta_order1[1:length(dist_order8)], dist_order8)
i_order_cor[8,1] <- paste("r=", round(dd8$estimate, 3), "p=", round(dd8$p.value, 3), "(",dd8$parameter,")")
i_order_cor[26,1] <- paste("r=", round(dt8$estimate, 3), "p=", round(dt8$p.value, 3), "(",dt8$parameter,")")
i_order_cor[26,2] <- paste("r=", round(tt8$estimate, 3), "p=", round(tt8$p.value, 3), "(",tt8$parameter,")")
i_order_cor[8,2] <- paste("r=", round(td8$estimate, 3), "p=", round(td8$p.value, 3), "(",td8$parameter,")")

#' Ninth order correlations 
dd9<-cor.test(dist_order1[1:length(dist_order9)], dist_order9)
dt9<-cor.test(dist_order1[1:length(ta_order9)], ta_order9)
tt9<-cor.test(ta_order1[1:length(ta_order9)], ta_order9)
td9 <- cor.test(ta_order1[1:length(dist_order9)], dist_order9)
i_order_cor[9,1] <- paste("r=", round(dd9$estimate, 3), "p=", round(dd9$p.value, 3), "(",dd9$parameter,")")
i_order_cor[27,1] <- paste("r=", round(dt9$estimate, 3), "p=", round(dt9$p.value, 3), "(",dt9$parameter,")")
i_order_cor[27,2] <- paste("r=", round(tt9$estimate, 3), "p=", round(tt9$p.value, 3), "(",tt9$parameter,")")
i_order_cor[9,2] <- paste("r=", round(td9$estimate, 3), "p=", round(td9$p.value, 3), "(",td9$parameter,")")

#' 10th order correlations 
dd10<-cor.test(dist_order1[1:length(dist_order10)], dist_order10)
dt10<-cor.test(dist_order1[1:length(ta_order10)], ta_order10)
tt10<-cor.test(ta_order1[1:length(ta_order10)], ta_order10)
td10 <- cor.test(ta_order1[1:length(dist_order10)], dist_order10)
i_order_cor[10,1] <- paste("r=", round(dd10$estimate, 3), "p=", round(dd10$p.value, 3), "(",dd10$parameter,")")
i_order_cor[28,1] <- paste("r=", round(dt10$estimate, 3), "p=", round(dt10$p.value, 3), "(",dt10$parameter,")")
i_order_cor[28,2] <- paste("r=", round(tt10$estimate, 3), "p=", round(tt10$p.value, 3), "(",tt10$parameter,")")
i_order_cor[10,2] <- paste("r=", round(td10$estimate, 3), "p=", round(td10$p.value, 3), "(",td10$parameter,")")

#' 11th order correlations 
dd11<-cor.test(dist_order1[1:length(dist_order11)], dist_order11)
dt11<-cor.test(dist_order1[1:length(ta_order11)], ta_order11)
tt11<-cor.test(ta_order1[1:length(ta_order11)], ta_order11)
td11 <- cor.test(ta_order1[1:length(dist_order11)], dist_order11)
i_order_cor[11,1] <- paste("r=", round(dd11$estimate, 3), "p=", round(dd11$p.value, 3), "(",dd11$parameter,")")
i_order_cor[29,1] <- paste("r=", round(dt11$estimate, 3), "p=", round(dt11$p.value, 3), "(",dt11$parameter,")")
i_order_cor[29,2] <- paste("r=", round(tt11$estimate, 3), "p=", round(tt11$p.value, 3), "(",tt11$parameter,")")
i_order_cor[11,2] <- paste("r=", round(td11$estimate, 3), "p=", round(td11$p.value, 3), "(",td11$parameter,")")

#' 12 order correlations 
dd12<-cor.test(dist_order1[1:length(dist_order12)], dist_order12)
dt12<-cor.test(dist_order1[1:length(ta_order12)], ta_order12)
tt12<-cor.test(ta_order1[1:length(ta_order12)], ta_order12)
td12 <- cor.test(ta_order1[1:length(dist_order12)], dist_order12)
i_order_cor[12,1] <- paste("r=", round(dd12$estimate, 3), "p=", round(dd12$p.value, 3), "(",dd12$parameter,")")
i_order_cor[30,1] <- paste("r=", round(dt12$estimate, 3), "p=", round(dt12$p.value, 3), "(",dt12$parameter,")")
i_order_cor[30,2] <- paste("r=", round(tt12$estimate, 3), "p=", round(tt12$p.value, 3), "(",tt12$parameter,")")
i_order_cor[12,2] <- paste("r=", round(td12$estimate, 3), "p=", round(td12$p.value, 3), "(",td12$parameter,")")

#' 13 order correlations 
dd13<-cor.test(dist_order1[1:length(dist_order13)], dist_order13)
dt13<-cor.test(dist_order1[1:length(ta_order13)], ta_order13)
tt13<-cor.test(ta_order1[1:length(ta_order13)], ta_order13)
td13 <- cor.test(ta_order1[1:length(dist_order13)], dist_order13)
i_order_cor[13,1] <- paste("r=", round(dd13$estimate, 3), "p=", round(dd13$p.value, 3), "(",dd13$parameter,")")
i_order_cor[31,1] <- paste("r=", round(dt13$estimate, 3), "p=", round(dt13$p.value, 3), "(",dt13$parameter,")")
i_order_cor[31,2] <- paste("r=", round(tt13$estimate, 3), "p=", round(tt13$p.value, 3), "(",tt13$parameter,")")
i_order_cor[13,2] <- paste("r=", round(td13$estimate, 3), "p=", round(td13$p.value, 3), "(",td13$parameter,")")

#' 14 order correlations 
dd14<-cor.test(dist_order1[1:length(dist_order14)], dist_order14)
dt14<-cor.test(dist_order1[1:length(ta_order14)], ta_order14)
tt14<-cor.test(ta_order1[1:length(ta_order14)], ta_order14)
td14 <- cor.test(ta_order1[1:length(dist_order14)], dist_order14)
i_order_cor[14,1] <- paste("r=", round(dd14$estimate, 3), "p=", round(dd14$p.value, 3), "(",dd14$parameter,")")
i_order_cor[32,1] <- paste("r=", round(dt14$estimate, 3), "p=", round(dt14$p.value, 3), "(",dt14$parameter,")")
i_order_cor[32,2] <- paste("r=", round(tt14$estimate, 3), "p=", round(tt14$p.value, 3), "(",tt14$parameter,")")
i_order_cor[14,2] <- paste("r=", round(td14$estimate, 3), "p=", round(td14$p.value, 3), "(",td14$parameter,")")

#' 15 order correlations 
dd15<-cor.test(dist_order1[1:length(dist_order15)], dist_order15)
dt15<-cor.test(dist_order1[1:length(ta_order15)], ta_order15)
tt15<-cor.test(ta_order1[1:length(ta_order15)], ta_order15)
td15 <- cor.test(ta_order1[1:length(dist_order15)], dist_order15)
i_order_cor[15,1] <- paste("r=", round(dd15$estimate, 3), "p=", round(dd15$p.value, 3), "(",dd15$parameter,")")
i_order_cor[33,1] <- paste("r=", round(dt15$estimate, 3), "p=", round(dt15$p.value, 3), "(",dt15$parameter,")")
i_order_cor[33,2] <- paste("r=", round(tt15$estimate, 3), "p=", round(tt15$p.value, 3), "(",tt15$parameter,")")
i_order_cor[15,2] <- paste("r=", round(td15$estimate, 3), "p=", round(td15$p.value, 3), "(",td15$parameter,")")

#' 16 order correlations 
dd16<-cor.test(dist_order1[1:length(dist_order16)], dist_order16)
dt16<-cor.test(dist_order1[1:length(ta_order16)], ta_order16)
tt16<-cor.test(ta_order1[1:length(ta_order16)], ta_order16)
td16 <- cor.test(ta_order1[1:length(dist_order16)], dist_order16)
i_order_cor[16,1] <- paste("r=", round(dd16$estimate, 3), "p=", round(dd16$p.value, 3), "(",dd16$parameter,")")
i_order_cor[34,1] <- paste("r=", round(dt16$estimate, 3), "p=", round(dt16$p.value, 3), "(",dt16$parameter,")")
i_order_cor[34,2] <- paste("r=", round(tt16$estimate, 3), "p=", round(tt16$p.value, 3), "(",tt16$parameter,")")
i_order_cor[16,2] <- paste("r=", round(td16$estimate, 3), "p=", round(td16$p.value, 3), "(",td16$parameter,")")

#' 17 order correlations 
dd17<-cor.test(dist_order1[1:length(dist_order17)], dist_order17)
dt17<-cor.test(dist_order1[1:length(ta_order17)], ta_order17)
tt17<-cor.test(ta_order1[1:length(ta_order17)], ta_order17)
td17 <- cor.test(ta_order1[1:length(dist_order17)], dist_order17)
i_order_cor[17,1] <- paste("r=", round(dd17$estimate, 3), "p=", round(dd17$p.value, 3), "(",dd17$parameter,")")
i_order_cor[35,1] <- paste("r=", round(dt17$estimate, 3), "p=", round(dt17$p.value, 3), "(",dt17$parameter,")")
i_order_cor[35,2] <- paste("r=", round(tt17$estimate, 3), "p=", round(tt17$p.value, 3), "(",tt17$parameter,")")
i_order_cor[17,2] <- paste("r=", round(td17$estimate, 3), "p=", round(td17$p.value, 3), "(",td17$parameter,")")

#' 18 order correlations 
dd18<-cor.test(dist_order1[1:length(dist_order18)], dist_order18)
dt18<-cor.test(dist_order1[1:length(ta_order18)], ta_order18)
tt18<-cor.test(ta_order1[1:length(ta_order18)], ta_order18)
td18 <- cor.test(ta_order1[1:length(dist_order18)], dist_order18)
i_order_cor[18,1] <- paste("r=", round(dd18$estimate, 3), "p=", round(dd18$p.value, 3), "(",dd18$parameter,")")
i_order_cor[36,1] <- paste("r=", round(dt18$estimate, 3), "p=", round(dt18$p.value, 3), "(",dt18$parameter,")")
i_order_cor[36,2] <- paste("r=", round(tt18$estimate, 3), "p=", round(tt18$p.value, 3), "(",tt18$parameter,")")
i_order_cor[18,2] <- paste("r=", round(td18$estimate, 3), "p=", round(td18$p.value, 3), "(",td18$parameter,")")

i_order_cor

#' Plot significant correlations
plot(dist_order1[1:length(dist_order2)], dist_order2, main = "Dist1 and Dist2")
plot(dist_order1[1:length(dist_order3)], dist_order3, main = "Dist1 and Dist3")
plot(dist_order1[1:length(dist_order4)], dist_order4, main = "Dist1 and Dist4")
plot(dist_order1[1:length(dist_order5)], dist_order5, main = "Dist1 and Dist5")
plot(dist_order1[1:length(dist_order6)], dist_order6, main = "Dist1 and Dist6")
plot(dist_order1[1:length(dist_order7)], dist_order7, main = "Dist1 and Dist7")
plot(dist_order1[1:length(dist_order8)], dist_order8, main = "Dist1 and Dist8")
plot(dist_order1[1:length(dist_order9)], dist_order9, main = "Dist1 and Dist9")
plot(dist_order1[1:length(dist_order10)], dist_order10, main = "Dist1 and Dist10")
plot(ta_order1[1:length(ta_order2)], ta_order2, main="TA1 and TA2")
plot(ta_order1[1:length(ta_order5)], ta_order5, main="TA1 and TA5")
plot(ta_order1[1:length(dist_order2)], dist_order2, main="TA1 and Dist2")
plot(ta_order1[1:length(dist_order18)], dist_order18, main="TA1 and Dist18")

#' Looking at the ith order correlations, distance moved between observations is correlated as far
#' as the 10th step, and the actual correlation value is >0.1 for steps 1-6. Turn angle is not
#' with distance moved. Turn angle is correlated with itself for the 2nd and 5th step. The correlation
#' coefficient is relatively small for both (-0.187 and -0.077, respectively). Because they
#' are negative, that indicates that they have a tendency to switch direction between moves. 
#' Interestingly, when you plot ta_order1 against ta_order 2 there is a clear diagonal line with a 
#' negative slope bisecting quadrants 4 and 2 .
#' Distance moved at moves 2 and 18 are correlated with turning angle at the first point. The 18th step correlation seems
#' iffy - there are relatively few data points. I think this is a spurious correlation. Why would it be correlated so far down
#' but with only one other? Comparing the correlation for these two variables is kind of weird because of the nature of
#' turn angle, so I'd be careful interpreting any of the dist/ta correlations without looking at graphical representation.
#' 
#' 
#' Spun with ezspin("scripts/correlation_analysis2017.R", out_dir="spin_output", fig_dir="figures", keep_md=F)