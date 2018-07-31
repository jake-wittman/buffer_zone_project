#' This script can be used to look at plots of the movement paths of each individual larvae. Useful for 
#' checking data entry accuracy.
#' 

library(moveHMM)

#import data
mvmt <- read.csv("data/cleaned_data_2017.csv", na.strings=".")
mvmt <- mvmt[,c("date","qbtag","x", "y", "g_temp")]

colnames(mvmt)[2] <- "ID"
colnames(mvmt)[3]<-"Easting"
colnames(mvmt)[4] <-"Northing"

data <- prepData(mvmt, type="UTM", coordNames=c("Easting","Northing"))
summary(data)
plot(data, compact=F)
