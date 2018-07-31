#Clear data
rm(list=ls())

#Load libraries
library(ggplot2)
library(plyr)
library(dplyr)
library(tidyr)


#import data
dat2016 <- read.csv("data/ground_temp2016.csv", na=".")
dat2017 <- read.csv("data/ground_temp2017.csv", na=".")
dat2016_2 <- read.csv("data/ground.temp.all.csv", na=".")

#' The data from 2016 is already organized in the long format by averages. I need to convert the
#' data from 2017 to long form and calculate averages at each hour.


#' Manipulate data
colnames(dat2016) <- c("substrate", "temp", "time24", "sub.id")
names(dat2017) <- tolower(names(dat2017)) #make variable names lowercase
dat2017 <- gather(dat2017, substrate, temperature,  air.temp:wood.chip) #reorganize dataset to be long rather than wide


#' Need to get the averages for each hour within each substrate
# The command below groups the data by substrate and time then returns the averages
avg_dat2017 <- dat2017 %>% group_by(substrate, time) %>% summarize(m=mean(temperature, na.rm=T),
                                                                   sd=sd(temperature, na.rm=T)) 
avg_dat2016 <- dat2016_2 %>% group_by(surf.type, hour) %>%
  summarize(m=mean(g.temp, na.rm=T), sd=sd(g.temp, na.rm=T))
avg_air2016 <- dat2016_2 %>% group_by(hour) %>% summarize(m=mean(air.temp, na.rm=T), 
                                                        sd=sd(air.temp, na.rm=T))
avg_air2016$surf.type <- as.factor("air")
avg_air2016 <- avg_air2016[,c(4,1,2,3)]

colnames(avg_dat2017) <- c("substrate", "time", "temp") #rename columns

#' Little more data cleaning
avg_dat2017$substrate <- revalue(avg_dat2017$substrate, c("air.temp" = "Air", "big.rock"="Granite",
                                 "dry.mulch" = "Dry Mulch", "gravel" = "Gravel",
                                 "wet.mulch" = "Wet Mulch", "wood.chip" = "Wood Chip")) #rename factor levels
avg_dat2017$time24 <- revalue(avg_dat2017$time, c("08:00" = 8, "09:00" = 9, "10:00" = 10,
                                                  "11:00" = 11, "12:00" = 12, "13:00" = 13,
                                                  "14:00" = 14, "15:00" = 15, "16:00" = 16,
                                                  "17:00" = 17)) #numeric time for graphing
avg_dat2017$time24 <- as.factor(as.numeric(as.character(avg_dat2017$time24))) 
avg_dat2017 <- avg_dat2017 %>% arrange(substrate, time24) #reorder data frame

dat2016$substrate <- revalue(dat2016$substrate, c("air" = "Air", "s.granite"="Granite",
                                                         "d.mulch" = "Dry Mulch", "g.granite" = "Gravel",
                                                         "w.mulch" = "Wet Mulch", "wood" = "Wood Chip"))
dat2016$time <- ifelse(dat2016$time24==6, "06:00",ifelse(dat2016$time24==7, "07:00", 
                     ifelse(dat2016$time24==8, "08:00", ifelse(dat2016$time24==9, "09:00",
                    ifelse(dat2016$time24==10, "10:00", ifelse(dat2016$time24==11, "11:00",
                    ifelse(dat2016$time24==12, "12:00", ifelse(dat2016$time24==13,"13:00",
                    ifelse(dat2016$time24==14, "14:00", ifelse(dat2016$time24==15, "15:00",
                    ifelse(dat2016$time24==16, "16:00", ifelse(dat2016$time24==17, "17:00", "18:00"))))))))))))
dat2016$time <- as.factor(dat2016$time)
avg_dat2016 <- dat2016

#' Plotting
ggplot(avg_dat2017, aes(x=time24, y=temp, group=substrate, colour=substrate)) + geom_line(size=1.25) +
  theme_bw() + labs(x="Time (24H)", y="Temperature"~degree*C, title="Temperature of Air and Different Substrates") +
  guides(colour=guide_legend(overrisde.aes=list(size=5))) + scale_x_discrete(labels=avg_dat2017$time)

ggplot(avg_dat2016, aes(x=time, y=temp, group=substrate, colour=substrate)) + geom_line(size=1.25) +
  theme_bw() + labs(x="Time (24H)", y="Temperature"~degree*C, title="Temperature of Air and Different Substrates") +
  guides(colour=guide_legend(overrisde.aes=list(size=5))) 

