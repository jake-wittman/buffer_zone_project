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

#Import raw data 2016
data <- read.csv("data/raw_data_2016.csv", na=".")

##########################Data cleaning and Derived Variables##########################


#Create a categorical variable for release point (presence of trees = 0, absence of trees = 1)

data$treat_id <- factor(data$treat_id, labels = c("Absence", "Presence"))
data$date <- as.Date(data$date, "%m/%d/%Y")

##########################Calculate (x,y) coordinates of larvae##########################
data$x <- NA
data$y <- NA
for(i in 1:nrow(data)){
  rads<-data$angle[i] * (pi/180)
  data$x[i] <- data$dist_center[i] * sin(rads)
  data$y[i] <- data$dist_center[i] * cos(rads)
  
}

data$x_m <- NA
data$y_m <- NA
data$dist_center_m <- data$dist_center * 0.3048
for(i in 1:nrow(data)){ #calculate x and y in terms of meters
  rads<-data$angle[i] * (pi/180)
  data$x_m[i] <- data$dist_center_m[i] * sin(rads)
  data$y_m[i] <- data$dist_center_m[i] * cos(rads)
  
}
##########################Calculate derived variables##########################
my_tags <- unique(data$qbtag) #Create vector of unique identifiers for each larvae

#Empty columns in data set to hold variables
data$bearing <- NA 
data$ta <- NA
data$card_travel_direction <- NA
data$starting_quad <- NA
data$segment_dist <- NA 
data$m_segment_dist <- NA
data$dist_sum <- NA
data$m_dist_sum <- NA

#Empty vectors or data frames needed for derived variables

mvmt_summary<- as.data.frame(matrix(NA, ncol=7, nrow=length(my_tags))) #empty data frame to hold movement summary information
colnames(mvmt_summary) <- c("qbtag", "total_path_distance", "displacement", 
                            "mvmt_index", "displacement_bearing", "treat_id", "starting_quad")
mvmt_summary$qbtag <- my_tags

#Loop to create temporary data sets for each larvae j
for(j in my_tags){ 
  dat_temp <- data[data$qbtag == j,]
  dat_temp$bearing <- NA
  dat_temp$ta <- NA
  ang <- matrix(NA, ncol=1, nrow=nrow(dat_temp))
  #Loop for calculating starting quadrant
  for(i in 1:nrow(dat_temp)){
    ### Calculate starting quadrant ###
    if(is.na(dat_temp$angle[1])){
       dat_temp$starting_quad[i] <- NA
      }else if(dat_temp$angle[1] == 0){
        dat_temp$starting_quad[i] <- NA
      }else if(dat_temp$angle[1] <= 67.5){
        dat_temp$starting_quad[i] <- "NE"
      } else if(dat_temp$angle[1] <= 157.5){
       dat_temp$starting_quad[i] <- "SE"
      }else if(dat_temp$angle[1] <= 247.5){
       dat_temp$starting_quad[i] <- "SW"
      }else{
       dat_temp$starting_quad[i] <- "NW"
       }
      
  }
  #Loop for calculating different derived variables
  for(i in 2:nrow(dat_temp)){ 
    
    ###Calculate direction of travel relative to north###
    ang[i] <- atan2(dat_temp$x[i] - dat_temp$x[i-1], dat_temp$y[i] - dat_temp$y[i-1]) #calculate bearing in radians
    if(is.na(ang[i])){ #to deal with NA values
      dat_temp$bearing[i] <- NA
    }
    else if(ang[i] >= 0){ 
      dat_temp$bearing[i] <- ang[i] * (180/pi) #converts radians to degrees
    } else{
      dat_temp$bearing[i] <- (ang[i] + 2*pi) * (180/pi) #converts negative radians to positive and then to degrees
    }
    
    ###Calculate cardinal direction of travel###
    if(is.na(dat_temp$bearing[i])){
        dat_temp$card_travel_direction[i] <- NA
      }
      else if(dat_temp$bearing[i] == 0){
        dat_temp$card_travel_direction[i] <- NA
      }
     else if(dat_temp$bearing[i] <= 22.5){
        dat_temp$card_travel_direction[i] <- "N"
      }
      else if(dat_temp$bearing[i] <= 67.5){
        dat_temp$card_travel_direction[i] <- "NE"
      }
      else if(dat_temp$bearing[i] <= 112.5){
        dat_temp$card_travel_direction[i] <- "E"
      }
      else if(dat_temp$bearing[i] <= 157.5){
        dat_temp$card_travel_direction[i] <- "SE"
      }
      else if(dat_temp$bearing[i] <= 202.5){
        dat_temp$card_travel_direction[i] <- "S"
      }
      else if(dat_temp$bearing[i] <= 247.5){
        dat_temp$card_travel_direction[i] <- "SW"
     }
      else if(dat_temp$bearing[i] <= 292.5){
        dat_temp$card_travel_direction[i] <- "W"
     }
      else if(dat_temp$bearing[i] <= 337.5){
       dat_temp$card_travel_direction[i] <- "NW"
     }else{
       dat_temp$card_travel_direction[i] <- "N"
    }
    #calculate segment distance
    dat_temp$segment_dist[i] <- sqrt((dat_temp$x[i] - dat_temp$x[i-1])^2 + (dat_temp$y[i] - dat_temp$y[i-1])^2) #calculate the length of each segment
    dat_temp$m_segment_dist[i] <- dat_temp$segment_dist[i] * 0.3048 #convert segment dist to meters
    dat_temp$dist_sum[1] <- 0 #to create the cumulative distance moved up to this time variable, need to set initial to 0
    dat_temp$dist_sum[i] <- dat_temp$segment_dist[i] + dat_temp$dist_sum[i-1] #add distance moved at step i to previous cumulative sum
    dat_temp$m_dist_sum[1] <-0 #same as above but for meters
    dat_temp$m_dist_sum[i] <- dat_temp$m_segment_dist[i] + dat_temp$m_dist_sum[i-1]
     }
  
  #After looping through temporary data set, save derived variables before starting on next temporary set
  data[data$qbtag == j, "bearing"] <- dat_temp$bearing #stores bearing
  data[data$qbtag == j, "starting_quad"] <- dat_temp$starting_quad #stores starting quadrant
  data[data$qbtag == j, "card_travel_direction"] <- dat_temp$card_travel_direction #stores cardinal direction of travel segment
  mvmt_summary[mvmt_summary$qbtag==j,"starting_quad"] <-dat_temp$starting_quad[1]
  mvmt_summary[mvmt_summary$qbtag==j,"total_path_distance"] <- sum_NA(dat_temp$segment_dist) #save the total path traveled by the larvae
  data[data$qbtag==j,"segment_dist"] <- dat_temp$segment_dist #save the length of each segment distance
  data[data$qbtag==j, "dist_sum"] <- dat_temp$dist_sum 
  data[data$qbtag==j, "m_dist_sum"] <- dat_temp$m_dist_sum
  data[data$qbtag==j, "m_segment_dist"] <- dat_temp$m_segment_dist
  mvmt_summary[mvmt_summary$qbtag==j,"treat_id"] <-dat_temp$treat_id[1]
  
  ##########################Calculate turn angle between segments##########################
  #This first section sets the first turn angle relative to north. So it basically assumes all
  #larvae were facing north when put down and for their first move, turned left or right from north.
  if(nrow(dat_temp) > 1){
    dat_temp$ta[2] <- (dat_temp$bearing[2] - 0) * (pi/180)
  }else{}
  if(is.na(dat_temp$ta[2])){
    dat_temp$ta[2] <- NA
  }else if(dat_temp$ta[2] < -pi){
    dat_temp$ta[2] <- dat_temp$ta[2] + (2*pi)
  }else if(dat_temp$ta[2] > pi){
    dat_temp$ta[2] <- dat_temp$ta[2] - (2*pi)
  }else{
    dat_temp$ta[2] <- dat_temp$ta[2]
  }
  
  for(i in 3:nrow(dat_temp)){ #This loop calculates the turn angle
    dat_temp$ta[i] <-  (dat_temp$bearing[i] - dat_temp$bearing[i-1]) * (pi/180) #calculate turn angle and convert to radians
    if(is.na(dat_temp$ta[i])){#deal with NA values
      dat_temp$ta[i] <- NA
    }
    else if(dat_temp$ta[i] < -pi){ #These next if statements adjust the TA to be [-180, 180]
      dat_temp$ta[i] <- dat_temp$ta[i] + (2* pi)
    }
    else if(dat_temp$ta[i] > pi){
      dat_temp$ta[i] <- dat_temp$ta[i] - (2* pi) 
    }else{
      dat_temp$ta[i] <- dat_temp$ta[i]
    }
  } 
  #After looping through temporary data set, save variables again. Displacement not saved until here because it requires removing rows from the temporary data set.
  data[data$qbtag==j, "ta"] <- dat_temp$ta * (180/pi)#store turn angle
  dat_temp <- dat_temp[!is.na(dat_temp$x),] #remove rows that have NA values in X coordinates to get the coordinates for the last spot the larvae was at
  mvmt_summary[mvmt_summary$qbtag==j,"displacement"] <- 
    sqrt((dat_temp$x[nrow(dat_temp)] - dat_temp$x[1])^2 + (dat_temp$y[nrow(dat_temp)] - dat_temp$y[1])^2) #calculate the displacement (distance between the last spot larvae was recorded at and its starting point)
  
  ang_disp <- atan2(dat_temp$x[nrow(dat_temp)] - dat_temp$x[1], dat_temp$y[nrow(dat_temp)] - dat_temp$y[1]) #calculate displacement bearing in radians
  if(is.na(ang_disp)){ #to deal with NA values
    dat_temp$bearing[i] <- NA
  }
  else if(ang_disp >= 0){ 
    mvmt_summary[mvmt_summary$qbtag==j, "displacement_bearing"] <- ang_disp * (180/pi) #converts radians to degrees
  } else{
    mvmt_summary[mvmt_summary$qbtag==j, "displacement_bearing"] <- (ang_disp + 2*pi) * (180/pi) #converts negative radians to positive and then to degrees
  }
}

data$ta <- ifelse(data$segment_dist == 0, NA, data$ta)
data$bearing[data$segment_dist == 0] <- NA #when segment distance is 0, sets bearing to NA
mvmt_summary$displacement_bearing[mvmt_summary$displacement == 0] <- NA #when displacement is 0, sets displacement bearing to NA
mvmt_summary$mvmt_index <- mvmt_summary$total_path_distance/mvmt_summary$displacement #calculate movement index
mvmt_summary$alt_index <- mvmt_summary$displacement/mvmt_summary$total_path_distance #calculate the opposite ratio for mvmt index
is.na(mvmt_summary) <- sapply(mvmt_summary, is.infinite) #turns Infs in the mvmt summary dataset to NA
is.na(mvmt_summary) <- sapply(mvmt_summary, is.nan) #turns NaNs in the mvmt summary dataset to NA
mvmt_summary$m_total_path <- mvmt_summary$total_path_distance * 0.3048 #onvert total path to meters
mvmt_summary$m_displacement <- mvmt_summary$displacement * 0.3048 #convert displacement to meters
#Set appropriate variables as factors
data$starting_quad <- as.factor(data$starting_quad) 
data$card_travel_direction <- as.factor(data$card_travel_direction)

mvmt_summary$treat_id <- ifelse(mvmt_summary$treat_id == 1, "Absence", "Presence")

write.csv(data, file="data/cleaned_data_2016.csv")
write.csv(mvmt_summary, file="data/summary_data_2016.csv")

##########################################################################################
##############################2017 Import raw data########################################
##########################################################################################


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
data <- read.csv("data/raw_data_2017.csv", na=".")

##########################Data cleaning and Derived Variables##########################


#Create a categorical variable for release point (presence of trees = 0, absence of trees = 1)

data$treat_id <- factor(data$treat_id, labels = c("Absence", "Presence"))
data$treat_fed <- factor(data$treat_fed, labels=c("Fed", "Starved"))
data$date <- as.Date(data$date, "%m/%d/%Y")

##########################Calculate (x,y) coordinates of larvae##########################
data$x <- NA
data$y <- NA 
for(i in 1:nrow(data)){ #calculate x and y in terms of feet
  rads<-data$angle[i] * (pi/180)
  data$x[i] <- data$dist_center[i] * sin(rads)
  data$y[i] <- data$dist_center[i] * cos(rads)
  
}
data$x_m <- NA
data$y_m <- NA
data$dist_center_m <- data$dist_center * 0.3048
for(i in 1:nrow(data)){ #calculate x and y in terms of meters
  rads<-data$angle[i] * (pi/180)
  data$x_m[i] <- data$dist_center_m[i] * sin(rads)
  data$y_m[i] <- data$dist_center_m[i] * cos(rads)
  
}

##########################Calculate derived variables##########################
my_tags <- unique(data$qbtag) #Create vector of unique identifiers for each larvae

#Empty columns in data set to hold variables
data$bearing <- NA 
data$ta <- NA
data$card_travel_direction <- NA
data$starting_quad <- NA
data$segment_dist <- NA 
data$m_segment_dist <- NA
data$dist_sum <- NA
data$m_dist_sum <- NA

#Empty vectors or data frames needed for derived variables

mvmt_summary<- as.data.frame(matrix(NA, ncol=9, nrow=length(my_tags))) #empty data frame to hold movement summary information
colnames(mvmt_summary) <- c("qbtag", "total_path_distance", "displacement", "displacement_bearing", "mvmt_index",
                            "treat_id", "treat_fed", "starting_quad", "instar")
mvmt_summary$qbtag <- my_tags

#Loop to create temporary data sets for each larvae j
for(j in my_tags){ 
  dat_temp <- data[data$qbtag == j,]
  dat_temp$bearing <- NA
  dat_temp$ta <- NA
  ang <- matrix(NA, ncol=1, nrow=nrow(dat_temp))
  if(nrow(dat_temp)>1){ #Need this because we can't do much with any larvae with 1 observation
  #Loop for calculating starting quadrant
  for(i in 1:nrow(dat_temp)){
    ### Calculate starting quadrant ###
    if(is.na(dat_temp$angle[1])){
      dat_temp$starting_quad[i] <- NA
    }else if(dat_temp$angle[1] == 0){
      dat_temp$starting_quad[i] <- NA
    }else if(dat_temp$angle[1] <= 67.5){
      dat_temp$starting_quad[i] <- "NE"
    } else if(dat_temp$angle[1] <= 157.5){
      dat_temp$starting_quad[i] <- "SE"
    }else if(dat_temp$angle[1] <= 247.5){
      dat_temp$starting_quad[i] <- "SW"
    }else{
      dat_temp$starting_quad[i] <- "NW"
    }
    
  }
  #Loop for calculating different derived variables
  for(i in 2:nrow(dat_temp)){ 
    
    ###Calculate direction of travel relative to north###
    ang[i] <- atan2(dat_temp$x[i] - dat_temp$x[i-1], dat_temp$y[i] - dat_temp$y[i-1]) #calculate bearing in radians
    if(is.na(ang[i])){ #to deal with NA values
      dat_temp$bearing[i] <- NA
    }
    else if(ang[i] >= 0){ 
      dat_temp$bearing[i] <- ang[i] * (180/pi) #converts radians to degrees
    } else{
      dat_temp$bearing[i] <- (ang[i] + 2*pi) * (180/pi) #converts negative radians to positive and then to degrees
    }
    
    ###Calculate cardinal direction of travel###
    if(is.na(dat_temp$bearing[i])){
      dat_temp$card_travel_direction[i] <- NA
    }
    else if(dat_temp$bearing[i] == 0){
      dat_temp$card_travel_direction[i] <- NA
    }
    else if(dat_temp$bearing[i] <= 22.5){
      dat_temp$card_travel_direction[i] <- "N"
    }
    else if(dat_temp$bearing[i] <= 67.5){
      dat_temp$card_travel_direction[i] <- "NE"
    }
    else if(dat_temp$bearing[i] <= 112.5){
      dat_temp$card_travel_direction[i] <- "E"
    }
    else if(dat_temp$bearing[i] <= 157.5){
      dat_temp$card_travel_direction[i] <- "SE"
    }
    else if(dat_temp$bearing[i] <= 202.5){
      dat_temp$card_travel_direction[i] <- "S"
    }
    else if(dat_temp$bearing[i] <= 247.5){
      dat_temp$card_travel_direction[i] <- "SW"
    }
    else if(dat_temp$bearing[i] <= 292.5){
      dat_temp$card_travel_direction[i] <- "W"
    }
    else if(dat_temp$bearing[i] <= 337.5){
      dat_temp$card_travel_direction[i] <- "NW"
    }else{
      dat_temp$card_travel_direction[i] <- "N"
    }
    
    dat_temp$segment_dist[i] <- sqrt((dat_temp$x[i] - dat_temp$x[i-1])^2 + (dat_temp$y[i] - dat_temp$y[i-1])^2) #calculate the length of each segment
    dat_temp$m_segment_dist[i] <- dat_temp$segment_dist[i] * 0.3048
    dat_temp$dist_sum[1] <- 0 #used to create cumulative summed distance, need to set initial distance moved to 0
    dat_temp$dist_sum[i] <- dat_temp$segment_dist[i] + dat_temp$dist_sum[i-1] #sum current segment distance with cumulative distance moved
    dat_temp$m_dist_sum[1] <-0
    dat_temp$m_dist_sum[i] <- dat_temp$m_segment_dist[i] + dat_temp$m_dist_sum[i-1]
  }
  
  #After looping through temporary data set, save derived variables before starting on next temporary set
  data[data$qbtag == j, "bearing"] <- dat_temp$bearing #stores bearing
  data[data$qbtag == j, "starting_quad"] <- dat_temp$starting_quad #stores starting quadrant
  data[data$qbtag == j, "card_travel_direction"] <- dat_temp$card_travel_direction #stores cardinal direction of travel segment
  mvmt_summary[mvmt_summary$qbtag==j,"starting_quad"] <-dat_temp$starting_quad[1]
  mvmt_summary[mvmt_summary$qbtag==j,"total_path_distance"] <- sum_NA(dat_temp$segment_dist) #save the total path traveled by the larvae
  data[data$qbtag==j,"segment_dist"] <- dat_temp$segment_dist #save the length of each segment distance
  data[data$qbtag==j, "dist_sum"] <- dat_temp$dist_sum
  data[data$qbtag==j, "m_dist_sum"] <- dat_temp$m_dist_sum
  data[data$qbtag==j, "m_segment_dist"] <- dat_temp$m_segment_dist
  mvmt_summary[mvmt_summary$qbtag==j,"treat_id"] <-dat_temp$treat_id[1]
  mvmt_summary[mvmt_summary$qbtag==j, "treat_fed"] <- dat_temp$treat_fed[1]
  mvmt_summary[mvmt_summary$qbtag==j, "instar"] <- dat_temp$instar[1]
  
  ##########################Calculate turn angle between segments##########################
  #This first section sets the first turn angle relative to north. So it basically assumes all
  #larvae were facing north when put down and for their first move, turned left or right from north.
  if(nrow(dat_temp) > 1){
    dat_temp$ta[2] <- (dat_temp$bearing[2] - 0) * (pi/180)
  }else{}
  if(is.na(dat_temp$ta[2])){
    dat_temp$ta[2] <- NA
  }else if(dat_temp$ta[2] < -pi){
    dat_temp$ta[2] <- dat_temp$ta[2] + (2*pi)
  }else if(dat_temp$ta[2] > pi){
    dat_temp$ta[2] <- dat_temp$ta[2] - (2*pi)
  }else{
    dat_temp$ta[2] <- dat_temp$ta[2]
  }
  
 if(nrow(dat_temp)>2){ #Need this if loop because larvae with less than 3 observations can't have a turn angle calculated
  for(i in 3:nrow(dat_temp)){ #This loop calculates the turn angle
    dat_temp$ta[i] <-  (dat_temp$bearing[i] - dat_temp$bearing[i-1]) * (pi/180) #calculate turn angle and convert to radians
    if(is.na(dat_temp$ta[i])){#deal with NA values
      dat_temp$ta[i] <- NA
    }
    else if(dat_temp$ta[i] < -pi){ #These next if statements adjust the TA to be [-180, 180]
      dat_temp$ta[i] <- dat_temp$ta[i] + (2* pi)
    }
    else if(dat_temp$ta[i] > pi){
      dat_temp$ta[i] <- dat_temp$ta[i] - (2* pi) 
    }else{
      dat_temp$ta[i] <- dat_temp$ta[i]
    }
  }
  }else{dat_temp$ta <- NA}
   
  #After looping through temporary data set, save variables again. Displacement not saved until here because it requires removing rows from the temporary data set.
  data[data$qbtag==j, "ta"] <- dat_temp$ta * (180/pi)#store turn angle
  dat_temp <- dat_temp[!is.na(dat_temp$x),] #remove rows that have NA values in X coordinates to get the coordinates for the last spot the larvae was at
  
    mvmt_summary[mvmt_summary$qbtag==j,"displacement"] <- 
      sqrt((dat_temp$x[nrow(dat_temp)] - dat_temp$x[1])^2 + (dat_temp$y[nrow(dat_temp)] - dat_temp$y[1])^2) #calculate the displacement (distance between the last spot larvae was recorded at and its starting point)
    
  }else{}
  
  ang_disp <- atan2(dat_temp$x[nrow(dat_temp)] - dat_temp$x[1], dat_temp$y[nrow(dat_temp)] - dat_temp$y[1]) #calculate displacement bearing in radians
  if(is.na(ang_disp)){ #to deal with NA values
    dat_temp$bearing[i] <- NA
  }
  else if(ang_disp >= 0){ 
    mvmt_summary[mvmt_summary$qbtag==j, "displacement_bearing"] <- ang_disp * (180/pi) #converts radians to degrees
  } else{
    mvmt_summary[mvmt_summary$qbtag==j, "displacement_bearing"] <- (ang_disp + 2*pi) * (180/pi) #converts negative radians to positive and then to degrees
  }
}

data$ta <- ifelse(data$segment_dist == 0, NA, data$ta)
data$bearing[data$segment_dist == 0] <- NA #when segment distance is 0, sets bearing to NA
mvmt_summary$displacement_bearing[mvmt_summary$displacement == 0] <- NA #when displacement is 0, sets displacement bearing to NA
data$release_time <- ifelse(data$time=="7:30", 0, ifelse(data$time=="8:00", 30, ifelse(data$time=="8:30", 60, 
                            ifelse(data$time=="9:00", 90, ifelse(data$time=="9:30", 120, ifelse(data$time=="10:00", 150,
                            ifelse(data$time=="10:30", 180, ifelse(data$time=="11:00", 210, ifelse(data$time=="11:30", 240,
                            ifelse(data$time=="12:00", 270, ifelse(data$time=="12:30", 300, ifelse(data$time=="13:00", 330,
                            ifelse(data$time=="13:30", 360, ifelse(data$time=="14:00", 390, ifelse(data$time=="14:30", 420,
                            ifelse(data$time=="15:00", 450, ifelse(data$time=="15:30", 480, ifelse(data$time=="16:00", 510,
                            ifelse(data$time=="16:30",540, 570)))))))))))))))))))
mvmt_summary$m_total_path <- mvmt_summary$total_path_distance * 0.3048 #onvert total path to meters
mvmt_summary$m_displacement <- mvmt_summary$displacement * 0.3048 #convert displacement to meters
mvmt_summary$mvmt_index <- mvmt_summary$total_path_distance/mvmt_summary$displacement #calculate movement index
mvmt_summary$alt_index <- mvmt_summary$displacement/mvmt_summary$total_path_distance #calculate the opposite ratio for mvmt index
is.na(mvmt_summary) <- sapply(mvmt_summary, is.infinite) #turns Infs in the mvmt summary dataset to NA
is.na(mvmt_summary) <- sapply(mvmt_summary, is.nan) #turns NaNs in the mvmt summary dataset to NA
#Set appropriate variables as factors
data$starting_quad <- as.factor(data$starting_quad) 
data$card_travel_direction <- as.factor(data$card_travel_direction)

mvmt_summary$treat_id <- ifelse(mvmt_summary$treat_id==1, "Absence", "Presence")
mvmt_summary$treat_fed <- ifelse(mvmt_summary$treat_fed == 1, "Fed", "Starved")


write.csv(data, file="data/cleaned_data_2017.csv")
write.csv(mvmt_summary, file="data/summary_data_2017.csv")
