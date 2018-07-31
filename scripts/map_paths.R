#load library
library("plotrix")

##################################2016 map###############################################
#get data
data <- read.csv("data/cleaned_data_2016.csv", na.strings="NA")
data$dist_center <- as.numeric(as.character(data$dist_center))
dat <- data[(!is.na(data$ground)) & data$ground == 1,c("qbtag", "date", "release_time", "dist_center_m", "angle", "ground", "treat_id")]

#create legend if applicable
#standard - 65 larval paths
tiff(filename="images/map_paths_2016.tiff", width=1000, height = 1000, units="px", res=200)
#plot.new()
frame()
#pdf("standard.pdf", width=7, height=7)
par(pch = 20)
par(mar=c(4.5, 4.5, 1, 1))
plot(NULL, xlab = "Distance (m)", ylab = "Distance(m)", xlim = c(-40, 40), ylim = c(-40, 40),
     yaxt = "n", xaxt="n")
axis(2, at = seq(from = -40, to=40, by =10))
axis(1, at = seq(-40, 40, 10))
title("Paths of Larvae on Ground 2016", line = -1 )
draw.circle(0, 0, 30, nv = 1000, border = NULL, col = NA, lty = 1, lwd = 1)


#for each caterpillar, plot points
prevID <- -1
for(i in 1:nrow(dat)) {
  
  catID <- dat[i,][1]
  
  #check that row is not empty
  if(is.na(catID)) {
    cat("End reached.\n")
    break
  }
  
  #don't repeat id's
  if(as.numeric(catID) <= as.numeric(prevID)) {
    next
  }
  
  date <- dat[i,][2]
  
  
  cat("||||||||||||||||||||||||||| ID: ", as.numeric(catID), "|||||||||||||||||||||||||||||||\n")
  
  #set initial point
  distance <- dat[i,][4]
  angle <- dat[i,][5]
  pointA <- c(0,0)
  deltaX1 <- as.numeric(distance) * cos(as.numeric(angle) * (pi / 180))
  deltaY1 <- as.numeric(distance) * sin(as.numeric(angle) * (pi / 180))
  pointA[1] <- deltaX1
  pointA[2] <- deltaY1
  pointB <- c(0, 0)
  
  j <- i
  dateToComp <- dat[j,][2]
  while(isTRUE(all.equal(date[1,], dateToComp[1,]))) {
    #get ID for current row (j)
    toComp <- dat[j,][1]
    
    cat("i = ", i, " catID = ", as.numeric(catID), " j = ", j, " toComp = ", as.numeric(toComp), "\n")
    
    if(isTRUE(all.equal(catID[1,], toComp[1,]))) {
      
      #get distance and angle
      distance <- dat[j,][4]
      angle <- dat[j,][5]
      
      if(!((as.character(distance) == '.' || as.character(angle) == '.'))) {
        #get x and y for next point
        deltaX <- as.numeric(distance) * cos(as.numeric(angle) * (pi / 180))
        deltaY <- as.numeric(distance) * sin(as.numeric(angle) * (pi / 180))
        pointB[1] <- deltaX
        pointB[2] <- deltaY
        
        #for testing
        cat("deltaX = ", deltaX, "\n")
        cat("deltaY = ", deltaY, "\n")
        cat("NextPoint: ", pointB[1], ",", pointB[2], "\n")
        
        color = "black"
        
        #by instar
        
        
        #plot point A
        points(pointA[1], pointA[2], type = "p", col = color, cex=0.75)
        cat("Point A printed @ (", pointA[1], ", ", pointA[2], ")\n")
        
        #plot point B
        points(pointB[1], pointB[2], type = "p", col = color, cex=0.75)
        cat("Point B printed @ (", pointB[1], ", ", pointB[2], ")\n")
        
        #draw line from A to B
        segments(pointA[1], pointA[2], pointB[1], pointB[2], col = color)
        cat("Line drawn from (", pointA[1], ", ", pointA[2], ") to (", pointB[1], ",", pointB[2], ")\n")
        
        #set A to B
        pointA <- pointB
      }
      #for testing only
      else {
        cat("Distance or angle is a character!\n")
      }
      cat("---------------------------------------------------\n")
      
    }
    j <- j + 1
    dateToComp <- dat[j,][2]
  }
  prevID <- catID
}
dev.off()


###################################2017 map#################################################
#get data
dat <- read.csv("data/cleaned_data_2017.csv", na.strings="NA")
dat$dist_center <- as.numeric(as.character(dat$dist_center))
dat <- dat[,c("qbtag", "date", "release_time", "dist_center_m", "angle", "ground", "treat_id")]
#dat<-dat[dat$qbtag==28.1,] Use this for mapping a specific larva

#create legend if applicable
#standard - 65 larval paths
#tiff(filename="images/map_paths_2017.tiff", width=1000, height = 1000, units="px", res=200)
plot.new()
frame()
#pdf("standard.pdf", width=7, height=7)
par(pch = 20)
par(mar=c(4.5, 4.5, 1, 1))
plot(NULL, xlab = "Distance (m)", ylab = "Distance(m)", xlim = c(-45, 45), ylim = c(-45, 45),  yaxt = "n", xaxt="n")
axis(2, at = seq(from = -40, to=40, by =10))
axis(1, at = seq(from = -40, to = 40, by = 10))
title("Paths of Larvae on Ground 2017", line = -1 )
draw.circle(0, 0, 30, nv = 1000, border = NULL, col = NA, lty = 1, lwd = 1)


#for each caterpillar, plot points
prevID <- -1
for(i in 1:nrow(dat)) {
  
  catID <- dat[i,][1]
  
  #check that row is not empty
  if(is.na(catID)) {
    cat("End reached.\n")
    break
  }
  
  #don't repeat id's
  if(as.numeric(catID) <= as.numeric(prevID)) {
    next
  }
  
  date <- dat[i,][2]
  
  
  cat("||||||||||||||||||||||||||| ID: ", as.numeric(catID), "|||||||||||||||||||||||||||||||\n")
  
  #set initial point
  distance <- dat[i,][4]
  angle <- dat[i,][5]
  pointA <- c(0,0)
  deltaX1 <- as.numeric(distance) * sin(as.numeric(angle) * (pi / 180))
  deltaY1 <- as.numeric(distance) * cos(as.numeric(angle) * (pi / 180))
  pointA[1] <- deltaX1
  pointA[2] <- deltaY1
  pointB <- c(0, 0)
  
  j <- i
  dateToComp <- dat[j,][2]
  while(isTRUE(all.equal(date[1,], dateToComp[1,]))) {
    #get ID for current row (j)
    toComp <- dat[j,][1]
    
    cat("i = ", i, " catID = ", as.numeric(catID), " j = ", j, " toComp = ", as.numeric(toComp), "\n")
    
    if(isTRUE(all.equal(catID[1,], toComp[1,]))) {
      
      #get distance and angle
      distance <- dat[j,][4]
      angle <- dat[j,][5]
      
      if(!((as.character(distance) == '.' || as.character(angle) == '.'))) {
        #get x and y for next point
        deltaX <- as.numeric(distance) * sin(as.numeric(angle) * (pi / 180))
        deltaY <- as.numeric(distance) * cos(as.numeric(angle) * (pi / 180))
        pointB[1] <- deltaX
        pointB[2] <- deltaY
        
        #for testing
        cat("deltaX = ", deltaX, "\n")
        cat("deltaY = ", deltaY, "\n")
        cat("NextPoint: ", pointB[1], ",", pointB[2], "\n")
        
        color = "black"
        
        #by instar
        
        
        #plot point A
        points(pointA[1], pointA[2], type = "p", col = color, cex=0.75)
        cat("Point A printed @ (", pointA[1], ", ", pointA[2], ")\n")
        
        #plot point B
        points(pointB[1], pointB[2], type = "p", col = color, cex=0.75)
        cat("Point B printed @ (", pointB[1], ", ", pointB[2], ")\n")
        
        #draw line from A to B
        segments(pointA[1], pointA[2], pointB[1], pointB[2], col = color)
        cat("Line drawn from (", pointA[1], ", ", pointA[2], ") to (", pointB[1], ",", pointB[2], ")\n")
        
        #set A to B
        pointA <- pointB
      }
      #for testing only
      else {
        cat("Distance or angle is a character!\n")
      }
      cat("---------------------------------------------------\n")
      
    }
    j <- j + 1
    dateToComp <- dat[j,][2]
  }
  prevID <- catID
}
dev.off()

###################################Individual maps#################################################
#get data
dat <- read.csv("data/cleaned_data_2016.csv", na.strings="NA")
dat$dist_center <- as.numeric(as.character(dat$dist_center))
dat <- dat[,c("qbtag", "date", "release_time", "dist_center", "angle", "ground", "treat_id")]
#dat<-dat[dat$qbtag==201,] #Use this for mapping a specific larva

#create legend if applicable
#standard - 65 larval paths
#tiff(filename="images/specific_larvae_map.tiff", width=1000, height = 1000, units="px", res=200)
plot.new()
frame()
#pdf("standard.pdf", width=7, height=7)
par(pch = 20)
par(mar=c(4.5, 4.5, 1, 1))

#' Uncomment the next 3 lines to generate map paths for ALL larvae on ground. Delete 2nd line
#' to map paths for ALL larvae regardless of vegetation treatment
dat <- dat[dat$treat_id == "Absence",]
for(t in unique(dat$qbtag)){
  dat_temp <- dat[dat$qbtag==t,]
  plot(NULL, xlab = "Distance (ft)", ylab = "Distance(ft)", xlim = c(-175, 175), ylim = c(-175, 175), asp = 1, yaxt = "n")
  axis(2, at = seq(from = -150, to=150, by =50))
  title(paste("Movement Paths of", toString(t)), line = -1 )
  draw.circle(0, 0, 100, nv = 1000, border = NULL, col = NA, lty = 1, lwd = 1)


#for each caterpillar, plot points
prevID <- -1
for(i in 1:nrow(dat_temp)) {
  
  catID <- dat_temp[i,][1]
  
  #check that row is not empty
  if(is.na(catID)) {
    cat("End reached.\n")
    break
  }
  
  #don't repeat id's
  if(as.numeric(catID) <= as.numeric(prevID)) {
    next
  }
  
  date <- dat_temp[i,][2]
  
  
  cat("||||||||||||||||||||||||||| ID: ", as.numeric(catID), "|||||||||||||||||||||||||||||||\n")
  
  #set initial point
  distance <- dat_temp[i,][4]
  angle <- dat_temp[i,][5]
  pointA <- c(0,0)
  deltaX1 <- as.numeric(distance) * sin(as.numeric(angle) * (pi / 180))
  deltaY1 <- as.numeric(distance) * cos(as.numeric(angle) * (pi / 180))
  pointA[1] <- deltaX1
  pointA[2] <- deltaY1
  pointB <- c(0, 0)
  
  j <- i
  dateToComp <- dat_temp[j,][2]
  while(isTRUE(all.equal(date[1,], dateToComp[1,]))) {
    #get ID for current row (j)
    toComp <- dat_temp[j,][1]
    
    cat("i = ", i, " catID = ", as.numeric(catID), " j = ", j, " toComp = ", as.numeric(toComp), "\n")
    
    if(isTRUE(all.equal(catID[1,], toComp[1,]))) {
      
      #get distance and angle
      distance <- dat_temp[j,][4]
      angle <- dat_temp[j,][5]
      
      if(!((as.character(distance) == '.' || as.character(angle) == '.'))) {
        #get x and y for next point
        deltaX <- as.numeric(distance) * sin(as.numeric(angle) * (pi / 180))
        deltaY <- as.numeric(distance) * cos(as.numeric(angle) * (pi / 180))
        pointB[1] <- deltaX
        pointB[2] <- deltaY
        
        #for testing
        cat("deltaX = ", deltaX, "\n")
        cat("deltaY = ", deltaY, "\n")
        cat("NextPoint: ", pointB[1], ",", pointB[2], "\n")
        
        color = "black"
        
        #by instar
        
        
        #plot point A
        points(pointA[1], pointA[2], type = "p", col = color, cex=0.75)
        cat("Point A printed @ (", pointA[1], ", ", pointA[2], ")\n")
        
        #plot point B
        points(pointB[1], pointB[2], type = "p", col = color, cex=0.75)
        cat("Point B printed @ (", pointB[1], ", ", pointB[2], ")\n")
        
        #draw line from A to B
        segments(pointA[1], pointA[2], pointB[1], pointB[2], col = color)
        cat("Line drawn from (", pointA[1], ", ", pointA[2], ") to (", pointB[1], ",", pointB[2], ")\n")
        
        #set A to B
        pointA <- pointB
      }
      #for testing only
      else {
        cat("Distance or angle is a character!\n")
      }
      cat("---------------------------------------------------\n")
      
    }
    j <- j + 1
    dateToComp <- dat_temp[j,][2]
  }
  prevID <- catID
  }
}
dev.off()


####################Paths By Release Area 2017#######################################
#get data
dat <- read.csv("data/cleaned_data_2016.csv", na.strings="NA")
dat$dist_center <- as.numeric(as.character(dat$dist_center))
dat <- dat[,c("qbtag", "date", "release_time", "dist_center_m", "angle", "ground", "treat_id", "starting_quad")]
dat <- dat[!is.na(dat$starting_quad),] #a few larvae have NAs for starting location because they were immediately lost, so remove them
dat$starting_quad <- factor(dat$starting_quad, levels(dat$starting_quad)[c(2,1,4,3)])

#create legend if applicable
#standard - 65 larval paths
tiff(filename="images/releasepoint_paths_2017.tiff", width=1000, height = 1000, units="px", res=200)
#plot.new()
frame()
#pdf("standard.pdf", width=7, height=7)
par(pch = 20)
par(mar=c(4.5, 4.5, 1, 1))
par(mfrow=c(2,2))
for(q in sort(unique(dat$starting_quad))){
  dat_temp <- dat[dat$starting_quad==q,]
  plot(NULL, xlab = "Distance (m)", ylab = "Distance(m)", xlim = c(-45, 45), ylim = c(-45, 45), 
       xaxt="n", yaxt = "n", cex.lab = 0.8 )
  axis(2, at = seq(from = -40, to=40, by =20), cex.axis=0.7)
  axis(1, at = seq(from = -40, to=40, by =20), cex.axis=0.7)
  title(paste("Paths of Larvae from", toString(q), "2017"), line = -1, cex.main=0.8 )
  draw.circle(0, 0, 30, nv = 1000, border = NULL, col = NA, lty = 1, lwd = 1)


  #for each caterpillar, plot points
  prevID <- -1
  for(i in 1:nrow(dat_temp)) {
  
    catID <- dat_temp[i,][1]
  
    #check that row is not empty
    if(is.na(catID)) {
      cat("End reached.\n")
      break
    }
  
    #don't repeat id's
    if(as.numeric(catID) <= as.numeric(prevID)) {
      next
    }
  
    date <- dat_temp[i,][2]
  
  
    cat("||||||||||||||||||||||||||| ID: ", as.numeric(catID), "|||||||||||||||||||||||||||||||\n")
  
   #set initial point
    distance <- dat_temp[i,][4]
    angle <- dat_temp[i,][5]
    pointA <- c(0,0)
    deltaX1 <- as.numeric(distance) * sin(as.numeric(angle) * (pi / 180))
    deltaY1 <- as.numeric(distance) * cos(as.numeric(angle) * (pi / 180))
    pointA[1] <- deltaX1
    pointA[2] <- deltaY1
    pointB <- c(0, 0)
  
    j <- i
    dateToComp <- dat_temp[j,][2]
    while(isTRUE(all.equal(date[1,], dateToComp[1,]))) {
      #get ID for current row (j)
      toComp <- dat_temp[j,][1]
    
      cat("i = ", i, " catID = ", as.numeric(catID), " j = ", j, " toComp = ", as.numeric(toComp), "\n")
    
      if(isTRUE(all.equal(catID[1,], toComp[1,]))) {
      
        #get distance and angle
        distance <- dat_temp[j,][4]
        angle <- dat_temp[j,][5]
      
        if(!((as.character(distance) == '.' || as.character(angle) == '.'))) {
          #get x and y for next point
          deltaX <- as.numeric(distance) * sin(as.numeric(angle) * (pi / 180))
          deltaY <- as.numeric(distance) * cos(as.numeric(angle) * (pi / 180))
          pointB[1] <- deltaX
          pointB[2] <- deltaY
        
          #for testing
          cat("deltaX = ", deltaX, "\n")
          cat("deltaY = ", deltaY, "\n")
          cat("NextPoint: ", pointB[1], ",", pointB[2], "\n")
        
          color = "black"
        
          # by instar
        
        
          #plot point A
          points(pointA[1], pointA[2], type = "p", col = color, cex=0.75)
          cat("Point A printed @ (", pointA[1], ", ", pointA[2], ")\n")
        
          #plot point B
          points(pointB[1], pointB[2], type = "p", col = color, cex=0.75)
          cat("Point B printed @ (", pointB[1], ", ", pointB[2], ")\n")
        
          #draw line from A to B
          segments(pointA[1], pointA[2], pointB[1], pointB[2], col = color)
          cat("Line drawn from (", pointA[1], ", ", pointA[2], ") to (", pointB[1], ",", pointB[2], ")\n")
        
          #set A to B
          pointA <- pointB
        }
        #for testing only
        else {
          cat("Distance or angle is a character!\n")
        }
        cat("---------------------------------------------------\n")
      
      }
      j <- j + 1
      dateToComp <- dat_temp[j,][2]
    }
    prevID <- catID
  }
}
dev.off()


####################Paths By Release Area 2016#######################################
#get data
dat <- read.csv("data/cleaned_data_2016.csv", na.strings="NA")
dat$dist_center <- as.numeric(as.character(dat$dist_center))
dat <- dat[,c("qbtag", "date", "release_time", "dist_center_m", "angle", "ground", "treat_id", "starting_quad")]
dat <- dat[!is.na(dat$starting_quad),] #a few larvae have NAs for starting location because they were immediately lost, so remove them
dat$starting_quad <- factor(dat$starting_quad, levels(dat$starting_quad)[c(2,1,4,3)])

#create legend if applicable
#standard - 65 larval paths
tiff(filename="images/releasepoint_paths_2016.tiff", width=1000, height = 1000, units="px", res=200)
#plot.new()
frame()
#pdf("standard.pdf", width=7, height=7)
par(pch = 20)
par(mar=c(4.5, 4.5, 1, 1))
par(mfrow=c(2,2))
for(q in sort(unique(dat$starting_quad))){
  dat_temp <- dat[dat$starting_quad==q,]
  plot(NULL, xlab = "Distance (m)", ylab = "Distance(m)", xlim = c(-45, 45), ylim = c(-45, 45), 
       xaxt="n", yaxt = "n", cex.lab = 0.8 )
  axis(2, at = seq(from = -40, to=40, by =20), cex.axis=0.7)
  axis(1, at = seq(from = -40, to=40, by =20), cex.axis=0.7)
  title(paste("Paths of Larvae from", toString(q), "2016"), line = -1, cex.main=0.8 )
  draw.circle(0, 0, 30, nv = 1000, border = NULL, col = NA, lty = 1, lwd = 1)
  
  
  #for each caterpillar, plot points
  prevID <- -1
  for(i in 1:nrow(dat_temp)) {
    
    catID <- dat_temp[i,][1]
    
    #check that row is not empty
    if(is.na(catID)) {
      cat("End reached.\n")
      break
    }
    
    #don't repeat id's
    if(as.numeric(catID) <= as.numeric(prevID)) {
      next
    }
    
    date <- dat_temp[i,][2]
    
    
    cat("||||||||||||||||||||||||||| ID: ", as.numeric(catID), "|||||||||||||||||||||||||||||||\n")
    
    #set initial point
    distance <- dat_temp[i,][4]
    angle <- dat_temp[i,][5]
    pointA <- c(0,0)
    deltaX1 <- as.numeric(distance) * sin(as.numeric(angle) * (pi / 180))
    deltaY1 <- as.numeric(distance) * cos(as.numeric(angle) * (pi / 180))
    pointA[1] <- deltaX1
    pointA[2] <- deltaY1
    pointB <- c(0, 0)
    
    j <- i
    dateToComp <- dat_temp[j,][2]
    while(isTRUE(all.equal(date[1,], dateToComp[1,]))) {
      #get ID for current row (j)
      toComp <- dat_temp[j,][1]
      
      cat("i = ", i, " catID = ", as.numeric(catID), " j = ", j, " toComp = ", as.numeric(toComp), "\n")
      
      if(isTRUE(all.equal(catID[1,], toComp[1,]))) {
        
        #get distance and angle
        distance <- dat_temp[j,][4]
        angle <- dat_temp[j,][5]
        
        if(!((as.character(distance) == '.' || as.character(angle) == '.'))) {
          #get x and y for next point
          deltaX <- as.numeric(distance) * sin(as.numeric(angle) * (pi / 180))
          deltaY <- as.numeric(distance) * cos(as.numeric(angle) * (pi / 180))
          pointB[1] <- deltaX
          pointB[2] <- deltaY
          
          #for testing
          cat("deltaX = ", deltaX, "\n")
          cat("deltaY = ", deltaY, "\n")
          cat("NextPoint: ", pointB[1], ",", pointB[2], "\n")
          
          color = "black"
          
          # by instar
          
          
          #plot point A
          points(pointA[1], pointA[2], type = "p", col = color, cex=0.75)
          cat("Point A printed @ (", pointA[1], ", ", pointA[2], ")\n")
          
          #plot point B
          points(pointB[1], pointB[2], type = "p", col = color, cex=0.75)
          cat("Point B printed @ (", pointB[1], ", ", pointB[2], ")\n")
          
          #draw line from A to B
          segments(pointA[1], pointA[2], pointB[1], pointB[2], col = color)
          cat("Line drawn from (", pointA[1], ", ", pointA[2], ") to (", pointB[1], ",", pointB[2], ")\n")
          
          #set A to B
          pointA <- pointB
        }
        #for testing only
        else {
          cat("Distance or angle is a character!\n")
        }
        cat("---------------------------------------------------\n")
        
      }
      j <- j + 1
      dateToComp <- dat_temp[j,][2]
    }
    prevID <- catID
  }
}
dev.off()
