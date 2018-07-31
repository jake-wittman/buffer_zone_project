#libraries
library(ggplot2)
library(fitdistrplus)
library(doParallel)
library(tidyr)
library(beepr)
library(plotrix)
library(ggforce)
library(ggsn)
library(GISTools)

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
dat2017$move <- ifelse(dat2017$m_segment_dist > 0, 1, 0)
dat2017$time <- factor(dat2017$time, ordered=T, levels= c("7:30", "8:00", "8:30", "9:00",
                                                          "9:30", "10:00", "10:30", "11:00",
                                                          "11:30", "12:00", "12:30", "13:00", 
                                                          "13:30", "14:00", "14:30", "15:00",
                                                          "15:30", "16:00", "16:30", "17:00"))

#' The goal of this analysis is to simulate the movement of gypsy moth larvae to determine
#' how frequently they have a total displacement of 100 feet or greater. The idea is to show how
#' frequently larvae might reach a log deck if a buffer zone of a certain radius was used. 
#' To simulate movement, I will release a larva on the edge of my buffer zone. I will simulate
#' a movement path for the larvae (described below). At each step in the path, I will evaluate
#' if the larva entered the log deck circle (or if it passed through the log deck) and record 
#' the results. Then I'll be able to calculate the proportion of larvae reaching the log deck 
#' or passing through it out of the total paths simulated.


#' I want to only use the data from the larvae on the ground. Maybe I should consider adding a 
#' level where I start the larvae in trees, but I don't want to start with that. I think
#' I'm also going to exclude the NW point data, as that was a qualitatively different environment.
#' And probably the data points from the NE where the larvae crawled to the plants.Another question 
#' I'm unsure of right now, is  whether or not to have a seperate distribution of move lengths
#' for each time.
#ggplot(dat_sub, aes(x=m_segment_dist)) + geom_histogram() + facet_grid(time ~.)
#' ### The model
#' My results suggest a correlated random walk
#' can serve as a decent approximation of their movement. I'll use a binomial distribution
#' to determine if the larvae move or not, based on the proportion that moved. Then, if they moved
#' I'll draw from a gamma distribution fit to the non-0 movement distances to get the distance
#' they moved. I'll also draw from the turn angle distribution (probably a uniform distribution, 
#' need to check that though). I'll do this for 19 time points, equivalent to 10 hours of movement.


#' Data cleaning
dat_sub <- dat2017[dat2017$treat_id == "Absence",] #only use larvae on ground
dat_sub <- dat_sub[dat_sub$starting_quad != "NW",] #remove NW starting quadrant larvae
dat_sub <- dat_sub[dat_sub$open_env == 0,] #remove larvae that moved into a seperate habitat type
dat_sub <- dat_sub[!is.na(dat_sub$m_segment_dist),]
dat_gamma <- dat_sub[dat_sub$m_segment_dist != 0,] #make a seperate data set for moves larger than 0

##################################################################################################
#
#                     Fit Distributions to Data
#
##################################################################################################

#' Fit distributions to data
#' Binomial - Need to calculate the proportion of moves that were 0 distance and >0 distance
#The line below this calculates the proportion of moves that were 0, which is used to give us the 
#probability of moving or not moving.
prop_nomove <- length(dat_sub[dat_sub$m_segment_dist==0,"m_segment_dist"])/length(dat_sub[!is.na(dat_sub$m_segment_dist), "m_segment_dist"])
prop_move <- 1-prop_nomove #Need to use the probability of moving in the binomial distribution

#' Gamma distribution for distance moved if >0
fitgamma <- fitdist(dat_gamma$m_segment_dist, "gamma")
plot(fitgamma) #This looks like a pretty good fit for the data
shape <- fitgamma$estimate[1]
rate <- fitgamma$estimate[2]

#' Distribution for turn angle.
hist(dat_gamma$ta) 
#' Need to remove NAs to fit dist for turn angle
dat_norm <- dat_gamma[!is.na(dat_gamma$ta),]
#' The turn angles for moves are actually normally distributed, implying some directional 
#' persistance. I think I'll use a normal distribution centered on 0 for this.
fitnorm <- fitdist(dat_norm$ta, "norm")
ta_mean <- fitnorm$estimate[1]
ta_sd <- fitnorm$estimate[2]
x <- seq(-200, 200, 1)
hist(dat_gamma$ta, prob=T)
curve(dnorm(x, mean=ta_mean, sd=ta_sd), add=T)

##################################################################################################
#
#                     Simulate Movement
#
##################################################################################################
#' Simulated locations will be recorded every half hour, just like in my experiment. The larvae will
#' be allowed to crawl for 10 hours, so that's 20 observations per caterpillar. At the end
#' of their movement, displacement will be calculated and recorded.

#' ### Load in required functions 
r <- function(degrees){ #This function is for properly calculating the (x,y) after a move distance and angle
  degrees * (pi/180) #trig functions in r use radians, not degrees, so we need to convert them
}
d <- function(radians){
  radians * (180/pi)
}

#' the atan2 function uses the x-axis as the reference for calculating the angle of a vector from
#' the origin. So I wrote the atan3 function to use the y axis and return the answer in degrees
atan3 <- function(x, y){
  if(x >= 0){
    atan2(x, y) * (180/pi)
  }
  else{
    360 + (atan2(x, y) * (180/pi))
  }
}

#' This function is used to calculate the range of values for bearing that the caterpillar
#' could take that would put it's movement within the circle.
bearing_range <- function(x, y){
  theta <- NA
  alpha <- (atan3(x, y) + 180) %% 360
  theta[1] <- (alpha + d(asin(log_r[l]/(log_r[l]+sqrt((x)^2+(y)^2))))) %% 360 #calculate the high end of the range
  theta[2] <- (alpha - d(asin(log_r[l]/(log_r[l]+sqrt((x)^2+(y)^2))))) %% 360 #calculate the low end of the range
  return(theta)
}

#' This function generates a random starting location along the circumference of a circle with a 
#' given radius
starting_loc <- function(buffer_r){
  yt <- seq(-buffer_r, buffer_r, 0.001)
  xs <- sqrt(buffer_r^2 - (yt)^2)
  neg_xs <- -1*sqrt(buffer_r^2 - (yt)^2)
  xs <- c(neg_xs, xs)
  ys <- rep(seq(-buffer_r, buffer_r, 0.001), 2)
  start_coord <- cbind(xs, ys)
  rand_coord <- sample(nrow(start_coord), 1, replace=T)
  start_x <- start_coord[rand_coord, 1]
  start_y <- start_coord[rand_coord, 2]
  return(c(start_x, start_y))
}

#' Need a function to calculate the opposite of alpha (+/- 180 degrees, depending on quad)
opp_alpha <- function(x, y){
  alpha <- atan3(x,y)
  if(alpha <= 180){
    return(alpha + 180)
  }else{
    return(alpha-180)
  }
}

nsims <- 10000 #number of simulations
nsteps <- 20 #number of "steps" in path
sim_dat <- matrix(NA, nrow=nsims, ncol=1) #store whether or not the insect reached the log deck
colnames(sim_dat) <- "sim_dat"
temp_move <- matrix(NA, nrow=(nsteps+1), ncol=3) #temporary matrix to store move coords and if the previous move took the larva through/into the buffer
log_r <- c(7.62, 15.24, 22.86)  #set radius of log deck, corresponds to 25 ft, 50 ft, and 75 ft
buffer_r <- c(22.86, 30.48, 38.1, 45.72, 53.34, 60.96) #set radius of buffer zone, corresponds to radii from 75 - 200 ft by 25 ft increments
results <- matrix(NA, nrow=length(log_r), ncol=length(buffer_r)) #matrix to store final results
rownames(results) <- log_r
colnames(results) <- buffer_r
bearing_matrix <- matrix(NA, nrow=(nsteps+1), ncol=1)

set.seed(1)
#registerDoParallel(cores=(detectCores() - 1 )) #set number of cores to use in parallel
#sim_dat <- foreach(i = 1:nsims, .combine=rbind) %dopar%{
for(b in 1:length(buffer_r)){
  print(sprintf("Buffer Zone: %d", b))
  flush.console()
  
  for(l in 1:length(log_r)){
    print(sprintf("Log Deck: %d", l))
    flush.console()
    
    for(i in 1:nsims){
      if(i%%500 == 0) {print(sprintf("Path #: %d", i))
        flush.console()
      }
      temp_move[1,1:2] <- starting_loc((buffer_r[b] + log_r[l])) #set starting location to (0,0)
      distmoved <- 0 #set to 0 at each new simulation
      
      for(j in 2:(nsteps+1)){
        move <- rbinom(1, prob=prop_move, size=1)
        
        
        #IF INSECTS MOVES
        if(move == 1) {
          dist <- rgamma(1, shape=shape, rate=rate) #randomly sample distance moved
          
          
          if(distmoved == 0){#this if section for the first time the insect is moving
            ta <- runif(1, 0, 360) #randomly sample starting direction
            bearing_matrix[j, 1] <- ta
            
          } else{
            ta <- rnorm(1, mean=ta_mean, sd=ta_sd) #randomly sample turn angle for CRW
            ta <- bearing_matrix[j-1, 1] + ta#convert turn angle to a bearing for proper path calculation
            bearing_matrix[j, 1] <- ta #store bearing
          }
          
          #ta <- runif(1, -180, 180) #randomly sample turn angle for a pure random walk
          temp_move[j, 1] <- dist * sin(r(ta)) + temp_move[j-1, 1] #store x coordinate of next move
          temp_move[j, 2] <- dist * cos(r(ta)) + temp_move[j-1, 2] #store y coordinate of next move
          distmoved <- dist+distmoved
          
          if(sqrt((temp_move[j, 1])^2 + (temp_move[j,2])^2) <= log_r[l]){
            temp_move[j, 3] <- 1 #Larva reaches log deck on this move
            #print("Reaches log deck, point inside log deck")
            break
            
          } else{
            ang <- r(ta) #calculate bearing in radians
            
            if(is.na(ang)){ #to deal with NA values
              bearing <- NA
              
            } else if(ang >= 0){ 
              bearing <- ang * (180/pi) #converts radians to degrees
              
            } else{
              bearing <- (ang + 2*pi) * (180/pi) #converts negative radians to positive and then to degrees
            }
            
            theta <- bearing_range(temp_move[j-1, 1], temp_move[j-1, 2]) #calculate range of bearings that would move insect into log deck
            
            
            #IF theta bounds are reversed from usual (will only happen in a few occasions)
            if(theta[2] > theta[1]){
              
              if(bearing < theta[1] || bearing > theta[2]){
                dist_to_center <- sqrt((temp_move[j-1, 1])^2 + (temp_move[j-1, 2])^2) + log_r[l] #distance from previous location to edge of log deck
                crit_dist <- dist_to_center * cos(r(abs(opp_alpha(temp_move[j-1, 1], temp_move[j-1,2])-bearing))) #distance larva would have to move to enter log deck
                
                if(dist >= crit_dist){
                  temp_move[j, 3] <- 1 #Larva reaches log deck on this move
                  #print("Reaches log deck, high theta")
                  break
                  
                }else{
                  temp_move[j, 3] <- 0 #Larva does not reach log deck on this move
                  #print("Does not reach log deck, high theta")
                  next
                } 
                
              }else{
                temp_move[j, 3] <- 0 #Larva does not reach log deck on this move
                #print("Does not reach log deck, high theta, not btwn bounds")
                next
              } 
            }
            
            #IF THE BEARING IS WITHIN THETA
            if(bearing < theta[1] && bearing > theta[2]){ #check if the bearing of the larva is within the danger zone
              dist_to_center <- sqrt((temp_move[j-1, 1])^2 + (temp_move[j-1, 2])^2)  #distance from previous location to center of log deck
              crit_dist <- dist_to_center * cos(r(abs(opp_alpha(temp_move[j-1, 1], temp_move[j-1,2])-bearing))) #distance larva would have to move to enter log deck
              
              if(dist >= crit_dist){
                temp_move[j, 3] <- 1 #Larva reaches log deck on this move
                #print("Reaches log deck, normal theta, btwn bounds")
                break
                
              }else{
                temp_move[j, 3] <- 0 #Larva does not reach log deck on this move
                #print("Not reaches log deck, normal theta, btwn bounds")
                next
              }
              
              #IF THE BEARING IS NOT WITHIN THETA
            }else{
              temp_move[j, 3] <- 0 #Larva does not reach log deck on this move
              #print("Bearing not within bounds, no log deck")
              next
            }
          }
        }
        
        #IF INSECT DOES NOT MOVE
        else{
          bearing_matrix[j, 1] <- bearing_matrix[j-1, 1]
          temp_move[j, 1] <- temp_move[j-1, 1]
          temp_move[j, 2] <- temp_move[j-1, 2]
          temp_move[j, 3] <- 0 #Larva does not reach log deck on this move
          #print("Insect does not move")
          next
        }
      }
      if(any(temp_move[,3] == 1, na.rm=T)){  
        sim_dat[i] <- 1 #If the larva reached the log deck anytime in the simulation, record that as a 1, otherwise 0
      }else{
        sim_dat[i] <- 0
      }
      
    }
    results[l, b] <- length(sim_dat[sim_dat==1])/length(sim_dat)#calculate the proportion of larvae reaching log deck out of # simulated and store
    
  }
  
}
beep(8)

results <- as.data.frame(results)
results
#write.csv(results, file="data/simulation_results.csv")

#' Use this chunk of code to plot the movement paths from the last simulated larva.
plot(NULL, xlab = "Distance (m)", ylab = "Distance(m)", 
     xlim = c((-1*(buffer_r[b] + log_r[l] + 10)), (buffer_r[b] + log_r[l] + 10)), 
     ylim = c((-1*(buffer_r[b] + log_r[l] + 10)), (buffer_r[b] + log_r[l] + 10)),
     yaxt = "n", xaxt="n")
axis(2, at = seq(from = (-1*(buffer_r[b] + log_r[l] + 10)), to=(buffer_r[b] + log_r[l] + 10), by =10))
axis(1, at = seq(from = (-1*(buffer_r[b] + log_r[l] + 10)), to=(buffer_r[b] + log_r[l] + 10), by =10))
draw.circle(0, 0, (buffer_r[b]+log_r[l]), nv = 1000, border = NULL, col = NA, lty = 1, lwd = 1)
draw.circle(0, 0, log_r[l])
points(temp_move[,1], temp_move[,2])
for(i in 2:nrow(temp_move)){
  segments(temp_move[i-1, 1], temp_move[i-1, 2], temp_move[i, 1], temp_move[i, 2])
}

#' Change reults from wide to long format
results <- read.csv("data/simulation_results.csv", row.names=1)
results <- as.matrix(results) #if you import results from csv, need this to plot with below code
prop <- c(results[1,], results[2,], results[3,])
prop <- unname(prop)
var_log_r <- rep(log_r, each=6)
var_buffer_r <- rep(buffer_r, 3)
results_long <- as.data.frame(cbind(prop, var_log_r, var_buffer_r))
results_long$var_buffer_r <- as.factor(results_long$var_buffer_r)
results_long$var_log_r <- as.factor(results_long$var_log_r)
log_names <- list("7.62" = "25", "15.24" = "50", "22.86" = "75") #Used for proper graph labelling

radius_labeller <- function(variable, value){ #custom function to change facet grid labels
  return(log_names[value])
}

ggplot(results_long, aes(y=prop, x=var_buffer_r)) +
  geom_col(fill="white", colour="black") +
  facet_grid(.~var_log_r, labeller=radius_labeller) +
  theme_bw() +
  theme(panel.background=element_rect(fill=NA),
        panel.border = element_rect(colour = "black", fill=NA, size=0.4),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position="none",
        axis.text = element_text(size=10, colour="black"), 
        axis.title=element_text(size=12), 
        axis.text.x = element_text(angle=45, hjust=1),
        strip.background = element_rect(colour="black", fill="white", size = 0.6),
        strip.text = element_text(size = 10),
        plot.title = element_text(size=12, hjust=0.5, colour="black")) +
  labs(x = "Buffer zone distance (ft)", y = "Larvae reaching log deck") +
  scale_y_continuous(labels = scales::percent,
                     limits = c(0, 0.13),
                     breaks = c(0, 0.025, 0.05, 0.075, 0.1, 0.125),
                     expand = c(0, 0.0005)) +
  ggtitle("Log deck radius (ft)") +
  scale_x_discrete(labels = c("75", "100", "125", "150", "175", "200"))
ggsave(file="figures/simulation.tiff", units="in", dpi=1200, height=4, width=6.7)

#plot a single graph
sub_dat <- subset(results_long, var_log_r == 7.62)
ggplot(sub_dat, aes(y=prop, x=var_buffer_r)) +
  geom_col(fill="white", colour="black") +
  theme_bw() +
  theme(panel.background=element_rect(fill=NA),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position="none",
        axis.text = element_text(size=16, colour="black"), 
        axis.title=element_text(size=20), 
        axis.text.x = element_text(angle=45, hjust=1),
        strip.background = element_rect(colour="black", fill="white"),
        strip.text = element_text(size = 14),
        plot.title = element_text(size=18, hjust=0.5, colour="black")) +
  labs(x = "Buffer Zone Distance (ft)", y = "Larvae Reaching Log Deck") +
  scale_y_continuous(labels= scales::percent) +
  ggtitle("Log Deck Radius (ft)") +
  scale_x_discrete(labels = c("75", "100", "125", "150", "175", "200"))

#' The results of the simulation for buffer/log deck size similar to ours are about what
#' we saw in the field (~2% of larvae reached log deck). Clearly as buffer zone area increases,
#' fewer larvae reach the log deck. As log deck size increase more larvae reach log deck.



#' Make a plot of the buffer zone and log deck as an example for publication
tiff(filename = "figures/example_sim.tiff", width = 5, height = 5, units = "in", type = "cairo", res = 1200)
 ggplot() + 
  geom_circle(aes(x0=0, y0=0, r=25)) +
  geom_circle(aes(x0=0, y0=0, r=100)) +
  geom_segment(aes(x=0, y=0, xend=25, yend=0)) +
  geom_segment(aes(x=-25, y=0, xend=-100, yend=0)) +
  labs(x="Distance (ft)", y="Distance (ft)") +
  annotate("text", x=-60, y=9, label="b", size=6) +
  annotate("text", x=12, y=9, label="r", size=6) +
  theme_classic() +
  theme(legend.position="none",
        axis.text = element_text(size=12, colour="black"), 
        axis.title=element_text(size=14), 
        strip.background = element_rect(colour="black", fill="white"))

#pdf(file = "example_sim.pdf", width= 5, height = 5, #' see how it looks at this size
  #  useDingbats=F)
#' Schematic diagram of field site
ggplot() +
  geom_polygon(aes(x = c(-160, -170, -160, -150), y = c(95, 75, 80, 75)), fill = "black") +
  geom_circle(aes(x0 = 0, y0 = 0, r = 100)) +
  geom_polygon(aes(x = c(-15, -4, 15, 4), y = c(4, 13, -4, -13)), fill = "black") +
  scale_fill_manual(values = "black") + 
  geom_point(aes(x = c((100 * cos(pi/4)), (100 * cos((3*pi)/4)),
                       (100 * cos((5 * pi)/4)), (100 * cos((7 * pi)/4)), -230),
                 y = c((100 * sin(pi/4)), (100 * sin((3*pi)/4)),
                       (100 * sin((5 * pi)/4)), (100 * sin((7 * pi)/4)), 0),
                 size = 3)) +
  geom_line(aes(x = c(0, 100), y = c(0, 0), size = 1)) +
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.position = "none") +
  annotate("text", x = -160, y = 0, label = "Release Points", size = 3) +
  geom_polygon(aes(x = c(-245, -105, -105, -245),
                   y = c(15, 15, -15, -15)), alpha = 0.001, colour = "black") +
  annotate("text", x = 0, y = 25, label = "Log Deck", size = 3) +
  annotate("text", x = -160, y = 60, label = "N", size = 5) +
  annotate("text", x = 45, y = 10, label = "100 ft", size = 3)
#north2(schematic, x = 0.1, y = 0.9, symbol = 9, scale = 0.15)
ggsave("figures/fieldschematic.tiff", units = "in", dpi = 1200, height = 2.0, width = 3.2)
 
dev.off()
# Old code, don't use.
# library(cairoDevice)
# tiff(filename = "figures/sim_ex.tiff", width = 8, height = 7.75, units = "in", type="cairo", res=600)
# par(mar=c(5, 5, 2, 1))
# plot(NULL, xlab = "Distance (ft)", ylab = "Distance(ft)", 
#      xlim = c((-1*(buffer_r[b] + log_r[l] + 10)), (buffer_r[b] + log_r[l] + 10)), 
#      ylim = c((-1*(buffer_r[b] + log_r[l] + 10)), (buffer_r[b] + log_r[l] + 10)),
#      yaxt = "n", xaxt="n", cex.lab=2.25)
# axis(2, at = seq(from = (-1*(buffer_r[b] + log_r[l])), to=(buffer_r[b] + log_r[l]), by =7.62),
#                  labels=seq(from=-125, to=125, by=25), cex.axis = 2.1)
# axis(1, at = seq(from = (-1*(buffer_r[b] + log_r[l] )), to=(buffer_r[b] + log_r[l] ), by =25),
#                  labels=seq(from=-125, to=125, by=25), cex.axis= 2.1)
# draw.circle(0, 0, (buffer_r[b]+log_r[l]), nv = 1000, border = NULL, col = NA, lty = 1, lwd = 1)
# draw.circle(0, 0, log_r[l])
# points(temp_move[,1], temp_move[,2])
# for(i in 2:nrow(temp_move)){
#   segments(temp_move[i-1, 1], temp_move[i-1, 2], temp_move[i, 1], temp_move[i, 2])
# }
# dev.off()

x <- c(2.5, 12.5, 22.5, 25, 25.75, 28, 20, -15, 7.5, 20, 2.5)
y <- c(-100, -67.5, -62.5, -50, -45, -30, -20, -7.5, -2.5, 25, 24.5)
temp_move <- as.data.frame(cbind(x, y))
circleFun <- function(center = c(0,0), diameter = 1, npoints = 100){
  r = diameter / 2
  tt <- seq(0,2*pi,length.out = npoints)
  xx <- center[1] + r * cos(tt)
  yy <- center[2] + r * sin(tt)
  return(data.frame(x = xx, y = yy))
}
circle1 <- circleFun(diameter = 50, npoints = 10000)
circle2 <- circleFun(diameter = 200, npoints = 10000)
ggplot(temp_move, aes(x = x, y = y)) +
  geom_point() +
  geom_path() +
  coord_cartesian(xlim = c(-100, 100), ylim = c(-100, 100)) +
  scale_x_continuous(name = "Distance (ft)",
                     breaks = seq(-100, 100, 25),
                     labels = seq(-100, 100, 25)) +
  scale_y_continuous(name = "Distance (ft)",
                     breaks = seq(-100, 100, 25),
                     labels = seq(-100, 100, 25)) +
  annotate("path", 
           x = circle1$x,
           y = circle1$y) +
  annotate("path", 
           x = circle2$x,
           y = circle2$y) +
  theme(axis.title = element_text(size = 18),
        axis.text = element_text(size = 18))
ggsave("figures/example_path2.tiff", dpi = 1200, height = 6, width = 6, units = "in")

#Spun with ezspin("scripts/simulation.R", out_dir="spin_output", fig_dir="figures", keep_md=F)