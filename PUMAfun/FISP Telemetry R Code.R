#Packages to install
install.packages(c("gpclib","ade4","adehabitat","adehabitatHR","shapefiles", "sp", "zoo", "maps", "ggplot2", "KernSmooth", "chron", "rgdal", 
                   "PROJ.4", "gstats", "lattice", "RSAGA", "akima", "plotKML","maptools","PBSmapping","xts","move","rgeos","ggplot2","GISTools"))
library(chron)  
library(adehabitat)
library(adehabitatHR)
library(maptools)
library(rgdal)
library(sp)
library(maps)
library (KernSmooth)
library (plotKML)
library (maptools)
library (PBSmapping)
library (xts)
library (move)
library (rgeos)
library (ggplot2)
library (GISTools)

rm(list = ls())

#set working directory
setwd("C:/Users/mpward/Desktop/FISP/2014 FISP DONE/164.077_2340-49882")


# combined all the data for a given individual
a<-readRDS(file="C:/Users/mpward/Desktop/FISP/2014 FISP DONE/164.077_2340-49882/164.0770_06-07-14.rds")
b<-readRDS(file="C:/Users/mpward/Desktop/FISP/2014 FISP DONE/164.077_2340-49882/164.0770_06-09-14.rds")
c<-readRDS(file="C:/Users/mpward/Desktop/FISP/2014 FISP DONE/164.077_2340-49882/164.0770_06-12-14.rds")

#Rounding Time (in seconds)
RTa <- 60
RTb <- 120



#################################################################################
#ADJUSTMENT FOR THE TOWERS NOT BEING AT ZERO DEGREES
adjust1<- -8
adjust2<- 0
adjust3<- 0
adjust4<- 0
#T4 is 8 degrees off???

##############SECOND STRONGEST SIGNAL STRENGTH <<<<<FILTER>>>>>>>
ssss <- -13000

##############NOISE ON SECOND STRONGEST ANTENNA <<<<FILTER>>>>>>>
n_cutoff <- -13000

#PULSE WIDTH low+++++++++++++++++++++++++++++++++<<<<FILTER>>>>>
PWlow<- 12

#PULSE WIDTH high++++++++++++++++++++++++++++++++<<<<FILTER>>>>>
PWhigh<-19


#SS Change for Activity
SS_act_constant <- 300

#Bearing Change for Activity
Bearing_act_constant <- 1.8

#Big change in signal strength
big <- 1000

#########TOWER Locations
UTMx1<-438968
UTMx2<-439358 
UTMx3<-439622
UTMx4<-438632

UTMy1<-4449459
UTMy2<-4449561
UTMy3<-4449326
UTMy4<-4449187

################Filter, concurrent locations have to be within this distance to be used by the algorithm (in meters)
################If you don't want to use this cut=off enter a big #, in this case 5000000
distance_cutoff <- 200

################################Super Close Filter########################
close_cutoff <- -78000


EE <- rbind(a)


t1 <- data.frame(EE$DT,EE$S1.TOWER1,EE$N1.TOWER1,EE$S2.TOWER1,EE$N2.TOWER1,EE$S3.TOWER1,EE$N3.TOWER1,EE$S4.TOWER1,EE$N4.TOWER1,EE$S5.TOWER1,EE$N5.TOWER1,EE$S6.TOWER1,EE$N6.TOWER1,EE$PW.TOWER1,EE$PI.TOWER1)
timer <- as.POSIXlt (t1$EE.DT)
t1$time<- align.time(timer,RTa)
t1 <- na.omit(t1)
t1$EE.DT <- NULL 

t2 <- data.frame(EE$DT,EE$S1.TOWER2,EE$N1.TOWER2,EE$S2.TOWER2,EE$N2.TOWER2,EE$S3.TOWER2,EE$N3.TOWER2,EE$S4.TOWER2,EE$N4.TOWER2,EE$S5.TOWER2,EE$N5.TOWER2,EE$S6.TOWER2,EE$N6.TOWER2,EE$PW.TOWER2,EE$PI.TOWER2)
timer2 <- as.POSIXlt (t2$EE.DT)
t2$time<- align.time(timer2,RTa)
t2 <- na.omit(t2)
t2$EE.DT <- NULL 

t3 <- data.frame(EE$DT,EE$S1.TOWER3,EE$N1.TOWER3,EE$S2.TOWER3,EE$N2.TOWER3,EE$S3.TOWER3,EE$N3.TOWER3,EE$S4.TOWER3,EE$N4.TOWER3,EE$S5.TOWER3,EE$N5.TOWER3,EE$S6.TOWER3,EE$N6.TOWER3,EE$PW.TOWER3,EE$PI.TOWER3)
timer3 <- as.POSIXlt (t3$EE.DT)
t3$time<- align.time(timer3,RTa)
t3 <- na.omit(t3)
t3$EE.DT <- NULL 

t4 <- data.frame(EE$DT,EE$S1.TOWER4,EE$N1.TOWER4,EE$S2.TOWER4,EE$N2.TOWER4,EE$S3.TOWER4,EE$N3.TOWER4,EE$S4.TOWER4,EE$N4.TOWER4,EE$S5.TOWER4,EE$N5.TOWER4,EE$S6.TOWER4,EE$N6.TOWER4,EE$PW.TOWER4,EE$PI.TOWER4)
timer4 <- as.POSIXlt (t4$EE.DT)
t4$time<- align.time(timer4,RTa)
t4 <- na.omit(t4)
t4$EE.DT <- NULL 

EE <- rbind(b,c)

t1a <- data.frame(EE$DT,EE$S1.TOWER1,EE$N1.TOWER1,EE$S2.TOWER1,EE$N2.TOWER1,EE$S3.TOWER1,EE$N3.TOWER1,EE$S4.TOWER1,EE$N4.TOWER1,EE$S5.TOWER1,EE$N5.TOWER1,EE$S6.TOWER1,EE$N6.TOWER1,EE$PW.TOWER1,EE$PI.TOWER1)
timer <- as.POSIXlt (t1a$EE.DT)
t1a$time<- align.time(timer,RTb)
t1a <- na.omit(t1a)
t1a$EE.DT <- NULL 

t2a <- data.frame(EE$DT,EE$S1.TOWER2,EE$N1.TOWER2,EE$S2.TOWER2,EE$N2.TOWER2,EE$S3.TOWER2,EE$N3.TOWER2,EE$S4.TOWER2,EE$N4.TOWER2,EE$S5.TOWER2,EE$N5.TOWER2,EE$S6.TOWER2,EE$N6.TOWER2,EE$PW.TOWER2,EE$PI.TOWER2)
timer2 <- as.POSIXlt (t2a$EE.DT)
t2a$time<- align.time(timer2,RTb)
t2a <- na.omit(t2a)
t2a$EE.DT <- NULL 

t3a <- data.frame(EE$DT,EE$S1.TOWER3,EE$N1.TOWER3,EE$S2.TOWER3,EE$N2.TOWER3,EE$S3.TOWER3,EE$N3.TOWER3,EE$S4.TOWER3,EE$N4.TOWER3,EE$S5.TOWER3,EE$N5.TOWER3,EE$S6.TOWER3,EE$N6.TOWER3,EE$PW.TOWER3,EE$PI.TOWER3)
timer3 <- as.POSIXlt (t3a$EE.DT)
t3a$time<- align.time(timer3,RTb)
t3a <- na.omit(t3a)
t3a$EE.DT <- NULL 

t4a <- data.frame(EE$DT,EE$S1.TOWER4,EE$N1.TOWER4,EE$S2.TOWER4,EE$N2.TOWER4,EE$S3.TOWER4,EE$N3.TOWER4,EE$S4.TOWER4,EE$N4.TOWER4,EE$S5.TOWER4,EE$N5.TOWER4,EE$S6.TOWER4,EE$N6.TOWER4,EE$PW.TOWER4,EE$PI.TOWER4)
timer4 <- as.POSIXlt (t4a$EE.DT)
t4a$time<- align.time(timer4,RTb)
t4a <- na.omit(t4a)
t4a$EE.DT <- NULL 



T1 <- rbind (t1,t1a)
T2 <- rbind (t2,t2a)
T3 <- rbind (t3,t3a)
T4 <- rbind (t4,t4a)

x1<-merge(T1,T2, all=TRUE)
x2<-merge(x1,T3, all=TRUE)
x3<-merge(x2,T4, all=TRUE)

All <- x3
All$id <- 1
write.csv(All,"merge.csv")


#################T1#############################################################################
#TOWER1
datetime<-c(All$time)
T1_value0<-c(All$EE.S1.TOWER1)
T1_value60<-c(All$EE.S2.TOWER1)
T1_value120<-c(All$EE.S3.TOWER1)
T1_value180<-c(All$EE.S4.TOWER1)
T1_value240<-c(All$EE.S5.TOWER1)
T1_value300<-c(All$EE.S6.TOWER1)
T1_noise0<-c(All$EE.N1.TOWER1)
T1_noise60<-c(All$EE.N2.TOWER1)
T1_noise120<-c(All$EE.N3.TOWER1)
T1_noise180<-c(All$EE.N4.TOWER1)
T1_noise240<-c(All$EE.N5.TOWER1)
T1_noise300<-c(All$EE.N6.TOWER1)
T1_PW <- c(All$EE.PW.TOWER1)
T1_PI <- c(All$EE.PI.TOWER1)

# TO DETERMINE STRONGEST AND SECONDSTRONGEST ANTENNA###############################################
data<-data.frame(T1_value0,T1_value60,T1_value120,T1_value180,T1_value240,T1_value300)
maxn <- function(n) function(x) order(x, decreasing=TRUE)[n]
T1_strongest <- apply(data, 1, function(x)x[maxn(1)(x)])
T1_secondstrongest <- apply(data, 1, function(x)x[maxn(2)(x)])
T1_sANT <- apply(data, 1, maxn(1))
T1_ssANT <- apply(data, 1, maxn(2))
T1_strongestANT <-as.character(T1_sANT)
T1_secondstrongestANT <- as.character(T1_sANT)

difference <- (T1_strongest - T1_secondstrongest)/100
bearingdiff <-30-(-0.0624*(difference**2))-(2.8346*difference)

vals <- c("1","2","3","4","5","6")
value.str2 <- (((match(T1_ssANT, vals)-1)*60))
value.str1 <- (((match(T1_sANT, vals)-1)*60))
change.ind <- abs(match(data, vals) - match(data, vals))
value.str2_adj_a <- value.str2+adjust1
value.str2_adj_b <- ifelse(value.str2_adj_a <0,(360+value.str2_adj_a),value.str2_adj_a)
value.str2_adj <- ifelse(value.str2_adj_b >360,(value.str2_adj_b-360),value.str2_adj_b)
value.str1_adj_a <- value.str1+adjust1
value.str1_adj_b <- ifelse(value.str1_adj_a <0,(360+value.str1_adj_a),value.str1_adj_a)
value.str1_adj <- ifelse(value.str1_adj_b >360,(value.str1_adj_b-360),value.str1_adj_b)
Ant_test <- abs(value.str2-value.str1)

#NOISE
noiseantenna <-(((match(T1_sANT, vals)-1)*60))
T1_noise<- ifelse ((noiseantenna==0), T1_noise0, ifelse((noiseantenna==60), T1_noise60, ifelse((noiseantenna==120), T1_noise120, ifelse((noiseantenna==180), T1_noise180, ifelse((noiseantenna==240), T1_noise240, ifelse((noiseantenna==300), T1_noise300, NA))))))

#BEARING ESTIMATION#################################################################################
Tower1_data <- data.frame(T1_secondstrongest,Ant_test,value.str1,value.str1_adj,value.str2,value.str2_adj,bearingdiff,T1_noise,T1_PW,T1_PI)
Tower1_data$bearing1a <-apply(Tower1_data, 1, function(x) ifelse(
  any( x["T1_secondstrongest"]< ssss,
       x["T1_noise"] > n_cutoff,
       abs(x["value.str1"]-x["value.str2"])==120 ,
       abs(x["value.str1"]-x["value.str2"])==180 ,
       abs(x["value.str1"]-x["value.str2"])==240 ), NA, ifelse (x["value.str1"] == 300 & x["value.str2"] ==0, x["value.str1_adj"] + x["bearingdiff"], ifelse (x["value.str1"] == 0 & x["value.str2"] == 300 , x["value.str1_adj"] - x["bearingdiff"] , ifelse (x["value.str2"] < x["value.str1"] , x["value.str1_adj"] - x["bearingdiff"], ifelse ( x["value.str2"] > x["value.str1"] , x["value.str1_adj"] + x["bearingdiff"], NA ) ) ) ) ) )
Tower1_data$bearing1b <- ifelse(Tower1_data$bearing1a <0, (360+Tower1_data$bearing1a),Tower1_data$bearing1a)
Tower1_data$bearing1 <- ifelse(Tower1_data$bearing1b >360, (Tower1_data$bearing1b-360),Tower1_data$bearing1b)

#PW test
T1_PW_2<-ifelse (T1_PW>PWlow & T1_PW<PWhigh,T1_PW,0)

###########Activity#########################################################################################
T1_act <-data.frame(All$time,T1_strongest,Tower1_data$bearing1)

T1_act$SS_previous <- append(T1_act$T1_strongest, 0, 0)[-nrow(T1_act)]
T1_act$Bearing_previous <- append(T1_act$Tower1_data.bearing1, 0, 0)[-nrow(T1_act)]
T1_act$SS_change <- abs(T1_act$T1_strongest - T1_act$SS_previous)
T1_act$Bearing_change <- abs(T1_act$Tower1_data.bearing1 - T1_act$Bearing_previous)

T1_act$SS_act_test <- ifelse(T1_act$SS_change < SS_act_constant, 0, T1_act$SS_change)
T1_act$Bearing_act_test <- ifelse(T1_act$Bearing_change < Bearing_act_constant, 0, T1_act$Bearing_change)

T1_act$Activity <- ifelse (T1_act$T1_strongest < ssss,NA,
                           +ifelse (T1_act$Bearing_act_test=="NA",NA,
                                    +ifelse (T1_PW_2==0,NA,
                                             +ifelse (T1_act$SS_act_test==0,0,
                                                      +ifelse (T1_act$SS_act_test>1 & T1_act$Bearing_act_test<1,0,
                                                               +ifelse (T1_act$SS_act_test>1 & T1_act$Bearing_act_test>1,1,NA))))))

############Activity with Bearing##################
T1_ACT_bearing <- ifelse (T1_act$T1_strongest < ssss,NA,
                          +ifelse (T1_PW_2==0,NA,
                                   +ifelse (T1_act$SS_act_test==0,0,
                                            +ifelse (T1_act$SS_act_test>1, 1,NA))))


#################T2##################################################################TOWER2
T2_value0<-c(All$EE.S1.TOWER2)
T2_value60<-c(All$EE.S2.TOWER2)
T2_value120<-c(All$EE.S3.TOWER2)
T2_value180<-c(All$EE.S4.TOWER2)
T2_value240<-c(All$EE.S5.TOWER2)
T2_value300<-c(All$EE.S6.TOWER2)
T2_noise0<-c(All$EE.N1.TOWER2)
T2_noise60<-c(All$EE.N2.TOWER2)
T2_noise120<-c(All$EE.N3.TOWER2)
T2_noise180<-c(All$EE.N4.TOWER2)
T2_noise240<-c(All$EE.N5.TOWER2)
T2_noise300<-c(All$EE.N6.TOWER2)
T2_PW <- c(All$EE.PW.TOWER2)
T2_PI <- c(All$EE.PI.TOWER2)

# TO DETERMINE STRONGEST AND SECONDSTRONGEST ANTENNA###############################################
data<-data.frame(T2_value0,T2_value60,T2_value120,T2_value180,T2_value240,T2_value300)
maxn <- function(n) function(x) order(x, decreasing=TRUE)[n]
T2_strongest <- apply(data, 1, function(x)x[maxn(1)(x)])
T2_secondstrongest <- apply(data, 1, function(x)x[maxn(2)(x)])
T2_sANT <- apply(data, 1, maxn(1))
T2_ssANT <- apply(data, 1, maxn(2))
T2_strongestANT <-as.character(T2_sANT)
T2_secondstrongestANT <- as.character(T2_sANT)

difference <- (T2_strongest - T2_secondstrongest)/100
bearingdiff <-30-(-0.0624*(difference**2))-(2.8346*difference)

vals <- c("1","2","3","4","5","6")
value.str2 <- (((match(T2_ssANT, vals)-1)*60))
value.str1 <- (((match(T2_sANT, vals)-1)*60))
change.ind <- abs(match(data, vals) - match(data, vals))
value.str2_adj_a <- value.str2+adjust2
value.str2_adj_b <- ifelse(value.str2_adj_a <0,(360+value.str2_adj_a),value.str2_adj_a)
value.str2_adj <- ifelse(value.str2_adj_b >360,(value.str2_adj_b-360),value.str2_adj_b)
value.str1_adj_a <- value.str1+adjust2
value.str1_adj_b <- ifelse(value.str1_adj_a <0,(360+value.str1_adj_a),value.str1_adj_a)
value.str1_adj <- ifelse(value.str1_adj_b >360,(value.str1_adj_b-360),value.str1_adj_b)
Ant_test <- abs(value.str2-value.str1)


#NOISE
noiseantenna <-(((match(T2_sANT, vals)-1)*60))
T2_noise<- ifelse ((noiseantenna==0), T2_noise0, ifelse((noiseantenna==60), T2_noise60, ifelse((noiseantenna==120), T2_noise120, ifelse((noiseantenna==180), T2_noise180, ifelse((noiseantenna==240), T2_noise240, ifelse((noiseantenna==300), T2_noise300, NA))))))

#BEARING ESTIMATION#################################################################################
Tower2_data <- data.frame(T2_secondstrongest,Ant_test,value.str1,value.str1_adj,value.str2,value.str2_adj,bearingdiff,T2_noise,T2_PW,T2_PI)
Tower2_data$bearing2a <-apply(Tower2_data, 1, function(x) ifelse(
  any( x["T2_secondstrongest"]< ssss,
       x["T2_noise"] > n_cutoff,
       abs(x["value.str1"]-x["value.str2"])==120 ,
       abs(x["value.str1"]-x["value.str2"])==180 ,
       abs(x["value.str1"]-x["value.str2"])==240 ), NA, ifelse (x["value.str1"] == 300 & x["value.str2"] ==0, x["value.str1_adj"] + x["bearingdiff"], ifelse (x["value.str1"] == 0 & x["value.str2"] == 300 , x["value.str1_adj"] - x["bearingdiff"] , ifelse (x["value.str2"] < x["value.str1"] , x["value.str1_adj"] - x["bearingdiff"], ifelse ( x["value.str2"] > x["value.str1"] , x["value.str1_adj"] + x["bearingdiff"], NA ) ) ) ) ) )
Tower2_data$bearing2b <- ifelse(Tower2_data$bearing2a <0, (360+Tower2_data$bearing2a),Tower2_data$bearing2a)
Tower2_data$bearing2 <- ifelse(Tower2_data$bearing2b >360, (Tower2_data$bearing2b-360),Tower2_data$bearing2b)

#PW test
T2_PW_2<-ifelse (T2_PW>PWlow & T2_PW<PWhigh,T2_PW,0)

###########Activity#########################################################################################
T2_act <-data.frame(All$time,T2_strongest,Tower2_data$bearing2)

T2_act$SS_previous <- append(T2_act$T2_strongest, 0, 0)[-nrow(T2_act)]
T2_act$Bearing_previous <- append(T2_act$Tower2_data.bearing2, 0, 0)[-nrow(T2_act)]
T2_act$SS_change <- abs(T2_act$T2_strongest - T2_act$SS_previous)
T2_act$Bearing_change <- abs(T2_act$Tower2_data.bearing2 - T2_act$Bearing_previous)

T2_act$SS_act_test <- ifelse(T2_act$SS_change < SS_act_constant, 0, T2_act$SS_change)
T2_act$Bearing_act_test <- ifelse(T2_act$Bearing_change < Bearing_act_constant, 0, T2_act$Bearing_change)

T2_act$Activity <- ifelse (T2_act$T2_strongest < ssss,NA,
                           +ifelse (T2_act$Bearing_act_test=="NA",NA,
                                    +ifelse (T2_PW_2==0,NA,
                                             +ifelse (T2_act$SS_act_test==0,0,
                                                      +ifelse (T2_act$SS_act_test>1 & T2_act$Bearing_act_test<1,0,
                                                               +ifelse (T2_act$SS_act_test>1 & T2_act$Bearing_act_test>1,1,NA))))))

############Activity with Bearing##################
T2_ACT_bearing <- ifelse (T2_act$T2_strongest < ssss,NA,
                          +ifelse (T2_PW_2==0,NA,
                                   +ifelse (T2_act$SS_act_test==0,0,
                                            +ifelse (T2_act$SS_act_test>1, 1,NA))))


#################T3####################################################TOWER3###########################################################
T3_value0<-c(All$EE.S1.TOWER3)
T3_value60<-c(All$EE.S2.TOWER3)
T3_value120<-c(All$EE.S3.TOWER3)
T3_value180<-c(All$EE.S4.TOWER3)
T3_value240<-c(All$EE.S5.TOWER3)
T3_value300<-c(All$EE.S6.TOWER3)
T3_noise0<-c(All$EE.N1.TOWER3)
T3_noise60<-c(All$EE.N2.TOWER3)
T3_noise120<-c(All$EE.N3.TOWER3)
T3_noise180<-c(All$EE.N4.TOWER3)
T3_noise240<-c(All$EE.N5.TOWER3)
T3_noise300<-c(All$EE.N6.TOWER3)
T3_PW <- c(All$EE.PW.TOWER3)
T3_PI <- c(All$EE.PI.TOWER3)

# TO DETERMINE STRONGEST AND SECONDSTRONGEST ANTENNA###############################################
data<-data.frame(T3_value0,T3_value60,T3_value120,T3_value180,T3_value240,T3_value300)
maxn <- function(n) function(x) order(x, decreasing=TRUE)[n]
T3_strongest <- apply(data, 1, function(x)x[maxn(1)(x)])
T3_secondstrongest <- apply(data, 1, function(x)x[maxn(2)(x)])
T3_sANT <- apply(data, 1, maxn(1))
T3_ssANT <- apply(data, 1, maxn(2))
T3_strongestANT <-as.character(T3_sANT)
T3_secondstrongestANT <- as.character(T3_sANT)

difference <- (T3_strongest - T3_secondstrongest)/100
bearingdiff <-30-(-0.0624*(difference**2))-(2.8346*difference)

vals <- c("1","2","3","4","5","6")
value.str2 <- (((match(T3_ssANT, vals)-1)*60))
value.str1 <- (((match(T3_sANT, vals)-1)*60))
change.ind <- abs(match(data, vals) - match(data, vals))
value.str2_adj_a <- value.str2+adjust3
value.str2_adj_b <- ifelse(value.str2_adj_a <0,(360+value.str2_adj_a),value.str2_adj_a)
value.str2_adj <- ifelse(value.str2_adj_b >360,(value.str2_adj_b-360),value.str2_adj_b)
value.str1_adj_a <- value.str1+adjust3
value.str1_adj_b <- ifelse(value.str1_adj_a <0,(360+value.str1_adj_a),value.str1_adj_a)
value.str1_adj <- ifelse(value.str1_adj_b >360,(value.str1_adj_b-360),value.str1_adj_b)
Ant_test <- abs(value.str2-value.str1)

#NOISE
noiseantenna <-(((match(T3_sANT, vals)-1)*60))
T3_noise<- ifelse ((noiseantenna==0), T3_noise0, ifelse((noiseantenna==60), T3_noise60, ifelse((noiseantenna==120), T3_noise120, ifelse((noiseantenna==180), T3_noise180, ifelse((noiseantenna==240), T3_noise240, ifelse((noiseantenna==300), T3_noise300, NA))))))

#BEARING ESTIMATION#################################################################################
Tower3_data <- data.frame(T3_secondstrongest,Ant_test,value.str1,value.str1_adj,value.str2,value.str2_adj,bearingdiff,T3_noise,T3_PW,T3_PI)
Tower3_data$bearing3a <-apply(Tower3_data, 1, function(x) ifelse(
  any( x["T3_secondstrongest"]< ssss,
       x["T3_noise"] > n_cutoff,
       abs(x["value.str1"]-x["value.str2"])==120 ,
       abs(x["value.str1"]-x["value.str2"])==180 ,
       abs(x["value.str1"]-x["value.str2"])==240 ), NA, ifelse (x["value.str1"] == 300 & x["value.str2"] ==0, x["value.str1_adj"] + x["bearingdiff"], ifelse (x["value.str1"] == 0 & x["value.str2"] == 300 , x["value.str1_adj"] - x["bearingdiff"] , ifelse (x["value.str2"] < x["value.str1"] , x["value.str1_adj"] - x["bearingdiff"], ifelse ( x["value.str2"] > x["value.str1"] , x["value.str1_adj"] + x["bearingdiff"], NA ) ) ) ) ) )
Tower3_data$bearing3b <- ifelse(Tower3_data$bearing3a <0, (360+Tower3_data$bearing3a),Tower3_data$bearing3a)
Tower3_data$bearing3 <- ifelse(Tower3_data$bearing3b >360, (Tower3_data$bearing3b-360),Tower3_data$bearing3b)

#PW test
T3_PW_2<-ifelse (T3_PW>PWlow & T3_PW<PWhigh,T3_PW,0)

###########Activity#########################################################################################
T3_act <-data.frame(All$time,T3_strongest,Tower3_data$bearing3)

T3_act$SS_previous <- append(T3_act$T3_strongest, 0, 0)[-nrow(T3_act)]
T3_act$Bearing_previous <- append(T3_act$Tower3_data.bearing3, 0, 0)[-nrow(T3_act)]
T3_act$SS_change <- abs(T3_act$T3_strongest - T3_act$SS_previous)
T3_act$Bearing_change <- abs(T3_act$Tower3_data.bearing3 - T3_act$Bearing_previous)

T3_act$SS_act_test <- ifelse(T3_act$SS_change < SS_act_constant, 0, T3_act$SS_change)
T3_act$Bearing_act_test <- ifelse(T3_act$Bearing_change < Bearing_act_constant, 0, T3_act$Bearing_change)

T3_act$Activity <- ifelse (T3_act$T3_strongest < ssss,NA,
                           +ifelse (T3_act$Bearing_act_test=="NA",NA,
                                    +ifelse (T3_PW_2==0,NA,
                                             +ifelse (T3_act$SS_act_test==0,0,
                                                      +ifelse (T3_act$SS_act_test>1 & T3_act$Bearing_act_test<1,0,
                                                               +ifelse (T3_act$SS_act_test>1 & T3_act$Bearing_act_test>1,1,NA))))))

############Activity with Bearing##################
T3_ACT_bearing <- ifelse (T3_act$T3_strongest < ssss,NA,
                          +ifelse (T3_PW_2==0,NA,
                                   +ifelse (T3_act$SS_act_test==0,0,
                                            +ifelse (T3_act$SS_act_test>1, 1,NA))))


#################T4########################################################################
#TOWER4
T4_value0<-c(All$EE.S1.TOWER4)
T4_value60<-c(All$EE.S2.TOWER4)
T4_value120<-c(All$EE.S3.TOWER4)
T4_value180<-c(All$EE.S4.TOWER4)
T4_value240<-c(All$EE.S5.TOWER4)
T4_value300<-c(All$EE.S6.TOWER4)
T4_noise0<-c(All$EE.N1.TOWER4)
T4_noise60<-c(All$EE.N2.TOWER4)
T4_noise120<-c(All$EE.N3.TOWER4)
T4_noise180<-c(All$EE.N4.TOWER4)
T4_noise240<-c(All$EE.N5.TOWER4)
T4_noise300<-c(All$EE.N6.TOWER4)
T4_PW <- c(All$EE.PW.TOWER4)
T4_PI <- c(All$EE.PI.TOWER4)


# TO DETERMINE STRONGEST AND SECONDSTRONGEST ANTENNA###############################################
data<-data.frame(T4_value0,T4_value60,T4_value120,T4_value180,T4_value240,T4_value300)
maxn <- function(n) function(x) order(x, decreasing=TRUE)[n]
T4_strongest <- apply(data, 1, function(x)x[maxn(1)(x)])
T4_secondstrongest <- apply(data, 1, function(x)x[maxn(2)(x)])
T4_sANT <- apply(data, 1, maxn(1))
T4_ssANT <- apply(data, 1, maxn(2))
T4_strongestANT <-as.character(T4_sANT)
T4_secondstrongestANT <- as.character(T4_sANT)

difference <- (T4_strongest - T4_secondstrongest)/100
bearingdiff <-30-(-0.0624*(difference**2))-(2.8346*difference)

vals <- c("1","2","3","4","5","6")
value.str2 <- (((match(T4_ssANT, vals)-1)*60))
value.str1 <- (((match(T4_sANT, vals)-1)*60))
change.ind <- abs(match(data, vals) - match(data, vals))
value.str2_adj_a <- value.str2+adjust4
value.str2_adj_b <- ifelse(value.str2_adj_a <0,(360+value.str2_adj_a),value.str2_adj_a)
value.str2_adj <- ifelse(value.str2_adj_b >360,(value.str2_adj_b-360),value.str2_adj_b)
value.str1_adj_a <- value.str1+adjust4
value.str1_adj_b <- ifelse(value.str1_adj_a <0,(360+value.str1_adj_a),value.str1_adj_a)
value.str1_adj <- ifelse(value.str1_adj_b >360,(value.str1_adj_b-360),value.str1_adj_b)
Ant_test <- abs(value.str2-value.str1)

#NOISE
noiseantenna <-(((match(T4_sANT, vals)-1)*60))
T4_noise<- ifelse ((noiseantenna==0), T4_noise0, ifelse((noiseantenna==60), T4_noise60, ifelse((noiseantenna==120), T4_noise120, ifelse((noiseantenna==180), T4_noise180, ifelse((noiseantenna==240), T4_noise240, ifelse((noiseantenna==300), T4_noise300, NA))))))

#BEARING ESTIMATION#################################################################################
Tower4_data <- data.frame(T4_secondstrongest,Ant_test,value.str1,value.str1_adj,value.str2,value.str2_adj,bearingdiff,T4_noise,T4_PW,T4_PI)
Tower4_data$bearing4a <-apply(Tower4_data, 1, function(x) ifelse(
  any( x["T4_secondstrongest"]< ssss,
       x["T4_noise"] > n_cutoff,
       abs(x["value.str1"]-x["value.str2"])==120 ,
       abs(x["value.str1"]-x["value.str2"])==180 ,
       abs(x["value.str1"]-x["value.str2"])==240 ), NA, ifelse (x["value.str1"] == 300 & x["value.str2"] ==0, x["value.str1_adj"] + x["bearingdiff"], ifelse (x["value.str1"] == 0 & x["value.str2"] == 300 , x["value.str1_adj"] - x["bearingdiff"] , ifelse (x["value.str2"] < x["value.str1"] , x["value.str1_adj"] - x["bearingdiff"], ifelse ( x["value.str2"] > x["value.str1"] , x["value.str1_adj"] + x["bearingdiff"], NA ) ) ) ) ) )
Tower4_data$bearing4b <- ifelse(Tower4_data$bearing4a <0, (360+Tower4_data$bearing4a),Tower4_data$bearing4a)
Tower4_data$bearing4 <- ifelse(Tower4_data$bearing4b >360, (Tower4_data$bearing4b-360),Tower4_data$bearing4b)


#PW test
T4_PW_2<-ifelse (T4_PW>PWlow & T4_PW<PWhigh,T4_PW,0)

###########Activity#########################################################################################
T4_act <-data.frame(All$time,T4_strongest,Tower4_data$bearing4)

T4_act$SS_previous <- append(T4_act$T4_strongest, 0, 0)[-nrow(T4_act)]
T4_act$Bearing_previous <- append(T4_act$Tower4_data.bearing4, 0, 0)[-nrow(T4_act)]
T4_act$SS_change <- abs(T4_act$T4_strongest - T4_act$SS_previous)
T4_act$Bearing_change <- abs(T4_act$Tower4_data.bearing4 - T4_act$Bearing_previous)

T4_act$SS_act_test <- ifelse(T4_act$SS_change < SS_act_constant, 0, T4_act$SS_change)
T4_act$Bearing_act_test <- ifelse(T4_act$Bearing_change < Bearing_act_constant, 0, T4_act$Bearing_change)

T4_act$Activity <- ifelse (T4_act$T4_strongest < ssss,NA,
                           +ifelse (T4_act$Bearing_act_test=="NA",NA,
                                    +ifelse (T4_PW_2==0,NA,
                                             +ifelse (T4_act$SS_act_test==0,0,
                                                      +ifelse (T4_act$SS_act_test>1 & T4_act$Bearing_act_test<1,0,
                                                               +ifelse (T4_act$SS_act_test>1 & T4_act$Bearing_act_test>1,1,NA))))))

############Activity with Bearing##################
T4_ACT_bearing <- ifelse (T4_act$T4_strongest < ssss,NA,
                          +ifelse (T4_PW_2==0,NA,
                                   +ifelse (T4_act$SS_act_test==0,0,
                                            +ifelse (T4_act$SS_act_test>1, 1,NA))))


#################################################################################TRIANGULATE##############################
#y=mx+b
bearing1<-Tower1_data$bearing1
bearing2<-Tower2_data$bearing2
bearing3<-Tower3_data$bearing3
bearing4<-Tower4_data$bearing4


x1<-UTMx1
x2<-UTMx2
x3<-UTMx3
x4<-UTMx4


y1<-UTMy1
y2<-UTMy2
y3<-UTMy3
y4<-UTMy4


radian1<-bearing1*(pi/180)
radian2<-bearing2*(pi/180)
radian3<-bearing3*(pi/180)
radian4<-bearing4*(pi/180)


#t1 and t2
line<-(1/(tan(radian1)))
ma<-(1/tan(radian1))
Ca<-(y1-(x1/(tan(radian1))))
mb<-(1/tan(radian2))
Cb<-(y2-(x2/(tan(radian2))))
X12<-(Cb-Ca)/(ma-mb)
Y12<-(line*X12+Ca)

#t1 and t3
line1<-(1/(tan(radian1)))
ma1<-(1/tan(radian1))
Ca1<-(y1-(x1/(tan(radian1))))
mb1<-(1/tan(radian3))
Cb1<-(y3-(x3/(tan(radian3))))
X13<-(Cb1-Ca1)/(ma1-mb1)
Y13<-(line1*X13+Ca1)

#t1 and t4
line2<-(1/(tan(radian1)))
ma2<-(1/tan(radian1))
Ca2<-(y1-(x1/(tan(radian1))))
mb2<-(1/tan(radian4))
Cb2<-(y4-(x4/(tan(radian4))))
X14<-(Cb2-Ca2)/(ma2-mb2)
Y14<-(line2*X14+Ca2)

#t2 and t3
line5<-(1/(tan(radian2)))
ma5<-(1/tan(radian2))
Ca5<-(y2-(x2/(tan(radian2))))
mb5<-(1/tan(radian3))
Cb5<-(y3-(x3/(tan(radian3))))
X23<-(Cb5-Ca5)/(ma5-mb5)
Y23<-(line5*X23+Ca5)

#t2 and t4
line6<-(1/(tan(radian2)))
ma6<-(1/tan(radian2))
Ca6<-(y2-(x2/(tan(radian2))))
mb6<-(1/tan(radian4))
Cb6<-(y4-(x4/(tan(radian4))))
X24<-(Cb6-Ca6)/(ma6-mb6)
Y24<-(line6*X24+Ca6)


#t3 and t4
line9<-(1/(tan(radian3)))
ma9<-(1/tan(radian3))
Ca9<-(y3-(x3/(tan(radian3))))
mb9<-(1/tan(radian4))
Cb9<-(y4-(x4/(tan(radian4))))
X34<-(Cb9-Ca9)/(ma9-mb9)
Y34<-(line9*X34+Ca9)



#TOO Close to triangulate #MIGHT NEED TO DAtaframe and then uuse apply function
#CloseT1x <- ifelse (merge2$T1S1> close_cutoff,NA,
#+ifelse (merge2$T1S2> close_cutoff, NA,
#+ifelse (merge2$T1S3> close_cutoff, NA,
#+ifelse (merge2$T1S4> close_cutoff, NA,
#+ifelse (merge2$T1S5> close_cutoff, NA,
#+ifelse (merge2$T1S6> close_cutoff, NA,UTMx1)))))) 

#CloseT1y <- ifelse (merge2$T1S1> close_cutoff,NA,
#+ifelse (merge2$T1S2> close_cutoff, NA,
#+ifelse (merge2$T1S3> close_cutoff, NA,
#+ifelse (merge2$T1S4> close_cutoff, NA,
#+ifelse (merge2$T1S5> close_cutoff, NA,
#+ifelse (merge2$T1S6> close_cutoff, NA,UTMy1)))))) 

#CloseT2x <- ifelse (merge2$T2S1> close_cutoff,NA,
#+ifelse (merge2$T2S2> close_cutoff, NA,
#+ifelse (merge2$T2S3> close_cutoff, NA,
#+ifelse (merge2$T2S4> close_cutoff, NA,
#+ifelse (merge2$T2S5> close_cutoff, NA,
#+ifelse (merge2$T2S6> close_cutoff, NA,UTMx2))))))

#CloseT2y <- ifelse (merge2$T2S1> close_cutoff,NA,
#+ifelse (merge2$T2S2> close_cutoff, NA,
#+ifelse (merge2$T2S3> close_cutoff, NA,
#+ifelse (merge2$T2S4> close_cutoff, NA,
#+ifelse (merge2$T2S5> close_cutoff, NA,
#+ifelse (merge2$T2S6> close_cutoff, NA,UTMy2))))))

#CloseT3x <- ifelse (merge2$T3S1> close_cutoff,NA,
#+ifelse (merge2$T3S2> close_cutoff, NA,
#+ifelse (merge2$T3S3> close_cutoff, NA,
#+ifelse (merge2$T3S4> close_cutoff, NA,
#+ifelse (merge2$T3S5> close_cutoff, NA,
#+ifelse (merge2$T3S6> close_cutoff, NA,UTMx3))))))

#CloseT3y <- ifelse (merge2$T3S1> close_cutoff,NA,
#+ifelse (merge2$T3S2> close_cutoff, NA,
#+ifelse (merge2$T3S3> close_cutoff, NA,
#+ifelse (merge2$T3S4> close_cutoff, NA,
#+ifelse (merge2$T3S5> close_cutoff, NA,
#+ifelse (merge2$T3S6> close_cutoff, NA,UTMy3))))))

#took close out of bearing test
Bearing_test <-data.frame(bearing1,bearing2,bearing3,bearing4,X12,Y12,X13,Y13,X14,Y14,X23,Y23,X24,Y24,X34,Y34)


#X12
FinalX12 <- ifelse ((Bearing_test$X12> x1 & Bearing_test$bearing1>180), NA, 
                    + ifelse ((Bearing_test$X12< x1 & Bearing_test$bearing1<180), NA,
                              + ifelse ((Bearing_test$X12< x2 & Bearing_test$bearing2< 180), NA, 
                                        + ifelse ((Bearing_test$X12> x2 & Bearing_test$bearing2> 180), NA,Bearing_test$X12))))

#Y12
FinalY12 <- ifelse ((Bearing_test$Y12> y1 & Bearing_test$bearing1>90 & Bearing_test$bearing1<270), NA, 
                    + ifelse ((Bearing_test$Y12< y1 & Bearing_test$bearing1> 270 & Bearing_test$bearing1< 90), NA, 
                              + ifelse ((Bearing_test$Y12> y2 & Bearing_test$bearing2> 90 & Bearing_test$bearing2< 270), NA, 
                                        + ifelse ((Bearing_test$Y12< y2 & Bearing_test$bearing2> 270 & Bearing_test$bearing2< 90), NA,Bearing_test$Y12))))

#X13
FinalX13 <- ifelse ((Bearing_test$X13> x1 & Bearing_test$bearing1>180), NA, 
                    + ifelse ((Bearing_test$X13< x1 & Bearing_test$bearing1<180), NA,
                              + ifelse ((Bearing_test$X13< x3 & Bearing_test$bearing3< 180), NA, 
                                        + ifelse ((Bearing_test$X13> x3 & Bearing_test$bearing3> 180), NA,Bearing_test$X13))))

#Y13
FinalY13 <- ifelse ((Bearing_test$Y13> y1 & Bearing_test$bearing1>90 & Bearing_test$bearing1<270), NA, 
                    + ifelse ((Bearing_test$Y13< y1 & Bearing_test$bearing1> 270 & Bearing_test$bearing1< 90), NA, 
                              + ifelse ((Bearing_test$Y13> y3 & Bearing_test$bearing3> 90 & Bearing_test$bearing3< 270), NA, 
                                        + ifelse ((Bearing_test$Y13< y3 & Bearing_test$bearing3> 270 & Bearing_test$bearing3< 90), NA,Bearing_test$Y13))))

#X14
FinalX14 <- ifelse ((Bearing_test$X14> x1 & Bearing_test$bearing1>180), NA, 
                    + ifelse ((Bearing_test$X14< x1 & Bearing_test$bearing1<180), NA,
                              + ifelse ((Bearing_test$X14< x4 & Bearing_test$bearing4< 180), NA, 
                                        + ifelse ((Bearing_test$X14> x4 & Bearing_test$bearing4> 180), NA,Bearing_test$X14))))

#Y14
FinalY14 <- ifelse ((Bearing_test$Y14> y1 & Bearing_test$bearing1>90 & Bearing_test$bearing1<270), NA, 
                    + ifelse ((Bearing_test$Y14< y1 & Bearing_test$bearing1> 270 & Bearing_test$bearing1< 90), NA, 
                              + ifelse ((Bearing_test$Y14> y4 & Bearing_test$bearing4> 90 & Bearing_test$bearing4< 270), NA, 
                                        + ifelse ((Bearing_test$Y14< y4 & Bearing_test$bearing4> 270 & Bearing_test$bearing4< 90), NA,Bearing_test$Y14))))

#X23
FinalX23 <- ifelse ((Bearing_test$X23> x2 & Bearing_test$bearing2>180), NA, 
                    + ifelse ((Bearing_test$X23< x2 & Bearing_test$bearing2<180), NA,
                              + ifelse ((Bearing_test$X23< x3 & Bearing_test$bearing3< 180), NA, 
                                        + ifelse ((Bearing_test$X23> x3 & Bearing_test$bearing3> 180), NA,Bearing_test$X23))))

#Y23
FinalY23 <- ifelse ((Bearing_test$Y23> y2 & Bearing_test$bearing2>90 & Bearing_test$bearing2<270), NA, 
                    + ifelse ((Bearing_test$Y23< y2 & Bearing_test$bearing2> 270 & Bearing_test$bearing2< 90), NA, 
                              + ifelse ((Bearing_test$Y23> y3 & Bearing_test$bearing3> 90 & Bearing_test$bearing3< 270), NA, 
                                        + ifelse ((Bearing_test$Y23< y3 & Bearing_test$bearing3> 270 & Bearing_test$bearing3< 90), NA,Bearing_test$Y23))))

#X24
FinalX24 <- ifelse ((Bearing_test$X24> x2 & Bearing_test$bearing2>180), NA, 
                    + ifelse ((Bearing_test$X24< x2 & Bearing_test$bearing2<180), NA,
                              + ifelse ((Bearing_test$X24< x4 & Bearing_test$bearing4< 180), NA, 
                                        + ifelse ((Bearing_test$X24> x4 & Bearing_test$bearing4> 180), NA,Bearing_test$X24))))

#Y24
FinalY24 <- ifelse ((Bearing_test$Y24> y2 & Bearing_test$bearing2>90 & Bearing_test$bearing2<270), NA, 
                    + ifelse ((Bearing_test$Y24< y2 & Bearing_test$bearing2> 270 & Bearing_test$bearing2< 90), NA, 
                              + ifelse ((Bearing_test$Y24> y4 & Bearing_test$bearing4> 90 & Bearing_test$bearing4< 270), NA, 
                                        + ifelse ((Bearing_test$Y24< y4 & Bearing_test$bearing4> 270 & Bearing_test$bearing4< 90), NA,Bearing_test$Y24))))

#X34
FinalX34 <- ifelse ((Bearing_test$X34> x3 & Bearing_test$bearing3>180), NA, 
                    + ifelse ((Bearing_test$X34< x3 & Bearing_test$bearing3<180), NA,
                              + ifelse ((Bearing_test$X34< x4 & Bearing_test$bearing4< 180), NA, 
                                        + ifelse ((Bearing_test$X34> x4 & Bearing_test$bearing4> 180), NA,Bearing_test$X34))))

#Y34
FinalY34 <- ifelse ((Bearing_test$Y34> y3 & Bearing_test$bearing3>90 & Bearing_test$bearing3<270), NA, 
                    + ifelse ((Bearing_test$Y34< y3 & Bearing_test$bearing3> 270 & Bearing_test$bearing3< 90), NA, 
                              + ifelse ((Bearing_test$Y34> y4 & Bearing_test$bearing4> 90 & Bearing_test$bearing4< 270), NA, 
                                        + ifelse ((Bearing_test$Y34< y4 & Bearing_test$bearing4> 270 & Bearing_test$bearing4< 90), NA,Bearing_test$Y34))))


############## Boundary of the study area ########################################################################
x_min <- 435500
x_max	<- 441000
y_min <- 4448500
y_max <- 4451000

#### Estimated location via the MEAN of all combination#########################################
X_loc <-data.frame(FinalX12,FinalX13,FinalX14,FinalX23,FinalX24,FinalX34)
mean_X <- apply(X_loc, 1, mean, na.rm=TRUE)
Final_X_mean <- ifelse (mean_X<x_min, NA, ifelse (mean_X>x_max, NA, mean_X)) 

Y_loc <-data.frame(FinalY12,FinalY13,FinalY14,FinalY23,FinalY24,FinalY34)
mean_Y <-apply(Y_loc, 1, mean, na.rm=TRUE)
Final_Y_mean <- ifelse (mean_Y<y_min, NA, ifelse (mean_Y>y_max, NA, mean_Y)) 


#### Estimated location via the MEDIAN of all combination#########################################
X_loc <-data.frame(FinalX12,FinalX13,FinalX14,FinalX23,FinalX24,FinalX34)
median_X <- apply(X_loc, 1, median, na.rm=TRUE)
Final_X_median <- ifelse (median_X<x_min, NA, ifelse (median_X>x_max, NA, median_X)) 

Y_loc <-data.frame(FinalY12,FinalY13,FinalY14,FinalY23,FinalY24,FinalY34)
median_Y <-apply(Y_loc, 1, median, na.rm=TRUE)
Final_Y_median <- ifelse (median_Y<y_min, NA, ifelse (median_Y>y_max, NA, median_Y)) 

####### Estimate location via Harominic Mean
#compute the harmonic mean  = 1/mean(1/a) 
h_mean_X <- apply((1/X_loc), 1, mean, na.rm=TRUE)
h_X <- 1/h_mean_X
Final_X_h <- ifelse (h_X<x_min, NA, ifelse (h_X>x_max, NA, h_X)) 

h_mean_Y <- apply((1/Y_loc), 1, mean, na.rm=TRUE)
h_Y <- 1/h_mean_Y
Final_Y_h <- ifelse (h_Y<y_min, NA, ifelse (h_Y>y_max, NA, h_Y)) 

#geometric mean
n <- apply(X_loc, 1, function(x) length(which(!is.na(x))))
gg_X <- apply (X_loc,1,prod, na.rm=TRUE)
g_X <- gg_X^(1/n)
Final_X_g <- ifelse (g_X<x_min, NA, ifelse (g_X>x_max, NA, g_X)) 

n <- apply(Y_loc, 1, function(x) length(which(!is.na(x))))
gg_Y <- apply (Y_loc,1,prod, na.rm=TRUE)
g_Y <- gg_X^(1/n)
Final_Y_g <- ifelse (g_Y<y_min, NA, ifelse (g_Y>y_max, NA, g_Y)) 




#### Estimated location via the CLOSEST TWO TOWERS TO THE TRANSMITTER #########################################
data3 <- data.frame(T1_strongest,T2_strongest,T3_strongest,T4_strongest)
data3[data3 == 99] <- NA

my.finder <- function(mydata) {
  my.fun <- function(data3) {
    strongest2 <- which.max(data3)
    secondstrongest <- which.max(data3[-strongest2])
    strongestantenna <- names(data3)[strongest2]
    secondstrongantenna <- names(data3[-strongest2])[secondstrongest]
    value <- matrix(c(data3[strongest2], data3[-strongest2][secondstrongest],
                      strongestantenna, secondstrongantenna), ncol =4)
    return(value)
  }
  dat <- apply(mydata, 1, my.fun)
  dat <- t(dat)
  dat <- as.data.frame(dat, stringsAsFactors = FALSE)
  colnames(dat) <- c("strongest2", "secondstrongest",
                     "strongestantenna", "secondstrongantenna")
  dat[ , "strongest2"] <- as.numeric(dat[ , "strongest2"])
  dat[ , "secondstrongest"] <- as.numeric(dat[ , "secondstrongest"])
  return(dat)}

Closest<-my.finder(data3)

vals <- c("T1_strongest", "T2_strongest", "T3_strongest", "T4_strongest")
ss2 <- (match(Closest$secondstrongantenna, vals))
ss1 <- (match(Closest$strongestantenna, vals))


Final_X_locate <- 
  +ifelse (ss1 == 1 & ss2==2,FinalX12,
           +ifelse (ss1 == 2 & ss2==1,FinalX12,
                    +ifelse (ss1 == 1 & ss2==2,FinalX13,
                             +ifelse (ss1 == 3 & ss1==1,FinalX13,
                                      +ifelse (ss1 == 1 & ss2==4,FinalX14,
                                               +ifelse (ss1 == 4 & ss2==1,FinalX14,
                                                        +ifelse (ss1 == 2 & ss2==3,FinalX23,
                                                                 +ifelse (ss1 == 3 & ss2==2,FinalX23,
                                                                          +ifelse (ss1 == 2 & ss2==4,FinalX24,
                                                                                   +ifelse (ss1 == 4 & ss2==2,FinalX24,
                                                                                            +ifelse (ss1 == 3 & ss2==4,FinalX34,
                                                                                                     +ifelse (ss1 == 4 & ss2==3,FinalX34,NA))))))))))))

Final_Y_locate <- 
  +ifelse (ss1 == 1 & ss2==2,FinalY12,
           +ifelse (ss1 == 2 & ss2==1,FinalY12,
                    +ifelse (ss1 == 1 & ss2==2,FinalY13,
                             +ifelse (ss1 == 3 & ss1==1,FinalY13,
                                      +ifelse (ss1 == 1 & ss2==4,FinalY14,
                                               +ifelse (ss1 == 4 & ss2==1,FinalY14,
                                                        +ifelse (ss1 == 2 & ss2==3,FinalY23,
                                                                 +ifelse (ss1 == 3 & ss2==2,FinalY23,
                                                                          +ifelse (ss1 == 2 & ss2==4,FinalY24,
                                                                                   +ifelse (ss1 == 4 & ss2==2,FinalY24,
                                                                                            +ifelse (ss1 == 3 & ss2==4,FinalY34,
                                                                                                     +ifelse (ss1 == 4 & ss2==3,FinalY34,NA))))))))))))


###########################Which location appraoch to use######################
#Final_X <- Final_X_mean
#Final_Y <- Final_Y_mean

#Final_X <- Final_X_median
#Final_Y <- Final_Y_median

#Final_X <- Final_X_locate
#Final_Y <- Final_Y_locate

Final_X <- Final_X_h
Final_Y <- Final_Y_h

#Final_X <- Final_X_g
#Final_Y <- Final_Y_g

# DISTANCE BETWEEN CONSECUTIVE POINTS CUTOFF (FILL IN THE FOLLOWING NUMBER)>>>>>>>>>>>>>>>>>>>>>>>..................<<<<<<<<<<<<<<<<<<<<<

distance <-data.frame(Final_X, Final_Y)
distance[distance == NA] <- 99
distance$Final_X_previous <- append(distance$Final_X, 0, 0)[-nrow(distance)]
distance$Final_Y_previous <- append(distance$Final_Y, 0, 0)[-nrow(distance)]
distance_apart<- sqrt(((distance$Final_X_previous - distance$Final_X)^2)+((distance$Final_Y_previous - distance$Final_Y)^2))
distance_test<- data.frame(Final_X, Final_Y, distance_apart)
X_1 <- ifelse ((distance_test$distance_apart> distance_cutoff), NA, Final_X+20)
Y_1 <- ifelse ((distance_test$distance_apart> distance_cutoff), NA, Final_Y)

id <- 1
location<- data.frame(All$time, X_1, Y_1,datetime, id)
location <-location[!is.na(location$X_1),]
location <-location[!is.na(location$Y_1),]

total <- data.frame(datetime,Tower1_data$Ant_test,T1_strongest,T1_noise,Tower2_data$Ant_test,T2_strongest,T2_noise,Tower3_data$Ant_test,T3_strongest,T3_noise,Tower4_data$Ant_test,T4_strongest,T4_noise,distance_test$Final_X,
                    +distance_test$Final_Y,distance_test$distance_apart,
                    +X12,Y12,X13,Y13,X14,Y14,X23,Y23,X24,Y24,X34,Y34,X_1,Y_1)
write.csv(total,"total.csv")



###TO get activity on strongest tower###################################################################
data3 <- data.frame(T1_strongest,T2_strongest,T3_strongest,T4_strongest)
data3[data3 == 99] <- NA

my.finder <- function(mydata) {
  my.fun <- function(data3) {
    strongest2 <- which.max(data3)
    secondstrongest <- which.max(data3[-strongest2])
    strongestantenna <- names(data3)[strongest2]
    secondstrongantenna <- names(data3[-strongest2])[secondstrongest]
    value <- matrix(c(data3[strongest2], data3[-strongest2][secondstrongest],
                      strongestantenna, secondstrongantenna), ncol =4)
    return(value)
  }
  dat <- apply(mydata, 1, my.fun)
  dat <- t(dat)
  dat <- as.data.frame(dat, stringsAsFactors = FALSE)
  colnames(dat) <- c("strongest2", "secondstrongest",
                     "strongestantenna", "secondstrongantenna")
  dat[ , "strongest2"] <- as.numeric(dat[ , "strongest2"])
  dat[ , "secondstrongest"] <- as.numeric(dat[ , "secondstrongest"])
  return(dat)}

Closest<-my.finder(data3)

ss1 <- (match(Closest$strongestantenna, vals))

Strongest_Activity <- ifelse(ss1==1,T1_act$Activity,
                             +ifelse(ss1==2,T2_act$Activity,
                                     +ifelse(ss1==3,T3_act$Activity,
                                             +ifelse(ss1==4,T4_act$Activity,NA))))

total <- data.frame(T1_act$Activity,T2_act$Activity,T3_act$Activity,T4_act$Activity) 
total[is.na(total)] <- -1
ACT <- apply(total,1,max)
ACT[ACT==-1]<-NA


total2 <-data.frame(T1_ACT_bearing,T2_ACT_bearing,T3_ACT_bearing,T4_ACT_bearing) 
total2[is.na(total2)] <- -1
ACT_no_bearing <- apply(total2,1,max)
ACT_no_bearing[ACT_no_bearing==-1]<-NA


SS_change <- data.frame(T1_act$SS_change,T2_act$SS_change,T3_act$SS_change,T4_act$SS_change) 
SS_change[is.na(SS_change)] <- 0
SS_1 <- apply(SS_change,1,max)

Big_change <- ifelse (ACT==1 &  SS_1 > big,1,NA)


ALL <-data.frame(All$time,T1_strongest,bearing1,T2_strongest,bearing2,T3_strongest,bearing3,T4_strongest,bearing4,ACT,Strongest_Activity,SS_1,Big_change,T1_PW,T2_PW,T3_PW,T4_PW)

ALL[is.na(ALL)]   <- " " 

write.csv (ALL,"Activity.csv")

#date <- as.Date(All$time)

#time <- do.call( rbind , strsplit( as.character( All$time ) , " " ) )

FINAL <- data.frame(All$time,ACT,ACT_no_bearing,Big_change,Final_X,Final_Y)

#FINAL[is.na(FINAL)]   <- " " 

write.csv (FINAL,"Final.csv")


###
###
######################################^^^^^^^^^^^^^^^SUBSET BY DATE^^^^^^^^^^^^^^^^^^^^^##########################################
#location$date <- as.Date(location$All.time)

#sub <- as.xts(location, order.by=location$date)
#### change to the dates you want|||||||||||||||||||||\\\\\\\\\\\\\\\\\\\\\\\\\\||||||||||||||||||||||||||||||||||||||||||||||||||
#location <- sub['2013-06-09/2013-06-09']
#location <- data.frame(date=index(location), coredata(location))

#X_1 <- as.numeric(as.character(location$X_1))
#Y_1 <- as.numeric(as.character(location$Y_1))
#location <- data.frame(location$All.time,X_1,Y_1)
###
###

# Produce KML file
#MAKE SURE TO NAME THE KML FILE.................................................................................>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
plot(location$X_1, location$Y_1)
coordinates(location)<- c("X_1","Y_1")
proj4string(location) <- CRS("+proj=utm +zone=16 +north")
new <- spTransform(location, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")) 
writeOGR(new, dsn="locs.kml", layer= "new", driver="KML", dataset_options=c("NameField=name"))

write.csv(location,"locs.csv")


#Plotting Animal Movement
id<-1
data<-data.frame(X_1, Y_1,id)
xy<-data[,c("X_1","Y_1")]
id<-data[,c("id")]
path<-as.ltraj(xy, date, data$id, typeII=FALSE)
path
plot(path)


#plug-in method to determine smoothing factor

x <- location$X_1
h <- dpih(x)
bins <- seq(min(x)-h, max(x)+h, by=h)
hist(x, breaks=bins)
h

y <- location$Y_1
h2 <- dpih(y)
bins <- seq(min(y)-h2, max(y)+h2, by=h2)
hist(y, breaks=bins)
h2

SF <- (h+h2)

xy_3 <- data.frame(location$X_1,location$Y_1)
xy_2 = SpatialPoints(xy_3)
ud<-kernelUD(xy_2,h=SF,grid=500)
image(ud, axes = TRUE, addpoints = FALSE)

HR20<-getverticeshr(ud,20)
HR25<-getverticeshr(ud,25)
HR30<-getverticeshr(ud,30)
HR35<-getverticeshr(ud,35)
HR40<-getverticeshr(ud,40)
HR45<-getverticeshr(ud,45)
HR50<-getverticeshr(ud,50)
HR55<-getverticeshr(ud,55)
HR60<-getverticeshr(ud,60)
HR65<-getverticeshr(ud,65)
HR70<-getverticeshr(ud,70)
HR75<-getverticeshr(ud,75)
HR80<-getverticeshr(ud,80)
HR85<-getverticeshr(ud,85)
HR90<-getverticeshr(ud,90)
HR95<-getverticeshr(ud,95)


proj4string(HR20) <- CRS("+proj=utm +zone=16 +north")
kml(HR20)
proj4string(HR25) <- CRS("+proj=utm +zone=16 +north")
kml(HR25)
proj4string(HR30) <- CRS("+proj=utm +zone=16 +north")
kml(HR30)
proj4string(HR35) <- CRS("+proj=utm +zone=16 +north")
kml(HR35)
proj4string(HR40) <- CRS("+proj=utm +zone=16 +north")
kml(HR40)
proj4string(HR45) <- CRS("+proj=utm +zone=16 +north")
kml(HR45)
proj4string(HR50) <- CRS("+proj=utm +zone=16 +north")
kml(HR50)
proj4string(HR55) <- CRS("+proj=utm +zone=16 +north")
kml(HR55)
proj4string(HR60) <- CRS("+proj=utm +zone=16 +north")
kml(HR60)
proj4string(HR65) <- CRS("+proj=utm +zone=16 +north")
kml(HR65)
proj4string(HR70) <- CRS("+proj=utm +zone=16 +north")
kml(HR70)
proj4string(HR75) <- CRS("+proj=utm +zone=16 +north")
kml(HR75)
proj4string(HR80) <- CRS("+proj=utm +zone=16 +north")
kml(HR80)
proj4string(HR85) <- CRS("+proj=utm +zone=16 +north")
kml(HR85)
proj4string(HR90) <- CRS("+proj=utm +zone=16 +north")
kml(HR90)
proj4string(HR95) <- CRS("+proj=utm +zone=16 +north")
kml(HR95)

##############################Plotting the territory via observations#######################################################################
#beh_obs <- readOGR(".", "2c")
#beh_obs_WGS <- spTransform(beh_obs, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")) 
#writeOGR(beh_obs_WGS, dsn="Beh_obs.kml", layer="beh_obs_WGS", driver="KML")

#################################################################NEST Locations############################################################




#for the nest 
X <- 439644	
Y <- 4449361
nest <-data.frame(X,Y)
nest_location<- SpatialPoints(nest, proj4string=CRS("+proj=utm +zone=16 +north"))
kml(nest_location,colour="yellow", shape="square")




#############################################################################Brownian Bridge#################################################

#####################Dynamic Brownian Bridge Contour################################################################################################
fisp <- move(x=location$X_1, y=location$Y_1, time=as.POSIXct(location$All.time, format="%Y-%m-%d %H:%M:%OS", tz="UTC"), proj=CRS("+proj=utm +zone=16 +north"))
r <- spTransform(fisp,CRS("+proj=utm +zone=16 +north"))
fisp_dbbmm <- brownian.bridge.dyn(r, dimSize=500, location.error=25, time.step=180)
# 180 is about 12 hours

plot(fisp_dbbmm, xlab="x", ylab="y")
fisp50 <- contour(fisp_dbbmm, levels=c(.10),add=TRUE)

BB20 <- raster2contour(fisp_dbbmm, level=c(.20))
t <-SpatialLines2PolySet(BB20)
BB20 <- PolySet2SpatialPolygons(t)
proj4string(BB20) <- CRS("+proj=utm +zone=16 +north")
kml(BB20)

BB25<- raster2contour(fisp_dbbmm, level=c(.25))
t <-SpatialLines2PolySet(BB25)
BB25<- PolySet2SpatialPolygons(t)
proj4string(BB25) <- CRS("+proj=utm +zone=16 +north")
kml(BB25)

BB30 <- raster2contour(fisp_dbbmm, level=c(.30))
t <-SpatialLines2PolySet(BB30)
BB30 <- PolySet2SpatialPolygons(t)
proj4string(BB30) <- CRS("+proj=utm +zone=16 +north")
kml(BB30)

BB35<- raster2contour(fisp_dbbmm, level=c(.35))
t <-SpatialLines2PolySet(BB35)
BB35<- PolySet2SpatialPolygons(t)
proj4string(BB35) <- CRS("+proj=utm +zone=16 +north")
kml(BB35)

BB40 <- raster2contour(fisp_dbbmm, level=c(.40))
t <-SpatialLines2PolySet(BB40)
BB40 <- PolySet2SpatialPolygons(t)
proj4string(BB40) <- CRS("+proj=utm +zone=16 +north")
kml(BB40)

BB45<- raster2contour(fisp_dbbmm, level=c(.45))
t <-SpatialLines2PolySet(BB45)
BB45<- PolySet2SpatialPolygons(t)
proj4string(BB45) <- CRS("+proj=utm +zone=16 +north")
kml(BB45)

BB50 <- raster2contour(fisp_dbbmm, level=c(.50))
t <-SpatialLines2PolySet(BB50)
BB50 <- PolySet2SpatialPolygons(t)
proj4string(BB50) <- CRS("+proj=utm +zone=16 +north")
kml(BB50)

BB55<- raster2contour(fisp_dbbmm, level=c(.55))
t <-SpatialLines2PolySet(BB55)
BB55<- PolySet2SpatialPolygons(t)
proj4string(BB55) <- CRS("+proj=utm +zone=16 +north")
kml(BB55)

BB60 <- raster2contour(fisp_dbbmm, level=c(.60))
t <-SpatialLines2PolySet(BB60)
BB60 <- PolySet2SpatialPolygons(t)
proj4string(BB60) <- CRS("+proj=utm +zone=16 +north")
kml(BB60)

BB65<- raster2contour(fisp_dbbmm, level=c(.65))
t <-SpatialLines2PolySet(BB65)
BB65<- PolySet2SpatialPolygons(t)
proj4string(BB65) <- CRS("+proj=utm +zone=16 +north")
kml(BB65)

BB70 <- raster2contour(fisp_dbbmm, level=c(.70))
t <-SpatialLines2PolySet(BB70)
BB70 <- PolySet2SpatialPolygons(t)
proj4string(BB70) <- CRS("+proj=utm +zone=16 +north")
kml(BB70)

BB75 <- raster2contour(fisp_dbbmm, level=c(.75))
t <-SpatialLines2PolySet(BB75)
BB75<- PolySet2SpatialPolygons(t)
proj4string(BB75) <- CRS("+proj=utm +zone=16 +north")
kml(BB75)

BB80 <- raster2contour(fisp_dbbmm, level=c(.80))
t <-SpatialLines2PolySet(BB80)
BB80 <- PolySet2SpatialPolygons(t)
proj4string(BB80) <- CRS("+proj=utm +zone=16 +north")
kml(BB80)

BB85 <- raster2contour(fisp_dbbmm, level=c(.85))
t <-SpatialLines2PolySet(BB85)
BB85<- PolySet2SpatialPolygons(t)
proj4string(BB85) <- CRS("+proj=utm +zone=16 +north")
kml(BB85)

BB90 <- raster2contour(fisp_dbbmm, level=c(.90))
t <-SpatialLines2PolySet(BB90)
BB90 <- PolySet2SpatialPolygons(t)
proj4string(BB90) <- CRS("+proj=utm +zone=16 +north")
kml(BB90)

BB95 <- raster2contour(fisp_dbbmm, level=c(.95))
t <-SpatialLines2PolySet(BB95)
BB95 <- PolySet2SpatialPolygons(t)
proj4string(BB95) <- CRS("+proj=utm +zone=16 +north")
kml(BB95)



#TO calculate overlap between kernels...............................>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
HR20.ps <- SpatialPolygons2PolySet(HR20)
HR25.ps <- SpatialPolygons2PolySet(HR25)
HR30.ps <- SpatialPolygons2PolySet(HR30)
HR35.ps <- SpatialPolygons2PolySet(HR35)
HR40.ps <- SpatialPolygons2PolySet(HR40)
HR45.ps <- SpatialPolygons2PolySet(HR45)
HR50.ps <- SpatialPolygons2PolySet(HR50)
HR55.ps <- SpatialPolygons2PolySet(HR55)
HR60.ps <- SpatialPolygons2PolySet(HR60)
HR65.ps <- SpatialPolygons2PolySet(HR65)
HR70.ps <- SpatialPolygons2PolySet(HR70)
HR75.ps <- SpatialPolygons2PolySet(HR75)
HR80.ps <- SpatialPolygons2PolySet(HR80)
HR85.ps <- SpatialPolygons2PolySet(HR85)
HR90.ps <- SpatialPolygons2PolySet(HR90)
HR95.ps <- SpatialPolygons2PolySet(HR95)

BB20.ps <- SpatialPolygons2PolySet(BB20)
BB25.ps <- SpatialPolygons2PolySet(BB25)
BB30.ps <- SpatialPolygons2PolySet(BB30)
BB35.ps <- SpatialPolygons2PolySet(BB35)
BB40.ps <- SpatialPolygons2PolySet(BB40)
BB45.ps <- SpatialPolygons2PolySet(BB45)
BB50.ps <- SpatialPolygons2PolySet(BB50)
BB55.ps <- SpatialPolygons2PolySet(BB55)
BB60.ps <- SpatialPolygons2PolySet(BB60)
BB65.ps <- SpatialPolygons2PolySet(BB65)
BB70.ps <- SpatialPolygons2PolySet(BB70)
BB75.ps <- SpatialPolygons2PolySet(BB75)
BB80.ps <- SpatialPolygons2PolySet(BB80)
BB85.ps <- SpatialPolygons2PolySet(BB85)
BB90.ps <- SpatialPolygons2PolySet(BB90)
BB95.ps <- SpatialPolygons2PolySet(BB95)


#########################Need to change this##############################
tkml <- getKMLcoordinates(kmlfile="B3.kml", ignoreAltitude=T)
p1 = Polygon(tkml)
p2 = Polygons(list(p1), ID = "drivetime")
p3= SpatialPolygons(list(p2),proj4string=CRS("+init=epsg:4326"))

beh_obs.ps <- SpatialPolygons2PolySet(p3)
attr(beh_obs.ps, "zone") <- 16
beh_UTM <- convUL(beh_obs.ps, km=FALSE)
BO_area <- calcArea (beh_UTM, rollup=1)
#In meter*2

HR_area_20 <-calcArea(HR20.ps, rollup=1)/10000
HR_area_25 <-calcArea(HR25.ps, rollup=1)/10000
HR_area_30 <-calcArea(HR30.ps, rollup=1)/10000
HR_area_35 <-calcArea(HR35.ps, rollup=1)/10000
HR_area_40 <-calcArea(HR40.ps, rollup=1)/10000
HR_area_45 <-calcArea(HR45.ps, rollup=1)/10000
HR_area_50 <-calcArea(HR50.ps, rollup=1)/10000
HR_area_55 <-calcArea(HR55.ps, rollup=1)/10000
HR_area_60 <-calcArea(HR60.ps, rollup=1)/10000
HR_area_65 <-calcArea(HR65.ps, rollup=1)/10000
HR_area_70 <-calcArea(HR70.ps, rollup=1)/10000
HR_area_75 <-calcArea(HR75.ps, rollup=1)/10000
HR_area_80 <-calcArea(HR80.ps, rollup=1)/10000
HR_area_85 <-calcArea(HR85.ps, rollup=1)/10000
HR_area_90 <-calcArea(HR90.ps, rollup=1)/10000
HR_area_95 <-calcArea(HR95.ps, rollup=1)/10000

HR_area <- data.frame(HR_area_20,HR_area_25,HR_area_30,HR_area_35,HR_area_40,HR_area_45,HR_area_50,HR_area_55,HR_area_60,HR_area_65,HR_area_70,HR_area_75,HR_area_80,HR_area_85,HR_area_90,HR_area_95)
HR_area <- t(HR_area)

BB_area_20 <-calcArea(BB20.ps, rollup=1)/10000
BB_area_25 <-calcArea(BB25.ps, rollup=1)/10000
BB_area_30 <-calcArea(BB30.ps, rollup=1)/10000
BB_area_35 <-calcArea(BB35.ps, rollup=1)/10000
BB_area_40 <-calcArea(BB40.ps, rollup=1)/10000
BB_area_45 <-calcArea(BB45.ps, rollup=1)/10000
BB_area_50 <-calcArea(BB50.ps, rollup=1)/10000
BB_area_55 <-calcArea(BB55.ps, rollup=1)/10000
BB_area_60 <-calcArea(BB60.ps, rollup=1)/10000
BB_area_65 <-calcArea(BB65.ps, rollup=1)/10000
BB_area_70 <-calcArea(BB70.ps, rollup=1)/10000
BB_area_75 <-calcArea(BB75.ps, rollup=1)/10000
BB_area_80 <-calcArea(BB80.ps, rollup=1)/10000
BB_area_85 <-calcArea(BB85.ps, rollup=1)/10000
BB_area_90 <-calcArea(BB90.ps, rollup=1)/10000
BB_area_95 <-calcArea(BB95.ps, rollup=1)/10000

BB_area <- data.frame(BB_area_20,BB_area_25,BB_area_30,BB_area_35,BB_area_40,BB_area_45,BB_area_50,BB_area_55,BB_area_60,BB_area_65,BB_area_70,BB_area_75,BB_area_80,BB_area_85,BB_area_90,BB_area_95)
BB_area <- t(BB_area)

overHR20 <- joinPolys(beh_UTM,HR20.ps,operation="INT")
BO_HR20_area <-calcArea(overHR20, rollup=1)/10000
overHR25 <- joinPolys(beh_UTM,HR25.ps,operation="INT")
BO_HR25_area <-calcArea(overHR25, rollup=1)/10000
overHR30 <- joinPolys(beh_UTM,HR30.ps,operation="INT")
BO_HR30_area <-calcArea(overHR30, rollup=1)/10000
overHR35 <- joinPolys(beh_UTM,HR35.ps,operation="INT")
BO_HR35_area <-calcArea(overHR35, rollup=1)/10000
overHR40 <- joinPolys(beh_UTM,HR40.ps,operation="INT")
BO_HR40_area <-calcArea(overHR40, rollup=1)/10000
overHR45 <- joinPolys(beh_UTM,HR45.ps,operation="INT")
BO_HR45_area <-calcArea(overHR45, rollup=1)/10000
overHR50 <- joinPolys(beh_UTM,HR50.ps,operation="INT")
BO_HR50_area <-calcArea(overHR50,rollup=1)/10000
overHR55 <- joinPolys(beh_UTM,HR55.ps,operation="INT")
BO_HR55_area <-calcArea(overHR55, rollup=1)/10000
overHR60 <- joinPolys(beh_UTM,HR60.ps,operation="INT")
BO_HR60_area <-calcArea(overHR60, rollup=1)/10000
overHR65 <- joinPolys(beh_UTM,HR65.ps,operation="INT")
BO_HR65_area <-calcArea(overHR65, rollup=1)/10000
overHR70 <- joinPolys(beh_UTM,HR70.ps,operation="INT")
BO_HR70_area <-calcArea(overHR70, rollup=1)/10000
overHR75 <- joinPolys(beh_UTM,HR75.ps,operation="INT")
BO_HR75_area <-calcArea(overHR75, rollup=1)/10000
overHR80 <- joinPolys(beh_UTM,HR80.ps,operation="INT")
BO_HR80_area <-calcArea(overHR80, rollup=1)/10000
overHR85 <- joinPolys(beh_UTM,HR85.ps,operation="INT")
BO_HR85_area <-calcArea(overHR85, rollup=1)/10000
overHR90 <- joinPolys(beh_UTM,HR90.ps,operation="INT")
BO_HR90_area <-calcArea(overHR90, rollup=1)/10000
overHR95 <- joinPolys(beh_UTM,HR95.ps,operation="INT")
BO_HR95_area <-calcArea(overHR95, rollup=1)/10000

BO_HR_area <- data.frame(BO_HR20_area,BO_HR25_area,BO_HR30_area,BO_HR35_area,BO_HR40_area,BO_HR45_area,BO_HR50_area,BO_HR55_area,BO_HR60_area,BO_HR65_area,BO_HR70_area,BO_HR75_area,BO_HR80_area,BO_HR85_area,BO_HR90_area,BO_HR95_area)
BO_HR_area <- t(BO_HR_area)


overBB20 <- joinPolys(beh_UTM,BB20.ps,operation="INT")
BO_BB20_area <-calcArea(overBB20, rollup=1)/10000
overBB25 <- joinPolys(beh_UTM,BB25.ps,operation="INT")
BO_BB25_area <-calcArea(overBB25, rollup=1)/10000
overBB30 <- joinPolys(beh_UTM,BB30.ps,operation="INT")
BO_BB30_area <-calcArea(overBB30, rollup=1)/10000
overBB35 <- joinPolys(beh_UTM,BB35.ps,operation="INT")
BO_BB35_area <-calcArea(overBB35, rollup=1)/10000
overBB40 <- joinPolys(beh_UTM,BB40.ps,operation="INT")
BO_BB40_area <-calcArea(overBB40, rollup=1)/10000
overBB45 <- joinPolys(beh_UTM,BB45.ps,operation="INT")
BO_BB45_area <-calcArea(overBB45, rollup=1)/10000
overBB50 <- joinPolys(beh_UTM,BB50.ps,operation="INT")
BO_BB50_area <-calcArea(overBB50, rollup=1)/10000
overBB55 <- joinPolys(beh_UTM,BB55.ps,operation="INT")
BO_BB55_area <-calcArea(overBB55, rollup=1)/10000
overBB60 <- joinPolys(beh_UTM,BB60.ps,operation="INT")
BO_BB60_area <-calcArea(overBB60, rollup=1)/10000
overBB65 <- joinPolys(beh_UTM,BB65.ps,operation="INT")
BO_BB65_area <-calcArea(overBB65, rollup=1)/10000
overBB70 <- joinPolys(beh_UTM,BB70.ps,operation="INT")
BO_BB70_area <-calcArea(overBB70, rollup=1)/10000
overBB75 <- joinPolys(beh_UTM,BB75.ps,operation="INT")
BO_BB75_area <-calcArea(overBB75, rollup=1)/10000
overBB80 <- joinPolys(beh_UTM,BB80.ps,operation="INT")
BO_BB80_area <-calcArea(overBB80, rollup=1)/10000
overBB85 <- joinPolys(beh_UTM,BB85.ps,operation="INT")
BO_BB85_area <-calcArea(overBB85, rollup=1)/10000
overBB90 <- joinPolys(beh_UTM,BB90.ps,operation="INT")
BO_BB90_area <-calcArea(overBB90, rollup=1)/10000
overBB95 <- joinPolys(beh_UTM,BB95.ps,operation="INT")
BO_BB95_area <-calcArea(overBB95, rollup=1)/10000

BO_BB_area <- data.frame(BO_BB20_area,BO_BB25_area,BO_BB30_area,BO_BB35_area,BO_BB40_area,BO_BB45_area,BO_BB50_area,BO_BB55_area,BO_BB60_area,BO_BB65_area,BO_BB70_area,BO_BB75_area,BO_BB80_area,BO_BB85_area,BO_BB90_area,BO_BB95_area)
BO_BB_area <- t(BO_BB_area)

column1 <- data.frame(0,20,0,25,0,30,0,35,0,40,0,45,0,50,0,55,0,60,0,65,0,70,0,75,0,80,0,85,0,90,0,95)
Percentage <- t(column1)

stats <- data.frame (Percentage,HR_area,BB_area,BO_HR_area,BO_BB_area)
stats <- stats[-c(1,3,5,7,9,11,13,15,17,19,21,23,25,27,29,31),]
bbb <- BO_area[1,2]/10000
stats$BO_not_n_HR <- (bbb - stats$BO_HR_area)
stats$HR_not_n_BO <- (stats$HR_area - stats$BO_HR_area)
stats$HR_optimal <- (stats$BO_not_n_HR + stats$HR_not_n_BO)
stats$BO_not_n_BB <- (bbb-stats$BO_BB_area)
stats$BB_not_n_BO <- (stats$BB_area - stats$BO_BB_area)
stats$BB_optimal <- (stats$BO_not_n_BB + stats$BB_not_n_BO)

###############get centroid of BO#########################################
centroid <- getSpPPolygonsLabptSlots(p3)
centroid

center1 <- project(centroid, "+proj=utm +zone=16 ellps=WGS84")
center1

center<- SpatialPoints(center1, proj4string=CRS("+proj=utm +zone=16 +north"))
kml(center,colour="yellow", shape="square")

BOcenter_to_nest<- sqrt(((center1[,1] - nest$X)^2)+((center1[,2] - nest$Y)^2))
BOcenter_to_nest

centroid_BB <- getSpPPolygonsLabptSlots(BB60)
centroid_BB

BBcenter_to_nest<- sqrt(((centroid_BB[,1] - nest$X)^2)+((centroid_BB[,2] - nest$Y)^2))
BBcenter_to_nest


#############Convert BO################
p4 <- fortify(p3)
x<-p4$lat
y<-p4$long
polygon <- data.frame(x,y)
transform(polygon, x = as.numeric(x))
transform(polygon, y = as.numeric(y))

##############Convert nest location to decimal degree############
nest_loc_2 <- spTransform (nest_location,CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
X <- nest_loc_2$X
Y <- nest_loc_2$Y
point <-c(X,Y)

get_Point_Dist_from_Polygon <- function(.polygon, .point){
  
  # Calculate all vertex distances from the target point.
  vertex_Distance <- sqrt((.point[1] - .polygon$x)^2 + (.point[2] - .polygon$y)^2)
  
  # Select two closest vertices.
  min_1_Index <- which.min(vertex_Distance)
  min_2_Index <- which.min(vertex_Distance[-min_1_Index])
  
  # Calculate lengths of triangle sides made of
  # the target point and two closest points.
  a <- vertex_Distance[min_1_Index]
  b <- vertex_Distance[min_2_Index]
  c <- sqrt(diff(.polygon$x[c(min_1_Index, min_2_Index)])^2 + diff(.polygon$y[c(min_1_Index, min_2_Index)])^2)
  
  if(abs(min_1_Index - min_2_Index) != 1 |
     acos((b^2 + c^2 - a^2)/(2*b*c)) >= pi/2 | 
     acos((a^2 + c^2 - b^2)/(2*a*c)) >= pi/2
  ){
    # Step 3 of algorithm.
    return(vertex_Distance[min_1_Index])
  } else {
    # Step 4 of algorithm.
    # Here we are using the law of cosines.
    return(sqrt((a+b-c) * (a-b+c) * (-a+b+c) * (a+b+c)) / (2 * c))
  }
}

BOedge_to_nest <- get_Point_Dist_from_Polygon(polygon, point)
BOedge_to_nest

##############################################################BBB
bb <-fortify(BB60)
y<-bb$lat
x<-bb$long
polygon <- data.frame(x,y)
transform(polygon, x = as.numeric(x))
transform(polygon, y = as.numeric(y))

X <- nest$X
Y <- nest$Y
point <-c(X,Y)

BBedge_to_nest <- get_Point_Dist_from_Polygon(polygon, point)
BBedge_to_nest

BO_area <- data.frame(bbb,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
stats$BO_area <- t(BO_area)

BOcenter_to_nest2 <- data.frame(BOcenter_to_nest,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
stats$BO_center_to_nest <- t(BOcenter_to_nest2)

BBcenter_to_nest2 <- data.frame(BBcenter_to_nest,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
stats$BB_center_to_nest <- t(BBcenter_to_nest2)

BOedge_to_nest2 <- data.frame(BOedge_to_nest,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
stats$BOedge_to_nest <- t(BOedge_to_nest2)

BBedge_to_nest2 <- data.frame(BBedge_to_nest,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
stats$BBedge_to_nest <- t(BBedge_to_nest2)
write.csv(stats,"Area_data_2.csv")



#####################polygons without points############
xy_points <- data.frame(location$X_1,location$Y_1)

points = SpatialPoints(xy_points, proj4string=CRS("+proj=utm +zone=16 +north"))

fisp_BB_1 = spTransform(BB60, CRS("+proj=utm +zone=16 +north"))
points_1 = spTransform(points, CRS("+proj=utm +zone=16 +north"))
intersection <- gIntersects(points_1, fisp_BB_1, byid = T)
outside <- apply(intersection == TRUE, MARGIN = 2, all)

X_previous <- append(location$X_1, 0, 0)
Y_previous <- append(location$Y_1, 0, 0)
distance_apart_2<- sqrt(((X_previous - location$X_1)^2)+((Y_previous - location$Y_1)^2))
distance <- head(distance_apart_2, -1) 
foray_1 <- ifelse(outside==TRUE,0,1) 

### Points in a series are needed for a foray == 3
mav <- function(x,n=3){filter(x,rep(1/n,n), sides=2)}
fff <- mav(foray_1)
foray_f <- ifelse(fff==1,1,0) 
foray_final <- ifelse(foray_1==0,0,foray_f) 
foray <- data.frame(location$X_1,location$Y_1,location$datetime,foray_1,distance,fff,foray_final)
write.csv(foray,"forays.csv")
f <- subset (foray,foray_final==1)
ff <- data.frame(f$location.X_1,f$location.Y_1)


### go through by hand and label the forays IDs, make sure to label points in the kernel (0) if you want the full info (adehabitatLT)
##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++##############################################################
## Labe the file "forays_ID"

FFF<- read.table("forays_ID.csv", header=TRUE, sep=",")
F_F <-data.frame(FFF$location.X_1,FFF$location.Y_1)
time <- as.POSIXct (FFF$location.datetime, tz="UTC", format="%m/%d/%Y %H:%M")
path<-as.ltraj(xy=F_F, date=time, id=FFF$id, typeII=TRUE)
plot(path)

foray_summary <- ld(path)
write.csv(foray_summary,"foray_summary.csv")

### points outside
#plot(ff)

ind_forays <- ltraj2sldf(path)
proj4string(ind_forays) <- CRS("+proj=utm +zone=16 +north")
plotKML(ind_forays,colour="red")

f_points<- SpatialPoints(ff, proj4string=CRS("+proj=utm +zone=16 +north"))
kml(f_points,colour="blue", shape="circle")



