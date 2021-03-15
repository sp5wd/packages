#installing packages
install.packages("maps")
install.packages("mapdata")
installed.packages("dplr")
install.packages("sf")
install.packages("tmap")
install.packages("tmaptools")
install.packages("leaflet")
install.packages("raster")
install.packages("spData")
library(sf)
library(raster)
library(dplyr)
library(spData)
library("tmap")
library("tmaptools")
library("leaflet")
##############################################################################
setwd("~/STAT3280/NASA/Files")
setwd("~/STAT3280/NASA/Files")
ele <- read.table("../Files/elevation.dat",header=TRUE)
Lon <- colnames(ele)
Lon <- unlist(lapply(Lon,function(x)gsub("X.","",x)))  #removes the corresponding screen & change to empty
names(ele) <- Lon

image(as.matrix(t(ele[59:1,])))

image(as.matrix(log(t(ele[59:1,])+1))) #in the original data file, all of the 0s correspond to the ocean


ele.Lon <- as.numeric(Lon) #change all columns to numeric variables
ele.Lat <- as.numeric(rownames(ele)) #change all row names ot numeric variale

metric.Lon <- scan("../Files/Lon.txt",what="",sep="\t")
metric.Lon <- unlist(lapply(metric.Lon,function(x) gsub("W","",x))) #remove the w and transform to numeric value
metric.Lon <- as.numeric(metric.Lon)
metric.Lon


metric.Lat <- scan("../Files/Lat.txt",what="",sep="\n")
metric.Lat <- unlist(lapply(metric.Lat,function(x){
  x <- gsub("N","",x)
  x <- gsub("S","",x)
}))  #remove north and south labels, remove negative 

metric.Lat <- as.numeric(metric.Lat)
metric.Lat[16:24] <- metric.Lat[16:24]

approx.elevation <- matrix(0,24,24)  #approximation for 24X24 grid

for(i in 1:24){
  lat <- metric.Lat[i]
  dist.seq <- abs(ele.Lat-lat)
  lat.index <- which.min(dist.seq)
  
  for(j in 1:24){
    lon <- metric.Lon[j]
    dist.seq <- abs(ele.Lon-lon)
    lon.index <- which.min(dist.seq)
    approx.elevation[i,j] <- ele[lat.index,lon.index]
  }
}

image(log(approx.elevation+1))

elevation <- approx.elevation

save(elevation,file="ApproxElevation.Rda")
load("ApproxElevation.Rda")


m <- 24
Adj <- matrix(0,m^2,m^2)

grid.to.graph <- function(i,j,m){
  return((j-1)*m + i)
}

#chnages index from 576 to i,j,m index
graph.to.grid <- function(v,m){
  i <- v%%m
  j <- (v-i)/m + 1
  if(i==0) i<- 24
  return(c(i,j))
}

for(i in 1:m){
  for(j in 1:m){
    v <- grid.to.graph(i,j,m)
    if(i>1){
      v.upper <- grid.to.graph(i-1,j,m)
      Adj[v,v.upper] <- 1
    }
    if(i<m){
      v.lower <- grid.to.graph(i+1,j,m)
      Adj[v,v.lower] <- 1
    }
    if(j>1){
      v.left <- grid.to.graph(i,j-1,m)
      Adj[v,v.left] <- 1
    }
    if(j<m){
      v.right <- grid.to.graph(i,j+1,m)
      Adj[v,v.right] <- 1
    }
  }
}

GridTimeSeries <- list()

for(t in 1:72){
  pressure <- read.table(paste("../Files/pressure",t,".txt",sep=""),header=FALSE,skip=7,na.strings="....")
  pressure <- as.numeric(as.matrix(pressure[,-(1:3)]))
  
  ozone <- read.table(paste("../Files/ozone",t,".txt",sep=""),header=FALSE,skip=7,na.strings="....")
  ozone <- as.numeric(as.matrix(ozone[,-(1:3)]))
  
  surftemp <- read.table(paste("../Files/surftemp",t,".txt",sep=""),header=FALSE,skip=7,na.strings="....")
  surftemp <- as.numeric(as.matrix(surftemp[,-(1:3)]))
  
  cloudhigh <- read.table(paste("../Files/cloudhigh",t,".txt",sep=""),header=FALSE,skip=7,na.strings="....")
  cloudhigh <- as.numeric(as.matrix(cloudhigh[,-(1:3)]))
  
  
  cloudmid <-  read.table(paste("../Files/cloudmid",t,".txt",sep=""),header=FALSE,skip=7,na.strings="....")
  cloudmid <- as.numeric(as.matrix(cloudmid[,-(1:3)]))
  
  cloudlow <- read.table(paste("../Files/cloudlow",t,".txt",sep=""),header=FALSE,skip=7,na.strings="....")
  cloudlow <- as.numeric(as.matrix(cloudlow[,-(1:3)]))
  
  temperature <- read.table(paste("../Files/temperature",t,".txt",sep=""),header=FALSE,skip=7,na.strings="....")
  temperature <- as.numeric(as.matrix(temperature[,-(1:3)]))
  
  
  
  load("ApproxElevation.Rda")
  elevation <- as.numeric(elevation)
  
  X <- data.frame(pressure=pressure,ozone=ozone,surftemp=surftemp,cloudhigh=cloudhigh,cloudmid=cloudmid,cloudlow=cloudlow,temperature=temperature,elevation=elevation)
  
  with.missing <- sum(is.na(X)) > 0
  
  GridTimeSeries[[t]] <- list(X=X,with.missing=with.missing)
}

save(GridTimeSeries,file="NASAGridTimeSeries.Rda")
print(GridTimeSeries)
##############################################################################

#QUESTION 1 (a): plot the monthly averages across the 6 year and the locations
#location 1, jan average, feb average..cont'd
#do this for every location
##############################################################################
##############################################################################
install.packages("ggmap")
library(ggmap)
##############################################################################
##specifically for ozone:  finding average of each month by by location
ozone <- matrix(nrow = 72,ncol = 576)
for(i in 1:72){
  ozone[i,] <- GridTimeSeries[[i]]$X$ozone
}

months <- c("January","Feburary","March","April","May","June","July","August","September",
            "October","November","December")
ozone.df <- data.frame(months,ozone)

ozone_by_month <- matrix(nrow = 12,ncol = 576)
for(i in 1:576){
  ozone_by_month[,i] <- tapply(ozone.df[,i+1],ozone.df$months,mean)
}

rownames(ozone_by_month) <- months
ozone_by_month[,1:5] ## this give the average of each year's month for each location

print(metric.Lat)
print(metric.Lon)


###
metric.Lon <- -metric.Lon #changing metric lon
metric.Lon #make sure these values are negative
metric.Lat[16:24] <--metric.Lat[16:24]
print(metric.Lat)

# make the dataframe to plot by month; each location with lon, lat, month, and ozone
ozone_not_by_month <- matrix(t(ozone_by_month),nrow = 576*12, ncol = 1,byrow = TRUE)
ozone_not_by_month[1:5,1]
ozone_by_month[,1:5]
plot_df <- data.frame(lat = rep(metric.Lat,each = 24),
                      long = metric.Lon, month = rep(months,each = 576), ozone = ozone_not_by_month )
print(plot_df)
head(plot_df)


##############################################################################
library(dplyr)
# Central America countries (+a few extra to round it out)
CA.countries <- c(
  "Panama", "Costa Rica", "Venezuela", "Colombia", "Ecuador", "Guyana", "Suriname",
  "Nicaragua", "Honduras", "El Salvador", "Belize", "Guatemala", "Mexico", "Peru", "Brazil", "Bolivia"
)
# Retrieve the map data
CA.maps <- map_data("world", region = CA.countries)


# Compute the centroid as the mean longitude and latitude
# Used as label coordinate for country's names 
region.lab.data <- CA.maps %>%
  group_by(region) %>%
  summarise(long = mean(long), lat = mean(lat))

# MAP !
ggplot(CA.maps, aes(x = long, y = lat)) +
  geom_polygon(aes( group = group, fill = region))+
  scale_fill_viridis_d()+
  theme_void()+
  theme(legend.position = "none")



##############################################################################
#Creating map for every monthly average
J.data <- subset( plot_df, month=="January")
F.data <- subset( plot_df, month=="Feburary")
March.data <- subset( plot_df, month=="March")
April.data <- subset( plot_df, month=="April")
May.data <- subset( plot_df, month=="May")
June.data <- subset( plot_df, month=="June")
July.data <- subset( plot_df, month=="July")
A.data <- subset( plot_df, month=="August")
S.data <- subset( plot_df, month=="September")
O.data <- subset( plot_df, month=="October")
N.data <- subset( plot_df, month=="November")
D.data <- subset( plot_df, month=="December")
##############################################################################
## January Data
J.data <- subset( plot_df, month=="January") # graph just for January data
names(plot_df)[2] <- "long"
names(plot_df)

# adding in data points
ggplot(CA.maps, aes(x = long, y = lat)) +
  geom_polygon(aes(group = group), fill= "lightgrey", alpha= 0.7) +
  scale_fill_viridis_d()+
  theme_void()+
  theme(legend.position = "none")+
  geom_point(data=J.data, aes(x=long, y=lat))

library(viridis)
sort(unique(J.data$ozone))
mybreaks <- c(240, 270, 300, 330, 360)


ggplot(CA.maps, aes(x = long, y = lat)) +
  geom_polygon(aes(group = group),fill="lightgrey", alpha=.4)+
  scale_fill_viridis_d()+
  theme(legend.position = "left") + 
  geom_point(data=J.data, aes(x=long, y=lat, size=ozone, color=ozone, alpha=ozone), shape=20, stroke=FALSE) +
  scale_color_viridis(option="magma", trans="log", breaks=mybreaks, name="Ozone Levels") +
  scale_size_continuous(trans="log", range=c(1,12), breaks=mybreaks) + 
  scale_alpha_continuous(range=c(0.1, .9), breaks=mybreaks) + 
  labs(title = "January", color="Ozone") 


#scale_fill_discrete(breaks=mybreaks) +
####
#Creating map for every monthly average
## February Data
sort(unique(F.data$ozone))
mybreaks <- c(240, 270, 300, 330, 360)
ggplot(CA.maps, aes(x = long, y = lat)) +
  geom_polygon(aes(group = group, fill="grey", alpha=0.4))+
  scale_fill_viridis_d()+
  theme_void()+
  theme(legend.position = "none")+
  geom_point(data=F.data, aes(x=long, y=lat, size=ozone, color=ozone, alpha=ozone), shape=20, stroke=FALSE) +
  scale_color_viridis(option="magma", trans="log", breaks=mybreaks) +
  scale_size_continuous(trans="log", range=c(1,12), breaks=mybreaks) + 
  scale_alpha_continuous(range=c(0.1, .9), breaks=mybreaks) 


##march
library(viridis)
sort(unique(March.data$ozone))
mybreaks <- c(240, 270, 300, 330, 360)
ggplot(CA.maps, aes(x = long, y = lat)) +
  geom_polygon(aes(group = group, fill="grey", alpha=0.4))+
  scale_fill_viridis_d()+
  theme_void()+
  theme(legend.position = "none")+
  geom_point(data=March.data, aes(x=long, y=lat, size=ozone, color=ozone, alpha=ozone), shape=20, stroke=FALSE) +
  scale_color_viridis(option="magma", trans="log", breaks=mybreaks) +
  scale_size_continuous(trans="log", range=c(1,12), breaks=mybreaks) + 
  scale_alpha_continuous(range=c(0.1, .9), breaks=mybreaks) 

##april
library(viridis)
sort(unique(April.data$ozone))
mybreaks <- c(240, 270, 300, 330, 360)
ggplot(CA.maps, aes(x = long, y = lat)) +
  geom_polygon(aes(group = group, fill="grey", alpha=0.4))+
  scale_fill_viridis_d()+
  theme_void()+
  theme(legend.position = "none")+
  geom_point(data=April.data, aes(x=long, y=lat, size=ozone, color=ozone, alpha=ozone), shape=20, stroke=FALSE) +
  scale_color_viridis(option="magma", trans="log", breaks=mybreaks) +
  scale_size_continuous(trans="log", range=c(1,12), breaks=mybreaks) + 
  scale_alpha_continuous(range=c(0.1, .9), breaks=mybreaks) 
##may
library(viridis)
sort(unique(May.data$ozone))
mybreaks <- c(240, 270, 300, 330, 360)
ggplot(CA.maps, aes(x = long, y = lat)) +
  geom_polygon(aes(group = group, fill="grey", alpha=0.4))+
  scale_fill_viridis_d()+
  theme_void()+
  theme(legend.position = "none")+
  geom_point(data=May.data, aes(x=long, y=lat, size=ozone, color=ozone, alpha=ozone), shape=20, stroke=FALSE) +
  scale_color_viridis(option="magma", trans="log", breaks=mybreaks) +
  scale_size_continuous(trans="log", range=c(1,12), breaks=mybreaks) + 
  scale_alpha_continuous(range=c(0.1, .9), breaks=mybreaks) 
##june
library(viridis)
sort(unique(June.data$ozone))
mybreaks <- c(240, 270, 300, 330, 360)
ggplot(CA.maps, aes(x = long, y = lat)) +
  geom_polygon(aes(group = group, fill="grey", alpha=0.4))+
  scale_fill_viridis_d()+
  theme_void()+
  theme(legend.position = "none")+
  geom_point(data=June.data, aes(x=long, y=lat, size=ozone, color=ozone, alpha=ozone), shape=20, stroke=FALSE) +
  scale_color_viridis(option="magma", trans="log", breaks=mybreaks) +
  scale_size_continuous(trans="log", range=c(1,12), breaks=mybreaks) + 
  scale_alpha_continuous(range=c(0.1, .9), breaks=mybreaks) 
##july
sort(unique(July.data$ozone))
mybreaks <- c(240, 270, 300, 330, 360)
ggplot(CA.maps, aes(x = long, y = lat)) +
  geom_polygon(aes(group = group, fill="grey", alpha=0.4))+
  scale_fill_viridis_d()+
  theme_void()+
  theme(legend.position = "none")+
  geom_point(data=July.data, aes(x=long, y=lat, size=ozone, color=ozone, alpha=ozone), shape=20, stroke=FALSE) +
  scale_color_viridis(option="magma", trans="log", breaks=mybreaks) +
  scale_size_continuous(trans="log", range=c(1,12), breaks=mybreaks) + 
  scale_alpha_continuous(range=c(0.1, .9), breaks=mybreaks) 
##august
sort(unique(A.data$ozone))
mybreaks <- c(240, 270, 300, 330, 360)
ggplot(CA.maps, aes(x = long, y = lat)) +
  geom_polygon(aes(group = group, fill="grey", alpha=0.4))+
  scale_fill_viridis_d()+
  theme_void()+
  theme(legend.position = "none")+
  geom_point(data=A.data, aes(x=long, y=lat, size=ozone, color=ozone, alpha=ozone), shape=20, stroke=FALSE) +
  scale_color_viridis(option="magma", trans="log", breaks=mybreaks) +
  scale_size_continuous(trans="log", range=c(1,12), breaks=mybreaks) + 
  scale_alpha_continuous(range=c(0.1, .9), breaks=mybreaks) 
##september
sort(unique(S.data$ozone))
mybreaks <- c(240, 270, 300, 330, 360)
ggplot(CA.maps, aes(x = long, y = lat)) +
  geom_polygon(aes(group = group, fill="grey", alpha=0.4))+
  scale_fill_viridis_d()+
  theme_void()+
  theme(legend.position = "none")+
  geom_point(data=S.data, aes(x=long, y=lat, size=ozone, color=ozone, alpha=ozone), shape=20, stroke=FALSE) +
  scale_color_viridis(option="magma", trans="log", breaks=mybreaks) +
  scale_size_continuous(trans="log", range=c(1,12), breaks=mybreaks) + 
  scale_alpha_continuous(range=c(0.1, .9), breaks=mybreaks) 
##oct
sort(unique(O.data$ozone))
mybreaks <- c(240, 270, 300, 330, 360)
ggplot(CA.maps, aes(x = long, y = lat)) +
  geom_polygon(aes(group = group, fill="grey", alpha=0.4))+
  scale_fill_viridis_d()+
  theme_void()+
  theme(legend.position = "none")+
  geom_point(data=O.data, aes(x=long, y=lat, size=ozone, color=ozone, alpha=ozone), shape=20, stroke=FALSE) +
  scale_color_viridis(option="magma", trans="log", breaks=mybreaks) +
  scale_size_continuous(trans="log", range=c(1,12), breaks=mybreaks) + 
  scale_alpha_continuous(range=c(0.1, .9), breaks=mybreaks) 
##nov
sort(unique(N.data$ozone))
mybreaks <- c(240, 270, 300, 330, 360)
ggplot(CA.maps, aes(x = long, y = lat)) +
  geom_polygon(aes(group = group, fill="grey", alpha=0.4))+
  scale_fill_viridis_d()+
  theme_void()+
  theme(legend.position = "none")+
  geom_point(data=N.data, aes(x=long, y=lat, size=ozone, color=ozone, alpha=ozone), shape=20, stroke=FALSE) +
  scale_color_viridis(option="magma", trans="log", breaks=mybreaks) +
  scale_size_continuous(trans="log", range=c(1,12), breaks=mybreaks) + 
  scale_alpha_continuous(range=c(0.1, .9), breaks=mybreaks) 
##dec
sort(unique(D.data$ozone))
mybreaks <- c(240, 270, 300, 330, 360)
ggplot(CA.maps, aes(x = long, y = lat)) +
  geom_polygon(aes(group = group, fill="grey", alpha=0.4))+
  scale_fill_viridis_d()+
  theme_void()+
  theme(legend.position = "none")+
  geom_point(data=D.data, aes(x=long, y=lat, size=ozone, color=ozone, alpha=ozone), shape=20, stroke=FALSE) +
  scale_color_viridis(option="magma", trans="log", breaks=mybreaks) +
  scale_size_continuous(trans="log", range=c(1,12), breaks=mybreaks) + 
  scale_alpha_continuous(range=c(0.1, .9), breaks=mybreaks) 


##############################################################################
##############################################################################

#QUESTION 1 (b): asks about the yearly trend
#taking the average ozone for every year and showing the trend
#can use any mechnism for the trend
#do this for every location

# each years average for every location
ozone <- matrix(nrow = 72,ncol = 576)
for(i in 1:72){
  ozone[i,] <- GridTimeSeries[[i]]$X$ozone
}

years <- c("year1","year2","year3","year4","year5","year6")
ozone.dfyears <- data.frame(years,ozone)
ozone.dfyears

ozone_by_year <- matrix(nrow = 6,ncol = 576)
for(i in 1:576){
  ozone_by_year[,i] <- tapply(ozone.df[,i+1],ozone.dfyears$years,mean)
}
rownames(ozone_by_year) <- years
ozone_by_year[,1:15] ## this give the average of each year's month for each location


## average ozone of each year for every location (lon, lat) 
ozone_not_by_year <- matrix(t(ozone_by_year),nrow = 576*6, ncol = 1,byrow = TRUE)
ozone_not_by_year[1:5,1]
ozone_by_year[,1:5]
plot_yearavg <- data.frame(lat = rep(metric.Lat,each = 24),
                      lon = metric.Lon, years = rep(years,each = 576), ozone = ozone_not_by_year )

print(plot_yearavg)

##############################################################################
##############################################################################
#QUESTION 2: evaluating the nationality bias from the judges
setwd("~/STAT3280")
x <- read.csv("Diving2000.csv", as.is=TRUE)
dim(x)
head(x, 200)
##############################################################################
# The fast version of some preliminary calculations:
q <- matrix(x$score, 7, 1541)
averages <- apply(q, 2, mean)
x$avg <- rep(averages, each=7)
x$avg6 <- (7*x$avg-x$score)/6
x$diff <- x$score - x$avg6

hist(x$diff)

# variable indicating "matching" between judge and diver country:
x$match <- (x$jcountry == x$dcountry)
head(x)
##############################################################################
#bias
bias <- function(x, judge, country) {
  temp <- mean(x$diff[x$judge==judge &
                        x$dcountry==country])
  temp <- temp - mean(x$diff[x$judge==judge &
                               x$dcountry!=country])
  return(temp)
}
##############################################################################
#jinfo
jinfo <- x[!duplicated(x$judge), 9:10] #create table of all of the judges, **need this table
jinfo
jinfo <- jinfo[order(jinfo$jcountry),]
rownames(jinfo) <- NULL
jinfo$numscores <- 0
jinfo$nummatch <- 0
jinfo$numother <- 0
jinfo$ownbias <- 0
for (i in 1:nrow(jinfo)) {
  thisjudge <- jinfo$judge[i]
  thiscountry <- jinfo$jcountry[i]
  y <- x[x$judge==thisjudge,]
  jinfo$numscores[i] <- nrow(y)
  jinfo$nummatch[i] <- sum(y$match)
  jinfo$numother[i] <- sum(!y$match)
  jinfo$ownbias[i] <- bias(y, thisjudge, thiscountry)
} 

print(jinfo)
##############################################################################
##with function above calculate differences,
#we want to use t test to see if bias is significant from zero, summarize everything
analysis <- function(x, judge, country) {
  if (sum(x$judge==judge &
          x$dcountry==country) > 1) {
    temp <- mean(x$diff[x$judge==judge &
                          x$dcountry==country])
    temp <- temp - mean(x$diff[x$judge==judge &
                                 x$dcountry!=country])
    testresult <- t.test(x$diff[x$judge==judge &
                                  x$dcountry==country],
                         x$diff[x$judge==judge &
                                  x$dcountry!=country])
    return(list(statistic=temp,
                p.value=testresult$p.value,
                n1=sum(x$judge==judge &
                         x$dcountry==country),
                n2=sum(x$judge==judge &
                         x$dcountry!=country)))
  } else {
    return(list(statistics=NA, p.value=NA, n1=NA, n2=NA))
  }
}

## figures for all the judges; on average there is a bias for their own countries
par(mfrow=c(4,4),mar=c(0,0,0,0))
for (i in 1:nrow(jinfo)) {
  thisjudge <- jinfo$judge[i]
  thiscountry <- jinfo$jcountry[i]
  if (thisjudge!="WANG Facheng" & jinfo$nummatch[i]>0) {
    y <- x[x$judge==thisjudge,]
    plot(density(y$diff[!y$match]), xlim=c(-3,3), ylim=c(0,1.25),
         xlab="", main="", ylab="", lwd=2, yaxt="n", xaxt="n")
    lines(density(y$diff[y$match]), col="red", lwd=2)
    abline(v=mean(y$diff[y$match]), col="red", lwd=2,lty=2)
    abline(v=mean(y$diff[!y$match]), lwd=2,lty=2)
    text(-2, 1, paste("# match:", sum(y$match)), col="red")
    text(-2, 0.9, paste("# non-match:", sum(!y$match)))
    text(2, 1, paste(thisjudge, "\n", thiscountry))
  }
}
##############################################################################
## Below from jinfo for SUI
#GEISSBUHLER Michael      SUI       401        3      398  0.792643774
## code for bias towards SUI v. non-SUI by SUI judge
suijudge <- "GEISSBUHLER Michael"
suicountry <- "SUI"
analysis(x, suijudge, suicountry)
bias(x, "GEISSBUHLER Michael", "SUI") #0.7926438
bias(x, "GEISSBUHLER Michael", "CHN") #-0.2323836
bias(x, "GEISSBUHLER Michael", "MEX") #0.04626553
bias(x, "GEISSBUHLER Michael", "RUS") #0.07577816
bias(x, "GEISSBUHLER Michael", "AUS") #-0.0212906
bias(x, "GEISSBUHLER Michael", "GER") #0.1171018
jinfo
GM <- x[x$judge=="GEISSBUHLER Michael",]
head(GM)
par(mfrow=c(2,1))
hist(GM$diff[GM$match], breaks=seq(-3,3,by=0.25),
     xlab="Scoring Differences", main="GEISSBUHLER: SUI Divers") ##histo rating SUI
abline(v=mean(GM$diff[GM$match]), lwd=4, col="red")
hist(GM$diff[!GM$match], breaks=seq(-3,3,by=0.25),
     xlab="Scoring Differences", main="GEISSBUHLER: non-SUI Divers")  ##histo rating for other countries
abline(v=mean(GM$diff[!GM$match]), lwd=4, col="green")

##############################################################################
#Code for bias bar plot for European judges v. non_europen judges:
## bias towards countries in their continent; does not include those that have NaN
europejudges <- jinfo[-c(1,3:9,15:20,23:25),]
print(europejudges)

europe <- par(mfrow=c(4,4),mar=c(0,0,0,0))
for (i in 1:nrow(europejudges)) {
  thisjudge <- europejudges$judge[i]
  thiscountry <- europejudges$jcountry[i]
  if (thisjudge!="WANG Facheng" & europejudges$nummatch[i]>0) {
    y <- x[x$judge==thisjudge,]
    plot(density(y$diff[!y$match]), xlim=c(-3,3), ylim=c(0,1.25),
         xlab="", main="", ylab="", lwd=2, yaxt="n", xaxt="n")
    title("Europe's Own Bias",line=-2, adj=0,cex=2)
    lines(density(y$diff[y$match]), col="blue", lwd=2)
    abline(v=mean(y$diff[y$match]), col="blue", lwd=2,lty=2)
    abline(v=mean(y$diff[!y$match]), lwd=2,lty=1)
    text(-2, 0.5, paste("# match:", sum(y$match)), col="blue")
    text(-2, 0.9, paste("# non-match:", sum(!y$match)))
    text(2, 1, paste(thisjudge, "\n", thiscountry))
  }
}


## non-european countries; does not include those that have NaN
noeurope <-jinfo[-c(2,8:15,17:19,21:23,25),]
print(noeurope)

not_europe <- par(mfrow=c(4,4),mar=c(0,0,0,0))
for (i in 1:nrow(noeurope)) {
  thisjudge <- noeurope$judge[i]
  thiscountry <- noeurope$jcountry[i]
  if (thisjudge!="GEISSBUHLER Michael" & noeurope$nummatch[i]>0) {
    y <- x[x$judge==thisjudge,]
    plot(density(y$diff[!y$match]), xlim=c(-3,3), ylim=c(0,1.25),
         xlab="", main="", ylab="", lwd=2, yaxt="n", xaxt="n")
    title("Non-Europe's Own Bias",line=-2, adj=0,cex=2)
    lines(density(y$diff[y$match]), col="green", lwd=2)
    abline(v=mean(y$diff[y$match]), col="green", lwd=2,lty=2)
    abline(v=mean(y$diff[!y$match]), lwd=2,lty=2)
    text(-2, 0.5, paste("# match:", sum(y$match)), col="green")
    text(-2, 0.9, paste("# non-match:", sum(!y$match)))
    text(2, 1, paste(thisjudge, "\n", thiscountry))
  }
}
##############################################################################

##installing package:
install.packages("ggpubr")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("forcats")
install.packages("gridExtra")
install.packages("tidypaleo")

##############################################################################
#Code for bias bar plot:
no_NaaN <- jinfo[complete.cases(jinfo), ]   # removes NaaN biases
print(no_NaaN)
p<-ggplot(data=no_NaaN, aes(x=judge, y=ownbias, fill=factor(jcountry))) + geom_bar(stat="identity")
p
p <- p + coord_flip()+ labs(x = "Judge", y = "Bias towards their own country", fill="Country")
p 
no_NaaN <- jinfo[complete.cases(jinfo), ]   # removes NaaN biases

##############################################################################
## bias towards countries in their continent; does not include those that have NaN
jinfo
europejudges <- jinfo[-c(1,3:9,15:20,23:25),]
print(europejudges)

## non-european countries; does not include those that have NaN
noeurope <-jinfo[-c(2,8:15,17:19,21:23,25),]
print(noeurope)

#Code for bias bar plot for European judges v. non_europen judges:
print(europejudges)
EJ<-ggplot(data=europejudges, aes(x=reorder(judge,-ownbias), y=ownbias, fill=factor(jcountry))) + geom_bar(stat="identity")
EJ
EJ <- EJ + coord_flip()+ labs(x = "Judge", y = "European Judges: Bias towards their own country",title = "Europe v. Non-Europe: Each Judge's Bias", fill="Country") + 
  theme(plot.title = element_text(hjust = 0.5)) + geom_text(aes(label = jcountry),hjust = .001, vjust=.5, size = 3.1)+ ylim(-0.05, 0.8)
EJ
print(noeurope)
NEJ<-ggplot(data=noeurope, aes(x=reorder(judge,-ownbias), y=ownbias, fill=factor(jcountry))) + geom_bar(stat="identity")
NEJ
NEJ <- NEJ + coord_flip()+ labs(x = "Judge", y = "Non-European Judges: Bias towards their own country", fill="Country")+ 
  theme(plot.title = element_text(hjust = 0.5)) + geom_text(aes(label = noeurope$jcountry),hjust = .001, vjust=.5, size = 3.1)+ ylim(-0.05, 0.8)
NEJ
grid.arrange(EJ, NEJ, ncol=1)
##############################################################################
##############################################################################
#bias for GEISSBUHLER Michael for eruopean countries
suijudge <- "GEISSBUHLER Michael"
suicountry <- "SUI"
analysis(x, suijudge, suicountry)
s<-bias(x, "GEISSBUHLER Michael", "SUI") #0.7926438
g<-bias(x, "GEISSBUHLER Michael", "GER") #0.1171018
r<-bias(x, "GEISSBUHLER Michael", "RUS") #0.07577816
a<-bias(x, "GEISSBUHLER Michael", "AUT") # -0.02975913
e<-bias(x, "GEISSBUHLER Michael", "ESP") #0.3756681
f<-bias(x, "GEISSBUHLER Michael", "FRA") #0.08608269
gb<-bias(x, "GEISSBUHLER Michael", "GBR") #0.1595437


suieurope <- data.frame("Country"=c("SUI","GER","RUS","AUT","ESP","FRA","GBR"),"SUI's Bias"= 
                          c(0.7926438,0.1171018,0.07577816,-0.02975913,0.3756681,0.08608269,0.1595437))
print(suieurope)

## data frame for bias and non-european countries
suijudge <- "GEISSBUHLER Michael"
suicountry <- "SUI"
analysis(x, suijudge, suicountry)
au<-bias(x, "GEISSBUHLER Michael", "AUS") #-0.0212906
c<-bias(x, "GEISSBUHLER Michael", "CAN") #-0.1651192
ch<-bias(x, "GEISSBUHLER Michael", "CHN") #-0.2323836
cu<-bias(x, "GEISSBUHLER Michael", "CUB") # -0.2441077
m<-bias(x, "GEISSBUHLER Michael", "MEX") # 0.04626553
p<-bias(x, "GEISSBUHLER Michael", "PUR") #-0.160533
u<-bias(x, "GEISSBUHLER Michael", "USA") #-0.05905523

sui_noteurope <- data.frame("Non-Europeann Country"=c("AUS","CAN","CHN","CUB","MEX","PUR","USA"),"SUI's Bias"= 
                              c(-0.0212906,-0.1651192,-0.2323836,-0.2441077,0.04626553,0.160533,-0.05905523))
print(sui_noteurope)

#combining the 2 dataframes
sui_noteurope <- data.frame("Non-European Country"=c("AUS","CAN","CHN","CUB","MEX","PUR","USA"),"SUI's Bias1"= 
                              c(-0.0212906,-0.1651192,-0.2323836,-0.2441077,0.04626553,0.160533,-0.05905523))
print(sui_noteurope)

suieurope <- data.frame("European Country"=c("SUI","GER","RUS","AUT","ESP","FRA","GBR"),"SUI's Bias2"= 
                          c(0.7926438,0.1171018,0.07577816,-0.02975913,0.3756681,0.08608269,0.1595437))
print(suieurope)

all_countries_SUIbias <-data.frame(sui_noteurope,suieurope)
print(all_countries_SUIbias)

#scatter plot of bias from SUI

sui_judgebias <- data.frame("Country"=c("AUS","CAN","CHN","CUB","MEX","PUR","USA",
                                        "SUI","GER","RUS","AUT","ESP","FRA","GBR"),"SUI's Judge's Bias"= c(-0.0212906,
                                                                                                           -0.1651192,-0.2323836,-0.2441077,0.04626553,0.160533,-0.05905523,0.7926438,0.1171018,
                                                                                                           0.07577816,-0.02975913,0.3756681,0.08608269,0.1595437))
print(sui_judgebias)

sui_plot <- ggplot(data=sui_judgebias, aes(x=reorder(Country,-sui_judgebias$SUI.s.Judge.s.Bias), y=SUI.s.Judge.s.Bias)) + geom_bar(stat="identity") 
sui_plot1 <- sui_plot + labs(x = "Country", y = "SUI's Judge's Bias", title = "Most Biased Towards Their Own Country: SUI") + 
  geom_hline(yintercept = mean(sui_judgebias$SUI.s.Judge.s.Bias), color ="blue") +
  theme(plot.title = element_text(hjust = 0.5))
print(sui_plot1)


##############################################################################
#Bias calculations for WANG Facheng
## data frame for bias and non-european countries
chnjudge <- "WANG Facheng"
chncountry <- "CHN"
analysis(x, chnjudge, chncountry)
print(wau<-bias(x, "WANG Facheng", "AUS")) #-0.3887494
print(wc<-bias(x, "WANG Facheng", "CAN")) #-0.2361546
print(wch<-bias(x, "WANG Facheng", "CHN")) #-0.001232474
print(wcu<-bias(x, "WANG Facheng", "CUB")) #0.1328431
print(wm<-bias(x, "WANG Facheng", "MEX")) # -0.1011473
print(wp<-bias(x, "WANG Facheng", "PUR")) # NaN
print(wu<-bias(x, "WANG Facheng", "USA")) #-0.227284
print(ws<-bias(x, "WANG Facheng", "SUI")) #-0.4277307
print(wg<-bias(x, "WANG Facheng", "GER")) #-0.2434306
print(wr<-bias(x, "WANG Facheng", "RUS")) #-0.0738919
print(wa<-bias(x, "WANG Facheng", "AUT")) #-0.4837571
print(we<-bias(x, "WANG Facheng", "ESP")) #0.02820048
print(wf<-bias(x, "WANG Facheng", "FRA")) #0.2608025
print(wgb<-bias(x, "WANG Facheng", "GBR")) #0.1019608

#scatter plot of bias from CHN (note: CHN does not have an entry for PUR)

chn_judgebias <- data.frame("Country"=c("AUS","CAN","CHN","CUB","MEX","USA",
                                        "SUI","GER","RUS","AUT","ESP","FRA","GBR"),
"CHN's Judge's Bias"= c(-0.3887494,-0.2361546,-0.001232474,0.1328431,-0.1011473,-0.22728,-0.4277307,-0.2434306,
                        -0.0738919,-0.4837571,0.02820048,0.2608025,0.1019608))
print(chn_judgebias)

chn_plot <-ggplot(data=chn_judgebias, aes(x=reorder(Country, -CHN.s.Judge.s.Bias), y=CHN.s.Judge.s.Bias)) + geom_bar(stat="identity") 
chn_plot1 <- chn_plot + labs(x = "Country", y = "CHN's Judge's Bias",title = "Least Bias Towards Their Own Country: CHN") +
  geom_hline(yintercept = mean(CHN.s.Judge.s.Bias), color = "blue")  
print(chn_plot1)


grid.arrange(sui_judgebias, chn_judgebias, ncol=2)
##############################################################################

jinfo

##############################################################################
#rearrange data w/ not o code #DO NOT PUT THIS CODE IN THE FINAL DOC
x <- read.csv("Diving2000.csv", as.is=TRUE)
dim(x)
head(x, 10)
head(jinfo)
unique(x$event)

own_scoredtotal <-ggplot(data = jinfo, aes(x=reorder(jinfo$numscores, jinfo$numscores), y=jinfo$ownbias)) + geom_bar(stat="identity")
own_scoredtotal1 <- own_scoredtotal + labs(x = "Number of Times Judge Has Scored", y = "Judges' Own Bias",title = "Does the Number of Times a Judge Score Affect Their Own Bias") +
  geom_hline(yintercept = mean(jinfo$ownbias), color = "blue") 
print(own_scoredtotal1)
geom_ab<-lm(jinfo$numscores ~ jinfo$ownbias, data = jinfo)

####
chn_judgebias <- data.frame("Country"=c("AUS","CAN","CHN","CUB","MEX","USA",
                                        "SUI","GER","RUS","AUT","ESP","FRA","GBR"),
                            "CHN's Judge's Bias"= c(-0.3887494,-0.2361546,-0.001232474,0.1328431,-0.1011473,-0.22728,-0.4277307,-0.2434306,
                                                    -0.0738919,-0.4837571,0.02820048,0.2608025,0.1019608))
print(chn_judgebias)

print(x$round)
cat(sprintf(x$round, x$jcountry))

