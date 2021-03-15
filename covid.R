library(ggplot2)
confirmed_world <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv",
                            stringsAsFactors = FALSE,
                            check.names =  FALSE)
# install.packages("reshape2")
library(reshape2)
### again, we will use the package reshape2 to transform this data set to a more convenient format
confirmed_world <- reshape2::melt(confirmed_world, id.vars = c("Province/State", "Country/Region", "Lat", "Long"), variable.name = "Date", value.name = "Confirmed")
##### load death counts and recovered counts
death_world <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv",stringsAsFactors = FALSE,
                        check.names =  FALSE)
recovered_world <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv",stringsAsFactors = FALSE, check.names =  FALSE)

death_world <- reshape2::melt(death_world, id.vars = c("Province/State", "Country/Region", "Lat", "Long"), variable.name = "Date", value.name = "Death")
recovered_world <- reshape2::melt(recovered_world, id.vars = c("Province/State", "Country/Region", "Lat", "Long"), variable.name = "Date", value.name = "Recovered")
head(death_world)
head(recovered_world)
world_history_data <- dplyr::left_join(confirmed_world, death_world, by = c("Province/State", "Country/Region", "Lat", "Long", "Date"))
head(world_history_data)
world_history_data <- dplyr::left_join(world_history_data, recovered_world, by = c("Province/State", "Country/Region", "Lat", "Long", "Date"))
head(world_history_data,20)
world_history_data$Date <- as.Date(as.character(world_history_data$Date), format = c("%m/%d/%y"))
head(world_history_data,20)
colnames(world_history_data) <- make.names(colnames(world_history_data))
colnames(world_history_data)
head(world_history_data,20)       ### world_history_data is the table that holds confirmed, death, and recovered data

################################
# set ggplot colors
library(RColorBrewer)
cols <- matrix(c(brewer.pal(9,"Set1"),brewer.pal(11,"Set3")),ncol=1)
################################

length(unique(world_history_data$Country.Region))
library(plyr)
world.summary.data <- ddply(world_history_data,.(Country.Region, Date),function(x){
  colSums(x[,c("Confirmed","Death","Recovered")])
})
dim(world.summary.data) #### too many of them, look at the countrires with the most confirmed cases

################################
# Most recent day setting! Make sure it works before submitting HW
lastday <- max(world.summary.data$Date)
# lastday <- "2020-04-04" -- set it manually if necessary
################################

world.summary.data <- world.summary.data[world.summary.data$Date<=lastday,] # all dates excluding most recent day
yesterday.data <- world.summary.data[world.summary.data$Date==lastday,]     # data for most recent day

sort.index <- sort(yesterday.data$Confirmed,decreasing=TRUE,index.return=TRUE)$ix

yesterday.data.major  <- yesterday.data[sort.index[1:20],]
yesterday.data.major
yesterday.data.major  <- data.frame(Country.Region=yesterday.data$Country.Region[sort.index[1:20]])
yesterday.data.major
yesterday.data.major$Country.Region  <- as.character(yesterday.data.major$Country.Region )
major.summary.data <- dplyr::inner_join(world.summary.data,yesterday.data.major,by = "Country.Region")
dim(major.summary.data)
head(major.summary.data)
length(unique(major.summary.data$Country.Region))
rownames(cols) <- unique(major.summary.data$Country.Region)
major.summary.data$Country.Region <- factor(major.summary.data$Country.Region,
                                            levels = rev(yesterday.data.major$Country.Region))

# Graphs of confirmed cases (with no population data!) ##########################################
# ggplot(major.summary.data[major.summary.data$Date==lastday,], aes(x = Country.Region, y = Confirmed, fill = Country.Region)) +
#  geom_col() + scale_fill_manual(values = cols) + theme_minimal() +
#  ylab("Confirmed") + xlab("") + labs(color = "Country/Region") + coord_flip() +
#  theme(text = element_text(size = 12), axis.text.x = element_text(angle = 90),
#        legend.position = "none")

# ggplot(major.summary.data[major.summary.data$Date==lastday,], aes(x = Country.Region, y = log(Confirmed), fill = Country.Region)) +
#  geom_col() + scale_fill_manual(values = cols) + theme_minimal() +
#  ylab("log-Confirmed") + xlab("") + labs(color = "Country/Region") + coord_flip() +
#  theme(text = element_text(size = 12), axis.text.x = element_text(angle = 90),
#        legend.position = "none")

### Generate a line graph to visualize the confirmed cases since Jan 21 (except mainland China)
#ggplot(major.summary.data[major.summary.data$Country.Region != "China", ],
#       aes(x = Date, y = Confirmed, col = Country.Region)) + geom_line(lwd = 1) + geom_point(size = 2) +
#  scale_color_manual(values = cols) + theme_minimal() + ylab("Confirmed") +
#  xlab("") + labs(color = "Country/Region") + scale_x_date(date_labels = "%m-%d",
#                                                           date_breaks = "1 day") + theme(text = element_text(size = 14, face = "bold"),
#                                                                                          axis.text.x = element_text(angle = 90), legend.position = "right")
########################################################################################################################################################################

# Question 1: Generate a graph to visualize infected per million (instead of total infected number).
# install.packages("wbstats")
library(wbstats)

# Adding in population data ################################
pop_data <- wb(indicator = "SP.POP.TOTL", mrv = 1) # mrv determines the most recent data
head(pop_data)
names(pop_data)
pop_data$iso3c <- NULL
pop_data$indicatorID <- NULL
pop_data$indicator <- NULL
pop_data$date <- NULL
pop_data$iso2c <- NULL
names(pop_data) <- c("Population", "Country")
unique(pop_data$Country)
# Make sure names of countries match up with our other tables: 
pop_data[pop_data$Country=="Iran, Islamic Rep.",2] <- "Iran"  # change name to Iran
pop_data[pop_data$Country=="Iran",]
pop_data[pop_data$Country=="United States",2] <- "US"
pop_data[pop_data$Country=="US",]
pop_data[pop_data$Country=="Korea, Rep.",2] <- "South Korea"
pop_data[pop_data$Country=="South Korea",]
pop_data[pop_data$Country=="Russian Federation",2] <- "Russia"
pop_data[pop_data$Country=="Russia",]
unique(pop_data$Country)
################################
unique(yesterday.data$Country)
yesterday.data[yesterday.data$Country=="Korea, South",1] <- "South Korea"
yesterday.data[yesterday.data$Country=="South Korea",]
names(yesterday.data)
names(yesterday.data)[names(yesterday.data)=="Country.Region"] <- "Country"
yesterday.data.pop <- dplyr::left_join(yesterday.data, pop_data, by = "Country")
head(yesterday.data.pop)

infect.per.million <- yesterday.data.pop$Confirmed / yesterday.data.pop$Population * 1000000 
yesterday.data.pop$Infected <-infect.per.million    #this adds the infected.per.million to our table
print(yesterday.data.pop)


yesterday.data.major  <- yesterday.data.pop[sort.index[1:20],]
yesterday.data.major
yesterday.data.major  <- data.frame(Country=yesterday.data.pop$Country[sort.index[1:20]])
yesterday.data.major
yesterday.data.major$Country <- as.character(yesterday.data.major$Country)
major.summary.data <- dplyr::inner_join(yesterday.data.pop,yesterday.data.major,by = "Country")
dim(major.summary.data)
head(major.summary.data)
length(unique(major.summary.data$Country))
rownames(cols) <- unique(major.summary.data$Country)
major.summary.data$Country <- factor(major.summary.data$Country,
                                     levels = rev(yesterday.data.major$Country))

# Bar plot - infected per million (NOT the question 1 graph!)
# ggplot(major.summary.data[major.summary.data$Date==lastday,], aes(x = Country, y = Infected, fill = Country)) +
#  geom_col() + scale_fill_manual(values = cols) + theme_minimal() +
#  ylab("Infected") + xlab("") + labs(color = "Country") + coord_flip() +
#  theme(text = element_text(size = 12), axis.text.x = element_text(angle = 90),
#        legend.position = "none")


#######################################
unique(world.summary.data$Country)
world.summary.data[world.summary.data$Country=="Korea, South",1] <- "South Korea"
world.summary.data[world.summary.data$Country=="South Korea",]
unique(pop_data$Country)
names(world.summary.data)
names(world.summary.data)[names(world.summary.data)=="Country.Region"] <- "Country"

world.summary.data.pop <- dplyr::left_join(world.summary.data, pop_data, by = c("Country"))

head(world.summary.data.pop)

# add in Infected column
infect.per.mill.world <- world.summary.data.pop$Confirmed / world.summary.data.pop$Population * 1000000
world.summary.data.pop$Infected <- infect.per.mill.world
head(world.summary.data.pop)
names(world.summary.data.pop)

########################
# (Finally) Generating the plot for question 1 
world.summary.data.pop <- world.summary.data.pop[world.summary.data.pop$Date<=lastday,]
yesterday.data.pop <- world.summary.data.pop[world.summary.data.pop$Date==lastday,]
print(yesterday.data.major)
###
dim(yesterday.data.pop)
head(yesterday.data.pop)
sort.index <- sort(yesterday.data.pop$Confirmed,decreasing=TRUE,index.return=TRUE)$ix
yesterday.data.major  <- yesterday.data.pop[sort.index[1:20],]
yesterday.data.major$Country  <- as.character(yesterday.data.major$Country )
# Combine tables - world summary data pop and yesterday data major --> only focusing on top 20 countries based on most recent data
major.summary.data <- dplyr::inner_join(world.summary.data.pop,yesterday.data.major,by = "Country")
unique(major.summary.data$Country)
names(major.summary.data)
# idk why there are so many duplicate columns - i think it is how i merged it so i am just removing the duplicates here!
major.summary.data$Date.y <- NULL
major.summary.data$Recovered.y<- NULL
major.summary.data$Confirmed.y <- NULL
major.summary.data$Death.y <- NULL
major.summary.data$Population.y <- NULL
major.summary.data$"Infected.y" <- NULL
names(major.summary.data) <- c("Country", "Date", "Confirmed", "Death", "Recovered", "Population", "Infected")
head(major.summary.data)
length(unique(major.summary.data$Country))
rownames(cols) <- unique(major.summary.data$Country)
major.summary.data$Country <- factor(major.summary.data$Country,levels = rev(yesterday.data.major$Country))

# (FINALLY) the line graph itself!                                                                            
ggplot(major.summary.data[major.summary.data$Country != "China", ],
       aes(x = Date, y = Infected, col = Country)) + geom_line(lwd = 1) + geom_point(size = 2) +
  scale_color_manual(values = cols) + theme_minimal() + ylab("Infected per Million") +
  xlab("") + labs(color = "Country") + scale_x_date(date_labels = "%m-%d",
                                                    date_breaks = "1 day") + theme(text = element_text(size = 14, face = "bold"),
                                                                                   axis.text.x = element_text(angle = 90), legend.position = "right")
####################################################################
#Question 3.2
#code to add the ratio as a column
names(major.summary.data)
ratio_death_recovered <- major.summary.data$Death/major.summary.data$Recovered
print(ratio_death_recovered)   
major.summary.data$Ratio <-ratio_death_recovered
major.summary.data$Ratio[which(is.na(major.summary.data$Ratio))] = 0.0
major.summary.data$Ratio[which(major.summary.data$Ratio == Inf)] = major.summary.data$Death[which(major.summary.data$Ratio == Inf)]
head(major.summary.data,10)

### ggplot for the ratio
ggplot(major.summary.data[major.summary.data$Country != "Canada" & major.summary.data$Country != "Netherlands", ],
       aes(x = Date, y =Ratio, col = Country)) + geom_line(lwd = 1) + geom_point(size = 2) +
  scale_color_manual(values = cols) + theme_minimal() + ylab("Ratio Between Death and Recovery") +
  xlab("") + labs(color = "Country") + scale_x_date(date_labels = "%m-%d",
                                                    date_breaks = "1 day") + theme(text = element_text(size = 14, face = "bold"),
                                                                                   axis.text.x = element_text(angle = 90), legend.position = "right")
####################################################################
#Question 3.4
china.data <- major.summary.data[major.summary.data$Country == "China", ]
china.diffs <- diff(china.data$Infected)
china.dates <- china.data$Date
china.dates <- china.dates[-1]
china.df <- data.frame("Date" = china.dates, "Differences"=china.diffs)
china.rate <- ggplot(china.df,aes(x = Date, y = Differences)) + geom_line(lwd = 1, color="red") + geom_point(size = 2,color="red") + ggtitle("China: Infection Rate") +
  scale_color_manual(values = cols) + theme_minimal() + ylab("Rate of Infected per Million")
china.rate
#adding verticle lines
print(china.df$Date[])
china.rate +  geom_vline(xintercept=as.numeric(china.df$Date[53]), linetype=4, color = "darkgreen") + geom_vline(xintercept=as.numeric(china.df$Date[64]), linetype=4,  color = "darkgreen")

# March 15, March 26



us.data <- major.summary.data[major.summary.data$Country == "US", ]
us.diffs <- diff(us.data$Infected)
us.dates <- us.data$Date
us.dates <- us.dates[-1]
us.df <- data.frame("Date" = us.dates, "Differences"=us.diffs)
us.rate <- ggplot(us.df,aes(x = Date, y = Differences)) + geom_line(lwd = 1, color="blue") + geom_point(size = 2,color="blue") + ggtitle("US: Infection Rate") +
  scale_color_manual(values = cols) + theme_minimal() + ylab("Rate of Infected per Million")
us.rate
#adding verticle lines
print(us.df$Date[])
##adding vertical lines
us.rate +  geom_vline(xintercept=as.numeric(us.df$Date[49]), linetype=4, color = "darkgreen", show.legend = TRUE) + geom_vline(xintercept=as.numeric(us.df$Date[9]), linetype=4, color ="darkgreen",show.legend = TRUE) + 
  geom_vline(xintercept=as.numeric(us.df$Date[54]), linetype=4, color ="darkgreen",show.legend = TRUE) +  
  geom_vline(xintercept=as.numeric(us.df$Date[56]), linetype=4, color = "darkgreen", show.legend = TRUE) +  
  geom_vline(xintercept=as.numeric(us.df$Date[57]), linetype=4, color = "darkgreen", show.legend = TRUE) +  
  geom_vline(xintercept=as.numeric(us.df$Date[59]), linetype=4, color = "darkgreen", show.legend = TRUE) 

#legend
plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
legend("topleft", legend =c("International Travel Ban Initiated", "China Infection Rate", "US Infection Rate"), lty= 5, pt.cex=1, cex=1, bty='n',
       col = c('darkgreen', 'red', 'blue'))
mtext("Legend", at=0.2, cex=1)

