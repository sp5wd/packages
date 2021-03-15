
##Question 1:
m <-matrix(c(-1.0000, -1.0000, -1.0000, -1.0000, -1.0000, -0.9280, -0.2040, 0.7510, 0.4660, 0.2340, -0.8090, -1.0000, -1.0000, -1.0000, -1.0000, -1.0000, -1.0000, -1.0000, -1.0000, -1.0000, -0.3700, 0.7390, 1.0000, 1.0000, 1.0000, 1.0000, 0.6440, -0.8900, -1.0000, -1.0000, -1.0000, -1.0000, -1.0000, -1.0000, -1.0000, -1.0000, 0.6160, 1.0000, 0.6880, -0.4550, -0.7310, 0.6590, 1.0000, -0.2870, -1.0000, -1.0000, -1.0000, -1.0000, -1.0000, -1.0000, -1.0000, -1.0000, -0.3760, -0.1860, -0.8740, -1.0000, -1.0000, -0.0140, 1.0000, -0.2530, -1.0000, -1.0000, -1.0000, -1.0000, -1.0000, -1.0000, -1.0000, -1.0000, -1.0000, -1.0000, -1.0000, -1.0000, -0.9780, 0.5010, 1.0000, -0.5400, -1.0000, -1.0000, -1.0000, -1.0000, -1.0000, -1.0000, -1.0000, -1.0000, -1.0000, -0.9980, -0.3410, 0.2960, 0.3710, 1.0000, 0.4170, -0.9890, -1.0000, -1.0000, -1.0000, -1.0000, -1.0000, -1.0000, -1.0000, -1.0000, -1.0000, -0.0080, 1.0000, 1.0000,1.0000, 1.0000, 0.7610, -0.7310, -1.0000, -1.0000, -1.0000, -1.0000, -1.0000, -1.0000, -1.0000, -1.0000, -1.0000, 0.2420, 1.0000, 1.0000, 0.3190, 0.2590, 1.0000, 0.7420, -0.7570, -1.0000, -1.0000, -1.0000, -1.0000, -1.0000, -1.0000, -1.0000, -1.0000, -0.9750, -0.4670, -0.9890, -1.0000, -1.0000, -0.1710, 0.9980, 0.6690, -0.9450, -1.0000, -1.0000, -1.0000, -1.0000, -1.0000, -1.0000, -1.0000, -1.0000, -1.0000, -1.0000, -1.0000, -1.0000, -1.0000, 0.2280, 1.0000, 0.0380, -1.0000, -1.0000, -1.0000, -1.0000, -1.0000, -1.0000, -1.0000, -1.0000, -1.0000, -1.0000, -1.0000, -1.0000, -1.0000, -0.8260, 0.9180, 0.9330, -0.7940, -1.0000, -1.0000, -1.0000, -0.6660, 0.3370, 0.2240, -0.9080, -1.0000, -1.0000, -1.0000, -1.0000, -1.0000, -1.0000, 0.4180, 1.0000, -0.2580, -1.0000, -1.0000, -0.2460, 1.0000, 1.0000, 0.3550, -0.9580, -1.0000, -1.0000, -1.0000, -1.0000, -1.0000, -1.0000, -0.0770, 1.0000, 0.3440, -1.0000, -1.0000, 0.0750, 1.0000, 1.0000, 0.6490, 0.2560, -0.2000, -0.3510, -0.7330, -0.7330, -0.7330, -0.4330, 0.6490, 1.0000, 0.0930, -1.0000, -1.0000, -0.9590, -0.0620, 0.8210, 1.0000, 1.0000, 1.0000, 1.0000, 1.0000, 1.0000, 1.0000, 1.0000, 1.0000, 0.5830, -0.8430, -1.0000, -1.0000, -1.0000, -1.0000, -0.8770, -0.3260, 0.1740, 0.4660, 0.6390, 1.0000, 1.0000, 0.7910, 0.4390, -0.1990, -0.8830, -1.0000, -1.0000), nrow =16, byrow=T)
m #printing the matrix
rotate <- function(x) t(apply(x, 2, rev)) # this is used to rotate matrix
image(rotate(m)) #create image of the rotated matrix
#the number depcited is '3'

##Question 2:

mixture <- read.csv("mixture.csv", header = T) #read in the data

plot(mixture$x1, mixture$x2, xlab = "Predictor One", ylab = "Predictor Two", main = "Class Label from Two Predictors") #create a scatterplot of x1 vs x2

one <- which(mixture$y == "1") #vector of the points where y = 1
zero <- which(mixture$y == "0") #points where y = 0

polygon(x = c(-3, -3, 5, 5), y = c(0.41202, 3.2, 3.2, 1.17882), col = "mistyrose")
polygon(x = c(-3, -3, 5, 5), y = c(0.41202, -2.5, -2.5, 1.17882), col = "darkolivegreen1")

points(mixture$x1[one], mixture$x2[one], col = "red", pch = 16) #color y = 1 red
points(mixture$x1[zero], mixture$x2[zero], col = "blue", pch = 16) #color y = 0 blue

legend("bottomleft", y = 0, legend = c("Class = 1", "Class = 0"), col = c("red", "blue"), pch = 16, cex = 0.75)


#solving for the intercept and slope
# 0 = -0.978 - 0.134x1 + 1.398x2
# 0.978 + 0.134x1 = 1.398x2
# 0.69957 + 0.09585x1 = x2

abline(a = 0.69957, b = 0.09585) #plot the classification line


##Question 3:
library(YaleToolkit)
install.packages("YaleToolkit")

teachershired <- read.csv("TeacherHires.csv")

dim(teachershired)
summary(teachershired)
whatis(teachershired)

head(teachershired)

wi <- whatis(teachershired)
class(wi)  #creates dataframe
wi$missing

#There are some columns which are empty.
which(whatis(teachershired)$missing==nrow(teachershired))  #extracts the columns that are missing
x <- teachershired[, -which(whatis(teachershired)$missing==nrow(teachershired))] # x is set to the columns that are missing

dim(x)
whatis(x) 
names(x)
#It is also helpful to improve variable names; try to use short-yet-descriptive words, without awkward capitalization. 
names(x) <- c("interviewed", "hired", "appdate",
              "age", "sex", "residence", "GPA.u",
              "GPA.g", "MA", "substitute",
              "teaching", "experience", "workkids",
              "volunteer")
whatis(x, type.truncate = 4) #use the type.truncate option to narrow the whatis display.

#cleaning up interviewed
x$interviewed <- as.character(x$interviewed)
unique(x$interviewed)
levels(x$interviewed)
x$interviewed[x$interviewed=="yes"] <- "yes"
x$interviewed[x$interviewed=="yes "] <- "yes"
x$interviewed[x$interviewed=="no"] <- "no"
x$interviewed <- factor(x$interviewed)
levels(x$interviewed)
table(x$interviewed)

#cleaning up hired
x$hired <- as.character(x$hired)
unique(x$hired)
levels(x$hired)
x$hired[x$hired=="yes"] <- "yes"
x$hired[x$hired=="yes "] <- "yes"
x$hired[x$hired=="yes*"] <- "yes"
x$hired[x$hired=="no"] <- "no"
x$hired <- factor(x$hired)
levels(x$hired)
table(x$hired)

whatis(x, type.truncate = 4)

# Clean up age: age was listed as a mixed factor
levels(x$age)
table(x$age)
sum(table(x$age))
sum(is.na(x$age))
### convert it to numeric
### but you cannot do this by directly using as.numeric
summary(as.numeric(x$age))

## this is because age is a factor now
as.numeric("N/A")
as.numeric("  ")
as.numeric("22")

summary(as.numeric(as.character(x$age)))
## this is what we want

##creating 2 age groups
x$age <- as.numeric(as.character(x$age))
x$agegroup <- factor(x$age<=39, levels=c(FALSE,TRUE),
                     labels=c("older", "younger"))
x[1:12, "agegroup"]
head(x[,c("age","agegroup")],12)

# Clean up sex:
x$sex <- as.character(x$sex)
unique(x$sex)
levels(x$sex)
x$sex[x$sex=="  "] <- "N/A"
x$sex[x$sex=="M"] <- "Male"
x$sex[x$sex=="F"] <- "Female"
x$sex <- factor(x$sex)
levels(x$sex)
table(x$sex)

#cleaning up MA
x$MA <- as.character(x$MA)
unique(x$MA)
levels(x$MA)
x$MA[x$MA=="no"] <- "no"
x$MA[x$MA=="yes"] <- "yes"
x$MA[x$MA=="yes "] <- "yes"
x$MA[x$MA=="pending"] <- "pending"
x$MA[x$MA=="N/A"] <- "N/A"
x$MA <- factor(x$MA)
levels(x$MA)
table(x$MA)

#cleaning up substitute
x$substitute <- as.character(x$substitute)
unique(x$substitute)
levels(x$substitute)
x$substitute[x$substitute=="no"] <- "no"
x$substitute[x$substitute=="no "] <- "no"
x$substitute[x$substitute=="yes"] <- "yes"
x$substitute[x$substitute==""] <- "N/A"
x$substitute <- factor(x$substitute)
levels(x$substitute)
table(x$substitute)

#cleanup teaching
x$teaching <- as.character(x$teaching)
unique(x$teaching)
levels(x$teaching)
x$teaching[x$teaching=="  "] <- "N/A"
x$teaching[x$teaching==""] <- "N/A"
x$teaching[x$teaching=="N/A"]<-"N/A"
x$teaching[x$teaching=="yes"] <- "yes"
x$teaching[x$teaching=="yes "] <- "yes"
x$teaching[x$teaching=="no"] <- "no"
x$teaching <- factor(x$teaching)
levels(x$teaching)
table(x$teaching)

x$experience = as.numeric(as.character(x$experience)) #need to convert months to year first

#cleanup working with kids
x$workkids <- as.character(x$workkids)
unique(x$workkids)
levels(x$workkids)
x$workkids[x$workkids==""] <- "N/A"
x$workkids[x$workkids=="no"] <- "no"
x$workkids[x$workkids=="no "]<-"no"
x$workkids[x$workkids=="yes"] <- "yes"
x$workkids <- factor(x$workkids)
levels(x$workkids)
table(x$workkids)

whatis(x, type.truncate = 4) #checking new table

#check voluneteer
x$volunteer <- as.character(x$volunteer)
unique(x$volunteer)
levels(x$volunteer)
table(x$volunteer)


#barplot for MA and hiring
hiring <- table(x$MA, x$hired)
barplot(hiring, col=c("white","indianred", "darkorange", "khaki1"), main = "Status of having an MA and getting Hired", 
        ylab="Count", xlab = "Hired", legend = rownames(hiring))

# barplot for age and hiring
hire_age <- table(x$hired, x$agegroup)
barplot(hire_age, col=c("indianred", "khaki1"), main = "Does Age Group Impact Hiring?", 
        ylab="Count", xlab="Hired", legend = rownames(hire_age))
summary(hire_age)


# barplot for age and interview
interviewed_age <- table(x$interviewed, x$agegroup)
barplot(interviewed_age, col=c("indianred", "khaki1"), main = "Age group and Receiving an Interview", 
        ylab="Count", xlab="interviewed", legend = rownames(interviewed_age))

# barplot for age and MA
MA_age <- table(x$MA, x$agegroup)
barplot(MA_age, col=c("ivory2","indianred","ivory4", "khaki1"), main = "Age group and MA Status", 
        ylab="Count", xlab="interviewed", legend = rownames(MA_age))

summary(x$hired)
summary(x)
summary(x$agegroup)
install.packages("plyr")
count(hire_age)

##Question 4
uva_unc <- read.delim("uvaunc.txt")
summary(uva_unc)

#cleaning data
uva_unc <- read.table("uvaunc.txt",header = F, sep ="|")
head(uva_unc)
uva_unc <- uva_unc[,-1]
head(uva_unc)
names(uva_unc) <- c("Year","Location","UNC","UNC score","UVA","UVA score",
                   "Contested","UNC rank","UVA rank")
head(uva_unc)
uva_unc <- uva_unc[,-c(3,5)]
head(uva_unc)
uva_unc[which(uva_unc$`UNC score` == uva_unc$`UVA score`),]
Winner <- rep("",length(uva_unc$Year))
for(i in 1:length(uva_unc$Year)){
  if(uva_unc[i,]$`UNC score` == uva_unc[i,]$`UVA score`){
    Winner[i] <- "Tie"
  }
  ifelse(uva_unc[i,]$`UNC score` < uva_unc[i,]$`UVA score`,Winner[i]<-"UVA",Winner[i]<-"UNC")
}
Winner[which(uva_unc$UVA.score == uva_unc$UNC.score)] <- "Tie"
uva_unc$`Winner` <- Winner
names(uva_unc) <- c("Year","Location","UNC.score","UVA.score",
                   "Contested","UNC.rank","UVA.rank","Winner")
write.table(uva_unc,file = "uvaunc_clean.txt")

print(uva_unc)

#Location Bar Plot
homeadvan <- table(uva_unc$Winner, uva_unc$Location)
barplot(homeadvan, col=c("grey","powderblue", "darkorange"), main = "Does location determine who wins?", 
        ylab="Number of Games Won", legend = rownames(homeadvan))

#Pie Chart
uva_wins <- sum(uva_unc$Winner == "UVA")
unc_wins <- sum(uva_unc$Winner == "UNC")
ties <- sum(uva_unc$Tie == "Tie")

all_wins <-c(uva_wins,unc_wins,ties)
lbls <- c("UVA","UNC","Tie")
pie(all_wins,labels =lbls, main = "Total Wins", col=c("darkorange","powderblue","grey"))

#scatter plot of scores over time
plot(uva_unc$Year, uva_unc$UNC.score, main = "Each Team's Scores Over The Years", 
     xlab = "Year", ylab= "Scores", col=("powderblue"))
points(uva_unc$Year,uva_unc$UVA.score, col="darkorange")
legend("topleft", legend=c("UVA", "UNC"),
       col=c("darkorange", "powderblue"), lty=1:2, cex=.90)
abline(lm(uva_unc$UNC.score~uva_unc$Year), col= "powderblue")
abline(lm(uva_unc$UVA.score~uva_unc$Year), col= "darkorange")

#scatter plot of rank over time
plot(uva_unc$Year, uva_unc$UNC.rank, main = "Each Team's Rank Over The Years", 
     xlab = "Year", ylab= "Rank", col=("powderblue"))
points(uva_unc$Year,uva_unc$UVA.rank, col="darkorange")
legend("topleft", legend=c("UVA", "UNC"),
       col=c("darkorange", "powderblue"),lty=1:1, cex=.75)

