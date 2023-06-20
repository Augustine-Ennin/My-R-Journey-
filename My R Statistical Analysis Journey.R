
#Advance Data Analysis
x <- c(1,2,3,4,5,6,7,8)
mean(x)
sd(x)
var(x)
length(x)

# Probability Function
rnorm(50, mean=50, sd=50) #generate 50 random variable of normal distribution with mean=50 and sd=50
qnorm(0.9, mean=500, sd=100) #90th percentile of norm dist with mean=500 & sd=100 

#Plot a standard normal curve on the interval [-3,3]
x <- pretty(c(-3,3), 30)
y <- dnorm(x)
plot(x, y,
     type="1",
     xlab="Normal Deviate",
     ylab="Density",
     yaxs= "i"
)
#Setting a seed to generate the same set of random numbers
set.seed(1234)
runif(2)
set.seed(1234)
runif(2)
set.seed(1)
runif(5)
set.seed(1)
runif(5)

#Generating Multivariate Normal data with n observations
library (MASS)
options(digits=3)
set.seed(1234) #To get a recurrence random data
mean <- c(230.7,146.7,3.6) #Mean vector
sigma <- matrix(c(15360.8, 6721.2, -47.1, 6721.20, 4700.9, -16.5, -47.1, -16.50, 0.3), nrow=3, ncol=3) #Covariance matrix
mydata <-mvrnorm(500, mean, sigma) #Generating mvrn for 500 observations
mydata <- as.data.frame(mydata) #Converting the matrix to data frame
names(mydata) <- c("y","x1","x2") #Names for the variables
dim(mydata)
head(mydata, n=10) #Display only 10 of our 500 observation

#Applying functions to data objects
a <- 5
sqrt(a) #Square root of a
b <- c(1.243, 5.654, 2.99)
round(b) #Round the vectors to the nearest whole no.
c <- matrix(runif(12), nrow=3, ncol=4) #Generate a 12 random 3 by 4 matrix of a uniform distribution
c
log(c) #log of base 10 of c
mean(c)
mydata <- matrix(rnorm(30), nrow=6)
mydata
apply(mydata, 1, mean) #Calculate the row means
apply(mydata, 2, mean) #Calculate the column means
apply(mydata, 2, mean, trim=0.2) #Calculate the column means with the bottom 20% and top 20%  of the values discarded

#Exercise 1
#A data of students and their respective scores in Math, Science, and English. Aggregate the scores and to determine a single performance indicator for each student. Assign A to the top 20%, a B to the next 20%. Also, sort the students name alphabetically.

options(digits=2)
Students <- c("John Davis", "Angela Williams", "Bullwinkle Moose", "David Jones", "Janice Markhammer", "Cheryl Cushing", "Reuven Ytzrhah", "Greg Knox", "Joel England", "Mary Rayburn")
Math <- c(502, 600, 412, 358, 495, 512, 410, 625, 573, 522)
Science <- c(95, 99, 80, 82, 75, 85, 80, 95, 89, 86)
English <- c(22, 22, 18, 15, 20, 28, 15, 30, 27, 18)
roster <- data.frame(Students, Math, Science, English, stringsAsFactors = FALSE)

z <- scale(roster[,2:4]) #standardize the exams scores 
score <- apply(z, 1, mean) #find the mean of the standardized score for each student
roster <- cbind(roster, score) #combine the standardized score mean of each student to the roster

y <- quantile(score, c(0.8, 0.6, 0.4, 0.2))

roster$grade[roster$score>=y[1]] <- "A" #assigning grades to the scores
roster$grade[roster$score<y[1]&roster$score>=y[2]] <- "B"
roster$grade[roster$score<y[2]&roster$score>=y[3]] <-"C"
roster$grade[roster$score<y[3]&roster$score>=y[4]] <-"D"
roster$grade[roster$score<y[4]]<- "F"

name <- strsplit(roster$Students, " ")

Lastname <- sapply(name, "[", 2) #split the students name to first name and last name
Firstname <- sapply(name, "[", 1)
roster <- cbind(Firstname,  Lastname, roster[,-1])

roster <- roster[order(Lastname, Firstname),] #order the students by last name then by first name 
roster

#aggregating mtcars data
options(digits=4)
attach(mtcars)
aggdata <- aggregate(mtcars, by=list(cyl, gear), FUN=mean, na.rm=TRUE)
aggdata

#VISUALIZATION
#Barplot
library(vcd)
counts <- table(Arthritis$Improved) #arthritis dataset from the vcd package
barplot(counts, main="Basic Bar Plot", xlab="Improvement", ylab="Frequency") #simple bar plot
barplot(counts, main="Horizontal Bar Plot", xlab="Frequency", ylab="Improvement", horiz=TRUE) #horizontal bar chart

counts <- table(Arthritis$Improved, Arthritis$Treatment)
barplot(counts, main="Stacked Bar Plot", xlab="Treatment", ylab="Frequency", col=c("purple", "blue", "gold"), legend=rownames(counts)) #stacked bar plot
barplot(counts, main="Group Bar Plot", xlab="Treatment", ylab="Frequency", col=c("purple", "blue", "gold"), legend=rownames(counts), beside=TRUE) #grouped bar plot

states <- data.frame(state.region, state.x77)
means <- aggregate(states$Illiteracy, by=list(state.region), mean)
means <- means[order(means$x),]
means
barplot(means$x, names.arg=means$Group.1, cex.names = 0.9, main= "Mean Illiteracy Rate", col=c("black")) #barplot of mean illiteracy rate

#Pie Chart
library(plotrix)
par(mfrow=c(2,2)) #combining 4 graphs into one
slices <- c(10, 12, 4, 16, 8)
lbls <- c("US", "UK", "AUSTRALIA", "GERMANY", "FRANCE")
pie(slices, labels=lbls, main="A simple Pie Chart") #simple pie chart

ptg <- round((slices)/sum(slices)*100) #slices as percentages
lbls2 <- paste(lbls, " ", ptg, "%", sep="")
pie(slices, labels=lbls2, col=rainbow(5), main="Pie Chart With Percentages") #slices as percentages
pie3D(slices, labels=lbls, explode=0.1, main="3D Pie Chart")

fan.plot(slices, labels=lbls, main="Fan Plot") #fan plot

#Histogram
hist(mtcars$mpg, freq=FALSE, breaks=12, col="grey", xlab="Miles/Gallon", main="Histogram, Rug Plot, and Density Curve") #histogram with rug plot and density curve 
rug(jitter(mtcars$mpg), side=1, col="black")
lines(density(mtcars$mpg), col="black", lwd=2)

#Kernel Density Plot
kd <- density(mtcars$mpg)
plot(kd, lwd=2, main="Kernel Density of mpg")
polygon(kd, col="grey")
rug(mtcars$mpg)

#Comparative Kernel Density Plot for mtcars$cyl and mtcars$mpg
library(sm)
attach(mtcars)
cyl.f <- factor(cyl, levels = c(4,6,8), labels = c("4 Cylinders", "6 Cylinders", "8 Cylinders"))
sm.density.compare(mpg, cyl, xlab = "Miles/Gallon", lwd=2)
title("Miles Per Gallons Distribution by Car Cylinders")
legend("topright", levels(cyl.f), fill = 2:(1+length(levels(cyl.f))), bty = "n")

#Box Plot
boxplot(mtcars$mpg, main="Box Plot", ylab="Miles per Gallon") # A simple box plot of mpg of the mtcars data
boxplot.stats(mtcars$mpg) #statistics of our boxplot

boxplot(mpg~cyl, data = mtcars, main="Car Mileage", xlab = "Number of Cylinders", ylab = "Miles Per Gallon", col="red") #different cylinders and their miles/gallon using boxplot

#BASIC STATISTICS
options(digits = 4)
library(e1071)
myvars <- c("mpg", "hp", "wt")
mystats <- function(x){
  complete_x <- x[complete.cases(x)]
  n <- length(x)
  kurt <-kurtosis(x)
  skew <- skewness(x)
  m <- mean(x)
  s <- sd(x)
  return(c(n=n, mean=m, stdv=s, skewness=skew, kurtosis=kurt))
} #A function to calculate the length, mean, sd, skewness, kurtosis, of some selected variables in the mtcars data 
sapply(mtcars[myvars], mystats)

library(psych)
describe(mtcars[myvars]) #using describe function in the psych package to calculate various statistical measure of myvars 

library(psych)
by(mtcars[myvars], mtcars$am, describe) #aggregate statistics of myvars and transmission type variables in the mtcars data

#Test of independence
library(vcd)
attach(Arthritis)
mytable1 <- xtabs(~ Treatment + Improved)
chisq.test(mytable1) #test of independence between treatment and improved using chi-square test. We reject the null hypothesis that there is independence between the variables because p<0.01

mytable2 <- table(Improved, Sex)
chisq.test(mytable2) #test of independence between improved and sex using chi-square test. We  refuse to reject the null hypothesis that there is independence between the variables because p>0.05
