#---------------------------------------------------
# Intro to R
# Prepared by Patricia Kirkland
# Last updated: 7 June 2017
#---------------------------------------------------

#---------------------------------------------------
#  WRITING & EXECUTING CODE IN R
#---------------------------------------------------

#---------------------------------------------------
# PERFORMING BASIC TASKS
#---------------------------------------------------


##### Setting up your work space

ls()
rm(list=ls(all=TRUE))

getwd()
setwd("/Users/patriciakirkland/Dropbox/Empiprical Reasoning Center/R Workshop")

getwd()		# check again

##### Installing and loading packages

# install packages

# install.packages("dplyr", dependencies=TRUE)
# install.packages("ggplot2", dependencies=TRUE)
# install.packages("foreign", dependencies=TRUE)
# install.packages("xtable", dependencies = TRUE)
# install.packages("stargazer", dependencies = TRUE)
# install.packages("arm", dependencies = TRUE)
# install.packages("modeest", dependencies=T)

# load packages

library(foreign)
library(xtable)
library(arm)
library(ggplot2)
library(dplyr)
library(stargazer)
library(modeest) 

# some useful packages:
# foreign --  load data formatted for other software
# xtable -- export code to produce tables in LaTeX
# arm -- applied regression and multi-level modeling
# ggplot2 -- make plots and figures
# dplyr -- user friendly data cleaning & manipulation
# more packages: http://cran.r-project.org/web/packages/

rm(list=ls(all=TRUE))		# clear all objects in memory 


## Comments

n <- 100
n

# n 

# n <- 1
n


## HELP

# funtion search
?table

# topic search
??variance

#---------------------------------------------------
#  PROGRAMMING BASICS
#---------------------------------------------------

## Basic calculations

4
"yes"
2+3
1039/49
46^700
(3.5+2.7)/(900*2)


## Assignment operator

x <- 3
x

y <- "this is a string"
y

z <- 2
z

x+z


###  Basic math operators & functions

# Arithmetic/Math/Numeric Operators
# 	+	addition
#	-	subtraction
#	*	multiplication
#	/	division
#	^ or **	exponentiation

z <- 1
y <- 0
x <- 5
w <- 100

a <- z + y
a
b <- x^2
b
c <- x * w
c
d <- sqrt(w) + x
d
e <- log(w)
e

###  Logical Operators 

#	<	less than
#	<=	less than or equal to
#	>	greater than
#	>=	greater than or equal to
#	==	exactly equal to
#	!=	not equal to
#	!x	Not x
#	x | y	x OR y
#	x & y	x AND y
#	isTRUE(x)	test if X is TRUE

x==5	# this is a logical operator
x

x <- TRUE	# assign logical values to variables
x
x+z		# explain this output ## numeric value of TRUE = 1, so 1 + 1

x <- FALSE
x==0

#---------------------------------------------------
# Data objects in R
#---------------------------------------------------

## Vectors

# The function c() allows you to concatenate
# a bunch of items into a vector

x <- c(1,2,3,4)
x
x[2]  ## the square brackets are used for indexing-- here, 2nd element of x

y <- c(5,6,7,8,9)
y
y[5]  ## the square brackets are used for indexing-- here, 5th element of y

# You can append one vector to another

z <- c(x,y)
z

# Another way to produce a vector containing
# a sequence of integers

 
q <- 1:5
q

a <- seq(from=2, to=100, by=2)
a

# You can repeat vectors multiple times

ab <- rep(1:5, times=10)
ab
ab <- rep(1:5, 10) ## you do not need the "times" with rep; also, notice that R lets you overwrite 

cd <- rep(c(1,3,7,9), times=2)
cd


## Data Frames

# bind vectors as columns in a data frame
data <- data.frame(cbind(a, ab))

# bind vectors as rows in a data frame
data <- data.frame(rbind(a, ab))



# -----------------------------------------------
# Working with Data
# -----------------------------------------------

##### Read in data

# csv file
data <- read.csv("teachingratingsexcel.csv", header=TRUE)
data


# .dta file (Stata)
# dtafile <- read.dta("fakedata.dta")
# dtafile

# .RData file
# load("fakedata1.RData")
# data


##### Looking at data: basic info

## look at variable names and dimensions

names(data)

dim(data)
dim(data)[1]  ## square brackets are used for indexing-- here, 1st element of dimensions
dim(data)[2]  ## square brackets are used for indexing-- here, 2nd element of dimensions


# refer to specific rows or columns in a data frame by the
# row or column number(s) -- this allows you to "look at"
# a subset of your data; you could even assign it to a new object
# and you would have effectively subset your data.

data[1,]    	# row 1 only
data[1:3,]	# rows 1 to 3 only

data[,1]		# column 1 only
data[,2:4]	# columns 2 to 4 only


## print to console

data
data[1:5,]
data[,3]

head(data)

data$course_eval
data$female
data$beauty

course_eval    	# error! why?

class(data)
class(data$course_eval)
class(data$female)

#####  Writing data to disk

write.csv(data, "evaluation_data.csv", row.names=FALSE)

write.dta(data, "evaluation_data.dta")

save(data, file="evaluation_data.RData")  # save just a data frame
save.image(file="course_evaluations.RData")  # save your current workspace

# -----------------------------------------------
# Basic Data Analysis
# -----------------------------------------------

## Tables 

# table() function
table(data$female, useNA="always")
crosstab <- table(data$female, data$minority, useNA="always", dnn=c("Gender", "Race or Ethnicity"))
crosstab

crosstab <- crosstab[c(2, 1, 3), c(2, 1, 3)]
crosstab

row.names(crosstab) <- c("Female", "Male", "NA")
colnames(crosstab) <- c("Minority", "White", "NA")
crosstab

mytable <- table(data$female, data$minority, useNA="always", dnn=c("Female", "Minority"))

margin.table(mytable, 1)
margin.table(mytable, 2)

prop.table(mytable)
prop.table(mytable, 1)
prop.table(mytable, 2)

# freq() function from the descr package-- also produces a barplot (add plot = FALSE to omit)

## load the descr package
library(descr)

freq(data$female)

freq(data$minority, plot = FALSE) 

# crosstab() function (in descr pkg)-- also produces a mosaic plot (add plot = FALSE to omit)
crosstab(data$minority, data$female)

crosstab(data$nnenglish, data$minority, prop.c = TRUE, plot = FALSE)


##### Looking at data: basic histograms and scatterplots

# hist()
hist(data$course_eval, breaks=50, main="Histogram of Outcome Variable - Course Evaluation", xlab="Outcome Variable Y")

# plot()
plot(data$beauty, data$course_eval, main="Scatterplot of Beauty and Course Evaluations", pch=16)
abline(v=0, col="red")
abline(h=3.5, col="grey80", lty=2, lwd=3)

# save to disk
pdf("basic_plot.pdf")
plot(data$beauty, data$course_eval, main="Scatterplot of Beauty and Course Evaluations", pch=2)
abline(v=0, col="red")
abline(h=3.5, col="grey80", lty=2, lwd=3)
dev.off()



#### A few basic analyses & statistical tests 

## note that missing values will create problems with these tests, so be sure to exclude them
## generally, for functions that evaluate 1 variable (e.g., mean, variance), use the argument na.rm = TRUE
## functions that evaluate 2 variables (e.g., correlation, covariance), take the argument use = complete.obs


# summary statistics
summary(data)
summary(data$course_eval)
summary(data$female)

quantile(data$course_eval)

min(data$course_eval)

max(data$course_eval)

## mean
mean(data$course_eval, na.rm=TRUE)
mean(data$course_eval[data$female == 0])
mean(data$course_eval[data$female == 1])


###
### Find the mean of the beauty variable for male and female instructors
###

## median
median(data$course_eval, na.rm=TRUE)
median(data$course_eval[data$female == 0])
median(data$course_eval[data$female == 1])

## mode
mfv(data$course_eval) # basic evaluation - most frequent value
mlv(data$course_eval, method="mfv") # can specify method from a variety of mode estimators

## standard deviation
sd(data$course_eval, na.rm=TRUE)
sd(data$course_eval[data$female == 0])
sd(data$course_eval[data$female == 1])

## by() function returns descriptive statistics by group or category
by(data$course_eval, data$female, mean, na.rm=TRUE)
by(data$course_eval, data$female, median, na.rm=TRUE)
by(data$course_eval, data$female, sd, na.rm=TRUE)

by(data$course_eval, data$female, summary)


## correlation 
cor(data$course_eval, data$beauty, use="complete.obs")
cor(data$course_eval, data$beauty, use="pairwise.complete.obs")
cor(data$course_eval, data$beauty, use="complete.obs", method="pearson") ## Pearson correlation is the default
cor(data$course_eval, data$beauty, use="complete.obs", method="kendall")
cor(data$course_eval, data$beauty, use="complete.obs", method="spearman")


cor(data$course_eval[data$female == 0], data$beauty[data$female == 0])
cor(data$course_eval[data$female == 1], data$beauty[data$female == 1])

## correlation with significance test 
cor.test(data$course_eval, data$beauty)
cor.test(data$course_eval, data$beauty, use="complete.obs", method="pearson") ## Pearson correlation is the default
cor.test(data$course_eval, data$beauty, use="complete.obs", method="kendall")
cor.test(data$course_eval, data$beauty, use="complete.obs", method="spearman")


###
### Is there a correlation between assessments of beauty and gender?
###


## t-test
t.test(data$course_eval ~ data$female)  # independent 2-group - first variable is numeric, second is binary factor
t.test(data$course_eval[data$female == 0], data$course_eval[data$female == 1])  # independent 2-group - both numeric variables


###
### Compare evaluations for white and non-white instructors.
###



## calculate the difference in means
mean(data$course_eval[data$female == 1]) - mean(data$course_eval[data$female == 0])

## extract the means from the t.test() result

names(t.test(data$course_eval ~ data$female))   # first, find the names of the output in t.test()

t.test(data$course_eval ~ data$female)$estimate[2] - t.test(data$course_eval ~ data$female)$estimate[1]

## chi-square test
female_minority_crosstab <- table(data$female, data$minority)
chisq.test(female_minority_crosstab)

## note that you do not need to save the table as an object
chisq.test(table(data$female, data$minority))

#### Regression models

fit_1 <- lm(course_eval ~ female, data=data)
summary(fit_1)

## include only a subset of the data

fit_1_male <- lm(course_eval ~ beauty, data=data, subset=female==0)
summary(fit_1_male)

fit_1_female <- lm(course_eval ~ beauty, data=data, subset=female==1)
summary(fit_1_female)


## include an interaction 

fit_2 <- lm(course_eval ~ female*beauty, data=data)
summary(fit_2)

fit_age <- lm(beauty ~ age*female, data=data)
summary(fit_age)

## include additional independent variables

fit_3 <- lm(course_eval ~ female + beauty + age, data=data)
summary(fit_3)

## add fixed effects

fit_4 <-  lm(course_eval ~ factor(intro) + female + beauty + age, data=data)
summary(fit_4)


###
### Specify your own model(s) based on your hypotheses (or speculation?) about the 
### factors that predict course evaluations.
###




## heteroskedasticity-robust standard errors

## packages
library(sandwich) 
library(lmtest)

summary(fit_3)
coeftest(fit_3, vcov=vcovHC(fit_3, type="HC0")) ## HC0 is the default
coeftest(fit_3, vcov=vcovHC(fit_3, type="HC1")) ## HC1 includes a degree of freedom correction (like , robust in Stata)


###
### Generate robust standard errors for the model you specified above.
###



    
## we'll address clustered standard errors in Session 3   


