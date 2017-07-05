# R Workshop
# Feb. 13, 2017
# Empirical Reasoning Center
# CORRUPTION PERCEPTIONS INDEX 2015
# http://www.transparency.org/cpi2015

# The following line clears all current data
rm(list=ls())

# Note: for help with a function type a ? or ?? before the command

# Setting a working directory

# Session>Set working directory>Choose Directory
# In the pop up window you want the find the folder that contains the data for this workshop

# To check what the current working directory is use the following line
getwd()

# Clicking through these menus is the same as running the following line
setwd("/Users/bencollins/Desktop/R_Workshop")

#Importing data using the file menu

#File>Import Dataset>From Excel...
#Click Browse to find the Excel file
#Change Skip from 0 to 1 to ignore the first row in Excel

# The following line will import the data
install.packages("readxl")
library(readxl)
CPI <- read_excel("2015_CPI_data.xlsx", skip=1)

#Saving an R file

save(CPI, file="01_CPI-data_Workshop.RData")

#Opening an R file

load("01_CPI-data_Workshop.RData")
#Only ".RData" datasets can be opened this way.  Note that this is different from importing an Excel, CSV, or any other dataset type.
 
#Viewing a dataset

#The following line displays the dataset like a spreadsheet
View(CPI)

#The following line displays the variable names
names(CPI)
#The following line displays the first 6 observations of all variables
head(CPI)

#Tabulating a variable

#A frequency table provides all of the different values that variable takes on and the corresponding number of observations
#The following lines will create a frequency table of the Country variable
table(CPI$Country)
 
#Renaming a variable

#The following lines change the name of the wbcode (Country abbreviation) variable to CT and tabulate CT
CPI$CT <- CPI$wbcode
table(CPI$CT)

#Exploring a variable 

#The following lines change the name of the World Bank CPIA variable to World.Bank and tabulate World.Bank
CPI$World.Bank <- CPI$`World Bank CPIA`
install.packages("psych")
library(psych)
describe(CPI$World.Bank)

#The following lines assign a table to an object and displays it
table1 <- table(CPI$Region, useNA="always")
table1

#The following line generates summary statistics
summary(CPI$CPI2015)
#The following line generates the 4th statistic from the summary
summary(CPI$CPI2015)[4]

#Recoding a variable

#The following line creates a categorical variable
CPI$CPI.cat <- cut(CPI$CPI2015,breaks=c(0,25,50,75,100))
table(CPI$CPI.cat)

#The following adds labels to the categorical variable
CPI$CPI.cat <- cut(CPI$CPI2015,breaks=c(0,25,50,75,100), label=c("very corrupt","corrupt","clean","very clean"))
table(CPI$CPI.cat)

#Crosstabulations

#The following line creates a frequency table for 2 variables and adds labels
crosstab1 <- table(CPI$Region, CPI$CPI.cat, useNA="always", dnn=c("Region", "Index Category"))
crosstab1

#The following line changes the column labels
colnames(crosstab1) <- c("Highly Corrupt", "Corrupt", "Clean", "Highly Clean","NA")
crosstab1

#The following lines generate a set crosstab with proportions
install.packages("gmodels")
library(gmodels)
CrossTable(CPI$Region, CPI$CPI.cat, prop.r=T, prop.c=T, prop.t=F, prop.chisq=F, format="SPSS")

#Histograms
hist(CPI$CPI2015, breaks=50, main="Histogram of Corruption Index (2015)", xlab="CPI 2015")

#Individual statistics
min(CPI$CPI2015, na.rm=TRUE)
max(CPI$CPI2015, na.rm=TRUE)
mean(CPI$CPI2015, na.rm=TRUE)
#Indexing
mean(CPI$CPI2015[CPI$Region == "SSA"], na.rm=TRUE)
sd(CPI$CPI2015[CPI$Region == "SSA"], na.rm=TRUE)

mean(CPI$CPI2015[CPI$Region == "AME"], na.rm=TRUE)
sd(CPI$CPI2015[CPI$Region == "AME"], na.rm=TRUE)

#Generating a new variable

#The following line creates an empty variable
CPI$Americas<-NA
#The following line uses conditional statements to assign values
CPI$Americas<-ifelse(CPI$Region=="AME",1,0)

#Subsetting Data

#The following line creates a subset of data
CPI.AME<- subset(CPI, Americas==1)

#Analysis

#The following line generates a correlation
cor(CPI.AME$CPI2015, CPI.AME$World.Bank, use="complete.obs")

#The following line generates regression output
fit.1 <- lm(CPI2015 ~World.Bank, data=CPI)
summary(fit.1)

#Charting

install.packages("ggplot2")
library(ggplot2)
install.packages("gapminder")
library(gapminder) 
gapminder

#The following line creates a set of axes
ggplot(gapminder, aes(x = gdpPercap, y = lifeExp))
#The following line assigns the chart to an object
p <- ggplot(data = gapminder, aes(x = gdpPercap, y = lifeExp))
#The following line adds points to create a scatter plot
p + geom_point() 
#The following line creates the simple scatter plot
ggplot( data = gapminder, aes(x = gdpPercap, y = lifeExp)) + geom_point()
#The following line uses a log transformation
ggplot(gapminder, aes(x = log10(gdpPercap), y = lifeExp)) + geom_point() + scale_x_log10()
#The following line adds the log scale to the object
p <- p + scale_x_log10()
#The following line maps the continent to color
p + geom_point(aes(color = continent))
#The following line puts it all together
ggplot(gapminder, aes(x = gdpPercap, y = lifeExp)) + geom_point(aes(color = continent)) + scale_x_log10()
#The following line changes the size and opacity of the points
p + geom_point(aes(color = continent), alpha = 0.3, size=3)
#The following line adds a fitted loess line
p + geom_point(aes(color = continent)) + geom_smooth() #loess
#The following line specifies a linear fitted line
p + geom_point(aes(color = continent)) + geom_smooth(method="lm")
#The following line maps the continent to the fitted line
p + geom_point(aes(color = continent)) + geom_smooth(se = FALSE, aes(color = continent))
#The following line maps the continent to the facet
ggplot(gapminder, aes(x = gdpPercap, y = lifeExp, color = continent)) + geom_point(alpha=0.5, size=3) + facet_grid( . ~ continent)
#The following line maps continent to a fitted line
p <- ggplot(gapminder, aes(x = gdpPercap, y = lifeExp, color = continent)) + geom_point(alpha=0.5, size=3) + facet_grid( . ~ continent) + geom_smooth(color="black", se=TRUE)

#Chart themes

install.packages("ggthemes")
library(ggthemes)

p + scale_x_log10("GDP per Capita", labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  ylab("Life Expectancy in Years") +
  theme_economist() +
  theme(legend.position="none") +
  ggtitle("The relationship between wealth and longevity")
