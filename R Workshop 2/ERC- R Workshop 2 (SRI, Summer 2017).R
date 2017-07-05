#---------------------------------------------------
# Intro to R - Session 2
# Prepared by Patricia Kirkland
# Last updated: 8 June 2017
#---------------------------------------------------


## clear the workspace
rm(list=ls(all=TRUE))

## load packages
library(foreign)
library(dplyr)
library(car)
library(ggplot2)
library(DataCombine)

## set working directory
setwd("/Users/patriciakirkland/Dropbox/Empiprical Reasoning Center/R Workshop")

## load dataset
load("muni_25K_finance_data.RData")

## some basic info about data
dim(COG.muni.25K)

head(COG.muni.25K)

names(COG.muni.25K)

## one way to change the name of a data frame
COG <- COG.muni.25K 

## remove the original copy (optional)
COG.muni.25K <- NULL


#### subset- take certain variables or rows only

## keep only specified columns
COG.subset <- COG[, c(1:4, 6:8, 9, 10, 25:554)]

## drop variables using select() from the dplyr package
## note the - before the variable names; leave out the - to select variables to keep
COG.subset <- select(COG.subset, -SortCode, -SurveyYr, -County, -Type.Code)


## subset based on one or more condition(s)

## use the subset() function
COG.50K <- subset(COG.subset, Population >= 50000)

COG.50K.NE <- subset(COG.subset, Population >= 50000 & Census.Region == 1)

COG.large <- subset(COG.subset, Population > median(Population))

## or use the filter() function from the dplyr package
COG.25K <- filter(COG.subset, Population < 50000) 

COG.small <- filter(COG.subset, Population < median(Population))

COG.25K.WMW <- filter(COG.subset, Population < 50000 & (Census.Region == 2 | Census.Region == 4))

## nest dplyr functions to select rows & columns at the same time
cities.100K <- select(filter(COG, Population <= 100000), Year4, ID, State.Code, Name, Census.Region, Population)

## append rows of data

COG.subset2 <- rbind(COG.50K, COG.25K)

## merge data

## read in a new dataset
fips <- read.dta("GOVS to FIPS Crosswalk.dta")

## you can select variables to keep or drop (optional)
fips <- select(fips, -areaname)

## need to rename some variables to merge on

## one option is rename() in the dplyr package
## there are other options--- e.g., rename() in the reshape package or names()
## note that to rename variables with the names() option, you must re-enter names for all variables in order
COG.subset <- rename(COG.subset, govsid = ID, govsstate = State.Code)
fips <- rename(fips, Name = name)


## the dplyr package has several merge options--- left_join() is probably most commonly used
#### left_join(x, y) merges 2 data frames x & y, returning all rows from x and all columns from x & y
#### dplyr supports the following merge functions:  
#### inner_join(), left_join(), right(join), semi_join(), anti_join(), and full_join()

COG.fips <- left_join(COG.subset, fips)

## note the error message 
## also, we're left with 427 observations in COG.subset without matches in fips
length(COG.fips$fipsplace[is.na(COG.fips$fipsplace)])

class(fips$Name)
class(COG.subset$Name)

COG.subset$Name <- as.character(COG.subset$Name)

length(COG.fips$fipsplace[is.na(COG.fips$fipsplace)])

## you can (but do not have) to specify variables to use for merge
COG.fips <- left_join(COG.subset, fips, by=c("govsid", "govsstate"))

####  cleaning/preparing data

## formatting variables
COG.fips$fipsplace <- sprintf("%05.0f", as.numeric(COG.fips$fipsplace))

## concatenating 2 variables
COG.fips$fipsid <- paste0(COG.fips$fipsstate, COG.fips$fipsplace)
class(COG.fips$fipsid)
COG.fips$fipsid <- as.numeric(COG.fips$fipsid)

## coding/recoding variables

# census regions --- 1 = Northeast / 2 = Midwest / 3 = South / 4 = West

COG.fips <- within(COG.fips, {
   
    ## create and code a variable for population categories
    Population.Category <- NULL
    Population.Category[Population < summary(COG.fips$Population)[2]] <- "Small"
    Population.Category[Population > summary(COG.fips$Population)[2] & Population < summary(COG.fips$Population)[5]] <- "Medium"
    Population.Category[Population > summary(COG.fips$Population)[5]] <- "Large"

    ## recode to replace region number with region name
    Census.Region <- recode(Census.Region, recodes = "1='Northeast'; 2='Midwest'; 3='South'; 4='West'")
})

## perform math operations with one or more variable(s)
COG.fips <- within(COG.fips, {
    Total.Revenue.PC <- Total.Revenue/Population
    Total.Taxes.PC <- Total.Taxes/Population
    Total.Expenditure.PC <- Total.Expenditure/Population
    
    log.Total.Revenue.PC <- log(Total.Revenue.PC)
    log.Total.Taxes.PC <- log(Total.Taxes.PC)
    log.Total.Expenditure <- log(Total.Expenditure.PC)
})


## lag & lead variables


# first, sort by city and year 
COG.fips <- COG.fips[order(COG.fips$fipsid, COG.fips$Year4),]

# a sort option from the dplyr package
COG.fips <- arrange(COG.fips, fipsid, Year4)


# one easy option is the slide function in the DataCombine package, which also creates lead variables
# note that slide() will lag or lead to the next observation of the TimeVar even if there is a gap
COG.fips <- slide(COG.fips, Var = "Total.Revenue.PC", TimeVar = "Year4", GroupVar = "fipsid", 
                  NewVar = "lag.Total.Revenue.PC", slideBy = -1)

# a loop is a less efficient but effective option

# first, sort by city and year 
COG.fips <- COG.fips[order(COG.fips$fipsid, COG.fips$Year4),]

COG.fips <- within(filter(COG.fips, !is.na(fipsid)), {
    ## make the new variables with null values
    lag.Total.Taxes.PC <- NULL
    lag.Total.Expenditure.PC <- NULL
    
    
    ## loop over rows to fill in lagged values
    for(i in 2:length(Year4)) {
            if(fipsid[i] == fipsid[i-1] & Year4[i] == Year4[i-1] + 1){
                lag.Total.Taxes.PC[i] <- Total.Taxes.PC[i-1]
                lag.Total.Expenditure.PC[i] <- Total.Expenditure.PC[i-1]
            }}
})


## remove some lagged values made using slide()
COG.fips$lag.Total.Revenue.PC <- ifelse(is.na(COG.fips$lag.Total.Taxes.PC) & is.na(COG.fips$lag.Total.Expenditure.PC), NA, COG.fips$lag.Total.Revenue.PC)


save(COG.fips, file = "muni_finance_data_cleaned.RData")


###  ggplot


## create factors-- factors designate groups or categories (this is optional, depending on the figures you need)
COG.fips$Population.Category <- factor(COG.fips$Population.Category, levels = c("Small", "Medium", "Large"))

COG.fips$Census.Region <- factor(COG.fips$Census.Region, levels = c("Northeast", "Midwest", "South", "West"))


#### qplot -- quick plots

# Kernel density plots
# grouped by number of gender (indicated by color)
qplot(log(Population), data=COG.fips, geom="density", fill=Census.Region, alpha=I(.5), 
      main="Distribution of Population", xlab="Population (log)", 
      ylab="Density")

# Histogram 
# grouped by number of gender (indicated by color)
qplot(log(Population), data=COG.fips, geom="histogram", fill=Census.Region, alpha=I(.5), 
      main="Distribution of Population", xlab="Population (log)", 
      ylab="Density")

# Scatterplot 
# in each facet, region is differentiated by shape and color
qplot(log(Population), Total.Expenditure.PC, data=COG.fips, shape=Census.Region, color=Census.Region, 
      facets=Census.Region~Population.Category, alpha=I(.5), size=I(3),
      xlab="Population (log)", ylab="Total Expentitures (per capita)") 

# add a trend line
qplot(log(Population), Total.Expenditure.PC, data=COG.fips, geom=c("point", "smooth"), 
      alpha=I(.4),
      main="Regression of Expenditures on Population", 
      xlab="Population (log)", ylab="Total Expentitures (per capita)")


# linear regression lines by region
qplot(log(Population), Total.Expenditure.PC, data=COG.fips, geom=c("point"), 
      color=Census.Region, alpha=I(.4),
      main="Regression of Expenditures on Population", 
      xlab="Population (log)", ylab="Total Expentitures (per capita)") +
      stat_smooth(method="lm", formula=y~x)


# Boxplots of course population by region
# observations (points) are overlayed and jittered
qplot(Census.Region, log(Population), data=COG.fips, geom="boxplot", 
      fill=Census.Region, main="Population by Region",
      xlab="", ylab="Population (log)")


#### ggplot


plot <- ggplot(COG.fips, aes(x=Total.Revenue.PC, y=Total.Expenditure.PC)) + geom_point(alpha=.5) + geom_smooth()
plot

plot_2 <- ggplot(filter(COG.fips, Population.Category == "Medium"), aes(x=Total.Revenue.PC, y=Total.Expenditure.PC)) + 
    geom_point(alpha=.5) + geom_smooth()
plot_2

plot_3 <- ggplot(COG.fips, aes(x=Total.Revenue.PC, y=Total.Expenditure.PC)) + 
    geom_point(alpha=.5) + 
    geom_smooth() +
    facet_grid(Population.Category ~ .) 
plot_3  

plot_4 <- ggplot(COG.fips, aes(x=Property.Tax/Population, y=Total.Revenue.PC)) +
    geom_point(alpha=.5) + 
    geom_smooth(method="lm", formula=y~x) 
plot_4

plot_5 <- plot_4 + facet_grid(Census.Region ~ Population.Category) +
    geom_smooth(method="lm", formula=y~x, se=FALSE) +
    theme_bw() +
    xlab("Property Tax (per capita)") +
    ylab("Total Revenue (per capita)")
plot_5

plot_6 <- plot_5 %+% aes(x=log((Property.Tax/Population)+1), y=log(Total.Revenue.PC+1)) +
    xlab("Property Tax (per capita logged)") +
    ylab("Total Revenue (per capita logged)")
plot_6    

plot_7 <- ggplot(COG.fips, aes(x=(Property.Tax/Population), fill=Census.Region)) + geom_histogram(binwidth=.05, alpha=.8)
plot_7

plot_8 <- plot_7 %+% aes(x=log((Property.Tax/Population)+1), fill=Census.Region)    
plot_8



## saving plots

# ## name the file
# pdf("plot_name.pdf")
# ## print the object (plot)
# print(plot__)
# ## close the figure file
# dev.off()

## ggsave
ggsave(file="revenue_plot.pdf", plot_6, height=6, width = 4)




###  Review + another shortcut

## summary stats

with(COG.fips, summary(Total.Expenditure.PC))
with(COG.fips, summary(Total.Revenue.PC))
with(COG.fips, summary(Total.Taxes.PC))

library(stargazer)

stargazer(select(COG.fips, Total.Expenditure.PC, Total.Revenue.PC, Total.Taxes.PC),
          type = "html", title = "Summary Statistics", 
          out = "/Users/patriciakirkland/Dropbox/Empiprical Reasoning Center/R Workshop/summary_statistics_table.htm")


## some regression models

fit_1 <- lm(Total.Expenditure.PC ~ Total.Taxes.PC, data=COG.fips)
summary(fit_1)

fit_2 <- lm(Total.Expenditure.PC ~ Total.Taxes.PC + Census.Region, data=COG.fips)
summary(fit_2)

fit_3 <- lm(Total.Expenditure.PC ~ Total.Taxes.PC + Population + Census.Region, data=COG.fips)
summary(fit_3)

fit_4 <- lm(Total.Expenditure.PC ~ Total.Taxes.PC + Population + Census.Region + Population.Category, data=COG.fips)
summary(fit_4)

stargazer(fit_1, fit_2, fit_3, fit_4,
          omit = c("Census.Region", "Population.Category"),
          add.lines = list(c("Fixed Effects", "N/A", "Region", "Region", "Region, Size")),
          title = "Results",
          type = "html",
          out = "regression_table_2.htm")


stargazer(fit_1, fit_2, fit_3, fit_4,
          title = "Results",
          type = "html",
          out = "regression_table_2.htm")

