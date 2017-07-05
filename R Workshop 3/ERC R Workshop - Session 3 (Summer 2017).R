#---------------------------------------------------
# Intro to R - Session 3
# Prepared by Patricia Kirkland
# Last updated: 8 June 2017
#---------------------------------------------------

rm(list=ls(all=TRUE))


library(dplyr)

## update with the file path to your working directory
setwd("/Users/patriciakirkland/Dropbox/Empiprical Reasoning Center/R Workshop")

### user-defined functions

addVectors <- function(a,b) {
    
    out.vec <- a + b
    
    return(out.vec)
    
}

a <- 1:10
b <- -1:-10

addVectors(a, b)

x <- 1:10
y <- -1:-10

addVectors(x, y)

z <- addVectors(a=x, b=y)

cluster_se   <- function(dat,fm, cluster){
    require(sandwich, quietly = TRUE)
    require(lmtest, quietly = TRUE)
    M <- length(unique(cluster))
    N <- length(cluster)
    K <- fm$rank
    dfc <- (M/(M-1))*((N-1)/(N-K))
    uj  <- apply(estfun(fm),2, function(x) tapply(x, cluster, sum));
    vcovCL <- dfc*sandwich(fm, meat=crossprod(uj)/N)
    coeftest(fm, vcovCL) }


load("muni_finance_data_cleaned.RData")

fit_3 <- lm(Total.Expenditure.PC ~ Total.Taxes.PC + Population + Census.Region, data=COG.fips)
summary(fit_3)

library(lmtest)
library(sandwich)
# heteroskedasticity-robust standard errors
coeftest(fit_3, vcov=vcovHC(fit_3, type="HC1"))

robust_se <- function(regmodel){
    require(sandwich, quietly = TRUE)
    require(lmtest, quietly = TRUE)
    coeftest(regmodel, vcov=vcovHC(regmodel, type="HC1"))
}

# robust SEs with our user-defined function
robust_se(fit_3)

# cluster-robust standard errors
cluster_se(COG.fips, fit_3, COG.fips$fipsid)


## clear the workspace
rm(list=ls(all=TRUE))

## update with the file path to your working directory
setwd("/Users/patriciakirkland/Dropbox/Empiprical Reasoning Center/R Workshop")

source("ERC R Workshop Source.R")

load("muni_finance_data_cleaned.RData")

fit_3 <- lm(Total.Expenditure.PC ~ Total.Taxes.PC + Population + Census.Region, data=COG.fips)
summary(fit_3)

# robust SEs with our user-defined function (from source file)
robust_se(fit_3)

# cluster-robust standard errors (from source file)
cluster_se(COG.fips, fit_3, COG.fips$fipsid)


#------------------------------------------------------
# 
# Automating tasks--- an example
#
#------------------------------------------------------

rm(list=ls(all=TRUE))


library(dplyr)

## update with the file path to your working directory
setwd("/Users/patriciakirkland/Dropbox/Empiprical Reasoning Center/R Workshop")



#### build annual .csv files from COG text files
## update with the file path for your directory
directory <- "/Users/patriciakirkland/Dropbox/Census of Governments/_IndFin_1967-2007"
year <- 2000:2003

COG.muni <- data.frame()

for(j in year){ 
    
i <- substr(as.character(j), 3, 4)
    
COG.a <- read.csv(paste0(directory, "/", "IndFin", formatC(i, width = 2, flag = "0"), "a",
                        ".txt"))

COG.b <- read.csv(paste0(directory, "/", "IndFin", formatC(i, width = 2, flag = "0"), "b",
                        ".txt"))

COG.c <- read.csv(paste0(directory, "/", "IndFin", formatC(i, width = 2, flag = "0"), "c",
                        ".txt"))

COGmerge <- left_join(COG.a, COG.b)

COGmerge <- left_join(COGmerge, COG.c)

# COG.muni.temp <- subset(COGmerge, Type.Code == 2)  

COG.muni <- rbind(COG.muni, subset(COGmerge, Type.Code == 2))

}


#------------------------------------------------------
# 
# Apply Functions 
#
#------------------------------------------------------



load("muni_finance_data_cleaned.RData")

### tapply() -- takes a vector & returns a vector; performs function on subsets by grouping variable(s)

## group or categorical variables typically should be factors
COG.fips$Census.Region <- factor(COG.fips$Census.Region)

## check the frequency distribution (optional)
table(COG.fips$Census.Region)

## use tapply() to get group means (you can use other functions as well)
tapply(COG.fips$Total.Expenditure, COG.fips$Census.Region, mean, na.rm=TRUE)


### sapply() takes a list, a data frame, or a subset of a data frame, 
### performs a function on each element, and returns a vector

## basic example
sapply(COG.fips[, 7:15], mean, na.rm=TRUE)

## create a data frame of summary statistics

## start by obtaining the statistics using sapply()
COG_means <- sapply(COG.fips[, 7:15], mean, na.rm=TRUE)
COG_medians <- sapply(COG.fips[, 7:15], median, na.rm=TRUE)
COG_stdevs <- sapply(COG.fips[, 7:15], sd, na.rm=TRUE)

## create a vector of variable names
COG_variable <- names(COG.fips[, 7:15])

## bind the vectors by columns
COG_summary <- cbind.data.frame(COG_variable, COG_means, COG_medians, COG_stdevs)

## remove the row names
row.names(COG_summary) <- NULL

COG_summary

#### lapply() -- takes a list or data frame; returns a list

## EXAMPLE:  run the same regression on multiple DVs
## specify a list of variables
varlist <- c("Total.Expenditure.PC", "Total.Taxes.PC", "Total.Revenue.PC")

## run the same regression on multiple DVs
COG_models <- lapply(varlist, function(x) {
    
    lm(substitute(COG.fips$i ~ COG.fips$Population + COG.fips$Census.Region, list(i = x)))
    
})

## to perform the summary() function
COG_results <- lapply(COG_models, summary)


## The functions above can be combined into a single lapply() functions
COG_models_results <- lapply(varlist, function(x) {
    
    summary(lm(substitute(COG.fips$i ~ COG.fips$Population + COG.fips$Census.Region, list(i = x))))
    
})

COG_models_results

## extract the first element
COG_models_results[[1]]

## Add names to the elements in a list
names(COG_models_results) <- varlist

COG_models_results

##  you can extract them using the names you assigned
COG_models_results[["Total.Taxes.PC"]]


## extract more detailed information
COG_models_results[["Total.Taxes.PC"]]$coefficients

COG_models_results[["Total.Taxes.PC"]]$coefficients[, 1:2]


#------------------------------------------------------
# 
# ANOVA Models --- several examples
#
#------------------------------------------------------

#### One-way ANOVA
## Adapted from https://personality-project.org/r/r.guide.html\#anova
    
## load data
datafilename <- "http://personality-project.org/r/datasets/R.appendix1.data"
data.ex1 <- read.table(datafilename,header=T)

## check the class of the variables -- the IV should be a factor (group or categorical) variable
class(data.ex1$Dosage)
class(data.ex1$Alertness)

## ANOVA model
aov.ex1 <-  aov(Alertness ~ Dosage, data = data.ex1)  
summary(aov.ex1)                                   

## obtain the means and the number of subjects per cell
model.tables(aov.ex1, "means")

## a basic boxplot -- you can add additional arguments to format the boxplot
## you could also do this in ggplot if you prefer
boxplot(Alertness ~ Dosage, data = data.ex1)

    
    
    
#### Two-way (between subjects) ANOVA
## Adapted from https://personality-project.org/r/r.guide.html\#anova
    
## load data
datafilename <- "http://personality-project.org/r/datasets/R.appendix2.data"
data.ex2 <- read.table(datafilename,header=T) 

## view data
data.ex2                                   

## ANOVA model
aov.ex2 <- aov(Alertness ~ Gender*Dosage, data = data.ex2)        
summary(aov.ex2)               

## obtain the means and the number of subjects per cell
model.tables(aov.ex2, "means")     

## graphical summary using a boxplot
boxplot(Alertness ~ Dosage*Gender, data = data.ex2) 

## another way to graph the means 
with(data.ex2, interaction.plot(Dosage, Gender, Alertness))  


    
#### One-way repeated measures ANOVA
## note that data must be in long format (one response variable in one column)  
## Adapted from https://personality-project.org/r/r.guide.html\#anova
    
## load data
datafilename <- "http://personality-project.org/r/datasets/R.appendix3.data"
data.ex3 <- read.table(datafilename, header=T)



## view data -- note, data are in long format
data.ex3  

## ANOVA model -- note the Error() term, indicates that the treatment (data.ex3$Valence) 
## is nested within subjects (data.ex3Subject)
aov.ex3 <- aov(Recall ~ Valence + Error(Subject/Valence), data = data.ex3)
summary(aov.ex3)

## obtain the means and the number of subjects per cell
model.tables(aov.ex3, "means")      

## boxplot (also could use ggplot)
boxplot(Recall ~ Valence, data = data.ex3)          

    
    
#### Two-way repeated measures ANOVA
## Adapted from https://personality-project.org/r/r.guide.html\#anova
    
## load data
datafilename="http://personality-project.org/r/datasets/R.appendix4.data"
data.ex4=read.table(datafilename,header=T)   

## ANOVA
aov.ex4=aov(Recall ~ (Task*Valence) + Error(Subject/(Task*Valence)), data = data.ex4)
summary(aov.ex4)

## obtain the means and the number of subjects/cell
model.tables(aov.ex4, "means")     

## boxplot
boxplot(Recall ~ Task*Valence, data = data.ex4) 

## interaction.plot -- another way to graph the interaction
with(data.ex4, interaction.plot(Valence, Task, Recall))    

    
    
    
#### 4-way ANOVA: 2 repeated measures and two between-subjects
## Adapted from https://personality-project.org/r/r.guide.html\#anova

## load data
datafilename="http://personality-project.org/r/datasets/R.appendix5.data"
data.ex5=read.table(datafilename,header=T)   

## ANOVA
aov.ex5 <- aov(Recall ~ (Task*Valence*Gender*Dosage) + 
                   Error(Subject/(Task*Valence)) + 
                   (Gender*Dosage), 
               data = data.ex5)

summary(aov.ex5) 

## obtain the means and the number of subjects per cell
model.tables(aov.ex5, "means")     

# graphical summary of means of the 36 cells
boxplot(Recall ~ Task*Valence*Gender*Dosage, data = data.ex5) 

# graphical summary of means of  18 cells
boxplot(Recall ~ Task*Valence*Dosage, data = data.ex5)



#------------------------------------------------------
# 
# Structural Equation Models --- an example
#
#------------------------------------------------------



## adapted from a presentation by Grace Charles, presented at Davis R Users' Group on May 15, 2015
## adapted from Jim Grace's SEM workshop and Lavaan tutorials
rm(list=ls())  

### install libraries
library(lavaan)
library(semPlot)
library(qgraph)

## here is an example model using a built-in dataset-- if you have problems accessing it, you may 
## need to load the datasets package

# built in dataset
data(PoliticalDemocracy)

# let's name the variables per the documentation
names(PoliticalDemocracy) <- c("free_press_1960", "free_oppo_1960", "fair_elect_1960", "leg_effective_1960",
                               "free_press_1965", "free_oppo_1965", "fair_elect_1965", "leg_effect_1965",
                               "GNP_pc_1960", "energy_pc_1960", "labor_force_pct_industry_1960")






## the measurement model equations are "latent" and represented by =~
## regressions are indicated by ~
## residual correlations (in this case because they represent different years of the same measurement) are represented by ~~

model <- '
# measurement model
industrialization_1960 =~ GNP_pc_1960 + energy_pc_1960 + labor_force_pct_industry_1960
democracy_1960 =~ free_press_1960 + free_oppo_1960 + fair_elect_1960 + leg_effective_1960
democracy_1965 =~ free_press_1965 + free_oppo_1965 + fair_elect_1965 + leg_effect_1965

# regressions
democracy_1960 ~ industrialization_1960
democracy_1965 ~ industrialization_1960 + democracy_1960

# residual correlations
free_press_1960 ~~ free_press_1965
free_oppo_1960 ~~ leg_effective_1960 
free_oppo_1960 ~~ free_oppo_1965
fair_elect_1960 ~~ fair_elect_1965
leg_effective_1960 ~~ leg_effect_1965
free_oppo_1965 ~~ leg_effect_1965
'


#fit your SEM
fit <- sem(model, data = PoliticalDemocracy)

#summarize results
summary(fit, standardized = TRUE, rsq = T)

##plot results using semPaths function in qgraph
semPaths(fit, "std", edge.label.cex = 0.5, curvePivot = TRUE, layout = "tree")

### use dev.off() to clear your plots after you finish with the path diagram
# dev.off()

##check to see if you missed anything. High mi values suggest that there is a path that you missed.

modindices(fit)

## looks good

##can also look at variance tables  
vartable(fit)


## sometimes you get warnings about the scale of your variables
#Warning message:
# In getDataFull(data = data, group = group, group.label = group.label, :
#                 lavaan WARNING: some observed variances are (at least) a factor 100 times larger than others; please rescale

# in this case, all you have to do to make this error go away is rescale variables

#model comparison

#you can compare alternative pathway models using AIC, BIC, etc:

#create second alternative model

model2 <- '
# measurement model
industrialization_1960 =~ GNP_pc_1960 + energy_pc_1960 + labor_force_pct_industry_1960
democracy_1960 =~ free_press_1960 + free_oppo_1960 + fair_elect_1960 + leg_effective_1960
democracy_1965 =~ free_press_1965 + free_oppo_1965 + fair_elect_1965 + leg_effect_1965

# regressions
# leave ind60 out of regression
democracy_1960 ~ industrialization_1960
democracy_1965 ~ democracy_1960 

# residual correlations
free_press_1960 ~~ free_press_1965
free_oppo_1960 ~~ leg_effective_1960 
free_oppo_1960 ~~ free_oppo_1965
fair_elect_1960 ~~ fair_elect_1965
leg_effective_1960 ~~ leg_effect_1965
free_oppo_1965 ~~ leg_effect_1965
'


fit2 <- sem(model2, data = PoliticalDemocracy)
summary(fit2)

AIC(fit, fit2)





