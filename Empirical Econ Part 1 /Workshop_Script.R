# R Workshop
# Feb. 13, 2017
# Empirical Reasoning Center
# CORRUPTION PERCEPTIONS INDEX 2015
# http://www.transparency.org/cpi2015

# The following line clears all current data
#1

# Note: for help with a function type a ? or ?? before the command

# Setting a working directory

# Session>Set working directory>Choose Directory
# In the pop up window you want the find the folder that contains the data for this workshop

# To check what the current working directory is use the following line
#2


# Clicking through these menus is the same as running the following line
#3


#Importing data using the file menu

#File>Import Dataset>From Excel...
#Click Browse to find the Excel file
#Change Skip from 0 to 1 to ignore the first row in Excel

# The following line will import the data
#4




#Saving an R file

#5


#Opening an R file

#6

#Only ".RData" datasets can be opened this way.  Note that this is different from importing an Excel, CSV, or any other dataset type.

#Viewing a dataset

#The following line displays the dataset like a spreadsheet
#7


#The following line displays the variable names
#8

#The following line displays the first 6 observations of all variables
#9


#Tabulating a variable

#A frequency table provides all of the different values that variable takes on and the corresponding number of observations
#The following lines will create a frequency table of the Country variable
#10


#Renaming a variable

#The following lines change the name of the wbcode (Country abbreviation) variable to CT and tabulate CT
#11



#Exploring a variable 

#The following lines change the name of the World Bank CPIA variable to World.Bank and tabulate World.Bank
#12





#The following lines assign a table to an object and displays it
#13



#The following line generates summary statistics
#14

#The following line generates the 4th statistic from the summary
#15


#Recoding a variable

#The following line creates a categorical variable
#16



#The following adds labels to the categorical variable
#17



#Crosstabulations

#The following line creates a frequency table for 2 variables and adds labels
#18



#The following line changes the column labels
#19



#The following lines generate a set crosstab with proportions
#20




#Histograms
#21

#Individual statistics
#22



#Indexing
#23



#24




#Generating a new variable

#The following line creates an empty variable
#25

#The following line uses conditional statements to assign values
#26


#Subsetting Data

#The following line creates a subset of data
#27


#Analysis

#The following line generates a correlation
#28


#The following line generates regression output
#29



#Charting

#30






#The following line creates a set of axes
#31

#The following line assigns the chart to an object
#32

#The following line adds points to create a scatter plot
#33

#The following line creates the simple scatter plot
#34

#The following line uses a log transformation
#35

#The following line adds the log scale to the object
#36

#The following line maps the continent to color
#37

#The following line puts it all together
#38

#The following line changes the size and opacity of the points
#39

#The following line adds a fitted loess line
#40

#The following line specifies a linear fitted line
#41

#The following line maps the continent to the fitted line
#42

#The following line maps the continent to the facet
#43

#The following line maps continent to a fitted line
#44

#Chart themes

#45




