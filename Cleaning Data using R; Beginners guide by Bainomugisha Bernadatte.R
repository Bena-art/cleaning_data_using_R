#Load libraries for reading and manipulating data.
library(readxl)
library(dplyr)
library(tidyr)
library(forcats)  # for factor recoding
#Import the dataset, it can be excel, csv, stata etc.You can go to file and then selct import dataset an d choose the option that fits your dataset.

#Explore dataset to understand variables and types.
#View the data structure to understand variable types and missingness
view(dataset) 
summary(Dataset)
glimpse(Dataset)
str(Dataset)

#If this is your first interaction with your dataset, firstly, it could be having variables you don't need, very long variables, poorly input data. 
# So first step, drop any variables you may not need.E.g 
(Dataset$`Name of the Interviewer`<-NULL)
#The column that appears after the dollar sign($) will then be dropped.
#In R, the dollar sign ($) is an access operator used to extract or access elements (usually variables or columns) from a list or a data frame by name.
#The $ operator lets you directly refer to a specific column by name. E.g from the 'dataset', if you want to access a specific varaible you can run code like this;
#dataset$age as shown above
#You can also access elements in a list where elements area named.
#e.g a list such as info <- list(name = "Bernadatte", country = "Uganda", age = 18)
#You can access an element in this list, with the code
#listname$ variable of interest , info$name

#Next step, rename long variable names to short, descriptive names for coding ease.
Datasetname<- Datasetname %>% rename(Latrine_Ownership = `__Do_you_own_a_latr_human_waste_disposal`)
#When renameing it is, rename(newvar_name= oldvar_name)
#Sometimes during data collection, data may be put in a very disorganised way that makes it hard to analyse.
#Your goal as an analyst is to make sure data is clean before analysis to enure ease of analysis and quality of results.
#Key questions to ask yourself , 
#Do I fully understand what this data represents and how it was collected?
#(Know the purpose, meaning of each variable, and data source.)
#Is the data complete and free from duplicates or missing values?
  #(Check for gaps, missing entries, and repeated records.)
sum(is.na(datasetname)) #this isdone to find out the total number of missing values in the dataset.
#Next, find out in hwich columns these missing variables are
colSums(is.na(datasetname))
#If the data is missing in essential variables/columns, it may affect the entry. However if data is missing in columns such as 'other challenges', it may not present a challenge as long as the main challenge variable is entered.
#Next, find out the extent of the missingness
#You can install a package called 'naniar' if it's not already installed.
install.packages("naniar")
#Tell R to use this package
library(naniar)
#Once you have used tools like naniar or is.na() to identify where your missing data are, what you do next depends on three things: How much data is missing?, Where is it missing (which columns)?, Why is it missing (randomly or systematically)?
#Next, summarize which variables have missingness
miss_var_summary(datasetname)
#Then,you can visualize missingness
vis_miss(datasetname)
#You can also visualize distributionof missingness amongst the variables
gg_miss_var(datasetname)
#OR visulaize which cases are mostly affected by missingness
gg_miss_case(datasetname)
#You can rename 'NA' to none if the missing value actually means “nothing” or “not applicable
#And when the variable is categorical, not numeric.
datasetname %>% mutate(other_variable=replace_na(other_variable, "none"))
#KEEP rows and HANDLE missingness if the variable is important but missing in some cases. You can impute numeric values (mean, median, or model-based), fill categorical values with “Unknown” or “None” if appropriate, or keep NAs but account for them during analysis
#for example;
mean(datasetname$height, na.rm = TRUE)
median(datasetname$height, na.rm = TRUE)
#This finds the mean or median while ignoring the missingness

#Imputation is the process of guessing what those missing values could have been, based on the information you already have. For example, for mean imputation, you can take the average (mean) of the known values and replacing missing values with the mean value. Median imputation can be used for skewed numeric data. For categorical variables, you can use the mode imputation by replacing missing values with the commonest response/mode response.
#You can safely drop rows with missing data if the number of missing rows is small (e.g., less than 5–10% of your dataset, the missing values are completely random (not following a pattern) and the missing variables are not essential for your analysis.
#Avoid dropping rows when Aa variable with missing data is important (e.g., income, location, outcome variable), missingness is systematic (e.g., poorer households skip certain questions). When dropping ‘NA’ can bias your analysis.
datasetname<-datasetname %>% filter(!complete.cases(.)) %>% drop_na(variablename)

#Then find out if there are any duplicates and which entries are duplicates using the command below
#If the output you get is, integer(0), then you have no duplicates in your dataset.
which(duplicated(datasetname))
#You can then drop the duplicates  using any of the following commands
datasetname <- datasetname[!duplicated(datasetname), ] #this directly drops the duplicates in your dataset.
#OR, you can create a new dataset/dataframe with only distinct and unique entries with the code below;
new_datasetname <- distinct(old_datasetname)

#Is the data consistent and correctly formatted?
  #(Ensure values, units, and categories are uniform and logical.)
#Start by finding out the unique entries in the unique entries in varaibles of interest. e.g
unique(datasetname$variable)
#Then recode as prefered. For example; 
datasetname<-datasetname %>% mutate(variable=recode
(variable, "oldvalue" = "newvalue","4__oldvalue" ="newvalue", "5__other"="other" ))

#After this, ask yourself the following;
#Are the data values accurate and within reasonable limits?
  #(Detect and handle outliers, impossible values, or any other data entry errors.)
#Is my dataset fully analysis-ready — with clear variable names, correctly derived variables, tidy structure, consistent handling of missing values, and properly saved, reproducible versions?
  #(Ensure names are short, descriptive, and free of spaces or special characters, handled missing and inconsistent values, confirm all missing or inconsistent entries have been treated and documented,  correct derived variables, tidy data structure)
#Lastly, before analysis, you may want to make sure variables are saved in the correct data format.
##In R, common data types include numeric, which represents decimal numbers used for calculations, such as 3.14 or 2.5; integer, which represents whole numbers stored as integers, like 5L (the L tells R it is an integer); character, used for text or strings, for example "Uganda" or "Yes"; factor, which represents categorical variables with levels, such as "Male" and "Female"; logical, representing TRUE/FALSE values; and Date/POSIXct, used for dates and times, for instance as.Date("2025-10-26").

##Next, identify data types of key variables
class(datasetname$variable)

###change data types as needed
Variablename<-as.factor(variablename)
Variablename<-as.integer(variablename)
Variablename<-as.character(variablename) #etc

#Analyse cleaned data or export it for future use. #Best of Luck