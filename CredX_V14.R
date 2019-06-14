#********************************************** BFS Capstone Project *********************************************#

#*****************************************************************************************************************#

#---------------------------------------------- CredX Risk Analytics ----------------------------------------------

# Below is Cross Industry Standard Process for Data Mining (CRISP-DM) framework steps to be followed in project.

# 1. Business Understanding
# 2. Data Understanding  
# 3. Data Preparation
# 4. Modelling
# 5. Model Evaluation
# 6. Model Deployment and Recommendations

#***************************************** Business Understanding  ***********************************#

# CredX is a leading credit card provider that gets thousands of credit card applicants every year. 
# But in the past few years, it has experienced an increase in credit loss. 
# The CEO believes that the best strategy to mitigate credit risk is to 'acquire the right customers'.

# Business Objective is to help CredX identify the right customers using predictive models. 
# Using past data of the bank's applicants, determine the factors affecting credit risk, 
# create strategies to mitigate the acquisition risk and assess the financial benefit.   

#***************************** Installing and loading the required packages *************************#

#install.packages('readr')
#install.packages('sqldf')
#install.packages("ggplot2")
#install.packages("cowplot")
#install.packages("caTools")
#install.packages('car')
#install.packages("ggthemes")
#install.packages("scales")
#install.packages("Information")
#install.packages("woeBinning")
#install.packages('carData')
#install.packages('haven')
#install.packages('glue')
#install.packages('dplyr')
#devtools::install_git("https://github.com/prk327/AtConP.git")
#install.packages("caret")
#install.packages("e1071")
#install.packages("caret", dependencies = c("Depends", "Suggests"))
#install.packages("cowplot")
#install.packages("haven")
#install.packages('ROCR');
#install.packages("GGally")
#install.packages("gplots")
#install.packages(c("rpart", "rpart.plot", "caret", "ggplot2"))
#install.packages("randomForest")
#install.packages("DMwR")

library(DMwR)
library(ROCR)
library(MASS)
library(car)
library(e1071)
library(caret);
library(ggplot2)
library(cowplot)
library(caTools)
library(caret);
library(readr)
library(sqldf)
library(ggplot2)
library(cowplot)
library(caTools)
library(car)
library(ggthemes)
library(scales)
library(Information)
library(dplyr)
library(AtConP)
library(MASS);
library(rpart);
library(rpart.plot)
library(randomForest)

#***************************************** Data Understanding ****************************************#

## Set working directory 
#setwd("C:/Users/aninchak/Desktop/Infinite/Capstone Project")

## Loading files
Demographic_data <- read_csv("Demographic data.csv")
Credit_Bureau_data <- read_csv("Credit Bureau data.csv")

########################################### Basic Data Quality ChecK #########################################

dim(Demographic_data) #71295 obs. of  12 variables
dim(Credit_Bureau_data) #71295 obs. of  19 variables

## Checking structure of both the data set.

str(Demographic_data)
str(Credit_Bureau_data)

## Checking summary of both the data set.

summary(Demographic_data)
summary(Credit_Bureau_data)

## Checking duplicate records
length(unique(Demographic_data$`Application ID`)) # 71292 unique records out of 71295 observations
length(unique(Credit_Bureau_data$`Application ID`)) # 71292 unique records out of 71295 observations


Demographic_data[duplicated(Demographic_data$`Application ID`),]
#765011468                                           
#653287861                                            
#671989187 

Credit_Bureau_data[duplicated(Credit_Bureau_data$`Application ID`),]
#765011468                                           
#653287861                                            
#671989187 

# Removing duplicate application id from Demographic and Credit Bureau data.
Demographic_data <- Demographic_data[!duplicated(Demographic_data$`Application ID`),]
length(Demographic_data$`Application ID`) #71292 records same as unique application ids
Credit_Bureau_data <- Credit_Bureau_data[!duplicated(Credit_Bureau_data$`Application ID`),]
length(Credit_Bureau_data$`Application ID`) ##71292 records same as unique application ids

Demographic_data[duplicated(Demographic_data$`Application ID`),]
Credit_Bureau_data[duplicated(Credit_Bureau_data$`Application ID`),]

# Now all duplicate records are removed from Demographic and Credit Bureau datasets
# Creating master data set by merging both the datasets on Application ID.

# Checking Application ID in Demographic and Credit Bureau datasets before creating master data.
setdiff(Demographic_data$`Application ID`, Credit_Bureau_data$`Application ID`)
  # Numeric (0) ... indicates Application ID in both the datasets are same

master_data <- merge(Demographic_data, Credit_Bureau_data, by = "Application ID")
sqldf("select `Application ID` , count(*) from master_data group by `Application ID` having count(*) > 1")
# There are no duplicate records in master_data
#View(master_data)
dim(master_data) # 71292 obs. of  30 variables

# Checking NA values 
sapply(master_data, function(x) sum(is.na(x))) # There are NA values present in dataset
sum(is.na(master_data)) / nrow(master_data) * 100 # 6.45% values in dataset are NA

# Checking values of performance tag of both the data frame equal or not.
sum(which(is.na(master_data$'Performance Tag.x')) == which(is.na(master_data$'Performance Tag.y')))
# 1425 NA values are there in Performnace tag x and performance tag y
sum(master_data$'Performance Tag.x' == master_data$'Performance Tag.y', na.rm = T)
# 69867 values are there in Performnace tag x and performance tag y after excluding 1425 records 
# 69867 + 1425 = 71292 

# Both the datasets ('Performance Tag.x' and 'Performance Tag.y') has equal values we can consider either one. 
# hence removing 'Performance Tag.y' (column# 12)from master datasets.
# And renaming 'Performance Tag.x' as Performance Tag

master_data <- master_data[, -12]
dim(master_data) #71292 obs. of  29 variables
colnames(master_data)[29] <- "Performance Tag"
#View(master_data)
summary(master_data)


# Performance Tag with NA values are rejected applications, Storing them in a seperate data frame 
Rejected <- master_data[which(is.na(master_data$'Performance Tag')),]
#View(Rejected) #1425 observations and 29 variables

# Removing 1425 NA records in performnace tag from master data.
master_data <- master_data[-which(is.na(master_data$'Performance Tag')),]
dim(master_data) #69867 observations and 29 variables after after removing 1425 records
sum(is.na(master_data$'Performance Tag')) #Resulted "0" that means all na values removed from Performance Tag

# 1425+69867 = 71292 (Total records) 

sapply(master_data, function(x) sum(is.na(x))) #There are NA values
(sum(is.na(master_data)) / nrow(master_data)) * 100 # 2.45% values are NA
# Since only 2% is NA, these values can be removed from the dataset

#************************************* Exploratory data analysis ********************************************#

#********* Age variable
sum(is.na(master_data$Age)) # 0 NA values
quantile(master_data$Age, seq(0, 1, 0.01))

# Age should be positive, also to apply credit card age must be at least 18 years.
# There are -3,0 in the age which should be removed
# Setting the age to 18 where the age is less than 18
master_data$Age[master_data$Age < 18] <- 18

#******** Gender Variable 
sum(is.na(master_data$Gender)) # 2 NA values are there, removing them from dataset.
master_data <- master_data[-which(is.na(master_data$Gender)),]
str(master_data$Gender) # Coverting cha vector (Gender) into factor as this data discrete in nature
master_data$Gender <- as.factor(master_data$Gender)
levels(master_data$Gender) # There are 2 levels "F" "M"

#******** Marital Status (at the time of application) variable
# Renaming column name in simplified form
colnames(master_data)[4] <- "Marital Status"
sum(is.na(master_data$'Marital Status')) # 6 NA values, removing them from dataset
master_data <- master_data[-which(is.na(master_data$`Marital Status`)),]
str(master_data$'Marital Status') # Coverting cha vector (Marital Status)into factor as this data discrete in nature
master_data$`Marital Status` <- as.factor(master_data$`Marital Status`)
levels(master_data$'Marital Status') # There are 2 levels "Married" "Single" 

#******** No of dependents variable
sum(is.na(master_data$`No of dependents`)) # 3 NA values, removing them from dataset.

master_data <- master_data[-which(is.na(master_data$`No of dependents`)),]
str(master_data$`No of dependents`) # Coverting Num vector(No of dependents) into factor as this data discrete in nature
master_data$`No of dependents` <- as.factor(master_data$`No of dependents`)
levels(master_data$`No of dependents`) # There are 5 levels  "1" "2" "3" "4" "5"

#********* Income variable
sum(is.na(master_data$Income)) # 0 NA values
quantile(master_data$Income, seq(0, 1, 0.01)) # Some people have negative incomes.
boxplot(master_data$Income)
nrow(subset(master_data, master_data$Income < 4.5)) # 197 records are with less than 4.5 Income, flooring with 4.5
master_data$Income[master_data$Income < 4.5] <- 4.5

#********* Education variable
sum(is.na(master_data$Education)) # 118 NA values
str(master_data$Education) # Coverting cha vector(Education) into factor as this data discrete in nature
master_data$Education <- as.factor(master_data$Education)
# Replacing NA values with others in Education
master_data$Education[which(is.na(master_data$Education))] <- "Others"
levels(master_data$Education) #There are 5 levels "Bachelor" "Masters" "Others" "Phd" "Professional"




#********** Profession variable
sum(is.na(master_data$Profession)) # 12 NA values, removing them from datasets
master_data <- master_data[-which(is.na(master_data$Profession)),]
str(master_data$Profession) # Coverting cha vector(Profession) into factor as this data discrete in nature
master_data$Profession <- as.factor(master_data$Profession)
levels(master_data$Profession) # There are 3 levels  "SAL"     "SE"      "SE_PROF"

#********** Type of residence variable
sum(is.na(master_data$`Type of residence`)) #8 NA values, removing them from dataset
master_data <- master_data[-which(is.na(master_data$`Type of residence`)),]
str(master_data$`Type of residence`) #Coverting cha vector(Type of residence) into factor as this data discrete in nature
master_data$`Type of residence` <- as.factor(master_data$`Type of residence`)
levels(master_data$`Type of residence`) #There are 5 levels "Company provided" "Living with Parents" "Others" "Owned" "Rented"

#********* No of months in current residence variable
sum(is.na(master_data$`No of months in current residence`)) # 0 NA Values
quantile(master_data$`No of months in current residence`, seq(0, 1, 0.01))
summary(master_data$`No of months in current residence`) #The quantile distribution of this column is ok 

#********** No of months in current company variable
sum(is.na(master_data$`No of months in current company`)) # 0 NA values
quantile(master_data$`No of months in current company`, seq(0, 1, 0.01))
summary(master_data$`No of months in current company`) #The quantile distribution of this column is ok

#********* Performance Tag variable
sum(is.na(master_data$`Performance Tag`)) #0 NA values
prop.table(table(master_data$`Performance Tag`)) # Around 96% are non defaulters, 4% are defaulters


#********* No of times 90 DPD or worse in last 6 months variable
sum(is.na(master_data$'No of times 90 DPD or worse in last 6 months')) # 0 NA values
summary(master_data$'No of times 90 DPD or worse in last 6 months')
master_data$`No of times 90 DPD or worse in last 6 months` <- as.factor(master_data$`No of times 90 DPD or worse in last 6 months`)
levels(master_data$`No of times 90 DPD or worse in last 6 months`) #There are 4 levels : 0,1,2,3

#********** No of times 60 DPD or worse in last 6 months variable       
sum(is.na(master_data$`No of times 60 DPD or worse in last 6 months`)) #0 NA values
summary(master_data$'No of times 60 DPD or worse in last 6 months')
master_data$`No of times 60 DPD or worse in last 6 months` <- as.factor(master_data$`No of times 60 DPD or worse in last 6 months`)
levels(master_data$`No of times 60 DPD or worse in last 6 months`) #There are 6 levels : 0,1,2,3,4,5

#********* No of times 30 DPD or worse in last 6 months variable
sum(is.na(master_data$`No of times 30 DPD or worse in last 6 months`)) #0 NA values
summary(master_data$'No of times 30 DPD or worse in last 6 months')
master_data$`No of times 30 DPD or worse in last 6 months` <- as.factor(master_data$`No of times 30 DPD or worse in last 6 months`)
levels(master_data$`No of times 30 DPD or worse in last 6 months`)#There are 8 levels : 0,1,2,3,4,5,6,7

#********* No of times 90 DPD or worse in last 12 months variable
sum(is.na(master_data$`No of times 90 DPD or worse in last 12 months`)) #0 NA values
summary(master_data$'No of times 90 DPD or worse in last 12 months')
master_data$`No of times 90 DPD or worse in last 12 months` <- as.factor(master_data$`No of times 90 DPD or worse in last 12 months`)
levels(master_data$`No of times 90 DPD or worse in last 12 months`) #There are 6 levels : 01,2,3,4,5

#******** No of times 60 DPD or worse in last 12 months variable
sum(is.na(master_data$`No of times 60 DPD or worse in last 12 months`)) #0 NA values
summary(master_data$'No of times 60 DPD or worse in last 12 months')
master_data$`No of times 60 DPD or worse in last 12 months` <- as.factor(master_data$`No of times 60 DPD or worse in last 12 months`)
levels(master_data$`No of times 60 DPD or worse in last 12 months`) #There are 8 levels : 0,1,2,3,4,5,6,7

#******** No of times 30 DPD or worse in last 12 months variable
sum(is.na(master_data$`No of times 30 DPD or worse in last 12 months`)) #0 NA values
summary(master_data$'No of times 30 DPD or worse in last 12 months')
master_data$`No of times 30 DPD or worse in last 12 months` <- as.factor(master_data$`No of times 30 DPD or worse in last 12 months`)
levels(master_data$`No of times 30 DPD or worse in last 12 months`)  #There are 10 levels : 0,1,2,3,4,5,6,7,8,9


#******** Avgas CC Utilization in last 12 months variable
sum(is.na(master_data$`Avgas CC Utilization in last 12 months`)) # 1023 NA values
# We are not removing these values as NA indicates that the CC is not used
quantile(master_data$`Avgas CC Utilization in last 12 months`, seq(0, 1, 0.01), na.rm= TRUE)

#******** No of trades opened in last 6 months variable      
sum(is.na(master_data$'No of trades opened in last 6 months')) # 1 NA value, removing from dataset
master_data <- master_data[-which(is.na(master_data$'No of trades opened in last 6 months')),]
quantile(master_data$`No of trades opened in last 6 months`, seq(0, 1, 0.01)) 
master_data$`No of trades opened in last 6 months` <- as.factor(master_data$`No of trades opened in last 6 months`)
levels(master_data$`No of trades opened in last 6 months`)  #There are 10 levels : 0,1,2,3,4,5,6,7,8,9



#******** No of trades opened in last 12 months variable
sum(is.na(master_data$`No of trades opened in last 12 months`)) # 0 NA value
quantile(master_data$`No of trades opened in last 12 months`, seq(0, 1, 0.01)) 
master_data$`No of trades opened in last 12 months` <- as.factor(master_data$`No of trades opened in last 12 months`)
levels(master_data$`No of trades opened in last 12 months`)

#******** No of PL trades opened in last 6 months variable
sum(is.na(master_data$`No of PL trades opened in last 6 months`)) # 0 NA value
quantile(master_data$`No of PL trades opened in last 6 months`, seq(0, 1, 0.01)) 
master_data$`No of PL trades opened in last 6 months` <- as.factor(master_data$`No of PL trades opened in last 6 months`)
levels(master_data$`No of PL trades opened in last 6 months`)

#******** No of PL trades opened in last 12 months variable
sum(is.na(master_data$`No of PL trades opened in last 12 months`)) # 0 NA value
quantile(master_data$`No of PL trades opened in last 12 months`, seq(0, 1, 0.01)) #capping at 99%ile
master_data$`No of PL trades opened in last 12 months` <- as.factor(master_data$`No of PL trades opened in last 12 months`)
levels(master_data$`No of PL trades opened in last 12 months`)

#******** No of Inquiries in last 6 months (excluding home & auto loans) variable
sum(is.na(master_data$`No of Inquiries in last 6 months (excluding home & auto loans)`)) # 0 NA value
quantile(master_data$`No of Inquiries in last 6 months (excluding home & auto loans)`, seq(0, 1, 0.01))
master_data$`No of Inquiries in last 6 months (excluding home & auto loans)` <- as.factor(master_data$`No of Inquiries in last 6 months (excluding home & auto loans)`)
levels(master_data$`No of Inquiries in last 6 months (excluding home & auto loans)`)

#******** No of Inquiries in last 12 months (excluding home & auto loans) variable
sum(is.na(master_data$`No of Inquiries in last 12 months (excluding home & auto loans)`)) # 0 NA value
quantile(master_data$`No of Inquiries in last 12 months (excluding home & auto loans)`, seq(0, 1, 0.01)) #capping at 99%ile
master_data$`No of Inquiries in last 12 months (excluding home & auto loans)` <- as.factor(master_data$`No of Inquiries in last 12 months (excluding home & auto loans)`)
levels(master_data$`No of Inquiries in last 12 months (excluding home & auto loans)`)


#******** Presence of open home loan
sum(is.na(master_data$`Presence of open home loan`)) # 272 NA value, removing them from dataset
master_data <- master_data[-which(is.na(master_data$`Presence of open home loan`)),]
str(master_data$`Presence of open home loan`) #Coverting cha vector(Presence of open home loan) into factor as this data discrete in nature
master_data$`Presence of open home loan` <- as.factor(master_data$`Presence of open home loan`)
levels(master_data$`Presence of open home loan`) # Two levels 0 & 1

#********* Outstanding Balance variable
sum(is.na(master_data$`Outstanding Balance`)) # 0 NA values
quantile(master_data$`Outstanding Balance`, seq(0, 1, 0.01))
nrow(subset(master_data, master_data$`Outstanding Balance` > 4250988.04)) # 696 rows with outstanding balance above 99th percentil value (4250988.04) 
#capping the data at 99th percentile value
master_data[which(master_data$`Outstanding Balance` > 4250988.04),]$`Outstanding Balance` <- 4250988.04
summary(master_data$`Outstanding Balance`)

#********* Total No of Trades variable
sum(is.na(master_data$`Total No of Trades`)) # 0 NA values
quantile(master_data$`Total No of Trades`, seq(0, 1, 0.01)) #capping at 99%ile
master_data[which(master_data$`Total No of Trades` > 31),]$`Total No of Trades` <- 31
summary(master_data$`Total No of Trades`)

#******** Presence of Open Auto loan
sum(is.na(master_data$`Presence of open auto loan`)) # 0 NA values
quantile(master_data$`Presence of open auto loan`, seq(0, 1, 0.01)) #quantile distribution is ok
str(master_data$`Presence of open auto loan`) #converting num vector(Presence of open auto loan) in factor vector
master_data$`Presence of open auto loan` <- as.factor(master_data$`Presence of open auto loan`)
levels(master_data$`Presence of open auto loan`) # Two levels 0 & 1


## Avgas CC utilisation has NA values which are replaced by 0

colnames(master_data)[colSums(is.na(master_data))>0]
sum(is.na(master_data$`Avgas CC Utilization in last 12 months`))
master_data$`Avgas CC Utilization in last 12 months`
#master_data[which(is.na(master_data$`Avgas CC Utilization in last 12 months`)),] <- 0

#************************************** Uni-variate Analysis ************************************************************

str(master_data)

# 1. Bar Charts for categorical variables

##ggplot(data = master_data, aes(Gender)) + geom_bar(stat = "count") + geom_text(stat = 'count', aes(label = ..count..), position = position_dodge(width = 0.9), vjust = -0.25) + xlab("Gender") + ylab("Count of Applicants") + ggtitle("No of Applicants in each Gender")

##ggplot(data = master_data, aes('Marital Status')) + geom_bar(stat = "count") + geom_text(stat = 'count', aes(label = ..count..), position = position_dodge(width = 0.9), vjust = -0.25) + xlab("Marital Status") + ylab("Count of Applicants") + ggtitle("No of Applicants versus Marital Status")

##ggplot(data = master_data, aes(Profession)) + geom_bar(stat = "count") + geom_text(stat = 'count', aes(label = ..count..), position = position_dodge(width = 0.9), vjust = -0.25) + xlab("Profession") + ylab("Count of Applicants") + ggtitle("No of Applicants in each Profession")

##ggplot(data = master_data, aes(Education)) + geom_bar(stat = "count") + geom_text(stat = 'count', aes(label = ..count..), position = position_dodge(width = 0.9), vjust = -0.25) + xlab("Education") + ylab("Count of Applicants") + ggtitle("No of Applicants versus Education")

#ggplot(data = master_data, aes(`Type of residence`)) + geom_bar(stat = "count") + geom_text(stat = 'count', aes(label = ..count..), position = position_dodge(width = 0.9), vjust = -0.25) + xlab("Type of residence") + ylab("Count of Applicants") + ggtitle("No of Applicants versus Type of residence")

#ggplot(data = master_data, aes(`No of dependents`)) + geom_bar(stat = "count") + geom_text(stat = 'count', aes(label = ..count..), position = position_dodge(width = 0.9), vjust = -0.25) + xlab("No of dependents") + ylab("Count of Applicants") + ggtitle("No of Applicants versus No of dependents")

#ggplot(data = master_data, aes(`No of times 90 DPD or worse in last 6 months`)) + geom_bar(stat = "count") + geom_text(stat = 'count', aes(label = ..count..), position = position_dodge(width = 0.9), vjust = -0.25) + xlab("No of times 90 DPD or worse in last 6 months") + ylab("Count of Applicants") + ggtitle("No of times 90 DPD or worse in last 6 months")

#ggplot(data = master_data, aes(`No of times 60 DPD or worse in last 6 months`)) + geom_bar(stat = "count") + geom_text(stat = 'count', aes(label = ..count..), position = position_dodge(width = 0.9), vjust = -0.25) + xlab("No of times 60 DPD or worse in last 6 months") + ylab("Count of Applicants") + ggtitle("No of times 60 DPD or worse in last 6 months")

#ggplot(data = master_data, aes(`No of times 30 DPD or worse in last 6 months`)) + geom_bar(stat = "count") + geom_text(stat = 'count', aes(label = ..count..), position = position_dodge(width = 0.9), vjust = -0.25) + xlab("No of times 30 DPD or worse in last 6 months") + ylab("Count of Applicants") + ggtitle("No of times 30 DPD or worse in last 6 months")

#ggplot(data = master_data, aes(`No of times 90 DPD or worse in last 12 months`)) + geom_bar(stat = "count") + geom_text(stat = 'count', aes(label = ..count..), position = position_dodge(width = 0.9), vjust = -0.25) + xlab("No of times 90 DPD or worse in last 12 months") + ylab("Count of Applicants") + ggtitle("No of times 90 DPD or worse in last 12 months")

#ggplot(data = master_data, aes(`No of times 60 DPD or worse in last 12 months`)) + geom_bar(stat = "count") + geom_text(stat = 'count', aes(label = ..count..), position = position_dodge(width = 0.9), vjust = -0.25) + xlab("No of times 60 DPD or worse in last 12 months") + ylab("Count of Applicants") + ggtitle("No of times 60 DPD or worse in last 12 months")

#ggplot(data = master_data, aes(`No of times 30 DPD or worse in last 12 months`)) + geom_bar(stat = "count") + geom_text(stat = 'count', aes(label = ..count..), position = position_dodge(width = 0.9), vjust = -0.25) + xlab("No of times 30 DPD or worse in last 12 months") + ylab("Count of Applicants") + ggtitle("No of times 30 DPD or worse in last 12 months")

#ggplot(data = master_data, aes(factor(`Presence of open home loan`))) + geom_bar(stat = "count") + geom_text(stat = 'count', aes(label = ..count..), position = position_dodge(width = 0.9), vjust = -0.25) + xlab("Presence of open home loan") + ylab("Count of Applicants") + ggtitle("Presence of open home loan")

#ggplot(data = master_data, aes(factor(`Presence of open auto loan`))) + geom_bar(stat = "count") + geom_text(stat = 'count', aes(label = ..count..), position = position_dodge(width = 0.9), vjust = -0.25) + xlab("Presence of open auto loan") + ylab("Count of Applicants") + ggtitle("Presence of open auto loan")

######################################### Uni-Variate Analysis #########################################

#bar_theme1 <- theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),legend.position = "none")

#plot_grid(#ggplot(master_data, aes(x = Gender, fill = factor('Performance Tag'))) + geom_bar() + bar_theme1,
          #ggplot(master_data, aes(x = 'Marital Status', fill = factor('Performance Tag'))) + geom_bar() + bar_theme1,
          #ggplot(master_data, aes(x = Education, fill = factor('Performance Tag'))) + geom_bar() + bar_theme1,
          #align = "h")

#plot_grid(#ggplot(master_data, aes(x = Profession, fill = factor('Performance Tag'))) + geom_bar() + bar_theme1,
          #ggplot(master_data, aes(x = `Type of residence`, fill = factor('Performance Tag'))) + geom_bar() + bar_theme1,
          #ggplot(master_data, aes(x = `No of dependents`, fill = factor('Performance Tag'))) + geom_bar() + bar_theme1,
          #align = "h")


#plot_grid(#ggplot(master_data, aes(x = `No of times 90 DPD or worse in last 6 months`, fill = factor('Performance Tag'))) + geom_bar() + bar_theme1,
          #ggplot(master_data, aes(x = `No of times 60 DPD or worse in last 6 months`, fill = factor('Performance Tag'))) + geom_bar() + bar_theme1,
          #ggplot(master_data, aes(x = `No of times 30 DPD or worse in last 6 months`, fill = factor('Performance Tag'))) + geom_bar() + bar_theme1,
          #align = "h")

#plot_grid(#ggplot(master_data, aes(x = `No of times 90 DPD or worse in last 12 months`, fill = factor('Performance Tag'))) + geom_bar() + bar_theme1,
          #ggplot(master_data, aes(x = `No of times 60 DPD or worse in last 12 months`, fill = factor('Performance Tag'))) + geom_bar() + bar_theme1,
          #ggplot(master_data, aes(x = `No of times 30 DPD or worse in last 12 months`, fill = factor('Performance Tag'))) + geom_bar() + bar_theme1,
          #align = "h")


#plot_grid(#ggplot(master_data, aes(x = factor(`Presence of open home loan`), fill = factor('Performance Tag'))) + geom_bar() + bar_theme1,
          #ggplot(master_data, aes(x = factor(`Presence of open auto loan`), fill = factor('Performance Tag'))) + geom_bar() + bar_theme1,
          #align = "h")

#*****************************************************************************************************************


##************************** IV &WOE Analysis ******************###

# Change the levels to "good " and "bad"
master_data$`Performance Tag` <- ifelse(master_data$`Performance Tag`==1,"bad","good") 
# Rechange the levels to "1" for good customer and "0" for bad customers (This change is only for this package)
master_data$`Performance Tag`<- ifelse(master_data$`Performance Tag`=="good",1,0)

prop.table(table(master_data$`Performance Tag`)) # So, the performance tags are reversed .

# Information Value
#?create_infotables
iv <- create_infotables(data = master_data[,-1], y = "Performance Tag" , parallel = TRUE)


# IV Plots
plot_infotables(iv,"Avgas CC Utilization in last 12 months")
plot_infotables(iv,"No of PL trades opened in last 12 months")
plot_infotables(iv,"No of Inquiries in last 12 months (excluding home & auto loans)")
plot_infotables(iv,"No of times 30 DPD or worse in last 6 months")
plot_infotables(iv,"No of PL trades opened in last 6 months")
plot_infotables(iv,"No of times 90 DPD or worse in last 12 months")
plot_infotables(iv,"Total No of Trades")
plot_infotables(iv,"No of Inquiries in last 6 months (excluding home & auto loans)")
plot_infotables(iv,"No of times 60 DPD or worse in last 6 months")
plot_infotables(iv,"No of times 30 DPD or worse in last 12 months")
plot_infotables(iv,"Outstanding Balance")
plot_infotables(iv,"No of trades opened in last 12 months")
plot_infotables(iv,"No of times 60 DPD or worse in last 12 months")
plot_infotables(iv,"No of times 90 DPD or worse in last 6 months")
plot_infotables(iv,"No of trades opened in last 6 months")
plot_infotables(iv,"Income")
plot_infotables(iv,"No of months in current residence")
plot_infotables(iv,"Presence of open home loan")
plot_infotables(iv,"No of months in current company")
plot_infotables(iv,"Age")
plot_infotables(iv,"No of dependents")
plot_infotables(iv,"Profession")
plot_infotables(iv,"Presence of open auto loan")
plot_infotables(iv,"Type of residence")
plot_infotables(iv,"Education")
plot_infotables(iv,"Gender")
plot_infotables(iv,"Marital Status")
iv$Tables


#iv_Value=data.frame(iv$Summary)
write.table(iv_Value, file = "iv_final.csv" , sep= ",")

#Checking the variables relationship based on IV

iv_Value=data.frame(iv$Summary)%>%mutate((Relationship=case_when(IV<0.02 ~ "Not useful",
                                                     IV>=0.02 & IV < 0.1  ~ "Weak",
                                                     IV>=0.1 & IV <0.3 ~ "Medium",
                                                     IV >=0.3 & IV <=0.5 ~ "Strong",
                                                     IV >0.5 ~ "Suspicious"))
                                                      )

#Based on the iv_Value , we can pick only those which are having strong/medium relationship with depedendent variable

#Creating master_woe by ignoring the application id column which is not necessary
master_woe <- DF.Replace.WOE(master_data[,-1],iv,"Performance Tag")


# Rechange the levels to "1" for bad customer and "0" for good customers 
master_woe$`Performance Tag`<- ifelse(master_woe$`Performance Tag`==1,0,1)

sum(master_woe$`Performance Tag`)


#Converting the dependent variable to factor
master_woe$`Performance Tag`<- as.factor(master_woe$`Performance Tag`)
levels(master_woe$`Performance Tag`)

#Changing the column names 

colnames(master_woe)[which(names(master_woe)=="No of times 90 DPD or worse in last 6 months:WOE")]<-"No_of_times_90_DPD_or_worse_in_last_6_months_WOE"
colnames(master_woe)[which(names(master_woe)=="No of times 60 DPD or worse in last 6 months:WOE")]<-"No_of_times_60_DPD_or_worse_in_last_6_months_WOE"
colnames(master_woe)[which(names(master_woe)=="No of times 30 DPD or worse in last 6 months:WOE")]<-"No_of_times_30_DPD_or_worse_in_last_6_months_WOE"
colnames(master_woe)[which(names(master_woe)=="No of times 90 DPD or worse in last 12 months:WOE")]<-"No_of_times_90_DPD_or_worse_in_last_12_months_WOE"
colnames(master_woe)[which(names(master_woe)=="No of times 60 DPD or worse in last 12 months:WOE")]<-"No_of_times_60_DPD_or_worse_in_last_12_months_WOE"
colnames(master_woe)[which(names(master_woe)=="No of times 30 DPD or worse in last 12 months:WOE")]<-"No_of_times_30_DPD_or_worse_in_last_12_months_WOE"
colnames(master_woe)[which(names(master_woe)=="Avgas CC Utilization in last 12 months:WOE")]<-"Avgas_CC_Utilization_in_last_12_months_WOE"
colnames(master_woe)[which(names(master_woe)=="No of trades opened in last 6 months:WOE")]<-"No_of_trades_opened_in_last_6_months_WOE"
colnames(master_woe)[which(names(master_woe)=="No of trades opened in last 12 months:WOE")]<-"No_of_trades_opened_in_last_12_months_WOE"
colnames(master_woe)[which(names(master_woe)=="No of PL trades opened in last 6 months:WOE")]<-"No_of_PL_trades_opened_in_last_6_months_WOE"
colnames(master_woe)[which(names(master_woe)=="No of PL trades opened in last 12 months:WOE")]<-"No_of_PL_trades_opened_in_last_12_months_WOE"
colnames(master_woe)[which(names(master_woe)=="No of Inquiries in last 6 months (excluding home & auto loans):WOE")]<-"No_of_Inquiries_in_last_6_months_excl_home_auto_loans_WOE"
colnames(master_woe)[which(names(master_woe)=="No of Inquiries in last 12 months (excluding home & auto loans):WOE")]<-"No_of_Inquiries_in_last_12_months_excl_home_auto_loans_WOE"
colnames(master_woe)[which(names(master_woe)=="Outstanding Balance:WOE")]<-"Outstanding_Balance_WOE"
colnames(master_woe)[which(names(master_woe)=="Total No of Trades:WOE")]<-"Total_No_of_Trades_WOE"
colnames(master_woe)[which(names(master_woe)=="Performance Tag")]<-"Performance_Tag"
colnames(master_woe)[which(names(master_woe)=="Age:WOE")]<-"Age_WOE"
colnames(master_woe)[which(names(master_woe)=="Gender:WOE")]<-"Gender_WOE"
colnames(master_woe)[which(names(master_woe)=="Marital Status:WOE")]<-"Marital_Status_WOE"
colnames(master_woe)[which(names(master_woe)=="No of dependents:WOE")]<-"No_of_dependents_WOE"
colnames(master_woe)[which(names(master_woe)=="Income:WOE")]<-"Income_WOE"
colnames(master_woe)[which(names(master_woe)=="Education:WOE")]<-"Education_WOE"
colnames(master_woe)[which(names(master_woe)=="Profession:WOE")]<-"Profession_WOE"
colnames(master_woe)[which(names(master_woe)=="Type of residence:WOE")]<-"Type_of_residence_WOE"
colnames(master_woe)[which(names(master_woe)=="No of months in current residence:WOE")]<-"No_of_months_in_current_residence_WOE"
colnames(master_woe)[which(names(master_woe)=="No of months in current company:WOE")]<-"No_of_months_in_current_company_WOE"
colnames(master_woe)[which(names(master_woe)=="Presence of open auto loan:WOE")]<-"Presence_of_open_auto_loan_WOE"
colnames(master_woe)[which(names(master_woe)=="Presence of open home loan:WOE")]<-"Presence_of_open_home_loan_WOE"

######################################################################################################################
###################################    Demographic Modelling   #######################################################
#######################################################################################################################

##################################   Demographic Data Logistic Regression ############################################

###Using the woe values, preparing model only with demographic data

demograph_woe <- master_woe[,c(1:10,28)]

# splitting the data between train and test
set.seed(100)

indices = sample.split(demograph_woe$Performance_Tag, SplitRatio = 0.7)

train_lr_demowoe = demograph_woe[indices,]

test_lr_demowoe = demograph_woe[!(indices),]

#First model
model_1 = glm(Performance_Tag ~ ., data = train_lr_demowoe, family = "binomial")
summary(model_1) 

# Stepwise selection
model_2<- stepAIC(model_1, direction="both")

summary(model_2)

# Removing multicollinearity through VIF check

vif(model_2)

#vif is almost same for all variables, 'age' is having low significance. So excluding that

model_3 = glm(Performance_Tag ~ No_of_dependents_WOE+Income_WOE+Profession_WOE 
              +No_of_months_in_current_residence_WOE+No_of_months_in_current_company_WOE,
              data = train_lr_demowoe, family = "binomial");

summary(model_3);
vif(model_3);

#vif is almost same for all variables, 'No_of_dependents_WOE is having low significance. So excluding that

model_4 = glm(Performance_Tag ~ Income_WOE+Profession_WOE 
              +No_of_months_in_current_residence_WOE+No_of_months_in_current_company_WOE,
              data = train_lr_demowoe, family = "binomial");

summary(model_4);
vif(model_4);

#vif is almost same for all variables, 'Profession_WOE  is having low significance. So excluding that

model_5 = glm(Performance_Tag ~ Income_WOE+No_of_months_in_current_residence_WOE+No_of_months_in_current_company_WOE,
              data = train_lr_demowoe, family = "binomial");

summary(model_5);
vif(model_5);

#All the 3 variables in the above model are highly significant variables.

final_logreg_demomodel <-model_5;

## Demographic Logistic Regression Model Evaluation

#predicted probabilities of default 1 for test data

test_pred = predict(final_logreg_demomodel, type = "response",newdata = test_lr_demowoe[,-1]);

# Checking the summary 

summary(test_pred)

test_actual_default <- factor(ifelse(test_lr_demowoe$Performance_Tag==1,"Yes","No"));

# Checking optimal probalility cutoff 

perform_fn <- function(cutoff) 
{
  predicted_default <- factor(ifelse(test_pred >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_default, test_actual_default, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}


# Summary of test probability

summary(test_pred);

s = seq(.01,.80,length=100)

OUT = matrix(0,100,3)


for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 


plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))


cutoff <- s[which((abs(OUT[,1]-OUT[,2]))==min(abs(OUT[,1]-OUT[,2])))]


# Using the Final cutoff obtained above

test_cutoff_default <- factor(ifelse(test_pred >=0.04192, "Yes", "No"));

conf_final <- confusionMatrix(test_cutoff_default, test_actual_default, positive = "Yes");

acc_demo_lr <- conf_final$overall[1];

sens_demo_lr <- conf_final$byClass[1];

spec_demo_lr <- conf_final$byClass[2];


### KS -statistic - Test Data ######

test_cutoff_default <- ifelse(test_cutoff_default=="Yes",1,0);
test_actual_default <- ifelse(test_actual_default=="Yes",1,0);


library(ROCR);
#on testing  data
pred_object_test<- prediction(test_cutoff_default, test_actual_default);

performance_measures_test<- performance(pred_object_test, "tpr", "fpr");

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]]);

ks_demo_lr <- max(ks_table_test);


#### ROC

eval <- performance(pred_object_test, "acc")
plot(eval)
abline(h = 0.96 , v = 0.094 , col = "red")

#Receiver Operating Characteristic Curve

plot(performance_measures_test , colorize = T ,
     main = "ROC Curve on Logistic Regression (Demographic Dataset)"  , 
     ylab = "True Positive Rate (Sensitivity)" , xlab = "False Positive Rate (1-Specificity)" )
abline(a = 0 , b =1 , col = "red")

# Area Under Curve
auc_demo_lr <- performance(pred_object_test , "auc")
auc_demo_lr <- unlist(slot(auc_demo_lr , "y.values"))
auc_demo_lr <- round(auc_demo_lr , 4)  # Area Under Curve is 0.5792

legend(0.6 , 0.4 ,auc_demo_lr, title = "AUC")

##################################################################################################################

####  Demographic Data Logistic Regression Model Result ###########################################


acc_demo_lr; #Accuracy = 0.5832575

sens_demo_lr; #Sensitivity =  0.5748299 

spec_demo_lr; #Specificity = 0.5836294 

ks_demo_lr #KS statistic: 0.1584593

auc_demo_lr #AUC =0.5792

##################################################################################################################

################################## Master Data Logistic Regression #################################################################

##Getting only columns whose iv_Values are 'strong' or 'medium' (i.e iv value between 0.1 and 0.5)
View(iv_Value);

#Below variables have the iv value between 0.1 and 0.5
#Avgas CC Utilization in last 12 months
#No of trades opened in last 12 months
#No of Inquiries in last 12 months (excluding home & auto loans)
#No of PL trades opened in last 12 months 
#Outstanding Balance  
#No of times 30 DPD or worse in last 6 months 
#Total No of Trades 
#No of PL trades opened in last 6 months
#No of times 30 DPD or worse in last 12 months
#No of times 90 DPD or worse in last 12 months  
#No of times 60 DPD or worse in last 6 months 
#No of Inquiries in last 6 months (excluding home & auto loans)
#No of trades opened in last 6 months 
#No of times 60 DPD or worse in last 12 months
#No of times 90 DPD or worse in last 6 months

#Observation : None of the variables from Demographic data are present in the above list.

###Using the woe values, preparing model for master data only for the above variables

master_woe_iv_based <- master_woe[,c(11:23,25,26,28)]

# splitting the data between train and test
set.seed(100)

indices = sample.split(master_woe_iv_based$Performance_Tag, SplitRatio = 0.7)

train_lr_woe = master_woe_iv_based[indices,]

test_lr_woe = master_woe_iv_based[!(indices),]


#First model
model_1 = glm(Performance_Tag ~ ., data = train_lr_woe, family = "binomial")
summary(model_1) 

# Stepwise selection
model_2<- stepAIC(model_1, direction="both")

summary(model_2);

# Removing multicollinearity through VIF check

vif(model_2);

#`No_of_times_30_DPD_or_worse_in_last_6_months_WOE `  has high vif value and low significance.
#So excluding that in the next step

model_3 <-glm(formula = Performance_Tag ~ No_of_times_90_DPD_or_worse_in_last_6_months_WOE + 
                No_of_times_90_DPD_or_worse_in_last_12_months_WOE + 
                No_of_times_60_DPD_or_worse_in_last_12_months_WOE + No_of_times_30_DPD_or_worse_in_last_12_months_WOE + 
                Avgas_CC_Utilization_in_last_12_months_WOE + No_of_trades_opened_in_last_12_months_WOE + 
                No_of_Inquiries_in_last_12_months_excl_home_auto_loans_WOE + 
                Outstanding_Balance_WOE + Total_No_of_Trades_WOE, family = "binomial", 
              data = train_lr_woe)

summary(model_3);

vif(model_3);


#No of times 60 DPD or worse in last 12 months:WOE`  has high value and low signifance and excluding that

model_4 <-glm(formula = Performance_Tag ~ No_of_times_90_DPD_or_worse_in_last_6_months_WOE + 
                No_of_times_90_DPD_or_worse_in_last_12_months_WOE + 
                No_of_times_30_DPD_or_worse_in_last_12_months_WOE + 
                Avgas_CC_Utilization_in_last_12_months_WOE + No_of_trades_opened_in_last_12_months_WOE + 
                No_of_Inquiries_in_last_12_months_excl_home_auto_loans_WOE + 
                Outstanding_Balance_WOE + Total_No_of_Trades_WOE, family = "binomial", 
              data = train_lr_woe)

summary(model_4);

vif(model_4);

#`No_of_trades_opened_in_last_12_months_WOE' has high vif and high significance 
# `Total_No_of_Trades_WOE` has high vif and low significance..so excluding that

model_5 <-glm(formula = Performance_Tag ~ No_of_times_90_DPD_or_worse_in_last_6_months_WOE + 
                No_of_times_90_DPD_or_worse_in_last_12_months_WOE + 
                No_of_times_30_DPD_or_worse_in_last_12_months_WOE + 
                Avgas_CC_Utilization_in_last_12_months_WOE + No_of_trades_opened_in_last_12_months_WOE + 
                No_of_Inquiries_in_last_12_months_excl_home_auto_loans_WOE + 
                Outstanding_Balance_WOE, family = "binomial", 
              data = train_lr_woe)


summary(model_5);

vif(model_5);

#No_of_times_90_DPD_or_worse_in_last_12_months_WOE  has high vif and low significance..excluding that

model_6 <-glm(formula = Performance_Tag ~ No_of_times_90_DPD_or_worse_in_last_6_months_WOE + 
                No_of_times_30_DPD_or_worse_in_last_12_months_WOE + 
                Avgas_CC_Utilization_in_last_12_months_WOE + No_of_trades_opened_in_last_12_months_WOE + 
                No_of_Inquiries_in_last_12_months_excl_home_auto_loans_WOE + 
                Outstanding_Balance_WOE, family = "binomial", 
              data = train_lr_woe)

summary(model_6);

vif(model_6);

#No_of_trades_opened_in_last_12_months_WOE  has high vif and low significance.so excluding that


model_7 <-glm(formula = Performance_Tag ~ No_of_times_90_DPD_or_worse_in_last_6_months_WOE + 
                No_of_times_30_DPD_or_worse_in_last_12_months_WOE + 
                Avgas_CC_Utilization_in_last_12_months_WOE +  
                No_of_Inquiries_in_last_12_months_excl_home_auto_loans_WOE + 
                Outstanding_Balance_WOE, family = "binomial", 
              data = train_lr_woe)
summary(model_7);

vif(model_7);

#No_of_times_90_DPD_or_worse_in_last_6_months_WOE  has high vif and low significance. So excluding that

model_8 <-glm(formula = Performance_Tag ~ No_of_times_30_DPD_or_worse_in_last_12_months_WOE + 
                Avgas_CC_Utilization_in_last_12_months_WOE +  
                No_of_Inquiries_in_last_12_months_excl_home_auto_loans_WOE + 
                Outstanding_Balance_WOE, family = "binomial", 
              data = train_lr_woe)

summary(model_8);

vif(model_8);

final_lr_model <- model_8;

### Model Evaluation

### Test Data ####

#predicted probabilities of default 1 for test data


test_pred = predict(final_lr_model, type = "response",newdata = test_lr_woe[,-1]);

test_actual_default <- factor(ifelse(test_lr_woe$Performance_Tag==1,"Yes","No"));

# Let's Choose the cutoff value. 

# Let's find out the optimal probalility cutoff 


s = seq(.01,.80,length=100)

OUT = matrix(0,100,3);


for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 


plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))


cutoff <- s[which((abs(OUT[,1]-OUT[,2]))==min(abs(OUT[,1]-OUT[,2])))];
View(cutoff);

# Choosing the above cutoff for final model

test_cutoff_default <- factor(ifelse(test_pred >=0.0498, "Yes", "No"));
levels(test_cutoff_default)
summary(test_cutoff_default)

conf_final <- confusionMatrix(test_cutoff_default, test_actual_default, positive = "Yes");

acc_master_lr <- conf_final$overall[1];

sens_master_lr <- conf_final$byClass[1];

spec_master_lr <- conf_final$byClass[2];

### KS -statistic - Test Data ######

test_cutoff_default <- ifelse(test_cutoff_default=="Yes",1,0);
test_actual_default <- ifelse(test_actual_default=="Yes",1,0);

summary(test_cutoff_default)
summary(test_actual_default)

library(ROCR);
#on testing  data
pred_object_test<- prediction(test_cutoff_default, test_actual_default);

performance_measures_test<- performance(pred_object_test, "tpr", "fpr");

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]]);

ks_master_lr <- max(ks_table_test);

### ROC

eval <- performance(pred_object_test, "acc")
plot(eval)
abline(h = 0.96 , v = 0.094 , col = "red")

#Receiver Operating Characteristic Curve

plot(performance_measures_test , colorize = T ,
     main = "ROC Curve on Logistic Regression (Master Dataset)"  , 
     ylab = "True Positive Rate (Sensitivity)" , xlab = "False Positive Rate (1-Specificity)" )
abline(a = 0 , b =1 , col = "red")

# Area Under Curve
auc_master_lr <- performance(pred_object_test , "auc")
auc_master_lr <- unlist(slot(auc_master_lr , "y.values"))
auc_master_lr <- round(auc_master_lr , 4)  # Area Under Curve is  0.6398

legend(0.6 , 0.4 ,auc_demo_lr, title = "AUC")


##################################################################################################################

##  Master Data Logistic Regression Model Result ###############


acc_master_lr; #Accuracy = 0.6494322

sens_master_lr; #Sensitivity =   0.6292517   

spec_master_lr; #Specificity = 0.6503227 

auc_master_lr # AUC = 0.6398

ks_master_lr #KS statistic= 0.2795744


#This is an improvement over the demographic data model.

#####################################################################################################################



################################## Master Data Random Forest ###########################################################


# splitting the data between train and test
set.seed(100)

indices = sample.split(master_woe_iv_based$Performance_Tag, SplitRatio = 0.7)

train_rf_woe = master_woe_iv_based[indices,]

test_rf_woe = master_woe_iv_based[!(indices),]

#Cutoff parameters obtained by trying out various values and verifying the model performance 

data.rf <- randomForest(Performance_Tag ~ ., data=train_rf_woe, proximity=FALSE,ntree=200, mtry=5, do.trace=TRUE, na.action=na.omit,cutoff=c(0.96,0.04))

test_pred <- predict(data.rf, newdata=test_rf_woe,type="prob")[,2]

table(test_pred, test_rf_woe$Performance_Tag)


test_actual_default <- factor(ifelse(test_rf_woe$Performance_Tag==1,"Yes","No"));

levels(test_actual_default)

predicted_default <- factor(ifelse(test_pred >=0.04, "Yes", "No"))

conf_final <- confusionMatrix(predicted_default, test_actual_default, positive = "Yes");

acc_master_rf <- conf_final$overall[1];

sens_master_rf <- conf_final$byClass[1];

spec_master_rf <- conf_final$byClass[2];

### KS -statistic - Test Data ######

test_pred_default <- ifelse(predicted_default=="Yes",1,0);
test_actual_default <- ifelse(test_actual_default=="Yes",1,0);

#on testing  data
pred_object_test<- prediction(test_pred_default, test_actual_default);

performance_measures_test<- performance(pred_object_test, "tpr", "fpr");

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]]);

ks_master_rf <- max(ks_table_test);


### ROC

eval <- performance(pred_object_test, "acc")
plot(eval)
abline(h = 0.96 , v = 0.094 , col = "red")

#Receiver Operating Characteristic Curve

plot(performance_measures_test , colorize = T ,
     main = "ROC Curve on Logistic Regression (Master Dataset)"  , 
     ylab = "True Positive Rate (Sensitivity)" , xlab = "False Positive Rate (1-Specificity)" )
abline(a = 0 , b =1 , col = "red")

# Area Under Curve
auc_master_rf <- performance(pred_object_test , "auc")
auc_master_rf <- unlist(slot(auc_master_rf , "y.values"))
auc_master_rf <- round(auc_master_rf, 4)  
legend(0.6 , 0.4 ,auc_master_rf, title = "AUC")

###################################################################################################

####  Master Data Random Forest Model Result ###############


acc_master_rf; #Accuracy = 0.6412382   

sens_master_rf; #Sensitivity =    0.5498866     

spec_master_rf; #Specificity =   0.6452694  

auc_master_rf #AC :  0.5976

ks_master_rf #kS statistic :  0.195156

###################################################################################################

######################## Balancing using SMOTE ###################################################

train_smote <- SMOTE(Performance_Tag~. ,data = train_lr_woe, perc.over = 100, perc.under = 200 )
#View (train_smote)
str(train_smote)


######################## Logistic Regression on SMOTE data ###################################################

model_smote_1 <- glm(Performance_Tag~. , data = train_smote , family = "binomial")
summary(model_smote_1) 

# Stepwise selection
model_smote_2<- stepAIC(model_smote_1, direction="both")
summary(model_smote_2)

# Removing multicollinearity through VIF check

vif(model_smote_2)

#'No_of_times_60_DPD_or_worse_in_last_12_months_WOE' has high vif and low significance..removing that
#'
model_smote_3 <-glm(formula = Performance_Tag ~ No_of_times_90_DPD_or_worse_in_last_6_months_WOE + 
                      No_of_times_30_DPD_or_worse_in_last_6_months_WOE + No_of_times_90_DPD_or_worse_in_last_12_months_WOE + 
                      Avgas_CC_Utilization_in_last_12_months_WOE + 
                      No_of_trades_opened_in_last_12_months_WOE + No_of_Inquiries_in_last_12_months_excl_home_auto_loans_WOE + 
                      Outstanding_Balance_WOE + Total_No_of_Trades_WOE, family = "binomial", 
                    data = train_smote)

summary(model_smote_3)

vif(model_smote_3)

#'No_of_times_90_DPD_or_worse_in_last_6_months_WOE' has high vif and low signifance. removing that
#'

model_smote_4 <-glm(formula = Performance_Tag ~ No_of_times_30_DPD_or_worse_in_last_6_months_WOE + No_of_times_90_DPD_or_worse_in_last_12_months_WOE + 
                      Avgas_CC_Utilization_in_last_12_months_WOE + 
                      No_of_trades_opened_in_last_12_months_WOE + No_of_Inquiries_in_last_12_months_excl_home_auto_loans_WOE + 
                      Outstanding_Balance_WOE + Total_No_of_Trades_WOE, family = "binomial", 
                    data = train_smote)

summary(model_smote_4)

vif(model_smote_4)

#' Total_No_of_Trades_WOE' has high vif and low significance. removing that
#'
model_smote_5 <-glm(formula = Performance_Tag ~ No_of_times_30_DPD_or_worse_in_last_6_months_WOE + No_of_times_90_DPD_or_worse_in_last_12_months_WOE + 
                      Avgas_CC_Utilization_in_last_12_months_WOE + 
                      No_of_trades_opened_in_last_12_months_WOE + No_of_Inquiries_in_last_12_months_excl_home_auto_loans_WOE + 
                      Outstanding_Balance_WOE , family = "binomial", 
                    data = train_smote)

summary(model_smote_5)

vif(model_smote_5)

#Removing No_of_times_30_DPD_or_worse_in_last_6_months_WOE '

model_smote_6 <-glm(formula = Performance_Tag ~ No_of_times_90_DPD_or_worse_in_last_12_months_WOE + 
                      Avgas_CC_Utilization_in_last_12_months_WOE + 
                      No_of_trades_opened_in_last_12_months_WOE + No_of_Inquiries_in_last_12_months_excl_home_auto_loans_WOE + 
                      Outstanding_Balance_WOE , family = "binomial", 
                    data = train_smote)

summary(model_smote_6)

vif(model_smote_6)


#Removing Outstanding_Balance_WOE

model_smote_7 <-glm(formula = Performance_Tag ~ No_of_times_90_DPD_or_worse_in_last_12_months_WOE + 
                      Avgas_CC_Utilization_in_last_12_months_WOE + 
                      No_of_trades_opened_in_last_12_months_WOE + No_of_Inquiries_in_last_12_months_excl_home_auto_loans_WOE 
                       , family = "binomial", 
                    data = train_smote)

summary(model_smote_7)

vif(model_smote_7)

final_smote_model <- model_smote_7

#Model Evaluation on SMOTE Train Data

test_pred = predict(final_smote_model, type = "response",newdata = train_smote[,-1])

test_actual_default <- factor(ifelse(train_smote$Performance_Tag==1,"Yes","No"))


s = seq(.01,.80,length=100)

OUT = matrix(0,100,3);


for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 


plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

cutoff <- s[which((abs(OUT[,1]-OUT[,2]))==min(abs(OUT[,1]-OUT[,2])))];
#View(cutoff);

# Let's choose a cutoff value of 0.3132 for final model

test_cutoff_default <- factor(ifelse(test_pred >=0.54, "Yes", "No"));

conf_final <- confusionMatrix(test_cutoff_default, test_actual_default, positive = "Yes")

acc_train_smote_lr <- conf_final$overall[1];

sens_train_smote_lr <- conf_final$byClass[1];

spec_train_smote_lr <- conf_final$byClass[2];

### KS -statistic - Test Data ######

test_cutoff_default <- ifelse(test_cutoff_default=="Yes",1,0);
test_actual_default <- ifelse(test_actual_default=="Yes",1,0);

#on testing  data
pred_object_test<- prediction(test_cutoff_default, test_actual_default);

performance_measures_test<- performance(pred_object_test, "tpr", "fpr");

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]]);

ks_train_smote_lr <- max(ks_table_test);

### ROC

eval <- performance(pred_object_test, "acc")
plot(eval)
abline(h = 0.96 , v = 0.094 , col = "red")

#Receiver Operating Characteristic Curve

plot(performance_measures_test , colorize = T ,
     main = "ROC Curve on Logistic Regression (Master Dataset)"  , 
     ylab = "True Positive Rate (Sensitivity)" , xlab = "False Positive Rate (1-Specificity)" )
abline(a = 0 , b =1 , col = "red")

# Area Under Curve
auc_train_smote_lr <- performance(pred_object_test , "auc")
auc_train_smote_lr <- unlist(slot(auc_train_smote_lr , "y.values"))
auc_train_smote_lr <- round(auc_train_smote_lr, 4)  # Area Under Curve is  0.6376

legend(0.6 , 0.4 ,auc_train_smote_lr, title = "AUC")


###################################################################################################

####  Statitisc after testing against Train SMOTE Data ###############


acc_train_smote_lr ; #Accuracy =0.6256684 

sens_train_smote_lr; #Sensitivity =  0.6191055    

spec_train_smote_lr; #Specificity = 0.6322314  

auc_train_smote_lr # AUC:  0.6257

ks_train_smote_lr # 0.2513369


#Almost same as logistic regression model for master data 

######################################################################################

#Model evaluation on Test Data

test_pred = predict(final_smote_model, type = "response",newdata = test_lr_woe[,-1])

test_actual_default <- factor(ifelse(test_lr_woe$Performance_Tag==1,"Yes","No"))


s = seq(.01,.80,length=100)

OUT = matrix(0,100,3);


for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 


plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

cutoff <- s[which((abs(OUT[,1]-OUT[,2]))==min(abs(OUT[,1]-OUT[,2])))];
#View(cutoff);

# Let's choose a cutoff value of 0.3132 for final model

test_cutoff_default <- factor(ifelse(test_pred >=0.54, "Yes", "No"));

conf_final <- confusionMatrix(test_cutoff_default, test_actual_default, positive = "Yes")

acc_test_lr <- conf_final$overall[1];

sens_test_lr<- conf_final$byClass[1];

spec_test_lr <- conf_final$byClass[2];

### KS -statistic - Test Data ######

test_cutoff_default <- ifelse(test_cutoff_default=="Yes",1,0);
test_actual_default <- ifelse(test_actual_default=="Yes",1,0);

#on testing  data
pred_object_test<- prediction(test_cutoff_default, test_actual_default);

performance_measures_test<- performance(pred_object_test, "tpr", "fpr");

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]]);

ks_test_lr <- max(ks_table_test);

### ROC

eval <- performance(pred_object_test, "acc")
plot(eval)
abline(h = 0.96 , v = 0.094 , col = "red")

#Receiver Operating Characteristic Curve

plot(performance_measures_test , colorize = T ,
     main = "ROC Curve on Logistic Regression (Master Dataset)"  , 
     ylab = "True Positive Rate (Sensitivity)" , xlab = "False Positive Rate (1-Specificity)" )
abline(a = 0 , b =1 , col = "red")

# Area Under Curve
auc_test_lr <- performance(pred_object_test , "auc")
auc_test_lr <- unlist(slot(auc_test_lr , "y.values"))
auc_test_lr <- round(auc_test_lr, 4)  # Area Under Curve is  0.6376

legend(0.6 , 0.4 ,auc_test_lr, title = "AUC")

###################################################################################################

####  Statitisc after testing against Test Data ###############


acc_test_lr ; #Accuracy =0.6427716  

sens_test_lr; #Sensitivity =   0.6190476    

spec_test_lr; #Specificity = 0.6438185   

auc_test_lr # AUC:  0.6314

ks_test_lr # 0.2628661


######################################################################################

#Model evaluation on Master Data

test_pred = predict(final_smote_model, type = "response",newdata = master_woe_iv_based[,-1])

test_actual_default <- factor(ifelse(master_woe_iv_based$Performance_Tag==1,"Yes","No"))


s = seq(.01,.80,length=100)

OUT = matrix(0,100,3);


for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 


plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

cutoff <- s[which((abs(OUT[,1]-OUT[,2]))==min(abs(OUT[,1]-OUT[,2])))];
#View(cutoff);

# Chosing a cut off value 

test_cutoff_default <- factor(ifelse(test_pred >=0.52, "Yes", "No"));

conf_final <- confusionMatrix(test_cutoff_default, test_actual_default, positive = "Yes")

acc_master_lr <- conf_final$overall[1];

sens_master_lr<- conf_final$byClass[1];

spec_master_lr <- conf_final$byClass[2];

### KS -statistic - Test Data ######

test_cutoff_default <- ifelse(test_cutoff_default=="Yes",1,0);
test_actual_default <- ifelse(test_actual_default=="Yes",1,0);

#on testing  data
pred_object_test<- prediction(test_cutoff_default, test_actual_default);

performance_measures_test<- performance(pred_object_test, "tpr", "fpr");

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]]);

ks_master_lr <- max(ks_table_test);

### ROC

eval <- performance(pred_object_test, "acc")
plot(eval)
abline(h = 0.96 , v = 0.094 , col = "red")

#Receiver Operating Characteristic Curve

plot(performance_measures_test , colorize = T ,
     main = "ROC Curve on Logistic Regression (Master Dataset)"  , 
     ylab = "True Positive Rate (Sensitivity)" , xlab = "False Positive Rate (1-Specificity)" )
abline(a = 0 , b =1 , col = "red")

# Area Under Curve
auc_master_lr <- performance(pred_object_test , "auc")
auc_master_lr <- unlist(slot(auc_master_lr , "y.values"))
auc_master_lr <- round(auc_master_lr, 4)  # Area Under Curve is  0.6376

legend(0.6 , 0.4 ,auc_master_lr, title = "AUC")

###################################################################################################

####  Statitisc after testing against Master Data ###############


acc_master_lr ; #Accuracy =0.5990253    

sens_master_lr; #Sensitivity =   0.6699558     

spec_master_lr; #Specificity = 0.5958964    

auc_master_lr # AUC:   0.6145

ks_master_lr # 0.2658521


############################################################################################

################################# Decision Tree #######################

tree.model <- rpart(Performance_Tag ~ ., data=train_smote, method = "class",                         # classification or regression
                                       control = rpart.control(minsplit = 750,     # min observations for node
                                         minbucket = 750,    # min observations for leaf node
                                         cp = 0.001))  

prp(tree.model)

tree.predict <- predict(tree.model, master_woe_iv_based, type = "class")
View(tree.predict)

# evaluate the results
conf_final<- confusionMatrix(tree.predict, master_woe_iv_based$Performance_Tag, positive = "1") 

acc_smote_master_dt <- conf_final$overall[1];

sens_smote_master_dt <- conf_final$byClass[1];

spec_smote_master_dt <- conf_final$byClass[2];

### KS -statistic - Test Data ######

#on testing  data
pred_object_test<- prediction(tree.predict, master_woe_iv_based$Performance_Tag);

performance_measures_test<- performance(pred_object_test, "tpr", "fpr");

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]]);

ks_smote_master_dt <- max(ks_table_test);

### ROC

eval <- performance(pred_object_test, "acc")
plot(eval)
abline(h = 0.96 , v = 0.094 , col = "red")

#Receiver Operating Characteristic Curve

plot(performance_measures_test , colorize = T ,
     main = "ROC Curve on Logistic Regression (Master Dataset)"  , 
     ylab = "True Positive Rate (Sensitivity)" , xlab = "False Positive Rate (1-Specificity)" )
abline(a = 0 , b =1 , col = "red")

# Area Under Curve
auc_smote_master_dt <- performance(pred_object_test , "auc")
auc_smote_master_dt<- unlist(slot(auc_smote_master_rf , "y.values"))
auc_smote_master_dt<- round(auc_smote_master_rf, 4)  

legend(0.6 , 0.4 ,auc_smote_master_dt ,title = "AUC")

####  SMOTE Data Decision Tree Model Result on Master Data set as the test ###############


acc_smote_master_dt; #Accuracy = 0.6196685     

sens_smote_master_dt; #Sensitivity =   0.6070092       

spec_smote_master_dt; #Specificity =   0.6202269      

auc_smote_master_dt #AC : 0.7688

ks_smote_master_dt #kS statistic : 0.537599

###############################################################################################################

#######               Random Forest on SMOTE data ############################


data.rf <- randomForest(Performance_Tag ~ ., data=train_smote, proximity=FALSE,ntree=500, mtry=5, do.trace=TRUE, na.action=na.omit)

test_pred <- predict(data.rf, newdata=train_lr_woe,type="prob")[,2]

# Let's see the summary 

test_actual_default <- factor(ifelse(train_lr_woe$Performance_Tag==1,"Yes","No"));

levels(test_actual_default)

predicted_default <- factor(ifelse(test_pred >=0.5, "Yes", "No"))

conf_final <- confusionMatrix(predicted_default, test_actual_default, positive = "Yes");

acc_smote_train_rf <- conf_final$overall[1];

sens_smote_train_rf <- conf_final$byClass[1];

spec_smote_train_rf <- conf_final$byClass[2];

### KS -statistic - Test Data ######

test_pred_default <- ifelse(predicted_default=="Yes",1,0);
test_actual_default <- ifelse(test_actual_default=="Yes",1,0);

#on testing  data
pred_object_test<- prediction(test_pred_default, test_actual_default);

performance_measures_test<- performance(pred_object_test, "tpr", "fpr");

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]]);

ks_smote_train_rf <- max(ks_table_test);

### ROC

eval <- performance(pred_object_test, "acc")
plot(eval)
abline(h = 0.96 , v = 0.094 , col = "red")

#Receiver Operating Characteristic Curve

plot(performance_measures_test , colorize = T ,
     main = "ROC Curve on Logistic Regression (Master Dataset)"  , 
     ylab = "True Positive Rate (Sensitivity)" , xlab = "False Positive Rate (1-Specificity)" )
abline(a = 0 , b =1 , col = "red")

# Area Under Curve
auc_smote_train_rf <- performance(pred_object_test , "auc")
auc_smote_train_rf <- unlist(slot(auc_smote_train_rf , "y.values"))
auc_smote_train_rf <- round(auc_smote_train_rf, 4)  # Area Under Curve is  0.6376

legend(0.6 , 0.4 ,auc_smote_train_rf, title = "AUC")

###################################################################################################

####  SMOTE Data Random Forest Model Result on Train Data ###############


acc_smote_train_rf; #Accuracy = 0.7989485  

sens_smote_train_rf; #Sensitivity =  0.8949927     

spec_smote_train_rf; #Specificity = 0.7947124   

auc_smote_train_rf #AC : 0.8449

ks_smote_train_rf #kS statistic : 0.6897051

############## Random Forest SMOTE data Testing  on Test data##############################

test_pred <- predict(data.rf, newdata=test_lr_woe,type="prob")[,2]

# Let's see the summary 

test_actual_default <- factor(ifelse(test_lr_woe$Performance_Tag==1,"Yes","No"));

levels(test_actual_default)

predicted_default <- factor(ifelse(test_pred >=0.5, "Yes", "No"))

conf_final <- confusionMatrix(predicted_default, test_actual_default, positive = "Yes");

acc_smote_test_rf <- conf_final$overall[1];

sens_smote_test_rf <- conf_final$byClass[1];

spec_smote_test_rf <- conf_final$byClass[2];

### KS -statistic - Test Data ######

test_pred_default <- ifelse(predicted_default=="Yes",1,0);
test_actual_default <- ifelse(test_actual_default=="Yes",1,0);

#on testing  data
pred_object_test<- prediction(test_pred_default, test_actual_default);

performance_measures_test<- performance(pred_object_test, "tpr", "fpr");

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]]);

ks_smote_test_rf <- max(ks_table_test);

### ROC

eval <- performance(pred_object_test, "acc")
plot(eval)
abline(h = 0.96 , v = 0.094 , col = "red")

#Receiver Operating Characteristic Curve

plot(performance_measures_test , colorize = T ,
     main = "ROC Curve on Logistic Regression (Master Dataset)"  , 
     ylab = "True Positive Rate (Sensitivity)" , xlab = "False Positive Rate (1-Specificity)" )
abline(a = 0 , b =1 , col = "red")

# Area Under Curve
auc_smote_test_rf <- performance(pred_object_test , "auc")
auc_smote_test_rf <- unlist(slot(auc_smote_test_rf , "y.values"))
auc_smote_test_rf <- round(auc_smote_test_rf, 4)  # Area Under Curve is  0.6376

legend(0.6 , 0.4 ,auc_smote_test_rf, title = "AUC")

####  SMOTE Data Random Forest Model Result ###############


acc_smote_test_rf; #Accuracy = 0.7615602    

sens_smote_test_rf; #Sensitivity =  0.3662132     

spec_smote_test_rf; #Specificity =  0.7790064    

auc_smote_test_rf #AC : 0.5726

ks_smote_test_rf #kS statistic : 0.1452195


############## Random Forest SMOTE data Testing  on Master data##############################

test_pred <- predict(data.rf, newdata=master_woe_iv_based,type="prob")

# Let's see the summary 


test_actual_default <- factor(ifelse(master_woe_iv_based$Performance_Tag==1,"Yes","No"));

levels(test_actual_default)

predicted_default <- factor(ifelse(test_pred[,2]>=0.5, "Yes", "No"))

table (predicted_default, test_actual_default)

conf_final <- confusionMatrix(predicted_default, test_actual_default, positive = "Yes");

acc_smote_master_rf <- conf_final$overall[1];

sens_smote_master_rf <- conf_final$byClass[1];

spec_smote_master_rf <- conf_final$byClass[2];

### KS -statistic - Test Data ######

test_pred_default <- ifelse(predicted_default=="Yes",1,0);
test_actual_default <- ifelse(test_actual_default=="Yes",1,0);

#on testing  data
pred_object_test<- prediction(test_pred_default, test_actual_default);

performance_measures_test<- performance(pred_object_test, "tpr", "fpr");

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]]);

ks_smote_master_rf <- max(ks_table_test);

### ROC

eval <- performance(pred_object_test, "acc")
plot(eval)
abline(h = 0.96 , v = 0.094 , col = "red")

#Receiver Operating Characteristic Curve

plot(performance_measures_test , colorize = T ,
     main = "ROC Curve on Logistic Regression (Master Dataset)"  , 
     ylab = "True Positive Rate (Sensitivity)" , xlab = "False Positive Rate (1-Specificity)" )
abline(a = 0 , b =1 , col = "red")

# Area Under Curve
auc_smote_master_rf <- performance(pred_object_test , "auc")
auc_smote_master_rf <- unlist(slot(auc_smote_master_rf , "y.values"))
auc_smote_master_rf <- round(auc_smote_master_rf, 4)  

legend(0.6 , 0.4 ,auc_smote_master_rf, title = "AUC")

####  SMOTE Data Random Forest Model Result on Master Data set as the test ###############

acc_smote_master_rf; #Accuracy = 0.7889395     

sens_smote_master_rf; #Sensitivity =  0.7352841       

spec_smote_master_rf; #Specificity =  0.7913064     

auc_smote_master_rf #AC : 0.7633

ks_smote_master_rf #kS statistic : 0.5265905



####  Application scorecard ###############

app_scorecard <- data.frame("Application_ID" =master_data$`Application ID`,
                            "prob_good"= test_pred[,1],
                            "prob_bad" =test_pred[,2],
                            "log_odds" = log(test_pred[,1]/test_pred[,2]),
                            "Performance_Tag"=master_woe_iv_based$Performance_Tag)



summary(app_scorecard)

#Below information is given in the case study

PDO=20
base_score=400

#Derived values 
factor_val = PDO/log(2) #28.85
offset = base_score - (factor_val * log(10)) #333.5614

app_scorecard$score = offset + (factor_val * app_scorecard$log_odds)

# Rounding to the near integer
app_scorecard$score <- round(app_scorecard$score,0)
summary(app_scorecard$score)

nrow(app_scorecard[which(app_scorecard$score == "Inf"),]) #There are 1948 rows with score as "Inf". This was because the prob of good was zero

#Setting the scorecard "Inf" to maximum value

app_scorecard$score[which(app_scorecard$score=="Inf")] <- max(app_scorecard$score[which(app_scorecard$score != Inf)])

#Plotting histogram of appscorecard

ggplot(app_scorecard , aes(app_scorecard$score))+geom_histogram(col = "blue", fill = "yellow")

#cutoff score

cutoff_Score <- app_scorecard$score[which(app_scorecard$prob_good==0.5)][1] #Cutoff score = 334

#Customers default percentage as per the model

(nrow(app_scorecard[which(app_scorecard$score < 334),]) / nrow(app_scorecard))*100 # 23%

#Percentage of defaulters correctly predicted
length(which(app_scorecard$score < 334 & app_scorecard$Performance_Tag == 1))*100 /length(which(app_scorecard$Performance_Tag == 1)) #74%

#Percentage of nondefaulters correctly predicted
length(which(app_scorecard$score >= 334 & app_scorecard$Performance_Tag == 0))*100 /length(which(app_scorecard$Performance_Tag == 0)) #79%


max(app_scorecard$score) #513
min(app_scorecard$score) #242

# Financial Benefit analysis :

conf_final # taking this confusion matrix as the final confusion matrix for the Financial Benefit Analysis

              #Reference
#Prediction    No   Yes
#        No  52626   775
#       Yes  13998   2164
                        


             