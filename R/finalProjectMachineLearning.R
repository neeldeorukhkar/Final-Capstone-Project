library(plyr)
library(dplyr)
library(tidyverse)
library(tibble)
library(lubridate)
library(ggplot2)
library(caret)
library(eeptools)
library(repr)

setwd("C:/Users/DeorukhkarN/OneDrive - Kantar/Attachments/Principles-of-Machine-Learning-R/Final-Capstone-Project/R")


##################################Actual Sales#########################################

#Load Data to DataFrame
AdvWorksCusts <- read.csv("AdvWorksCusts.csv", header = TRUE, stringsAsFactors = FALSE)
AW_AveMonthSpend <- read.csv("AW_AveMonthSpend.csv", header = TRUE, stringsAsFactors = FALSE)
AW_BikeBuyer <- read.csv("AW_BikeBuyer.csv", header = TRUE, stringsAsFactors = FALSE)

#Check summary statistics and structure of dataset
AdvWorksCusts <- as_tibble(AdvWorksCusts)
AW_AveMonthSpend <- as_tibble(AW_AveMonthSpend)
AW_BikeBuyer <- as_tibble(AW_BikeBuyer)

glimpse(AdvWorksCusts)
glimpse(AW_AveMonthSpend)
glimpse(AW_BikeBuyer)

handleDupRecordsFunc <- function(df, x){
  df <- unique(df)
  df <- df[order(df$CustomerID, -abs(df[,x])), ]
  df <- df[!duplicated(df$CustomerID), ] 
  return(df)
}

age <- function(dob, age.day = today(), units = "years", floor = TRUE) {
  calc.age = interval(dob, age.day) / duration(num = 1, units = units)
  if (floor) return(as.integer(floor(calc.age)))
  return(calc.age)
}

AdvWorksCusts <- handleDupRecordsFunc(AdvWorksCusts, "YearlyIncome")
AW_AveMonthSpend <- handleDupRecordsFunc(AW_AveMonthSpend, "AveMonthSpend")
AW_BikeBuyer <- handleDupRecordsFunc(AW_BikeBuyer, "BikeBuyer")

finalAdvWorksCusts <- plyr::join(AdvWorksCusts, AW_AveMonthSpend, by = "CustomerID", type = "left")
finalAdvWorksCusts <- plyr::join(finalAdvWorksCusts, AW_BikeBuyer, by = "CustomerID", type = "left")

#Drop Not needed Columns
finalAdvWorksCusts$PhoneNumber = NULL
finalAdvWorksCusts$Title = NULL
finalAdvWorksCusts$FirstName = NULL
finalAdvWorksCusts$MiddleName = NULL
finalAdvWorksCusts$LastName = NULL
finalAdvWorksCusts$Suffix = NULL
finalAdvWorksCusts$AddressLine1 = NULL
finalAdvWorksCusts$AddressLine2 = NULL
finalAdvWorksCusts$City = NULL
finalAdvWorksCusts$StateProvinceName = NULL
finalAdvWorksCusts$PostalCode = NULL


#Check missing value in Columns.
sapply(finalAdvWorksCusts, function(x) {sum(is.na(x))})

numcols <- c('HomeOwnerFlag', 'NumberCarsOwned', 'NumberChildrenAtHome', 'TotalChildren', 'YearlyIncome')
for(col in numcols){
  temp = finalAdvWorksCusts[,col]
  finalAdvWorksCusts[,col] = ifelse(temp == '?', NA, finalAdvWorksCusts[,col])
}

#####################Data Exploration and Assignment Questions####################################

#Assignment Questions
summary(finalAdvWorksCusts)
sd(finalAdvWorksCusts$AveMonthSpend)

finalAdvWorksCusts %>% group_by(Occupation) %>% summarise(medIncome = median(YearlyIncome)) %>% arrange(medIncome)

finalAdvWorksCusts$BirthDate <- as.Date(finalAdvWorksCusts$BirthDate, format = "%Y-%m-%d")
glimpse(finalAdvWorksCusts)
finalAdvWorksCusts$age <- age(finalAdvWorksCusts$BirthDate, age.day = "1998-01-01")
finalAdvWorksCusts$BirthDate = NULL

#for(col in colnames(finalAdvWorksCusts)){
#  if(is.character(finalAdvWorksCusts[, col])){
#    cat('\n')
#    cat(paste('Frequency table for', col))
#    print(table(finalAdvWorksCusts[, col]))
#  }
#}

#Assignment Question 8 - Based on their age at the time when the data was collected (1st January 1998),which group of customers accounts for the highest AveMonthSpend
numcols = c('age')
plot_scatter_cl = function(df, cols, col_y = 'AveMonthSpend', alpha = 1.0){
  options(repr.plot.width=5, repr.plot.height=3.5) # Set the initial plot area dimensions
  for(col in cols){
    p = ggplot(df, aes_string(col, col_y)) +
      geom_density_2d() +
      geom_point(aes(color = Gender), alpha = alpha) +
      ggtitle(paste('Scatter plot of', col_y, 'vs.', col,
                    '\n and color by Gender'))
    print(p)
  }
}
plot_scatter_cl(finalAdvWorksCusts, numcols, alpha = 0.2)

#Assignment Question 9 - Which of the following are true
# 1. Married customers have a higher median AvgMonthSpend than single customers.
# 2. Customers with no car have a higher median AvgMonthSpend than customers with three or more cars.
# 3. Male customers have a higher median AvgMonthSpend than female customers.
# 4. Female customers have a wider range of AvgMonthSpend values than male customers.
# 5. Customers with no children at home have a lower median AvgMonthSpend values than customers with one or more children at home.
 
plot_box = function(df, cols, col_y = 'AveMonthSpend'){
  options(repr.plot.width=4, repr.plot.height=3.5) # Set the initial plot area dimensions
  for(col in cols){
    df[,col] <- as.factor(df[,col])
    p = ggplot(df, aes_string(col, col_y)) +
      geom_boxplot() +
      ggtitle(paste('Box plot of', col, 'vs.', col_y))
    print(p)
  }
}
cat_cols = c('MaritalStatus', 'NumberCarsOwned', 'Gender', 'TotalChildren')
plot_box(finalAdvWorksCusts, cat_cols)

#Assignment Question no 10 = Which questions about bike buyers are true?
# 1. The median YearlyIncome is higher for customers who bought a bike than for customers who didn't.
# 2. The median number of cars owned by customers who bought a bike is lower than for customers who didn't.
# 3. The most common occupation type for customers who bought a bike is skilled manual.
# 4. Male customers are more likely to buy bikes than female customers.
# 5. A maried customer is more likely to buy a bike.


#1.
cat_cols = c('BikeBuyer')
plot_box(finalAdvWorksCusts, col = cat_cols, col_y = 'YearlyIncome')

#2.
cat_cols = c('BikeBuyer')
plot_box(finalAdvWorksCusts, col = cat_cols, col_y = 'NumberCarsOwned')

#3.
table(finalAdvWorksCusts[finalAdvWorksCusts$BikeBuyer == 1, 'Occupation'])

#4.
table(finalAdvWorksCusts$BikeBuyer, finalAdvWorksCusts$Gender)

#5. 
table(finalAdvWorksCusts$BikeBuyer, finalAdvWorksCusts$MaritalStatus)


#####Build Classification Model#################

#Data Preparation
glimpse(finalAdvWorksCusts)
finalAdvWorksCusts <- finalAdvWorksCusts %>% 
                        select(-AveMonthSpend)

options(repr.plot.width=4, repr.plot.height=4)

#Aggregate Categories to make variable more useful
#Correlations
corCols <- c("NumberCarsOwned","NumberChildrenAtHome","TotalChildren", "YearlyIncome","age")
cor(finalAdvWorksCusts[,corCols])

#Dropping Highly Colinear Columns : TotalChildren and NumberChildrenAtHome
finalAdvWorksCusts$NumberChildrenAtHome = NULL

#NumberCarsOwned
table(finalAdvWorksCusts$NumberCarsOwned)

finalAdvWorksCusts$NumberCarsOwned <- ifelse(finalAdvWorksCusts$NumberCarsOwned %in% c(0,1),0, ifelse(finalAdvWorksCusts$NumberCarsOwned == 2, 1, 2))

finalAdvWorksCusts$NumberCarsOwned <- ordered(finalAdvWorksCusts$NumberCarsOwned, levels = 0:2,
                                              labels = c("Zero_One", "Two", "Three_Four"))

table(finalAdvWorksCusts$NumberCarsOwned)

ggplot(finalAdvWorksCusts, aes(NumberCarsOwned,YearlyIncome)) +
  geom_boxplot()

#TotalChildren
table(finalAdvWorksCusts$TotalChildren)

finalAdvWorksCusts$TotalChildren <- ifelse(finalAdvWorksCusts$TotalChildren %in% c(0,1),0, ifelse(finalAdvWorksCusts$TotalChildren == 2, 1, 2))

finalAdvWorksCusts$TotalChildren <- ordered(finalAdvWorksCusts$TotalChildren, levels = 0:2,
                                              labels = c("Low", "Medium", "High"))

table(finalAdvWorksCusts$TotalChildren)

ggplot(finalAdvWorksCusts, aes(TotalChildren,YearlyIncome)) +
  geom_boxplot()

#TotalChildren
table(finalAdvWorksCusts$NumberChildrenAtHome)

finalAdvWorksCusts$TotalChildren <- ifelse(finalAdvWorksCusts$TotalChildren %in% c(0,1),0, ifelse(finalAdvWorksCusts$TotalChildren == 2, 1, 2))

finalAdvWorksCusts$TotalChildren <- ordered(finalAdvWorksCusts$TotalChildren, levels = 0:2,
                                            labels = c("Low", "Medium", "High"))

table(finalAdvWorksCusts$TotalChildren)

ggplot(finalAdvWorksCusts, aes(TotalChildren,YearlyIncome)) +
  geom_boxplot()

#YearlyIncome
plot_hist = function(df, col = 'YearlyIncome', bins = 10){
  options(repr.plot.width=4, repr.plot.height=3) # Set the initial plot area di
  bw = (max(df[,col]) - min(df[,col]))/(bins + 1)
  p = ggplot(df, aes_string(col)) +
    geom_histogram(binwidth = bw, aes(y=..density..), alpha = 0.5) +
    geom_density(aes(y=..density..), color = 'blue') +
    geom_rug()
  print(p)
}
plot_hist(finalAdvWorksCusts)

#Create Log
finalAdvWorksCusts[,c('log_YearlyIncome')] = log(finalAdvWorksCusts[,c('YearlyIncome')])

plot_hist(finalAdvWorksCusts, col = 'log_YearlyIncome')


#Logistic Regression Model
library(ROCR)
library(pROC)

#Check class balance
table(finalAdvWorksCusts$BikeBuyer)
finalAdvWorksCusts$BikeBuyer <- ifelse(finalAdvWorksCusts$BikeBuyer == 0, 'no', 'yes')
finalAdvWorksCusts$BikeBuyer <- factor(finalAdvWorksCusts$BikeBuyer, levels = c('no', 'yes'))
finalAdvWorksCusts$BikeBuyer[1:5]

#Split the Data into Training and Test Dataset
set.seed(1955)
## Randomly sample cases to create independent training and test data
partition = createDataPartition(finalAdvWorksCusts$BikeBuyer, times = 1, p = 0.7, list = FALSE)
training = finalAdvWorksCusts[partition,] # Create the training sample
dim(training)
test = finalAdvWorksCusts[-partition,] # Create the test sample
dim(test)

#Transform Variable
num_cols = c('YearlyIncome','log_YearlyIncome')
preProcValues <- preProcess(training[,num_cols], method = c("center", "scale"))
training[,num_cols] = predict(preProcValues, training[,num_cols])
test[,num_cols] = predict(preProcValues, test[,num_cols])
head(training[,num_cols])

#Drop YearlyIncome
finalAdvWorksCusts$YearlyIncome = NULL

#Logisitic Regression model
glimpse(finalAdvWorksCusts)
set.seed(5566)
logistic_mod = glm(BikeBuyer ~ CountryRegionName + Education +
                     Occupation + Gender + age + 
                     MaritalStatus + HomeOwnerFlag + log_YearlyIncome,
                     family = binomial, data = training)

logistic_mod$coefficients

test$probs = predict(logistic_mod, newdata = test, type = 'response')
test[1:20, c('BikeBuyer','probs')]

score_model = function(df, threshold){
  df$score = ifelse(df$probs < threshold, 'no', 'yes')
  df
}
test = score_model(test, 0.5)
test[1:20, c('BikeBuyer','probs','score')]

logistic.eval <- function(df){
  # First step is to find the TP, FP, TN, FN cases
  df$conf = ifelse(df$BikeBuyer == 'yes' & df$score == 'yes', 'TP',
                   ifelse(df$BikeBuyer == 'yes' & df$score == 'no', 'FN',
                          ifelse(df$BikeBuyer == 'no' & df$score == 'no'
                                 , 'TN', 'FP')))
  # Elements of the confusion matrix
  TP = length(df[df$conf == 'TP', 'conf'])
  FP = length(df[df$conf == 'FP', 'conf'])
  TN = length(df[df$conf == 'TN', 'conf'])
  FN = length(df[df$conf == 'FN', 'conf'])
  ## Confusion matrix as data frame
  out = data.frame(Negative = c(TN, FN), Positive = c(FP, TP))
  row.names(out) = c('Actual Negative', 'Actual Positive')
  print(out)
  # Compute and print metrics
  P = TP/(TP + FP)
  R = TP/(TP + FN)
  F1 = 2*P*R/(P+R)
  cat('\n')
  cat(paste('accuracy =', as.character(round((TP + TN)/(TP + TN + FP + FN), 3
  )), '\n'))
  cat(paste('precision =', as.character(round(P, 3)), '\n'))
  cat(paste('recall =', as.character(round(R, 3)), '\n'))
  cat(paste('F1 =', as.character(round(F1,3)),'\n'))
  roc_obj <- roc(df$BikeBuyer, df$probs)
  cat(paste('AUC =', as.character(round(auc(roc_obj),3)),'\n'))
}
logistic.eval(test)

ROC_AUC = function(df){
  options(repr.plot.width=5, repr.plot.height=5)
  pred_obj = prediction(df$probs, df$BikeBuyer)
  perf_obj <- performance(pred_obj, measure = "tpr", x.measure = "fpr")
  AUC = performance(pred_obj,"auc")@y.values[[1]] # Access the AUC from the slot of the S4 object
  plot(perf_obj)
  abline(a=0, b= 1, col = 'red')
  text(0.8, 0.2, paste('AUC = ', as.character(round(AUC, 3))))
}
ROC_AUC(test)