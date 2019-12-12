library(plyr)
library(dplyr)
library(tidyverse)
library(tibble)
library(lubridate)
library(ggplot2)
library(caret)

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

AdvWorksCusts <- handleDupRecordsFunc(AdvWorksCusts, "YearlyIncome")
AW_AveMonthSpend <- handleDupRecordsFunc(AW_AveMonthSpend, "AveMonthSpend")
AW_BikeBuyer <- handleDupRecordsFunc(AW_BikeBuyer, "BikeBuyer")

finalAdvWorksCusts <- plyr::join(AdvWorksCusts, AW_AveMonthSpend, by = "CustomerID", type = "left")
finalAdvWorksCusts <- plyr::join(finalAdvWorksCusts, AW_BikeBuyer, by = "CustomerID", type = "left")

#Check missing value in Columns.
sapply(finalAdvWorksCusts, function(x) {sum(is.na(x))})

numcols <- c('HomeOwnerFlag', 'NumberCarsOwned', 'NumberChildrenAtHome', 'TotalChildren', 'YearlyIncome')
for(col in numcols){
  temp = AdvWorksCusts[,col]
  AdvWorksCusts[,col] = ifelse(temp == '?', NA, AdvWorksCusts[,col])
}



