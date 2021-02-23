#IXIS Data Science Challenge Code

#Reading in the counts.csv file
counts <- read.csv(file.choose())
View(counts)
#Reading in the cart.csv file
cart <- read.csv(file.choose())
View(cart)

#Loading in possible necessary packages
library(openxlsx)
library(tidyverse)
library(dplyr)

#Worksheet 1
wb <- createWorkbook("IXIS Data Science Challenge Workbook")
addWorksheet(wb, "Month * Device Aggregate")

#Getting rid of dim_browser and adding an ECR for the new variable (=transactions/sessions) 
#using dplyr
counts <- counts %>%
  select(dim_date, dim_deviceCategory, sessions, transactions, QTY) %>%
  mutate(ECR=transactions/sessions)

#Creating the Month*Device data aggregation for the new counts data frame
counts$dim_date <- strptime(as.character(counts$dim_date), "%m/%d/%y")
counts$dim_date <- format(counts$dim_date, format="%m-%d-%y")
aggMonthDevice <- aggregate(counts, by=list(counts$dim_date, counts$dim_deviceCategory), FUN="mean")

#Renaming the first two columns as Date and Device_Type because they were changed to Group.1 
#and Group.2 after doing the data aggregation
aggMonthDevice <- aggMonthDevice %>%
  rename(Date=Group.1, Device_Type=Group.2)

#Getting rid of the blank dim_date and dim_deviceCategory columns after the data aggregation
#and arranging the data frame in descending order by the new Date column. I also readded ECR 
#using mutate because it was returning NaN's in the new dataframe
aggMonthDevice <- aggMonthDevice %>%
  select(Date, Device_Type, sessions, transactions, QTY, ECR) %>%
  mutate(ECR=transactions/sessions) %>%
  arrange(desc(as.Date(Date, format="%Y/%m/%d")))

#Worksheet 2
addWorksheet(wb, "Two Recent Month Analysis")

#Loading in necessary packages
library(lubridate)
library(data.table)

#Reformatting Date variable because when trying to do the next step (~month(Date)), R would 
#return an error. This code allowed the data aggregation on the next line of code
aggMonthDevice$Date <- strptime(as.character(aggMonthDevice$Date), "%m-%d-%y")
aggMonthDevice$Date <- format(aggMonthDevice$Date, "%y-%m-%d")

#Data aggregation by month(Date) inlcuding all columns
recentTwelveMonth <- aggregate(cbind(sessions, transactions, QTY, ECR)~month(Date), data=aggMonthDevice, FUN="sum")

#Renaming month(Date) to Month to make the merge possible
recentTwelveMonth <- recentTwelveMonth %>%
  rename(Month="month(Date)")

#Creating a MonthYear column that combines dim_month and dim_year that will be kept after
#merging by just Month
cart$MonthYear <- paste(cart$dim_month, cart$dim_year, sep="-")

#Renaming dim_month to Month to make the merge possible
cart <- cart %>%
  rename(Month=dim_month)

#Merging recentTwelveMonth and cart by Month and including all columns using "all=TRUE"
#ECR incorrect in this table and will fix in next line of code
monthCartMerge <- merge(x=recentTwelveMonth, y=cart, by="Month", all=TRUE)
View(monthCartMerge)

#Selecting the columns I want to keep while also reordering them based on the order I 
#include them in the select function and recreating the ECR variable now that I have 
#montly totals for both sessions and transactions
monthCartMerge <- monthCartMerge %>%
  select(MonthYear, sessions, transactions, ECR, QTY, addsToCart) %>%
  mutate(ECR=transactions/sessions)

#Creating a new dataframe that inlcudes the three most recent months. This will allow me
#to create new columns that include prior month calculations for the two most recent 
#months. April will later be removed once all calculations are done
recentTwoMonth <- monthCartMerge %>%
  slice(-1:-3, -7:-12)

#Making recentTwoMonth a data.table
setDT(recentTwoMonth)

#Using data.table package to create previous month columns for all 5 variables
recentTwoMonth[,sessionsPrev := shift(sessions)]
recentTwoMonth[,transactionsPrev := shift(transactions)]
recentTwoMonth[,ECRPrev := shift(ECR)]
recentTwoMonth[,QTYPrev := shift(QTY)]
recentTwoMonth[,addsToCartPrev := shift(addsToCart)]

#Taking out April now that previous month calculations are done
recentTwoMonth <- recentTwoMonth %>%
  slice(-1)

#Making recentTwoMonth back to a data.frame
as.data.frame(recentTwoMonth)

#Adding Relative Difference columns for all 5 variables
recentTwoMonth$sessionsRelDiff <- recentTwoMonth$sessions-recentTwoMonth$sessionsPrev
recentTwoMonth$transactionsRelDiff <- recentTwoMonth$transactions-recentTwoMonth$transactionsPrev
recentTwoMonth$ECRRelDiff <- recentTwoMonth$ECR-recentTwoMonth$ECRPrev
recentTwoMonth$QTYRelDiff <- recentTwoMonth$QTY-recentTwoMonth$QTYPrev
recentTwoMonth$addsToCartRelDiff <- recentTwoMonth$addsToCart-recentTwoMonth$addsToCartPrev

#Adding Absolute Difference columns for all 5 variables 
recentTwoMonth$sessionsAbsDiff <- abs(recentTwoMonth$sessions-recentTwoMonth$sessionsPrev)
recentTwoMonth$transactionsAbsDiff <- abs(recentTwoMonth$transactions-recentTwoMonth$transactionsPrev)
recentTwoMonth$ECRAbsDiff <- abs(recentTwoMonth$ECR-recentTwoMonth$ECRPrev)
recentTwoMonth$QTYAbsDiff <- abs(recentTwoMonth$QTY-recentTwoMonth$QTYPrev)
recentTwoMonth$addsToCartAbsDiff <- abs(recentTwoMonth$addsToCart-recentTwoMonth$addsToCartPrev)

#Using dplyr to reorder the columns so that all sessions,. transactions, etc. are next to each other in 
#final dataframe
recentTwoMonth <- recentTwoMonth %>%
  select(MonthYear, sessions, sessionsPrev, sessionsAbsDiff, sessionsRelDiff, transactions, transactionsPrev, 
         transactionsAbsDiff, transactionsRelDiff, ECR, ECRPrev, ECRAbsDiff, ECRRelDiff, QTY, QTYPrev, 
         QTYAbsDiff, QTYRelDiff, addsToCart, addsToCartPrev, addsToCartAbsDiff, addsToCartRelDiff)
View(recentTwoMonth)






