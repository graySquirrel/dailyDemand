library(dplyr)
library(ggplot2)
library(httr)
library(jsonlite)
library(tidyr)
library(mc2d)

source("dailyDemandFuncs.R")

setwd("/Users/fritzebner/Documents/R/dailyDemand/")
set.seed(21)

daysPerWeekCustomersTakeout <- 1
daysToSimulate <- 30
plan <- 4
numRestaurants <- 1
numCustomers <- 30
initialTotalInventory <- 400
#initialRestaurantInventory <- 200
#numRestaurants <- 3
#initialRestaurantInventory <- initialTotalInventory / numRestaurants / 2
initialRestaurantInventory <- initialTotalInventory / numRestaurants / 2

rest <- mkInitialVendors(numRestaurants, initialRestaurantInventory)
cust <- mkCusts(numCustomers, plan)

trs <- makeTransactions(initialTotalInventory, daysPerWeekCustomersTakeout)
transactions <- trs[[1]]
numRestOutEvents <- trs[[2]]
numWashOutEvents <- trs[[3]]

average_daily_demand <- getDemand(transactions)

# assuming 1 day lead time...
average_daily_demand$reorderpoint <- average_daily_demand$avgdemand + 2*average_daily_demand$stddemand

print(average_daily_demand)
print(paste("number of restaurant out events",numRestOutEvents))
print(paste("number of washer out events",numWashOutEvents))

