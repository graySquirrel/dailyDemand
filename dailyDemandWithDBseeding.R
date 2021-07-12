library(dplyr)
library(ggplot2)
library(httr)
library(jsonlite)
library(tidyr)
library(mc2d)
talkToServer <- FALSE
#talkToServer <- TRUE
setwd("/Users/fritzebner/Documents/R/dailyDemand/")
set.seed(21)

theURL <- "http://10.0.1.29:50536/"
ricochetLogin <- paste0(theURL, "api/auth/login")
addtransURL <- paste0(theURL, "api/transaction/add")

if (talkToServer) {
  #r <- POST(ricochetLogin, body='{"email":"fritzebner@gmail.com", "password":"P@ssw0rd1!"}', encode="raw", content_type_json())
  r <- POST(ricochetLogin, body='{"email":"fritzebner@gmail.com", "password":"Password01"}', encode="raw", content_type_json())
  print(paste("login response",r$status_code,content(r)))
  token <- content(r, "parsed")$AccessToken
  if (nchar(token) < 100) 
  {
    print("bad token")
    exit
  }
}
daysToSimulate <- 30
# setup: create csv files from a ricochet that has fresh users, vendors, no transactions
# csv files need: 
# customer: name, target, reorderPoint, inventory, id, inventoryID
# restaurant: name, plan, pickupday, inventory, id, inventoryID
# there is an inventory for each customer and each vendor/sku pair
# get user IDs and vendor IDs and put them into the csv files
# add transaction has cols: createdatetime, from, to, amount, fromid, toid, fromNewAmt, toNewAmt
# fromid and toid are inventory IDs.

rest <- read.csv("restaurant.csv", stringsAsFactors = FALSE)
cust <- read.csv("customer.csv", stringsAsFactors = FALSE)

haulerID <- rest$id[which(rest$name == "hauler")]
washerID <- rest$id[which(rest$name == "washer")]
skuid <- 5

addTransaction <- function(dtm, From, To, numItems, ID1, ID2)
{
  # From and To is from: "Customer", "Vendor"
  # lets start with transType always being "Other"
  # if from is customer, to has to be vendor and VendId1
  if (From == "Customer") # to has to be "Vendor 1"
  {
    cust[ID1,"inventory"] <<- cust[ID1,"inventory"] - numItems
    rest[ID2, "inventory"] <<- rest[ID2, "inventory"] + numItems
    #           date,  from, to,  num,   , id of from, id of to, newfrom amt, newto amt
    onetrans <- data.frame(dtm, From, To, numItems, ID1, ID2, cust[ID1, "inventory"], rest[ID2,"inventory"])
    colnames(onetrans) <- c("date","from","to","amount","fromid","toid","newFromAmt","newToAmt")
    transactions <<- rbind(transactions, onetrans)
  } 
  else if (From == "Vendor")
  {
    if(To == "Customer")
    {
      rest[ID1, "inventory"] <<- rest[ID1, "inventory"] - numItems
      cust[ID2,"inventory"] <<- cust[ID2,"inventory"] + numItems
      #           date,  from, to,  num,   , id of from, id of to, newfrom amt, newto amt
      onetrans <- data.frame(dtm, From, To, numItems, ID1, ID2, rest[ID1, "inventory"], cust[ID2,"inventory"])
      colnames(onetrans) <- c("date","from","to","amount","fromid","toid","newFromAmt","newToAmt")
      transactions <<- rbind(transactions, onetrans)
    } 
    else #its "Vendor" (restaurant, hauler, or washer)
    {
      rest[ID1, "inventory"] <<- rest[ID1, "inventory"] - numItems
      rest[ID2,"inventory"] <<- rest[ID2,"inventory"] + numItems
      #           date,  from, to,  num,   , id of from, id of to, newfrom amt, newto amt
      onetrans <- data.frame(dtm, From, To, numItems, ID1, ID2, rest[ID1, "inventory"], rest[ID2,"inventory"])
      colnames(onetrans) <- c("date","from","to","amount","fromid","toid","newFromAmt","newToAmt")
      transactions <<- rbind(transactions, onetrans)
    }
  } 
  else if (From == "NA")
  {
    # Vendor better by 'washer'
    rest[ID1, "inventory"] <<- rest[ID1, "inventory"] + numItems
    #           date,  from, to,  num,   , id of from, id of to, newfrom amt, newto amt
    onetrans <- data.frame(dtm, From, To, numItems, 0, ID1, 0, rest[ID1,"inventory"])
    colnames(onetrans) <- c("date","from","to","amount","fromid","toid","newFromAmt","newToAmt")
    transactions <<- rbind(transactions, onetrans)
  }
  else
  {
    print(paste("ERROR"))
    exit(1)
  }
  colnames(transactions) <<- c("date","from","to","amount","fromid","toid","newFromAmt","newToAmt")
  
}
#####################################################################################################
#endDay <- as.POSIXct("2021-05-01 00:00:01", tz = "EST")
endDay <- as.POSIXct(paste(Sys.Date(),"00:00:00"))
startDay <- endDay - as.difftime(daysToSimulate, unit="days")
theDate <- startDay
# do initialization transactions
transactions <- data.frame(startDay, "Vendor", "Vendor", 0, 0, 0, 0, 0)
colnames(transactions) <- c("date","from","to","amount","fromid","toid","newFromAmt","newToAmt")

# initial transactions to fill up vendors
# from, to, amount, fromid, toid, fromNewAmt, toNewAmt, createdatetime
#### Start washer out with 2x total restaurant initial target
addTransaction(startDay, "NA", "Vendor", sum(rest$target)*2, washerID, "NA")

listOfRestIDs <- rest[which(rest$type == "restaurant"),"id"]

# give restaurants starter fillup to target
for (i in listOfRestIDs) 
{
  # createdate, from, to, amount, fromid, toid
  addTransaction(startDay, "Vendor", "Vendor", rest[i,"target"], washerID, i)
}
# get rid of initial dummy transaction
transactions <- transactions[-1,]
daycount <- 1
while (theDate <= endDay - as.difftime(1, unit = "days")) 
{
  print(paste("DAY", daycount, "is", theDate))
  daycount <<- daycount + 1
  nextDay <- theDate + as.difftime(1, unit="days")
  # start the day at 5pm
  theDate <- theDate + as.difftime(17, unit="hours")
  # do restaurant to customer transactions during the day
  # while(theDate < nextDay) 
  # {
  #   # pick a restaurant and customer
  #   ri <- sample(listOfRestIDs)[1]
  #   ci <- sample(1:dim(cust)[1])[1]
  #   amount <- 1 # keep it simple
  #   # can transact?
  #   if (rest[ri,"inventory"] >= amount && 
  #       (cust[ci,"plan"] - cust[ci,"inventory"]) >= amount)
  #   {
  #     print(paste(theDate, "amount",amount,"from",rest[ri,"name"],"to",cust[ci,"name"]))
  #     addTransaction(theDate, "Vendor", "Customer", amount, ri, ci)
  #   }
  #   # customer orders every hour or so.
  #   theDate <- theDate + as.difftime(runif(1)*2, unit="hours")
  # }
  ## new loop through days goes:
  # for each day 
  #    for each customer
  #        determine if they take out (rbern(1,0.5))
  #        if yes
  #           determine number of containers (sample(1:plan, 1))
  #           if num <= credits available
  #              select a time for transaction
  #              pick restaurant, if inv > num do transaction
  for (ci in 1:dim(cust)[1])
  {
    # take out?
    if (rbern(1,0.5) == 1)
    {
      trytocheckout <- sample(1:cust$plan[ci], 1)
      creds <- cust$plan[ci] - cust$inventory[ci]
      if (creds >= trytocheckout)
      {
        ri <- sample(listOfRestIDs, 1)
        if (rest$inventory[ri] >= trytocheckout)
        {
          # get a random time between 5pm and 11:59:59pm
          #theDate is now at 5pm.  add random minutes 7hours*60minutes/hour
          transactionDT <- theDate + as.difftime(runif(1) * 7 * 60, unit="mins")
          print(paste(transactionDT, "amount",trytocheckout,"from",rest$name[ri],"which has",rest$inventory[ri], "to",cust$name[ci], "who has", cust$inventory[ci]))
          addTransaction(transactionDT, "Vendor", "Customer", trytocheckout, ri, ci)
        } 
        else
        {
          print(paste(rest$name[ri], "has",rest$inventory[ri], "ran out of inventory trying to give", cust$name[ci], trytocheckout, "containers"))
        }
      }
      else
      {
        print(paste(cust$name[ci], "has too few credits", creds, "to checkout", trytocheckout, "containers."))
      }
    }
  }
  theDate <- theDate + as.difftime(6.9999, unit="hours")
  print(paste("End of day", theDate)) # 23:59:59 of current day
  
  # after the day is done, collect from customers
  # time will be some small time before midnight.  advance clock to 9ish am
  theDate <- theDate + as.difftime(9, unit="hours")
  dow <- as.POSIXlt(theDate)$wday
  for (i in 1:dim(cust)[1])
  {
    if (dow == cust[i,"pickupday"] && cust$inventory[i] > 0) 
    {
      # add some small random number to datetime
      theDate <- theDate + as.difftime(runif(1)*10, unit="mins")
      print(paste(theDate,"day",dow,"pickup",cust[i,"inventory"],"from",cust[i,"name"]))
      addTransaction(theDate, "Customer", "Vendor", cust[i, "inventory"], i, haulerID)
      print(paste(theDate,"hauler has",rest[haulerID, "inventory"]))
    }
  }
  # after collection from customers, give to washer
  if (rest[haulerID, "inventory"] > 0)
  {
    # add some small random number to datetime
    theDate <- theDate + as.difftime(runif(1)*10, unit="mins")
    print(paste(theDate, "hauler to washer",rest[haulerID, "inventory"]))
    addTransaction(theDate, "Vendor", "Vendor", rest[haulerID, "inventory"], haulerID, washerID)
    print(paste(theDate,"washer has", rest[washerID, "inventory"]))
  }
  # after washed, restock restaurants if below reorder point
  for (i in listOfRestIDs) 
  {
    if (rest[i,"inventory"] < rest[i,"reorderPoint"])
    {
      # replenish every hour or so.
      theDate <- theDate + as.difftime(runif(1), unit="hours")
      replenishAmount <- rest[i,"target"] - rest[i,"inventory"]
      if (replenishAmount > rest[washerID,"inventory"]) 
      {
        print(paste("washer is OUT of STOCK"))
        exit()
      }
      addTransaction(theDate, "Vendor", "Vendor", replenishAmount, washerID, i)
      print(paste(theDate, "replenish",rest[i,"name"], "with",replenishAmount))
    }
  }
  print(paste("resetting day to next day", nextDay + as.difftime(17, unit="hours"), "from", theDate))
  if (theDate > (nextDay + as.difftime(17, unit="hours"))) {
    print(paste("error: theDate went too far"))
    exit()
  }
  theDate <- nextDay
}


###########################
# calculate:
# Daily demand for each restaurant (to customer)
cust_checkouts <- transactions[which(transactions$from == "Vendor" &
                                       transactions$to == "Customer"),]
# only look at last 30 days of demand
cust_checkouts <- cust_checkouts[cust_checkouts$date > Sys.time() - as.difftime(30, units = "days"),]

daily_sum_demand <- cust_checkouts %>%
  mutate(day = as.Date(date, format="%Y-%m-%d", tz="EST")) %>%
  mutate(name = rest[fromid, "name"]) %>%
  complete(day = seq.Date(as.Date(Sys.time() - as.difftime(30, units = "days")), as.Date(Sys.time()), by="day"), name) %>%
  group_by(name, day) %>%
  summarise(daily_demand=sum(amount)) 

daily_sum_demand$daily_demand[is.na(daily_sum_demand$daily_demand)] <- 0

average_daily_demand <- daily_sum_demand %>%
  summarise(avgdemand=mean(daily_demand), stddemand=sd(daily_demand))

print(average_daily_demand)

daily_inventory_restaurant <- cust_checkouts %>%
  mutate(day = as.Date(date, format="%Y-%m-%d", tz="EST")) %>%
  mutate(name = rest[fromid, "name"]) %>%
  group_by(name, day) %>%
  summarise(daily_inventory=mean(newFromAmt)) 

daily_inventory_washer <- transactions[which((transactions$fromid == washerID & transactions$from == "Vendor") |
                                             (transactions$toid == washerID & transactions$to == "Vendor") ),]



p1 <- ggplot(data = daily_inventory_restaurant, aes(x = day, y = daily_inventory, group = name, colour = name)) + geom_line()
print(p1)

p2 <- ggplot(data = daily_sum_demand, aes(x = day, y = daily_demand, group = name, colour = name)) + 
  geom_col(width=.5, position = "dodge")
print(p2)

daily_sum_demand %>% data.frame

###########################
# Post to ricochet test server
if (talkToServer) {
  headers <- httr::add_headers(
    "Authorization" = paste("Bearer",token, sep = " ")
  )
}
sendTransaction <- function(arow)
{
  if(arow$from=="NA") {
    if(arow$to=="Vendor" && arow$toid == washerID) {
      body <- paste0('{"CreatedById":"ignore", "CreatedDate":"',arow$date,'","SKUId":',skuid,',"NumItems":',arow$amount,
                     ',"TransactionType":"RicochetToWasher", "CustomerId":"", "Vendor1Id":',washerID,',"Vendor2Id":""}')
    }
  }
  else {
    if (arow$from == "Vendor") {
      if (arow$to == "Vendor") {
        body <- paste0('{"CreatedById":"ignore", "CreatedDate":"',arow$date,'","SKUId":',skuid,',"NumItems":',arow$amount,
                       ',"TransactionType":"Other", "From":"Vendor 1", "To":"Vendor 2", "CustomerId":"", "Vendor1Id":',
                       arow$fromid,',"Vendor2Id":',arow$toid,'}')
      } else { # to Customer
        body <- paste0('{"CreatedById":"ignore", "CreatedDate":"',arow$date,'","SKUId":',skuid,',"NumItems":',arow$amount,
                       ',"TransactionType":"Other", "From":"Vendor 1", "To":"Customer", "CustomerId":',arow$toid,',"Vendor1Id":',
                       arow$fromid,',"Vendor2Id":""}')
      }
    } else if (arow$from == "Customer") {
      if (arow$to == "Vendor") {
        body <- paste0('{"CreatedById":"ignore", "CreatedDate":"',arow$date,'","SKUId":',skuid,',"NumItems":',arow$amount,
                       ',"TransactionType":"Other", "From":"Customer", "To":"Vendor 1", "CustomerId":',arow$fromid,',"Vendor1Id":',
                       arow$toid,',"Vendor2Id":""}')
      }
    }
  }
  if (talkToServer) {
    r <- POST(addtransURL, body=body, encode="raw", content_type_json(),config=headers) #verbose(), 
    if (r$status_code != 200) {
      print(paste("bad call",content(r)))
      exit
    }
  }
  return(prettify(body))
}
jsonblob <- "["
for (i in 1:dim(transactions)[1])
{
  #print(paste("sending transaction",i))
  jsonblob <- c(jsonblob,",", sendTransaction(transactions[i,]))
}
jsonblob <- c(jsonblob,"]")
write(jsonblob, "jsonTransactions.txt")

if (talkToServer)
{
  body <- paste0('{"PageIndex":0,"PageSize":25}')
  r <- POST(paste0(theURL, "api/vendor/list"), body=body, encode="raw", content_type_json(), config=headers, verbose())
  
  print(paste(content(r)[[1]][[2]]$VendorName,content(r)[[1]][[2]]$avgDailyDemand,content(r)[[1]][[2]]$stdDailyDemand))
  print(paste(content(r)[[1]][[3]]$VendorName,content(r)[[1]][[3]]$avgDailyDemand,content(r)[[1]][[3]]$stdDailyDemand))
  print(paste(content(r)[[1]][[4]]$VendorName,content(r)[[1]][[4]]$avgDailyDemand,content(r)[[1]][[4]]$stdDailyDemand))
}



