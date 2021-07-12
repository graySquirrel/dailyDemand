

printme <- function(arg) {
  if (exists("debugme")) {
    print(arg)
  }
}
#####################################################################################################
# setup: create csv files from a ricochet that has fresh users, vendors, no transactions
# csv files need: 
# customer: name, target, reorderPoint, inventory, id, inventoryID
# restaurant: name, plan, pickupday, inventory, id, inventoryID
# there is an inventory for each customer and each vendor/sku pair
# get user IDs and vendor IDs and put them into the csv files
# add transaction has cols: createdatetime, from, to, amount, fromid, toid, fromNewAmt, toNewAmt
# fromid and toid are inventory IDs.
#rest <- read.csv("restaurant.csv", stringsAsFactors = FALSE)
#cust <- read.csv("customer.csv", stringsAsFactors = FALSE)
## instead of reading from file, create these df's on the fly.
mkInitialVendors <- function(n, t) {
  names <- as.character(seq(from = 1, to = n))
  targs <- rep(t, n)
  zs <- rep(0, n)
  fours <- rep(4, n)
  ids <- seq(from = 1, to = n)
  types <- rep("restaurant",n)
  df <- data.frame(names, targs, fours,zs,ids,types, stringsAsFactors = FALSE)
  colnames(df) <- c("name","target","reorderPoint","inventory","id","type")
  df[nrow(df) + 1,] <- list("hauler",0,0,0,0,"hauler")
  df[nrow(df) + 1,] <- list("washer",0,0,0,0,"washer")
  df$id <- seq(from = 1, to = nrow(df))
  df
}
#####################################################################################################
mkCusts <- function(n, plan) {
  names <- as.character(seq(from = 1, to = n))
  plans <- rep(plan, n)
  pickupdays <- sample(3, n, replace = TRUE) + 1
  zs <- rep(0,n)
  ids <- seq(1,n)
  df <- data.frame(names, plans, pickupdays, zs, ids)
  colnames(df) <- c("name","plan","pickupday","inventory","id")
  df
}
#####################################################################################################
initializeTransactionsDf <- function(initTot, startDay) {
  washerID <- rest$id[which(rest$name == "washer")]
  
  tr <- data.frame(startDay, "Vendor", "Vendor", 0, 0, 0, 0, 0)
  colnames(tr) <- c("date","from","to","amount","fromid","toid","newFromAmt","newToAmt")
  
  # initial transactions to fill up vendors
  # from, to, amount, fromid, toid, fromNewAmt, toNewAmt, createdatetime
  #### Start washer out with 2x total restaurant initial target
  #addTransaction(startDay, "NA", "Vendor", sum(rest$target)*2, washerID, "NA")
  tr <- rbind(tr, addTransaction(startDay, "NA", "Vendor", initTot, washerID, "NA"))
  
  listOfRestIDs <- rest[which(rest$type == "restaurant"),"id"]
  
  # give restaurants starter fillup to target
  for (i in listOfRestIDs) 
  {
    # createdate, from, to, amount, fromid, toid
    tr <- rbind(tr, addTransaction(startDay, "Vendor", "Vendor", rest[i,"target"], washerID, i))
  }
  # get rid of initial dummy transaction
  tr <- tr[-1,]
  
  tr
}
#####################################################################################################
# relies on cust and rest global objects to be present and properly formed.
#####################################################################################################
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
  } 
  else if (From == "Vendor")
  {
    if(To == "Customer")
    {
      rest[ID1, "inventory"] <<- rest[ID1, "inventory"] - numItems
      cust[ID2,"inventory"] <<- cust[ID2,"inventory"] + numItems
      #           date,  from, to,  num,   , id of from, id of to, newfrom amt, newto amt
      onetrans <- data.frame(dtm, From, To, numItems, ID1, ID2, rest[ID1, "inventory"], cust[ID2,"inventory"])
    } 
    else #its "Vendor" (restaurant, hauler, or washer)
    {
      rest[ID1, "inventory"] <<- rest[ID1, "inventory"] - numItems
      rest[ID2,"inventory"] <<- rest[ID2,"inventory"] + numItems
      #           date,  from, to,  num,   , id of from, id of to, newfrom amt, newto amt
      onetrans <- data.frame(dtm, From, To, numItems, ID1, ID2, rest[ID1, "inventory"], rest[ID2,"inventory"])
    }
  } 
  else if (From == "NA")
  {
    # Vendor better by 'washer'
    rest[ID1, "inventory"] <<- rest[ID1, "inventory"] + numItems
    #           date,  from, to,  num,   , id of from, id of to, newfrom amt, newto amt
    onetrans <- data.frame(dtm, From, To, numItems, 0, ID1, 0, rest[ID1,"inventory"])
  }
  else
  {
    print(paste("ERROR"))
    exit(1)
  }
  colnames(onetrans) <- c("date","from","to","amount","fromid","toid","newFromAmt","newToAmt")
  onetrans
}
#####################################################################################################
makeTransactions <- function(initialTotalInventory, daysPerWeekTakeout) {
  endDay <- as.POSIXct(paste(Sys.Date(),"00:00:00")) #Today is end day...
  
  # do initialization transactions
  listOfRestIDs <- rest[which(rest$type == "restaurant"),"id"]
  haulerID <- rest$id[which(rest$name == "hauler")]
  washerID <- rest$id[which(rest$name == "washer")]
  startDay <- endDay - as.difftime(daysToSimulate, unit="days")
  theDate <- startDay
  transactions <- initializeTransactionsDf(initialTotalInventory, startDay)
  numRestaurantOutEvents <- 0
  numWasherOutEvents <- 0
  daycount <- 1
  while (theDate <= endDay - as.difftime(1, unit = "days")) 
  {
    printme(paste("DAY", daycount, "is", theDate))
    daycount <<- daycount + 1
    nextDay <- theDate + as.difftime(1, unit="days")
    # start the day at 5pm
    theDate <- theDate + as.difftime(17, unit="hours")
    ##  loop through days goes:
    # for each day 
    #    for each customer
    #        determine if they take out (rbern(1,0.5))
    #        if yes
    #           determine number of containers (sample(1:plan, 1))
    #           if num <= credits available
    #              select a time for transaction
    #              pick restaurant, if inv > num do transaction - if restaurant doesnt have enough, this is an error.
    for (ci in 1:dim(cust)[1])
    {
      # take out?  right now, every customer has a 50% chance to order takeout every day.  
      # that is too high... should be N*1/7 for N days/week eating takeout.
      if (rbern(1,daysPerWeekTakeout * 1/7) == 1)
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
            printme(paste(transactionDT, "amount",trytocheckout,"from",rest$name[ri],"which has",
                        rest$inventory[ri], "to",cust$name[ci], "who has", cust$inventory[ci]))
            transactions <- rbind(transactions, addTransaction(transactionDT, "Vendor", "Customer", trytocheckout, ri, ci))
          } 
          else
          {
            printme(paste("ERRORRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRR",rest$name[ri], "has",rest$inventory[ri], 
                        "ran out of inventory trying to give", cust$name[ci], trytocheckout, "containers"))
            numRestaurantOutEvents <- numRestaurantOutEvents + 1
          }
        }
        else
        {
          printme(paste(cust$name[ci], "has too few credits", creds, "to checkout", trytocheckout, "containers."))
        }
      }
    }
    theDate <- theDate + as.difftime(6.9999, unit="hours")
    printme(paste("End of day", theDate)) # 23:59:59 of current day
    
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
        printme(paste(theDate,"day",dow,"pickup",cust[i,"inventory"],"from",cust[i,"name"]))
        transactions <- rbind(transactions, addTransaction(theDate, "Customer", "Vendor", cust[i, "inventory"], i, haulerID))
        printme(paste(theDate,"hauler has",rest[haulerID, "inventory"]))
      }
    }
    # after collection from customers, give to washer
    if (rest[haulerID, "inventory"] > 0)
    {
      # add some small random number to datetime
      theDate <- theDate + as.difftime(runif(1)*10, unit="mins")
      printme(paste(theDate, "hauler to washer",rest[haulerID, "inventory"]))
      transactions <- rbind(transactions, addTransaction(theDate, "Vendor", "Vendor", rest[haulerID, "inventory"], haulerID, washerID))
      printme(paste(theDate,"washer has", rest[washerID, "inventory"]))
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
          print(paste("ERRORRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRR washer is OUT of STOCK"))
          numWasherOutEvents <- numWasherOutEvents + 1
        }
        else {
          transactions <- rbind(transactions, addTransaction(theDate, "Vendor", "Vendor", replenishAmount, washerID, i))
          printme(paste(theDate, "replenish",rest[i,"name"], "with",replenishAmount))
        }
      }
    }
    printme(paste("resetting day to next day", nextDay + as.difftime(17, unit="hours"), "from", theDate))
    if (theDate > (nextDay + as.difftime(17, unit="hours"))) {
      print(paste("error: theDate went too far"))
      exit()
    }
    theDate <- nextDay
  }
  
  retvals <- list(transactions, numRestaurantOutEvents, numWasherOutEvents)
}
#####################################################################################################
# calculate:
# Daily demand for each restaurant (to customer)
getDemand <- function(transactions) {
  endDay <- as.POSIXct(paste(Sys.Date(),"00:00:00")) #Today is end day...
  
  washerID <- rest$id[which(rest$name == "washer")]
  
  cust_checkouts <- transactions[which(transactions$from == "Vendor" &
                                         transactions$to == "Customer"),]
  # only look at last 30 days of demand
  cust_checkouts <- cust_checkouts[cust_checkouts$date > endDay - as.difftime(30, units = "days"),]
  
  daily_sum_demand <- cust_checkouts %>%
    mutate(day = as.Date(date, format="%Y-%m-%d", tz="EST")) %>%
    mutate(name = rest[fromid, "name"]) %>%
    complete(day = seq.Date(as.Date(endDay - as.difftime(30, units = "days")), as.Date(endDay), by="day"), name) %>%
    group_by(name, day) %>%
    summarise(daily_demand=sum(amount)) 
  
  daily_sum_demand$daily_demand[is.na(daily_sum_demand$daily_demand)] <- 0
  
  average_daily_demand <- daily_sum_demand %>%
    summarise(avgdemand=mean(daily_demand), stddemand=sd(daily_demand))
  
  
  daily_inventory_restaurant <- cust_checkouts %>%
    mutate(day = as.Date(date, format="%Y-%m-%d", tz="EST")) %>%
    mutate(name = rest[fromid, "name"]) %>%
    group_by(name, day) %>%
    summarise(daily_inventory=mean(newFromAmt)) 
  
  daily_inventory_washer <- transactions[which((transactions$fromid == washerID & transactions$from == "Vendor") |
                                                 (transactions$toid == washerID & transactions$to == "Vendor") ),]
  
  # p1 <- ggplot(data = daily_inventory_restaurant, aes(x = day, y = daily_inventory, group = name, colour = name)) + geom_line()
  # print(p1)
  # 
  # p2 <- ggplot(data = daily_sum_demand, aes(x = day, y = daily_demand, group = name, colour = name)) + 
  #   geom_col(width=.5, position = "dodge")
  # print(p2)
  
  average_daily_demand
}
#####################################################################################################