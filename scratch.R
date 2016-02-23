#Get the data
# 1: setup a new directoy in the working directory to import the zip data file into


if(!file.exists("./data")){dir.create("./data")}

# 2: download the zip file from url


fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
download.file(fileUrl, destfile ="./data/StormData.csv.bz2")


#libraries
library(dplyr)
library(lubridate)

#capture date of download
#dateDownloaded <- date()


###read the csv file from the archive into a data table
#(This will take some time...)

data <- read.csv("./data/StormData.csv.bz2", header=TRUE, sep = ",")


### An intial look at the data


dim(data)

head(data)

tail(data)

summary(data)

### 3: Start cutting the data down to what we're intested in

newData <- data[ ,c(1,2,5,6,7,8,23,24,25,26,27,28)]

# new improved newData
newData <- data[ ,c(2,5,7,8,23,24,25,26,27,28)]

#the health info is stored in 2 variables FATALITIES and INJURIES

max(newData$FATALITIES)

max(newData$INJURIES)


#economic data is stored in 4 variables PROPDMG, PROPDMGEXP, CROPDMG & CROPDMGEXP

max(newData$PROPDMG)
#levels(newData$PROPDMGEXP)
max(newData$CROPDMG)
#levels(newData$CROPDMGEXP)

#storm event types are stored in the EVTYPE variable

levels(newData$EVTYPE)


#looking at the data

by_event <- newData %>% group_by(EVTYPE) %>% summarise(FATALITIES=sum(FATALITIES))

by_event <- newData %>% group_by(EVTYPE) %>% summarise(INJURIES=sum(INJURIES))

#Fix the dates with lubridate

newData$BGN_DATE <- mdy_hms(newData$BGN_DATE)

# 3.1: only interested in non zero health and economics

newData <- filter(newData, FATALITIES > 0 | INJURIES > 0 |  PROPDMG > 0 | CROPDMG > 0)



#need to convert PROPDMGEXP to multiplier

#newData_2 <- mutate(newData, prop_mult = If{ newData$PROPDMGEXP == "B" | newData$PROPDMGEXP =="b", 10000000 else if newData$PROPDMGEXP == "M" | newData$PROPDMGEXP =="m", 1000000, else if newData$PROPDMGEXP == "K" | newData$PROPDMGEXP =="k", 1000, else if newData$PROPDMGEXP == "H" | newData$PROPDMGEXP =="h", 100 else 1})

#take 2

#newData$propCost <- ifelse(newData$PROPDMGEXP == "B" | newData$PROPDMGEXP == "b", 10000000,(ifelse(newData$PROPDMGEXP == "M" | newData$PROPDMGEXP == "m", 1000000),(ifelse(newData$PROPDMGEXP == "K" | newData$PROPDMGEXP == "k", 1000),(ifelse(newData$PROPDMGEXP == "H" | newData$PROPDMGEXP == "h", 100),1))))



#Add a year column

#newData_2 <- mutate(newData, YEAR = year(newData$BGN_DATE))


#Fix values for property and crops - take 3

#newData_3 <- mutate(newData, PROPVALUE = paste(newData$PROPDMG, newData$PROPDMGEXP))


#PROPCOSTS <- if(newData$PROPDMGEXP == "B" | newData$PROPDMGEXP == "b")
#                {newData$PROPVALUE<- newData$PROPDMG * 1000000000} 
 #       else if(newData$PROPDMGEXP == "M" | newData$PROPDMGEXP == "m")
#                {newData$PROPVALUE<- newData$PROPDMG * 1000000} 
#        else if(newData$PROPDMGEXP == "K" | newData$PROPDMGEXP == "k")
#                {newData$PROPVALUE<- newData$PROPDMG * 1000} 
#        else if(newData$PROPDMGEXP == "H" | newData$PROPDMGEXP == "h")
#                {newData$PROPVALUE<- newData$PROPDMG * 100} 
#        else{newData$PROPVALUE<- newData$PROPDMG * 1}

#take 92
#add new column with mutate

# 3.2: Only interested in continental US Hawai & DC so exclude all others

#try this first
newData <- filter(newData, match(newData$STATE, state.abb))

                  #and this if it doesn't work

newData$is_STATE <- mapply(function(st) st %in% state.abb, newData$STATE)
newData <- newData[newData$is_STATE==TRUE,]

#newData$STATE_NAME <- mapply(function(st_name) st_name %in% state.name, newData$STATE) 
#newData$STATE_NAME <- mapply(function(st_name) st_name %match% newData$STATE,state.abb)
#state names
newData$STATE_NAME <- state.name[match(newData$STATE,state.abb)]

#Justify ignoring data quality issues in PROPDMGEXP and CROPDMGEXP variables
summary(newData$PROPDMGEXP)
summary(newData$CROPDMGEXP)

#newData <- mutate(newData, PROPVALUE= paste(as.numeric(newData$PROPDMG)))

#newData$PROPVALUE = newData$PROPDMG
calcCOST <- function(dmg, dmg_exp) dmg * switch(to_upper(dmg_exp), H=100, K=1000, M=1000000, B=1000000000, 1)
newData$PROPCOSTS <- mapply(calcCOST, newData$PROPDMG, newData$PROPDMGEXP)
newData$CROPCOSTS <- mapply(calcCOST, newData$CROPDMG, newData$CROPDMGEXP)

##
#PROPCOSTS <- if(newData$PROPDMGEXP == "B"){newData$PROPVALUE<- newData$PROPDMG * 1000000000} else
#                 if(newData$PROPDMGEXP == "b"){newData$PROPVALUE<- newData$PROPDMG * 1000000000} else
#                 if(newData$PROPDMGEXP == "M"){newData$PROPVALUE<- newData$PROPDMG * 1000000} else
#                 if(newData$PROPDMGEXP == "m"){newData$PROPVALUE<- newData$PROPDMG * 1000000} else
#                 if(newData$PROPDMGEXP == "K"){newData$PROPVALUE<- newData$PROPDMG * 1000} else
#                 if(newData$PROPDMGEXP == "k"){newData$PROPVALUE<- newData$PROPDMG * 1000} else
#                 if(newData$PROPDMGEXP == "H"){newData$PROPVALUE<- newData$PROPDMG * 100} else
#                 if(newData$PROPDMGEXP == "h"){newData$PROPVALUE<- newData$PROPDMG * 100} else
#                {newData$PROPVALUE<- newData$PROPDMG * 1}

#PROPCOSTS <- if(newData$PROPDMGEXP == "B" | newData$PROPDMGEXP == "b"){newData$PROPVALUE<- newData$PROPDMG * 1000000000} else if(newData$PROPDMGEXP == "M" | newData$PROPDMGEXP == "m"){newData$PROPVALUE<- newData$PROPDMG * 1000000} else if(newData$PROPDMGEXP == "K" | newData$PROPDMGEXP == "k"){newData$PROPVALUE<- newData$PROPDMG * 1000} else if(newData$PROPDMGEXP == "H" | newData$PROPDMGEXP == "h"){newData$PROPVALUE<- newData$PROPDMG * 100} else{newData$PROPVALUE<- newData$PROPDMG * 1}

#CROPCOSTS <- if(newData$CROPDMGEXP == "B" | newData$CROPDMGEXP == "b")
#                {newData$CROPVALUE<- newData$CROPDMG * 1000000000} 
#        else if(newData$CROPDMGEXP == "M" | newData$CROPDMGEXP == "m")
#                {newData$CROPVALUE<- newData$CROPDMG * 1000000} 
#        else if(newData$CROPDMGEXP == "K" | newData$CROPDMGEXP == "k")
#                {newData$CROPVALUE<- newData$CROPDMG * 1000} 
#        else if(newData$CROPDMGEXP == "H" | newData$CROPDMGEXP == "h")
#                {newData$CROPVALUE<- newData$CROPDMG * 100} 
#        else{newData$CROPVALUE<- newData$CROPDMG * 1}

#propCost <- ifelse(newData_3$PROPDMGEXP == "B", 1000000000,
#                   ifelse(newData_3$PROPDMGEXP == "b", 1000000000)
#                   ifelse(newData_3$PROPDMGEXP == "M", 1000000)
#                   ifelse(newData_3$PROPDMGEXP == "m", 1000000)
#                   ifelse(newData_3$PROPDMGEXP == "K", 1000)
#                   ifelse(newData_3$PROPDMGEXP == "k", 1000)
#                   ifelse(newData_3$PROPDMGEXP == "H", 100)
#                   ifelse(newData_3$PROPDMGEXP == "h", 100)1)


#^disnae work either
