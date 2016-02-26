#Get the data
# 1: setup a new directoy in the working directory to import the zip data file into


if(!file.exists("./data")){dir.create("./data")}

# 2: download the zip file from url


fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
download.file(fileUrl, destfile ="./data/StormData.csv.bz2")


#libraries
library(dplyr)
library(lubridate)
library(ggplot2)

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

str(data)

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



#Add a year column

#newData_2 <- mutate(newData, YEAR = year(newData$BGN_DATE))


#Fix values for property and crops - take 3



#take 92
#add new column with mutate

# 3.2: Only interested in continental US Hawai & DC so exclude all others

#try this first
#newData <- filter(newData, match(newData$STATE, state.abb))

                  #and this if it doesn't work
#cache this bit
newData$IS_STATE <- mapply(function(st) st %in% state.abb, newData$STATE)
newData <- newData[newData$IS_STATE==TRUE,]

#newData$STATE_NAME <- mapply(function(st_name) st_name %in% state.name, newData$STATE) 
#newData$STATE_NAME <- mapply(function(st_name) st_name %match% newData$STATE,state.abb)

#state names for supplementary analysis
newData$STATENAME <- state.name[match(newData$STATE,state.abb)]

#Justify ignoring data quality issues in PROPDMGEXP and CROPDMGEXP variables
summary(newData$PROPDMGEXP)
summary(newData$CROPDMGEXP)

#newData <- mutate(newData, PROPVALUE= paste(as.numeric(newData$PROPDMG)))

#newData$PROPVALUE = newData$PROPDMG

#cache this bit
CalcCost <- function(dmg, dmg_exp) dmg * switch(toupper(dmg_exp), H=100, K=1000, M=1000000, B=1000000000, 1)
newData$PROPCOSTS <- mapply(CalcCost, newData$PROPDMG, newData$PROPDMGEXP)
newData$CROPCOSTS <- mapply(CalcCost, newData$CROPDMG, newData$CROPDMGEXP)

# Group the 985 different event types in the dataset into a more manageable number
GrpEvents <- function(wEvent) {
        wEvent <- tolower(wEvent)
        ifelse(grepl("lightning", wEvent), "lightning",
        ifelse(grepl("hail|rain|wet|precipitation", wEvent), "precipitation",
        ifelse(grepl("flood|fld", wEvent), "flood",
        ifelse(grepl("snow|winter|wintry|blizzard|sleet|cold|ice|freeze|avalanche|icy", wEvent), "winter",
        ifelse(grepl("thunder|tstm|tornado|wind|hurricane|funnel|tropical +storm", wEvent), "storm",
        ifelse(grepl("fire", wEvent), "fire",
        ifelse(grepl("fog|visibility|dark|dust", wEvent), "visibility",
        ifelse(grepl("surf|surge|tide|tsunami|current|seiche", wEvent), "tidal",
        ifelse(grepl("heat|high +temp|record +temp|warm|dry|drought", wEvent), "heat",
        ifelse(grepl("volcan|landslide", wEvent), "seismic activity",
        "Other"))))))))))
        }
#cache this bit
newData$WEATHERGROUP <- mapply(GrpEvents, newData$EVTYPE)


#Plots - want to plot health and cost data against weather groupings
#Casualties
newData$CASUALTIES <- transform(newData, CASUALTIES = (newData$FATALITIES + newData$INJURIES))
storm_casualties <- newData %>% group_by(WEATHERGROUP) %>% summarise(sum(CASUALTIES))

#Plot
g <- ggplot(storm_casualties, aes(x=WEATHERGROUP, y=CASUALTIES))
g <- g + geom_bar(stat = "identity") + theme_bw() + theme(axis.text.x = element_text(angle=90, hjust=1))
print(g)

#costs
newData$TOTCOST <- transform(newData, TOTCOST = (newData$PROPCOSTS + newData$CROPCOSTS))

#storm_fatalities <- newData %>% group_by(WEATHERGROUP) %>% summarise(FATALITIES = sum(FATALITIES))
#storm_injuries <- newData %>% group_by(WEATHERGROUP) %>% summarise(INJURIES = sum(INJURIES))

storm_casualties <- newData %>% group_by(WEATHERGROUP) %>% summarise(sum(FATALITIES),sum(INJURIES))
colnames(storm_casualties) <- c("WEATHERGROUP","FATALITIES","INJURIES")
storm_casualties$WEATHERGROUP <- as.factor(storm_casualties$WEATHERGROUP)
#scratch
plot1 <- ggplot(hdat, aes(x=weatherCategory, y=value, fill=variable)) 
+ geom_bar(stat="identity") + coord_flip() + ggtitle("Total Casualties per Weather Type") 
+ xlab("Weather type") + ylab("# Casualties")

plot1 <- ggplot(newData, aes(x=WEATHERGROUP, y=FATALITIES)) + ggtitle("Total Casualties by Weather Grouping")

# 1st attempt
g <- ggplot(storm_casualties, aes(x=WEATHERGROUP, y=INJURIES))
g <- g + geom_bar(stat = "identity") + theme_bw() + theme(axis.text.x = element_text(angle=90, hjust=1))
print(g)

# 2nd attempt
reorder_size <- function(x) {
        factor(x, levels = names(sort(table(storm_casualties))))
        }
g <- ggplot(storm_casualties, aes(reorder_size(WEATHERGROUP)))
g <- g + geom_bar(stat = "identity")




#supplementary maps
storm_health <- mutate(newData, HEALTH = (newData$FATALITIES + newData$INJURIES))
sum_health <- storm_health %>% group_by(tolower(STATE_NAME)) %>% summarise(HEALTH=sum(HEALTH))
colnames(sum_health) <- c("region", "value")
state_choropleth(sum_health)

storm_costs <- mutate(newData, TOTCOST = (newData$PROPCOSTS + newData$CROPCOSTS))
sum_costs <- storm_costs %>% group_by(tolower(STATE_NAME)) %>% summarise(TOTCOST=sum(TOTCOST))
colnames(sum_costs) <- c("region", "value")
state_choropleth(sum_costs)

sum_fatal <- newData %>% group_by(tolower(STATE_NAME)) %>% summarise(FATALITIES=sum(FATALITIES))
colnames(sum_fatal) <- c("region", "value")
state_choropleth(sum_fatal)



