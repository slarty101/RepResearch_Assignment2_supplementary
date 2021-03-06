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
        ifelse(grepl("thunder|tstm|tornado|wind|hurricane|funnel|tropical +storm", wEvent), "wind",
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
#newData$CASUALTIES <- transform(newData, CASUALTIES = (newData$FATALITIES + newData$INJURIES))
#newData2 <- newData
newData2$WEATHERGROUP <- as.factor(newData2$WEATHERGROUP)
newData3 <- mutate(newData2, CASUALTIES = (FATALITIES + INJURIES))
storm_casualties <- newData3 %>% group_by(WEATHERGROUP) %>% summarise(CASUALTIES=sum(CASUALTIES))

#take 2
#storm_casualties <- newData[, c()]
#storm_casualties <- aggregate(.~WEATHERGROUP, data = newData, FUN = sum)
#storm_casualties <- newData %>% group_by(WEATHERGROUP) %>% summarise(sum(CASUALTIES))
#^ disnae work - crashes R

#Plot
g <- ggplot(storm_casualties, aes(x=WEATHERGROUP, y=CASUALTIES))
g <- g + geom_bar(stat = "identity") + theme_bw() + theme(axis.text.x = element_text(angle=90, hjust=1))
print(g)
#Plot take x
storm_casualties$WEATHERGROUP <- reorder(storm_casualties$WEATHERGROUP, desc(storm_casualties$CASUALTIES))
g <- ggplot(storm_casualties, aes(x=WEATHERGROUP, y=CASUALTIES/1000)) + ggtitle("Total Casualties by Weather Grouping")
g <- g + geom_bar(stat = "identity") + theme_bw() + theme(axis.text.x = element_text(angle=90, hjust=1))
g <- g + geom_bar(aes(x=WEATHERGROUP), data=storm_casualties, stat="identity" , fill = heat.colors(11))
g <- g + xlab("Weather Grouping") + ylab("Total Casualties (1000s)")
print(g)



#costs
#newData2$WEATHERGROUP <- as.factor(newData2$WEATHERGROUP)
newData3 <- mutate(newData2, TOTCOSTS = (PROPCOSTS + CROPCOSTS))
storm_costs <- newData3 %>% group_by(WEATHERGROUP) %>% summarise(TOTCOSTS=sum(TOTCOSTS))

#storm_costs <- arrange(storm_costs,desc(TOTCOSTS))
storm_costs$WEATHERGROUP <- reorder(storm_costs$WEATHERGROUP, desc(storm_costs$TOTCOSTS))

#g + geom_bar(aes(x=WEATHERGROUP), data=storm_costs, stat="identity")
g <- ggplot(storm_costs, aes(x=WEATHERGROUP, y=TOTCOSTS/1000000000)) + ggtitle("Total Costs by Weather Grouping")
g <- g + geom_bar(stat = "identity") + theme_bw() + theme(axis.text.x = element_text(angle=90, hjust=1))
g <- g + geom_bar(aes(x=WEATHERGROUP), data=storm_costs, stat="identity", fill = heat.colors(11))
g <- g + xlab("Weather Grouping") + ylab("Total Damage Costs (US$B)")
print(g)
#g <- ggplot(storm_costs, aes(x=reorder(WEATHERGROUP, WEATHERGROUP, function(x) -length(x))  y=TOTCOSTS))
#g <- g + geom_bar(stat = "identity") + theme_bw() + theme(axis.text.x = element_text(angle=90, hjust=1))
#print(g)
#newData$TOTCOST <- transform(newData, TOTCOST = (newData$PROPCOSTS + newData$CROPCOSTS))

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
#state_casualities <- mutate(newData, CASUALTIES = (newData$FATALITIES + newData$INJURIES))
state_casualties <- newData %>% group_by(tolower(STATENAME)) %>% summarise(CASUALTIES=sum(CASUALTIES))
colnames(state_casualties) <- c("region", "value")
state_choropleth(state_casualties, title = "Storm Damage Casualties Across US States", legend = "Casualties")

newData <- mutate(newData, TOTCOST = (newData$PROPCOSTS + newData$CROPCOSTS))
state_costs <- newData %>% group_by(tolower(STATENAME)) %>% summarise(TOTCOST=sum(TOTCOST)/1000000000)
#state_costs$STATENAME <- reorder(state_costs$STATENAME, desc(state_costs$TOTCOSTS))
state_costs <- arrange(state_costs, TOTCOST)
colnames(state_costs) <- c("region", "value")
state_choropleth(state_costs, title = "Storm Damage Costs Across US States", legend = "Economic Costs (US$B)" )


#xtables
xt_cas <- xtable(state_casualties[1:10,])
print(xt_cas, type = "html")

xt_cost <- xtable(state_costs[1:10,])
print(xt_cost, type = "html")





sum_fatal <- newData %>% group_by(tolower(STATE_NAME)) %>% summarise(FATALITIES=sum(FATALITIES))
colnames(sum_fatal) <- c("region", "value")
state_choropleth(sum_fatal)

state_costs$STATENAME <- reorder(state_costs$STATENAME, desc(state_costs$TOTCOSTS))

