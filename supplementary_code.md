<!-- rmarkdown v1 -->
---
title: "Reproducible Research Assignment 2 - Supplimentary Analysis"
author: "Simon West"
date: "14 February 2016"
output: html_document
---
#An Analysis of the Health and Economic Effects of Storms in the US (1950 - 2011)

##Synopsis
  The [National Oceanic & Atmospheric Administration (NOAA)][1] Weather Service Storm data. This report undertakes analyses some supplimentary analysis and answers the two questions; Which states suffer the most  casualities due to storm events? & Which states suffer the greatest economic consequences due to storm events? From this supplimentary analysis it can be seen that Texas has suffered the most casualties related to storm events and that California and Louisana have suffered the greatest economic consequenses due to storm events.

##Data Processing

###Get the data

Set up a directory to download the data to.

```r
if(!file.exists("./data")){dir.create("./data")}
```

Download the datafile from Coursera

```r
fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
download.file(fileUrl, destfile ="./data/StormData.csv.bz2")
```


###Load the data
Load the required libraries for processing the data.

```r
library(lubridate)
library(dplyr,warn.conflicts = FALSE, quietly=TRUE)
library(ggplot2)
library(choroplethr)
library(xtable)
```

Load the datafile to our workspace for processing.


```r
data <- read.csv("./data/StormData.csv.bz2", header=TRUE, sep = ",")
```
Lets have a quick look at the dataset.

```r
dim(data)
```

```
## [1] 902297     37
```

```r
str(data)
```

```
## 'data.frame':	902297 obs. of  37 variables:
##  $ STATE__   : num  1 1 1 1 1 1 1 1 1 1 ...
##  $ BGN_DATE  : Factor w/ 16335 levels "10/10/1954 0:00:00",..: 6523 6523 4213 11116 1426 1426 1462 2873 3980 3980 ...
##  $ BGN_TIME  : Factor w/ 3608 levels "000","0000","00:00:00 AM",..: 212 257 2645 1563 2524 3126 122 1563 3126 3126 ...
##  $ TIME_ZONE : Factor w/ 22 levels "ADT","AKS","AST",..: 7 7 7 7 7 7 7 7 7 7 ...
##  $ COUNTY    : num  97 3 57 89 43 77 9 123 125 57 ...
##  $ COUNTYNAME: Factor w/ 29601 levels "","5NM E OF MACKINAC BRIDGE TO PRESQUE ISLE LT MI",..: 13513 1873 4598 10592 4372 10094 1973 23873 24418 4598 ...
##  $ STATE     : Factor w/ 72 levels "AK","AL","AM",..: 2 2 2 2 2 2 2 2 2 2 ...
##  $ EVTYPE    : Factor w/ 985 levels "?","ABNORMALLY DRY",..: 830 830 830 830 830 830 830 830 830 830 ...
##  $ BGN_RANGE : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ BGN_AZI   : Factor w/ 35 levels "","E","Eas","EE",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ BGN_LOCATI: Factor w/ 54429 levels "","?","(01R)AFB GNRY RNG AL",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ END_DATE  : Factor w/ 6663 levels "","10/10/1993 0:00:00",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ END_TIME  : Factor w/ 3647 levels "","?","0000",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ COUNTY_END: num  0 0 0 0 0 0 0 0 0 0 ...
##  $ COUNTYENDN: logi  NA NA NA NA NA NA ...
##  $ END_RANGE : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ END_AZI   : Factor w/ 24 levels "","E","ENE","ESE",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ END_LOCATI: Factor w/ 34506 levels "","(0E4)PAYSON ARPT",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ LENGTH    : num  14 2 0.1 0 0 1.5 1.5 0 3.3 2.3 ...
##  $ WIDTH     : num  100 150 123 100 150 177 33 33 100 100 ...
##  $ F         : int  3 2 2 2 2 2 2 1 3 3 ...
##  $ MAG       : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ FATALITIES: num  0 0 0 0 0 0 0 0 1 0 ...
##  $ INJURIES  : num  15 0 2 2 2 6 1 0 14 0 ...
##  $ PROPDMG   : num  25 2.5 25 2.5 2.5 2.5 2.5 2.5 25 25 ...
##  $ PROPDMGEXP: Factor w/ 19 levels "","-","?","+",..: 17 17 17 17 17 17 17 17 17 17 ...
##  $ CROPDMG   : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ CROPDMGEXP: Factor w/ 9 levels "","?","0","2",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ WFO       : Factor w/ 542 levels "","2","43","9V9",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ STATEOFFIC: Factor w/ 250 levels "","ALABAMA, Central",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ ZONENAMES : Factor w/ 25112 levels "","                                                                                                                               "| __truncated__,..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ LATITUDE  : num  3040 3042 3340 3458 3412 ...
##  $ LONGITUDE : num  8812 8755 8742 8626 8642 ...
##  $ LATITUDE_E: num  3051 0 0 0 0 ...
##  $ LONGITUDE_: num  8806 0 0 0 0 ...
##  $ REMARKS   : Factor w/ 436781 levels ""," ","  ","   ",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ REFNUM    : num  1 2 3 4 5 6 7 8 9 10 ...
```
We can see that the dataset contains 902297 observations of 37 variables. We only need a  subset of this data for our analysis. The dataset can be reduced to the variables of interest and then we can remove rows that are not required for our analysis. From the codebook for the data supplied by [NOAA] [2] we can see that the data relating to health is stored in 2 variables FATALITIES & INJURIES. The economic data is stored in 4 variables PROPDMG, PROPDMGEXP, CROPDMG & CROPDMGEXP.

##Process the data  

**Reduce the dataset to the columns of interest for our analysis.**

```r
newData <- data[ ,c(2,5,7,8,23,24,25,26,27,28)]
```

Fix the dates with lubridate

```r
newData$BGN_DATE <- mdy_hms(newData$BGN_DATE)
```

**Only interested in non zero health and economics data**


```r
newData <- filter(newData, FATALITIES > 0 | INJURIES > 0 |  PROPDMG > 0 | CROPDMG > 0)
```
**Economic Data Processing**  
The financial info - crop damage and property damage are stored as a numerical value in PROPDMG & CROPDMG variables with an exponential in the PROPDMGEXP & CROPDMGEXP variables. The exponential values are coded (B = Billion, M = Million, K = Thousand & H = Hundred). To get the $ costs we need to mutliply the numeric value by the decoded exponential however there are some data quality issues.


```r
summary(newData$PROPDMGEXP)
```

```
##             -      ?      +      0      1      2      3      4      5 
##  11585      1      0      5    210      0      1      1      4     18 
##      6      7      8      B      h      H      K      m      M 
##      3      3      0     40      1      6 231428      7  11320
```

```r
summary(newData$CROPDMGEXP)
```

```
##             ?      0      2      B      k      K      m      M 
## 152664      6     17      0      7     21  99932      1   1985
```
Ignoring case, we can see that there are 246 PROPDMGEXP results and 23 CROPDMGEXP results that don't fit the descriptors B, M, K or H. These results are a very small proportion of the overall dataset and will be ignored for our analysis.  

We can now decode the exponential varaibles and multiply the numeric PROPDMG & CROPDMG putting the output in 2 new variables PROPCOSTS & CROPCOSTS.   


```r
CalcCost <- function(dmg, dmg_exp) dmg * switch(toupper(dmg_exp), H=100, K=1000, M=1000000, B=1000000000, 1)

newData$PROPCOSTS <- mapply(CalcCost, newData$PROPDMG, newData$PROPDMGEXP)
newData$CROPCOSTS <- mapply(CalcCost, newData$CROPDMG, newData$CROPDMGEXP)
```

**Geographic Data processing**  
The dataset contains records for storm events for the continental USA, Alaska, Hawaii and overseas territories and dependancies. We want to restrict our analysis to just the continental US, Alaska & Hawaii as the choroplethr library can only map these states. 


```r
newData$IS_STATE <- mapply(function(st) st %in% state.abb, newData$STATE)
newData <- newData[newData$IS_STATE==TRUE,]
```
Now we need to add in the state name for choroplethr to map.  


```r
newData$STATENAME <- state.name[match(newData$STATE,state.abb)]
```

**Weather Event Grouping**  
The original dataset has 985 levels in the EVTYPE variable. This is too many to comfortably work with so we have grouped them into 11 groupings (fire, flood, heat, lightning, precipitation, seismic activity, tidal, visibility, wind, winter & Other). These are then stored in a new variable WEATHERGROUP. 


```r
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

newData$WEATHERGROUP <- mapply(GrpEvents, newData$EVTYPE)
```

##Results  

**Which states suffer the most  casualities due to storm events?**


```r
newData <- mutate(newData, CASUALTIES = (FATALITIES + INJURIES))
```

```r
state_casualties <- newData %>% group_by(tolower(STATENAME)) %>% summarise(CASUALTIES=sum(CASUALTIES))
colnames(state_casualties) <- c("region", "value")
state_choropleth(state_casualties, title = "Storm Damage Casualties Across US States", legend = "Casualties")
```

```
## Warning in self$bind(): The following regions were missing and are being
## set to NA: district of columbia
```

![plot of chunk map of casualties](figure/map of casualties-1.png)
Now put into decsending order

```r
state_casualties <- arrange(state_casualties, desc(value))
```
And then put in a table

```r
xt_cas <- xtable(state_casualties[1:10,])
print(xt_cas, type = "html")
```

<!-- html table generated in R 3.2.3 by xtable 1.8-2 package -->
<!-- Mon Feb 29 18:58:04 2016 -->
<table border=1>
<tr> <th>  </th> <th> region </th> <th> value </th>  </tr>
  <tr> <td align="right"> 1 </td> <td> texas </td> <td align="right"> 19033.00 </td> </tr>
  <tr> <td align="right"> 2 </td> <td> missouri </td> <td align="right"> 9752.00 </td> </tr>
  <tr> <td align="right"> 3 </td> <td> alabama </td> <td align="right"> 9526.00 </td> </tr>
  <tr> <td align="right"> 4 </td> <td> ohio </td> <td align="right"> 7515.00 </td> </tr>
  <tr> <td align="right"> 5 </td> <td> mississippi </td> <td align="right"> 7230.00 </td> </tr>
  <tr> <td align="right"> 6 </td> <td> illinois </td> <td align="right"> 6984.00 </td> </tr>
  <tr> <td align="right"> 7 </td> <td> florida </td> <td align="right"> 6664.00 </td> </tr>
  <tr> <td align="right"> 8 </td> <td> oklahoma </td> <td align="right"> 6168.00 </td> </tr>
  <tr> <td align="right"> 9 </td> <td> arkansas </td> <td align="right"> 6080.00 </td> </tr>
  <tr> <td align="right"> 10 </td> <td> tennessee </td> <td align="right"> 5723.00 </td> </tr>
   </table>

  So we can see from the table that state that has suffered the most casualties from storm events is Texas.

**Which types of events have the greatest economic consequences?**

```r
newData <- mutate(newData, TOTCOST = (newData$PROPCOSTS + newData$CROPCOSTS))
```


```r
state_costs <- newData %>% group_by(tolower(STATENAME)) %>% summarise(TOTCOST=sum(TOTCOST)/1000000000)
colnames(state_costs) <- c("region", "value")
state_choropleth(state_costs, title = "Storm Damage Costs Across US States", legend = "Economic Costs (US$B)" )
```

```
## Warning in self$bind(): The following regions were missing and are being
## set to NA: district of columbia
```

![plot of chunk map of totcosts](figure/map of totcosts-1.png)
Now put into decsending order

```r
state_costs <- arrange(state_costs, desc(value))
```
And then put in a table

```r
xt_cost <- xtable(state_costs[1:10,])
print(xt_cost, type = "html")
```

<!-- html table generated in R 3.2.3 by xtable 1.8-2 package -->
<!-- Mon Feb 29 18:58:05 2016 -->
<table border=1>
<tr> <th>  </th> <th> region </th> <th> value </th>  </tr>
  <tr> <td align="right"> 1 </td> <td> california </td> <td align="right"> 127.12 </td> </tr>
  <tr> <td align="right"> 2 </td> <td> louisiana </td> <td align="right"> 61.30 </td> </tr>
  <tr> <td align="right"> 3 </td> <td> florida </td> <td align="right"> 45.41 </td> </tr>
  <tr> <td align="right"> 4 </td> <td> mississippi </td> <td align="right"> 36.42 </td> </tr>
  <tr> <td align="right"> 5 </td> <td> texas </td> <td align="right"> 33.94 </td> </tr>
  <tr> <td align="right"> 6 </td> <td> alabama </td> <td align="right"> 17.85 </td> </tr>
  <tr> <td align="right"> 7 </td> <td> illinois </td> <td align="right"> 14.09 </td> </tr>
  <tr> <td align="right"> 8 </td> <td> north carolina </td> <td align="right"> 10.28 </td> </tr>
  <tr> <td align="right"> 9 </td> <td> iowa </td> <td align="right"> 10.19 </td> </tr>
  <tr> <td align="right"> 10 </td> <td> missouri </td> <td align="right"> 7.93 </td> </tr>
   </table>
  So we can see from the table that states that have suffered the most economic impact are Calirornia and Louisiana.


##Conclusion
From our analysis of the National Oceanic & Atmospheric Administration (NOAA) Weather Service Storm data we can see that the states sufferings the most casualties due to storm events is Texas and that the states suffering the most economic damage due to storm events are California and Louisana. 

##References

[1]:http://www.noaa.gov "National Oceanic & Atmospheric Administration (NOAA)"
[2]:http://https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2Fpd01016005curr.pdf "NOAA"



End of report.

