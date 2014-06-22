Title : Natural events which greatly affect human health & have severe economic consequences
========================================================

# Synopsis:

This report examines the natural events which have great effect on human health and has deep economic consequences. For this report we used storm database provided by U.S. National Oceanic and Atmospheric Administration's (NOAA) . This database has events starting from 1950 to 2011. We analysed the data to calculate average fatalities, injuries, damage to property & crop for last 61 years. From this analysis we picked top ten events which caused on average most fatalities & injuries. Also we picked top 10 events which caused on average maximum damage to property & crop individually.

# Data Processing:

### Loading and cleaning data from file to Storm_DF dataframe


```r
#install.packages("stringr", dependencies=TRUE)
library(plyr)
```

```
## Warning: package 'plyr' was built under R version 3.0.3
```

```r
library(stringr)
#Read csv file 
Storm_DF <- read.table("repdata-data-StormData.csv.bz2",sep=',',header=TRUE,stringsAsFactors = FALSE,strip.white = TRUE, blank.lines.skip = TRUE)
#Remove leading spaces and covert all event type to uppercase 
Storm_DF$EVTYPE <-toupper(str_trim(Storm_DF$EVTYPE))
```

## Across the United States, which types of events are most harmful with respect to population health?
### In this section we are calculating average fatalities caused by each event for last 61 years, sorting it in descending order to see what caused most fatalities



```r
#Average Fatalities per event in last 61 years.
AverageFatalitiesByEvent <- aggregate(Storm_DF$FATALITIES,by=list(Storm_DF$EVTYPE),FUN=mean,na.rm=TRUE)
colnames(AverageFatalitiesByEvent) <- c("EVENT","FATALITIES")
AverageFatalitiesByEvent <- arrange(AverageFatalitiesByEvent,desc(AverageFatalitiesByEvent$FATALITIES),na.last=TRUE)
AverageFatalitiesByEvent$FATALITIES <- sprintf("%.5f",AverageFatalitiesByEvent$FATALITIES)
```

### In this section we are calculating average injuries caused by each event for last 61 years, sorting it in descending order to see what caused most injuries



```r
#Average Injuries per event in last 61 years.
AverageInjuriesByEvent <- aggregate(Storm_DF$INJURIES,by=list(Storm_DF$EVTYPE),FUN=mean,na.rm=TRUE)
colnames(AverageInjuriesByEvent) <- c("EVENT","INJURIES")
AverageInjuriesByEvent <- arrange(AverageInjuriesByEvent,desc(AverageInjuriesByEvent$INJURIES),na.last=TRUE)
AverageInjuriesByEvent$INJURIES <- sprintf("%.5f",AverageInjuriesByEvent$INJURIES)
```

### In this section we are merging average fatalities & injuries dataframes to see top events which are most harmful to population health



```r
#Merge both the dataframes
AvgFatInjByEvent <-merge(AverageFatalitiesByEvent,AverageInjuriesByEvent,by.x="EVENT",by.y="EVENT")
AvgFatInjByEvent$FATALITIES <- as.numeric(AvgFatInjByEvent$FATALITIES)
AvgFatInjByEvent$INJURIES <- as.numeric(AvgFatInjByEvent$INJURIES)
AvgFatInjByEvent <- arrange(AvgFatInjByEvent,desc(FATALITIES),na.last=TRUE)
```


## Across the United States, which types of events have the greatest economic consequences?

### In this section we are calculating average damage to properties caused by each event for last 61 years, sorting it in descending order to see what events caused most property damage


```r
#Average Property Damage per event in last 61 years.
AveragePropDamagByEvent <- aggregate(Storm_DF$PROPDMG,by=list(Storm_DF$EVTYPE),FUN=mean,na.rm=TRUE)
colnames(AveragePropDamagByEvent) <- c("EVENT","PROPERTY_DAMAGE")
AveragePropDamagByEvent <- arrange(AveragePropDamagByEvent,desc(AveragePropDamagByEvent$PROPERTY_DAMAGE),na.last=TRUE)
AveragePropDamagByEvent$PROPERTY_DAMAGE <- sprintf("%.5f",AveragePropDamagByEvent$PROPERTY_DAMAGE)
```

### In this section we are calculating average damage to crops caused by each event for last 61 years, sorting it in descending order to see what events caused most crop damage


```r
#Average Crope Damage per event in last 61 years.
AverageCropDamagByEvent  <- aggregate(Storm_DF$CROPDMG,by=list(Storm_DF$EVTYPE),FUN=mean,na.rm=TRUE)
colnames(AverageCropDamagByEvent) <- c("EVENT","CROP_DAMAGE")
AverageCropDamagByEvent <- arrange(AverageCropDamagByEvent,desc(AverageCropDamagByEvent$CROP_DAMAGE),na.last=TRUE)
AverageCropDamagByEvent$CROP_DAMAGE <- sprintf("%.5f",AverageCropDamagByEvent$CROP_DAMAGE)
```


# Results:

## Top ten events which are most harmful to population health


```r
AvgFatInjByEvent <- head(AvgFatInjByEvent,10) # Take top 10 events for plotting.
#To size the circle properly , draw radious out of total Fatalities & Injuries 
radius <- sqrt((AvgFatInjByEvent$FATALITIES+AvgFatInjByEvent$INJURIES)/ pi )
symbols(AvgFatInjByEvent$FATALITIES, AvgFatInjByEvent$INJURIES, circles=radius,inches=0.35, fg="white", bg="red", xlab="Fatalities", ylab="Injuries",main="Top 10 Events which are most harmful to population health")
text(AvgFatInjByEvent$FATALITIES, AvgFatInjByEvent$INJURIES, AvgFatInjByEvent$EVENT, cex=0.5)
```

![plot of chunk plot_top10_Fatalities_Injury_Events](figure/plot_top10_Fatalities_Injury_Events.png) 

## Top ten events which caused most Property Damage in past 61 years


```r
#Top 10 events which on average caused most property damage in last 61 year
head(AveragePropDamagByEvent,10)
```

```
##                             EVENT PROPERTY_DAMAGE
## 1                 COASTAL EROSION       766.00000
## 2            HEAVY RAIN AND FLOOD       600.00000
## 3          RIVER AND STREAM FLOOD       600.00000
## 4           BLIZZARD/WINTER STORM       500.00000
## 5                    FLASH FLOOD/       500.00000
## 6  FLASH FLOODING/THUNDERSTORM WI       500.00000
## 7               FLOOD/RIVER FLOOD       500.00000
## 8                   FROST\\FREEZE       500.00000
## 9                 HEAVY RAIN/SNOW       500.00000
## 10        HEAVY SNOW/WINTER STORM       500.00000
```

## Top ten events which caused most Crop Damage in past 61 years


```r
#Top 10 events which on average caused most Crop Damage in last 61 year
head(AverageCropDamagByEvent,10)
```

```
##                      EVENT CROP_DAMAGE
## 1    DUST STORM/HIGH WINDS   500.00000
## 2             FOREST FIRES   500.00000
## 3    TROPICAL STORM GORDON   500.00000
## 4          HIGH WINDS/COLD   401.00000
## 5          HURRICANE FELIX   250.00000
## 6            WINTER STORMS   166.66667
## 7        EXCESSIVE WETNESS   142.00000
## 8                  TYPHOON    75.00000
## 9  COLD AND WET CONDITIONS    66.00000
## 10               WILDFIRES    62.50000
```
