# Storm Impact on population health and the economy

Put summary here


```r
if("stringi" %in% rownames(installed.packages()) == FALSE) {install.packages("stringi")}

storm_data <- read.csv("repdata-data-StormData.csv.bz2", stringsAsFactors=FALSE)

head(storm_data)
```

```
##   STATE__           BGN_DATE BGN_TIME TIME_ZONE COUNTY COUNTYNAME STATE
## 1       1  4/18/1950 0:00:00     0130       CST     97     MOBILE    AL
## 2       1  4/18/1950 0:00:00     0145       CST      3    BALDWIN    AL
## 3       1  2/20/1951 0:00:00     1600       CST     57    FAYETTE    AL
## 4       1   6/8/1951 0:00:00     0900       CST     89    MADISON    AL
## 5       1 11/15/1951 0:00:00     1500       CST     43    CULLMAN    AL
## 6       1 11/15/1951 0:00:00     2000       CST     77 LAUDERDALE    AL
##    EVTYPE BGN_RANGE BGN_AZI BGN_LOCATI END_DATE END_TIME COUNTY_END
## 1 TORNADO         0                                               0
## 2 TORNADO         0                                               0
## 3 TORNADO         0                                               0
## 4 TORNADO         0                                               0
## 5 TORNADO         0                                               0
## 6 TORNADO         0                                               0
##   COUNTYENDN END_RANGE END_AZI END_LOCATI LENGTH WIDTH F MAG FATALITIES
## 1         NA         0                      14.0   100 3   0          0
## 2         NA         0                       2.0   150 2   0          0
## 3         NA         0                       0.1   123 2   0          0
## 4         NA         0                       0.0   100 2   0          0
## 5         NA         0                       0.0   150 2   0          0
## 6         NA         0                       1.5   177 2   0          0
##   INJURIES PROPDMG PROPDMGEXP CROPDMG CROPDMGEXP WFO STATEOFFIC ZONENAMES
## 1       15    25.0          K       0                                    
## 2        0     2.5          K       0                                    
## 3        2    25.0          K       0                                    
## 4        2     2.5          K       0                                    
## 5        2     2.5          K       0                                    
## 6        6     2.5          K       0                                    
##   LATITUDE LONGITUDE LATITUDE_E LONGITUDE_ REMARKS REFNUM
## 1     3040      8812       3051       8806              1
## 2     3042      8755          0          0              2
## 3     3340      8742          0          0              3
## 4     3458      8626          0          0              4
## 5     3412      8642          0          0              5
## 6     3450      8748          0          0              6
```

Let's only keep the data that we need for our analysis. Such as the EVTYPE for the events, FATALITIES, and INIJURIES for the population health, and anything starting with PROP and CROP for the economic consequences.

The result will then be saved in the a variable called storm.data. Note that it is not necessary to keep location data such as the State is not since the analysis is for all of the US.


```r
relevant.coplumns <- c("EVTYPE", "FATALITIES", "INJURIES", "PROPDMG", "PROPDMGEXP", "CROPDMG", 
    "CROPDMGEXP")

storm.data <- storm_data[relevant.coplumns]

summary(storm.data)
```

```
##     EVTYPE            FATALITIES          INJURIES        
##  Length:902297      Min.   :  0.0000   Min.   :   0.0000  
##  Class :character   1st Qu.:  0.0000   1st Qu.:   0.0000  
##  Mode  :character   Median :  0.0000   Median :   0.0000  
##                     Mean   :  0.0168   Mean   :   0.1557  
##                     3rd Qu.:  0.0000   3rd Qu.:   0.0000  
##                     Max.   :583.0000   Max.   :1700.0000  
##     PROPDMG         PROPDMGEXP           CROPDMG         CROPDMGEXP       
##  Min.   :   0.00   Length:902297      Min.   :  0.000   Length:902297     
##  1st Qu.:   0.00   Class :character   1st Qu.:  0.000   Class :character  
##  Median :   0.00   Mode  :character   Median :  0.000   Mode  :character  
##  Mean   :  12.06                      Mean   :  1.527                     
##  3rd Qu.:   0.50                      3rd Qu.:  0.000                     
##  Max.   :5000.00                      Max.   :990.000
```


```r
fatalities.per.event <- aggregate(FATALITIES ~ EVTYPE, data=storm.data, FUN=sum)
fatalities.per.event <- fatalities.per.event[order(fatalities.per.event$FATALITIES, decreasing=TRUE),]
injuries.per.event <- aggregate(INJURIES ~ EVTYPE, data=storm.data, FUN=sum)
injuries.per.event <- injuries.per.event[order(injuries.per.event$INJURIES, decreasing=TRUE),]

library(stringi)
fatalities.per.event$EVTYPE <- stri_trans_totitle(fatalities.per.event$EVTYPE)
injuries.per.event$EVTYPE <- stri_trans_totitle(injuries.per.event$EVTYPE)
                                                
                                                
head(fatalities.per.event,15)
```

```
##                EVTYPE FATALITIES
## 834           Tornado       5633
## 130    Excessive Heat       1903
## 153       Flash Flood        978
## 275              Heat        937
## 464         Lightning        816
## 856         Tstm Wind        504
## 170             Flood        470
## 585       Rip Current        368
## 359         High Wind        248
## 19          Avalanche        224
## 972      Winter Storm        206
## 586      Rip Currents        204
## 278         Heat Wave        172
## 140      Extreme Cold        160
## 760 Thunderstorm Wind        133
```

```r
head(injuries.per.event,15)
```

```
##                EVTYPE INJURIES
## 834           Tornado    91346
## 856         Tstm Wind     6957
## 170             Flood     6789
## 130    Excessive Heat     6525
## 464         Lightning     5230
## 275              Heat     2100
## 427         Ice Storm     1975
## 153       Flash Flood     1777
## 760 Thunderstorm Wind     1488
## 244              Hail     1361
## 972      Winter Storm     1321
## 411 Hurricane/Typhoon     1275
## 359         High Wind     1137
## 310        Heavy Snow     1021
## 957          Wildfire      911
```

```r
##summary(fatalities.per.event)

unique(storm.data$PROPDMGEXP)
```

```
##  [1] "K" "M" ""  "B" "m" "+" "0" "5" "6" "?" "4" "2" "3" "h" "7" "H" "-"
## [18] "1" "8"
```
Looking at the top 15 I determined that it was good enough to get the top 5 events that have caused fatalities and injuries


```r
fatalities.top.5 <- fatalities.per.event[1:5,]
injuries.top5 <- injuries.per.event[1:5,]
```
Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.
