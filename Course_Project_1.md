# Interactive Map of Major US Storms: 1996 - 2011
Mark Blackmore  
August 16, 2017  



## Introduction
This report explores the NOAA Storm Database and creates interactive maps, in the interest of answering two basic questions:  

1. Across the United States, which types of events have had at least ten million dollars in property damage? 
2. Across the United States, which types of events have had at least ten fatalities? 

## NOAA Storm Data: Download and Import
Storm data were read from the NOAA source url to the local level and read into R for processing.  Also listed are url's for the NOAA database codebook, and  the Coursera Reproducible Reasearch Forum on how to handle economic damage variables.


```r
library(tidyverse)

### NOAA Storn Database
url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"

### Codebook
codebook_url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2Fpd01016005curr.pdf"

### Forum Notes on Handling PROPDMGEXP, CROPDMGWXP Variables
forum_url <- "https://www.coursera.org/learn/reproducible-research/discussions/weeks/4/threads/IdtP_JHzEeaePQ71AQUtYw"

exp_vars_url <- "https://rstudio-pubs-static.s3.amazonaws.com/58957_37b6723ee52b455990e149edde45e5b6.html"

#download.file(url, destfile = "StormData.csv")
stormData <- read.csv("StormData.csv", stringsAsFactors = FALSE)
```

## Data Wrangling
The original data was treated as read-only. Processing occured in steps, beginning with a copy of the source data. Date variables were coerced to class-date.  

The entire data set begins in 1950 but initally only included the event type tornado. More event types were added over time. From 1996 on, all event types were used. Hence, we subsetted the data to events occurring on or after January 1, 1996.

The storm data type variable "EVTYPE" and exponent variables were coerced to class factor for processing and to aid in investigation.  Several event types were renamed to conform with the NOAA Event Type classification table.  This reclassification also focused on major events and did not handle every instance, only those thought to have a major impact on this analysis.  

Variables related to property damage and crop damage caused by storm events were converted to a common dollar basis and scaled to $ millions. Further disucssion here is technical and may be skipped without losing the overall point of this analysis.

Variables for economic damage include PROPDMG for property damage, CROPDMG for crop damage, PROPDMGEXP for the power of ten exponent related to PROPDMG, and CROPDMGEXP for the power of ten exponent related to CROPDMG.  We used the discussion provided in exp_vars_url, listed above, as a guide to changing PROPDMG and CROPDMG to dollars in millions.  


```r
### Treat downloaded data as read-only
stormData1 <- stormData

## Change event begin date varaiable class from character to date
stormData1$BGN_DATE <- as.Date(stormData$BGN_DATE, format = "%m/%d/%Y")

## Subset to years 1996 - 2011
stormData2 <- stormData1 %>% filter(BGN_DATE >= "1996-01-01")

## Change event type variable from character to factor for further investigation
stormData2$EVTYPE   <- factor(stormData2$EVTYPE)
stormData2$PROPDMGEXP   <- factor(stormData2$PROPDMGEXP)  
stormData2$CROPDMGEXP   <- factor(stormData2$CROPDMGEXP)

## Correct Event Types to NOAA specification
stormData2$EVTYPE[stormData2$EVTYPE=="HURRICANE/TYPHOON"] <- "HURRICANE"
stormData2$EVTYPE[stormData2$EVTYPE=="TYPHOON"] <- "HURRICANE"
stormData2$EVTYPE[stormData2$EVTYPE=="STORM SURGE"] <- "STORM SURGE/TIDE"
stormData2$EVTYPE[stormData2$EVTYPE=="TSTM WIND"] <- "THUNDERSTORM WIND"
stormData2$EVTYPE[stormData2$EVTYPE=="TSTM WIND/HAIL"] <- "THUNDERSTORM WIND"
stormData2$EVTYPE[stormData2$EVTYPE=="MARINE TSTM WIND"] <- "MARINE THUNDERSTORM WIND"
stormData2$EVTYPE[stormData2$EVTYPE=="WILD/FOREST FIRE"] <- "WILDFIRE"
stormData2$EVTYPE[stormData2$EVTYPE=="FOG"] <- "DENSE FOG"
stormData2$EVTYPE[stormData2$EVTYPE=="URBAN/SML STREAM FLD"] <- "FLOOD"
stormData2$EVTYPE[stormData2$EVTYPE=="WINTER WEATHER/MIX"] <- "WINTER WEATHER"
stormData2$EVTYPE[stormData2$EVTYPE=="HEAVY SURF/HIGH SURF"] <- "HIGH SURF"
stormData2$EVTYPE[stormData2$EVTYPE=="TSTM WIND (G45)"] <- "THUNDERSTORM WIND"
stormData2$EVTYPE[stormData2$EVTYPE=="TSTM WIND (G40)"] <- "THUNDERSTORM WIND"
stormData2$EVTYPE[stormData2$EVTYPE=="STRONG WINDS"] <- "HIGH WIND"
stormData2$EVTYPE[stormData2$EVTYPE=="FREEZING RAIN"] <- "SLEET"
stormData2$EVTYPE[stormData2$EVTYPE=="EXTREME WINDCHILL TEMPERATURES"] <- "EXTREME COLD/WIND CHILL"
stormData2$EVTYPE[stormData2$EVTYPE=="COLD/WIND CHILL"] <- "EXTREME COLD/WIND CHILL"
stormData2$EVTYPE[stormData2$EVTYPE=="EXTREME COLD"] <- "EXTREME COLD/WIND CHILL"
stormData2$EVTYPE[stormData2$EVTYPE=="EXTREME WINDCHILL"] <- "EXTREME COLD/WIND CHILL"
stormData2$EVTYPE[stormData2$EVTYPE=="COLD"] <- "EXTREME COLD/WIND CHILL"
stormData2$EVTYPE[stormData2$EVTYPE=="RECORD COLD"] <- "EXTREME COLD/WIND CHILL"
stormData2$EVTYPE[stormData2$EVTYPE=="UNSEASONABLY COLD"] <- "EXTREME COLD/WIND CHILL"
stormData2$EVTYPE[stormData2$EVTYPE=="Cold"] <- "EXTREME COLD/WIND CHILL"
stormData2$EVTYPE[stormData2$EVTYPE=="UNUSUALLY COLD"] <- "EXTREME COLD/WIND CHILL"
stormData2$EVTYPE[stormData2$EVTYPE=="SNOW"] <- "HEAVY SNOW"
stormData2$EVTYPE[stormData2$EVTYPE=="Snow"] <- "HEAVY SNOW"
stormData2$EVTYPE[stormData2$EVTYPE=="EXCESSIVE SNOW"] <- "HEAVY SNOW"
stormData2$EVTYPE[stormData2$EVTYPE=="WIND"] <- "HIGH WIND"
stormData2$EVTYPE[stormData2$EVTYPE=="Heavy Rain"] <- "HEAVY RAIN"
stormData2$EVTYPE[stormData2$EVTYPE=="RECORD RAINFALL"] <- "HEAVY RAIN"
stormData2$EVTYPE[stormData2$EVTYPE=="RECORD WARMTH"] <- "HEAT"
stormData2$EVTYPE[stormData2$EVTYPE=="UNUSUAL WARMTH"] <- "HEAT"
stormData2$EVTYPE[stormData2$EVTYPE=="RECORD HEAT"] <- "HEAT"
stormData2$EVTYPE[stormData2$EVTYPE=="UNSEASONABLY WARM"] <- "HEAT"
stormData2$EVTYPE[stormData2$EVTYPE=="Winter Weather"] <- "WINTER WEATHER"
stormData2$EVTYPE[stormData2$EVTYPE=="FREEZE"] <- "FROST/FREEZE"
stormData2$EVTYPE   <- factor(stormData2$EVTYPE)

## Mutate to common numeric/dollar basis, incorporating exp variables. Rescaled to Millions of Dollars ($000,000)
stormData3 <- stormData2 %>%
  mutate(prop_damage = ifelse(PROPDMGEXP == "", PROPDMG*10^-6, 
    ifelse(PROPDMGEXP == "B", PROPDMG*10^3, 
    ifelse(PROPDMGEXP == "M", PROPDMG*10^0, 
    ifelse(PROPDMGEXP == "K", PROPDMG*10^-3,
    ifelse(PROPDMGEXP ==  0,   PROPDMG*10^-5, PROPDMG)))))) %>%
  mutate(crop_damage = ifelse(CROPDMGEXP == "", CROPDMG*10^-6, 
    ifelse(CROPDMGEXP == "B", CROPDMG*10^3, 
    ifelse(CROPDMGEXP == "M", CROPDMG*10^0, 
    ifelse(CROPDMGEXP == "K", CROPDMG*10^-3,
    ifelse(CROPDMGEXP ==  0,  CROPDMG*10^-5, CROPDMG))))))

## Checking Outliers & Correcting Entries
stormData3$prop_damage[stormData3$prop_damage == 1.15e+05] <- 115

## Correct Entry for Katrina
stormData3$LATITUDE[stormData3$prop_damage == 3.13e+04]  <- 2995
stormData3$LONGITUDE[stormData3$prop_damage == 3.13e+04] <- 9007
```

## Map of Major Storms in 50 States: Over $10M Property Damages  


```r
## For 50 States Only use 
stormData4 <- stormData3[(stormData3$STATE %in% state.abb),]

## Storms with over $10M Property Damage
library(leaflet)
stormData5 <- stormData4 %>% filter(LONGITUDE != 0) %>% 
  filter(prop_damage >= 1e+02) %>% mutate(lat = LATITUDE/100, 
    lng = -abs(LONGITUDE)/100) %>% 
  select(lat,lng, EVTYPE)

## Create Interactive Map 
stormDataMap1 <- stormData5  %>% leaflet() %>% addTiles() %>%  
  addMarkers(clusterOptions = markerClusterOptions(), popup = stormData5$EVTYPE)
stormDataMap1
```

<!--html_preserve--><div id="htmlwidget-a91d0d7cbee47429d933" style="width:864px;height:480px;" class="leaflet html-widget"></div>
<script type="application/json" data-for="htmlwidget-a91d0d7cbee47429d933">{"x":{"options":{"crs":{"crsClass":"L.CRS.EPSG3857","code":null,"proj4def":null,"projectedBounds":null,"options":{}}},"calls":[{"method":"addTiles","args":["//{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",null,null,{"minZoom":0,"maxZoom":18,"maxNativeZoom":null,"tileSize":256,"subdomains":"abc","errorTileUrl":"","tms":false,"continuousWorld":false,"noWrap":false,"zoomOffset":0,"zoomReverse":false,"opacity":1,"zIndex":null,"unloadInvisibleTiles":null,"updateWhenIdle":null,"detectRetina":false,"reuseTiles":false,"attribution":"&copy; <a href=\"http://openstreetmap.org\">OpenStreetMap<\/a> contributors, <a href=\"http://creativecommons.org/licenses/by-sa/2.0/\">CC-BY-SA<\/a>"}]},{"method":"addMarkers","args":[[33.29,35.23,35.26,39.53,38.06,31.06,33.54,33.23,25.48,36.59,44.17,44.47,44.49,44.48,45.01,44.48,36.04,37.29,35.18,35.24,33.24,40.44,38.56,38.47,38.38,38.47,38.46,41.16,41.16,46.5,46.5,35.17,29.25,38.33,35.2,35.24,39.44,40.3,38.27,38.28,38.05,29.95,45,30.28,37.51,33.17,38.05,38.28,38.27,30.19,30.28,40.01,40,29.5,43.28,31.16,31.58,37.23,41.04,41.01,30.34,40.13,39.09,39.12,41.48,42.09,34.38,34.59,35.4,33.27,43.04,30.3,35.45,32,34.32,37.45,37.44,32.54,39.36,39.39,31.43,42.07,32.42,36.03,36.04,41.33,41.43,37.43,33.31,33.16,33.24,35.35,35.02,34.39,34.52,33.49,33.4,33.38,31.22,33.21,32.45,34.06,33.03,37.03,44.57,48.58,48.58,34.3,35,36.58,39.52,42.06,42.06,39.45,45,45,41.04,40.42,40.42,43.13,41.04,40.13,42.06,42.05,40.13,42.05],[-112.04,-94.25,-94.21,-104.57,-85.44,-97.21,-93.2,-87.14,-80.17,-86.37,-94.1,-93.2,-93.19,-93.19,-93.09,-93.19,-86.55,-97.22,-97.36,-97.27,-94.44,-111.52,-76.59,-90.3,-90.27,-90.2,-90.15,-96,-96,-100.45,-100.54,-99,-98.3,-77.11,-97.32,-97.29,-104.56,-96.47,-122.53,-122.18,-122.42,-90.07,-93.29,-97.59,-111.33,-112.17,-122.42,-122.18,-122.53,-92.28,-92.42,-82.52,-82.36,-97.58,-89.28,-85.55,-84.27,-99.22,-81.31,-83.39,-98.16,-104.45,-85.53,-85.54,-91.51,-91.49,-99.19,-89.59,-88.51,-94.04,-88.54,-97.49,-86.51,-102.09,-94.19,-89.04,-89.34,-91.15,-104.42,-105.04,-106.16,-85.38,-90.4,-89.08,-86.37,-83.32,-87.32,-97.37,-111.51,-111.58,-112.18,-78.55,-79.04,-87.04,-92.1,-86.03,-86.31,-87.25,-90.14,-87.12,-85.52,-88.09,-87.49,-94.34,-93.21,-102.15,-102.15,-90.35,-90.18,-89.09,-90.3,-72.45,-72.45,-104.36,-72.33,-72.34,-74.33,-74.43,-74.43,-72.28,-74.33,-76.36,-76.15,-75.54,-76.36,-76.03],null,null,null,{"clickable":true,"draggable":false,"keyboard":true,"title":"","alt":"","zIndexOffset":0,"opacity":1,"riseOnHover":false,"riseOffset":250},["THUNDERSTORM WIND","TORNADO","TORNADO","HAIL","TORNADO","HAIL","TORNADO","TORNADO","TORNADO","HAIL","TORNADO","THUNDERSTORM WIND","HAIL","HAIL","TORNADO","THUNDERSTORM WIND","TORNADO","TORNADO","TORNADO","TORNADO","TORNADO","TORNADO","TORNADO","HAIL","HAIL","HAIL","HAIL","HAIL","HAIL","HAIL","HAIL","TORNADO","HAIL","TORNADO","TORNADO","TORNADO","HAIL","TORNADO","FLOOD","FLOOD","FLOOD","STORM SURGE/TIDE","THUNDERSTORM WIND","HAIL","FLOOD","THUNDERSTORM WIND","FLOOD","FLOOD","FLOOD","FLOOD","FLOOD","HAIL","HAIL","HAIL","HAIL","TORNADO","TORNADO","TORNADO","HAIL","FLASH FLOOD","FLASH FLOOD","TORNADO","FLOOD","FLASH FLOOD","FLOOD","FLOOD","THUNDERSTORM WIND","TORNADO","TORNADO","HAIL","FLASH FLOOD","HAIL","TORNADO","HAIL","TORNADO","THUNDERSTORM WIND","THUNDERSTORM WIND","THUNDERSTORM WIND","HAIL","HAIL","HAIL","HAIL","TORNADO","FLOOD","FLOOD","TORNADO","FLASH FLOOD","HAIL","HAIL","HAIL","HAIL","TORNADO","TORNADO","TORNADO","TORNADO","TORNADO","TORNADO","TORNADO","TORNADO","TORNADO","TORNADO","TORNADO","TORNADO","TORNADO","TORNADO","FLOOD","FLOOD","FLOOD","FLOOD","FLASH FLOOD","FLASH FLOOD","TORNADO","TORNADO","HAIL","FLOOD","FLOOD","FLOOD","FLOOD","FLOOD","FLASH FLOOD","FLOOD","FLOOD","FLOOD","FLOOD","FLOOD","FLOOD"],null,{"showCoverageOnHover":true,"zoomToBoundsOnClick":true,"spiderfyOnMaxZoom":true,"removeOutsideVisibleBounds":true,"spiderLegPolylineOptions":{"weight":1.5,"color":"#222","opacity":0.5},"freezeAtZoom":false},null,null,null,null]}],"limits":{"lat":[25.48,48.58],"lng":[-122.53,-72.28]}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

## Map of Major Storms in 50 States: Over 10 Fatalities  


```r
## Storms with over 10 Fatalities
library(leaflet)
stormData6 <- stormData4 %>% filter(LONGITUDE != 0) %>% 
  filter(FATALITIES >= 10) %>% mutate(lat = LATITUDE/100, 
    lng = -abs(LONGITUDE)/100) %>% 
  select(lat,lng, EVTYPE)

## Create Interactive Map 
stormDataMap2 <- stormData6  %>% leaflet() %>% addTiles() %>%  
  addMarkers(clusterOptions = markerClusterOptions(), popup = stormData5$EVTYPE)
stormDataMap2
```

<!--html_preserve--><div id="htmlwidget-ce60962e5d42979e4cfd" style="width:864px;height:480px;" class="leaflet html-widget"></div>
<script type="application/json" data-for="htmlwidget-ce60962e5d42979e4cfd">{"x":{"options":{"crs":{"crsClass":"L.CRS.EPSG3857","code":null,"proj4def":null,"projectedBounds":null,"options":{}}},"calls":[{"method":"addTiles","args":["//{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",null,null,{"minZoom":0,"maxZoom":18,"maxNativeZoom":null,"tileSize":256,"subdomains":"abc","errorTileUrl":"","tms":false,"continuousWorld":false,"noWrap":false,"zoomOffset":0,"zoomReverse":false,"opacity":1,"zIndex":null,"unloadInvisibleTiles":null,"updateWhenIdle":null,"detectRetina":false,"reuseTiles":false,"attribution":"&copy; <a href=\"http://openstreetmap.org\">OpenStreetMap<\/a> contributors, <a href=\"http://creativecommons.org/licenses/by-sa/2.0/\">CC-BY-SA<\/a>"}]},{"method":"addMarkers","args":[[34.31,30.49,33.23,28.42,28.14,34.23,35.08,35.18,35.24,33.03,31.1,35.31,37.57,36.07,28.57,37.23,36.55,36.27,36.04,34.25,33.4,36.05,34.03,34.26,33.21,34.18,34.23,34.06,33.03,37.03],[-92.31,-97.37,-87.14,-81.21,-81.29,-83.54,-97.51,-97.36,-97.27,-87.42,-84.16,-89.02,-87.33,-89.31,-81.35,-99.22,-94.37,-86.1,-86.37,-93.55,-86.31,-76.57,-88.26,-87.31,-87.12,-87.47,-85.58,-88.09,-87.49,-94.34],null,null,null,{"clickable":true,"draggable":false,"keyboard":true,"title":"","alt":"","zIndexOffset":0,"opacity":1,"riseOnHover":false,"riseOffset":250},["THUNDERSTORM WIND","TORNADO","TORNADO","HAIL","TORNADO","HAIL","TORNADO","TORNADO","TORNADO","HAIL","TORNADO","THUNDERSTORM WIND","HAIL","HAIL","TORNADO","THUNDERSTORM WIND","TORNADO","TORNADO","TORNADO","TORNADO","TORNADO","TORNADO","TORNADO","HAIL","HAIL","HAIL","HAIL","HAIL","HAIL","HAIL","HAIL","TORNADO","HAIL","TORNADO","TORNADO","TORNADO","HAIL","TORNADO","FLOOD","FLOOD","FLOOD","STORM SURGE/TIDE","THUNDERSTORM WIND","HAIL","FLOOD","THUNDERSTORM WIND","FLOOD","FLOOD","FLOOD","FLOOD","FLOOD","HAIL","HAIL","HAIL","HAIL","TORNADO","TORNADO","TORNADO","HAIL","FLASH FLOOD","FLASH FLOOD","TORNADO","FLOOD","FLASH FLOOD","FLOOD","FLOOD","THUNDERSTORM WIND","TORNADO","TORNADO","HAIL","FLASH FLOOD","HAIL","TORNADO","HAIL","TORNADO","THUNDERSTORM WIND","THUNDERSTORM WIND","THUNDERSTORM WIND","HAIL","HAIL","HAIL","HAIL","TORNADO","FLOOD","FLOOD","TORNADO","FLASH FLOOD","HAIL","HAIL","HAIL","HAIL","TORNADO","TORNADO","TORNADO","TORNADO","TORNADO","TORNADO","TORNADO","TORNADO","TORNADO","TORNADO","TORNADO","TORNADO","TORNADO","TORNADO","FLOOD","FLOOD","FLOOD","FLOOD","FLASH FLOOD","FLASH FLOOD","TORNADO","TORNADO","HAIL","FLOOD","FLOOD","FLOOD","FLOOD","FLOOD","FLASH FLOOD","FLOOD","FLOOD","FLOOD","FLOOD","FLOOD","FLOOD"],null,{"showCoverageOnHover":true,"zoomToBoundsOnClick":true,"spiderfyOnMaxZoom":true,"removeOutsideVisibleBounds":true,"spiderLegPolylineOptions":{"weight":1.5,"color":"#222","opacity":0.5},"freezeAtZoom":false},null,null,null,null]}],"limits":{"lat":[28.14,37.57],"lng":[-99.22,-76.57]}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

