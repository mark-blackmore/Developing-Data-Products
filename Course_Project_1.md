# Major US Storms: 1996 - 2011
Mark Blackmore  
August 16, 2017  



# NOAA Storm Data: Download and Import

```r
library(tidyverse)
#url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
#download.file(url, destfile = "StormData.csv")
stormData <- read.csv("StormData.csv", stringsAsFactors = FALSE)
```

# Data Wrangling

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
```

# Map Major Storms in 50 States

```r
## For 50 States Only use 
stormData4 <- stormData3[(stormData3$STATE %in% state.abb),]

## Storms with High Property Damage
library(leaflet)
stormData5 <- stormData4 %>% filter(LONGITUDE != 0) %>% 
  filter(prop_damage >= 1e+02) %>% mutate(lat = LATITUDE/100, 
    lng = -abs(LONGITUDE)/100) %>% 
  select(lat,lng, EVTYPE)

## Create Interactive Map 
stormDataMap <- stormData5  %>% leaflet() %>% addTiles() %>%  
  addMarkers(clusterOptions = markerClusterOptions(), popup = stormData5$EVTYPE)
```

```
## Assuming 'lng' and 'lat' are longitude and latitude, respectively
```

```r
stormDataMap
```

<!--html_preserve--><div id="htmlwidget-942b89d62bd8e85f060e" style="width:672px;height:480px;" class="leaflet html-widget"></div>
<script type="application/json" data-for="htmlwidget-942b89d62bd8e85f060e">{"x":{"calls":[{"method":"addTiles","args":["http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",null,null,{"minZoom":0,"maxZoom":18,"maxNativeZoom":null,"tileSize":256,"subdomains":"abc","errorTileUrl":"","tms":false,"continuousWorld":false,"noWrap":false,"zoomOffset":0,"zoomReverse":false,"opacity":1,"zIndex":null,"unloadInvisibleTiles":null,"updateWhenIdle":null,"detectRetina":false,"reuseTiles":false,"attribution":"&copy; <a href=\"http://openstreetmap.org\">OpenStreetMap\u003c/a> contributors, <a href=\"http://creativecommons.org/licenses/by-sa/2.0/\">CC-BY-SA\u003c/a>"}]},{"method":"addMarkers","args":[[33.29,35.23,35.26,39.53,38.06,31.06,33.54,33.23,25.48,36.59,44.17,44.47,44.49,44.48,45.01,44.48,36.04,37.29,35.18,35.24,33.24,40.44,38.56,38.47,38.38,38.47,38.46,41.16,41.16,46.5,46.5,35.17,29.25,38.33,35.2,35.24,39.44,40.3,38.27,38.28,38.05,45,30.28,37.51,33.17,38.05,38.28,38.27,30.19,30.28,40.01,40,29.5,43.28,31.16,31.58,37.23,41.04,41.01,30.34,40.13,39.09,39.12,41.48,42.09,34.38,34.59,35.4,33.27,43.04,30.3,35.45,32,34.32,37.45,37.44,32.54,39.36,39.39,31.43,42.07,32.42,36.03,36.04,41.33,41.43,37.43,33.31,33.16,33.24,35.35,35.02,34.39,34.52,33.49,33.4,33.38,31.22,33.21,32.45,34.06,33.03,37.03,44.57,48.58,48.58,34.3,35,36.58,39.52,42.06,42.06,39.45,45,45,41.04,40.42,40.42,43.13,41.04,40.13,42.06,42.05,40.13,42.05],[-112.04,-94.25,-94.21,-104.57,-85.44,-97.21,-93.2,-87.14,-80.17,-86.37,-94.1,-93.2,-93.19,-93.19,-93.09,-93.19,-86.55,-97.22,-97.36,-97.27,-94.44,-111.52,-76.59,-90.3,-90.27,-90.2,-90.15,-96,-96,-100.45,-100.54,-99,-98.3,-77.11,-97.32,-97.29,-104.56,-96.47,-122.53,-122.18,-122.42,-93.29,-97.59,-111.33,-112.17,-122.42,-122.18,-122.53,-92.28,-92.42,-82.52,-82.36,-97.58,-89.28,-85.55,-84.27,-99.22,-81.31,-83.39,-98.16,-104.45,-85.53,-85.54,-91.51,-91.49,-99.19,-89.59,-88.51,-94.04,-88.54,-97.49,-86.51,-102.09,-94.19,-89.04,-89.34,-91.15,-104.42,-105.04,-106.16,-85.38,-90.4,-89.08,-86.37,-83.32,-87.32,-97.37,-111.51,-111.58,-112.18,-78.55,-79.04,-87.04,-92.1,-86.03,-86.31,-87.25,-90.14,-87.12,-85.52,-88.09,-87.49,-94.34,-93.21,-102.15,-102.15,-90.35,-90.18,-89.09,-90.3,-72.45,-72.45,-104.36,-72.33,-72.34,-74.33,-74.43,-74.43,-72.28,-74.33,-76.36,-76.15,-75.54,-76.36,-76.03],null,null,null,{"clickable":true,"draggable":false,"keyboard":true,"title":"","alt":"","zIndexOffset":0,"opacity":1,"riseOnHover":false,"riseOffset":250},["THUNDERSTORM WIND","TORNADO","TORNADO","HAIL","TORNADO","HAIL","TORNADO","TORNADO","TORNADO","HAIL","TORNADO","THUNDERSTORM WIND","HAIL","HAIL","TORNADO","THUNDERSTORM WIND","TORNADO","TORNADO","TORNADO","TORNADO","TORNADO","TORNADO","TORNADO","HAIL","HAIL","HAIL","HAIL","HAIL","HAIL","HAIL","HAIL","TORNADO","HAIL","TORNADO","TORNADO","TORNADO","HAIL","TORNADO","FLOOD","FLOOD","FLOOD","THUNDERSTORM WIND","HAIL","FLOOD","THUNDERSTORM WIND","FLOOD","FLOOD","FLOOD","FLOOD","FLOOD","HAIL","HAIL","HAIL","HAIL","TORNADO","TORNADO","TORNADO","HAIL","FLASH FLOOD","FLASH FLOOD","TORNADO","FLOOD","FLASH FLOOD","FLOOD","FLOOD","THUNDERSTORM WIND","TORNADO","TORNADO","HAIL","FLASH FLOOD","HAIL","TORNADO","HAIL","TORNADO","THUNDERSTORM WIND","THUNDERSTORM WIND","THUNDERSTORM WIND","HAIL","HAIL","HAIL","HAIL","TORNADO","FLOOD","FLOOD","TORNADO","FLASH FLOOD","HAIL","HAIL","HAIL","HAIL","TORNADO","TORNADO","TORNADO","TORNADO","TORNADO","TORNADO","TORNADO","TORNADO","TORNADO","TORNADO","TORNADO","TORNADO","TORNADO","TORNADO","FLOOD","FLOOD","FLOOD","FLOOD","FLASH FLOOD","FLASH FLOOD","TORNADO","TORNADO","HAIL","FLOOD","FLOOD","FLOOD","FLOOD","FLOOD","FLASH FLOOD","FLOOD","FLOOD","FLOOD","FLOOD","FLOOD","FLOOD"],{"showCoverageOnHover":true,"zoomToBoundsOnClick":true,"spiderfyOnMaxZoom":true,"removeOutsideVisibleBounds":true},null]}],"limits":{"lat":[25.48,48.58],"lng":[-122.53,-72.28]}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

