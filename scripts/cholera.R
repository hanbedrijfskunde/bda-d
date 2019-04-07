library(leaflet)
library(tidyr)
library(tidyverse)
library(htmltools)

## John Snow
snowDeaths <- read.csv("data/cholera.csv", stringsAsFactors = FALSE)
snowDeaths$geometry <- gsub("<.*?>", "", snowDeaths$geometry)
snowDeaths <- separate(data=snowDeaths, col=geometry, into=c("lng", "lat"), sep=",")
snowDeaths$lng <- as.numeric(snowDeaths$lng)
snowDeaths$lat <- as.numeric(snowDeaths$lat)
snowDeaths$type <- ifelse(snowDeaths$count == -999, "Waterpump", "Household")

summary(snowDeaths)

pump = makeIcon("img/pump.png",
                iconWidth = 38,
                iconHeight = 38)
ctr <- with(snowDeaths, c(mean(lng), mean(lat)))

m1 <- leaflet(snowDeaths, width = "800") %>%
  addTiles() %>%
  addCircles(lng = subset(snowDeaths, type == "Household")$lng,
             lat = subset(snowDeaths, type == "Household")$lat,
             radius = ~subset(snowDeaths, type == "Household")$count*1.5,
             color = "Red",
             label = ~paste("# deaths: ", snowDeaths$count)
  ) %>%
  setView(lng = ctr[1], lat = ctr[2], zoom = 16)

m2 <- m1 %>%
  addMarkers(lng = subset(snowDeaths, type == "Waterpump")$lng,
             lat = subset(snowDeaths, type == "Waterpump")$lat,
             icon = pump
  )