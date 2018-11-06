getwd()
F<-3 
G<-3
H<-F+G
H
A=2
A
a
A <- 200
ls()
rm(C)
ls()

Data1 <- c(1:100)
Data2 <- c(101:200)
plot(Data1,Data2,col="red")
Data3 <- rnorm(100, mean=53, sd=34)
Data4 <- rnorm(100, mean=64, sd=14)
plot(Data3,Data4, col="dark orange")
?c
df <- data.frame(Data1,Data2)
plot(df,col="green")
df[1:10,1]
df[1:13,1]
df[5:15,2]
df[,2]
colnames(df) <- c("column1",'column2')
df[["column1"]]

LondonDataOSK<-read.csv("LondonData.csv")
rm(A,B,F,G,H)
class(LondonDataOSK)
datatypelist <- data.frame(cbind(lapply(LondonDataOSK,class)))
LondonDataOSK<-read.csv("LondonData.csv")
LondonDataOSK <- edit(LondonDataOSK)
summary(df)
names(LondonDataOSK)
LondonBoroughs <- LondonDataOSK[626:658,]
LondonDataOSK <- data.frame(LondonDataOSK)
LondonBoroughs1 <- LondonDataOSK[grep("^E09",LondonDataOSK[,3]),]
head(LondonBoroughs1)
LondonBoroughs1 <- LondonBoroughs1[2:34,]
LondonBoroughs1 <- LondonBoroughs1[,c(1,19,20,21)]
names(LondonBoroughs1)[1] <- c("Borough Name")
plot(LondonBoroughs1$Male_life_expectancy_2009_to_2013,LondonBoroughs1$Percentage_children_in_reception_year_who_are_obese_2011and12_2013and14, col="red")
library(plotly)
install.packages("plotly")
install.packages("plotly","D:\\RProject1")
library(plotly,"D:\\RProject1\\plotly\\data")
plot_ly(LondonBoroughs, x = ~Male_life_expectancy_2009_to_2013, y = ~Percentage_children_in_reception_year_who_are_obese_2011and12_2013and14, text = ~LondonBoroughs1$`Borough Name`, type = "scatter", mode = "markers")


install.packages("maptools","D:\\RProject1")
install.packages(c("classint", "OpenStreetMap", "tmap"),"D:\\RProject1")
install.packages("RColorBrewer","D:\\RProject1")
install.packages("Sp","D:\\RProject1")
install.packages("rgeos","D:\\RProject1")
install.packages("tmap","D:\\RProject1")
install.packages("tmaptools","D:\\RProject1")
install.packages("sf","D:\\RProject1")
install.packages("downloader","D:\\RProject1")
install.packages("rgdal","D:\\RProject1")
install.packages("geojsonio","D:\\RProject1")
install.packages("OpenStreetMap","D:\\RProject1")
library(maptools)
library(RColorBrewer)
library(classInt)
library(OpenStreetMap)
library(sp)
library(rgeos)
library(tmap)
library(tmaptools)
library(sf)
library(rgdal)
library(geojsonio)
install.packages("OpenStreetMap","D:\\RProject1")


EW <- geojson_read("http://geoportal.statistics.gov.uk/datasets/8edafbe3276d4b56aec60991cbddda50_2.geojson", what = "sp")
install.packages("tidyverse","D:\\RProject1")
library(tidyverse)


LondonMap <- EW[grep("^E09",EW@data$lad15cd),]
qtm(LondonMap)
BoroughMapSF <- read_shape("C:\\Program Files (x86)\\ArcGIS\\Desktop10.6\\BoundryData\\england_lad_2011Polygon.shp", as.sf = TRUE)
BoroughMapSP <- LondonMap
qtm(BoroughMapSF)
qtm(BoroughMapSP)
library(methods)
class(BoroughMapSF)
class(BoroughMapSP)
newSF <- st_as_sf(BoroughMapSP)
newSP <- as(newSF, "Spatial")
BoroughMapSP <- as(BoroughMapSF, "Spatial")



BoroughMapSP@data <- data.frame(BoroughMapSP@data,LondonDataOSK[match(BoroughMapSP@data[,"code"],LondonDataOSK[,"New_code"]),])
head(BoroughMapSP@data)
BoroughDataMap <- append_data(BoroughMapSF,LondonDataOSK, key.shp = "code", key.data = "New_code", ignore.duplicates = TRUE)


library(tmap)
library(tmaptools)
tmap_mode("plot")

qtm(BoroughDataMap, fill = "Rate_of_JobSeekers_Allowance_JSA_Claimants_2015")

london_osm <- read_osm(BoroughDataMap, type = "esri", zoom = NULL)
qtm(london_osm) + 
  tm_shape(BoroughDataMap) + 
  tm_polygons("Rate_of_JobSeekers_Allowance_JSA_Claimants_2015", 
              style="jenks",
              palette="YlOrBr",
              midpoint=NA,
              title="Rate per 1,000 people",
              alpha = 0.5) + 
  tm_compass(position = c("left", "bottom"),type = "arrow") + 
  tm_scale_bar(position = c("left", "bottom")) +
  tm_layout(title = "Job seekers' Allowance Claimants", legend.position = c("right", "bottom"))

tm_shape(BoroughDataMap) +
  tm_polygons(c("Average_Public_Transport_Accessibility_score_2014", "Violence_against_the_person_rate_2014_to_15"), 
              style=c("jenks", "pretty"),
              palette=list("YlOrBr", "Purples"),
              auto.palette.mapping=FALSE,
              title=c("Average Public Transport Accessibility", "Violence Against the Person Rate"))

install.packages("shinyjs","D:\\RProject1")
library(shinyjs)
tmaptools::palette_explorer()

tmap_mode("view")
tm_shape(BoroughDataMap) +
  tm_polygons("Percentage_children_in_year6_who_are_obese_2011and12_2013and14",
              style="jenks",
              palette="PuRd",
              midpoint=NA,
              title="Truffle Shuffle Intensity")


ggplot()+geom_sf(mapping = aes(geometry=geometry),data = BoroughDataMap)+theme_minimal()
ggplot()+geom_sf(mapping = aes(geometry=geometry, fill=Median_House_Price_Pounds_2014),data = BoroughDataMap)+theme_minimal()

palette1<-scale_fill_continuous(low="white", high="red", "Price(£)")
labels<-labs(list(title="Average House Price 2014",x="Longitude", y="Latitude"))
ggplot()+geom_sf(mapping = aes(geometry=geometry, fill=Median_House_Price_Pounds_2014),data = BoroughDataMap)+theme_minimal()+palette1+labels



BoroughMapSP <- read_shape("C:\\Program Files (x86)\\ArcGIS\\Desktop10.6\\BoundryData\\england_lad_2011Polygon.shp", current.projection = 27700)
BoroughMapSF <- st_read("C:\\Program Files (x86)\\ArcGIS\\Desktop10.6\\BoundryData\\england_lad_2011Polygon.shp") %>% st_set_crs(27700)

UKBNG <- "+init=epsg:27700"
proj4string(BoroughMapSP) <- CRS(UKBNG)

install.packages("spTransform","D:\\RProject1")
install.packages("sp","D:\\RProject1")
library(raster)
library(sp)

print(BoroughMapSP)
BoroughMapSF <- BoroughMapSF %>% st_set_crs(27700)
print(BoroughMapSF)

proj4string(BoroughMapSP)
BoroughMapSPWGS84 <-spTransform(BoroughMapSP, CRS("+proj=longlat +datum=WGS84"))
print(BoroughMapSPWGS84)

BoroughMapSPBNG <-spTransform(BoroughMapSP, CRS(UKBNG))
print(BoroughMapSPBNG)

latlong <- "+init=epsg:4326"
BoroughMapSFWGS84 <- st_transform(BoroughMapSF, 4326)
print(BoroughMapSFWGS84)
library(spTransform)
library(rgdal)










