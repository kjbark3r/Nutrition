###############################################################
## Summer and Winter Home Range Sizes of NSERP Adult Females ##
##                Kristin Barker - Jan 2017                  ##
###############################################################

#### SETUP ####

# WD

wd_workcomp <- "C:\\Users\\kristin.barker\\Documents\\GitHub\\Nutrition"
wd_laptop <- "C:\\Users\\kjbark3r\\Documents\\GitHub\\Nutrition"
if (file.exists(wd_workcomp)) {
  setwd(wd_workcomp)
} else {
  if(file.exists(wd_laptop)) {
    setwd(wd_laptop)
  } else {
    cat("Are you SURE you got that file path right?\n")
  }
}
rm(wd_workcomp, wd_laptop)

# PACKAGES

library(dplyr)
library(adehabitatHR)
library(raster)
library(rgdal)

memory.limit(size = 7500000)

#### DATA & SETUP ####

# read in predicted de rasters (from Vegetation/de_model.R)
de14 <- raster("../Vegetation/DE2014.tif")
de15 <- raster("../Vegetation/DE2015.tif")


# read & prep elk locations (from Access DB, processed in ElkDatabase/dataprep.R)
locs <- read.csv("../ElkDatabase/collardata-locsonly-equalsampling.csv") %>%
  dplyr::select(c(AnimalID, Date, Time, Lat, Long, Sex)) %>%
  within(Date <- as.Date(Date, format = "%Y-%m-%d")) %>% #format date
  filter(Sex == "Female") %>% #not using males for nutrition analysis
  subset(between(Date, as.Date("2014-07-01"), as.Date("2014-08-31")) | #summer
         between(Date, as.Date("2015-07-01"), as.Date("2015-08-31"))) %>% 
  mutate(IndivYr = ifelse(Date < "2015-01-01", 	 # add indiv id (elk-year)
						   paste(AnimalID, "-14", sep=""),
						   paste(AnimalID, "-15", sep="")))

# define projections
latlong <- CRS("+init=epsg:4326")
stateplane <- CRS("+init=epsg:2818")


#### CALCULATE AREA OF EACH INDIV HOME RANGE ####

xy <- data.frame("x" = locs$Long, "y" = locs$Lat)
ll <- SpatialPointsDataFrame(xy, locs, proj4string = latlong)
stpln <- spTransform(ll, stateplane)
kud <- kernelUD(stpln[,7]) #create kde for each indiv (7=IndivYr)
hrs <- getverticeshr(kud) #calculate kde areas
hr.a <- as.data.frame(hrs) %>%
  rename(IndivYr = id, HRarea = area)

write.csv(hr.a, file = "homerangeareas.csv", row.names = FALSE)
#sumraster <- raster(kud)
#writeRaster(sumraster, paste("summerkdes"), format="GTiff", overwrite=TRUE)

####  REDO FOR WINTER TO MAKE PRESN MAPS ####
# read & prep elk locations (from Access DB, processed in ElkDatabase/dataprep.R)
winlocs <- read.csv("../ElkDatabase/collardata-locsonly-equalsampling.csv") %>%
  dplyr::select(c(AnimalID, Date, Time, Lat, Long, Sex)) %>%
  within(Date <- as.Date(Date, format = "%Y-%m-%d")) %>% #format date
  filter(Sex == "Female") %>% #not using males for nutrition analysis
  subset(between(Date, as.Date("2014-02-26"), as.Date("2014-03-31")) | #summer
         between(Date, as.Date("2015-02-15"), as.Date("2015-03-31"))) %>% 
  mutate(IndivYr = ifelse(Date < "2015-01-01", 	 # add indiv id (elk-year)
						   paste(AnimalID, "-14", sep=""),
						   paste(AnimalID, "-15", sep="")))
xy <- data.frame("x" = winlocs$Long, "y" = winlocs$Lat)
ll <- SpatialPointsDataFrame(xy, winlocs, proj4string = latlong)
stpln <- spTransform(ll, stateplane)
winkud <- kernelUD(stpln[,7]) #create kde for each indiv (7=IndivYr)
winhrs <- getverticeshr(winkud)

plot(de14)
plot(hrs, add = T)
plot(winhrs, add = T, col = "blue")

winraster <- raster(winhrs)
writeRaster(winraster, paste("winterkdes"), format="GTiff", overwrite=TRUE)
