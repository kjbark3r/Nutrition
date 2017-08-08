###############################################################
## Summer and Winter Home Range Sizes of NSERP Adult Females ##
##                Kristin Barker - Jan 2017                  ##
###############################################################

#### SETUP ####

# WD

wd_workcomp <- "C:\\Users\\kristin.barker\\Documents\\GitHub\\Nutrition"
wd_laptop <- "C:\\Users\\kjbark3r\\Documents\\GitHub\\Nutrition"
wd_worklaptop <- "C:\\Users\\kristin\\Documents\\Nutrition"
if (file.exists(wd_workcomp)) {setwd(wd_workcomp)
} else {
  if(file.exists(wd_laptop)) {setwd(wd_laptop)
  } else {
    setwd(wd_worklaptop)
  }
}
rm(wd_workcomp, wd_laptop, wd_worklaptop)

# PACKAGES

library(dplyr)
library(adehabitatHR)
library(raster)
library(rgdal)

memory.limit(size = 7500000)

#### DATA & SETUP ####


# read & prep elk locations (from Access DB, processed in DatabasesEtc/dataprep.R)
locs <- read.csv("../DatabasesEtc/collardata-locsonly-equalsampling.csv") %>%
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
hrs <- getverticeshr(kud, unin = "m", unout = "km2") #calculate kde areas
hr.a <- as.data.frame(hrs) %>%
  rename(IndivYr = id, SumArea = area)

write.csv(hr.a, file = "homerangeareas.csv", row.names = FALSE)
sumraster <- raster(kud)
writeRaster(sumraster, paste("summerkdes"), format="GTiff", overwrite=TRUE)

####  REDO FOR WINTER TO MAKE PRESN MAPS ####
# read & prep elk locations (from Access DB, processed in DatabasesEtc/dataprep.R)
winlocs <- read.csv("../DatabasesEtc/collardata-locsonly-equalsampling.csv") %>%
  dplyr::select(c(AnimalID, Date, Time, Lat, Long, Sex)) %>%
  within(Date <- as.Date(Date, format = "%Y-%m-%d")) %>% #format date
  filter(Sex == "Female") %>% #not using males for nutrition analysis
  subset(between(Date, as.Date("2014-02-26"), as.Date("2014-03-31")) | #winter
         between(Date, as.Date("2015-02-15"), as.Date("2015-03-31"))) %>% 
  mutate(IndivYr = ifelse(Date < "2015-01-01", 	 # add indiv id (elk-year)
						   paste(AnimalID, "-14", sep=""),
						   paste(AnimalID, "-15", sep="")))
xy <- data.frame("x" = winlocs$Long, "y" = winlocs$Lat)
ll <- SpatialPointsDataFrame(xy, winlocs, proj4string = latlong)
stpln <- spTransform(ll, stateplane)
winkud <- kernelUD(stpln[,7]) #create kde for each indiv (7=IndivYr)
winhrs <- getverticeshr(winkud, unin = "m", unout = "km2")
hr.b <- as.data.frame(winhrs) %>%
  rename(IndivYr = id, WinArea = area)

winraster <- raster(winhrs)
writeRaster(winraster, paste("winterkdes"), format="GTiff", overwrite=TRUE)



#### Combine summer and winter HR data ####
# and check whether intermediates may be "expanders"
allhrs <- hr.a %>%
  full_join(hr.b, by = "IndivYr") %>%
  mutate(DiffArea = SumArea - WinArea ) 
summary(allhrs$DiffArea)
mig <- read.csv("migstatus.csv")
hrmig <- full_join(allhrs, mig, by = "IndivYr")
intmig <- filter(hrmig, MigStatus == "Intermediate")
summary(intmig$DiffArea) # nope, more like contracters

# sanity check to make sure they look reasonable
# with predicted de raster from Vegetation/de_model.R as background
de14 <- raster("../Vegetation/DE2014.tif")
plot(de14) 
plot(hrs, add = T, col = "red")
plot(winhrs, add = T, col = "blue")




