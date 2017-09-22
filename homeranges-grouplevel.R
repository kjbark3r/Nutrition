### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ## #
## Calculating proportion of each landcover type available in ##
## population-level summer home range of NSERP adult females  ##
##                  Kristin Barker - Sept 2017                ##
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ## #


#### setup ####

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

library(adehabitatHR)
library(raster)
library(rgdal)
library(sp)
library(dplyr)

memory.limit(size = 7500000)



#### data ####


# migratory behavior status per individual (from... one of the nute analysis .R's, this repo)
migstatus <- read.csv("migstatus.csv") %>%
  dplyr::select(IndivYr, MigStatus)


# read & prep elk locations (from Access DB, processed in DatabasesEtc/dataprep.R)
locs <- read.csv("../DatabasesEtc/collardata-locsonly-equalsampling.csv") %>%
  dplyr::select(c(AnimalID, Date, Time, Lat, Long, Sex)) %>%
  within(Date <- as.Date(Date, format = "%Y-%m-%d")) %>% #format date
  filter(Sex == "Female") %>% #not using males for nutrition analysis
  subset(between(Date, as.Date("2014-07-01"), as.Date("2014-08-31")) | #summer
         between(Date, as.Date("2015-07-01"), as.Date("2015-08-31"))) %>% 
  mutate(IndivYr = ifelse(Date < "2015-01-01", 	 # add indiv id (elk-year)
						   paste(AnimalID, "-14", sep=""),
						   paste(AnimalID, "-15", sep=""))) %>%
  inner_join(migstatus, by = "IndivYr") # inner_join to remove ski hill elk


# bring in landcover types
lc <- raster("../Vegetation/writtenrasters/uncropped/landcov_14.tif")
plot(lc) 
covtypes <- read.csv("../Vegetation/landcov-classes.csv")

# define projections
latlong <- CRS("+init=epsg:4326")
stateplane <- CRS("+init=epsg:2818")



#### summer kde for entire population ####

xy <- data.frame("x" = locs$Long, "y" = locs$Lat)
ll <- SpatialPointsDataFrame(xy, locs, proj4string = latlong)
stpln <- spTransform(ll, stateplane)
kud.all <- kernelUD(stpln) 
kud99 <- getverticeshr(kud.all, percent = 99)
lc.crop <- crop(lc, kud99)
lc.crop <- mask(lc, kud99)
plot(lc.crop)


#### proportion of each habitat type ####

ppndat <- data.frame(freq(lc.crop)) %>%
  filter(!is.na(value)) %>%
  rename(landcov = value,
         total = count) %>%
  full_join(covtypes, by = "landcov") %>%
  mutate(ppn = total/sum(total))
  
# sanity check
sum(ppndat$total)
ppndat$total[1]
ppndat$total[1]/sum(ppndat$total)
ppndat$ppn[1]
sum(ppndat$ppn)

write.csv(ppndat, file = "landcov-ppns-smr.csv", row.names = FALSE)


#### summer kde for each migratory behavior ####
# to determine whether this helps with discussion of relative density #

# make spatial and covert to stateplane projection
xy <- data.frame("x" = locs$Long, "y" = locs$Lat)
ll <- SpatialPointsDataFrame(xy, locs, proj4string = latlong)
stpln <- spTransform(ll, stateplane)
# estimate summer ud and calculate area of 95% KDE
kud.behavs <- kernelUD(stpln[,8]) 
hrs <- getverticeshr(kud.behavs, percent = 95, 
                     unin = "m", unout = "km2") #calculate kde areas
hr.a <- as.data.frame(hrs) %>%
  rename(Behav = id, SumArea = area) 
View(hr.a)


### ### ### ##
#### CUTS ####
### ### ### ##



    
#### older code that made separate kdes (need estUD, not estUDm, to store as raster) ####

    # prep loop 
    behavs <- as.character(unique(locs$MigStatus))
    n <- length(behavs)
    
    ## loop creating separate kde for each behavior
    for (i in 1:n) {
      behav <- behavs[i]
      # for ea behav
      grp <- filter(locs, MigStatus == behav)
      # make spatial and covert to stateplane projection
      xy <- data.frame("x" = grp$Long, "y" = grp$Lat)
      ll <- SpatialPointsDataFrame(xy, grp, proj4string = latlong)
      stpln <- spTransform(ll, stateplane)
      # estimate summer kde and save as raster
      kud.behav <- kernelUD(stpln) 
      rast.behav <- raster(kud.behav)
      writeRaster(rast.behav, file = paste0("smrkde-", behav, ".tif"), overwrite = TRUE)
      # calculate and store area of 95% kde 
      hrs <- getverticeshr(kud.behav, unin = "m", unout = "km2") #calculate kde areas
      hr.a <- as.data.frame(hrs) %>%
        rename(Behav = id, SumArea = area)
    }
    
    # read rasters back in (since you didn't figure out how to store them separately)
    mig <- raster(paste0("smrkde-", behavs[1], ".tif"))
    int <- raster(paste0("smrkde-", behavs[2], ".tif"))
    res <- raster(paste0("smrkde-", behavs[3], ".tif"))




