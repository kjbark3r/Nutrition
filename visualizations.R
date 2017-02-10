~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# NUTRITIONAL CONSEQUENCES OF VARYING MIGRATORY BEHAVIORS #
#           -JUST VISUALIZATIONS-            #
#                    KRISTIN BARKER                       #
#                  FEB 2017                    #
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

~~~~~~~~~~~~~~~~~
####  Setup  ####
~~~~~~~~~~~~~~~~~


# PACKAGES #

library(tidyr)
library(ggplot2) # graphics
library(reshape2)
library(gridExtra) # >1 plot per display
library(pscl)
library(lme4)
library(rgl)
library(adehabitatHR)
library(raster)
library(rasterVis)
library(dplyr)

# WORKING DIRECTORY #

wd_workcomp <- "C:\\Users\\kristin.barker\\Documents\\GitHub\\Nutrition"
wd_laptop <- "C:\\Users\\kjbark3r\\Documents\\GitHub\\Nutrition"
if (file.exists(wd_workcomp)) {setwd(wd_workcomp)
} else {setwd(wd_laptop)}
rm(wd_workcomp, wd_laptop)

memory.limit(size = 7500000)

~~~~~~~~~~~~~~~~~
####  Data  ####
~~~~~~~~~~~~~~~~~

# read & prep elk locations (from Access DB, processed in ElkDatabase/dataprep.R)
locs <- read.csv("../ElkDatabase/collardata-locsonly-equalsampling.csv") %>%
  dplyr::select(c(AnimalID, Date, Time, Lat, Long, Sex)) %>%
  within(Date <- as.Date(Date, format = "%Y-%m-%d")) %>% #format date
  dplyr::filter(Sex == "Female") %>% #not using males for nutrition analysis
  mutate(IndivYr = ifelse(Date < "2015-01-01", 	 # add indiv id (elk-year)
						   paste(AnimalID, "-14", sep=""),
						   paste(AnimalID, "-15", sep="")))

# read in migratory status
migstatus <- read.csv("migstatus.csv")

# define projections
latlong <- CRS("+init=epsg:4326")
stateplane <- CRS("+init=epsg:2818")


###### RESIDENT ####

# subset resident example
res.win <- subset(locs, IndivYr == "140630-15") %>%
  filter(between(Date, as.Date("2015-02-15"), as.Date("2015-03-31"))) #winter
res.sum <- subset(locs, IndivYr == "140630-15") %>%
   subset(between(Date, as.Date("2015-07-15"), as.Date("2015-08-31"))) #summer 

#### create winter kde and attempt 3d plot ####
xy.win <- data.frame("x" = res.win$Long, "y" = res.win$Lat)
ll.win <- SpatialPointsDataFrame(xy.win, res.win, proj4string = latlong)
stpln.win <- spTransform(ll.win, stateplane)
kud.win <- kernelUD(stpln.win) #create kde 
vol.win <- getvolumeUD(kud.win) #create ud
rast.win <- raster(vol.win)
plot3D(rast.win) #hoooooolyfuckingshit

#### ditto summer ####
xy.sum <- data.frame("x" = res.sum$Long, "y" = res.sum$Lat)
ll.sum <- SpatialPointsDataFrame(xy.sum, res.sum, proj4string = latlong)
stpln.sum <- spTransform(ll.sum, stateplane)
kud.sum <- kernelUD(stpln.sum) #create kde 
vol.sum <- getvolumeUD(kud.sum) #create ud
rast.sum <- raster(vol.sum)
plot3D(rast.sum) #hoooooolyfuckingshit



###### MIGRANT ####

# subset migrant example
mig.win <- subset(locs, IndivYr == "150360-15") %>%
  filter(between(Date, as.Date("2015-02-15"), as.Date("2015-03-31"))) #winter
mig.sum <- subset(locs, IndivYr == "150360-15") %>%
   subset(between(Date, as.Date("2015-07-15"), as.Date("2015-08-31"))) #summer 

#### create winter kde and attempt 3d plot ####
xy.win <- data.frame("x" = mig.win$Long, "y" = mig.win$Lat)
ll.win <- SpatialPointsDataFrame(xy.win, mig.win, proj4string = latlong)
stpln.win <- spTransform(ll.win, stateplane)
kud.win <- kernelUD(stpln.win) #create kde 
vol.win <- getvolumeUD(kud.win) #create ud
rast.win <- raster(vol.win)
plot3D(rast.win) #hoooooolyfuckingshit

#### ditto summer ####
xy.sum <- data.frame("x" = mig.sum$Long, "y" = mig.sum$Lat)
ll.sum <- SpatialPointsDataFrame(xy.sum, mig.sum, proj4string = latlong)
stpln.sum <- spTransform(ll.sum, stateplane)
kud.sum <- kernelUD(stpln.sum) #create kde 

vol.sum <- getvolumeUD(kud.sum) #create ud
rast.sum <- raster(vol.sum)
plot3D(rast.sum) #hoooooolyfuckingshit

# beginning to work on plotting the acual overlap 
# need to feed the below data that includes both seasons for same indiv
vol <- kerneloverlaphr(kud.sum, method = "VI", percent = 95, conditional = TRUE)