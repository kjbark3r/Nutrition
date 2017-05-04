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


~~~~~~~~~~~~~~~~~
####  ELK  ####
~~~~~~~~~~~~~~~~~

#### 3D kud images ####

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
plot3D(rast.sum) 



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



~~~~~~~~~~~~~~~~~
####  VEG  ####
~~~~~~~~~~~~~~~~~


#### nute per landcover ####


# data #
lc <- read.csv("../Vegetation/DE-model-data.csv")
de <- read.csv("de-by-landcover.csv")


# violin/boxplot #
lc$class_name <- factor(lc$class_name,
                        levels = c("Mesic Forest (Burn >15)",
                                   "Dry Forest Burn 6-15",
                                   "Mesic Forest Burn 6-15",
                                   "Dry Ag",
                                   "Mesic Forest Burn 0-5",
                                   "Grass/Shrub/Open Woodland",
                                   "Montane Riparian",
                                   "Dry Forest (Burn >15)",
                                   "Rx Dry Forest Burn 0-5",
                                   "Valley Bottom Riparian",
                                   "Dry Forest Burn 0-5",
                                   "Irrigated Ag"))
viol <- ggplot(lc, aes(y = DE, x = class_name)) +
  geom_violin() +
  geom_boxplot()
viol


# dotplot #
## now with programmatically defined factor orders! ##



de <- read.csv("de-by-landcover.csv")

de <- de %>%
  dplyr::rename(Landcover = class_name) %>%
  transform(Landcover = ifelse(Landcover == "Irrigated Ag",
                             "Irrigated Agricultural Land", 
                             ifelse(Landcover == "Rx Dry Forest Burn 0-5",
                                    "Dry Forest - recent prescribed burn",
                             ifelse(Landcover == "Dry Forest Burn 0-5",
                                    "Dry Forest - recent wildfire",
                             ifelse(Landcover == "Dry Ag",
                                    "Non-irrigated Agricultural Land",
                             ifelse(Landcover == "Mesic Forest Burn 0-5",
                                    "Wet Forest - recent wildfire",
                             ifelse(Landcover == "Mesic Forest Burn 6-15",
                                    "Wet Forest - mid-successional",
                             ifelse(Landcover == "Dry Forest Burn 6-15",
                                   "Dry Forest - mid-successional",
                             ifelse(Landcover == "Mesic Forest (Burn >15)",
                                    "Wet forest - late successional",
                             ifelse(Landcover == "Dry Forest (Burn >15)",
                                    "Dry Forest - late successional",
                                    paste(Landcover))))))))))) 
de$Landcover <- factor(de$Landcover,
                        levels = de$Landcover[order(de$mean)],
                        ordered = TRUE)


dot <- ggplot(data = de, 
              aes(y = mean, x = Landcover,
                  ymin = mean-2*sd,
                  ymax = mean+2*sd)) +
  geom_point(position = position_dodge(width = 0.2)) +
        geom_errorbar(position = position_dodge(width = 0.2), 
                      width = 0.1) +
  geom_hline(yintercept = 2.75) +
  geom_hline(yintercept = 2.9, linetype="dotted") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        text = element_text(size = 15)) +
  labs(x = "Land cover type", y = "Forage Quality (kcal/g)") 
dot
                  

horiz <- ggplot(data = de, 
              aes(y = Landcover, x = mean,
                  xmin = mean-2*sd,
                  xmax = mean+2*sd)) +
  geom_point(size = 3) +
        geom_errorbarh() +
  geom_vline(xintercept = 2.75) +
  theme(text = element_text(size = 18)) +
  labs(x = "Forage Quality (kcal/g)", y = "") 
horiz























