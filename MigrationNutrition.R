###########################################################
# NUTRITIONAL CONSEQUENCES OF VARYING MIGRATORY BEHAVIORS #
#                -DATA FILE CREATION-                     #
#                    KRISTIN BARKER                       #
#                 NOV 2016 / JAN 2017                     #
###########################################################

#################
####  Setup  ####
#################


# PACKAGES #

library(RODBC)
library(ggplot2)
library(adehabitatHR)
library(raster)
library(dplyr)


# WORKING DIRECTORY #

wd_workcomp <- "C:\\Users\\kristin.barker\\Documents\\GitHub\\Nutrition"
wd_laptop <- "C:\\Users\\kjbark3r\\Documents\\GitHub\\Nutrition"
if (file.exists(wd_workcomp)) {
  setwd(wd_workcomp)
  wd <- wd_workcomp
} else {
    setwd(wd_laptop)
    wd <- wd_laptop
}

######################################
####  Body condition & Pregnancy  ####
######################################

# DATABASE CONNECTION #

if (file.exists(wd_workcomp)) {
  channel <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};
                                dbq=C:/Users/kristin.barker/Documents/NSERP/Databases and Mort Reports/SapphireElkProject_ElkDatabase.accdb")
  } else {
      channel <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};
                                    dbq=C:/Users/kjbark3r/Documents/NSERP/Databases/SapphireElkProject_ElkDatabase.accdb")
    }
rm(wd_workcomp, wd_laptop)

# DATA #

id <- sqlQuery(channel, paste("select AnimalID, LabID
                               from AnimalInfo"))
fat <- sqlQuery(channel, paste("select LabID, RUMP, MAXFAT, Chest, girth, LactStatus
                               from BCSdataSapphires"))
preg <- sqlQuery(channel, paste("select LabID, PSPB, Age
                               from PregnancyData"))
bod <- fat %>%
  left_join(id, by = "LabID") %>%
  left_join(preg, by = "LabID") %>%
  select(c(AnimalID, Age, MAXFAT, RUMP, PSPB, Chest, girth, LactStatus))

write.csv(bod, file = "elk-condition.csv", row.names=F)


#####################################
####  Digestible Energy Per Day  ####
#####################################

# DATA #

# elk locations
locs <- read.csv("../ElkDatabase/collardata-locsonly-equalsampling.csv") %>%
  dplyr::select(c(AnimalID, Date, Time, Lat, Long, Sex)) 
locs$Date <- as.Date(locs$Date, format = "%Y-%m-%d")
locs$Time <- as.numeric(gsub("[[:punct:]]", "", locs$Time))
locs$IndivYr <- ifelse(locs$Date < "2015-01-01", 
                       paste(locs$AnimalID, "-14", sep=""),
                       paste(locs$AnimalID, "-15", sep=""))  

# predicted de rasters (from Vegetation/de_model.R)
de14 <- raster("../Vegetation/DE2014.tif")
de15 <- raster("../Vegetation/DE2015.tif")

# projection definitions
#latlong <- CRS("+init=epsg:4326") # elk locs - WGS84
#stateplane <- gdm14@crs # rasters - NAD83(HARN) / Montana


# CALCULATIONS #

# 2014
smr14 <- locs %>%
  filter(Sex == "Female")  %>% # not using males for nutrition analysis
  subset(between(Date, as.Date("2014-07-01"), as.Date("2014-08-31"))) %>%
  subset(Time < 800 | Time > 1700) #remove common bedding times
xy14 <- data.frame("x" = smr14$Long, "y" = smr14$Lat)
spdf.ll14 <- SpatialPointsDataFrame(xy14, smr14, proj4string = latlong) #spatial
#spdf14 <- spTransform(spdf.ll14, stateplane) # match projection of gdm tifs
ext14 <- as.data.frame(extract(de14, spdf.ll14)) #gdm from each foraging location
colnames(ext14) <- "DE"
ext14 <- cbind(smr14, ext14) #combine locations with extracted gdm data

nute14 <- ext14 %>%
  group_by(IndivYr, Date) %>%
  summarise(AvgDE14 = mean(DE, na.rm=T)) %>%
  ungroup() %>%
  group_by(IndivYr) %>%
  summarise(AvgDE = mean(AvgDE14, na.rm=T))

# 2015
smr15 <- locs %>%
  filter(Sex == "Female")  %>% # not using males for nutrition analysis
  subset(between(Date, as.Date("2015-07-01"), as.Date("2015-08-31"))) %>%
  subset(Time < 800 | Time > 1700) #remove mostly bedding locations
xy15 <- data.frame("x" = smr15$Long, "y" = smr15$Lat)
spdf.ll15 <- SpatialPointsDataFrame(xy15, smr15, proj4string = latlong) #spatial
spdf15 <- spTransform(spdf.ll15, stateplane) # match projection of gdm tifs
ext15 <- as.data.frame(extract(gdm15, spdf15)) #gdm from each foraging location
colnames(ext15) <- "GDM"
ext15 <- cbind(smr15, ext15) #combine locations with extracted gdm data

nute15 <- ext15 %>%
  group_by(IndivYr, Date) %>%
  summarise(AvgGDM15 = mean(GDM, na.rm=T)) %>%
  ungroup() %>%
  group_by(IndivYr) %>%
  summarise(AvgGDM = mean(AvgGDM15, na.rm=T))

nute <- rbind(nute14, nute15)
write.csv(nute, file = "avg-daily-gdm.csv", row.names=FALSE)

#############################################
####  Nutrition, Migration, & Condition  ####
#############################################

# DATA #

mig <- read.csv("../Migration/HRoverlap/migstatus.csv")
nute <- read.csv("avg-daily-gdm.csv") # skip if code run in full
bod <- read.csv("elk-condition.csv") # skip if code run in full

mignutebod <- mig %>%
  right_join(nute, by = "IndivYr") %>%
  right_join(bod, by = "AnimalID")

write.csv(mignutebod, file = "mig-nute-cndtn.csv", row.names=F)

############################################################################
## CUT CODE ####
# to tweak later, store somewhere else, or delete entirely #
############################################################################

#to make fake pretty  map for WILD180 talk
loggdm14 <- raster("../Vegetation/pred2014.tif")
plot(loggdm14)
plot(hrs14, add = TRUE)

#works, but can't figure out how to subset hrs by year later
smr <- locs %>%
  filter(Sex == "Female")  %>% # not using males for nutrition analysis
  subset(between(Date, as.Date("2014-07-15"), as.Date("2014-08-31")) #summer 
        |between(Date, as.Date("2015-07-15"), as.Date("2015-08-31"))) %>%
  subset(Time < 800 | Time > 1700) #remove mostly bedding locations
xy <- data.frame("x" = smr$Long, "y" = smr$Lat)
spdf.ll <- SpatialPointsDataFrame(xy, smr, proj4string = latlong) #spatial
spdf <- spTransform(spdf.ll, stateplane) # match projection of gdm tifs
kuds <- kernelUD(spdf[,8], h = "href", same4all = FALSE) #[,8] is IndivYr
hrs <- getverticeshr(kuds) #home range outline ploygons

# extracting from >1 polygon
test <- extract(gdm14, hrs, method = "simple", small = TRUE, fun = sum)
  #only worked for 17 rows; all rest NA
    #maybe need to add na.rm argument? gets weird with sum fcn.
  #also doesn't retain IndivYr
    #df=TRUE could help?