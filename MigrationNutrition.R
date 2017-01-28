###########################################################
#   PROCESSING AND COMBINING ALL DATA RELATED TO THE      #
# NUTRITIONAL CONSEQUENCES OF VARYING MIGRATORY BEHAVIORS #
#        -in preparation for actual analyses-             #
#                    KRISTIN BARKER                       #
#                 NOV 2016 / JAN 2017                     #
###########################################################


#################
####  Setup  ####
#################


# PACKAGES #

library(RODBC)
library(rgdal)
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
####  Body Condition & Pregnancy  ####
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

# pull lab id's and animal id's; connect animal ids to ifbf lab data
id <- sqlQuery(channel, paste("select AnimalID, LabID
                               from AnimalInfo"))
ifbf <- read.csv("ifbf.csv") %>%
  rename(LabID = SampleID)
bod <- ifbf %>%
  left_join(id, by = "LabID") %>%
  select(c(AnimalID, IFBF, preg_pspb, LactStatus, girthmass, adjmass, Age))
write.csv(bod, file = "bodycondition.csv", row.names=F)

# pull capture locations in order to remove ski hill elk from analysis
caploc <- sqlQuery(channel, paste("select AnimalID, Location
                               from AnimalInfo"))
write.csv(caploc, file = "capture-locations.csv", row.names=F)


##############################
####  Migratory Behavior  ####
##############################


# DATA #

vi95 <- read.csv("../Migration/HRoverlap/volumeintersection.csv")
vi50 <- read.csv("../Migration/HRoverlap/volumeintersection50.csv")
caploc <- read.csv("capture-locations.csv")

# tweak and clean; remove ski hill elk
mig <- vi95 %>%
  rename(VI95 = SprVI) %>%
  dplyr::select(-c(AnimalID, Sex)) %>% 
  left_join(vi50, by = "IndivYr") %>%
  rename(VI50 = SprVI) %>%
  dplyr::select(IndivYr, AnimalID, VI95, VI50) %>%
  left_join(caploc, by = "AnimalID") %>%
  filter(Location != "Ski Hill") %>% # remove Ski Hill elk (n=6)
  dplyr::select(-Location)
  

# CLASSIFICATION #

## rank strength of mig behavior, and discretize behavior
## using volume intersection of winter/summer KDEs
  # resident is any indiv whose CORES (50% UD) overlap between seasons
  # migrant is any indiv whose HRs (95% UD) overlap less than 5%
  # intermediate is everyone else
  # rank is sorted first by 95% VI, then by 50% VI

mig <- mig %>%
  arrange(desc(VI95)) %>%
  arrange(desc(VI50)) %>%
  mutate(MigRank = row_number(),
         MigStatus = ifelse(VI50 > 0, "Resident",
                     ifelse(VI95 == 0, "Migrant",
                            "Intermediate")))

write.csv(mig, file = "migstatus.csv", row.names=FALSE)


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

# projection definition
latlong <- CRS("+init=epsg:4326") # WGS84


# CALCULATIONS #

# 2014
smr14 <- locs %>%
  filter(Sex == "Female")  %>% # not using males for nutrition analysis
  subset(between(Date, as.Date("2014-07-01"), as.Date("2014-08-31"))) %>%
  subset(Time < 1400 | Time > 1800) # remove hottest times of day (bedding)
xy14 <- data.frame("x" = smr14$Long, "y" = smr14$Lat) # pull coords
spdf.ll14 <- SpatialPointsDataFrame(xy14, smr14, proj4string = latlong) #spatial
spdf14 <- spTransform(spdf.ll14, de14@crs) # match projection of de tifs
ext14 <- as.data.frame(extract(de14, spdf14)) #de from each foraging location
colnames(ext14) <- "DE"
ext14 <- cbind(smr14, ext14) #combine locations with extracted de data

nute14 <- ext14 %>%
  group_by(IndivYr, Date) %>%
  summarise(AvgDE = mean(DE, na.rm=T)) %>%
  ungroup()

# 2015
smr15 <- locs %>%
  filter(Sex == "Female")  %>% # not using males for nutrition analysis
  subset(between(Date, as.Date("2015-07-01"), as.Date("2015-08-31"))) %>%
  subset(Time < 1400 | Time > 1800) # remove hottest times of day (bedding)
xy15 <- data.frame("x" = smr15$Long, "y" = smr15$Lat)
spdf.ll15 <- SpatialPointsDataFrame(xy15, smr15, proj4string = latlong) #spatial
spdf15 <- spTransform(spdf.ll15, de15@crs) # match projection of de tifs
ext15 <- as.data.frame(extract(de15, spdf15)) #de from each foraging location
colnames(ext15) <- "DE"
ext15 <- cbind(smr15, ext15) #combine locations with extracted de data

nute15 <- ext15 %>%
  group_by(IndivYr, Date) %>%
  summarise(AvgDE = mean(DE, na.rm=T)) %>%
  ungroup() 


# COMBINE YEARS AND ADD NUTRITION CLASSIFICATION INFO #

nute <- rbind(nute14, nute15)
nute$DEclass <- ifelse(nute$AvgDE >= 2.9, "Excellent", #Cook FQ definitions
                ifelse(nute$AvgDE >= 2.75 & nute$AvgDE < 2.9, "Good",
                ifelse(nute$AvgDE > 2.40 & nute$AvgDE < 2.75, "Marginal",
                       "Poor")))
write.csv(nute, file = "avg-daily-de.csv", row.names=FALSE)

nutedays <- nute %>%
  group_by(IndivYr) %>%
  summarise(nExc = length(which(DEclass == "Excellent")),
            nGood = length(which(DEclass == "Good")),
            nMarg = length(which(DEclass == "Marginal")),
            nPoor = length(which(DEclass == "Poor"))) %>%
    ungroup()
write.csv(nutedays, file = "nClass-daily-de.csv", row.names=FALSE)


##################################
####  Forb Abundance Per Day  ####
##################################

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
gherb14 <- raster("../Vegetation/gherb2014.tif")
gherb15 <- raster("../Vegetation/gherb2015.tif")

# projection definition
latlong <- CRS("+init=epsg:4326") # WGS84


# CALCULATIONS #

# 2014
smr14 <- locs %>%
  filter(Sex == "Female")  %>% # not using males for nutrition analysis
  subset(between(Date, as.Date("2014-07-01"), as.Date("2014-08-31"))) %>%
  subset(Time < 1400 | Time > 1800) # remove hottest times of day (bedding)
xy14 <- data.frame("x" = smr14$Long, "y" = smr14$Lat) # pull coords
spdf.ll14 <- SpatialPointsDataFrame(xy14, smr14, proj4string = latlong) #spatial
spdf14 <- spTransform(spdf.ll14, gherb14@crs) # match projection of de tifs
ext14 <- as.data.frame(raster::extract(gherb14, spdf14)) #de from each foraging location
colnames(ext14) <- "gHerb"
ext14 <- cbind(smr14, ext14) #combine locations with extracted de data

habund14 <- ext14 %>%
  group_by(IndivYr, Date) %>%
  summarise(AvgGHerb = mean(gHerb, na.rm=T)) %>%
  ungroup()

# 2015
smr15 <- locs %>%
  filter(Sex == "Female")  %>% # not using males for nutrition analysis
  subset(between(Date, as.Date("2015-07-01"), as.Date("2015-08-31"))) %>%
  subset(Time < 1400 | Time > 1800) # remove hottest times of day (bedding)
xy15 <- data.frame("x" = smr15$Long, "y" = smr15$Lat)
spdf.ll15 <- SpatialPointsDataFrame(xy15, smr15, proj4string = latlong) #spatial
spdf15 <- spTransform(spdf.ll15, gherb15@crs) # match projection of de tifs
ext15 <- as.data.frame(raster::extract(gherb15, spdf15)) #de from each foraging location
colnames(ext15) <- "gHerb"
ext15 <- cbind(smr15, ext15) #combine locations with extracted de data

habund15 <- ext15 %>%
  group_by(IndivYr, Date) %>%
  summarise(AvgGHerb = mean(gHerb, na.rm=T)) %>%
  ungroup() 


# COMBINE YEARS AND ADD NUTRITION CLASSIFICATION INFO #

habund <- rbind(habund14, habund15)
write.csv(habund, file = "avg-daily-gHerb.csv", row.names=FALSE)


###################################
####  Shrub Abundance Per Day  ####
###################################


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
gshrub14 <- raster("../Vegetation/gshrub2014.tif")
gshrub15 <- raster("../Vegetation/gshrub2015.tif")

# projection definition
latlong <- CRS("+init=epsg:4326") # WGS84


# CALCULATIONS #

# 2014
smr14 <- locs %>%
  filter(Sex == "Female")  %>% # not using males for nutrition analysis
  subset(between(Date, as.Date("2014-07-01"), as.Date("2014-08-31"))) %>%
  subset(Time < 1400 | Time > 1800) # remove hottest times of day (bedding)
xy14 <- data.frame("x" = smr14$Long, "y" = smr14$Lat) # pull coords
spdf.ll14 <- SpatialPointsDataFrame(xy14, smr14, proj4string = latlong) #spatial
spdf14 <- spTransform(spdf.ll14, gshrub14@crs) # match projection of de tifs
ext14 <- as.data.frame(raster::extract(gshrub14, spdf14)) #de from each foraging location
colnames(ext14) <- "gShrub"
ext14 <- cbind(smr14, ext14) #combine locations with extracted de data

sabund14 <- ext14 %>%
  group_by(IndivYr, Date) %>%
  summarise(AvgGShrub = mean(gShrub, na.rm=T)) %>%
  ungroup()

# 2015
smr15 <- locs %>%
  filter(Sex == "Female")  %>% # not using males for nutrition analysis
  subset(between(Date, as.Date("2015-07-01"), as.Date("2015-08-31"))) %>%
  subset(Time < 1400 | Time > 1800) # remove hottest times of day (bedding)
xy15 <- data.frame("x" = smr15$Long, "y" = smr15$Lat)
spdf.ll15 <- SpatialPointsDataFrame(xy15, smr15, proj4string = latlong) #spatial
spdf15 <- spTransform(spdf.ll15, gshrub15@crs) # match projection of de tifs
ext15 <- as.data.frame(raster::extract(gshrub15, spdf15)) #de from each foraging location
colnames(ext15) <- "gShrub"
ext15 <- cbind(smr15, ext15) #combine locations with extracted de data

sabund15 <- ext15 %>%
  group_by(IndivYr, Date) %>%
  summarise(AvgGShrub = mean(gShrub, na.rm=T)) %>%
  ungroup() 


# COMBINE YEARS AND ADD NUTRITION CLASSIFICATION INFO #

sabund <- rbind(sabund14, sabund15)
write.csv(sabund, file = "avg-daily-gShrub.csv", row.names=FALSE)

# COMBINE BOTH ABUNDANCE MODELS #

abund <- habund %>%
  left_join(sabund, by = c("IndivYr", "Date")) 
write.csv(abund, "avg-daily-g.csv", row.names = FALSE)

###########################
####  Home Range Area  ####
###########################

# read in predicted de rasters (from Vegetation/de_model.R)
de14 <- raster("../Vegetation/DE2014.tif")
de15 <- raster("../Vegetation/DE2015.tif")

# define projections
latlong <- CRS("+init=epsg:4326")
stateplane <- CRS("+init=epsg:2818")

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


### CALCULATE AREA OF EACH INDIV HOME RANGE ###

xy <- data.frame("x" = locs$Long, "y" = locs$Lat)
ll <- SpatialPointsDataFrame(xy, locs, proj4string = latlong)
stpln <- spTransform(ll, stateplane)
kud <- kernelUD(stpln[,7]) #create kde for each indiv (7=IndivYr)
hrs <- getverticeshr(kud) #calculate kde areas
hr.a <- as.data.frame(hrs) %>%
  rename(IndivYr = id, HRarea = area)

## ADD MIG STATUS INFO AND EXPORT AS SHP ##

hrs@data$IndivYr <- hrs@data$id
hrs@data <- left_join(hrs@data, migstatus, by = "IndivYr") %>%
    mutate(Year = ifelse(grepl("-14", IndivYr), 2014, 2015))

par(mfrow=c(1,2))
plot(subset(hrs, grepl("14$", hrs@data$id)), main = "2014",
     border = hrs@data$MigStatus)
plot(subset(hrs, grepl("15$", hrs@data$id)), main = "2015",
     border = hrs@data$MigStatus)

writeOGR(hrs, dsn = "../../NSERP/GIS/aOrganized/Shapefiles", 
         layer = "SummmerHRs", driver = "ESRI Shapefile",
         overwrite = TRUE)


write.csv(hr.a, file = "homerangeareas.csv", row.names = FALSE)


##########################
####  Fecal Nitrogen  ####
##########################

pellets <- read.csv("FecalN-DefMigNonmig.csv")
fecaln <- read.csv("fecalnitrogen-raw.csv")

fn <- pellets %>%
  rename(SampleID = Sample_ID) %>%
  dplyr::select(-c(Date, Latitude, Longitude)) %>%
  left_join(fecaln, by = "SampleID") %>%
  dplyr::select(-c(SamplingYear, General.Location, Mig))

write.csv(fn, file = "fecalnitrogen.csv", row.names = FALSE)


###########################################################################


###############################
####  All Together Now...  ####
###############################

# data csvs from above
bod <- read.csv("bodycondition.csv") 
mig <- read.csv("migstatus.csv") 
nute <- read.csv("avg-daily-de.csv") 
nutedays <- read.csv("nClass-daily-de.csv")
abund <- read.csv("avg-daily-g.csv")
hr <- read.csv("homerangeareas.csv")

# combine forage quality and quantity
frg <- left_join(abund, nute, by = c("IndivYr", "Date")) %>%
  mutate(AvgGForage = AvgGHerb+AvgGShrub)


## per individual elk-year... ##

# nutrition as average daily DE exposure
mignute.avg <- mig %>% 
  right_join(frg, by = "IndivYr") %>% 
  filter(!is.na(MigRank)) %>% #remove Ski Hill elk
  left_join(bod, by = "AnimalID") %>%
  left_join(hr, by = "IndivYr") 
write.csv(mignute.avg, file = "mig-avgforage.csv", row.names=F)

# nutrition as number of days with each level of DE
mignute.ndays <- nutedays %>%
  right_join(mig, by = "IndivYr") %>%
  left_join(bod, by = "AnimalID") %>%
  left_join(hr, by = "IndivYr")
write.csv(mignute.ndays, file = "mig-ndaysDE.csv", row.names=F)
