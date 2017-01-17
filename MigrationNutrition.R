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


##############################
####  Migratory Behavior  ####
##############################


# DATA #

vi95 <- read.csv("../Migration/HRoverlap/volumeintersection.csv")
vi50 <- read.csv("../Migration/HRoverlap/volumeintersection50.csv")
mig <- vi95 %>%
  rename(VI95 = SprVI) %>%
  select(-c(AnimalID, Sex)) %>% 
  left_join(vi50, by = "IndivYr") %>%
  rename(VI50 = SprVI) %>%
  select(IndivYr, AnimalID, VI95, VI50) 


# CLASSIFICATION #

## discretize behavior using volume intersection of winter/summer KDEs
  # resident is any indiv whose CORES (50% UD) overlap between seasons
  # migrant is any indiv whose HRs (95% UD) never overlap
  # intermediate is everyone else

mig <- transform(mig, 
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

# projection definitions
latlong <- CRS("+init=epsg:4326") # WGS84


# CALCULATIONS #

# 2014
smr14 <- locs %>%
  filter(Sex == "Female")  %>% # not using males for nutrition analysis
  subset(between(Date, as.Date("2014-07-01"), as.Date("2014-08-31"))) %>%
  subset(Time <= 800 | Time >= 1800) #remove common bedding times
xy14 <- data.frame("x" = smr14$Long, "y" = smr14$Lat) # pull coords
spdf.ll14 <- SpatialPointsDataFrame(xy14, smr14, proj4string = latlong) #spatial
spdf14 <- spTransform(spdf.ll14, de14@crs) # match projection of de tifs
ext14 <- as.data.frame(extract(de14, spdf.ll14)) #de from each foraging location
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
  subset(Time <= 800 | Time >= 1800) #remove mostly bedding locations
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

#############################################
####  Nutrition, Migration, & Condition  ####
#############################################

# data csvs from above
#mig <- read.csv("migstatus.csv") 
#nute <- read.csv("avg-daily-de.csv") 
#nutedays <- read.csv("nClass-daily-de.csv")
#bod <- read.csv("elk-condition.csv") 

# nutrition as average daily DE exposure
mignutebod <- mig %>%
  right_join(nute, by = "IndivYr") %>%
  right_join(bod, by = "AnimalID")
write.csv(mignutebod, file = "mig-avgDE.csv", row.names=F)

# nutrition as number of days with each level of DE
bod$AnimalID <- as.character(bod$AnimalID) #otherwise won't join
mignutebod2 <- nutedays %>%
  right_join(mig, by = "IndivYr") %>%
  mutate(AnimalID = sub("\\-.*$",  "", IndivYr)) %>%
  right_join(bod, by = "AnimalID")
write.csv(mignutebod2, file = "mig-ndaysDE.csv", row.names=F)
