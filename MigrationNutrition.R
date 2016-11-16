###########################################################
# NUTRITIONAL CONSEQUENCES OF VARYING MIGRATORY BEHAVIORS #
#                    KRISTIN BARKER                       #
#                       NOV 2016                          #
###########################################################

#################
####  Setup  ####
#################


# PACKAGES #

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
rm(wd_workcomp, wd_laptop)


############################
####  Nutrition per HR  ####
############################

# DATA #

# elk locations
locs <- read.csv("../ElkDatabase/collardata-locsonly-equalsampling.csv") %>%
  dplyr::select(c(AnimalID, Date, Time, Lat, Long, Sex, EndDate)) 
locs$Date <- as.Date(locs$Date, format = "%Y-%m-%d")
locs$Time <- as.numeric(gsub("[[:punct:]]", "", locs$Time))
locs$IndivYr <- ifelse(locs$Date < "2015-01-01", 
                       paste(locs$AnimalID, "-14", sep=""),
                       paste(locs$AnimalID, "-15", sep=""))  

# gdm rasters
gdm14 <- raster("../Vegetation/GDM2014.tif")
gdm15 <- raster("../Vegetation/GDM2015.tif")

# projection definitions
latlong <- CRS("+init=epsg:4326") # elk locs - WGS84
stateplane <- gdm14@crs # rasters - NAD83(HARN) / Montana


# CALCULATIONS #

# 2014
smr14 <- locs %>%
  filter(Sex == "Female")  %>% # not using males for nutrition analysis
  subset(between(Date, as.Date("2014-07-15"), as.Date("2014-08-31"))) %>%
  subset(Time < 800 | Time > 1700) #remove mostly bedding locations
xy14 <- data.frame("x" = smr14$Long, "y" = smr14$Lat)
spdf.ll14 <- SpatialPointsDataFrame(xy14, smr14, proj4string = latlong) #spatial
spdf14 <- spTransform(spdf.ll14, stateplane) # match projection of gdm tifs
kuds14 <- kernelUD(spdf14[,8], h = "href", same4all = FALSE) #[,8] is IndivYr
hrs14 <- getverticeshr(kuds14) #home range outline ploygons
ext14 <- extract(gdm14, hrs14) #gdm in each hr
nute14 <- as.data.frame(
          sapply(ext14, function(x) 
            if (!is.null(x)){
                sum(x, na.rm = TRUE) #sum gdm per hr
            } else NA))
ids14 <- as.data.frame(unique(hrs14@data$id))
nute14 <- bind_cols(ids14, nute14) #add IndivYr
colnames(nute14) = c("IndivYr", "SumGDM")

# 2015
smr15 <- locs %>%
  filter(Sex == "Female")  %>% # not using males for nutrition analysis
  subset(between(Date, as.Date("2015-07-15"), as.Date("2015-08-31"))) %>%
  subset(Time < 800 | Time > 1700) #remove mostly bedding locations
xy15 <- data.frame("x" = smr15$Long, "y" = smr15$Lat)
spdf.ll15 <- SpatialPointsDataFrame(xy15, smr15, proj4string = latlong) #spatial
spdf15 <- spTransform(spdf.ll15, stateplane) # match projection of gdm tifs
kuds15 <- kernelUD(spdf15[,8], h = "href", same4all = FALSE) #[,8] is IndivYr
hrs15 <- getverticeshr(kuds15) #home range outline ploygons
ext15 <- extract(gdm15, hrs15) #gdm in each hr
nute15 <- as.data.frame(
          sapply(ext15, function(x) 
            if (!is.null(x)){
                sum(x, na.rm = TRUE) #sum gdm per hr
            } else NA))
ids15 <- as.data.frame(unique(hrs15@data$id))
nute15 <- bind_cols(ids15, nute15) #add IndivYr
colnames(nute15) = c("IndivYr", "SumGDM")

nute <- bind_rows(nute14, nute15)
write.csv(nute, file = "availnute.csv", row.names=FALSE)

###################################
####  Nutrition and Migration  ####
###################################

# DATA #

mig <- read.csv("../Survival/migstatus.csv")
nute <- read.csv("availnute.csv")
mignute <- mig %>%
  right_join(nute, by = "IndivYr") %>%
  transform(MigStatus = factor(MigStatus,
                        levels = c("Resident",
                                   "Intermediate",
                                   "Migrant"),
                            ordered = TRUE))
write.csv(mignute, file = "mignute.csv", row.names=FALSE)

# PLOTS #

scatter.smooth(mignute$SumGDM ~ I(mignute$VI95*-1),
               xlab = "Strength of Migratory Bahavior",
               ylab = "Available Nutrition")

ggplot(data = mignute, 
       aes(x = MigStatus, y = SumGDM)) +
       geom_boxplot(aes(fill = SumGDM))
  
# STATS #

hist(mignute$SumGDM) # normal enough i think

nut.mod <- lm(SumGDM ~ MigStatus, data = mignute)
summary(nut.mod)
anova(nut.mod)

# in progress...

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