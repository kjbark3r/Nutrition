###########################################################
# NUTRITIONAL CONSEQUENCES OF VARYING MIGRATORY BEHAVIORS #
#                    KRISTIN BARKER                       #
#                       NOV 2016                          #
###########################################################

#################
####  Setup  ####
#################


# PACKAGES #

library(adehabitatHR)
library(raster)
library(dplyr)


# WORKING DIRECTORY #

wd_workcomp <- "C:\\Users\\kristin.barker\\Documents\\GitHub\\Nutrition"
wd_laptop <- "C:\\Users\\kjbark3r\\Documents\\UMT\\Thesis\\Migration_Consequences\\Analyses\\Nutrition"
if (file.exists(wd_workcomp)) {
  setwd(wd_workcomp)
  wd <- wd_workcomp
} else {
    setwd(wd_laptop)
    wd <- wd_laptop
}
rm(wd_workcomp, wd_laptop)


# DATA #

# elk locations
locs <- read.csv("../ElkDatabase/collardata-locsonly-equalsampling.csv") %>%
  dplyr::select(c(AnimalID, Date, Time, Lat, Long, Sex, EndDate)) %>%
  filter(Sex == "Female") # not using males for nutrition analysis
locs$Date <- as.Date(locs$Date, format = "%Y-%m-%d")
locs$IndivYr <- ifelse(locs$Date < "2015-01-01", 
                       paste(locs$AnimalID, "-14", sep=""),
                       paste(locs$AnimalID, "-15", sep=""))  
locs14 <- subset(locs, Date < "2015-01-01")
locs15 <- subset(locs, Date >= "2015-01-01")

# migratory behavior
mig <- read.csv("../Migration/HRoverlap/volumeintersection.csv")

# gdm rasters
gdm14 <- raster("../Vegetation/GDM2014.tif")
gdm15 <- raster("../Vegetation/GDM2015.tif")


# projection definitions
latlong <- CRS("+init=epsg:4326") # elk locs (WGS84)
stateplane <- gdm14@crs # rasters (NAD83(HARN) / Montana)

############################
####  Nutrition per HR  ####
############################

#kernelUD() #can do for all indivs at once from spdf, then store all outlines with getverticeshr
 # i think it knows indiv based on spdf having just one extra non-locn id column
	# tweak the below accordingly


hr.gdm <- data.frame(matrix(ncol = 3, # df to store results
                      nrow = length(unique(locs$IndivYr))*nrow(rdat)))
colnames(hr.gdm) <- c("IndivYr", "GDM", "HRarea")

indivlist <- unique(locs$IndivYr)

for(i in 1:nrow(indivlist)) { 
  elk = indivlist[i]
  # create summer home range
  sub <- subset(locs, IndivYr == elk) %>% # pull indiv locs
    dplyr::select(c(IndivYr, Lat, Long)) # only use pertinent info
  xy <- data.frame("x" = sub$Long, "y" = sub$Lat) # make it spatial
  nad <- spTransform(SpatialPointsDataFrame(xy, sub, 
                       proj4string = latlong), stateplane) # match raster proj
  kud <- kernelUD(nad[,1]) # create kde for each indiv
  hr <- getverticeshr(kud) # create polygon outlines from kdes
  hr$id <- as.character(hr$id) # to populate final df properly
  # read in associated gdm info
  dat <- gsub(".*/", "", sub$date) # extract year from date
  gdm <- ifelse(dat == 2014, gdm2014, gdm2015)
  # calculate total gdm per indiv home range
  hr.tmp <- data.frame(matrix(ncol = 4, # store results from each loop
                              nrow = length(indivlist)))
  colnames(hr.tmp) <- c("IndivYr", "NDVI", "HRarea", "TimePd")
  for(j in 1:length(indivlist)) {
    indivyr <- indivlist[j] 
    sub <- subset(hr, id == indivyr) # subset individual home range polygon
    if(nrow(sub) < 1) { # ignore null polygons (if no locs in timestep)
      next
    } else {
      tmp <- extract(ndvi, sub, weights=TRUE, fun=sum) # weighted avg
    hr.tmp[j,1] <- sub$id
    hr.tmp[j,2] <- tmp
    hr.tmp[j,3] <- sub$area
    hr.tmp[j,4] <- date}
  }
  hr.gdm <- bind_rows(hr.tmp, hr.ndvi) # add results to final df
}

hr.gdm <- hr.gdm[complete.cases(hr.gdm),] # rm na rows (no locs in timestep)

write.csv(hr.gdm, file = "hr-gdm.csv", row.names = FALSE)
