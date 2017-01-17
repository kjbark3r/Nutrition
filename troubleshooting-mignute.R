###########################################################
# Misc code related to migration-nutrition analyses #
#                    KRISTIN BARKER                       #
###########################################################


#to make fake pretty nutrition map for WILD180 talk
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

#total shitshow of a graph, but code is usable
# shows avgDE for each indiv each day, color-coded by MigStatus
ggplot(mignute.avg, 
       aes(DOY, AvgDE, colour = MigStatus)) +
       geom_line() +
       geom_point()