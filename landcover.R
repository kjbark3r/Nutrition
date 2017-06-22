### ### ### ### ### ### ### ### ### ### ### ### ### ##
#        NUTRITION IN DIFFERENT LANDCOVER TYPES      #
#               NSERP - KRISTIN BARKER               #
#                     JUNE 2017                      #
### ### ### ### ### ### ### ### ### ### ### ### ### ##


## ## ## ## ## ## 
####  SETUP  ####
## ## ## ## ## ## 



#### packages ####
library(raster)
library(ggplot2)
library(dplyr)



#### working directory ####
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



#### data ####

# de per plot
fq.plot <- read.csv("../Vegetation/ALL_DE_data.csv") %>%
  filter(Area == "Nsapph") %>%
  within(Date <- as.Date(Date, format = "%m/%d/%Y"))

# landcover rasters
lc14 <- raster("../Vegetation/writtenrasters/uncropped/landcov_14.tif")
lc15 <- raster("../Vegetation/writtenrasters/uncropped/landcov_15.tif")
lcstk <- stack(lc14, lc15)

# landcover definitions
lc.raw <- read.csv("../Vegetation/landcov-classes.csv") 



## ## ## ## ## ## ## ## ## ## ## #
####  LANDCOVER TYPE PER PLOT ####
## ## ## ## ## ## ## ## ## ## ## #


# define projection
latlong <- CRS("+init=epsg:4326") # WGS84


# make plot data spatial & match raster projection
xy.plot <- data.frame("x" = fq.plot$Longitude, "y" = fq.plot$Latitude)
sp.ll.plot <- SpatialPointsDataFrame(xy.plot, fq.plot, proj4string = latlong)
sp.plot <- spTransform(sp.ll.plot, lc14@crs)


# extract landcover from rasters
ext <- as.data.frame(raster::extract(lcstk, sp.plot))


# combine valley bottom and montane riparian land cover types
# because much of the valley bottom is oddly classified as montane
lc <- lc.raw
lc$class_name <- as.factor(ifelse(grepl("Riparian", lc$class_name), "Riparian", 
                                  as.character(lc$class_name)))
lc$class_name <- relevel(lc$class_name, "Grass/Shrub/Open Woodland")


# select correct year per plot; make open grassland reference level
de.plot <- cbind(fq.plot, ext) %>%
  mutate(Landcov = ifelse(Date < "2015-01-01", landcov_14, landcov_15)) %>%
  select(PlotID, Date, Longitude, Latitude, DE, Landcov) %>%
  left_join(lc, by = c("Landcov" = "landcov")) %>%
  rename(LcNum = Landcov, Landcover = class_name) 
write.csv(de.plot, file = "de-per-plot.csv", row.names=F)


# estimated DE per landcover type
de.lc <- de.plot %>%
  group_by(Landcover) %>%
  summarise(MeanDE = mean(DE, na.rm=TRUE),
            StDev = sd(DE, na.rm=TRUE)) %>%
  ungroup()
write.csv(de.lc, "../Vegetation/de-landcov-NSapph-GENERALMODEL.csv", row.names=F)

# modelled DE per landcover type
mod <- glm(DE ~ Landcover, data = de.plot)
summary(mod)

# compare model with landcover to one without
mod2 <- update(mod, . ~ . - Landcover)
summary(mod2)
anova(mod, mod2)


# frequency distribution of DE, alone and per landcover type
hist(de.plot$DE)
hist(log(de.plot$DE))
summary(de.plot$DE)
count(de.plot, 'Landcover')
meanvals <- data.frame(tapply(de.plot$DE, de.plot$Landcover, mean))
tapply(de.plot$DE, de.plot$Landcover, length)




#### PLOTS ####

de.viz <- de.plot

# violin/boxplot #
de.viz$Landcover <- factor(de.viz$Landcover,
                        levels = c("Mesic Forest (Burn >15)",
                                   "Dry Forest Burn 6-15",
                                   "Mesic Forest Burn 6-15",
                                   "Dry Ag",
                                   "Mesic Forest Burn 0-5",
                                   "Grass/Shrub/Open Woodland",
                                   "Riparian",
                                   "Dry Forest (Burn >15)",
                                   "Rx Dry Forest Burn 0-5",
                                   "Dry Forest Burn 0-5",
                                   "Irrigated Ag"))
viol <- ggplot(de.viz, aes(y = DE, x = Landcover)) +
  geom_violin()
viol

# plot DE by landcover
de.lc <- de.plot %>%
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
                                                                  "Wet Forest - burned 6-15 years ago",
                                                                  ifelse(Landcover == "Dry Forest Burn 6-15",
                                                                         "Dry Forest - burned 6-15 years ago",
                                                                         ifelse(Landcover == "Mesic Forest (Burn >15)",
                                                                                "Wet forest - burned >15 years ago",
                                                                                ifelse(Landcover == "Dry Forest (Burn >15)",
                                                                                       "Dry Forest - burned >15 years ago",
                                                                                       paste(Landcover))))))))))) %>%
  group_by(Landcover) %>%
  summarise(Mean = mean(DE), Median = median(DE), n = n(), SD = sd(DE)) %>%
  ungroup()
de.lc$Landcover <- factor(de.lc$Landcover,
                       levels = de.lc$Landcover[order(de.lc$Mean)],
                       ordered = TRUE)

horiz <- ggplot(data = de.lc, 
                aes(y = Landcover, x = Mean,
                    xmin = Mean-2*SD,
                    xmax = Mean+2*SD)) +
  geom_point(size = 3) +
  geom_errorbarh() +
  geom_vline(xintercept = 2.75) +
  theme(text = element_text(size = 18)) +
  labs(x = "Forage Quality (kcal/g)", y = "") 
horiz
ggsave("de-landcov-horiz.jpg", plot = horiz, device = "jpeg",
       dpi = 300)