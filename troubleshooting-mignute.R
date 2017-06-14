###########################################################
# Misc code related to migration-nutrition analyses #
#                    KRISTIN BARKER                       #
###########################################################


########################
#to make fake pretty nutrition map for WILD180 talk
loggdm14 <- raster("../Vegetation/pred2014.tif")
plot(loggdm14)
plot(hrs14, add = TRUE)

########################
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

########################
# extracting from >1 polygon
test <- extract(gdm14, hrs, method = "simple", small = TRUE, fun = sum)
  #only worked for 17 rows; all rest NA
    #maybe need to add na.rm argument? gets weird with sum fcn.
  #also doesn't retain IndivYr
    #df=TRUE could help?

########################
#total shitshow of a graph, but code is usable
# shows avgDE for each indiv each day, color-coded by MigStatus
ggplot(mignute.avg, 
       aes(DOY, AvgDE, colour = MigStatus)) +
       geom_line() +
       geom_point()

########################
# adding MigRank to dfs ####

test <- read.csv("mig-ndaysDE.csv") %>%
  transform(MigStatus = factor(MigStatus,
                        levels = c("Resident",
                                   "Intermediate",
                                   "Migrant"),
                            ordered = TRUE)) %>%
  mutate(nAdequate = nExc+nGood) %>%
  mutate(nInadequate = nMarg+nPoor) %>%
  mutate(MigRank = rank(-VI95, ties.method = "random"))
scatter.smooth(test$nAdequate ~ test$MigRank)
# cool, added in data prep and re-read in data for analysis

 #something like 
#transform(SprAORank = ave(SprAO, Year, FUN = function(x) rank(-x, ties.method = "average")),

########################
# attempting to model relationship bt vi95 and nute ####

# zero-infl bc of the migrant 0s
library(pscl) # zero-inflated model
mod <- zeroinfl(nAdequate ~ VI95, dist = "negbin", link = "logit",
                data = mignute.ndays)
# doesn't converge
summary(mod)
a <- predict(mod)
# but it does functionally do something . . .
resid <- resid(mod, type = "pearson")
dispersion <- sum(resid^2)/(nrow(mignute.ndays) - 5) #5=df
dispersion 
par(mfrow=c(1,1)); plot(resid ~ a) #yeesh

mod <- zeroinfl(nAdequate ~ VI95, dist = "poisson", link = "log",
                data = mignute.ndays)
# glm bc fuck it
hist(avgday.indiv$AvgDayDE)
mod2 <- glm(AvgDayDE ~ VI95, data = avgday.indiv)
summary(mod2)
par(mfrow=c(2,2)); plot(mod2)

########################
# rank migstatus sorting 1st by 95 then by 50 ####

test <- mig %>%
  mutate(MigRank = rank(order(-VI95, -VI50), ties.method = "random"))
# newp

test <- mig %>%
  mutate(MigRank = rank(c(-VI95, -VI50), ties.method = "random"))
# newp

test <- mig %>%
  mutate(MigRank = rank(-VI95, -VI50, ties.method = "random"))
# yup :)

# more than one mutated column in same argument?
test <- mig %>%
  mutate(MigRank = rank(-VI95, -VI50, ties.method = "random"),
         MigStatus = ifelse(VI50 > 0, "Resident",
                   ifelse(VI95 == 0, "Migrant",
                          "Intermediate"))) 
# yup.

# add in consideration of migstatus
# so residents and intermediates aren't ranked together
test <- mig %>%
  mutate(MigStatus = ifelse(VI50 > 0, "Resident",
                   ifelse(VI95 == 0, "Migrant",
                          "Intermediate")),
         MigRank = rank(MigStatus, -VI95, -VI50, 
                        ties.method = "random")) 
# nope
# first, sort by VI95, descending
# THEN, sort by VI50, descending
# wait, that should have been all i needed to do, hm
test <- test %>%
  dplyr::select(-MigRank) %>%
  arrange(desc(VI95))
# so far so good

test <- mig %>%
  arrange(desc(VI95, VI50)) %>%
  mutate(MigStatus = ifelse(VI50 > 0, "Resident",
                   ifelse(VI95 == 0, "Migrant",
                          "Intermediate")))

test <- mig %>%
  arrange(desc(VI50, VI95)) %>%
  mutate(MigStatus = ifelse(VI50 > 0, "Resident",
                   ifelse(VI95 == 0, "Migrant",
                          "Intermediate")))
# kill me.

test <- mig %>%
  arrange(desc(VI95)) %>%
  arrange(desc(VI50)) %>%
  mutate(MigStatus = ifelse(VI50 > 0, "Resident",
                   ifelse(VI95 == 0, "Migrant",
                          "Intermediate")),
         MigRank = row_number())
# only bummer is ties.method is basically animalid now
# bc thats how df was arranged originally
# but i consider that random enough


########################
# checking ski hill elk removal ####
# and fixing migranks accordingly

test <- mignutebod %>%
  dplyr::select(IndivYr, Location) %>%
  distinct()
# ski hill elk comprise 7 elk-years; n = 82 incl them
# ok verified removal works; just need to fix ranks now
test <- mignutebod %>%
  dplyr::select(IndivYr, MigRank) %>%
  distinct()

test <- mignutebod %>%
  dplyr::select(IndivYr, MigRank) %>%
  mutate(MigRank = arrange())

         
########################
# pie graph - proportion resident/intermediate/migrant ####
piedat <- mignute.ndays %>%
  group_by(MigStatus) %>%
  transmute(Prop = n()/nrow(mignute.ndays)) %>%
  ungroup() %>%
  distinct()
pie(piedat$Prop, labels = piedat$Prop)
gpie <- ggplot(piedat, 
               aes(x="", y=Prop, fill=MigStatus)) +
  coord_polar("y", start=0)
#eh screw it, i'll just throw piedat into excel


########################
# subsetting only lactating females for body condition ####

lac <- ifbf.nona %>%
  filter(LactStatus == "Yes")
ifbf.lac <- ggplot(data = lac, 
           aes(x = MigStatus, y = IFBF)) +
           geom_boxplot(aes(fill = MigStatus)) +
           labs(title = "IFBF")
# no actual switchers in this analysis
# but some elk classified intermed in one yr but not another
# changing to make them classified as res/mig
# and not be counted for 2 data points
# stand by while i slaughter my sample size
test <- lac %>%
  group_by(AnimalID) %>%
  distinct(IFBF) %>%
  ungroup()
# shit, going down to 10.
# looking anyway out of curiosity
test <- lac %>%
  group_by(AnimalID) %>%
  distinct(MigStatus) %>%
  ungroup() %>%
  mutate(NewStatus = ifelse(AnimalID == 140040, "Migrant", 
                     ifelse(AnimalID == 140120, "Migrant",
                     ifelse(AnimalID == 140400, "Resident",
                     ifelse(AnimalID == 140960, "Resident",
                     ifelse(AnimalID == 141060, "Resident",
                            paste(MigStatus))))))) %>%
  dplyr::select(-MigStatus) %>%
  left_join(ifbf.nona, by = "AnimalID") %>%
  dplyr::select(AnimalID, NewStatus, IFBF) %>%
  distinct() %>%
  transform(NewStatus = factor(NewStatus,
                        levels = c("Resident",
                                   "Intermediate",
                                   "Migrant"),
                            ordered = TRUE)) 
(ifbf.lac <- ggplot(data = test, 
           aes(x = NewStatus, y = IFBF)) +
           geom_boxplot(aes(fill = NewStatus)) +
           labs(title = "IFBF - Corrected"))
# compare uncorrected to corrected
grid.arrange(ifbf, ifbf.lac, nrow=2)
# essentially the same; removed some outliers
# running stats on corrected
ifbf2 <- aov(IFBF ~ NewStatus, data = test)
summary(ifbf2)
# sooo not significant


########################
# subsetting only nonlactating females for body condition ####

nolac <- ifbf.nona %>%
  filter(LactStatus == "No")
test2 <- nolac %>%
  group_by(AnimalID) %>%
  distinct(IFBF) %>%
  ungroup()
View(test2)
#27, sweet
test2 <- nolac %>%
  group_by(AnimalID) %>%
  distinct(MigStatus) %>%
  ungroup() %>%
  mutate(NewStatus = ifelse(AnimalID == 140050, "Resident", 
                     ifelse(AnimalID == 140060, "Resident",
                     ifelse(AnimalID == 140100, "Resident",
                     ifelse(AnimalID == 140560, "Migrant",
                     ifelse(AnimalID == 140630, "Resident",
                     ifelse(AnimalID == 140710, "Resident",
                     ifelse(AnimalID == 140850, "Migrant",
                     ifelse(AnimalID == 140910, "Migrant",
                     ifelse(AnimalID == 140940, "Resident",
                     ifelse(AnimalID == 140980, "Migrant",
                     ifelse(AnimalID == 141080, "Resident",
                     ifelse(AnimalID == 141100, "Resident",
                     ifelse(AnimalID == 141490, "Resident",
                            paste(MigStatus))))))))))))))) %>%
  dplyr::select(-MigStatus) %>%
  left_join(ifbf.nona, by = "AnimalID") %>%
  dplyr::select(AnimalID, NewStatus, IFBF) %>%
  distinct() %>%
  transform(NewStatus = factor(NewStatus,
                        levels = c("Resident",
                                   "Intermediate",
                                   "Migrant"),
                            ordered = TRUE)) 
ifbf.nolac <- ggplot(data = test2, 
           aes(x = NewStatus, y = IFBF)) +
           geom_boxplot(aes(fill = NewStatus)) +
           labs(title = "IFBF Non-lactators")
grid.arrange(ifbf.lac, ifbf.nolac, nrow=2)

########################
# checking whether DE differed much before study ####
# based on the predictive covariates that change year to year

library(raster)

#precip
precip13 <- raster("../Vegetation/writtenrasters/orig/2012-2013data/precip_2013.tif")
precip14 <- raster("../Vegetation/writtenrasters/covs2014/precip_14.tif")
precip15 <- raster("../Vegetation/writtenrasters/covs2015/precip_15.tif")
# match extents
precip13 <- crop(precip13, extent(precip14))
precip13 <- resample(precip13, precip14, "ngb")
summary(precip13)
summary(precip14)
par(mfrow=c(3,1))
plot(precip13, main = "2013"); plot(precip14, main = "2014")
plot(precip15, main = "2015")
# precip more similar 2013-2014 than 2013-2015 or 2014-2015

# ndvi_amp
ndvi13 <- raster("../Vegetation/writtenrasters/orig/2012-2013data/ndvi_amp_2013.tif")
ndvi14 <- raster("../Vegetation/writtenrasters/covs2014/ndvi_amp_14.tif")
ndvi15 <- raster("../Vegetation/writtenrasters/covs2015/ndvi_amp_15.tif")
# match extents
ndvi13 <- crop(ndvi13, extent(ndvi14))
ndvi13 <- resample(ndvi13, ndvi14, "ngb")
par(mfrow=c(3,1))
plot(ndvi13, main = "2013"); plot(ndvi14, main = "2014")
plot(ndvi15, main = "2015")
summary(ndvi13)
summary(ndvi14)
# ok ndvi_amp does look pretty different
# backcasting nute to 2013 to see whether diff ndvi_amps really change de
# (see de_model_backcast2013.R for details)


########################
# comparing migrant and resident FN ####
# [hail mary. . .]

fn <- read.csv("fecalnitrogen.csv") %>%
  na.omit()
fn$Mig <- tolower(fn$Mig)
boxplot(fn$PctN ~ fn$Mig)

fnres <- subset(fn, Mig == "resident")
fnmig <- subset(fn, Mig == "migratory")

fnt <- t.test(fnres$PctN, fnmig$PctN)
fnt
# migrant tends to be a little higher
# but not significantly so
# fucking cool


########################
# looking at HR size and density ####

par(mfrow=c(1,2))
plot(subset(hrs, grepl("14$", hrs@data$id)), main = "2014")
plot(subset(hrs, grepl("15$", hrs@data$id)), main = "2015")

# attempting to add migstatus

hrs@data$IndivYr <- hrs@data$id
hrs@data <- left_join(hrs@data, migstatus, by = "IndivYr")
# hoooly shit it worked! dplyr ftw

par(mfrow=c(1,2))
plot(subset(hrs, grepl("14$", hrs@data$id)), main = "2014",
     border = hrs@data$MigStatus)
plot(subset(hrs, grepl("15$", hrs@data$id)), main = "2015",
     border = hrs@data$MigStatus)

# seems kinda weird. checking out in arcmap
library(rgdal)
writeOGR(hrs, dsn = "../../NSERP/GIS/aOrganized/Shapefiles", 
         layer = "SummmerHRs", driver = "ESRI Shapefile",
         overwrite = TRUE)



########################
# making migstatus for ifbf be from 2014 ####

# LACTATING ELK body condition
lac <- ifbf.lac %>%
  filter(LactStatus == "Yes") %>% 
  arrange(IndivYr) %>% #so 2014 is 1st for ea indiv
  group_by(AnimalID) %>%
  mutate(NewStatus = first(MigStatus)) %>%
  ungroup() %>%
  dplyr::select(-MigStatus) %>%
  dplyr::select(AnimalID, NewStatus, IFBF) %>%
  distinct() %>%
  transform(NewStatus = factor(NewStatus,
                        levels = c("Resident",
                                   "Intermediate",
                                   "Migrant"),
                            ordered = TRUE))
#i may be fucking up all these data analyses
#but i can use dplyr pretty well now
#so i've got that going for me which is nice



#### just checking out mig frequency plots ####
m <- read.csv("migstatus.csv")
colnames(m)
par(mfrow=c(2,2))
hist(m$VI95); hist(m$VI50); hist(m$Dist); hist(m$MigRank)
sd(m$VI95)/mean(m$VI95)

########################################################################
########################################################################

# CUT CODE ####


# anova: ifbf of all elk
ifbf <- aov(IFBF ~ MigStatus, data = ifbf.nona)
summary(ifbf)
#insig

# anova: ifbf of lactators only
ifbf2 <- aov(IFBF ~ NewStatus, data = lac)
summary(ifbf2)

# anova: ifbf of nonlactators only
ifbf3 <- aov(IFBF ~ NewStatus, data = nolac)
summary(ifbf3)
