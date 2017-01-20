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
  mutate(MigRank = arrange()


