### Floral resource data cleaning and exploration
### started on May 24, 2022,
### Jens U and Erin M

# check that wd is all good
getwd()

# Installed tidyverse, as I had just redownloaded R and did not have it installed! Ensured that ggplot2 was in my library.
library(tidyverse)
library(ggplot2)
library(gtools)
library(lmerTest)
library(lme4)
library(lubridate) # for reading dates

## read data (2021 only)
# quadrat floral resource data from 2021
quadrat_data <- read.csv("./quadrat_flower_surveys_2021.csv")
 
# hectare floral resource data from 2021
hectare_data <- read.csv("./hectare_floral_surveys_2021.csv")

# data structure
str(quadrat_data)
str(hectare_data)

# recode factors as factors and numeric as numeric
quadrat_data$SITE <- as.factor(quadrat_data$SITE)
quadrat_data$SPECIES <- as.factor(quadrat_data$SPECIES)
quadrat_data$ROUND <- as.factor(quadrat_data$ROUND)
quadrat_data$NUM_FLORAL_UNITS <- as.numeric(quadrat_data$NUM_FLORAL_UNITS)

hectare_data$SITE <- as.factor(hectare_data$SITE)
hectare_data$SPECIES <- as.factor(hectare_data$SPECIES)
hectare_data$ROUND <- as.factor(hectare_data$ROUND)
hectare_data$NUM_FLORAL_UNITS <- as.numeric(hectare_data$NUM_FLORAL_UNITS)

# check levels
levels(hectare_data$SPECIES)
levels(hectare_data$SITE)

# Next, add treatment column to the hectare data and quadrat data.
# And both site and species as factors.
# 0 == unmowed/meadow; 1 == mowed biweekly
quadrat <- quadrat_data %>% 
  mutate(TREATMENT=as.factor(ifelse(
    SITE=="rupert"|SITE=="slocan"|SITE=="bobolink"|SITE=="gordon"|
      SITE=="moberly"|SITE=="winona"|SITE=="balaclava"|SITE=="quilchena"|
      SITE=="kensington", "1", "0")))

hectare <- hectare_data %>% 
  mutate(TREATMENT=as.factor(ifelse(
    SITE=="rupert"|SITE=="slocan"|SITE=="bobolink"|SITE=="gordon"|
      SITE=="moberly"|SITE=="winona"|SITE=="balaclava"|SITE=="quilchena"|
      SITE=="kensington", "1", "0")))

# Use lubridate to show R how to read the dates
quadrat$DATE<-as.Date(quadrat$DATE, "%d/%m/%Y")
hectare$DATE<-as.Date(hectare$DATE, "%Y-%m-%d")

# Next, sort original dataframe by month-- this will put the month into the date column, allowing me to group data by month.
quadrat$MONTH<-as.factor(month(quadrat$DATE))
# date has been entered in multiple weird ways so should fix manually in the data sheet,
# all to same format as quadrat: "%d/%m/%Y"
hectare$MONTH<-as.factor(month(hectare$DATE))

# Make sure we are counting NA's (no flowers detected) as an abundance of 0 
# Later confirm that we are not counting this row included in a species richness count
quadrat$NUM_FLORAL_UNITS[is.na(quadrat$NUM_FLORAL_UNITS)]<-0
hectare$NUM_FLORAL_UNITS[is.na(hectare$NUM_FLORAL_UNITS)]<-0

# Make a new dataframe with total species and abudance counts (i.e. total number of species and number of floral units observed for each park)
# Changed abundances to per square metre aka divide by 20

# should redo this by round - since that's where we will be focusing for bees,
# and becuase it's our finest level of data

# what is TOT_COUNTS_QUAD1 used for?

TOT_COUNTS_QUAD1 <- quadrat %>% 
  #na.omit() %>% 
  group_by(SITE, TREATMENT) %>% 
  mutate(TOT_ABUND=((sum(NUM_FLORAL_UNITS))/20),
            TOT_RICHNESS=n_distinct(SPECIES),
            MAY_TOT_ABUND=((sum(subset(NUM_FLORAL_UNITS, MONTH=="5")))/20),
            MAY_TOT_RICH=n_distinct(subset(SPECIES, MONTH=="5")),
            JUN_TOT_ABUND=((sum(subset(NUM_FLORAL_UNITS, MONTH=="6")))/20),
            JUN_TOT_RICH=n_distinct(subset(SPECIES, MONTH=="6")),
            JUL_TOT_ABUND=((sum(subset(NUM_FLORAL_UNITS, MONTH=="7")))/20), 
            JUL_TOT_RICH=n_distinct(subset(SPECIES, MONTH=="7")),
            AUG_TOT_ABUND=((sum(subset(NUM_FLORAL_UNITS, MONTH=="8")))/20),
            AUG_TOT_RICH=n_distinct(subset(SPECIES, MONTH=="8")),
            MEAN_ABUND=mean(c(subset(NUM_FLORAL_UNITS, MONTH=="5"), subset(NUM_FLORAL_UNITS, MONTH=="6"), subset(NUM_FLORAL_UNITS, MONTH=="7"), subset(NUM_FLORAL_UNITS, MONTH=="8"))),
            MEAN_RICH=mean(c(MAY_TOT_RICH, JUN_TOT_RICH, JUL_TOT_RICH, AUG_TOT_RICH))
         )


TOT_COUNTS_QUAD <- quadrat %>% 
  group_by(SITE, TREATMENT) %>% 
  summarise(TOT_ABUND=((sum(NUM_FLORAL_UNITS))/20),
         TOT_RICHNESS=n_distinct(SPECIES),
         MAY_TOT_ABUND=((sum(subset(NUM_FLORAL_UNITS, MONTH=="5")))/20),
         MAY_TOT_RICH=n_distinct(subset(SPECIES, MONTH=="5")),
         JUN_TOT_ABUND=((sum(subset(NUM_FLORAL_UNITS, MONTH=="6")))/20),
         JUN_TOT_RICH=n_distinct(subset(SPECIES, MONTH=="6")),
         JUL_TOT_ABUND=((sum(subset(NUM_FLORAL_UNITS, MONTH=="7")))/20), 
         JUL_TOT_RICH=n_distinct(subset(SPECIES, MONTH=="7")),
         AUG_TOT_ABUND=((sum(subset(NUM_FLORAL_UNITS, MONTH=="8")))/20),
         AUG_TOT_RICH=n_distinct(subset(SPECIES, MONTH=="8")),
         MEAN_ABUND=mean(c(subset(NUM_FLORAL_UNITS, MONTH=="5"), subset(NUM_FLORAL_UNITS, MONTH=="6"), subset(NUM_FLORAL_UNITS, MONTH=="7"), subset(NUM_FLORAL_UNITS, MONTH=="8"))),
         MEAN_RICH=mean(c(MAY_TOT_RICH, JUN_TOT_RICH, JUL_TOT_RICH, AUG_TOT_RICH))
  )

# Same table, but for hectare data
levels(hectare$SITE)
hectare %>% 
  na.omit() %>% 
  group_by(SITE, TREATMENT) %>% 
  summarise(
            TOT_ABUND=((sum(NUM_FLORAL_UNITS))/20),
            TOT_RICHNESS=n_distinct(SPECIES),
            MAY_TOT_ABUND=((sum(subset(NUM_FLORAL_UNITS, MONTH=="5")))/20),
            MAY_TOT_RICH=n_distinct(subset(SPECIES, MONTH=="5")),
            JUN_TOT_ABUND=((sum(subset(NUM_FLORAL_UNITS, MONTH=="6")))/20),
            JUN_TOT_RICH=n_distinct(subset(SPECIES, MONTH=="6")),
            JUL_TOT_ABUND=((sum(subset(NUM_FLORAL_UNITS, MONTH=="7")))/20), 
            JUL_TOT_RICH=n_distinct(subset(SPECIES, MONTH=="7")),
            AUG_TOT_ABUND=((sum(subset(NUM_FLORAL_UNITS, MONTH=="8")))/20),
            AUG_TOT_RICH=n_distinct(subset(SPECIES, MONTH=="8")),
            MEAN_ABUND=mean(c(subset(NUM_FLORAL_UNITS, MONTH=="5"), subset(NUM_FLORAL_UNITS, MONTH=="6"), subset(NUM_FLORAL_UNITS, MONTH=="7"), subset(NUM_FLORAL_UNITS, MONTH=="8"))),
            MEAN_RICH=mean(c(MAY_TOT_RICH, JUN_TOT_RICH, JUL_TOT_RICH, AUG_TOT_RICH)))->TOT_COUNTS_HECTARE
            

# I need to think a bit more about this-- is the mean for each month just the mean of all observations? I think so, and if so, this is pretty meaningless data

## Plot data
# First, how do total abundances vary between mowed and unmowed parks?
ggplot(TOT_COUNTS_QUAD, aes(x=TREATMENT, y=TOT_ABUND))+
         geom_boxplot(stat="boxplot")+
         geom_point()

t.test(TOT_ABUND~TREATMENT, data=TOT_COUNTS_QUAD, var.equal=FALSE)

# Next, how do total species richnesses vary between mowed and unmowed parks?
ggplot(TOT_COUNTS_QUAD, aes(x=TREATMENT, y=TOT_RICHNESS))+
  geom_boxplot(stat="boxplot")+
  geom_point()

t.test(TOT_RICHNESS~TREATMENT, data=TOT_COUNTS_QUAD, var.equal=FALSE)

# Then, how do mean species richnesses vary between mowed and unmowed parks?
ggplot(TOT_COUNTS_QUAD, aes(x=TREATMENT, y=MEAN_RICH))+
  geom_boxplot(stat="boxplot")+
  geom_point()

t.test(MEAN_RICH~TREATMENT, data=TOT_COUNTS_QUAD, var.equal=FALSE)

# mean abundance and treatment?
ggplot(TOT_COUNTS_QUAD, aes(x=TREATMENT, y=MEAN_ABUND))+
  geom_boxplot(stat="boxplot")+
  geom_point()

t.test(MEAN_ABUND~TREATMENT, data=TOT_COUNTS_QUAD, var.equal=FALSE)

# JCU: taking the mean might condense the data down a little bit more if there is more
# variation in one group versus another. Either way, we don't want to lose the unique inform
# from those repeated samples from each park. We actually have ~18 * 7 = 126 data points, not just 18.
# Hence we should used mixed effects linear models for estimating effect of treatment
# on floral abundance and diversity, as in:
quadrat_site_by_round <- quadrat %>%
  group_by(SITE, ROUND) %>%
  # calculate floral abundance at the site visit (from all flower species)
  mutate(floral_abundance = sum(NUM_FLORAL_UNITS)) %>%
  # calculate species richness at the site visit
  # don't want to calculate rows of 0 abundance as a species
  mutate(species_richness = length(NUM_FLORAL_UNITS[NUM_FLORAL_UNITS>0])) %>%
  # now filter for distinct
  distinct(SITE, ROUND, .keep_all = TRUE) %>%
  select(SITE, ROUND, TREATMENT, floral_abundance, species_richness) %>%
  ungroup()

# null model
(m1_quadrat_floral_abundance <- lmer(data = quadrat_site_by_round,
                                    floral_abundance ~ 
                                      (1|SITE)))

summary(m1_quadrat_floral_abundance)

# treatment effect model
(m2_quadrat_floral_abundance <- lmer(data = quadrat_site_by_round,
                                     floral_abundance ~ TREATMENT + 
                                       (1|SITE)))
summary(m2_quadrat_floral_abundance)

# model two is not particularly better than model 1 (suggesting weak if any effect
# of the treatment on the abundance)
anova(m1_quadrat_floral_abundance, m2_quadrat_floral_abundance)

# bayesian model
library(rstanarm)
m3_stan_quadrat_floral_abundance <- stan_lmer(data = quadrat_site_by_round,
                                    floral_abundance ~ TREATMENT + 
                                      (1|SITE))

m3_stan_quadrat_floral_abundance
m3_stan_quadrat_floral_abundance[1] # site intercepts
# Although negative effect of mowing, the estimate for TREATMENT does not overlap 
# with 0 at the 50% credible interval,
# but DOES overlap with 0 at the 95% credible interval,
# suggesting weak if any effect of the treatment on the floral abundance
posterior_interval(m3_stan_quadrat_floral_abundance)




# Boxplot showing difference between treatments over the four months of surveys

quadrat %>% 
  na.omit() %>% 
  group_by(MONTH, TREATMENT) %>% 
  summarise(mean_abund=mean(NUM_FLORAL_UNITS))->quad_month

ggplot(quad_month, aes(x=TREATMENT, y=mean_abund, fill=quad_month$MONTH))+
  geom_boxplot(stat="boxplot")

### Time to do a few mixed models. First, make a dataframe that actually works for this type of thing

quadrat$NUM_FLORAL_UNITS[is.na(quadrat$NUM_FLORAL_UNITS)]<-0

quadrat_mlm <- quadrat %>%
  group_by(SITE, ROUND) %>% 
  mutate(MEAN_ABUND=mean(NUM_FLORAL_UNITS), MEAN_RICH=mean(n_distinct(SPECIES))) %>%
  distinct(SITE, .keep_all = TRUE)

### Make list of top ten most abundant plant species in the parks

quadrat_data$SPECIES<-as.factor(quadrat_data$SPECIES)

quadrat_data %>% 
  group_by(SPECIES) %>% 
  summarise(sum_inds=sum(NUM_FLORAL_UNITS))->plant_abundance

### What about the hectare data? I read it in earlier in the code and added the treatment (0=unmowed, 1=mowed)
str(hectare)

hectare %>% 
  filter(SHRUB_OR_TREE=="Y")->hectare

hectare %>% 
  group_by(SITE, TREATMENT) %>% 
  summarise(numfloralunits=sum(NUM_FLORAL_UNITS), richness=n_distinct(SPECIES))->hectare_sites


# Falaise, Lorcarno, Gordon, West Memorial, and Queen Elizabeth had the highest diversity of shrubs and trees, while locarno, slocan, falaise, south memorial, and balaclava had the highest number of floral units in total.

ggplot(hectare_sites, aes(x=TREATMENT, y=numfloralunits))+
  geom_boxplot(stat="boxplot")

lm2<-lm(numfloralunits~TREATMENT, data=hectare_sites)
summary(lm2)
anova(lm2)
t.test(numfloralunits~TREATMENT, data=hectare_sites, var.equal=FALSE)

ggplot(hectare_sites, aes(x=TREATMENT, y=richness))+
  geom_boxplot(stat="boxplot")
lm3<-lm(richness~TREATMENT, data=hectare_sites)
summary(lm3)
anova(lm3)
t.test(richness~TREATMENT, data=hectare_sites, var.equal=FALSE)

### Read in the bee data

library(tidyverse)


bee_data<-read.csv("./bee_data_2021.csv")

bee_data %>% 
  mutate(treatment=ifelse(site=="rupert"|site=="slocan"|site=="bobolink"|site=="gordon"|site=="moberly"|site=="winona"|site=="balaclava"|site=="quilchena"|site=="kensington", "1", "0"))->bee_data1

# str(bee_data1)

# bee_data1 %>% 
  #group_by(site, treatment) %>% 
  #summarise(sum_inds=sum(n_distinct(bee_id)))->bee_abundance

#bee_data %>% 
  filter(bee_id=="MISSING???")->nummissing

bee_data$bee_id<-as.factor(bee_data$bee_id)
levels(bee_data$bee_id)
# 7 missing identifications

bee_data %>% 
  filter(bee_id=="Bombus ?"|bee_id=="Bombus impatiens"|bee_id=="Bombus mixtus"|bee_id=="Bombus sp."|bee_id=="Bombus flavifrons"|bee_id=="Bombus melanopygus"|bee_id=="Bombus nevadensis"|bee_id=="Bombus vosnesenskii")->numbombus

# 674 individuals are in Bombus-- about 674/1869 total individuals
# first, add genera to each observation using mutate

bee_data1 %>% 
  mutate(genus= case_when((bee_id=="Bombus ?"|bee_id=="Bombus impatiens"|bee_id=="Bombus mixtus"|bee_id=="Bombus sp."|bee_id=="Bombus flavifrons"|bee_id=="Bombus melanopygus"|bee_id=="Bombus nevadensis"|bee_id=="Bombus vosnesenskii")~"Bombus", (bee_id=="Halictus"|bee_id=="Halictus "|bee_id=="Halictus rubicundus")~"Halictus", (bee_id=="Agapostemon")~"Agapestemon", (bee_id=="Anthidium")~"Anthidium", (bee_id=="Andrena")~"Andrena", (bee_id=="Apis mellifera")~"Apis", (bee_id=="Ceratina")~"Ceratina", (bee_id=="Colletes")~"Colletes", (bee_id=="Hoplitis")~"Hoplitis", (bee_id=="Hylaeus")~"Hylaeus", (bee_id=="Lasioglossum")~"Lasioglossum", (bee_id=="Megachile")~"Megachile", (bee_id=="Melecta")~"Melecta", (bee_id=="Melissodes")~"Melissodes", (bee_id=="Nomada")~"Nomada", (bee_id=="Osmia")~"Osmia", (bee_id=="Sphecodes")~"Sphecodes", (bee_id=="Syrphidae")~"Syrphidae", TRUE~"Other"))->bee_data_genus

# For the purposes of making a table with the number of genera in each park, let's get rid of the unknown, or "other" genera
# bee_data_genus %>% 
  subset(genus!="Other")->bee_data_genus

bee_data_genus %>% 
  group_by(site, treatment) %>% 
  summarise(sum_genera=sum(n_distinct(genus)))->bee_diversity

# let's check that this is working correctly by seeing counting the number of genera at balaclava

balaclava<-bee_data %>% 
  filter(site=="balaclava") %>% 
  group_by(bee_id)

ggplot(bee_diversity, aes(x=treatment, y=sum_genera))+
  geom_boxplot(stat="boxplot")

t.test(sum_genera~treatment, data=bee_diversity, var.equal=FALSE)
 # xlab("treatment")+
  #ylab=("total number of genera found at each site")

# what if I used mutate instead, to create a line for each round of sampling, and then made a linear model?

bee_data_genus %>% 
  group_by(site, sampling_round) %>% 
  mutate(sum_genera=sum(n_distinct(genus)) %>%(summarise_all(mean, na.rm = TRUE))->bee_diversity_round
         

bee_data_genus %>% 
  group_by(sampling_round, site) %>% 
  transmute(sum_genera=sum(n_distinct(genus)), sum_plant_family=sum(n_distinct(plant_netted_from_famly)), treatment=treatment) %>% 
  distinct(site, .keep_all = TRUE)->bee_div_round

bee_div_round$sampling_round<-as.factor(bee_div_round$sampling_round)

# is there a difference in genera-level bee diversity in between rounds?
ggplot(bee_div_round, aes(x=sampling_round, y=sum_genera))+
  geom_boxplot(stat="boxplot")

lm2<-lm(sum_genera~sampling_round, data=bee_div_round)
anova(lm2)

# is ther a difference in genera-level bee diversity in between parks?

ggplot(bee_div_round, aes(x=site, y=sum_genera))+
  geom_boxplot(stat="boxplot")

lm3<-lm(sum_genera~site, data=bee_div_round)
anova(lm3)


## List of interactions 

bee_data_genus %>% 
  group_by(plant_netted_from_genus) %>% 
  summarise(n_distinct(genus))->interactionslist

# make list of plant sp. and how many bees were netted off of each one
bee_data_genus %>% 
  group_by(plant_netted_from_sci_name) %>% 
  summarise(num_bees_netted=length(plant_netted_from_sci_name))->plants

# One andrena, one bombus, one melissodes, and one "other" bee have no plant scientific name. Just a heads up as to why there are four observations w/ no associated plant sp.
# Someone put Solanum dulcamara as the genus for a Solanum observation and I don't know how to fix it. I tried to change it from Solanum dulcamara->Solanum but that does not help because for that observation, no one put the scientific name into the scientific name slot.

bee_data_genus$plant_netted_from_genus[which(bee_data_genus$plant_netted_from_genus=="Solanum dulcamara")]<-"Solanum"

plants %>% 
  filter(num_bees_netted>3)->interactions_filtered

## What about by round? When did bee sampling peak?


bee_data_genus %>% 
  group_by(sampling_round) %>% 
  summarise(num_bees_netted=length(plant_netted_from_sci_name))->bees_by_round







install.packages("lme4")
library(lme4)
install.packages("lmerTest")
library(lmerTest)

lmerround<-lmer(sum_genera~treatment+(1|sampling_round), data=bee_div_round)
summary(lmerround)

ggplot(bee_div_round, aes(x=treatment, y=sum_genera))+
  geom_smooth(stat="smooth", method="lm", aes(colour=sampling_round))

summary(lmerround)
anova(lmround)
# ok so this would suggest that there is a significant difference in bee diversity between the two parks, with unmowed parks having highr bee diversity than mowed parks.
# Problem: this appears to somehow be combining the extra rounds of sampling into one?

# how many bees are being netted at each round?

bee_data_genus %>% 
  group_by(sampling_round) %>% 
  summarise(n_distinct(genus))->numbeesperround

ggplot(bee_data_genus, aes(x=))

# how many plants species are being netted off of?

levels(bee_data_genus$plant_netted_from_sci_name)

# 69 species (some bee specimens have not been linked to a plant species)

# What is the most common plant being netted off of in these parks?

bee_data_genus %>% 
  group_by(plant_netted_from_sci_name) %>% 
  summarise(numobservations=n_distinct(unique_specimen_id))->most_common_plants

sum(most_common_plants$numobservations)
# five most common plants being netted off of are trifolium repens, Hypochaeris radicata, Crepis capillaris, Achillea millefolium, and Ranunculus repens
# note that there are 37 bee ids that have no associated plant species


  















