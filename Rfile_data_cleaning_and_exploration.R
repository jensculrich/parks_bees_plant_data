### Floral resource data cleaning and exploration
### started on May 24, 2022,
### Jens U and Erin M

# check that wd is all good
getwd()

# Installed tidyverse, as I had just redownloaded R and did not have it installed! Ensured that ggplot2 was in my library.

install.packages("tidyverse")
library(tidyverse)
library(ggplot2)

## read data
# quadrat floral resource data from 2021
quadrat_data <- read.csv("./quadrat_flower_surveys_2021.csv")
 
# hectare floral resource data from 2021
hectare_data <- read.csv("./hectare_floral_surveys_2021.csv")
hectare_data<-hectare_data[!is.na(hectare_data$SPECIES),]

## Clean data
# First, add whether each site is mowed or unmowed. Unmowed=0 and mowed=1.
# Start with quadrat data
quadrat_data %>% 
  mutate(TREATMENT=ifelse(SITE=="rupert"|SITE=="slocan"|SITE=="bobolink"|SITE=="gordon"|SITE=="moberly"|SITE=="winona"|SITE=="balaclava"|SITE=="quilchena"|SITE=="kensington", "1", "0"))->quadrat

str(quadrat)
levels(quadrat$SITE)

quadrat$SITE[which(quadrat$SITE=="Memorial west")]<-"Memorial West"
quadrat$SITE[which(quadrat$SITE=="South Memorial")]<-"Memorial South"
quadrat$SITE<-as.factor(quadrat$SITE)
levels(quadrat$SITE)

# Next, add treatment column to the hectare data.
hectare_data %>% 
  mutate(TREATMENT=ifelse(SITE=="Rupert"|SITE=="Slocan"|SITE=="Bobolink"|SITE=="Gordon"|SITE=="Moberly"|SITE=="Winona"|SITE=="Balaclava"|SITE=="Quilchena"|SITE=="Kensington", "1", "0"))->hectare

str(hectare)

hectare$SITE[which(hectare$SITE=="South Memorial")]<-"Memorial South"

hectare$SITE[which(hectare$SITE=="West Memorial")]<-"Memorial West"

hectare$SITE[which(hectare$SITE=="Memorial west")]<-"Memorial West"

hectare$SITE[which(hectare$SITE=="Lucarno")]<-"Locarno"

hectare$SITE<-as.factor(hectare$SITE)
levels(hectare$SITE)

# It worked!
# Time for some more cleaning. I'm going to check out whether these species have been spelled properly, or if there are actually duplicates
quadrat$SPECIES<-as.factor(quadrat$SPECIES)
levels(quadrat$SPECIES)
hectare$SPECIES<-as.factor(hectare$SPECIES)
levels(hectare$SPECIES)
# duplicates to change: change "acer capestre"-> acer campestre; "achillea milleflolium"-> achillea milleflolium, "Crateaugus douglasii"->"Crateagus douglasii"; "Crateaugous monogyna"->"Crateagus monogyna"; "Magnolia X soulangeana"->"Magnolia x soulangeana"; "Malus sp"->"Malus sp."; "PLA SOL"->???, "Rosa sp"->"Rosa sp.", "Solanum dulcmara"->"Solanum dulcamara"

## Make a new dataframe with summary stats
# use summarise() to make a new dataframe with floral richness, abundance
# First, use lubridate to show R how to read the dates

library(lubridate)
quadrat$DATE<-as.Date(quadrat$DATE, "%d/%m/%Y")
hectare$DATE<-as.Date(hectare$DATE, "%m.%d.%Y")

# Next, sort original dataframe by month-- this will put the month into the date column, allowing me to group data by month.
quadrat$MONTH<-month(quadrat$DATE)
hectare$MONTH<-month(hectare$DATE)

# Okay time to make the new dataframe
# First, make sure R is reading the "species" and "date" columns as factors

quadrat$SPECIES<-as.factor(quadrat$SPECIES)
quadrat$DATE<-as.factor(quadrat$DATE)
quadrat$NUM_FLORAL_UNITS<-as.numeric(quadrat$NUM_FLORAL_UNITS)

hectare$SPECIES<-as.factor(hectare$SPECIES)
hectare$DATE<-as.factor(hectare$DATE)
hectare$NUM_FLORAL_UNITS<-as.numeric(hectare$NUM_FLORAL_UNITS)

# Make a new dataframe with total species and abudance counts (i.e. total number of species and number of floral units observed for each park)
# Changed abundances to per square metre aka divide by 20

quadrat %>% 
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
         )->TOT_COUNTS_QUAD1

quadrat$NUM_FLORAL_UNITS[is.na(quadrat$NUM_FLORAL_UNITS)]<-0

quadrat %>% 
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
  )->TOT_COUNTS_QUAD



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


### Read in the bee data

library(tidyverse)


bee_data<-read.csv("./bee_data_2021.csv")

bee_data %>% 
  mutate(treatment=ifelse(site=="rupert"|site=="slocan"|site=="bobolink"|site=="gordon"|site=="moberly"|site=="winona"|site=="balaclava"|site=="quilchena"|site=="kensington", "1", "0"))->bee_data1

str(bee_data1)

bee_data1 %>% 
  group_by(site, treatment) %>% 
  summarise(sum_inds=sum(n_distinct(bee_id)))->bee_abundance

bee_data %>% 
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
bee_data_genus %>% 
  subset(genus!="Other")->bee_data_genus

bee_data_genus %>% 
  group_by(site, treatment) %>% 
  summarise(sum_genera=sum(n_distinct(genus)))->bee_diversity

# let's check that this is working correctly

balaclava<-bee_data %>% 
  filter(site=="balaclava") %>% 
  group_by(bee_id)

ggplot(bee_diversity, aes(x=treatment, y=sum_genera))+
  geom_boxplot(stat="boxplot")
 # xlab("treatment")+
  #ylab=("total number of genera found at each site")

# what if I used mutate instead, to create a line for each round of sampling, and then made a linear model?

bee_data_genus %>% 
  group_by(site, sampling_round) %>% 
  mutate(sum_genera=sum(n_distinct(genus)) %>%(summarise_all(mean, na.rm = TRUE))->bee_diversity_round
         

bee_data_genus %>% 
  group_by(sampling_round, site) %>% 
  transmute(sum_genera=sum(n_distinct(genus)), sum_plant_family=sum(n_distinct(plant_netted_from_famly)), treatment=treatment) %>% 
  distinct(site, .keep_all = TRUE)->bee_diversity_round

lmround<-lm(sum_genera~treatment, data=bee_diversity)
summary(lmround)
anova(lmround)
# ok so this would suggest that there is a significant difference in bee diversity between the two parks, with unmowed parks having highr bee diversity than mowed parks.
# Problem: this appears to somehow be combining the extra rounds of sampling into one?

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


  















