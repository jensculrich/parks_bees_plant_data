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
  mutate(TREATMENT=ifelse(SITE=="Rupert"|SITE=="Slocan"|SITE=="Bobolink"|SITE=="Gordon"|SITE=="Moberly"|SITE=="Winona"|SITE=="Balaclava"|SITE=="Quilchena"|SITE=="Kensington", "1", "0"))->quadrat

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
quadrat$DATE<-as.Date(quadrat$DATE, "%m.%d.%Y")
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
            MEAN_RICH=mean(c(MAY_TOT_RICH, JUN_TOT_RICH, JUL_TOT_RICH, AUG_TOT_RICH)))->TOT_COUNTS_QUAD



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
  group_by(TREATMENT, MONTH) %>% 
  summarise(mean_abund=mean














