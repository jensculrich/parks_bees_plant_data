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


## Clean data
# First, add whether each site is mowed or unmowed. Unmowed=0 and mowed=1.
# Start with quadrat data
quadrat_data %>% 
  mutate(TREATMENT=ifelse(SITE=="Rupert"|SITE=="Slocan"|SITE=="Bobolink"|SITE=="Gordon"|SITE=="Moberly"|SITE=="Winona"|SITE=="Balaclava"|SITE=="Quilchena"|SITE=="Kensington", "1", "0"))->quadrat

str(quadrat)

# Next, add treatment column to the hectare data.
hectare_data %>% 
  mutate(TREATMENT=ifelse(SITE=="Rupert"|SITE=="Slocan"|SITE=="Bobolink"|SITE=="Gordon"|SITE=="Moberly"|SITE=="Winona"|SITE=="Balaclava"|SITE=="Quilchena"|SITE=="Kensington", "1", "0"))->hectare

str(hectare)

# Now, make a table showing total floral resources and floral diversity by each park
# First, merge the two data frames to create one dataframe

hectare$NUM_FLORAL_UNITS<-as.numeric(hectare$NUM_FLORAL_UNITS)
quadrat$NUM_FLORAL_UNITS<-as.numeric(quadrat$NUM_FLORAL_UNITS)
data_total<-bind_rows(hectare, quadrat)

# Group by site to make things easier!
data_total %>% 
  group_by(SITE)->data_total_sorted

# Hm, it would appear that Memorial South, Memorial West, and Locarno are written in a few different ways! This code should fix things:

data_total_sorted$SITE[which(data_total_sorted$SITE=="South Memorial")]<-"Memorial South"

data_total_sorted$SITE[which(data_total_sorted$SITE=="West Memorial")]<-"Memorial West"

data_total_sorted$SITE[which(data_total_sorted$SITE=="Memorial west")]<-"Memorial West"

data_total_sorted$SITE[which(data_total_sorted$SITE=="Lucarno")]<-"Locarno"

# Double-check that the code worked!
data_total_sorted$SITE<-as.factor(data_total_sorted$SITE)
levels(data_total_sorted$SITE)
str(data_total_sorted)
# It worked!
# Time for some more cleaning. I'm going to check out whether these species have been spelled properly, or if there are actually duplicates
levels(data_total_sorted$SPECIES)
# duplicates to change: change "acer capestre"-> acer campestre; "achillea milleflolium"-> achillea milleflolium, "Crateaugus douglasii"->"Crateagus douglasii"; "Crateaugous monogyna"->"Crateagus monogyna"; "Magnolia X soulangeana"->"Magnolia x soulangeana"; "Malus sp"->"Malus sp."; "PLA SOL"->???, "Rosa sp"->"Rosa sp.", "Solanum dulcmara"->"Solanum dulcamara"

## Make a new dataframe with summary stats
# use summarise() to make a new dataframe with floral richness, abundance
# First, use lubridate to show R how to read the dates

library(lubridate)
data_total_sorted$DATE<-as.Date(data_total_sorted$DATE, "%m.%d.%Y")

# Next, sort original dataframe by month-- this will put the month into the date column, allowing me to group data by month.
data_total_sorted$DATE<-month(data_total_sorted$DATE)

# Okay time to make the new dataframe
# First, make sure R is reading the "species" and "date" columns as factors

data_total_sorted$SPECIES<-as.factor(data_total_sorted$SPECIES)

data_total_sorted$DATE<-as.factor(data_total_sorted$DATE)

# Make a new dataframe with total species and abudance counts (i.e. total number of species and number of floral units observed for each park)
# Changed abundances to per square metre aka divide by 20
data_total_sorted %>% 
  na.omit() %>% 
  group_by(SITE, TREATMENT) %>% 
  summarise(TOT_ABUND=((sum(NUM_FLORAL_UNITS))/20),
            TOT_RICHNESS=n_distinct(SPECIES),
            MAY_TOT_ABUND=((sum(subset(NUM_FLORAL_UNITS, DATE=="5")))/20),
            MAY_TOT_RICH=n_distinct(subset(SPECIES, DATE=="5")),
            JUN_TOT_ABUND=((sum(subset(NUM_FLORAL_UNITS, DATE=="6")))/20),
            JUN_TOT_RICH=n_distinct(subset(SPECIES, DATE=="6")),
            JUL_TOT_ABUND=((sum(subset(NUM_FLORAL_UNITS, DATE=="7")))/20), 
            JUL_TOT_RICH=n_distinct(subset(SPECIES, DATE=="7")),
            AUG_TOT_ABUND=((sum(subset(NUM_FLORAL_UNITS, DATE=="8")))/20),
            AUG_TOT_RICH=n_distinct(subset(SPECIES, DATE=="8")),
            MEAN_ABUND=mean(c(subset(NUM_FLORAL_UNITS, DATE=="5"), subset(NUM_FLORAL_UNITS, DATE=="6"), subset(NUM_FLORAL_UNITS, DATE=="7"), subset(NUM_FLORAL_UNITS, DATE=="8"))))->TOT_COUNTS

# Come back to this to add in mean richness!!!

## Plot data
# First, how do mean abundances vary between mowed and unmowed parks?
ggplot(TOT_COUNTS, aes(x=TREATMENT, y=MEAN_ABUND))+
  geom_boxplot(stat="boxplot")

## Statistical tests
# time to try a t-test just for fun

t.test(TOT_RICHNESS~TREATMENT, data=TOT_COUNTS, var.equal=FALSE)

# The mean total species richness per group is 27.67 for unmowed parks and 15.44 for mowed parks, with a p-value of 0.01659

t.test(MAY_TOT_ABUND~TREATMENT, data=TOT_COUNTS, var.equal=FALSE)
# not significant












