# dataset 
library(tidytuesdayR)
library(ggplot2)
library(tidyverse)
library(ggridges)
library(data.table)
tuesdata <- tidytuesdayR::tt_load('2026-02-03')
df <- tuesdata$edible_plants

====# data cleaning #====
#relevant columns at a glance: cultivation, sunlight, water, ph, nutrients, soil, season, temp class, grow temp, days harvest
#not as relevant: tax name, description, requirements, sensitivities, energy, nutritional info

#cultivation
#brassicas assumed to be a typo of brassica
df$cultivation[df$cultivation == "Brassicas"] <- "Brassica"

#water 
# values are "Very High" "Medium"    "Very Low"  "High"      "Low"       "very high" "Very low"  "high" 
#as the variables share the same name but are classified differently due to capitalisation, I create a new column with names fixed
df$waterN <- str_to_title(df$water)
#to reorder the variables from lowest to highest 
df$waterN <- factor(df$waterN, 
                    levels = c("Very Low", "Low", "Medium", "High", "Very High"),
                    ordered = TRUE)

#nutrients 
#resultant values  are "Medium" "High" "Low" "High potassium fertiliser every 2 weeks." "high" "low" "Medium to high"  
#medium to high will be categorised as medium, and high potassium will just be high
df$nutrientsN <- tolower(df$nutrients)
df$nutrientsN[startsWith(df$nutrientsN, "high")] <- "High"
df$nutrientsN[startsWith(df$nutrientsN, "medium")] <- "Medium"
df$nutrientsN[startsWith(df$nutrientsN, "low")] <- "Low"
df$nutrientsN <- factor(df$nutrientsN, 
                        levels = c("Low", "Medium", "High"), 
                        ordered = T)

#temp class
# half hardy will be changed to Medium for consistency and very hard will be grouped with very hardy (assuming it was a typo)
# title case will also be applied for standardisation 
df$temp_classN <- str_to_title(df$temperature_class)
df$temp_classN[df$temp_classN == "Very Hard"] <- "Very Hardy"
df$temp_classN[df$temp_classN == "Half Hardy"] <- "Medium"
df$temp_classN <- factor(df$temp_classN, 
                         levels = c("Very Tender", "Tender", "Medium", "Hardy", "Very Hardy"), ordered =T)

#ph upper and lower
##optimum ph midpoint
#since I am looking at common cultivation profile, from ph upper and ph lower, I obtain an optimum ph midpoint for better comparison

df <- mutate(df,
             ph_midpt = (preferred_ph_upper + preferred_ph_lower)/2)
summary(df$ph_midpt)
##ph range 
df <- mutate(df,
             ph_range = preferred_ph_upper - preferred_ph_lower)
summary(df$ph_range)

#temp growing and #temp germination
df$averageT <- as.character(df$temperature_growing)
## Although three temperature-related variables were available, only optimal growing temperature was used
# Source - https://stackoverflow.com/a/32017230
# Posted by MichaelChirico, modified by community. See post 'Timeline' for change history
# Retrieved 2026-02-04, License - CC BY-SA 3.0
setDT(df)[ , averageT := sapply(strsplit(averageT, split = "-"),
                                   function(x) mean(as.numeric(x)))]
                                   
#days harvest
unique(df$days_harvest) 
# days of germination not taken into account since already counted in days of harvest, provides less info than days of harvest
# and not what this analysis is interested in 
table(df$days_harvest)
# convert all values to numeric; 59 non-numeric entries converted to NA and excluded during analysis 
df$harvestdays <- df$days_harvest
df$harvestdays[
  !is.na(match(df$harvestdays, c("continual", "x", "Not applicable.")))
] <- NA
df$harvestdays <- as.character(df$harvestdays)
# Source - https://stackoverflow.com/a/32017230
# Posted by MichaelChirico, modified by community. See post 'Timeline' for change history
# Retrieved 2026-02-04, License - CC BY-SA 3.0
setDT(df)[ , harvestdays := sapply(strsplit(harvestdays, split = "-"),
                                  function(x) mean(as.numeric(x)))]

#season 
sum(is.na(df$season)) #73 empty values 
#since at least 50% of data is missing, remaining data is also a confusing mix, season is excluded

#soil 
unique(df$soil)
#due to the sheer amount of different soil types and combinations, along with vague descriptions, soil profile will not be taken into account

#sunlight
#data variables show a string of sun, partial and full shade
#after a google search, using https://gardeningsg.nparks.gov.sg/page-index/horticulture-techniques/gauging-light/ which reccommends 
#overestimation of sun needs as it is easier remedied than lack of sunlight, i use the upper bound of sun needs 
df$sunlightN <- tolower(df$sunlight)
# Source - https://stackoverflow.com/a/54481371
# Posted by Ronak Shah, modified by community. See post 'Timeline' for change history
# Retrieved 2026-02-04, License - CC BY-SA 4.0
df$sunlightN <- with(df, replace(sunlightN, grepl('full sun', sunlightN),'Full Sun'))
df$sunlightN <- str_to_title(df$sunlightN)

#energy
sum(is.na(df$energy)) #energy will be excluded since about 90% (128) of data is NA thus does not seem to be able to provide any valuable information

#cultivation 
unique(df$cultivation) #left as is 

#final dataset
df1 <- select(df, 
             Name = common_name,
             Cultivation = cultivation,
             Water = waterN, 
             Nutrients = nutrientsN, 
             Hardiness = temp_classN, 
             pH = ph_midpt,
             pHrange = ph_range,
             Temperature = averageT, 
             Harvest = harvestdays, 
             Sunlight = sunlightN, 
             preferred_ph_lower,
             preferred_ph_upper
)
