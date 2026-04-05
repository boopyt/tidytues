# dataset 
library(tidytuesdayR)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(ggridges)
library(data.table)
tuesdata <- tidytuesdayR::tt_load('2026-02-03')
df <- tuesdata$edible_plants

====# data cleaning #====
colnames(df) 
#relevant columns at a glance: cultivation, sunlight, water, ph, nutrients, soil, season, temp class, grow temp, days harvest
#not as relevant: tax name, description, requirements, sensitivities, energy, nutritional info (mostly empty)
unique(df$cultivation)
#brassicas assumed to be a typo of brassica
df$cultivation[df$cultivation == "Brassicas"] <- "Brassica"

#water 
unique(df$water)
sum(is.na(df$water)) #no missing values
# "Very High" "Medium"    "Very Low"  "High"      "Low"       "very high" "Very low"  "high" 
#as the variables share the same name but are classified different due to capitalisation, i create a new column with names fixed
df$waterN <- str_to_title(df$water)
unique(df$waterN)
#to reorder the variables from lowest to highest 
df$waterN <- factor(df$waterN, 
                    levels = c("Very Low", "Low", "Medium", "High", "Very High"),
                    ordered = TRUE)
unique(df$waterN)

#nutrients 
unique(df$nutrients)
sum(is.na(df$nutrients)) #no missing values
#resultant characters are "Medium" "High" "Low" "High potassium fertiliser every 2 weeks." "high" "low" "Medium to high"  
#medium to high will be categorised as medium (for conservation sake) and high potassium etcetc will just be high
df$nutrientsN <- tolower(df$nutrients)
df$nutrientsN[startsWith(df$nutrientsN, "high")] <- "High"
df$nutrientsN[startsWith(df$nutrientsN, "medium")] <- "Medium"
df$nutrientsN[startsWith(df$nutrientsN, "low")] <- "Low"
unique(df$nutrientsN)
df$nutrientsN <- factor(df$nutrientsN, 
                        levels = c("Low", "Medium", "High"), 
                        ordered = T)

#temp class
unique(df$temperature_class)
sum(is.na(df$temperature_class))
# half hardy will be changed to Medium for consistency and very hard will be grouped with very hardy (assuming it was a typo)
# title case will also be applied for standardisation purposes
df$temp_classN <- str_to_title(df$temperature_class)
df$temp_classN[df$temp_classN == "Very Hard"] <- "Very Hardy"
df$temp_classN[df$temp_classN == "Half Hardy"] <- "Medium"
unique(df$temp_classN)
df$temp_classN <- factor(df$temp_classN, 
                         levels = c("Very Tender", "Tender", "Medium", "Hardy", "Very Hardy"), ordered =T)
unique(df$temp_classN)

#ph upper and lower
##optimum ph midpoint
#since i am looking at common cultivation profile, from ph upper and ph lower, i obtain a optimum ph midpoint for better comparison
sum(is.na(df$preferred_ph_lower))
sum(is.na(df$preferred_ph_upper))
df <- mutate(df,
             ph_midpt = (preferred_ph_upper + preferred_ph_lower)/2)
summary(df$ph_midpt)
##ph range 
df <- mutate(df,
             ph_range = preferred_ph_upper - preferred_ph_lower)
summary(df$ph_range)

#temp growing and #temp germination
unique(df$temperature_growing)
sum(is.na(df$temperature_growing))
df$averageT <- as.character(df$temperature_growing)
## Although three temperature-related variables were available, only optimal growing temperature was used to 
## define cultivation profiles in order to avoid over-representing temperature and to focus on typical growth 
## conditions. Germination temperature and temp class were retained for supplementary analyses.
# will not be focusing on germination temp for simplicity sake 
# using the average growing temp
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
sum(is.na(df$days_harvest))
# hence we see that there is 59 non numeric values, all non numeric shall be converted to NA and excluded during analysis on prediction of days of harvest
df$harvestdays <- df$days_harvest
df$harvestdays[
  !is.na(match(df$harvestdays, c("continual", "x", "Not applicable.")))
] <- NA
unique(df$harvestdays)
sum(is.na(df$harvestdays))
df$harvestdays <- as.character(df$harvestdays)
# Source - https://stackoverflow.com/a/32017230
# Posted by MichaelChirico, modified by community. See post 'Timeline' for change history
# Retrieved 2026-02-04, License - CC BY-SA 3.0
setDT(df)[ , harvestdays := sapply(strsplit(harvestdays, split = "-"),
                                  function(x) mean(as.numeric(x)))]

#season 
unique(df$season)
sum(is.na(df$season)) #73 empty values 
#since at least 50% of data is missing, data is also confusing (some listed as biennial, 
#annual etc while others are like shrub, evergreen), data is excluded

#soil 
unique(df$soil)
sum(is.na(df$cultivation))
#due to sheer amount of different soil types and combinations along with vague descriptions, i will not be taking soil profile into account for simplicity 

#sunlight
unique(df$sunlight)
#data variables show a string of sun, partial and full shade
#thus after a google search, using https://gardeningsg.nparks.gov.sg/page-index/horticulture-techniques/gauging-light/ which reccommends 
#overestimation of sun needs as it is easier remedied than lack of sunlight, i use the upper bound of sun needs 
df$sunlightN <- tolower(df$sunlight)
# Source - https://stackoverflow.com/a/54481371
# Posted by Ronak Shah, modified by community. See post 'Timeline' for change history
# Retrieved 2026-02-04, License - CC BY-SA 4.0
df$sunlightN <- with(df, replace(sunlightN, grepl('full sun', sunlightN),'Full Sun'))
df$sunlightN <- str_to_title(df$sunlightN)
unique(df$sunlightN)
# now we are left with only full sun and partial shade

#energy
unique(df$energy)
sum(is.na(df$energy)) #energy will be excluded since about 90% (128) of data is NA thus does not seem to be able to provide any valuable information

#cultivation 
unique(df$cultivation) #left as is 
sum(is.na(df$cultivation))

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
