library(tidyverse)
#KEEN 2019 Hack-A-Thon!


#load data
#Universal point counts, standardized to % cover
cover <- read_csv("../../observational_data/cleaned_data/keen_cover.csv")
#Fish counts
fish <- read_csv("../../observational_data/cleaned_data/keen_fish.csv")
#quadrat data
quads <- read_csv("../../observational_data/cleaned_data/keen_quads.csv")
#site metadata
sites <- read_csv("../../observational_data/cleaned_data/keen_sites.csv")
#swath data
swath <- read_csv("../../observational_data/cleaned_data//keen_swath.csv")
#kelp morphometrics
kelp <- read_csv("../../observational_data/cleaned_data//keen_kelp.csv")
#fish biomass conversion lookup table
fish_biomass_coefs <- read_csv("../data/fish_biomass_coefs.csv")
#kelp biomass data
kelp_biomass <- read_csv("../data/kelp_quads_biomass.csv")

#derived data - wide form in preparation for modeling
quads_with_fish_wide <- read_csv("../data/quads_with_fish_wide.csv")
kelp_biomass_wide <- read_csv("../data/kelp_biomass_wide.csv")



#grab biomass data from kelp size repo - use this to update.
download.file("http://raw.githubusercontent.com/kelpecosystems/kelp_size_dist_change/master/data/kelp_quads_biomass.csv", 
              destfile = "../data/kelp_quads_biomass.csv") #, method = "curl")



######################
#fish data wrangling

#prepare for joining - can drop unnecessary columns as there should be only one set of records
#for each site/year combo
fish <- select(fish, -MONTH, -DAY)
fish$SIDE <- as.character(fish$SIDE)

#add length/weight coefficients
fish <- left_join(fish, fish_biomass_coefs, by = "SPECIES")



#fish length adjustments
#use median value of each size class to represent "actual size" of fish for biomass conversion
fish_length <- data.frame(FISH.SIZE = c("YOY", "0-10", "10-50", "50-100", ">100"),
                          actual_size = c(5, 5, 15, 60, 60))
fish <- left_join(fish, fish_length)

#note that counts are per 80 meters: need to divide count by 80 to get density/square meter

fish_sum <- fish %>%
  group_by(YEAR, SITE, TRANSECT, SPECIES, actual_size, ALPHA, BETA) %>%
  summarise(count = sum(COUNT)) %>%
  ungroup()

fish_sum$density <- fish_sum$count[]/80

#calculate mass of individual fish
fish_sum <- fish_sum %>%
  mutate(ind.fish_mass = ALPHA * (actual_size^BETA))

#calculate total biomass per square meter of fish
fish_sum <- fish_sum %>%
  mutate(fish_mass = density * ind.fish_mass)

#retain relevant columns
fish_sum <- fish_sum %>%
  select(YEAR, SITE, TRANSECT, SPECIES, actual_size, fish_mass)
#############################

#quadrat data wrangling
#exclude small kelps for now - not necessarily relevant for fish habitat

#Species codes in this vector will be carried through
species_of_interest <- c('AGCL', 'ALES', 'LADI', 'SADE', 'SL', "HOAM", "CABO",
                         "CAIR", "MYSP", "PHGU", "ASFO", "ASFOS", "ASRU", "ASRUS", "HESA", "HESAS", "SDS", "SDL")
quads <- select(quads, -MONTH, -DAY) 
quads_kelp <- quads %>%
  filter(SP_CODE %in% species_of_interest)
  
  
#counts are already density because quads are 1 m2
quads_sum <- quads_kelp %>%
  group_by(YEAR, SITE, SP_CODE) %>% 
  summarise(mean_observation_density = mean(COUNT))

#mean temps - dive computer data from datasheets, not buoy data
mean_temps <- sites %>%
  group_by(YEAR, SITE) %>%
  summarise(mean_dive_computer_temp = mean(TEMPERATURE_C))

#cover of rocky substrate, derived from proportion of point counts with rocky bottom
#edit rocky_subs to include desired substrata
rocky_subs <- c('B', 'BL', 'BM', 'BS')
cover_rocky <- filter(cover, SP_CODE %in% rocky_subs)
cover_rocky <- cover_rocky %>%
  group_by(YEAR, SITE) %>%
  summarise(sub_cover = mean(PERCENT_COVER))


#depths from dive computers
mean_depths<- sites %>%
  group_by(YEAR, SITE) %>%
  summarise(mean_start_depth = mean(START_DEPTH_M),
            mean_end_depth = mean(END_DEPTH_M))

#join quadrat data with fish data
data_complete <- left_join(quads_sum, fish_sum, by = c("YEAR", "SITE"))
data_complete <- left_join(data_complete, quads_sum, by = c("YEAR", "SITE"))

#join with temps, rocky cover, and depth
data_complete <- left_join(data_complete, mean_temps)
data_complete <- left_join(data_complete, cover_rocky)
data_complete <- left_join(data_complete, mean_depths)

#write out combined dataframe
#write_csv(data_complete, "../data/fish_quads_kelp_biomass.csv")


#filter to fish and species of interest to investigate specific relationships between fish and kelp/inverts
foi <- c("Tautogolabrus adspersus")
soi <- c("CABO", "CABOS")


#plot, excluding fish above 200 grams
ggplot(data = data_complete %>%filter(mean_biomass<200)  %>% 
         dplyr::filter(SP_CODE %in% soi), 
       mapping = aes(x = mean_observation_density, y = mean_biomass, color = SPECIES)) +
  geom_point() 



##################################
#how variable is kelp density?
quads_var <- quads_kelp %>%
group_by(YEAR, SITE, TRANSECT, QUAD) %>% 
  summarise(count = mean(COUNT))
quads_var <- quads_var %>%
  mutate(tran_id = paste(SITE, TRANSECT))

#basically bad box plots 
ggplot(data = quads_var, mapping = aes(x = tran_id, y = count)) +
  geom_point() +
  facet_wrap(~YEAR)


##########
#Day 2
#bring in quadrat data and convert to wide format at transect level



#append sizes on to species code so that each size/species combo ultimately gets a column
quads_wide <- quads %>%
    unite(SP_CODE_WITH_SIZE, SP_CODE, SIZE)
#remove all NA size codes
quads_wide$SP_CODE_WITH_SIZE <- str_replace(quads_wide$SP_CODE_WITH_SIZE,"_NA", "")

#grab relevant columns
quads_wide <- quads_wide %>%
  select(NETWORK, YEAR, SITE, 
         TRANSECT, QUAD, COUNT, SP_CODE_WITH_SIZE)

#summarise species counts at the transect level
quads_wide <- quads_wide %>%
group_by(YEAR, SITE, TRANSECT, SP_CODE_WITH_SIZE) %>%
  summarise(mean_count = mean(COUNT))

#go wide
quads_wide <- spread(quads_wide, key = SP_CODE_WITH_SIZE, value = mean_count)

#now fish 
fish_sum <- fish_sum %>%
  unite(SPECIES_WITH_SIZE, SPECIES, actual_size) 

fish_wide <- spread(fish_sum, key = SPECIES_WITH_SIZE, value = fish_mass)

#create master df with all algae, fish, and inverts
data_wide <- left_join(quads_wide, fish_wide) 


#######
#Now bring in environmental data at transect level - same workflow as before but 
#grouped at transect level instead of site

#temps
mean_temps <- sites %>%
  group_by(YEAR, SITE, TRANSECT) %>%
  summarise(mean_dive_computer_temp = mean(TEMPERATURE_C))

#cover
rocky_subs <- c('B', 'BL', 'BM', 'BS')
cover_rocky <- filter(cover, SP_CODE %in% rocky_subs)
cover_rocky <- cover_rocky %>%
  group_by(YEAR, TRANSECT, SITE) %>%
  summarise(sub_cover = sum(PERCENT_COVER))

#depths
mean_depths<- sites %>%
  group_by(YEAR, SITE, TRANSECT) %>%
  summarise(mean_start_depth = mean(START_DEPTH_M),
            mean_end_depth = mean(END_DEPTH_M))


#join to observational data
data_wide<- left_join(data_wide, cover_rocky)
data_wide<- left_join(data_wide, mean_temps)
data_wide<- left_join(data_wide, mean_depths)

write_csv(data_wide, "../data/quads_with_fish_wide.csv")

######
#attach kelp biomass data
biomass_clean <- kelp_biomass %>%
  select(SITE, TRANSECT, YEAR, SP_CODE,
         transect_mean_wet_weight, transect_mean_dry_weight,
         transect_median_wet_weight, transect_median_dry_weight)


biomass_sum <- biomass_clean %>%
  group_by(SITE, TRANSECT, YEAR, SP_CODE) %>%
  summarise(mean_wet_weight = mean(transect_mean_wet_weight))
  

biomass_wide <- biomass_sum %>%
  spread(key = SP_CODE, value = mean_wet_weight)


biomass_wide <- biomass_wide %>%
  rename(AGCL_wet_weight = AGCL,
         ALES_wet_weight = ALES,
        LADI_wet_weight = LADI,
         SL_wet_weight = SL,
         SLJ_wet_weight = SLJ)


#################################


all_data <- left_join(quads_with_fish_wide, kelp_biomass_wide)

#to do:
#replace NAs with 0s when appropriate

