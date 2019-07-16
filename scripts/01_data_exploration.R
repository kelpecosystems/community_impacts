library(tidyverse)
#KEEN 2019 Hack-A-Thon!


#load data
cover <- read_csv("../../observational_data/cleaned_data/keen_cover.csv")
fish <- read_csv("../../observational_data/cleaned_data/keen_fish.csv")
quads <- read_csv("../../observational_data/cleaned_data/keen_quads.csv")
sites <- read_csv("../../observational_data/cleaned_data/keen_sites.csv")
swath <- read_csv("../../observational_data/cleaned_data//keen_swath.csv")
kelp <- read_csv("../../observational_data/cleaned_data//keen_kelp.csv")
fish_biomass_coefs <- read_csv("../data/fish_biomass_coefs.csv")
kelp_biomass <- read_csv("../data/kelp_quads_biomass.csv")

#derived data
quads_with_fish_wide <- read_csv("../data/quads_with_fish_wide.csv")
kelp_biomass_wide <- read_csv("../data/kelp_biomass_wide.csv")



#grab biomass data from kelp size repo
download.file("http://raw.githubusercontent.com/kelpecosystems/kelp_size_dist_change/master/data/kelp_quads_biomass.csv", 
              destfile = "../data/kelp_quads_biomass.csv") #, method = "curl")



######################


#sea star decline? pull in temp data, look for patterns, compare up and down coast (GOM and south)


#methods comparison between protocols 
#crabs/stars in quads/swath
#browns/tunicates/molluscs in quads/upc
#Cnidarians (at least CEBO) - swath/upc


#protocol comparison: fish counts between swath and quad

#data wrangling

#prepare for joining
fish <- select(fish, -MONTH, -DAY)
fish$SIDE <- as.character(fish$SIDE)
#add length/weight coefficients
fish <- left_join(fish, fish_biomass_coefs, by = "SPECIES")



#fish length adjustments
fish_length <- data.frame(FISH.SIZE = c("YOY", "0-10", "10-50", "50-100", ">100"),
                          actual_size = c(5, 5, 15, 60, 60))
fish <- left_join(fish, fish_length)

#note that counts are per 80 meters

fish_sum <- fish %>%
  group_by(YEAR, SITE, TRANSECT, SPECIES, actual_size, ALPHA, BETA) %>%
  summarise(count = sum(COUNT)) %>%
  ungroup()

fish_sum$density <- fish_sum$count[]/80

fish_sum <- fish_sum %>%
  mutate(ind.fish_mass = ALPHA * (actual_size^BETA))

fish_sum <- fish_sum %>%
  mutate(fish_mass = density * ind.fish_mass)

#remove unneeded rows
fish_sum <- fish_sum %>%
  select(YEAR, SITE, TRANSECT, SPECIES, actual_size, fish_mass)


#quadrat data wrangling
#exclude juv. for now - not necessarily relevant for fish habitat
species_of_interest <- c('AGCL', 'ALES', 'LADI', 'SADE', 'SL', "HOAM", "CABO",
                         "CAIR", "MYSP", "PHGU", "ASFO", "ASFOS", "ASRU", "ASRUS", "HESA", "HESAS", "SDS", "SDL")
quads <- select(quads, -MONTH, -DAY) 
quads_kelp <- quads %>%
  filter(SP_CODE %in% species_of_interest)
  
  
#counts are already density because quads are 1 m2
quads_sum <- quads_kelp %>%
  group_by(YEAR, SITE, SP_CODE) %>% 
  summarise(mean_observation_density = mean(COUNT))

#temps
mean_temps <- sites %>%
  group_by(YEAR, SITE) %>%
  summarise(mean_dive_computer_temp = mean(TEMPERATURE_C))

#cover
rocky_subs <- c('B', 'BL', 'BM', 'BS')
cover_rocky <- filter(cover, SP_CODE %in% rocky_subs)
cover_rocky <- cover_rocky %>%
  group_by(YEAR, TRANSECT, SITE) %>%
  summarise(sub_cover = sum(PERCENT_COVER))


#depths
mean_depths<- sites %>%
  group_by(YEAR, SITE) %>%
  summarise(mean_start_depth = mean(START_DEPTH_M),
            mean_end_depth = mean(END_DEPTH_M))

#join with fish
data_complete <- left_join(quads_sum, fish_sum, by = c("YEAR", "SITE"))
data_complete <- left_join(data_complete, quads_sum, by = c("YEAR", "SITE"))

#join with temps, rocky cover, and depth
data_complete <- left_join(data_complete, mean_temps)
data_complete <- left_join(data_complete, cover_rocky)
data_complete <- left_join(data_complete, mean_depths)
#write_csv(data_complete, "../data/fish_quads_kelp_biomass.csv")


#filter fish of interest for plotting if needed
foi <- c("Tautogolabrus adspersus")
soi <- c("CABO", "CABOS")
ggplot(data = data_complete %>%filter(mean_biomass<200)  %>% dplyr::filter(SP_CODE %in% soi), 
       mapping = aes(x = mean_observation_density, y = mean_biomass, color = SPECIES)) +
  geom_point() 
  #geom_line()
  #facet_wrap(~YEAR)



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
#bring in quadrat data
#wide format at transect level
quads_wide <- quads %>%
    unite(SP_CODE_WITH_SIZE, SP_CODE, SIZE)
  
  
quads_wide$SP_CODE_WITH_SIZE <- str_replace(quads_wide$SP_CODE_WITH_SIZE,"_NA", "")

quads_wide <- quads_wide %>%
  select(NETWORK, YEAR, SITE, 
         TRANSECT, QUAD, COUNT, SP_CODE_WITH_SIZE)

quads_wide <- quads_wide %>%
group_by(YEAR, SITE, TRANSECT, SP_CODE_WITH_SIZE) %>%
  summarise(mean_count = mean(COUNT))

quads_wide <- spread(quads_wide, key = SP_CODE_WITH_SIZE, value = mean_count)

#now fish
fish_sum <- fish_sum %>%
  unite(SPECIES_WITH_SIZE, SPECIES, actual_size) 

fish_wide <- spread(fish_sum, key = SPECIES_WITH_SIZE, value = fish_mass)

data_wide <- left_join(quads_wide, fish_wide) 


#######
#environmental data at transect level

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



data_wide<- left_join(data_wide, cover_rocky)
data_wide<- left_join(data_wide, mean_temps)
data_wide<- left_join(data_wide, mean_depths)

test <- data_wide %>%
  select(YEAR, SITE, TRANSECT, mean_start_depth, mean_end_depth, mean_dive_computer_temp, sub_cover  )

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
#split up this document
#replace NAs with 0s when appropriate
#SEM!

