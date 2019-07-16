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
  summarise(count = sum(COUNT)) 

fish_sum$density <- fish_sum$count[]/80

fish_sum <- fish_sum %>%
  mutate(ind.fish_mass = ALPHA * (actual_size^BETA))

fish_sum <- fish_sum %>%
  mutate(fish_mass = density * ind.fish_mass)

#quadrat data wrangling
#exclude juv. for now - not necessarily relevant for fish habitat
kelps <- c('AGCL', 'ALES', 'LADI', 'SADE', 'SL')
quads <- select(quads, -MONTH, -DAY) 
quads_kelp <- quads %>%
  filter(SP_CODE %in% kelps)
  
  
#counts are already density because quads are 1 m2
quads_sum <- quads_kelp %>%
  group_by(YEAR, SITE) %>% 
  summarise(mean_kelp_density = mean(COUNT))

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
data_complete <- left_join(site_sum, quads_sum, by = c("YEAR", "SITE"))

#join with temps, rocky cover, and depth
data_complete <- left_join(data_complete, mean_temps)
data_complete <- left_join(data_complete, cover_rocky)
data_complete <- left_join(data_complete, mean_depths)


#filter fish of interest for plotting if needed
foi <- c("Tautogolabrus adspersus")

ggplot(data = data_complete %>%filter(mean_biomass<200), 
       mapping = aes(x = mean_kelp_density, y = log(mean_biomass), color = SPECIES)) +
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

str(quads)

quads_wide <- quads %>%
  select(-)
quads_wide <- spread(quads, key = SPECIES, value = COUNT)



