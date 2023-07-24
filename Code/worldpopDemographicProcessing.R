# load packages ####
library(tidyverse)

# load data set 1 ####
# worldpop processed data for all cities
worldpop_all <- read.csv("")

# age adjusted national-level death rate
death_rate_age_raw <- read.csv("")

# extract useful information for later ####
city_details <- select(worldpop_all, zone, city_name, country_iso, country_name, continent_name, latitude, longitude)
city_details <- distinct(city_details) # keep only one set of observations per city

# calculate young, working, old, total population for each city/year ####

worldpop_all_wide <- worldpop_all %>%
  select(year, age, sum, zone)

worldpop_all_wide <- worldpop_all_wide %>%
  group_by(zone, year, age) %>%
  summarise_each(funs(sum))

worldpop_all_wide$age <- as.factor(worldpop_all_wide$age)
worldpop_all_wide <- spread(worldpop_all_wide, age, sum)

worldpop_all_wide <- worldpop_all_wide %>% 
  rowwise() %>% 
  mutate(young_sum = sum(c(`0`,`1`,`5`,`10`))) %>%
  mutate(working_sum = sum(c(`15`, `20`,`25`,`30`,`35`,`40`,`45`,`50`,`55`,`60`))) %>%
  mutate(old_sum = sum(c(`65`,`70`,`75`,`80`))) %>%
  mutate(total_pop = sum(c(`0`,`1`,`5`,`10`,`15`, `20`,`25`,`30`,`35`,`40`,`45`,`50`,`55`,`60`,`65`, `70`,`75`,`80`))) %>%
  mutate(birthing_age = sum(c(`15`, `20`,`25`,`30`,`35`,`40`,`45`))) %>%
  mutate(birth_rate = (`0`/(total_pop/1000))) %>%
  mutate(fertility_rate = ((`0`/birthing_age)*1000))

# keep useful data
worldpop_all_wide <- dplyr::select(worldpop_all_wide, zone, year, young_sum, working_sum, old_sum, total_pop, birthing_age, birth_rate, fertility_rate)


# calculate dependency ratio ####

# total dependency ratio
worldpop_all_wide <- worldpop_all_wide %>%
  mutate(dependency_ratio = ((young_sum + old_sum)/working_sum))

# young dependency ratio
worldpop_all_wide <- worldpop_all_wide %>%
  mutate(young_dependency_ratio = (young_sum/working_sum))

# old dependency ratio
worldpop_all_wide <- worldpop_all_wide %>%
  mutate(old_dependency_ratio = (old_sum/working_sum))


# calculate sex ratio ####
sex_ratio = dplyr::select(worldpop_all, zone, year, sex, age, sum)

sex_ratio <- sex_ratio %>%
  tidyr::pivot_wider(
    names_from  = c(age, sex), # Can accommodate more variables, if needed.
    values_from = c(sum))

sex_ratio <- sex_ratio %>% 
  rowwise() %>% 
  mutate(young_sum_male = sum(c(`0_m`,`1_m`,`5_m`,`10_m`,`15_m`))) %>%
  mutate(young_sum_female = sum(c(`0_f`,`1_f`,`5_f`,`10_f`,`15_f`))) %>%
  mutate(working_sum_male = sum(c(`20_m`,`25_m`,`30_m`,`35_m`,`40_m`,`45_m`,`50_m`,`55_m`,`60_m`,`65_m`))) %>%
  mutate(working_sum_female = sum(c(`20_f`,`25_f`,`30_f`,`35_f`,`40_f`,`45_f`,`50_f`,`55_f`,`60_f`,`65_f`))) %>%
  mutate(old_sum_male = sum(c(`70_m`,`75_m`,`80_m`))) %>%
  mutate(old_sum_female = sum(c(`70_f`,`75_f`,`80_f`))) %>%
  mutate(total_pop_male = sum(c(`0_m`,`1_m`,`5_m`,`10_m`,`15_m`, `20_m`,`25_m`,`30_m`,`35_m`,`40_m`,`45_m`,`50_m`,`55_m`,`60_m`,`65_m`, `70_m`,`75_m`,`80_m`))) %>%
  mutate(total_pop_female = sum(c(`0_f`,`1_f`,`5_f`,`10_f`,`15_f`, `20_f`,`25_f`,`30_f`,`35_f`,`40_f`,`45_f`,`50_f`,`55_f`,`60_f`,`65_f`, `70_f`,`75_f`,`80_f`))) %>%
  mutate(total_pop = sum(c(`0_m`,`1_m`,`5_m`,`10_m`,`15_m`, `20_m`,`25_m`,`30_m`,`35_m`,`40_m`,`45_m`,`50_m`,`55_m`,`60_m`,`65_m`, `70_m`,`75_m`,`80_m`,
                           `0_f`,`1_f`,`5_f`,`10_f`,`15_f`, `20_f`,`25_f`,`30_f`,`35_f`,`40_f`,`45_f`,`50_f`,`55_f`,`60_f`,`65_f`, `70_f`,`75_f`,`80_f`)))


sex_ratio <- dplyr::select(sex_ratio, zone, year,
                           young_sum_male, young_sum_female, 
                           working_sum_male, working_sum_female, 
                           old_sum_male, old_sum_female, 
                           total_pop_male, total_pop_female,
                           total_pop)

sex_ratio <- sex_ratio %>%
  mutate(young_sex_ratio = ((young_sum_male/young_sum_female)*100)) %>%
  mutate(working_sex_ratio = ((working_sum_male/working_sum_female)*100)) %>%
  mutate(old_sex_ratio = ((old_sum_male/old_sum_female)*100)) %>%
  mutate(total_sex_ratio = ((total_pop_male/total_pop_female)*100)) %>%
  select(zone, year, young_sex_ratio, working_sex_ratio, old_sex_ratio, total_sex_ratio)

worldpop_all_wide <- left_join(worldpop_all_wide, sex_ratio, by = c('zone', 'year'), all.x=TRUE)

# calculate city-size for each city/year ####
worldpop_all_wide <- worldpop_all_wide %>% 
  mutate(city_size = case_when(total_pop <= 50000 ~ "<50k",
                               total_pop > 50000 & total_pop <= 100000 ~ "50-100k",
                               total_pop > 100000 & total_pop <= 300000 ~ "100-300k",
                               total_pop > 300000 & total_pop <= 500000 ~ "300-500k",
                               total_pop > 500000 & total_pop <= 1000000 ~ "500k-1M",
                               total_pop > 1000000 & total_pop <= 5000000 ~ "1-5M",
                               total_pop > 5000000 & total_pop <= 10000000 ~ "5-10M",
                               total_pop > 10000000 ~ ">10M"))

# set city_size as a factor and order appropriately
worldpop_all_wide$city_size <- factor(worldpop_all_wide$city_size, levels = c("<50k", "50-100k", "100-300k", "300-500k", "500k-1M", "1-5M", "5-10M", ">10M"))

# merge back in useful information ####
#merge back in city_name and continent
worldpop_all_wide <- left_join(worldpop_all_wide, city_details, by = c('zone'), all.x=TRUE)

length(unique(worldpop_all_wide$zone))

# save data set 2 ####

write.csv(worldpop_all_wide, "")

# calculate change in key variables ####

change_raw <- dplyr::select(worldpop_all_wide, zone, year,
                            young_sum, working_sum, old_sum, total_pop,
                            birthing_age, birth_rate, fertility_rate,
                            dependency_ratio, young_dependency_ratio, old_dependency_ratio, 
                            young_sex_ratio, working_sex_ratio, old_sex_ratio, total_sex_ratio)

change_raw <- filter(change_raw, year %in% c(2000, 2020))

change_data <- change_raw %>%
  group_by(zone) %>%
  arrange(year) %>%
  
  mutate(young_sum_delta = `young_sum` - lag(`young_sum`)) %>%
  mutate(young_sum_perc = (young_sum_delta / lag(`young_sum`))*100) %>%
  
  mutate(working_sum_delta = `working_sum` - lag(`working_sum`)) %>%
  mutate(working_sum_perc = (working_sum / lag(`working_sum`))*100) %>%
  
  mutate(old_sum_delta = `old_sum` - lag(`old_sum`)) %>%
  mutate(old_sum_perc = (old_sum / lag(`old_sum`))*100) %>%
  
  mutate(total_pop_delta = `total_pop` - lag(`total_pop`)) %>%
  mutate(total_pop_perc = (total_pop / lag(`total_pop`))*100) %>%
  
  mutate(birthing_age_delta = `birthing_age` - lag(`birthing_age`)) %>%
  mutate(birthing_age_perc = (birthing_age / lag(`birthing_age`))*100) %>%
  
  mutate(birth_rate_delta = `birth_rate` - lag(`birth_rate`)) %>%
  mutate(birth_rate_perc = (birth_rate / lag(`birth_rate`))*100) %>%
  
  mutate(fertility_rate_delta = `fertility_rate` - lag(`fertility_rate`)) %>%
  mutate(fertility_rate_perc = (fertility_rate / lag(`fertility_rate`))*100) %>%
  
  mutate(dependency_ratio_delta = `dependency_ratio` - lag(`dependency_ratio`)) %>%
  mutate(dependency_ratio_perc = (dependency_ratio / lag(`dependency_ratio`))*100) %>%
  
  mutate(young_dependency_ratio_delta = `young_dependency_ratio` - lag(`young_dependency_ratio`)) %>%
  mutate(young_dependency_ratio_perc = (young_dependency_ratio / lag(`young_dependency_ratio`))*100) %>%
  
  mutate(old_dependency_ratio_delta = `old_dependency_ratio` - lag(`old_dependency_ratio`)) %>%
  mutate(old_dependency_ratio_perc = (old_dependency_ratio / lag(`old_dependency_ratio`))*100) %>%
  
  mutate(young_sex_ratio_delta = `young_sex_ratio` - lag(`young_sex_ratio`)) %>%
  mutate(young_sex_ratio_perc = (young_sex_ratio / lag(`young_sex_ratio`))*100) %>%
  
  mutate(working_sex_ratio_delta = `working_sex_ratio` - lag(`working_sex_ratio`)) %>%
  mutate(working_sex_ratio_perc = (working_sex_ratio / lag(`working_sex_ratio`))*100) %>%
  
  mutate(old_sex_ratio_delta = `old_sex_ratio` - lag(`old_sex_ratio`)) %>%
  mutate(old_sex_ratio_perc = (old_sex_ratio / lag(`old_sex_ratio`))*100) %>%
  
  mutate(total_sex_ratio_delta = `total_sex_ratio` - lag(`total_sex_ratio`)) %>%
  mutate(total_sex_ratio_perc = (total_sex_ratio / lag(`total_sex_ratio`))*100) %>%
  
  filter(year == 2020)

change_data <- left_join(change_data, city_details, by = c('zone'), all.x=TRUE)

# add city size
change_data <- change_data %>% 
  mutate(city_size = case_when(total_pop <= 50000 ~ "<50k",
                               total_pop > 50000 & total_pop <= 100000 ~ "50-100k",
                               total_pop > 100000 & total_pop <= 300000 ~ "100-300k",
                               total_pop > 300000 & total_pop <= 500000 ~ "300-500k",
                               total_pop > 500000 & total_pop <= 1000000 ~ "500k-1M",
                               total_pop > 1000000 & total_pop <= 5000000 ~ "1-5M",
                               total_pop > 5000000 & total_pop <= 10000000 ~ "5-10M",
                               total_pop > 10000000 ~ ">10M"))

# set city_size as a factor and order appropriately
change_data$city_size <- factor(change_data$city_size, levels = c("<50k", "50-100k", "100-300k", "300-500k", "500k-1M", "1-5M", "5-10M", ">10M"))

# process migration data ####

migration_data <-worldpop_all %>%
  select(year, age, sum, zone) %>%
  group_by(zone, year, age) %>%
  summarise_each(funs(sum))

birth_data <- migration_data %>%
  filter(age == 0) %>%
  group_by(zone, year) %>%
  summarise(total_births = sum(sum))

total_data <- migration_data %>%
  group_by(zone, year) %>%
  summarise(total_pop = sum(sum))

migration_calc_data <- merge(birth_data, total_data, by = c("zone", "year"))
migration_calc_data <- left_join(x = migration_calc_data, y = worldpop_all_wide[, c("zone", "country_iso")], by = "zone", all.x=TRUE)
migration_calc_data <- unique(migration_calc_data)

migration_calc_data$year <- as.factor(migration_calc_data$year)
death_rate_age_raw$year <- as.factor(death_rate_age_raw$year)

#add in death rate data
migration_calc_data <- left_join(migration_calc_data, death_rate_age_raw, by = c('country_iso', 'year'), all.x=TRUE)

migration_calc_data <- unique(migration_calc_data)
migration_calc_data <- migration_calc_data %>% select(zone, country_iso, country_name, year, total_births, total_pop, age_standardized_death_rate)
colnames(migration_calc_data) <- c("zone", "country_iso", "country_name", "year", "total_births", "total_pop", "death_rate_age")

migration_calc_data$total_deaths_age <- ((migration_calc_data$total_pop/100000) * migration_calc_data$death_rate_age)

# condense to include totals for births and deaths

migration_calc_data_condensed <- migration_calc_data %>%
  group_by(zone) %>%
  summarise(
    total_births_all = sum(total_births),
    total_deaths_age_all = sum(total_deaths_age))

migration_calc_data_condensed <- left_join(x = migration_calc_data_condensed, y = change_data[, c("zone", "total_pop_delta")], by = "zone", all.x=TRUE)
migration_calc_data_condensed <- left_join(x = migration_calc_data_condensed, y = worldpop_all_wide_2020[, c("zone", "total_pop")], by = "zone", all.x=TRUE)

migration_calc_data_condensed$natural_change = migration_calc_data_condensed$total_births_all - migration_calc_data_condensed$total_deaths_age_all

migration_calc_data_condensed$total_migration = migration_calc_data_condensed$total_pop_delta - (migration_calc_data_condensed$total_births_all-migration_calc_data_condensed$total_deaths_age_all)

migration_calc_data_condensed$migration_perc = migration_calc_data_condensed$total_migration / migration_calc_data_condensed$total_pop_delta * 100

migration_calc_data_condensed <- select(migration_calc_data_condensed, zone, total_births_all, total_deaths_age_all, natural_change, total_migration, migration_perc)

migration_calc_data_condensed <- left_join(migration_calc_data_condensed, change_data, by = c('zone'), all.x=TRUE)
leaflet_dataset <- migration_calc_data_condensed

migration_calc_data_condensed <- select(migration_calc_data_condensed, zone, city_name, country_iso, country_name, continent_name, latitude, longitude, total_pop, total_pop_delta, total_pop_perc,
                                        total_births_all, total_deaths_age_all, natural_change, total_migration, migration_perc, young_sum_delta, young_sum_perc, working_sum_delta, 
                                        working_sum_perc, old_sum_delta, old_sum_perc, birthing_age_delta, birthing_age_perc, birth_rate_delta, birth_rate_perc, fertility_rate_delta, fertility_rate_perc,
                                        dependency_ratio_delta, young_dependency_ratio_delta, old_dependency_ratio_delta, young_sex_ratio_delta, working_sex_ratio_delta, old_sex_ratio_delta,
                                        total_sex_ratio_delta)

# save data set 3 ####

write.csv(migration_calc_data_condensed, "")

# assemble data set for leaflet mapping (data set 2 and 3 together) ####

write.csv(leaflet_dataset, "")
