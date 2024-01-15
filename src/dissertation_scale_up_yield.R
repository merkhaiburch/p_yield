# # ------------------------------------------------------------------------------
# Author.... Merritt Khaipho-Burch
# Contact... mbb262@cornell.edu
# Date...... 2023-09-12
# Updated... 2024-01-04
#
# Description:
# Scaling up yield data from small-scale studies and counting sample sizes
# ------------------------------------------------------------------------------

# Load packages
library(dplyr)
library(ggplot2)
library(tidyr)

# Set working directory
setwd("~/Library/CloudStorage/Box-Box/Cornell_PhD/thesis/bad_yield_papers/data")

##  Function scales yield data --> plots to hectares --------
scale_yield_grams_2_taHa <- function(num_rows, num_plants_per_row, space_between_rows, space_between_plants, their_yield_grams) {
  
  # 1) scale up plots to meters
  # num_rows * num_plants_per_row * space_between_rows * space_between_plants = plot
  (plot <- num_rows*num_plants_per_row*space_between_rows*space_between_plants)
  
  # 2) square meters in a hectare
  # 1 hectare=(100 meters)^2=10000 meters^2
  hectare <- 10000
  
  # 3) number of their plots that could fit in a hectare
  # of their plots that could fit in a ha: (1 ha = 10,000m^2)/(plot) = their_plot_hectares
  their_plot_hectares <- hectare/plot
  
  # 4) number of their hectares times their yield per hectare
  # their_plot_hectares/ha * their_yield_grams = yield_ta_ha
  yield_ta_ha <- (their_plot_hectares*their_yield_grams)/1000000
  
  # Return results
  return(yield_ta_ha)
}

## Function scales yield data --> 1 m2 plots to hectares ----
scale_yield_plots_2_taHa <- function(their_plot_yield_grams) {
  
  # 1) scale up plots to meters
  # This function takes in a 1 square meter plot so it = 1
  plot <- 1
  
  # 2) square meters in a hectare
  # 1 hectare=(100 meters)^2=10000 meters^2
  hectare <- 10000
  
  # 3) number of their plots that could fit in a hectare
  # of their plots that could fit in a ha: (1 ha = 10,000m^2)/(plot) = their_plot_hectares
  their_plot_hectares <- hectare/plot
  
  # 4) number of their hectares times their yield per hectare
  # their_plot_hectares/ha * their_plot_yield_grams = yield_ta_ha
  yield <-  # in grams
  yield_ta_ha <- (their_plot_hectares*their_plot_yield_grams)/1000000
  
  # Return results
  return(yield_ta_ha)
}

## Function to add metadata ---------------
add_metadata <- function(df, study_id, year, crop, yield_type, country){
  
  # Combine study, year, crop, component measured, yield
  metadata_df <- cbind(rep(study_id, nrow(df)), 
                       rep(year, nrow(df)), 
                       rep(crop, nrow(df)), 
                       rep(yield_type, nrow(df)),
                       rep(country, nrow(df)))
  
  # Change column names
  colnames(metadata_df) <- c("study_id", "year", "crop", "yield_type", "country")
  
  # Combine with df
  df <- cbind(df, metadata_df)
  return(df)
}


# Get global average yields per crop -------------------------------------------

# Tobacco, wheat, soybean, rice, maize

# South 2019 Science -----------------------------------------------------------
# Tobacco

# Load data
south_s5 <- read.csv("South_2019_Science_2017Trial_TableS5.csv")
south_s10 <- read.csv("South_2019_Science_2016Trial_TableS10.csv")

# Count sample size by year
# south_s5 %>% group_by(Genotype, Block) %>% summarise(n = n())
# south_s10 %>% group_by(Genotype) %>% summarise(n = n())

# Control for block
model_s5 <- lm(formula = Leaf_Mass_g ~ Genotype + as.factor(Block), data = south_s5)
south_s5$fitted_leaf_mass_g <- fitted(model_s5)

# Take means across block
mean_model_s5 <- south_s5 %>% group_by(Genotype) %>% summarise(mean = mean(fitted_leaf_mass_g))
mean_model_s10 <- south_s10 %>% group_by(Genotype) %>% summarise(mean = mean(Leaf_g))

# Use function
mean_model_s5$yield_tons_ha <- scale_yield_grams_2_taHa(num_rows = 4, 
                         num_plants_per_row = 4, 
                         space_between_rows = 0.3, 
                         space_between_plants = 0.3, 
                         their_yield_grams = mean_model_s5$mean)
mean_model_s5 <- add_metadata(df = mean_model_s5,
                              study_id = "South 2019", 
                              year = 2016, 
                              crop = "Tobacco", 
                              yield_type = "Leaf Mass (g)",
                              country = "United States")

mean_model_s10$yield_tons_ha <- scale_yield_grams_2_taHa(num_rows = 4, 
                         num_plants_per_row = 4, 
                         space_between_rows = 0.3, 
                         space_between_plants = 0.3, 
                         their_yield_grams = mean_model_s10$mean)
mean_model_s10 <- add_metadata(df = mean_model_s10,
                               study_id = "South 2019", 
                               year = 2017, 
                               crop = "Tobacco", 
                               yield_type = "Leaf Mass (g)",
                               country = "United States")
                         
# Combine results
all_results <- rbind(mean_model_s5, mean_model_s10)

# Remove extra variables
rm(south_s5)
rm(south_s10)
rm(model_s5)
rm(mean_model_s5)
rm(mean_model_s10)


# Kromdijk 2016 Science --------------------------------------------------------
# Tobacco

# Load data
kromdijk <- read.csv("Kromdijk_2016_Science_SD15.csv")

# Count sample size
kromdijk %>% group_by(Genotype) %>% summarise(n = n())

# Control for block
model_kromdijk <- lm(formula = Leaf_g ~ Genotype + as.factor(Block), data = kromdijk)
kromdijk$fitted_leaf_mass_g <- fitted(model_kromdijk)

# Take means across block
scale_yield <- kromdijk %>% group_by(Genotype) %>% summarise(mean = mean(fitted_leaf_mass_g))

# Use functions
scale_yield$yield_tons_ha <- scale_yield_grams_2_taHa(num_rows = 1,
                                          num_plants_per_row = 4, 
                                          space_between_rows = 0.3, 
                                          space_between_plants = 0.3, 
                                          their_yield_grams = scale_yield$mean)
scale_yield <- add_metadata(df = scale_yield,
                            study_id = "Kromdijk 2016", 
                            year = 2016, 
                            crop = "Tobacco", 
                            yield_type = "Leaf Mass (g)",
                            country = "United States")

# combine results
all_results <- rbind(all_results, scale_yield)

# Remove variables
rm(scale_yield)
rm(model_kromdijk)
rm(kromdijk)


# Yoon 2020 Nature Food --------------------------------------------------------

# Load data, remove low yielding antisense treatment
yoon_mt2 <- read.csv("Yoon_2020_mainTable2.csv") %>% select(-"RBCSantisense_grain_yield_grams_per_meter_squared")

# Melt data
long <- yoon_mt2 %>% 
  pivot_longer(
    cols = `Wild_type_grain_yield_grams_per_meter_squared`:`RBCS_sense_grain_yield_grams_per_meter_squared`, 
    names_to = "genotype",
    values_to = "mean"
  )

# Clean up names
long$genotype <- gsub("_grain_yield_grams_per_meter_squared", "", long$genotype)
long$genotype <- gsub("Wild_type", "WT", long$genotype)
long$genotype <- paste0(long$genotype, "_NLevel", long$N_fertilization_grams_n_per_meter_squared)
long <- long %>% select(-"N_fertilization_grams_n_per_meter_squared")

# Use function
long$yield_tons_ha <- scale_yield_plots_2_taHa(their_plot_yield_grams = long$mean)
long <- add_metadata(df = long,
                     study_id = "Yoon 2020",
                     year = 2020,
                     crop = "Rice",
                     yield_type = "Grain Yield (g)",
                     country = "Japan")

# Fix the year
long$year <- long$Year
long <- long %>% select(-"Year")
colnames(long)[1] <- "Genotype"

# Combine results
all_results <- rbind(all_results, long)

# Remove extra variables
rm(yoon_mt2)
rm(long)


# De Souza 2022 Science --------------------------------------------------------

# Load data
desouza_2020 <- read.csv("DeSouza_2022_Science_2020Yield_D1_mainmanuscript.csv")
desouza_2021 <- read.csv("DeSouza_2022_Science_2021Yield_D10.csv") %>% select(-"Weight_100_seeds_g", -"Number_seeds_per_plant_g")

# Take mean
colnames(desouza_2020)[1] <- "Genotype"
colnames(desouza_2021)[1] <- "Genotype"
desouza_2020 <- desouza_2020 %>% group_by(Genotype) %>% summarise(mean = mean(Yield_tons_per_hectare))
desouza_2021 <- desouza_2021 %>% group_by(Genotype) %>% summarise(mean = mean(yield_t_ha))

# They already provide data in tons per hectare, copy the column to a different name
desouza_2020$yield_tons_ha <- desouza_2020$mean
desouza_2021$yield_tons_ha <- desouza_2021$mean

# add metadata
desouza_2020 <- add_metadata(df = desouza_2020,
                            study_id = "DeSouza 2022", 
                            year = 2020, 
                            crop = "Soy", 
                            yield_type = "Plot Yield (ta/ha)",
                            country = "United States")
desouza_2021 <- add_metadata(df = desouza_2021,
                            study_id = "DeSouza 2022", 
                            year = 2021, 
                            crop = "Soy", 
                            yield_type = "Grain Yield (ta/ha)",
                            country = "United States")

# Combine results
all_results <- rbind(all_results, desouza_2020, desouza_2021)

# Remove extra variables
rm(desouza_2020)
rm(desouza_2021)


# Wei 2022 rice ----------------------------------------------------------------

# Load data, contains plot yield
wei_2022_rice <- read.csv("Wei_2022_Science_TableS1_to_S5.csv") %>% select("Genotype", "location_year", "grain_yield_per_plot_grams")
colnames(wei_2022_rice)[3] <- "mean"

# Use functions
wei_2022_rice$yield_tons_ha <- scale_yield_grams_2_taHa(num_rows = 20,
                                        num_plants_per_row = 10,
                                        space_between_rows = (23.3/100),
                                        space_between_plants = (16.7/100),
                                        their_yield_grams = wei_2022_rice$mean)
wei_2022_rice <- add_metadata(df = wei_2022_rice,
                            study_id = "Wei 2022", 
                            year = 2022, 
                            crop = "Rice", 
                            yield_type = "Grain Yield (g)",
                            country = "China")

# Swap years
wei_2022_rice$year <- wei_2022_rice$location_year
wei_2022_rice <- wei_2022_rice %>% select(-location_year)

# Combine results
all_results <- rbind(all_results, wei_2022_rice)

# Remove extra variables
rm(wei_2022_rice)


# # Wei 2022 wheat ---------------------------------------------------------------
# 
# # Load data
# temp <- read.delim("temp.tsv")
# 
# # Use functions
# temp$yield_tons_ha <- temp_grams_2_taHa(num_rows = ,
#                                                       num_plants_per_row = ,
#                                                       space_between_rows = ,
#                                                       space_between_plants = ,
#                                                       their_yield_grams = )
# temp <- add_metadata(df = temp,
#                             study_id = "", 
#                             year = , 
#                             crop = "", 
#                             yield_type = "")
# 
# 
# # Combine results
# all_results <- rbind(all_results, )
# 
# # Remove extra variables
# rm(temp)


# Song 2023 wheat --------------------------------------------------------------

# Load data, data comes as: grain_yield_kg_ha
song_2023 <- read.csv("Song_2023_Nature_Table1.csv") %>% select(-percent_difference)
colnames(song_2023) <- c("Genotype", "mean")

# Make their kg data to metric tons
song_2023$yield_tons_ha <- song_2023$mean/1000

# Add metadata
song_2023 <- add_metadata(df = song_2023,
                          study_id = "Song 2023", 
                          year = 2020, 
                          crop = "Wheat", 
                          yield_type = "Grain Yield (kg/ha)",
                          country = "China")


# Combine results
all_results <- rbind(all_results, song_2023)

# Remove extra variables
rm(song_2023)


# Chen 2022 maize --------------------------------------------------------------

# Load data
chen_2022_maize <- read.csv("Chen_2022_Science_TableS3MainFigure4B_maize.csv") %>% select(genotype, environment, mean_gy_kg_ha)
colnames(chen_2022_maize) <- c("Genotype", "environment", "mean")

# Data comes as kg per ha, change to metric tons per hectare
chen_2022_maize$yield_tons_ha <- chen_2022_maize$mean/1000

# Add metadata
chen_2022_maize <- add_metadata(df = chen_2022_maize,
                     study_id = "Chen 2022", 
                     year = 2020, 
                     crop = "Maize", 
                     yield_type = "Grain Yield (kg/ha)",
                     country = "China")

# Replace year column
chen_2022_maize$environment <- gsub("18", "2018_", chen_2022_maize$environment)
chen_2022_maize$environment <- gsub("19", "2019_", chen_2022_maize$environment)
chen_2022_maize$year <- chen_2022_maize$environment
chen_2022_maize <- chen_2022_maize %>% select(-environment)

# Combine results
all_results <- rbind(all_results, chen_2022_maize)

# Remove extra variables
rm(chen_2022_maize)


# Chen 2022 rice ---------------------------------------------------------------

# Dropped because no plot size information was available to scale


# # Zhang 2022 wheat -------------------------------------------------------------
# 
# # Load data
# temp_t2 <- read.csv("Zhang_2022_Science_TableS3.csv") %>% filter(generation == "T2") %>% select(genotype, grain_yield_kg_meters_squared)
# temp_t3 <- read.csv("Zhang_2022_Science_TableS3.csv") %>% filter(generation == "T3") %>% select(genotype, grain_yield_kg_meters_squared)
# 
# # Use functions
# temp$yield_tons_ha <- scale_yield_grams_2_taHa(num_rows = ,
#                                                       num_plants_per_row = ,
#                                                       space_between_rows = ,
#                                                       space_between_plants = ,
#                                                       their_yield_grams = )
# temp <- add_metadata(df = temp,
#                             study_id = "", 
#                             year = , 
#                             crop = "", 
#                             yield_type = "")
# 
# 
# # Combine results
# all_results <- rbind(all_results, )
# 
# # Remove extra variables
# rm(temp)

# temp -------------------------------------------------------------------------

# Load in yield data
avgYield <- read.csv("average_yields_2021.csv")
colnames(avgYield) <- c("crop", "country", "year", "yield")

# Merge in yield data
merged_all_results <- merge(all_results, avgYield, by = c("country", "crop"))
merged_all_results$yield_difference <- merged_all_results$yield - merged_all_results$yield_tons_ha

# Make plot
library(ggplot2)
ggplot(merged_all_results, aes(x = Genotype, y = yield_difference)) +
  geom_boxplot() +
  facet_wrap(vars(study_id), scales = "free")


# Yu 2021 rice -----------------------------------------------------------------
# Yu 2021 potato ---------------------------------------------------------------
# Wu 2023 rice -----------------------------------------------------------------
# Song 2007 rice ---------------------------------------------------------------
# Li 2023 rice -----------------------------------------------------------------
# Li 2018 rice -----------------------------------------------------------------
# Li 2018 wheat ----------------------------------------------------------------
# Wang 2008 rice ---------------------------------------------------------------
# Xue 2008 rice ----------------------------------------------------------------
# Wang 2022 wheat --------------------------------------------------------------

# Load data
temp <- read.csv("")

# Use functions
temp$yield_tons_ha <- scale_yield_grams_2_taHa(num_rows = ,
                                                      num_plants_per_row = ,
                                                      space_between_rows = ,
                                                      space_between_plants = ,
                                                      their_yield_grams = )
temp <- add_metadata(df = temp,
                            study_id = "", 
                            year = , 
                            crop = "", 
                            yield_type = "")


# Combine results
all_results <- rbind(all_results, )

# Remove extra variables
rm(temp)
