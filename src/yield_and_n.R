# ------------------------------------------------------------------------------
# Author.... Merritt Khaipho-Burch
# Contact... mbb262@cornell.edu
# Date...... 2023-05-03
# Updated... 2023-05-03
#
# Description:
# - Plot amount of cropland fertilizer scaled by yield and year
# Nitrogen data: "Land, Inputs, and Sustainability" --> "Sustainability Indicators" -->
#       Cropland Nutrient Budget for all countries, Synthetic fertilizers, cropland N per unit area, for all years
# Yield: Production --> "Crops and livestock products"
# ------------------------------------------------------------------------------

# Load packages
library(dplyr)
library(ggplot2)

# Load data
nitrogen <- read.csv("~/Downloads/FAOSTAT_data_en_5-3-2023 Cropland Nutrient Budget.csv")
yield <- read.csv("~/Downloads/FAOSTAT_data_yield.csv")
rice <- read.csv("~/Downloads/FAOSTAT_data_en_5-4-2023.csv")

# Subset data by country
nitrogen_maize <- nitrogen %>% filter(Area %in% c("China", "India", "Mexico", "United States of America", "Brazil"))
nitrogen_maize <- nitrogen_maize %>% filter(Item  == "Synthetic Fertilizers")
yield <- yield %>% filter(Area %in% c("China", "India", "Mexico", "United States of America", "Brazil"))

rice <- rice %>% filter(Area %in% c("China", "India", "Indonesia", "Bangladesh", "Viet Nam", "Thailand"))
nitrogen_rice <- nitrogen %>% filter(Area %in% c("China", "India", "Indonesia", "Bangladesh", "Viet Nam", "Thailand"))
nitrogen_rice <- nitrogen_rice %>% filter(Item  == "Synthetic Fertilizers")

# Merge yield and nitrogen by year and country
yield_nitrogen <- merge(x = nitrogen_maize, y = yield, 
                        by = c("Area", "Year"), 
                        suffixes = c("_N", "_yield"))

rice_nitrogen <- merge(x = nitrogen_rice, y = rice, 
                        by = c("Area", "Year"), 
                        suffixes = c("_N", "_yield"))

# Make yield in kg/ha not hg/ha
yield_nitrogen$Value_yield <- yield_nitrogen$Value_yield * 0.1
rice_nitrogen$Value_yield <- rice_nitrogen$Value_yield * 0.1

# Scale nitrogen by year
yield_nitrogen$scaled_yield <- yield_nitrogen$Value_N/yield_nitrogen$Value_yield

# Scale it for each unit of N
yield_nitrogen$scaled_N <- yield_nitrogen$Value_yield/yield_nitrogen$Value_N
rice_nitrogen$scaled_N <- rice_nitrogen$Value_yield/rice_nitrogen$Value_N


# Set plotting valirables
axis_text_size <- 8
title_text_size <- 9.5
tag_size <- 10
filt_text_phys <- 22
ran_text_phys <- 21
filt_text <- 10
ran_text <- 8
r2_text_x <- 0
annotate_size <- 2.5
pointSize <- 0.5
legend_text_size <- 8
legend_shape_size <- 0.6


# Plot scaled yield data
scaled_yield <- ggplot(yield_nitrogen, aes(x = Year, y = scaled_yield, group = Area)) +
  geom_line(aes(color=Area)) +
  xlab("Year") +
  ylab("Yield (kg/ha) / Applied Nitrogen (kg/ha)") +
  theme_classic() +
  theme(axis.text=element_text(size=axis_text_size), 
        axis.title = element_text(size=axis_text_size),
        plot.title = element_text(size=title_text_size), 
        legend.position="bottom", 
        legend.text = element_text(size = legend_text_size),
        legend.title=element_blank(),
        legend.key.size = unit(legend_shape_size, 'cm'))
ggsave(plot = scaled_yield, filename = "~/Downloads/scaled.png", height = 4, width = 5, units = "in")

# Plot scaled N data - maize
scaled_n <- ggplot(yield_nitrogen, aes(x = Year, y = scaled_N, group = Area)) +
  geom_line(aes(color=Area)) +
  xlab("Year") +
  ylab("Applied Nitrogen (kg/ha) / Yield (kg/ha)") +
  theme_classic() +
  theme(axis.text=element_text(size=axis_text_size), 
        axis.title = element_text(size=axis_text_size),
        plot.title = element_text(size=title_text_size), 
        legend.position="bottom", 
        legend.text = element_text(size = legend_text_size),
        legend.title=element_blank(),
        legend.key.size = unit(legend_shape_size, 'cm'))
ggsave(plot = scaled_n, filename = "~/Downloads/scaled_n.png", height = 4, width = 5, units = "in")

# Plot scaled N data - rice
scaled_n_rice <- ggplot(rice_nitrogen, aes(x = Year, y = scaled_N, group = Area)) +
  geom_line(aes(color=Area)) +
  xlab("Year") +
  ylab("Applied Nitrogen (kg/ha) / Rice Yield (kg/ha)") +
  theme_classic() +
  theme(axis.text=element_text(size=axis_text_size), 
        axis.title = element_text(size=axis_text_size),
        plot.title = element_text(size=title_text_size), 
        legend.position="bottom", 
        legend.text = element_text(size = legend_text_size),
        legend.title=element_blank(),
        legend.key.size = unit(legend_shape_size, 'cm'))
ggsave(plot = scaled_n_rice, filename = "~/Downloads/scaled_n_rice.png", height = 4, width = 5, units = "in")


# Plot regular yield
a <- ggplot(yield_nitrogen, aes(x = Year, y = Value_yield, group = Area)) +
  geom_line(aes(color=Area)) +
  xlab("Year") +
  ylab("Yield (kg/ha)") +
  theme_classic() +
  theme(axis.text=element_text(size=axis_text_size), 
        axis.title = element_text(size=axis_text_size),
        plot.title = element_text(size=title_text_size), 
        legend.position="bottom", 
        legend.text = element_text(size = legend_text_size),
        legend.title=element_blank(),
        legend.key.size = unit(legend_shape_size, 'cm'))
ggsave(plot = a, filename = "~/Downloads/yield.png", height = 4, width = 5, units = "in")

# Plot nitrogen
nitro <- ggplot(yield_nitrogen, aes(x = Year, y = Value_N, group = Area)) +
  geom_line(aes(color=Area)) +
  xlab("Year") +
  ylab("Applied Nitrogen (kg/ha)") +
  theme_classic() +
  theme(axis.text=element_text(size=axis_text_size), 
        axis.title = element_text(size=axis_text_size),
        plot.title = element_text(size=title_text_size), 
        legend.position="bottom", 
        legend.text = element_text(size = legend_text_size),
        legend.title=element_blank(),
        legend.key.size = unit(legend_shape_size, 'cm'))
ggsave(plot = nitro, filename = "~/Downloads/nitrogen.png", height = 4, width = 5, units = "in")






