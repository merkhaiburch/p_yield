# ------------------------------------------------------------------------------
# Author.... Merritt Khaipho-Burch
# Contact... mbb262@cornell.edu
# Date...... 2023-08-18
# Updated... 2023-08-18
#
# Description:
# Plot yield over time in the famous yield figure from the civil war to 2022
# Data taken from:
# https://usda.library.cornell.edu/concern/publications/c534fn92g
#
# Classifed into years using:
# https://doi.org/10.2135/cropsci2005.0065
# Interdisciplinary strategies to enable data-driven plant breeding in a changing climate
# ------------------------------------------------------------------------------

# Load packages
library(ggplot2)
library(dplyr)
library(broom)
library(tidyverse) 

# Set directory
setwd("~/../mbb262-admin/Library/CloudStorage/Box-Box/git_projects/p_yield/data")

# Colorblind pallete
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# Load data
yield <- read.csv("corn_yield_by_year_1886_2022.csv")
colnames(yield)[3] <- "Value"


# Plot yield and annotate with eras --------------------------------------------

# Add a new, simplified color annotatio 
yield <- yield %>% 
  mutate(Era2 = if_else(Year < 1930, "Open-pollinated", "Hybrid"))

# Change order that legened will appear
yield$Era2 <- factor(yield$Era2, levels = c("Open-pollinated", "Hybrid"))

# Remove years before 1900
yield <- yield %>% filter(Year >= 1900)

# Calculate tons per hectare
yield$taha <- (((yield$Value * 56) * 0.4536) * 2.471)/1000
(((200*56)*0.4536)*2.471)/1000 # Testing example from the ag decision maker (File C6-80)

# Annotated plot
anno_size = 5
scaling_factor = 0.095
22
v1 <- ggplot(yield, aes(x = Year, y = taha)) +
  geom_point() +
  ylab("Average US Corn Yield (tons/ha)") +
  theme_classic() +
  theme(legend.position = c(0.2, 0.85),
        legend.title=element_blank(),
        text = element_text(size=20)) +
  scale_color_manual(values=cbPalette) +
  xlim(1900,2035) +
  annotate(geom="text", x=1945, y=1, label="Hybrids", size = anno_size, color = "darkgreen", hjust = 0) +
  geom_segment(aes(x = 1960, y = 1, xend = 2035, yend = 1), arrow = arrow(length = unit(0.5, "cm"))) +
  
  annotate(geom="text", x=1957, y=2.5, label="Quantitative Genetics", size = anno_size, color = "darkgreen", hjust = 0) +
  geom_segment(aes(x = 1997, y = 2.5, xend = 2035, yend = 2.5), arrow = arrow(length = unit(0.5, "cm"))) +
  
  annotate(geom="text", x=1975, y=3.7, label="Synthetic Fertilizer", size = anno_size, color = "blue", hjust = 0) +
  geom_segment(aes(x = 2010, y = 3.7, xend = 2035, yend = 3.7), arrow = arrow(length = unit(0.5, "cm"))) +
  
  annotate(geom="text", x=1995, y=5.8, label="Transgenics (GM)", size = anno_size, color = "purple", hjust = 0) +
  geom_segment(aes(x = 2030, y = 5.8, xend = 2035, yend = 5.8), arrow = arrow(length = unit(0.5, "cm"))) +
  
  annotate(geom="text", x=2003, y=6.9, label="Genomic\nSelection", size = anno_size, color = "darkgreen", hjust = 0) +
  geom_segment(aes(x = 2022, y = 6.9, xend = 2035, yend = 6.9), arrow = arrow(length = unit(0.5, "cm"))) +
  
  annotate(geom="text", x=2014, y=8.8, label="Precision\nAgriculture", size = anno_size, color = "blue", hjust = 0) +
  geom_segment(aes(x = 2030, y = 8.8, xend = 2035, yend = 8.8), arrow = arrow(length = unit(0.5, "cm"))) +
  
  annotate(geom="text", x=2020, y=11.7, label="CRISPR?", size = anno_size, color = "black", hjust = 0) +
  
  annotate(geom="text", x=1943, y=6.2, label="Current GM\nImpact 6.4%", size = anno_size, color = "purple", hjust = 0) +
  geom_segment(aes(x = 1968, y = 5.7, xend = 1968, yend = 6.8), arrow = arrow(length = unit(0.5, "cm")), color = "purple")

# Save to file
ggsave("../images/annotated_yield_figure_annotated.png", v1, width = 8, height = 6, units = "in")


# Perfect if using bushels per acre
# ggplot(yield, aes(x = Year, y = taha)) +
#   geom_point() +
#   ylab("Average US Corn Yield (bu/ac)") +
#   theme_classic() +
#   theme(legend.position = c(0.2, 0.85),
#         legend.title=element_blank(),
#         text = element_text(size=20)) +
#   scale_color_manual(values=cbPalette) +
#   xlim(1900,2035) +
#   annotate(geom="text", x=1945, y=23, label="Hybrids", size = anno_size, color = "darkgreen", hjust = 0) +
#   geom_segment(aes(x = 1960, y = 23, xend = 2035, yend = 23), arrow = arrow(length = unit(0.5, "cm"))) +
#   
#   annotate(geom="text", x=1957, y=37, label="Quantitative Genetics", size = anno_size, color = "darkgreen", hjust = 0) +
#   geom_segment(aes(x = 1997, y = 37, xend = 2035, yend = 37), arrow = arrow(length = unit(0.5, "cm"))) +
#   
#   annotate(geom="text", x=1975, y=65, label="Synthetic Fertilizer", size = anno_size, color = "blue", hjust = 0) +
#   geom_segment(aes(x = 2010, y = 65, xend = 2035, yend = 65), arrow = arrow(length = unit(0.5, "cm"))) +
#   
#   annotate(geom="text", x=1995, y=100, label="Transgenics (GM)", size = anno_size, color = "purple", hjust = 0) +
#   geom_segment(aes(x = 2030, y = 100, xend = 2035, yend = 100), arrow = arrow(length = unit(0.5, "cm"))) +
#   
#   annotate(geom="text", x=2004, y=115, label="Genomic\nSelection", size = anno_size, color = "darkgreen", hjust = 0) +
#   geom_segment(aes(x = 2022, y = 115, xend = 2035, yend = 115), arrow = arrow(length = unit(0.5, "cm"))) +
#   
#   annotate(geom="text", x=2012, y=140, label="Precision\nAgriculture", size = anno_size, color = "blue", hjust = 0) +
#   geom_segment(aes(x = 2030, y = 140, xend = 2035, yend = 140), arrow = arrow(length = unit(0.5, "cm"))) +
#   
#   annotate(geom="text", x=2019, y=190, label="CRISPR?", size = anno_size, color = "black", hjust = 0) +
#   
#   annotate(geom="text", x=1943, y=100, label="Current GM\nImpact 6.4%", size = anno_size, color = "purple", hjust = 0) +
#   geom_segment(aes(x = 1968, y = 91, xend = 1968, yend = 110), arrow = arrow(length = unit(0.5, "cm")), color = "purple")
# 
# 












# Plot classic figure colored by era -------------------------------------------

# Calculate out betas for each era
trends <- yield %>% 
  tidyr::nest(data = -Era) %>% 
  dplyr::mutate(model = map(data, ~lm(Value ~ Year, data = .)), tidied = map(model, tidy)) %>% 
  tidyr::unnest(tidied) %>% 
  dplyr::filter(term == "Year") 
trends$estimate <- round(trends$estimate, 2)

# Plot by year and practice
ggplot(yield, aes(x = Year, y = Value, color = Era)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  ylab("Average Corn Yield (bu/ac)") +
  theme_classic() +
  theme(legend.position = "bottom") +
  annotate(geom="text", x=1900, y=40, label=paste0("b=", trends[1,5])) +
  annotate(geom="text", x=1945, y=50, label=paste0("b=", trends[2,5])) +
  annotate(geom="text", x=1970, y=110, label=paste0("b=", trends[3,5])) +
  annotate(geom="text", x=2005, y=175, label=paste0("b=", trends[4,5])) +
  scale_color_manual(values=cbPalette)







