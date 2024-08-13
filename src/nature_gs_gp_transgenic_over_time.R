# ------------------------------------------------------------------------------
# Author.... Merritt Khaipho-Burch
# Contact... mbb262@cornell.edu
# Date...... 2023-08-16
# Updated... 2024-08-12
#
# Description:
# Plot rise in transgenic yield papers across fields pulled from pubmed
# First plot:
# ((genomic prediction) OR (genomic selection)) AND (yield)
# ((transgeneic) OR (transgene)) AND (yield)
#
# Second plot
# ((genomic prediction) OR (genomic selection)) AND (yield) AND ((crop) OR (plant) OR (grain) OR (seed))
# ((transgenic) OR (transgene)) AND (yield) AND ((crop) OR (plant) OR (grain) OR (seed))
#
# Third plot
# (("genomic prediction") OR ("genomic selection") OR ("genome-wide prediction") OR ("whole-genome regression") OR ("genomic-enabled prediction") OR ("genome-enabled prediction") OR ("whole-genome prediction")) AND (yield) AND ((crop) OR (plant) OR (grain) OR (seed))
# ((transgenic) OR (transgene) OR (CRISPR) OR (transformation) OR ("genetic construct") OR (transformed) OR (agrobacterium)) AND (yield) AND ((crop) OR (plant) OR (grain) OR (seed))
# ------------------------------------------------------------------------------

# Set directory
setwd("/Users/merkhaiburch/Documents/Box Backup Feb 21 2024/git_projects/p_yield/")

# Load libraries
library(ggplot2)
library(dplyr)

# The palette with grey:
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
cbPalette_swapped <- c("#E69F00", "#999999")


# Format web of science data ---------------------------------------------------

# # Load in separate transgenic studies, combine, add identifier
# trans1 <- read.delim("data/Aug 23 WOS PS Only (USED IN PUBLICATION)/trans_1.txt") %>% select(PT, AU, TI, SO, PY)
# trans2 <- read.delim("data/Aug 23 WOS PS Only (USED IN PUBLICATION)/trans_2.txt", sep = "\t") %>% select(PT, AU, TI, SO, PY)
# trans3 <- read.delim("data/Aug 23 WOS PS Only (USED IN PUBLICATION)/trans_3.txt") %>% select(PT, AU, TI, SO, PY)
# trans4 <- read.delim("data/Aug 23 WOS PS Only (USED IN PUBLICATION)/trans_4.txt") %>% select(PT, AU, TI, SO, PY)
# trans5 <- read.delim("data/Aug 23 WOS PS Only (USED IN PUBLICATION)/trans_5.txt") %>% select(PT, AU, TI, SO, PY)
# trans6 <- read.delim("data/Aug 23 WOS PS Only (USED IN PUBLICATION)/trans_6.txt") %>% select(PT, AU, TI, SO, PY)
# trans_all <- rbind(trans1, trans2, trans3, trans4, trans5, trans6)
# trans_all$id <- rep("Transgenic", nrow(trans_all))
# 
# # Load in separate genomic prediction plots, combine
# gs1 <- read.delim("data/Aug 23 WOS PS Only (USED IN PUBLICATION)/gs_1.txt") %>% select(PT, AU, TI, SO, PY)
# gs_all <- gs1
# gs_all$id <- rep("Genomic Selection", nrow(gs_all))
# 
# # Combine all
# together <- rbind(trans_all, gs_all)
# 
# # Change column names
# colnames(together) <- c("Publication_Type", "Authors", "Article_Name", "Journal", "Year", "Type")
# 
# # Save data
# write.csv(together, "Aug 23 WOS PS Only/aggregated_web_of_science.csv", row.names = F, quote = T)

# Read data in
together <- read.csv("data/Aug 23 WOS PS Only (USED IN PUBLICATION)/aggregated_web_of_science.csv")


# Make WOS plot ----------------------------------------------------------------

# Aggregate data by year
together_agg <- together %>% 
  group_by(Year, Type) %>% 
  summarise(Count = n()) %>% 
  filter(Year != 2023)

# Get stats
temp <- together %>% 
  group_by(Year, Type) %>% 
  summarise(Count = n()) 

# Get statistics
sub_trans <- temp %>% filter(Type == "Transgenic") %>% select(Count)
sub_gs <- temp %>% filter(Type == "Genomic Selection") %>% select(Count)
mean(sub_trans$Count)
mean(sub_gs$Count)

# Plot
v4 <- ggplot(together_agg, aes(x = Year, y = Count, fill = Type)) +
  geom_area() +
  theme_classic() +
  theme(legend.position = "bottom") +
  ylab("Number of Web of Science Publications") +
  scale_fill_manual(values=cbPalette_swapped)
v4

# Save to file
ggsave("images/thesis_webofscience_transgenic_gsgp_yield_year_plant_science_only.png",
       v4, width = 6, height = 4, units = "in")


# Format pubmed data -----------------------------------------------------------

# # Load transgenic studies (already aggregated by year), add identifier
# trans <- read.csv("data/Aug 23 PubMed/transgenic_Results_by_Year.csv")
# trans$id <- rep("Transgenic", nrow(trans))
# 
# # Load in genomic prediction studies (already aggregated by year), add identifier
# gs <- read.csv("data/Aug 23 PubMed/gs_Results_by_Year.csv")
# gs$id <- rep("Genomic Selection", nrow(gs))
# 
# # Combine all, remove 2023
# together <- rbind(trans, gs) %>% 
#   filter(Year <= 2022 & Year >= 1990)
# 
# # Change column names
# colnames(together) <- c("Year", "Count", "Type")
# 
# # Write data to file
# write.csv(together, "Aug 23 PubMed/pubmed_article_count_yield.csv", quote = F, row.names = F)

# Read data in a skip the above steps
together <- read.csv("data/Aug 23 PubMed/pubmed_article_count_yield.csv")


# Make the pub med data plot ---------------------------------------------------

# Make plot
v5 <- ggplot(together, aes(x = Year, y = Count, fill = Type)) +
  geom_area() +
  theme_classic() +
  theme(legend.position = "bottom") +
  ylab("Number of PubMed Publications") +
  scale_fill_manual(values=cbPalette_swapped)
v5

# Save to file
ggsave("images/thesis_pubmed_transgenic_gsgp_yield_year_no2023_quotes_additional_terms.png", v5,
       width = 6, height = 4, units = "in")


