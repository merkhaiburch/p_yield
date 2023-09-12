# ------------------------------------------------------------------------------
# Author.... Merritt Khaipho-Burch
# Contact... mbb262@cornell.edu
# Date...... 2023-08-16
# Updated... 2023-08-16
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
setwd("~/../mbb262-admin/Library/CloudStorage/Box-Box/git_projects/p_yield/data")

# Load libraries
library(ggplot2)
library(dplyr)

# # First plot -------------------------------------------------------------------
# 
# trans <- read.csv("transgenic or transgene both and yield PubMed_Timeline_Results_by_Year.csv")
# gp <- read.csv("GS OR GP both AND YIELD PubMed_Timeline_Results_by_Year.csv")
# 
# trans$Type <- rep("Transgenic or Transgene AND Yield", nrow(trans))
# gp$Type <- rep("Genomic Selection or Prediction AND Yield", nrow(gp))
# 
# together <- rbind(trans, gp)
# 
# v1 <- ggplot(together, aes(x = Year, y = Count, fill = Type, color = Type)) +
#   geom_area() +
#   theme(legend.position = "bottom") +
#   ylab("Number of PubMed Publications")
# ggsave("../images/pubmed_transgenic_gsgp_yield_year.png", v1)
# 
# 
# # Second plot ------------------------------------------------------------------
# 
# # Try adding terms to narrow it down to plants
# trans_plants <- read.csv("transgenic or transgene both and yield with plant terms and johnathan termsPubMed_Timeline_Results_by_Year.csv")
# gp_plants <- read.csv("GS OR GP both AND YIELD with plant terms PubMed_Timeline_Results_by_Year.csv")
# 
# trans_plants$Type <- rep("Transgenic or Transgene AND Yield AND Plant", nrow(trans_plants))
# gp_plants$Type <- rep("Genomic Selection or Prediction AND Yield AND Plant", nrow(gp_plants))
# 
# together_plants <- rbind(trans_plants, gp_plants)
# 
# v2 <- ggplot(together_plants, aes(x = Year, y = Count, fill = Type, color = Type)) +
#   geom_area() +
#   theme(legend.position = "bottom") +
#   ylab("Number of PubMed Publications")
# ggsave("../images/pubmed_transgenic_gsgp_yield_year_plantterms_moreterms.png", v2)
# 
# 
# # Third plot -------------------------------------------------------------------
# 
# trans <- read.csv("transgenic or transgene both and yield with plant terms Aug 21 PubMed_Timeline_Results_by_Year.csv")
# gp <- read.csv("GS OR GP both AND YIELD Aug 21 with plant terms PubMed_Timeline_Results_by_Year.csv")
# 
# trans$Type <- rep("Transgenic or Transgene AND Yield", nrow(trans))
# gp$Type <- rep("Genomic Selection or Prediction AND Yield", nrow(gp))
# 
# together <- rbind(trans, gp) %>% filter(Year <= 2022)
# 
# write.csv(together, "pubmed_article_count_yield.csv", quote = F, row.names = F)
# 
# v3 <- ggplot(together, aes(x = Year, y = Count, fill = Type, color = Type)) +
#   geom_area() +
#   theme(legend.position = "bottom") +
#   ylab("Number of PubMed Publications")
# v3
# ggsave("../images/pubmed_transgenic_gsgp_yield_year_no2023_quotes.png", v3)
# 
# 
# # Fourth plot ------------------------------------------------------------------
# 
# # Load in separate transgenic studies, combine, add identifier
# trans1 <- read.delim("Aug 22 WOS/transgenic_papers_1.txt") %>% select(PT, AU, TI, SO, PY)
# trans2 <- read.delim("Aug 22 WOS/transgenic_papers_2.txt") %>% select(PT, AU, TI, SO, PY)
# trans3 <- read.delim("Aug 22 WOS/transgenic_papers_3.txt") %>% select(PT, AU, TI, SO, PY)
# trans4 <- read.delim("Aug 22 WOS/transgenic_papers_4.txt") %>% select(PT, AU, TI, SO, PY)
# trans_all <- rbind(trans1, trans2, trans3, trans4)
# trans_all$id <- rep("Transgenic", nrow(trans_all))
# 
# # Load in separate genomic prediction plots, combine
# gs1 <- read.delim("Aug 22 WOS/GS_papers_1.txt") %>% select(PT, AU, TI, SO, PY)
# gs2 <- read.delim("Aug 22 WOS/GS_papers_2.txt") %>% select(PT, AU, TI, SO, PY)
# gs_all <- rbind(gs1, gs2)
# gs_all$id <- rep("Genomic Selection", nrow(gs_all))
# 
# # Combine all
# together <- rbind(trans_all, gs_all)
# 
# # Change column names
# colnames(together) <- c("Publication_Type", "Authors", "Article_Name", "Journal", "Year", "Type")
# 
# # Save data
# write.csv(together, "Aug 22 WOS/aggregated_web_of_science.csv", row.names = F, quote = F)
# 
# # Aggregate data by year
# together_agg <- together %>% 
#   group_by(Year, Type) %>% 
#   summarise(Count = n()) %>% 
#   filter(Year != 2023)
# 
# # Get stats
# temp <- together %>% 
#   group_by(Year, Type) %>% 
#   summarise(Count = n()) 
# 
# sub_trans <- temp %>% filter(Type == "Transgenic") %>% select(Count)
# sub_gs <- temp %>% filter(Type == "Genomic Selection") %>% select(Count)
# 
# mean(sub_trans$Count)
# mean(sub_gs$Count)
# 
# 
# # Plot
# v4 <- ggplot(together_agg, aes(x = Year, y = Count, fill = Type, color = Type)) +
#   geom_area() +
#   theme_classic() +
#   theme(legend.position = "bottom") +
#   ylab("Number of Web of Science Publications")
# ggsave("../images/webofscience_transgenic_gsgp_yield_year.png", v4)
# 

# Fifth plot ------------------------------------------------------------------

# Load in separate transgenic studies, combine, add identifier
trans1 <- read.delim("Aug 23 WOS PS Only/trans_1.txt") %>% select(PT, AU, TI, SO, PY)
trans2 <- read.delim("Aug 23 WOS PS Only/trans_2.txt", sep = "\t") %>% select(PT, AU, TI, SO, PY)
trans3 <- read.delim("Aug 23 WOS PS Only/trans_3.txt") %>% select(PT, AU, TI, SO, PY)
trans4 <- read.delim("Aug 23 WOS PS Only/trans_4.txt") %>% select(PT, AU, TI, SO, PY)
trans5 <- read.delim("Aug 23 WOS PS Only/trans_5.txt") %>% select(PT, AU, TI, SO, PY)
trans6 <- read.delim("Aug 23 WOS PS Only/trans_6.txt") %>% select(PT, AU, TI, SO, PY)
trans_all <- rbind(trans1, trans2, trans3, trans4, trans5, trans6)
trans_all$id <- rep("Transgenic", nrow(trans_all))

# Load in separate genomic prediction plots, combine
gs1 <- read.delim("Aug 23 WOS PS Only/gs_1.txt") %>% select(PT, AU, TI, SO, PY)
gs_all <- gs1
gs_all$id <- rep("Genomic Selection", nrow(gs_all))

# Combine all
together <- rbind(trans_all, gs_all)

# Change column names
colnames(together) <- c("Publication_Type", "Authors", "Article_Name", "Journal", "Year", "Type")

# Save data
write.csv(together, "Aug 23 WOS PS Only/aggregated_web_of_science.csv", row.names = F, quote = T)

# Aggregate data by year
together_agg <- together %>% 
  group_by(Year, Type) %>% 
  summarise(Count = n()) %>% 
  filter(Year != 2023)

# Get stats
temp <- together %>% 
  group_by(Year, Type) %>% 
  summarise(Count = n()) 

sub_trans <- temp %>% filter(Type == "Transgenic") %>% select(Count)
sub_gs <- temp %>% filter(Type == "Genomic Selection") %>% select(Count)

mean(sub_trans$Count)
mean(sub_gs$Count)

# The palette with grey:
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
cbPalette_swapped <- c("#E69F00", "#999999")

# Plot
v4 <- ggplot(together_agg, aes(x = Year, y = Count, fill = Type)) +
  geom_area() +
  theme_classic() +
  theme(legend.position = "bottom") +
  ylab("Number of Web of Science Publications") +
  scale_fill_manual(values=cbPalette_swapped)
v4
ggsave("../images/thesis_webofscience_transgenic_gsgp_yield_year_plant_science_only.png",
       v4, width = 6, height = 4, units = "in")


# Sixth plot with pubmed data again ----------------------------------------------------

# Load transgenic studies (already aggregated by year), add identifier
trans <- read.csv("Aug 23 PubMed/transgenic_Results_by_Year.csv")
trans$id <- rep("Transgenic", nrow(trans))

# Load in genomic prediction studies (already aggregated by year), add identifier
gs <- read.csv("Aug 23 PubMed/gs_Results_by_Year.csv")
gs$id <- rep("Genomic Selection", nrow(gs))

# Combine all, remove 2023
together <- rbind(trans, gs) %>% 
  filter(Year <= 2022 & Year >= 1990)

# Change column names
colnames(together) <- c("Year", "Count", "Type")

# Write data to file
# write.csv(together, "Aug 23 PubMed/pubmed_article_count_yield.csv", quote = F, row.names = F)

# The palette with grey:
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
cbPalette_swapped <- c("#E69F00", "#999999")

# Make plot
v5 <- ggplot(together, aes(x = Year, y = Count, fill = Type)) +
  geom_area() +
  theme_classic() +
  theme(legend.position = "bottom") +
  ylab("Number of PubMed Publications") +
  scale_fill_manual(values=cbPalette_swapped)
v5
ggsave("../images/thesis_pubmed_transgenic_gsgp_yield_year_no2023_quotes_additional_terms.png", v5,
       width = 6, height = 4, units = "in")


