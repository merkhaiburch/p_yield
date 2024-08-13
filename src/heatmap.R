# ------------------------------------------------------------------------------
# Author.... Merritt Khaipho-Burch
# Contact... mbb262@cornell.edu
# Date...... 2024-01-03
# Updated... 2024-01-03
#
# Description:
# Heatmap of yield comparison tables
# ------------------------------------------------------------------------------

# set working directory
setwd("/Users/merkhaiburch/Documents/Box Backup Feb 21 2024/git_projects/p_yield/")

#pakcages
library(dplyr)
library(reshape2)
library(ggplot2)

# Load in data
allCrops <- read.csv(file = "data/formatted_comparing_bad_yield_studies_numeric.csv")

# Format column names
colnames(allCrops) <- gsub("\\.et.al...", "_", colnames(allCrops))


# Rice heatmap -----------------------------------------------------------------

# Subset out rice names (should be 13)
riceNames <- c("Issue", 
               "Yoon_2020",
               "Wei_2022",
               "Ambavaram_2014",
               "Chen_2022.1",
               "Yu_2021",
               "Wu_2023",
               "Ashikari_2005",
               "Song_2007",
               "Li_2023",
               "Miura_2010",
               "Li_2018",
               "Wang_2008",
               "Xue_2008")
rice <- allCrops[, riceNames]

# Reshape data
melt_rice <- reshape2::melt(rice, id.vars = "Issue")

# See ideal order for clustering
rownames(rice) <- rice[,1] # add rownames
rice <- rice[,-1] # delete old column that is now rownames
heatmap(as.matrix(rice), Rowv = NA)

# Make a cleaner set of names
melt_rice$variable <- gsub("_", " ", melt_rice$variable)
melt_rice$variable <- gsub("\\.1", "", melt_rice$variable)
melt_rice$variable <- gsub("\\.", " ", melt_rice$variable)

# Set order of factors according to clustering
melt_rice$variable <- factor(melt_rice$variable, levels=c(
  "Wei 2022",
  "Yoon 2020",
  "Li 2023",
  "Chen 2022",
  "Wu 2023",
  "Miura 2010",
  "Ambavaram 2014",
  "Yu 2021",
  "Xue 2008",
  "Li 2018",
  "Song 2007",
  "Ashikari 2005",
  "Wang 2008"
  ))

melt_rice$Issue <- factor(melt_rice$Issue, levels = c(
  "Percent increase language", 
  "Tested elite germplasm", 
  "Backcrossed edits", 
  "Tested wild type", 
  "Tested empty vector", 
  "Tested in field", 
  "Individual replication (>=20)", 
  "Plot replication (p >= 3)", 
  "Enough site year combinations (>=3)", 
  "Measured plot yield", 
  "Normal sowing practices", 
  "Randomized plot designs", 
  "Corrected for field effects", 
  "Standard planting density", 
  "Avoided edge effects", 
  "Mechanically harvested", 
  "Measured relevant yield type", 
  "Controlled for moisture"))


# everything but rice ----------------------------------------------------------

# Subset out the necessary columns (should be 11)
notRiceNames <- c("Issue",
                  "South_2019",
                  "Kromdijk_2016",
                  "De.Souza_2022",
                  "Wei_2022.1",
                  "Song_2023",
                  "Chen_2022",
                  "Zhang_2022",
                  "Yu_2021.1",
                  "Ribeiro_2020", 
                  "Li_2018.1",
                  "Wang_2022")

notRice <- allCrops[,notRiceNames]

# Reshape data
melt_notrice <- reshape2::melt(notRice, id.vars = "Issue")

# Delete extra rows, format to numeric
rownames(notRice) <- notRice[,1] # add rownames
notRice <- notRice[,-1] # delete old column that is now rownames
heatmap(as.matrix(notRice))

# Make a cleaner set of names
melt_notrice$variable <- gsub("_", " ", melt_notrice$variable)
melt_notrice$variable <- gsub("De.Souza.2022", "DeSouza 2022", melt_notrice$variable)
melt_notrice$variable <- gsub("\\.1", "", melt_notrice$variable)
melt_notrice$variable <- gsub("\\.", " ", melt_notrice$variable)

# Add crop
melt_notrice$variable <- gsub("Ribeiro 2020", "Ribeiro 2020 (maize)", melt_notrice$variable)
melt_notrice$variable <- gsub("South 2019", "South 2019 (tobacco)", melt_notrice$variable)
melt_notrice$variable <- gsub("Kromdijk 2016", "Kromdijk 2016 (tobacco)", melt_notrice$variable)
melt_notrice$variable <- gsub("Song 2023", "Song 2023 (wheat)", melt_notrice$variable)
melt_notrice$variable <- gsub("Li 2018", "Li 2018 (wheat)", melt_notrice$variable)
melt_notrice$variable <- gsub("Yu 2021", "Yu 2021 (potato)", melt_notrice$variable)
melt_notrice$variable <- gsub("Chen 2022", "Chen 2022 (maize)", melt_notrice$variable)
melt_notrice$variable <- gsub("Zhang 2022", "Zhang 2022 (wheat)", melt_notrice$variable)
melt_notrice$variable <- gsub("Wang 2022", "Wang 2022 (wheat)", melt_notrice$variable)
melt_notrice$variable <- gsub("Wei 2022", "Wei 2022 (wheat)", melt_notrice$variable)
melt_notrice$variable <- gsub("DeSouza 2022", "DeSouza 2022 (soybean)", melt_notrice$variable)

# Set order of factors according to clustering
melt_notrice$variable <- factor(melt_notrice$variable, levels=c(
  "Song 2023 (wheat)",
  "Li 2018 (wheat)",
  "Yu 2021 (potato)",
  "Zhang 2022 (wheat)",
  "Wei 2022 (wheat)",
  "Wang 2022 (wheat)",
  "Chen 2022 (maize)",
  "Ribeiro 2020 (maize)",
  "DeSouza 2022 (soybean)",
  "South 2019 (tobacco)",
  "Kromdijk 2016 (tobacco)"
  ))

melt_notrice$Issue <- factor(melt_notrice$Issue, levels = c(
  "Percent increase language", 
  "Tested elite germplasm", 
  "Backcrossed edits", 
  "Tested wild type", 
  "Tested empty vector", 
  "Tested in field", 
  "Individual replication (>=20)", 
  "Plot replication (p >= 3)", 
  "Enough site year combinations (>=3)", 
  "Measured plot yield", 
  "Normal sowing practices", 
  "Randomized plot designs", 
  "Corrected for field effects", 
  "Standard planting density", 
  "Avoided edge effects", 
  "Mechanically harvested", 
  "Measured relevant yield type", 
  "Controlled for moisture"))


# Plot both things -------------------------------------------------------------

# Set color pallete
# Order: 0,1,2,3,4
# Yes = 4
# No = 3
# No information, unknown, unclear = 2
# Not applicable = 1
# ask = 0

# Order: c("Asking", "Not Applicable", "No Info/Unclear", "No/Not-Met", "Yes/Met")

# colors <- c("#FDAE61", "#FFFFBF", "#ABDDA4") # second best
# colors <- c("#000000", "#DCDCDC", "#ABDDA4") #best so far 3 color options
# colors <- c("#D7191C", "#FDAE61", "#FFFFBF", "#ABDDA4", "#2B83BA") # second best option
colors <- c("#D7191C", "#2B83BA", "#DCDCDC", "#000000", "#ABDDA4")
colorsNotRice <- c("#2B83BA", "#DCDCDC", "#000000", "#ABDDA4")

# Extract specific r color brewer colors
library(RColorBrewer)
brewer.pal(n=5,"Spectral")

# Plotting variables
axis_text_size <- 13

# Rice ----------------------------------
ricePlot <-ggplot(melt_rice, aes(x = variable, y = Issue, fill = factor(value))) + 
  geom_tile() +
  theme_classic()+
  scale_fill_manual(labels = c("Asking", "Not Applicable", "No Info/Unclear", "No/Not-Met", "Yes/Met"), 
                    values = colors) +
  labs(fill = "Criteria Met?") +
  xlab("Publication") +
  ylab("Criteria") +
  theme(axis.text=element_text(size=axis_text_size), 
        axis.title = element_text(size=axis_text_size),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        legend.position="bottom")

ggsave("images/rice_heatmap_v2.png",
       ricePlot,
       height = 10,
       width = 10,
       units = "in")


# Not Rice -------------------------------

notRicePlot <- ggplot(melt_notrice, aes(x = variable, y = Issue, fill = factor(value))) + 
  geom_tile() +
  theme_classic()+
  scale_fill_manual(labels = c("Not Applicable", "No Info/Unclear", "No/Not-Met", "Yes/Met"), 
                    values = colorsNotRice) +
  labs(fill = "Criteria Met?") +
  xlab("Publication") +
  ylab("Criteria") +
  theme(axis.text=element_text(size=axis_text_size), 
        axis.title = element_text(size=axis_text_size),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        legend.position="bottom")

ggsave("images/notrice_heatmap_v2.png",
       notRicePlot,
       height = 10,
       width = 10,
       units = "in")



