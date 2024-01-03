#heatmap of yield comparison tables

# set working directory
setwd("/Users/mbb262-admin/Library/CloudStorage/Box-Box/Cornell_PhD/thesis/bad_yield_papers/images/")

#pakcages
library(dplyr)
library(reshape2)
library(ggplot2)


# rice data ------------------------------------------

# Load in data and subset out non-rice studies
rice <- read.csv(file = "heatmap_data.csv")
rice <- rice[, c(1, 13:23)]

# Reshape data
melt_rice <- reshape2::melt(rice, id.vars = "Criteria")

# See ideal order for clustering
rownames(rice) <- rice[,1] # add rownames
rice <- rice[,-1] # delete old column that is now rownames
# heatmap(as.matrix(rice), Rowv = NA)

# Make a cleaner set of names
melt_rice$variable <- gsub("\\.1", "", melt_rice$variable)
melt_rice$variable <- gsub("\\.", " ", melt_rice$variable)

# Set order of factors according to clustering
melt_rice$variable <- factor(melt_rice$variable, levels=c(
  "Ambavaram 2014", 
  "Wei 2022",
  "Song 2007", 
  "Miura 2010",
  "Xue 2008", 
  "Ashikari 2005",
  "Yu 2021", 
  "Wang 2008",
  "Li 2023", 
  "Chen 2022",
  "Wu 2023"))

melt_rice$Criteria <- factor(melt_rice$Criteria, levels = c(
  "Percent increase language", 
  "Tested elite germplasm", 
  "Backcrossed edits", 
  "Tested wild type", 
  "Tested empty vector", 
  "Tested in field", 
  "Individual replication", 
  "Plot replication", 
  "Enough years tested", 
  "Enough site year combinations", 
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

# Load in data and subset out non-rice studies
notRice <- read.csv(file = "heatmap_data.csv")
notRice <- notRice[,c(1:12)]

# Reshape data
melt_notrice <- reshape2::melt(notRice, id.vars = "Criteria")

# Delete extra rows, format to numeric
rownames(notRice) <- notRice[,1] # add rownames
notRice <- notRice[,-1] # delete old column that is now rownames
# heatmap(as.matrix(notRice), Rowv = NA)

# Make a cleaner set of names
melt_notrice$variable <- gsub("De.Souza.2022", "DeSouza 2022", melt_notrice$variable)
melt_notrice$variable <- gsub("\\.", " ", melt_notrice$variable)

# Set order of factors according to clustering
melt_notrice$variable <- factor(melt_notrice$variable, levels=c(
  "Zhang 2022",
  "Wang 2022",
  "Wei 2022",
  "Song 2023",
  "Yu 2021",
  "Li 2018",
  "Chen 2022",
  "Ribeiro 2020",
  "DeSouza 2022",
  "Kromdijk 2016",
  "South 2019"))

melt_notrice$Criteria <- factor(melt_notrice$Criteria, levels = c(
  "Percent increase language", 
  "Tested elite germplasm", 
  "Backcrossed edits", 
  "Tested wild type", 
  "Tested empty vector", 
  "Tested in field", 
  "Individual replication", 
  "Plot replication", 
  "Enough years tested", 
  "Enough site year combinations", 
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
# Order: c("No Info/Unclear", "No/Not-Met", "Yes/Met")
# colors <- c("#FDAE61", "#FFFFBF", "#ABDDA4") # second best
# colors <- c("#000000", "#FFFFBF", "#ABDDA4") #best so far
colors <- c("#000000", "#DCDCDC", "#ABDDA4") #best so far

# Plotting variables
axis_text_size <- 15

# Rice
ricePlot <-ggplot(melt_rice, aes(x = variable, y = Criteria, fill = factor(value))) + 
  geom_tile() +
  theme_classic()+
  scale_fill_manual(labels = c("No Info/Unclear", "No/Not-Met", "Yes/Met"), 
                    values = colors) +
  labs(fill = "Criteria Met?") +
  xlab("Publication") +
  ylab("Criteria") +
  theme(axis.text=element_text(size=axis_text_size), 
        axis.title = element_text(size=axis_text_size),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        legend.position="bottom")

ggsave("../images/rice_heatmap.png",
       ricePlot,
       height = 10,
       width = 8,
       units = "in")

# Not Rice
notRicePlot <- ggplot(melt_notrice, aes(x = variable, y = Criteria, fill = factor(value))) + 
  geom_tile() +
  theme_classic()+
  scale_fill_manual(labels = c("No Info/Unclear", "No/Not-Met", "Yes/Met"), 
                    values = colors) +
  labs(fill = "Criteria Met?") +
  xlab("Publication") +
  ylab("Criteria") +
  theme(axis.text=element_text(size=axis_text_size), 
        axis.title = element_text(size=axis_text_size),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        legend.position="bottom")

ggsave("../images/notrice_heatmap.png",
       notRicePlot,
       height = 10,
       width = 8,
       units = "in")



