# ------------------------------------------------------------------------------
# Author.... Merritt Khaipho-Burch
# Contact... mbb262@cornell.edu
# Date...... 2024-01-03
# Updated... 2024-01-04
#
# Description:
# Plot hypothesized yield effects
# ------------------------------------------------------------------------------

# Packages
library(dplyr)

# set working directory
setwd("/Users/mbb262-admin/Library/CloudStorage/Box-Box/Cornell_PhD/thesis/bad_yield_papers/")

# Read in table
reviewTable <- read.csv("data/formatted_comparing_bad_yield_studies.csv")

# Remove rows explaining genes and paper parameters
reviewTable <- reviewTable[c(2,4,5,7:9,20), -c(2:3, ncol(reviewTable))]

# Transpose, format
reviewTable <- data.frame(t(reviewTable))
colnames(reviewTable) <- reviewTable[1,]
reviewTable <- reviewTable[-1,]
reviewTable$study <- rownames(reviewTable)
rownames(reviewTable) <- NULL
reviewTable$study <- gsub("\\.et.al...", " ", reviewTable$study)
reviewTable$study <- gsub("\\.1", "", reviewTable$study)
reviewTable$study <- gsub("De.Souza", "DeSouza", reviewTable$study)
reviewTable$cited <- as.numeric(as.character(reviewTable$cited))
reviewTable$altmetrix <- as.numeric(as.character(reviewTable$altmetrix))
reviewTable$average_yield <- as.numeric(as.character(reviewTable$average_yield))
reviewTable$minimum_yield <- as.numeric(as.character(reviewTable$minimum_yield))
reviewTable$maximum_yield <- as.numeric(as.character(reviewTable$maximum_yield))

# Format single or plot yield data
reviewTable$yieldType <- gsub("no.*", "Single-Plant", reviewTable$`Measured plot yield`)
reviewTable$yieldType <- gsub("yes.*", "Plot", reviewTable$yieldType)

# Add in crop to paper labels
reviewTable$study <- paste0(reviewTable$study, " (", reviewTable$crop, ")")

# Rearrange factor levels by last name and by crop
reviewTable$study <- factor(reviewTable$study, levels = c(
  "Chen 2022 (maize)", 
  "Ribeiro 2020 (maize)",
  "Yu 2021 (potato)",
  "Ambavaram 2014 (rice)", 
  "Ashikari 2005 (rice)",
  "Chen 2022 (rice)",
  "Li 2018 (rice)",
  "Li 2023 (rice)", 
  "Miura 2010 (rice)",
  "Song 2007 (rice)",
  "Wang 2008 (rice)",
  "Wei 2022 (rice)",
  "Wu 2023 (rice)",
  "Xue 2008 (rice)", 
  "Yoon 2020 (rice)",
  "Yu 2021 (rice)",
  "DeSouza 2022 (soybean)", 
  "Kromdijk 2016 (tobacco)", 
  "South 2019 (tobacco)",
  "Li 2018 (wheat)",  
  "Song 2023 (wheat)", 
   "Wang 2022 (wheat)", 
   "Wei 2022 (wheat)",
  "Zhang 2022 (wheat)"))

# List mean percent grain yield increase
mean(reviewTable$average_yield)
mean(c(reviewTable$minimum_yield, reviewTable$maximum_yield))
mean(reviewTable$minimum_yield)
mean(reviewTable$maximum_yield)

# Plot %percent grain yield increase by name of QTLs
axis_text_size <- 13
maxPercent <- ggplot(reviewTable, aes(x = study, y = maximum_yield, shape = yieldType)) +
  geom_point(size = 3)+
  theme_classic() +
  geom_hline(yintercept = mean(reviewTable$maximum_yield)) +
  xlab("Study") +
  ylab("Maximum Percent Grain Yield Increase") +
  guides(shape=guide_legend(title="Yield Type"))+
  theme(axis.text=element_text(size=axis_text_size), 
        axis.title = element_text(size=axis_text_size),
        axis.text.x = element_text(angle = 65, vjust = 1, hjust=1),
        legend.position = "bottom")
ggsave("images/maximum_percent_increase.png",
       maxPercent,
       height = 6.5,
       width = 6,
       units = "in")

# Plot minimum percnet increase
minPercent <- ggplot(reviewTable, aes(x = study, y = minimum_yield, shape = yieldType)) +
  geom_point(size = 3)+
  theme_classic() +
  geom_hline(yintercept = mean(reviewTable$minimum_yield)) +
  xlab("Study") +
  ylab("Minimum Percent Grain Yield Increase") +
  guides(shape=guide_legend(title="Yield Type"))+
  theme(axis.text=element_text(size=axis_text_size), 
        axis.title = element_text(size=axis_text_size),
        axis.text.x = element_text(angle = 65, vjust = 1, hjust=1),
        legend.position = "bottom")
ggsave("images/minimum_percent_increase.png",
       minPercent,
       height = 6.5,
       width = 6,
       units = "in")


# Number of citations versus percent increase ----------------------------------

# Read in table
paperData <- read.csv("data/formatted_comparing_bad_yield_studies.csv")

# Subset the data
paperData <- paperData[c(2:9), -c(2:3, ncol(paperData))]

# Transpose, format
paperData <- data.frame(t(paperData))
colnames(paperData) <- paperData[1,]
paperData <- paperData[-1,]
paperData$study <- rownames(paperData)
rownames(paperData) <- NULL
paperData$study <- gsub("\\.et.al...", " ", paperData$study)

# Turn data numeric
paperData$year <- as.numeric(as.character(paperData$year))
paperData$cited <- as.numeric(as.character(paperData$cited))
paperData$altmetrix <- as.numeric(as.character(paperData$altmetrix))
paperData$minimum_yield <- as.numeric(as.character(paperData$minimum_yield))
paperData$maximum_yield <- as.numeric(as.character(paperData$maximum_yield))
paperData$average_yield <- as.numeric(as.character(paperData$average_yield))

# Get stats
min(paperData$minimum_yield)
max(paperData$maximum_yield)

# Run model
summary(lm(cited ~ average_yield, paperData))
cor(paperData$cited, paperData$average_yield)^2

summary(lm(cited ~ minimum_yield, paperData))
cor(paperData$cited, paperData$minimum_yield)^2

summary(lm(cited ~ maximum_yield, paperData))
cor(paperData$cited, paperData$maximum_yield)^2

# Plot altmetric by citation count
citations_yield <- ggplot(paperData, aes(x = cited, y = average_yield, shape = crop))+
  geom_point(size = 3)+
  theme_bw() +
  geom_hline(yintercept = median(reviewTable$average_yield)) +
  ylab("Average Percent Yield Increase") +
  xlab("Number of Citations") +
  theme(axis.text=element_text(size=axis_text_size), 
        axis.title = element_text(size=axis_text_size))
citations_yield

ggsave("images/average_percent_increase.png",
       citations_yield,
       height = 5,
       width = 6.6,
       units = "in")



