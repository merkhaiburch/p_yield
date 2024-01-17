# ------------------------------------------------------------------------------
# Author.... Merritt Khaipho-Burch
# Contact... mbb262@cornell.edu
# Date...... 2024-01-03
# Updated... 2024-01-03
#
# Description:
# Turn table data numeric and export for plotting
# ------------------------------------------------------------------------------

# Packages
library(dplyr)
library(reshape2)

# set working directory
setwd("/Users/mbb262-admin/Library/CloudStorage/Box-Box/Cornell_PhD/thesis/bad_yield_papers/")

# Read in table
reviewTable <- read.csv("data/formatted_comparing_bad_yield_studies.csv")

# Remove rows explaining genes and paper parameters
reviewTable <- reviewTable[-c(1:9, 18, 29:nrow(reviewTable)), -c(2:3, ncol(reviewTable))]


# Format Data ------------------------------------------------------------------

# Do a series of gsub commands to turn yes and no answers to numeric values where:
# Yes = 4
# No = 3
# No information, unknown, unclear = 2
# Not applicable = 1
# ask = 0
reviewTable <- apply(reviewTable, 2, function(y) gsub("yes.*", "4", y))
reviewTable <- apply(reviewTable, 2, function(y) gsub("no information.*", "2", y))
reviewTable <- apply(reviewTable, 2, function(y) gsub("not applicable.*", "1", y))
reviewTable <- apply(reviewTable, 2, function(y) gsub("^no.*", "3", y))
reviewTable <- apply(reviewTable, 2, function(y) gsub("ask.*", "0", y))
reviewTable <- apply(reviewTable, 2, function(y) gsub("unclear.*", "2", y))

# Turn into a dataframe
reviewTable <- data.frame(reviewTable)

# Turn into numeric values
reviewTable[,2:25] <- sapply(reviewTable[,2:25],as.numeric)


# Export table -----------------------------------------------------------------

# Unmelted version
write.csv(reviewTable,
          file = "data/formatted_comparing_bad_yield_studies_numeric.csv",
          row.names = FALSE,
          quote = FALSE)





