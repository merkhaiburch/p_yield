# ------------------------------------------------------------------------------
# Author.... Merritt Khaipho-Burch
# Contact... mbb262@cornell.edu
# Date...... 2022-11-11
# Updated... 2022-11-11
#
# Description:
# Using Web of Science search commands to count the number of genomic selection
# or genomic prediction papers occuring within high-impact journals
#
# Plot counts of the number of times an article appears in a specific journal
# ------------------------------------------------------------------------------

# Data downloaded from the web of science using this command
# (ALL=("genomic prediction")) OR ALL=("genomic selection")
#https://www.webofscience.com/wos/woscc/summary/96cb19e7-9c19-4d3e-a091-bf02ed87f89b-5d550e90/relevance/1

# To just see the results for GS and GP in Nature or science
# (((SO=(Nature)) OR SO=(Science)) AND ALL=("genomic prediction")) OR ALL=("genomic selection")
# https://www.webofscience.com/wos/woscc/summary/0bdc7dbd-0d41-41e2-b9b3-e5a4c7117933-5d546bc1/relevance/1

# Libraries
library(dplyr)
library(ggplot2)
library(data.table)

# Load in all csv files
files <- list.files("~/git_projects/p_yield/data/wos/", full.names = TRUE, pattern = "*.csv")

# Load in all files, rbind together
wos <- lapply(files, data.table::fread)
wos <- data.table::rbindlist(wos)

# Only keep the journal column
journal <- wos %>% 
  group_by(`Source Title`) %>% 
  summarise(n = n())

# Plot barchart of the top 20 journals then nature and science at the end
sub <- journal %>% top_n(20)
sub <- rbind(sub, journal[477,]) # add on nature
sub <- rbind(sub, journal[599,]) # add on science
colnames(sub) <- c("Journal", "n")

# Type - make nature and science a different color
sub$Type <- c(rep("A", 20), rep("B", 2))

# Make barchart
a <- ggplot(sub, aes(x = reorder(Journal, n), y = n, fill = Type)) +
  geom_bar(stat="identity") +
  theme_bw() +
  theme(legend.position = "none",
        text = element_text(size=20)) +
  scale_fill_manual(values=c("#999999", "#D55E00")) +
  xlab("Journal") +
  ylab("# papers mentioning GS or GP") +
  coord_flip()

ggsave(plot = a,width = 11, height = 8, units = "in",
       filename = "~/git_projects/p_yield/images/top_20_journal_count.png",)


