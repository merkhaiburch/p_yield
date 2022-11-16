# ------------------------------------------------------------------------------
# Author.... Merritt Khaipho-Burch
# Contact... mbb262@cornell.edu
# Date...... 2022-11-06
# Updated... 2022-11-06
#
# Description:
# Plot yield data from Lewis 2006 tobacco paper for Zeavolution talk
# DOI 10.1007/s00122-006-0482-0
# ------------------------------------------------------------------------------

# Libraries
library(ggplot2)

# Load in data
yield <- read.csv("~/Box Sync/Cornell_PhD/publications/Khaipho-Burch 2022 Yield Letter/data/lewis_tobacco_yield.csv")

# Plot
a <- ggplot(yield, aes(x = reorder(Genotype, -Plot_Yield), y = Plot_Yield, fill = Type)) +
  geom_bar(stat="identity") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 55, vjust = 1, hjust=1), 
        text = element_text(size=20)) +
  scale_fill_manual(values=c("#999999", "#D55E00")) +
  xlab("Genotype") +
  ylab("Plot Yield (g)")
  
ggsave(plot = a,width = 11, height = 8, units = "in",
       filename = "~/Box Sync/Cornell_PhD/publications/Khaipho-Burch 2022 Yield Letter/data/yield_plot.png",)
