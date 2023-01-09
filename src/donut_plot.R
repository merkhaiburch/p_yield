# ------------------------------------------------------------------------------
# Author.... Merritt Khaipho-Burch
# Contact... mbb262@cornell.edu
# Date...... 2022-12-31
# Updated... 2022-12-31
#
# Description:
# Making a donut plot of publication counts
# https://r-graph-gallery.com/128-ring-or-donut-plot.html
# ------------------------------------------------------------------------------

# Load in packages
library(dplyr)
library(ggplot2)
library(data.table)


# ------------------------------------------------------------------------------
##              Web of science queries across categories 
# ------------------------------------------------------------------------------

# GS OR GP AND yield across all journals - all categories 
# ((ALL=("genomic selection")) OR ALL=("genomic prediction")) AND ALL=("yield")
# https://www.webofscience.com/wos/woscc/summary/fde859df-25db-44d4-bcbd-5a9fe5fec81e-6810f694/relevance/1 
temp1 <- read.delim("~/git_projects/p_yield/data/wos_2022_12_31/gs_gp_yield_all_journals_allCategories_wos_1.txt", sep = ",")
temp2 <- read.delim("~/git_projects/p_yield/data/wos_2022_12_31/gs_gp_yield_all_journals_allCategories_wos_2.txt", sep = ",")
gs_gp_all_journals <- rbind(temp1, temp2)
gs_gp_all_journals <- gs_gp_all_journals %>% 
  select(Title, Authors, Source.Title, Publication.Year) %>% 
  unique()


# transgenic and yield across all journals
# Had to format it like this because I wanted to weed out the two dfs with weird formatting (6 and 8)
# ((((ALL=("transgenic")) OR ALL=("transgene")) OR ALL=("overexpression lines")) OR ALL=("knockout")) AND ALL=("yield")
# https://www.webofscience.com/wos/woscc/summary/1f3f315d-bf60-47f8-a289-ade4d351e4ac-68118dd4/relevance/1
trans_yield_part1 <- read.delim("~/git_projects/p_yield/data/wos_2022_12_31/transgenic_yield_all_journals_allCategories_wos_1.txt") %>% select(TI, AU, SO, PY)
trans_yield_part2 <- read.delim("~/git_projects/p_yield/data/wos_2022_12_31/transgenic_yield_all_journals_allCategories_wos_2.txt") %>% select(TI, AU, SO, PY)
trans_yield_part3 <- read.delim("~/git_projects/p_yield/data/wos_2022_12_31/transgenic_yield_all_journals_allCategories_wos_3.txt") %>% select(TI, AU, SO, PY)
trans_yield_part4 <- read.delim("~/git_projects/p_yield/data/wos_2022_12_31/transgenic_yield_all_journals_allCategories_wos_4.txt") %>% select(TI, AU, SO, PY)
trans_yield_part5 <- read.delim("~/git_projects/p_yield/data/wos_2022_12_31/transgenic_yield_all_journals_allCategories_wos_5.txt") %>% select(TI, AU, SO, PY)
trans_yield_part6 <- read.delim("~/git_projects/p_yield/data/wos_2022_12_31/transgenic_yield_all_journals_allCategories_wos_6.csv", sep = ",") %>% select(TI, AU, SO, PY)
trans_yield_part7 <- read.delim("~/git_projects/p_yield/data/wos_2022_12_31/transgenic_yield_all_journals_allCategories_wos_7.txt") %>% select(TI, AU, SO, PY)
trans_yield_part8 <- read.delim("~/git_projects/p_yield/data/wos_2022_12_31/transgenic_yield_all_journals_allCategories_wos_8.csv", sep = ",") %>% select(TI, AU, SO, PY)
trans_yield_part9 <- read.delim("~/git_projects/p_yield/data/wos_2022_12_31/transgenic_yield_all_journals_allCategories_wos_9.txt") %>% select(TI, AU, SO, PY)
trans_yield_all <- rbind(trans_yield_part1, trans_yield_part2, trans_yield_part3,
                         trans_yield_part4, trans_yield_part5, trans_yield_part6,
                         trans_yield_part7, trans_yield_part8, trans_yield_part9) %>% unique()
colnames(trans_yield_all) <- c("Title", "Authors", "Source.Title", "Publication.Year")


# Remove unused variables 
rm(temp1, temp2)
rm(trans_yield_part1, trans_yield_part2, trans_yield_part3, trans_yield_part4, trans_yield_part5, trans_yield_part6, trans_yield_part7, trans_yield_part8, trans_yield_part9)


## Subset data ----------------------------------------------------------------

# GS OR GP AND yield solely in nature and science across all categories
gs_nat_sci_all <- gs_gp_all_journals %>% filter(Source.Title %in% c("NATURE", "SCIENCE"))
gs_nat_sci_all$Type <- rep("Genomic Selection", nrow(gs_nat_sci_all))
dim(gs_nat_sci_all)

# GS OR GP AND yield NOT in nature or science across all categories
gs_other_all <- gs_gp_all_journals %>% filter(!Source.Title %in% c("NATURE", "SCIENCE"))
gs_other_all$Type <- rep("Genomic Selection", nrow(gs_other_all))
dim(gs_other_all)

# transgenic and yield in nature and science across all categories
yield_nat_sci_all <- trans_yield_all %>% filter(Source.Title %in% c("NATURE", "SCIENCE"))
yield_nat_sci_all$Type <- rep("Transgenic & Yield", nrow(yield_nat_sci_all))
dim(yield_nat_sci_all)

# transgenic and yield NOT in nature and science across all categories
yield_other_all <- trans_yield_all %>% filter(!Source.Title %in% c("NATURE", "SCIENCE"))
yield_other_all$Type <- rep("Transgenic & Yield", nrow(yield_other_all))
dim(yield_other_all)

# Combine all
nature_trends <- rbind(gs_nat_sci_all, yield_nat_sci_all) %>% 
  group_by(Type) %>% 
  summarise(n = n())

other_trends <- rbind(gs_other_all, yield_other_all) %>% 
  group_by(Type) %>% 
  summarise(n = n())


## Plot data in a doughnut plot ------------------------------------------------

# Load in custom nature pallete
# Guessing nature's color pallete
# light grey, medium grey, dark grey, light blue, dark blue, yellow, orange
# ncp <- c("#B9C3CC", "#88939D", "#5B6571", "#1CADE3", "#106FB9", "#FDA750", "#FB501E")
ncp <- c( "#FB501E", "#B9C3CC")


# Compute percentages
nature_trends$fraction <- (nature_trends$n / sum(nature_trends$n))*100
other_trends$fraction <- (other_trends$n / sum(other_trends$n))*100

# Compute the cumulative percentages (top of each rectangle)
nature_trends$ymax <- cumsum(nature_trends$fraction)
other_trends$ymax <- cumsum(other_trends$fraction)

# Compute the bottom of each rectangle
nature_trends$ymin <- c(0, head(nature_trends$ymax, n=-1))
other_trends$ymin <- c(0, head(other_trends$ymax, n=-1))

# Compute label position
nature_trends$labelPosition <- (nature_trends$ymax + nature_trends$ymin) / 2
other_trends$labelPosition <- (other_trends$ymax + other_trends$ymin) / 2

# Make the plot for nature and science
a <- ggplot(nature_trends, aes(x = 2, y = fraction, fill = Type)) +
  geom_bar(stat = "identity", color = "white") +
  coord_polar(theta = "y", start = 0)+
  geom_text(aes(label = paste0(round(fraction, digits = 2), "%", "\n", "n = ", n)), position = position_stack(vjust = 0.5)) +
  annotate(geom = 'text', x = 0.5, y = 0, label = "Nature and Science")+
  scale_fill_manual(values = ncp) +
  theme_void() +
  xlim(.5, 2.5) +
  guides(fill=guide_legend(title="Article Type"))

b <- ggplot(other_trends, aes(x = 2, y = fraction, fill = Type)) +
  geom_bar(stat = "identity", color = "white") +
  coord_polar(theta = "y", start = 0)+
  geom_text(aes(label = paste0(round(fraction, digits = 2), "%", "\n", "n = ", n)), position = position_stack(vjust = 0.5)) +
  annotate(geom = 'text', x = 0.5, y = 0, label = "All Other Journals")+
  scale_fill_manual(values = ncp) +
  theme_void() +
  xlim(.5, 2.5)  +
  theme(legend.position="bottom")+
  guides(fill=guide_legend(title="Article Type"))


# Export to file
ggpubr::ggarrange(a, b,
                  nrow = 1, ncol = 2, 
                  common.legend = TRUE, 
                  legend = "none", 
                  align = "v")


ggsave(filename = "~/git_projects/p_yield/images/gs_vs_transgenic_nat_sci_vs_all_others_v2.png",
       plot = ggpubr::ggarrange(a, b,
                                nrow = 1, ncol = 2, 
                                common.legend = TRUE, 
                                legend = "bottom", 
                                align = "v"),
       width = 8.5,
       height = 6,
       units = "in",
       dpi = "retina"
)


# Old versions of plots --------------------------------------------------------
# Version 1
# good but have to manyally adjust y positions
# nature_trends$lab.ypos <- c(98, 50)
# ggplot(nature_trends, aes(x = 2, y = fraction, fill = Type)) +
#   geom_bar(stat = "identity", color = "white") +
#   coord_polar(theta = "y", start = 0)+
#   geom_text(aes(y = lab.ypos, label = paste0("n = ", n, ", \n", round(fraction, digits = 2), "%")), color = "black")+
#   annotate(geom = 'text', x = 0.5, y = 0, label = "Nature and Science")+
#   scale_fill_manual(values = ncp) +
#   theme_void() +
#   xlim(.5, 2.5) 

# Version 2
# Compute a good label
# nature_trends$label <- paste0(nature_trends$Type, "\n value: ", nature_trends$n)
# other_trends$label <- paste0(other_trends$Type, "\n value: ", other_trends$n)
# nature_trends$label <- paste0(nature_trends$Type, nature_trends$n)
# other_trends$label <- paste0(other_trends$Type, other_trends$n)

# ggplot(other_trends, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Type)) +
#   geom_rect() +
#   geom_text( x=2, aes(y=labelPosition, label=label), size=6) + # x here controls label position (inner / outer)
#   scale_fill_manual(values = ncp) +
#   scale_color_manual(values = ncp) +
#   coord_polar(theta="y") +
#   xlim(c(-1, 4)) +
#   theme_void() +
#   theme(legend.position = "none") +
#   ggtitle("#WOS papers for all other journals")


# ------------------------------------------------------------------------------
##          Web of science queries for Plant Science & Agronomy 
# ------------------------------------------------------------------------------

# GS OR GP AND yield across all journals - Plant Sciences and Agronomy
# ((ALL=("genomic selection")) OR ALL=("genomic prediction")) AND ALL=("yield")
# Refined By:Web of Science Categories: Plant Sciences or Agronomy
# https://www.webofscience.com/wos/woscc/summary/8466f122-c878-4522-a493-4031331ccf9a-68117bc9/relevance/1
gs_gp_plantSci_agron <- read.delim("~/git_projects/p_yield/data/wos_2022_12_31/gs_gp_yield_all_journals_plantSciences_or_agronomy.txt", sep = ",")
gs_gp_plantSci_agron <- gs_gp_plantSci_agron %>% 
  select(Title, Authors, Source.Title, Publication.Year) %>% 
  unique()

# transgenic and yield across all journals - Plant Sciences and Agronomy
# ((((ALL=("transgenic")) OR ALL=("transgene")) OR ALL=("overexpression lines")) OR ALL=("knockout")) AND ALL=("yield")
# https://www.webofscience.com/wos/woscc/summary/1f3f315d-bf60-47f8-a289-ade4d351e4ac-68118dd4/relevance/1
trans_yield_plantSci_agron_1 <- read.delim("~/git_projects/p_yield/data/wos_2022_12_31/transgenic_yield_all_journals_plantSciences_or_agronomy_1.txt") %>% select(TI, AU, SO, PY)
trans_yield_plantSci_agron_2 <- read.delim("~/git_projects/p_yield/data/wos_2022_12_31/transgenic_yield_all_journals_plantSciences_or_agronomy_2.txt") %>% select(TI, AU, SO, PY)
trans_yield_plantSci_agron_3 <- read.delim("~/git_projects/p_yield/data/wos_2022_12_31/transgenic_yield_all_journals_plantSciences_or_agronomy_3.txt") %>% select(TI, AU, SO, PY)
trans_yield_plantSci_agron_4 <- read.delim("~/git_projects/p_yield/data/wos_2022_12_31/transgenic_yield_all_journals_plantSciences_or_agronomy_4.csv", sep = ",") %>% select(TI, AU, SO, PY)
trans_yield_plantSci_agron_all <- rbind(trans_yield_plantSci_agron_1, trans_yield_plantSci_agron_2,
                                        trans_yield_plantSci_agron_3, trans_yield_plantSci_agron_4) %>% unique()
colnames(trans_yield_plantSci_agron_all) <- c("Title", "Authors", "Source.Title", "Publication.Year")


# Remove extra variables
rm(trans_yield_plantSci_agron_1, trans_yield_plantSci_agron_2,trans_yield_plantSci_agron_3, trans_yield_plantSci_agron_4)


## Subset data ----------------------------------------------------------------

# GS OR GP AND yield solely in nature and science across all categories
gs_nat_sci_psAgron <- gs_gp_plantSci_agron %>% filter(Source.Title %in% c("NATURE", "SCIENCE"))
gs_nat_sci_psAgron$Type <- rep("Genomic Selection Papers", nrow(gs_nat_sci_psAgron))
dim(gs_nat_sci_psAgron)

# GS OR GP AND yield NOT in nature or science across all categories
gs_other_psAgron <- gs_gp_plantSci_agron %>% filter(!Source.Title %in% c("NATURE", "SCIENCE"))
gs_other_psAgron$Type <- rep("Genomic Selection Papers", nrow(gs_other_psAgron))
dim(gs_other_psAgron)

# transgenic and yield in nature and science across all categories
yield_nat_sci_psAgron <- trans_yield_plantSci_agron_all %>% filter(Source.Title %in% c("NATURE", "SCIENCE"))
yield_nat_sci_psAgron$Type <- rep("Transgenic & Yield Papers", nrow(yield_nat_sci_psAgron))
dim(yield_nat_sci_psAgron)

# transgenic and yield NOT in nature and science across all categories
yield_other_psAgron <- trans_yield_plantSci_agron_all %>% filter(!Source.Title %in% c("NATURE", "SCIENCE"))
yield_other_psAgron$Type <- rep("Transgenic & Yield Papers", nrow(yield_other_psAgron))
dim(yield_other_psAgron)

# Combine all
nature_trends_psAgron <- rbind(gs_nat_sci_psAgron, yield_nat_sci_psAgron) %>% 
  group_by(Type) %>% 
  summarise(n = n())

other_trends_psAgron <- rbind(gs_other_psAgron, yield_other_psAgron) %>% 
  group_by(Type) %>% 
  summarise(n = n())


## Plot data in a doughnut plot ------------------------------------------------

# Load in custom nature pallete
# Guessing nature's color pallete
# light grey, medium grey, dark grey, light blue, dark blue, yellow, orange
# ncp <- c("#B9C3CC", "#88939D", "#5B6571", "#1CADE3", "#106FB9", "#FDA750", "#FB501E")
ncp <- c( "#FB501E", "#B9C3CC")

# Compute percentages
nature_trends_psAgron$fraction <- nature_trends_psAgron$n / sum(nature_trends_psAgron$n)
other_trends_psAgron$fraction <- other_trends_psAgron$n / sum(other_trends_psAgron$n)

# Compute the cumulative percentages (top of each rectangle)
nature_trends_psAgron$ymax <- cumsum(nature_trends_psAgron$fraction)
other_trends_psAgron$ymax <- cumsum(other_trends_psAgron$fraction)

# Compute the bottom of each rectangle
nature_trends_psAgron$ymin <- c(0, head(nature_trends_psAgron$ymax, n=-1))
other_trends_psAgron$ymin <- c(0, head(other_trends_psAgron$ymax, n=-1))

# Compute label position
nature_trends_psAgron$labelPosition <- (nature_trends_psAgron$ymax + nature_trends_psAgron$ymin) / 2
other_trends_psAgron$labelPosition <- (other_trends_psAgron$ymax + other_trends_psAgron$ymin) / 2

# Compute a good label
nature_trends_psAgron$label <- paste0(nature_trends_psAgron$Type, "\n value: ", nature_trends_psAgron$n)
other_trends_psAgron$label <- paste0(other_trends_psAgron$Type, "\n value: ", other_trends_psAgron$n)

# Make the plot for nature and science
a <- ggplot(nature_trends_psAgron, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Type)) +
  geom_rect() +
  geom_text( x=2, aes(y=labelPosition, label=label), size=6) + # x here controls label position (inner / outer)
  scale_fill_manual(values = ncp) +
  scale_color_manual(values = ncp) +
  coord_polar(theta="y") +
  xlim(c(-1, 4)) +
  theme_void() +
  theme(legend.position = "none")+
  ggtitle("#WOS papers for Nature and Science (Categories = PS & Agronomy)")


b <- ggplot(other_trends_psAgron, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Type)) +
  geom_rect() +
  geom_text( x=2, aes(y=labelPosition, label=label), size=6) + # x here controls label position (inner / outer)
  scale_fill_manual(values = ncp) +
  scale_color_manual(values = ncp) +
  coord_polar(theta="y") +
  xlim(c(-1, 4)) +
  theme_void() +
  theme(legend.position = "none") +
  ggtitle("#WOS papers for all other journals (Categories = PS & Agronomy)")


# Export to file
ggsave(filename = "~/git_projects/p_yield/images/gs_vs_transgenic_nat_sci_vs_all_others_plantScience_agronomy.png",
       plot = b,
       width = 5.5,
       height = 5.5,
       units = "in",
       dpi = "retina"
)



