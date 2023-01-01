# ------------------------------------------------------------------------------
# Author.... Merritt Khaipho-Burch
# Contact... mbb262@cornell.edu
# Date...... 2022-11-30
# Updated... 2022-11-30
#
# Description:
# Using Scopus and Pubmed search commands to count the number of genomic selection
# or genomic prediction papers occuring within journals and plotting
#
# Plot counts of the number of times an article appears in a specific journal
# ------------------------------------------------------------------------------

# Data downloaded from the web of science using this command

# Scopus search
# ( ALL ( "genomic prediction" )  AND  ALL ( "genomic selection" ) ) 
# https://www.scopus.com/results/results.uri?sort=plf-f&src=s&st1=%22genomic+prediction%22&st2=%22genomic+selection%22&sid=2d98fdd84b230560d3838842160b817b&sot=b&sdt=b&sl=56&s=%28ALL%28%22genomic+prediction%22%29+AND+ALL%28%22genomic+selection%22%29%29&origin=searchbasic&editSaveSearch=&yearFrom=Before+1960&yearTo=Present

# Alternatively to grab the mewissen paper
# ( ALL ( "genomic prediction" )  OR  ALL ( "genomic selection" )  OR  ALL ( "genetic values predicted from markers" ) ) 
# https://www.scopus.com/results/results.uri?sort=plf-f&src=s&nlo=&nlr=&nls=&sid=4a93da2f7a9dbb1af2e6469abb9896a8&sot=a&sdt=a&sl=103&s=%28ALL%28%22genomic+prediction%22%29+OR+ALL%28%22genomic+selection%22%29+OR+ALL%28%22genetic+values+predicted+from+markers%22%29%29&cl=t&offset=1&origin=resultslist&ss=plf-f&ws=r-f&ps=r-f&cs=r-f&cc=10&txGid=7ced7be7cedaf431550a97d977d6f447&DDMDownload=true&selectAllCheckBox=true

# Pubmed search
# (("genomic prediction") OR ("genomic selection"))
# https://pubmed.ncbi.nlm.nih.gov/?term=%28%28%22genomic+prediction%22%29+OR+%28%22genomic+selection%22%29%29


# ------------------------------------------------------------------------------
# Prep Data
# ------------------------------------------------------------------------------

# Libraries
library(dplyr)
library(ggplot2)
library(data.table)

# Pallet
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# Guessing nature's color pallete
# light grey, medium grey, dark grey, light blue, dark blue, yellow, orange
ncp <- c("#B9C3CC", "#88939D", "#5B6571", "#1CADE3", "#106FB9", "#FDA750", "#FB501E")


# Load scopus data
scopus <- read.csv("~/git_projects/p_yield/data/scopus_gs_gp_all.csv") %>% 
  filter(Document.Type %in% c("Article", "Review")) %>% 
  select("Authors", "Title", "Year", "Source.title", "DOI")

# Shorten PNAS
scopus$Source.title <- gsub("Proceedings of the National Academy of Sciences of the United States of America", "PNAS", scopus$Source.title)
# Load pubmed
pub <- read.csv("~/git_projects/p_yield/data/pubmed_gs_gp_all.csv") %>% 
  select("Authors", "Title", "Publication.Year", "Journal.Book", "DOI")

# Combine pub and scopus data, add metadata first, make names similar
colnames(pub) <- c("Authors", "Title", "Year", "Source.title", "DOI")
pub$Source <- rep("PubMed", nrow(pub))
scopus$Source <- rep("Scopus", nrow(scopus))
scopus_pub <- rbind(pub, scopus)

# ------------------------------------------------------------------------------
# Plot line graph by year
# ------------------------------------------------------------------------------

# Calculate the number of articles published by year
yearCount <- scopus_pub %>% 
  group_by(Year, Source) %>% 
  summarise(n = n())

# Remove 2023 and things with no Year
yearCount <- yearCount %>% 
  filter(Year < 2023 & Year >= 2001)

# Plot number of articles by year
b <- ggplot(data=yearCount, aes(x=as.factor(Year), y=n, group=Source)) +
  geom_line(aes(color=Source)) +
  geom_point() +
  theme_bw() +
  theme(text = element_text(size=20),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  xlab("Publication Year") +
  ylab("Number of papers mentioning GS or GP")  +
  scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9"))
b

# Save to file
ggsave(plot = b,width = 8, height = 6, units = "in",
       filename = "~/git_projects/p_yield/images/gs_gp_papers_by_year_scopus_pubmed.png",)


# ------------------------------------------------------------------------------
# Split split by journals - top 10 plant journals vs Science/Nature/PNAS in SCOPUS
# (no flavor journals, i.e. Nature Communications, Nature Genetics, etc)
# ------------------------------------------------------------------------------

# What are the top 15 journals publishing these papers?
scopus %>% 
  group_by(Source.title) %>% 
  summarise(n=n()) %>% 
  top_n(20) %>% 
  arrange(desc(n))

# Filter to top plant journals plus nature and science
sub_scopus <- scopus %>% 
  filter(Source.title %in% c("Science", 
                             "Nature",
                             "PNAS",
                             "Frontiers in Plant Science",
                             "Frontiers in Genetics",
                             "G3: Genes, Genomes, Genetics",
                             "Theoretical and Applied Genetics",
                             "PLoS ONE",
                             "BMC Genomics",
                             "Crop Science", 
                             "Plant Genome",
                             "Genetics",
                             "BMC Genetics"))


# Take counts by journal
count_sub_scopus <- sub_scopus %>% 
  group_by(Year, Source.title) %>% 
  summarise(n = n())

# Create a simiplified color scheme
count_sub_scopus$Journal <- count_sub_scopus$Source.title
count_sub_scopus$Journal <- gsub("Frontiers in Plant Science", "Top 10 Domain Specific Plant Journals", count_sub_scopus$Journal)
count_sub_scopus$Journal <- gsub("Frontiers in Genetics", "Top 10 Domain Specific Plant Journals", count_sub_scopus$Journal)
count_sub_scopus$Journal <- gsub("G3: Genes, Genomes, Genetics", "Top 10 Domain Specific Plant Journals", count_sub_scopus$Journal)
count_sub_scopus$Journal <- gsub("Theoretical and Applied Genetics", "Top 10 Domain Specific Plant Journals", count_sub_scopus$Journal)
count_sub_scopus$Journal <- gsub("PLoS ONE", "Top 10 Domain Specific Plant Journals", count_sub_scopus$Journal)
count_sub_scopus$Journal <- gsub("BMC Genomics", "Top 10 Domain Specific Plant Journals", count_sub_scopus$Journal)
count_sub_scopus$Journal <- gsub("Crop Science", "Top 10 Domain Specific Plant Journals", count_sub_scopus$Journal)
count_sub_scopus$Journal <- gsub("Plant Genome", "Top 10 Domain Specific Plant Journals", count_sub_scopus$Journal)
count_sub_scopus$Journal <- gsub("BMC Genetics", "Top 10 Domain Specific Plant Journals", count_sub_scopus$Journal)
count_sub_scopus$Journal <- gsub("Genetics", "Top 10 Domain Specific Plant Journals", count_sub_scopus$Journal)

# Plot a stacked barchart by year - see all journal names
c <- ggplot(count_sub_scopus, aes(fill=Source.title, y=n, x=as.factor(Year))) + 
  geom_bar(position="stack", stat="identity") +
  theme_classic() +
  theme(text = element_text(size=20),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        legend.position = "bottom") +
  xlab("Publication Year") +
  ylab("# papers mentioning GS or GP")  +
  ggtitle("Scopus: Top 10 domain specific journas plus Nature, Science, and PNAS") +
  scale_fill_brewer(palette="Paired")
ggsave(plot = c,width = 13, height = 8, units = "in",
       filename = "~/git_projects/p_yield/images/gs_gp_papers_by_year_scopus_top10.png",)

# Grey out most colors
d <- ggplot(count_sub_scopus, aes(fill=Journal, y=n, x=as.factor(Year))) + 
  geom_bar(position="stack", stat="identity") +
  theme_classic() +
  theme(text = element_text(size=20),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        legend.position = "bottom") +
  xlab("Publication Year") +
  ylab("# of papers mentioning GS or GP")  + 
  ggtitle("Scopus: Top 10 domain specific journas plus Nature, Science, and PNAS") +
  scale_fill_manual(values=c("#FB501E", "#FDA750","#B9C3CC"))
ggsave(plot = d,width = 13, height = 8, units = "in",
       filename = "~/git_projects/p_yield/images/gs_gp_papers_by_year_scopus_top10_grouped.png",)

# Calculate a percentage
temp <- sub_scopus %>% 
  group_by(Source.title) %>% 
  summarise(n = n()) 
temp$percentage <- (temp$n/sum(temp$n))*100
temp


# ------------------------------------------------------------------------------
# Split split by journals - top 10 plant journals vs Science/Nature/PNAS in pubMed
# (no flavor journals, i.e. Nature Communications, Nature Genetics, etc)
# ------------------------------------------------------------------------------

# What are the top 15 journals publishing these papers?
pub %>% 
  group_by(Source.title) %>% 
  summarise(n=n()) %>% 
  top_n(20) %>% 
  arrange(desc(n))

# Filter to top plant journals plus nature and science
sub_pub <- pub %>% 
  filter(Source.title %in% c("Science", 
                             "Nature",
                             "Proc Natl Acad Sci U S A",
                             "Front Plant Sci",
                             "Front Genet",
                             "G3 (Bethesda)",
                             "Theor Appl Genet",
                             "PLoS ONE",
                             "BMC Genomics",
                             "Genes (Basel)", 
                             "Plant Genome",
                             "Genetics",
                             "BMC Genet"))


# Take counts by journal
count_sub_pub <- sub_pub %>% 
  filter(Year > 2001) %>% 
  group_by(Year, Source.title) %>% 
  summarise(n = n())

# Create a simiplified color scheme
count_sub_pub$Journal <- count_sub_pub$Source.title
count_sub_pub$Journal <- gsub("Front Plant Sci", "Top 10 Domain Specific Plant Journals", count_sub_pub$Journal)
count_sub_pub$Journal <- gsub("Front Genet", "Top 10 Domain Specific Plant Journals", count_sub_pub$Journal)
count_sub_pub$Journal <- gsub("G3.*", "Top 10 Domain Specific Plant Journals", count_sub_pub$Journal)
count_sub_pub$Journal <- gsub("Theor Appl Genet", "Top 10 Domain Specific Plant Journals", count_sub_pub$Journal)
count_sub_pub$Journal <- gsub("PLoS ONE", "Top 10 Domain Specific Plant Journals", count_sub_pub$Journal)
count_sub_pub$Journal <- gsub("BMC Genomics", "Top 10 Domain Specific Plant Journals", count_sub_pub$Journal)
count_sub_pub$Journal <- gsub("Genes.*", "Top 10 Domain Specific Plant Journals", count_sub_pub$Journal)
count_sub_pub$Journal <- gsub("Plant Genome", "Top 10 Domain Specific Plant Journals", count_sub_pub$Journal)
count_sub_pub$Journal <- gsub("BMC Genet", "Top 10 Domain Specific Plant Journals", count_sub_pub$Journal)
count_sub_pub$Journal <- gsub("Genetics", "Top 10 Domain Specific Plant Journals", count_sub_pub$Journal)
count_sub_pub$Journal <- gsub("Proc Natl Acad Sci U S A", "PNAS", count_sub_pub$Journal)

# Plot a stacked barchart by year - see all journal names
e <- ggplot(count_sub_pub, aes(fill=Source.title, y=n, x=as.factor(Year))) + 
  geom_bar(position="stack", stat="identity") +
  theme_classic() +
  theme(text = element_text(size=20),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        legend.position = "bottom") +
  xlab("Publication Year") +
  ylab("# papers mentioning GS or GP")  +
  ggtitle("PubMed: Top 10 domain specific journas plus Nature, Science, and PNAS") +
  scale_fill_brewer(palette="Paired")
e
ggsave(plot = e,width = 13, height = 8, units = "in",
       filename = "~/git_projects/p_yield/images/gs_gp_papers_by_year_pubmed_top10.png",)

# Grey out most colors
f <- ggplot(count_sub_pub, aes(fill=Journal, y=n, x=as.factor(Year))) + 
  geom_bar(position="stack", stat="identity") +
  theme_classic() +
  theme(text = element_text(size=20),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        legend.position = "bottom") +
  xlab("Publication Year") +
  ylab("# of papers mentioning GS or GP")  + 
  ggtitle("PubMed: Top 10 domain specific journas plus Nature, Science, and PNAS") +
  scale_fill_manual(values=c("#FB501E", "#FDA750","#106FB9", "#B9C3CC"))
f
ggsave(plot = f,width = 13, height = 8, units = "in",
       filename = "~/git_projects/p_yield/images/gs_gp_papers_by_year_pubmed_top10_grouped.png",)

# Calculate a percentage
temp <- sub_pub %>% 
  group_by(Source.title) %>% 
  summarise(n = n()) 
temp$percentage <- (temp$n/sum(temp$n))*100
temp


# ------------------------------------------------------------------------------
# Try a line plot colored by jounal
# ------------------------------------------------------------------------------

# Show the top 10 journals by year
pub %>% 
  group_by(Source.title) %>% 
  summarise(n=n()) %>% 
  top_n(20) %>% 
  arrange(desc(n))

# Select top 10 journals plus the two we're interested in
sub_pub_line <- pub %>% 
  select(Year, Source.title) %>% 
  filter(Source.title %in% c("Science", 
                             "Nature",
                             "Genet Sel Evo",
                             "J Dairy Sci",
                             "G3 (Bethesda)",
                             "Front Plant Sci",
                             "Front Genet",
                             "Theor Appl Genet",
                             "PLoS ONE",
                             "BMC Genomics",
                             "J Anim Sci",
                             "Plant Genome"))

# Create a simplified name for coloring the plot
sub_pub_line$Journal <- sub_pub_line$Source.title
sub_pub_line$Journal <- gsub("Genet Sel Evo", "Top 10 Domain Specific Plant Journals", sub_pub_line$Journal)
sub_pub_line$Journal <- gsub("J Dairy Sci", "Top 10 Domain Specific Plant Journals", sub_pub_line$Journal)
sub_pub_line$Journal <- gsub("G3.*", "Top 10 Domain Specific Plant Journals", sub_pub_line$Journal)
sub_pub_line$Journal <- gsub("Front Plant Sci", "Top 10 Domain Specific Plant Journals", sub_pub_line$Journal)
sub_pub_line$Journal <- gsub("Front Genet", "Top 10 Domain Specific Plant Journals", sub_pub_line$Journal)
sub_pub_line$Journal <- gsub("Theor Appl Genet", "Top 10 Domain Specific Plant Journals", sub_pub_line$Journal)
sub_pub_line$Journal <- gsub("PLoS ONE", "Top 10 Domain Specific Plant Journals", sub_pub_line$Journal)
sub_pub_line$Journal <- gsub("BMC Genomics", "Top 10 Domain Specific Plant Journals", sub_pub_line$Journal)
sub_pub_line$Journal <- gsub("J Anim Sci", "Top 10 Domain Specific Plant Journals", sub_pub_line$Journal)
sub_pub_line$Journal <- gsub("Plant Genome", "Top 10 Domain Specific Plant Journals", sub_pub_line$Journal)

# # add in zero count data for nature and science
# sci <- c()
# sci$Year <- c(seq(1995,2013), seq(2015, 2019), 2021, 2022)
# sci <- data.frame(sci)
# sci$Source.title <- rep("Science", nrow(sci))
# sci$Journal <- rep("Science", nrow(sci))
# 
# nat <- c()
# nat$Year <- c(seq(1995,2020))
# nat <- data.frame(nat)
# nat$Source.title <- rep("Nature", nrow(nat))
# nat$Journal <- rep("Nature", nrow(nat))
# 
# # Combine everything
# sci <- rbind(sci, nat)
# sub_pub_line <- rbind(sub_pub_line, sci)

# Take counts of number of articles published in a journal
sub_pub_line_mean <- sub_pub_line %>% 
  group_by(Year, Journal) %>% 
  summarise(n = n())  %>% 
  group_by(Year, Journal) %>% 
  summarise(mean = mean(n))
part2

# Plot
# 2001 original paper came out
# 2011 - rrBLUP by Jeffrey Endelman
# 2014 - BGLR by Gustavo de los Campos and Paulino Perez Rodriguez
g <- ggplot(data=sub_pub_line_mean, aes(x=Year, y=mean, group=Journal)) +
  geom_line(aes(color=Journal)) +
  geom_vline(xintercept = 2001) +
  geom_vline(xintercept = 2007) +
  geom_vline(xintercept = 2011) +
  geom_vline(xintercept = 2014) +
  geom_point() +
  theme_classic() +
  theme(text = element_text(size=20),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        legend.position = "bottom") +
  xlab("Publication Year") +
  ylab("Number of papers mentioning GS or GP")  +
  scale_fill_brewer(palette="Paired") +
  ggtitle("PubMed: Top 10 domain specific journas plus Nature & Science")
g

# Save to file
ggsave(plot = g,width = 8, height = 6, units = "in",
       filename = "~/git_projects/p_yield/images/line_gs_gp_papers_by_year_pubmed.png",)




