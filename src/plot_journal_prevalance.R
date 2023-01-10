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
# https://www.webofscience.com/wos/woscc/summary/96cb19e7-9c19-4d3e-a091-bf02ed87f89b-5d550e90/relevance/1

# To just see the results for GS and GP in Nature or science
# (((SO=(Nature)) OR SO=(Science)) AND ALL=("genomic prediction")) OR ALL=("genomic selection")
# https://www.webofscience.com/wos/woscc/summary/0bdc7dbd-0d41-41e2-b9b3-e5a4c7117933-5d546bc1/relevance/1

# Could potentially add "estimate the breeding value" --> in mewiwessen paper

# Scopus search
# ( ALL ( "genomic prediction" )  OR  ALL ( "genomic selection" )  OR  ALL ( "genetic values predicted from markers" ) ) 
# https://www.scopus.com/results/results.uri?sort=plf-f&src=s&nlo=&nlr=&nls=&sid=4a93da2f7a9dbb1af2e6469abb9896a8&sot=a&sdt=a&sl=103&s=%28ALL%28%22genomic+prediction%22%29+OR+ALL%28%22genomic+selection%22%29+OR+ALL%28%22genetic+values+predicted+from+markers%22%29%29&cl=t&offset=1&origin=resultslist&ss=plf-f&ws=r-f&ps=r-f&cs=r-f&cc=10&txGid=7ced7be7cedaf431550a97d977d6f447&DDMDownload=true&selectAllCheckBox=true


# ------------------------------------------------------------------------------
# Prep Data
# ------------------------------------------------------------------------------

# Libraries
library(dplyr)
library(ggplot2)
library(data.table)

# Load in all csv files
files <- list.files("~/git_projects/p_yield/data/wos/", full.names = TRUE, pattern = "*.csv")

# Load in all files, rbind together
wos <- lapply(files, data.table::fread)
wos <- data.table::rbindlist(wos)

# Filter out some extra columns, no books or conference titles
wos <- wos %>% 
  filter(`Publication Type` == "J") %>% 
  select("Authors", "Article Title", "Publication Year",
         "Source Title", "DOI Link")

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
a

# Save to file
ggsave(plot = a,width = 11, height = 8, units = "in",
       filename = "~/git_projects/p_yield/images/top_20_journal_count.png",)








