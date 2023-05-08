# Calculate tons per hectare from nature paper
# https://www.nature.com/articles/s41588-022-01283-w#Sec32

# scale up plots
# 1 rows*10 plants per row*0.6 interspace rows *0.25 interspace plants=1.5 meters^2/plot
number_of_rows <- 1
plants_per_row <- 10
interspace_rows <- 0.6
interspace_plants <- 0.25
(plot <- number_of_rows*plants_per_row*interspace_rows*interspace_plants)

# square meters in a hectare
# 1 hectare=(100 meters)^2=10000 meters^2
hectare <- 10000

# number of their plots that could fit in a hectare
# of their plots that could fit in a ha: (10,000m^2)/(1.5 m^2/plot)= 6666.667 plots/ha
(their_plot_hectares <- hectare/plot)

# number of their hectares times their yield per hectare
# 6666.667 plots/ha*138g=0.92 T/ha
yield <- 138 # in grams
(their_plot_hectares*yield)/1000000

