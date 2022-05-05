library(shiny)
library(tidyverse)

# Reading data in
games_sales <- read_csv("data/Video_Games_Sales_as_at_22_Dec_2016.csv")

# Creating dataframe for the first Graph
# The first graph answers the question, What are the Best Selling DS
# Games By publisher across the years provided in the data?
# Line graph using subplots

# Filtering the Data to only keep Rows that have DS as platform
# And selecting only the necessary data needed.

games_filtered_by_DS <- games_sales %>% 
  filter(`Platform` == "DS") %>% 
  select(`Name`,`Platform`,`Year_of_Release`,`Global_Sales`)

# Getting number of years present in the data
games_filtered_by_DS %>% 
  distinct(`Year_of_Release`) # It looks like there's an outlier, 1985

# Lets check how many games that have 1985 which is the outlier.
# the DS didn't exist back in 1985.
# There is also the N/A Value in the Year Column, which proves to be an issue
# After manually checking out the N/A rows in the data, there are simply
# too many entries that I can't just get remove.

games_filtered_by_DS %>% 
  filter(`Year_of_Release` == 1985) %>% 
  count() # Theres only one entry. I suspect it is probably a re-release,
  # or a possible error in the data entry.

# Judgment call, I'm going to get rid of the row that contains 1985, solely
# Because there aren't any other data points besides that ONE point, which would look
# Really weird, as the subplot would include 1985 with just that game being
# The dominator.
# The N/A Column will not be removed, as it has too many values.
# I could go the extra mile and figure out how to automate searching online
# and grabbing the release year in R, but thats beyond the scope of this project.

games_filtered_by_DS <- games_filtered_by_DS %>% 
  filter(`Year_of_Release` >= 2004)

# Checking the values of Year Of Release column again

games_filtered_by_DS %>% 
  distinct(`Year_of_Release`)

# Theres also simply TOO many values for it to properly fit within subplots
# About 2151 games that were released total on the Nintendo DS.
# I will only be accounting for the top 5 per year in the data.

games_filtered_by_DS_top_5_by_yr <- games_filtered_by_DS %>% 
  arrange(desc(`Global_Sales`)) %>% 
  group_by(`Year_of_Release`) %>% 
  slice(1:5)

# Graphing 
games_filtered_by_DS_top_5_by_yr %>% 
  ggplot(aes(x = reorder(Name, Global_Sales),
             y = Global_Sales,
             group = Year_of_Release,
             fill = Name)) +
  geom_bar(stat='identity') + 
  facet_wrap(~Year_of_Release, scales="free_x", ncol=3) +
  theme(strip.text = element_text(face="bold"),
        axis.text.x = element_blank(),
        legend.text = element_text(size=5.5)) 
