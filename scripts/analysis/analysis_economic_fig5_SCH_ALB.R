#' ----
#' title: script to prepare Fig. 5 with trade-offs of scenarios for both regions
#' author: Esteban Menares
#' date: 30.12.2024
#' ----

#' [NOTE]: indicator = single conservation goal; scenario = joint conservation goal

# 1. Set up  -------------------------------------------------------------

library(tidyverse)
conflicted::conflicts_prefer(dplyr::filter())
library(ggpubr) # for extra ggplot functionalities
library(ggthemes) # extra themes 
library(fmsb) # for radarcharts

# load source script for fig_label function
source('scripts/source_script.R')

## ---- set plotting theme
theme_set(theme_clean(10))
par(family = "Helvetica")

## ---- Read in data

scenarios_alb <- readRDS(file = 'data/processed/trade_offs_scenarios_ALB.rds')
scenarios_sch <- readRDS(file = 'data/processed/trade_offs_scenarios_SCH.rds')


# ALB ---------------------------------------------------------------------

# keep only numeric vars in df for radar chart

radar_charts_num <-
  scenarios_alb %>% 
  filter(optimisation %in% c("all_scores", 
                             "ESR", 
                             "ESS", 
                             "RDR", 
                             "TIN", 
                             "TSC", 
                             "all_scores_budget",
                             "ESR_budget", 
                             "ESS_budget", 
                             "RDR_budget", 
                             "TIN_budget", 
                             "TSC_budget"
  )) %>% 
  column_to_rownames("optimisation")

## --- create radar chart plots 

# To use the fmsb package, I have to add 2 lines to the dataframe: the max and min of each topic to show on the plot!

# add 0-100 to make radar charts comparable with each other

radar_charts_num <- rbind(rep(100, 12) , rep(0, 12) , radar_charts_num)
radar_charts_num

# Prepare file names

myfile_names <-
  scenarios_alb %>% 
    select(optimisation) %>% 
    filter(optimisation %in% c("all_scores", "ESR", "ESS", "RDR", "TIN", "TSC")) %>% 
    pull()
myfile_names

# prepare plot titles 

mytitle <-
  myfile_names %>% 
  str_replace_all("all_scores", "All scores")
mytitle

# prepare labels
vlabels_indicators <-
  radar_charts_num %>% 
  names() %>% 
  str_replace("connectance", "Connectance") %>% 
  str_replace("crop_pest_potential", "Crop pest \npotential") %>% 
  str_replace("red_list_plants", "Red list \nplants") %>% 
  str_replace("pollination_potential", "Pollination \npotential") %>% 
  str_replace("nestedness", " \nNestedness") %>% 
  str_replace("butterfly_richness", "Butterfly richness") %>% 
  str_replace("regional_distribution_butterflies", "Reg. dist. \nbutterflies") %>% 
  str_replace("\\btrophic_interactions\\b", "Trophic ints.") %>% 
  str_replace("regional_distribution_plants", "Reg. dist. \nplants") %>% 
  str_replace("red_list_butterflies", "Red list \nbutterflies") %>% 
  str_replace("plant_richness", "Plant \nrichness") %>% 
  str_replace("unique_trophic_interactions", "Unique \ntrophic ints.")
  

## ---- Radar chart with all 19 plots combined in one image

# assign paths for image
pdf(file = "output/plots/economic_modelling/ALB/radar_charts_scenarios/tradeoffs/radar_chart_tradeoffs_scenarios_collage_small.pdf",
    width = 7.5, 
    height = 4.5)

# Prepare the screen 
par(mar = c(0, 0, 2.5, 0))
par(mfrow = c(2, 3))

# Loop for each plot
for(i in 1:6) {
  
  # Custom the radar chart
  radarchart(
    radar_charts_num[c(1:2, 2+i, 8+i),],
    axistype = 1,
    maxmin = TRUE, # row 1 = max, row 2 = min
    
    # custom polygon
    seg = 5, # n of segment per axis
    # pcol = c(hcl.colors(n = 2, palette = "viridis", alpha = 1, rev = T)), # color codes 
    pcol = "black",
    pfcol = NULL, # color codes for filling polygons
    plwd = 0.6, # line widths 
    plty = 1 , # line types 
    pty = c(20, 13), # point symbol
    cex = 5,
    
    # custom the grid
    cglcol = "grey", # Line color for radar grids
    cglty = 1, # Line type for radar grids
    axislabcol = "grey50", # Color of axis label and numbers
    cglwd = 0.6, # Line width for radar grids
    
    # custom labels
    vlcex = 0.9, # font size
    calcex = 0.6, # font size for caxislabels
    vlabels = vlabels_indicators,
    
    #title
    title = mytitle[i]
  )
}

# Add a custom label to the margin
fig_label(text = "(a)", cex = 2.5, region = "device", pos = "topleft")

dev.off()



# SCH ---------------------------------------------------------------------

# keep only numeric vars in df for radar chart

radar_charts_num <-
  scenarios_sch %>% 
  filter(optimisation %in% c("all_scores", 
                             "ESR", 
                             "ESS", 
                             "RDR", 
                             "TIN", 
                             "TSC", 
                             "all_scores_budget",
                             "ESR_budget", 
                             "ESS_budget", 
                             "RDR_budget", 
                             "TIN_budget", 
                             "TSC_budget"
  )) %>% 
  column_to_rownames("optimisation")

## --- create radar chart plots 

# To use the fmsb package, I have to add 2 lines to the dataframe: the max and min of each topic to show on the plot!

# add 0-100 to make radar charts comparable with each other

radar_charts_num <- rbind(rep(100, 10) , rep(0, 10) , radar_charts_num)
radar_charts_num

# Prepare file names

myfile_names <-
  scenarios_sch %>% 
  select(optimisation) %>% 
  filter(optimisation %in% c("all_scores", "ESR", "ESS", "RDR", "TIN", "TSC")) %>% 
  pull()
myfile_names

# prepare plot titles 

mytitle <-
  myfile_names %>% 
  str_replace_all("all_scores", "All scores")
mytitle

# prepare labels
vlabels_indicators <-
  radar_charts_num %>% 
  names() %>% 
  str_replace("connectance", "Connectance") %>% 
  str_replace("crop_pest_potential", "Crop pest \npotential") %>% 
  str_replace("pollination_potential", "Pollination \npotential") %>% 
  str_replace("nestedness", " \nNestedness") %>% 
  str_replace("butterfly_richness", "Butterfly richness") %>% 
  str_replace("regional_distribution_butterflies", "Reg. dist. \nbutterflies") %>% 
  str_replace("\\btrophic_interactions\\b", "Trophic ints.") %>% 
  str_replace("regional_distribution_plants", "Reg. dist. \nplants") %>% 
  str_replace("plant_richness", "Plant \nrichness") %>% 
  str_replace("unique_trophic_interactions", "Unique trophic ints.")


## ---- Radar chart with all 19 plots combined in one image

# assign paths for image
pdf(file = "output/plots/economic_modelling/SCH/radar_charts_scenarios/tradeoffs/radar_chart_tradeoffs_scenarios_collage_small.pdf",
    width = 7.5, 
    height = 4.5)

# Prepare the screen 
par(mar = c(0, 0, 2.5, 0))
par(mfrow = c(2, 3))

# Loop for each plot
for(i in 1:6) {
  
  # Custom the radar chart
  radarchart(
    radar_charts_num[c(1:2, 2+i, 8+i),],
    axistype = 1,
    maxmin = TRUE, # row 1 = max, row 2 = min
    
    # custom polygon
    seg = 5, # n of segment per axis
    # pcol = c(hcl.colors(n = 2, palette = "viridis", alpha = 1, rev = T)), # color codes 
    pcol = "black",
    pfcol = NULL, # color codes for filling polygons
    plwd = 0.6, # line widths 
    plty = 1 , # line types 
    pty = c(20, 13), # point symbol
    cex = 5,
    
    # custom the grid
    cglcol = "grey", # Line color for radar grids
    cglty = 1, # Line type for radar grids
    axislabcol = "grey50", # Color of axis label and numbers
    cglwd = 0.6, # Line width for radar grids
    
    # custom labels
    vlcex = 0.9, # font size
    calcex = 0.6, # font size for caxislabels
    vlabels = vlabels_indicators,
    
    #title
    title = mytitle[i]
  )
}

# Add a custom label to the margin
fig_label(text = "(b)", cex = 2.5, region = "device", pos = "topleft")

dev.off()

