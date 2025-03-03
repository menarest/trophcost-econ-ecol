#' ----
#' title: plot-level scores pre-processing for ecological-economic optimisation
#' author: Esteban Menares
#' date: 2023-10-11
#' ----


#' **AIM**: to pre-process scores selected from the exploratory analysis done in  ~/scores_exploration.qmd for economic (cost-benefit) optimisation. 

#' **steps**: 
#' - because for the economic optimisation we optimise by maximising values, we need to calculate reciprocal of pest potential (more pest is bad, so we should optimise for smaller values) and average distribution range of lepi and plants (we should optimise for species with smaller distribution range because they might be more endangered). For pest potential, we have only 0, 1 and 2 values, so we do manual reclassification of the values of pest potential by inverting their values. 
#' - we min-max scale all values to be bounded between 0 and 1


# Set up --------------------------------------------------------------

library(tidyverse)

source('scripts/source_script.R')

# Read in data

scores <- 
  read_csv("data/processed/scores_economic.csv") %>% 
  as_tibble()


# Wrangling ---------------------------------------------------------------

scores_scaled <- 
  scores %>% 
  
  # calculate reciprocal of selected scores  
  mutate(
    across(
      c(av_reg_dist_lepi_pa, av_reg_dist_plant_pa),
      ~ 1 / .),
    
    # reclassify values of pests 
    n_crop_pests_pa = case_when(
      n_crop_pests_pa == 0 ~ 1, 
      n_crop_pests_pa == 1 ~ 0.5,
      n_crop_pests_pa == 2 ~ 0)) %>% 
  
  # min-max scale variables per region
  mutate(
    across(
      where(is.numeric),
      ~ norm_minmax(., na.rm = TRUE)),
    .by = region) 

# check reciprocal and scaling of n_crop_pests_pa 

scores %>% 
  select(epid, n_crop_pests_pa) %>% 
  left_join(
    scores_scaled %>% 
      select(epid, n_crop_pests_pa_rcpl = n_crop_pests_pa),
    by = "epid")


# Save data ---------------------------------------------------------------

write_csv(scores_scaled, "data/processed/scores_economic_scaled.csv")