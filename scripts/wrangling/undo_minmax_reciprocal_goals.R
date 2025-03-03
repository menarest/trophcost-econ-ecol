#' ----
#' title: this script undo the min-max scaling and the reciprocal of optimisation results resulting in the original scale values of ecological goals.
#' author: Esteban Menares
#' date: 2024.06.01
#' ----



# 1. Set up  -------------------------------------------------------------

library(tidyverse)
conflicted::conflicts_prefer(dplyr::filter())

## ---- Read in data

# first we read the optimisations results. Here we use the single files from each optimization round for region ALB. These can be downloaded as a single file from BExIS using ID 31798 and running R script under ID 31807 to split them into individual files.

list(
  optis = read_csv("data/processed/single_ecol_alb.csv"),
  optis_1344 = read_csv("data/processed/single_ecol_econ_1344_alb.csv"),
  optis_991 = read_csv("data/processed/single_ecol_econ_991_alb.csv"),
  optis_scenarios = read_csv("data/processed/joint_ecol_alb.csv"),
  optis_scenarios_1344 = read_csv("data/processed/joint_ecol_econ_1344_alb.csv"),
  optis_scenarios_991 = read_csv("data/processed/joint_ecol_econ_991_alb.csv")
) %>% 
  map(
    ~ .x %>%
      # set col names to lower case and remove _pa of some vars
      rename_with(
        ., ~tolower(str_replace_all(., "_pa$", ""))) %>% 
      # remove the "_pa" ending within the optimisation col
      mutate(
        optimisation = optimisation %>% 
          str_replace_all(., "_pa$", "")
      ) %>% 
      # set names of list elements 
      set_names(
        names(.))) %>% 
  # assign to global environment 
  list2env(., envir = .GlobalEnv)

# then we read the original (unscaled) indicators to undo the calculations. Note: this is  the full list of indicators which can be downloaded from BExIS using ID 31791. As example, we will work with optimization results from region ALB only. We need to first filter values for region ALB and then select only the goals we previously optimized for in the table with all goals (scores).

scores <-
  read_csv("data/raw/scores_site.csv") %>% # replace the path and name of file
  as_tibble() %>% 
  filter(region == "ALB") %>% 
  # remove the "_pa" ending within the optimisation col
  rename_with(.fn = ~str_replace_all(., "_pa$", ""), 
              .cols = everything()) %>% 
  select(
    "region",
    "epid",
    "n_lepi", 
    "n_plant", 
    "n_red_list_lepi", 
    "n_red_list_plant",     
    "av_reg_dist_lepi",
    "av_reg_dist_plant",
    "sum_troph_int",
    "n_unique_int",
    "connectance",
    "z_score_nest",
    "sum_crops",
    "n_crop_pests"
  )

## ---- Define a function used for normalise min-max 

# Min-Max Normalization transforms x to x’ by converting each value of features to a range between 0 and 1, and this is also known as (0–1) Normalization. If the data has negative values the range would have been between -1 and 1.

norm_minmax <- function(x, na.rm = FALSE) {
  (x - min(x, na.rm = na.rm)) / (max(x, na.rm = na.rm) - min(x, na.rm = na.rm))
}

## ---- Define a function to undo min-max scaling 

# Adapted from: rMIDAS::undo_minmax()

# Arguments:
# scaled: A numeric vector or column, scaled between 0 and 1.
# unscaled: The numeric unscaled vector

undo_minmax <- function(scaled, unscaled, na.rm = FALSE) {
  unscaled_min <- min(unscaled, na.rm = na.rm)
  unscaled_range <- max(unscaled, na.rm = na.rm) - unscaled_min
  original <- scaled * unscaled_range + unscaled_min
  return(original)
}

# Example: 
# set.seed(1)
# ex_num <- runif(100,1,10)
# scaled_var <- norm_minmax(ex_num)
# undo_scale <- undo_minmax(scaled = scaled_var, unscaled = ex_num)

# Prove two are identical
# all.equal(ex_num, undo_scale)

## ---- Define a function to calculate inverse of a number 

inv <- function(x)1/x



## ---- Undo transformations ------------------------------------------------

my_list <-
  list(
    optis,
    optis_1344,
    optis_991,
    optis_scenarios,
    optis_scenarios_991,
    optis_scenarios_1344
  ) 

# New names for the list elements
new_names <-
  c(
    "new_optis",
    "new_optis_1344",
    "new_optis_991",
    "new_optis_scenarios",
    "new_optis_scenarios_991",
    "new_optis_scenarios_1344"
  )

my_new_list <-
  map(
    my_list, 
    ~ .x %>% 
      filter(region == "ALB") %>% 
      
      # reclassify values of pests manually back to original values
      mutate(
        n_crop_pests = case_when(
          n_crop_pests == 1 ~ 0,
          n_crop_pests == 0.5 ~ 1,
          n_crop_pests == 0 ~ 2)) %>% 
      
      # undo minmax scaling
      mutate(
        across(
          c(n_lepi,
            n_plant,
            n_red_list_lepi,
            n_red_list_plant,
            sum_troph_int,
            n_unique_int,
            connectance,
            z_score_nest,
            sum_crops
          ),
          ~ undo_minmax(scaled = .x, 
                        unscaled = scores %>% 
                          filter(region == "ALB") %>%  
                          select(cur_column()) %>% pull(), 
                        na.rm = TRUE)),
        
        # undo the reciprocal of regional distribution range
        across(
          c(av_reg_dist_lepi, av_reg_dist_plant), 
          ~1/undo_minmax(scaled = .x, 
                         unscaled = scores %>% 
                           filter(region == "ALB") %>%  
                           select(cur_column()) %>% 
                           pull() %>% 
                           inv(), 
                         na.rm = TRUE)
        ))) %>% 
  set_names(new_names)

# Assign the list elements to the global environment
list2env(my_new_list, envir = .GlobalEnv)