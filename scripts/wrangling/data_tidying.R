
# title: data tidying for TrophCost data
# author: Esteban Menares
# last update: 2024.08.27


# NOTE: many of the datasets were obtained from BExIS (BExIS (https://www.bexis.uni-jena.de/). Use the indicated ID (and version) to search the original datasets on their website. 


# Setup ---------------------------------------------------------------

library(tidyverse) # for data wrangling
library(data.table) # for data wrangling 

source('scripts/source_script.R')



# ECONOMIC OPTIMISATION ---------------------------------------------------

# ID for ALB: https://www.bexis.uni-jena.de/ddm/data/Showdata/31798 
# ID for SCH: https://www.bexis.uni-jena.de/ddm/data/Showdata/31832 

#' *Steps*: 
# - read each optimisation file
# - add column name with the name of the file (i.e. optimisation)
# - wrangle all files and merge
# - save each individual optimisation file merged with the all scores table 
# - save all files as one big table as well 
# - do the same for scenarios 



## 15.1 ALB -------------------------------------------------------------------

library(tidyverse)

## ---- read in and tidy data

# read big table with all scores and measures 

all_measures_alb <-
  read_delim("data/processed/EP_all_measures_scores_ALB_cost_indicators.csv") %>% 
  rename_with(tolower, .cols = everything()) %>% 
  relocate(region, .before = measure_id) %>% 
  relocate(epid, id, .after = region)



### 15.1.1 Unconstrained optimisations for single goals -----------------------

# list of paths for unconstrained optimisations

#' [NOTE]: check the length of "paths" vector every time you load it, this should match the number of opti files in your local folder, otherwise adapt the pattern = "^Result_" to recognise different files. This uses strings and regular expresions. 

paths <- file.path("resources_troph-cost/economic/optimisations/ALB/indicators",
                   list.files("resources_troph-cost/economic/optimisations/ALB/indicators",
                              pattern = "^Result_"))

# create a list with all optimisations

optis_list <-
  paths %>%
  set_names(
    basename(.) %>%
      str_remove_all(., "Result_") %>%
      str_remove_all(., ".csv") %>%
      str_remove(., "_(\\d*)$|^(\\d*)_") %>%
      str_replace_all(., " \\* ", "_")
  ) %>% 
  
  # loop over each file
  map(
    ~ read.csv(.x, sep = ";", dec = ",") %>%
      as_tibble() %>%
      select(-X) %>%
      
      # clean names
      rename_with(.fn = tolower, .cols = everything()) %>%
      rename_with(~ gsub("x.", "", .x, fixed = TRUE)) %>%
      rename_with(~ gsub(".", "", .x, fixed = TRUE)) %>%
      rename(measure_id = matches("MID"),
             epid = matches("FID")) %>%
      
      # add the file name as a col name and extract budget and indicator name
      mutate(optimisation =
               basename(.x) %>%
               str_remove(., "Result_") %>%
               str_remove(., ".csv") %>%
               str_remove(., "^\\d*_") %>% 
               str_remove(., "_(\\d*)$") %>% 
               str_replace_all(., " \\+ ", "_"), 
             .before = epid) %>%
      
      # create a variable with the budget (= sum of costs across all sites)
      mutate(budget = sum(totalcost, na.rm = TRUE) %>% 
               ceiling(.), .after = totalcost) %>% 
      
      # merge the files with all measures with each optimisation in the list
      inner_join(all_measures_alb, .,
                 by = c("epid", "measure_id")) %>% 
      rename(cost = totalcost))

## ---- save all individual merged files

walk2(optis_list, names(optis_list), ~ {
  file_name <- paste0("resources_troph-cost/economic/optimisations/ALB/indicators",
                      "/merged_unconstrained-", .y, ".csv")
  write_csv(.x, file_name)
})


## ---- combine all files into one

optis <-
  optis_list %>%
  list_rbind() %>%
  # add a unique identifier to each row
  unite(
    optimisation,
    epid,
    col = "id",
    sep = "_",
    remove = FALSE,
    na.rm = FALSE
  ) %>%
  relocate(id, .before = region) %>% 
  filter(ecovalue != 0) %>% 
  mutate(budget = sum(cost, na.rm = TRUE) %>% 
           ceiling(.),
         .by = optimisation) 

# save the combined long file 

write_csv(optis, "data/processed/single_ecol_alb.csv")



### 15.1.2 Constrained optimisations for a 1344€ budget  ----------------------

## ---- read each optimization file 

# list of paths for constrained optimisations for a 1344€ budget 
paths_1344 <- file.path("resources_troph-cost/economic/optimisations/ALB/1344",
                        list.files("resources_troph-cost/economic/optimisations/ALB/1344",
                                   pattern = "^Result_"))


optis_list_1344 <-
  paths_1344 %>%
  set_names(
    basename(.) %>%
      str_remove_all(., "Result_") %>%
      str_remove_all(., ".csv") %>%
      str_remove(., "_(\\d*)$|^(\\d*)_") %>%
      str_replace_all(., " \\* ", "_")
  ) %>% 
  
  # loop over each file
  map(
    ~ read.csv(.x, sep = ";", dec = ",") %>%
      as_tibble() %>%
      select(-X) %>%
      
      # clean names
      rename_with(.fn = tolower, .cols = everything()) %>%
      rename_with(~ gsub("x.", "", .x, fixed = TRUE)) %>%
      rename_with(~ gsub(".", "", .x, fixed = TRUE)) %>%
      rename(measure_id = matches("MID"),
             epid = matches("FID")) %>%
      
      # add the file name as a col name and extract budget and indicator name
      mutate(optimisation =
               basename(.x) %>%
               str_remove(., "Result_") %>%
               str_remove(., ".csv") %>%
               str_remove(., "^\\d*_") %>% 
               str_remove(., "_(\\d*)$") %>% 
               str_replace_all(., " \\+ ", "_"), 
             .before = epid) %>%
      
      # create a variable with the budget (= sum of costs across all sites)
      mutate(budget = sum(totalcost, na.rm = TRUE) %>% 
               ceiling(.), .after = totalcost) %>% 
      
      # merge the files with all measures with each optimisation in the list
      inner_join(all_measures_alb, .,
                 by = c("epid", "measure_id")) %>% 
      rename(cost = totalcost)) 

## ---- save all individual merged files

walk2(optis_list_1344, names(optis_list_1344), ~ {
  file_name <- paste0("resources_troph-cost/economic/optimisations/ALB/1344",
                      "/merged-1344-", .y, ".csv")
  write_csv(.x, file_name)
})


## ---- combine all files into one

optis_1344 <- 
  optis_list_1344 %>%
  list_rbind() %>%
  # add a unique identifier to each row
  unite(
    optimisation,
    epid,
    col = "id",
    sep = "_",
    remove = FALSE,
    na.rm = FALSE
  ) %>%
  relocate(id, .before = region) %>% 
  filter(ecovalue != 0) %>% 
  mutate(budget = sum(cost, na.rm = TRUE) %>% 
           ceiling(.),
         .by = optimisation) 

# save the combined long file 

write_csv(optis_1344, "data/processed/single_ecol_econ_1344_alb.csv")





### 15.1.3 Constrained optimisations for a 991€ budget  ----------------------

## ---- read each optimization file 

# list of paths for constrained optimisations for a 991€ budget 
paths_991 <- file.path("resources_troph-cost/economic/optimisations/ALB/991",
                        list.files("resources_troph-cost/economic/optimisations/ALB/991",
                                   pattern = "^Result_"))


optis_list_991 <-
  paths_991 %>%
  set_names(
    basename(.) %>%
      str_remove_all(., "Result_") %>%
      str_remove_all(., ".csv") %>%
      str_remove(., "_(\\d*)$|^(\\d*)_") %>%
      str_replace_all(., " \\* ", "_")
  ) %>% 
  
  # loop over each file
  map(
    ~ read.csv(.x, sep = ";", dec = ",") %>%
      as_tibble() %>%
      select(-X) %>%
      
      # clean names
      rename_with(.fn = tolower, .cols = everything()) %>%
      rename_with(~ gsub("x.", "", .x, fixed = TRUE)) %>%
      rename_with(~ gsub(".", "", .x, fixed = TRUE)) %>%
      rename(measure_id = matches("MID"),
             epid = matches("FID")) %>%
      
      # add the file name as a col name and extract budget and indicator name
      mutate(optimisation =
               basename(.x) %>%
               str_remove(., "Result_") %>%
               str_remove(., ".csv") %>%
               str_remove(., "^\\d*_") %>% 
               str_remove(., "_(\\d*)$") %>% 
               str_replace_all(., " \\+ ", "_"), 
             .before = epid) %>%
      
      # create a variable with the budget (= sum of costs across all sites)
      mutate(budget = sum(totalcost, na.rm = TRUE) %>% 
               ceiling(.), .after = totalcost) %>% 
      
      # merge the files with all measures with each optimisation in the list
      inner_join(all_measures_alb, .,
                 by = c("epid", "measure_id")) %>% 
      rename(cost = totalcost)) 

## ---- save all individual merged files

walk2(optis_list_991, names(optis_list_991), ~ {
  file_name <- paste0("resources_troph-cost/economic/optimisations/ALB/991",
                      "/merged-991-", .y, ".csv")
  write_csv(.x, file_name)
})


## ---- combine all files into one

optis_991 <- 
  optis_list_991 %>%
  list_rbind() %>%
  # add a unique identifier to each row
  unite(
    optimisation,
    epid,
    col = "id",
    sep = "_",
    remove = FALSE,
    na.rm = FALSE
  ) %>%
  relocate(id, .before = region) %>% 
  filter(ecovalue != 0) %>% 
  mutate(budget = sum(cost, na.rm = TRUE) %>% 
           ceiling(.),
         .by = optimisation) 

# save the combined long file 

write_csv(optis_991, "data/processed/single_ecol_econ_991_alb.csv")




### 15.1.4 Unconstrained optimisations for scenarios --------------------------

# joint goals = scenarios

## ---- read each optimization file 

# list of paths for unconstrained optimisations

paths_scenarios <-
  file.path(
    "resources_troph-cost/economic/optimisations/ALB/scenarios",
    list.files(
      "resources_troph-cost/economic/optimisations/ALB/scenarios",
      pattern = "^Result_"
    )
  )

# create a list with all optimisations

scenarios_list <-
  paths_scenarios %>%
  set_names(
    basename(.) %>%
      str_remove_all(., "Result_") %>%
      str_remove_all(., ".csv") %>%
      str_remove(., "_(\\d*)$|^(\\d*)_") %>%
      str_replace_all(., " \\* ", "_") %>% 
      str_replace_all(., " \\+ ", "_")
  ) %>% 
  
# loop over each file
  map(
    ~ read.csv(.x, sep = ";", dec = ",") %>%
      as_tibble() %>%
      select(-X) %>%
      
      # clean names
      rename_with(.fn = tolower, .cols = everything()) %>%
      rename_with(~ gsub("x.", "", .x, fixed = TRUE)) %>%
      rename_with(~ gsub(".", "", .x, fixed = TRUE)) %>%
      rename(measure_id = matches("MID"),
             epid = matches("FID")) %>%
      
      # add the file name as a col name and extract budget and indicator name
      mutate(optimisation =
               basename(.x) %>%
               str_remove(., "Result_") %>%
               str_remove(., ".csv") %>%
               str_remove(., "^\\d*_") %>% 
               str_remove(., "_(\\d*)$") %>% 
               str_replace_all(., " \\+ ", "_"), 
             .before = epid) %>%
      
      # create a variable with the budget (= sum of costs across all sites)
      mutate(budget = sum(totalcost, na.rm = TRUE) %>% 
               ceiling(.), .after = totalcost) %>% 
      
      # merge the files with all measures with each optimisation in the list
      inner_join(all_measures_alb, .,
                 by = c("epid", "measure_id")) %>% 
      rename(cost = totalcost)) 

## ---- save all individual merged files

walk2(scenarios_list, names(scenarios_list), ~ {
  file_name <- paste0("resources_troph-cost/economic/optimisations/ALB/scenarios",
                      "/merged_scenario_unconstrained-", .y, ".csv")
  write_csv(.x, file_name)
})


## ---- combine all files into one

optis_scenarios <-
  scenarios_list %>%
  list_rbind() %>%
  # add a unique identifier to each row
  unite(
    optimisation,
    epid,
    col = "id",
    sep = "_",
    remove = FALSE,
    na.rm = FALSE
  ) %>%
  relocate(id, .before = region) %>% 
  filter(ecovalue != 0) %>% 
  mutate(budget = sum(cost, na.rm = TRUE) %>% 
           ceiling(.),
         .by = optimisation) 

# save the combined long file 

write_csv(optis_scenarios, "data/processed/joint_ecol_alb.csv")



### 15.1.5 Optimisations scenarios for a 991€ budget -------------------------

## ---- read each optimization file 

# list of paths for unconstrained optimisations

paths_scenarios_991 <-
  file.path(
    "resources_troph-cost/economic/optimisations/ALB/scenarios_budget991",
    list.files(
      "resources_troph-cost/economic/optimisations/ALB/scenarios_budget991",
      pattern = "^Result_"
    )
  )

# create a list with all optimisations

scenarios_list_991 <-
  paths_scenarios_991 %>%
  set_names(
    basename(.) %>%
      str_remove_all(., "Result_") %>%
      str_remove_all(., ".csv") %>%
      str_remove(., "_(\\d*)$|^(\\d*)_") %>%
      str_replace_all(., " \\* ", "_") %>% 
      str_replace_all(., " \\+ ", "_")
  ) %>% 
  
  # loop over each file
  map(
    ~ read.csv(.x, sep = ";", dec = ",") %>%
      as_tibble() %>%
      select(-X) %>%
      
      # clean names
      rename_with(.fn = tolower, .cols = everything()) %>%
      rename_with(~ gsub("x.", "", .x, fixed = TRUE)) %>%
      rename_with(~ gsub(".", "", .x, fixed = TRUE)) %>%
      rename(measure_id = matches("MID"),
             epid = matches("FID")) %>%
      
      # add the file name as a col name and extract budget and indicator name
      mutate(optimisation =
               basename(.x) %>%
               str_remove(., "Result_") %>%
               str_remove(., ".csv") %>%
               str_remove(., "^\\d*_") %>% 
               str_remove(., "_(\\d*)$") %>% 
               str_replace_all(., " \\+ ", "_"), 
             .before = epid) %>%
      
      # create a variable with the budget (= sum of costs across all sites)
      mutate(budget = sum(totalcost, na.rm = TRUE) %>% 
               ceiling(.), .after = totalcost) %>% 
      
      # merge the files with all measures with each optimisation in the list
      inner_join(all_measures_alb, .,
                 by = c("epid", "measure_id")) %>% 
      rename(cost = totalcost)) 

## ---- save all individual merged files

walk2(scenarios_list_991, names(scenarios_list_991), ~ {
  file_name <- paste0("resources_troph-cost/economic/optimisations/ALB/scenarios_budget991",
                      "/merged_scenario_991-", .y, ".csv")
  write_csv(.x, file_name)
})


## ---- combine all files into one

optis_scenarios_991 <-
  scenarios_list_991 %>%
  list_rbind() %>%
  # add a unique identifier to each row
  unite(
    optimisation,
    epid,
    col = "id",
    sep = "_",
    remove = FALSE,
    na.rm = FALSE
  ) %>%
  relocate(id, .before = region) %>% 
  filter(ecovalue != 0) %>% 
  mutate(budget = sum(cost, na.rm = TRUE) %>% 
           ceiling(.),
         .by = optimisation) 

# save the combined long file 

write_csv(optis_scenarios_991, "data/processed/joint_ecol_econ_991_alb.csv")




### 15.1.6 Optimisations scenarios for a 1344 € budget ------------------------

## ---- read each optimization file 

# list of paths for unconstrained optimisations

paths_scenarios_1344 <-
  file.path(
    "resources_troph-cost/economic/optimisations/ALB/scenarios_budget1344",
    list.files(
      "resources_troph-cost/economic/optimisations/ALB/scenarios_budget1344",
      pattern = "^Result_"
    )
  )

# create a list with all optimisations

scenarios_list_1344 <-
  paths_scenarios_1344 %>%
  set_names(
    basename(.) %>%
      str_remove_all(., "Result_") %>%
      str_remove_all(., ".csv") %>%
      str_remove(., "_(\\d*)$|^(\\d*)_") %>%
      str_replace_all(., " \\* ", "_") %>% 
      str_replace_all(., " \\+ ", "_")
  ) %>% 
  
  # loop over each file
  map(
    ~ read.csv(.x, sep = ";", dec = ",") %>%
      as_tibble() %>%
      select(-X) %>%
      
      # clean names
      rename_with(.fn = tolower, .cols = everything()) %>%
      rename_with(~ gsub("x.", "", .x, fixed = TRUE)) %>%
      rename_with(~ gsub(".", "", .x, fixed = TRUE)) %>%
      rename(measure_id = matches("MID"),
             epid = matches("FID")) %>%
      
      # add the file name as a col name and extract budget and indicator name
      mutate(optimisation =
               basename(.x) %>%
               str_remove(., "Result_") %>%
               str_remove(., ".csv") %>%
               str_remove(., "^\\d*_") %>% 
               str_remove(., "_(\\d*)$") %>% 
               str_replace_all(., " \\+ ", "_"), 
             .before = epid) %>%
      
      # create a variable with the budget (= sum of costs across all sites)
      mutate(budget = sum(totalcost, na.rm = TRUE) %>% 
               ceiling(.), .after = totalcost) %>% 
      
      # merge the files with all measures with each optimisation in the list
      inner_join(all_measures_alb, .,
                 by = c("epid", "measure_id")) %>% 
      rename(cost = totalcost)) 

## ---- save all individual merged files

walk2(scenarios_list_1344, names(scenarios_list_1344), ~ {
  file_name <- paste0("resources_troph-cost/economic/optimisations/ALB/scenarios_budget1344",
                      "/merged_scenario_1344-", .y, ".csv")
  write_csv(.x, file_name)
})


## ---- combine all files into one

optis_scenarios_1344 <-
  scenarios_list_1344 %>%
  list_rbind() %>%
  # add a unique identifier to each row
  unite(
    optimisation,
    epid,
    col = "id",
    sep = "_",
    remove = FALSE,
    na.rm = FALSE
  ) %>%
  relocate(id, .before = region) %>% 
  filter(ecovalue != 0) %>% 
  mutate(budget = sum(cost, na.rm = TRUE) %>% 
           ceiling(.),
         .by = optimisation) 

# save the combined long file 

write_csv(optis_scenarios_1344, "data/processed/joint_ecol_econ_1344_alb.csv")


### 15.1.7 Merging single files -----------------------------------------------

# this section can be read just one time because after reading it, we add two cols which are then used to wrangle the files afterwards 

## ---- Read in files

# individual optimisation files for merging into a single file as in ID 31798

list(
  optis_alb = read_csv("data/processed/single_ecol_alb.csv"),
  optis_1344_alb = read_csv("data/processed/single_ecol_econ_1344_alb.csv"),
  optis_991_alb = read_csv("data/processed/single_ecol_econ_991_alb.csv"),
  optis_scenarios_alb = read_csv("data/processed/joint_ecol_alb.csv"),
  optis_scenarios_1344_alb = read_csv("data/processed/joint_ecol_econ_1344_alb.csv"),
  optis_scenarios_991_alb = read_csv("data/processed/joint_ecol_econ_991_alb.csv")
) %>% 
  map(
    ~ .x %>%
      # set col names to lower case 
      rename_with(., ~tolower(.)) %>% 
      # set names of list elements 
      set_names(
        names(.)) %>% 
      as_tibble()) %>% 
  # assign to global environment 
  list2env(., envir = .GlobalEnv)


## ---- data wrangling

# List of dataframes

data_list <- list(
  optis_alb,
  optis_1344_alb,
  optis_991_alb,
  optis_scenarios_alb,
  optis_scenarios_1344_alb,
  optis_scenarios_991_alb
)

# Vector of opti_type values to recycle

opti_types <- c("ecol", "ecol_econ_1344", "ecol_econ_991")

# Piped statement to add columns and bind rows

final_df <- data_list %>%
  imap(~ .x %>% 
         # add goal type determining the value based on the index of list elements
         mutate(goal_type = ifelse(.y <= 3, "single", "joint"), 
                .before = region) %>%
         # add opti_type, recycle values based on the index of list elements: ".y" is the index of the current dataframe in the list; "(.y - 1)" adjusts the index to start from 0 (because R is 1-indexed, but modulo operation is easier to understand with 0-indexing); "%% length(opti_types)" calculates the remainder when the adjusted index is divided by the length of opti_types (which is 3); "+ 1": Adjusts the result back to 1-indexing for correct vector indexing in R
         mutate(opti_type = opti_types[(.y - 1) %% length(opti_types) + 1],
                .after = goal_type)) %>%
  bind_rows() 

# Inspect the final dataframe

final_df %>% View()

# test that the operation is correct

all.equal(
  final_df %>% 
    filter(goal_type == "single" & opti_type == "ecol_econ_1344") %>% 
    select(-c(goal_type, opti_type)),
  optis_1344_alb
)

all.equal(
  final_df %>% 
    filter(goal_type == "joint" & opti_type == "ecol_econ_991") %>% 
    select(-c(goal_type, opti_type)),
  optis_scenarios_991_alb
)

## ---- save the final dataset 

write_excel_csv(final_df, 'data/processed/all_optimisations_alb.csv')

## ---- UPDATE: add a column with the EPIDs without leading zero 

all_optis_alb <- read_csv('data/processed/all_optimisations_alb.csv')

all_optis_alb <-
  all_optis_alb %>%
  mutate(site_id = epid %>%
           str_remove(., "(?<=G)0"),
         .after = epid)

write_excel_csv(all_optis_alb, 'data/processed/all_optimisations_alb.csv')


## ---- UPDATE: add a column with the EPIDs without leading zero

df <- read_csv('resources_troph-cost/economic/optimisations/ALB/EP_all_measures_scores_ALB_cost_indicators.csv')

df <- df %>%
  mutate(site_id = epid %>%
           str_remove(., "(?<=G)0"),
         .after = epid)

write_excel_csv(df, 'data/processed/EP_all_measures_scores_ALB_cost_indicators.csv')





## 15.2 SCH -------------------------------------------------------------------

library(tidyverse)

## ---- read in and tidy data

# read big table with all scores and measures 

all_measures_sch <-
  read_delim("data/processed/EP_all_measures_scores_SCH_cost_indicators.csv") %>% 
  rename_with(tolower, .cols = everything()) %>% 
  relocate(region, .before = measure_id) %>% 
  relocate(epid, id, .after = region) %>% 
  select(-n_red_list_lepi_pa, -n_red_list_plant_pa)

str(all_measures_sch)

### 15.2.1 Unconstrained optimisations for single goals -----------------------

# list of paths for unconstrained optimisations

#' [NOTE]: check the length of "paths" vector every time you load it, this should match the number of opti files in your local folder, otherwise adapt the pattern = "^Result_" to recognise different files. This uses strings and regular expresions. 

paths <-
  file.path(
    "resources_troph-cost/economic/optimisations/SCH/indicators",
    list.files(
      "resources_troph-cost/economic/optimisations/SCH/indicators",
      pattern = "^Result_"
    )
  )

# create a list with all optimisations

optis_list <-
  paths %>%
  set_names(
    basename(.) %>%
      str_remove_all(., "Result_") %>%
      str_remove_all(., ".csv") %>%
      str_remove(., "_(\\d*)$|^(\\d*)_") %>%
      str_replace_all(., " \\* ", "_")
  ) %>% 
  
  # loop over each file
  map(
    ~ read.csv(.x, sep = ";", dec = ",") %>%
      as_tibble() %>%
      select(-X) %>%
      
      # clean names
      rename_with(.fn = tolower, .cols = everything()) %>%
      rename_with(~ gsub("x.", "", .x, fixed = TRUE)) %>%
      rename_with(~ gsub(".", "", .x, fixed = TRUE)) %>%
      rename(measure_id = matches("MID"),
             epid = matches("FID")) %>%
      
      # add the file name as a col name and extract budget and indicator name
      mutate(optimisation =
               basename(.x) %>%
               str_remove(., "Result_") %>%
               str_remove(., ".csv") %>%
               str_remove(., "^\\d*_") %>% 
               str_remove(., "_(\\d*)$") %>% 
               str_replace_all(., " \\+ ", "_"), 
             .before = epid) %>%
      
      # create a variable with the budget (= sum of costs across all sites)
      mutate(budget = sum(totalcost, na.rm = TRUE) %>% 
               ceiling(.), .after = totalcost) %>% 
      
      # merge the files with all measures with each optimisation in the list
      inner_join(all_measures_sch, .,
                 by = c("epid", "measure_id")) %>% 
      rename(cost = totalcost)) 

## ---- save all individual merged files

walk2(optis_list, names(optis_list), ~ {
  file_name <- paste0("resources_troph-cost/economic/optimisations/SCH/indicators",
                      "/merged_unconstrained-", .y, ".csv")
  write_csv(.x, file_name)
})


## ---- combine all files into one

optis <-
  optis_list %>%
  list_rbind() %>%
  # add a unique identifier to each row
  unite(
    optimisation,
    epid,
    col = "id",
    sep = "_",
    remove = FALSE,
    na.rm = FALSE
  ) %>%
  relocate(id, .before = region) %>% 
  filter(ecovalue != 0) %>% 
  mutate(budget = sum(cost, na.rm = TRUE) %>% 
           ceiling(.),
         .by = optimisation) 

# save the combined long file 

write_csv(optis, "data/processed/single_ecol_sch.csv")



### 15.2.2 Constrained optimisations for a 2505€ budget  ----------------------

## ---- read each optimization file 

# list of paths for constrained optimisations for a 2505€ budget 
paths_2505 <- 
  file.path("resources_troph-cost/economic/optimisations/SCH/indicators_budget2505",
            list.files("resources_troph-cost/economic/optimisations/SCH/indicators_budget2505", pattern = "^Result"))


optis_list_2505 <-
  paths_2505 %>%
  set_names(
    basename(.) %>%
      str_remove_all(., "Result_|Result(\\d*)_") %>%
      str_remove_all(., ".csv") %>%
      str_remove(., "_(\\d*)$|^(\\d*)_") %>%
      str_replace_all(., " \\* ", "_")
  ) %>% 
  
  # loop over each file
  map(
    ~ read.csv(.x, sep = ";", dec = ",") %>%
      as_tibble() %>%
      select(-X) %>%
      
      # clean names
      rename_with(.fn = tolower, .cols = everything()) %>%
      rename_with(~ gsub("x.", "", .x, fixed = TRUE)) %>%
      rename_with(~ gsub(".", "", .x, fixed = TRUE)) %>%
      rename(measure_id = matches("MID"),
             epid = matches("FID")) %>%
      
      # add the file name as a col name and extract budget and indicator name
      mutate(optimisation =
               basename(.x) %>%
               str_remove(., "Result_|Result(\\d*)_") %>%
               str_remove(., ".csv") %>%
               str_remove(., "^\\d*_") %>% 
               str_remove(., "_(\\d*)$") %>% 
               str_replace_all(., " \\+ ", "_"), 
             .before = epid) %>%
      
      # create a variable with the budget (= sum of costs across all sites)
      mutate(budget = sum(totalcost, na.rm = TRUE) %>% 
               ceiling(.), .after = totalcost) %>% 
      
      # merge the files with all measures with each optimisation in the list
      inner_join(all_measures_sch, .,
                 by = c("epid", "measure_id")) %>% 
      rename(cost = totalcost)) 

## ---- save all individual merged files

walk2(optis_list_2505, names(optis_list_2505), ~ {
  file_name <- paste0("resources_troph-cost/economic/optimisations/SCH/indicators_budget2505", "/merged-2505-", .y, ".csv")
  write_csv(.x, file_name)
})


## ---- combine all files into one

optis_2505 <- 
  optis_list_2505 %>%
  list_rbind() %>%
  # add a unique identifier to each row
  unite(
    optimisation,
    epid,
    col = "id",
    sep = "_",
    remove = FALSE,
    na.rm = FALSE
  ) %>%
  relocate(id, .before = region) %>% 
  filter(ecovalue != 0) %>% 
  mutate(budget = sum(cost, na.rm = TRUE) %>% 
           ceiling(.),
         .by = optimisation) 

# save the combined long file 

write_csv(optis_2505, "data/processed/single_ecol_econ_2505_sch.csv")




### 15.2.3 Constrained optimisations for a 2942€ budget  ----------------------

## ---- read each optimization file 

# list of paths for constrained optimisations for a 2942€ budget 
paths_2942 <-
  file.path(
    "resources_troph-cost/economic/optimisations/SCH/indicators_budget2942",
    list.files(
      "resources_troph-cost/economic/optimisations/SCH/indicators_budget2942",
      pattern = "^Result"
    )
  )


optis_list_2942 <-
  paths_2942 %>%
  set_names(
    basename(.) %>%
      str_remove_all(., "Result_|Result(\\d*)_") %>%
      str_remove_all(., ".csv") %>%
      str_remove(., "_(\\d*)$|^(\\d*)_") %>%
      str_replace_all(., " \\* ", "_")
  ) %>% 
  
  # loop over each file
  map(
    ~ read.csv(.x, sep = ";", dec = ",") %>%
      as_tibble() %>%
      select(-X) %>%
      
      # clean names
      rename_with(.fn = tolower, .cols = everything()) %>%
      rename_with(~ gsub("x.", "", .x, fixed = TRUE)) %>%
      rename_with(~ gsub(".", "", .x, fixed = TRUE)) %>%
      rename(measure_id = matches("MID"),
             epid = matches("FID")) %>%
      
      # add the file name as a col name and extract budget and indicator name
      mutate(optimisation =
               basename(.x) %>%
               str_remove(., "Result_|Result(\\d*)_") %>%
               str_remove(., ".csv") %>%
               str_remove(., "^\\d*_") %>% 
               str_remove(., "_(\\d*)$") %>% 
               str_replace_all(., " \\+ ", "_"), 
             .before = epid) %>%
      
      # create a variable with the budget (= sum of costs across all sites)
      mutate(budget = sum(totalcost, na.rm = TRUE) %>% 
               ceiling(.), .after = totalcost) %>%  
      
      # merge the files with all measures with each optimisation in the list
      inner_join(all_measures_sch, .,
                 by = c("epid", "measure_id")) %>% 
      rename(cost = totalcost)) 

## ---- save all individual merged files

walk2(optis_list_2942, names(optis_list_2942), ~ {
  file_name <- paste0("resources_troph-cost/economic/optimisations/SCH/indicators_budget2942", "/merged-2942-", .y, ".csv")
  write_csv(.x, file_name)
})


## ---- combine all files into one

optis_2942 <- 
  optis_list_2942 %>%
  list_rbind() %>%
  # add a unique identifier to each row
  unite(
    optimisation,
    epid,
    col = "id",
    sep = "_",
    remove = FALSE,
    na.rm = FALSE
  ) %>%
  relocate(id, .before = region) %>% 
  filter(ecovalue != 0) %>% 
  mutate(budget = sum(cost, na.rm = TRUE) %>% 
           ceiling(.),
         .by = optimisation) 

# save the combined long file 

write_csv(optis_2942, "data/processed/single_ecol_econ_2942_sch.csv")




### 15.2.4 Unconstrained optimisations for scenarios -------------------------

# joint goals = scenarios

## ---- read each optimization file 

# list of paths for unconstrained optimisations

paths_scenarios <-
  file.path(
    "resources_troph-cost/economic/optimisations/SCH/scenarios",
    list.files(
      "resources_troph-cost/economic/optimisations/SCH/scenarios",
      pattern = "^Result_"
    )
  )

# create a list with all optimisations

scenarios_list <-
  paths_scenarios %>%
  set_names(
    basename(.) %>%
      str_remove_all(., "Result_|Result(\\d*)_") %>%
      str_remove_all(., ".csv") %>%
      str_remove(., "_(\\d*)$|^(\\d*)_") %>%
      str_replace_all(., " \\* ", "_") %>% 
      str_replace_all(., " \\+ ", "_")
  ) %>% 

  # loop over each file
  map(
    ~ read.csv(.x, sep = ";", dec = ",") %>%
      as_tibble() %>%
      select(-X) %>%
      
      # clean names
      rename_with(.fn = tolower, .cols = everything()) %>%
      rename_with(~ gsub("x.", "", .x, fixed = TRUE)) %>%
      rename_with(~ gsub(".", "", .x, fixed = TRUE)) %>%
      rename(measure_id = matches("MID"),
             epid = matches("FID")) %>%
      
      # add the file name as a col name and extract budget and indicator name
      mutate(optimisation =
               basename(.x) %>%
               str_remove(., "Result_|Result(\\d*)_") %>%
               str_remove(., ".csv") %>%
               str_remove(., "^\\d*_") %>% 
               str_remove(., "_(\\d*)$") %>% 
               str_replace_all(., " \\+ ", "_"), 
             .before = epid) %>%
      
      # create a variable with the budget (= sum of costs across all sites)
      mutate(budget = sum(totalcost, na.rm = TRUE) %>% 
               ceiling(.), .after = totalcost) %>%  
      
      # merge the files with all measures with each optimisation in the list
      inner_join(all_measures_sch, .,
                 by = c("epid", "measure_id")) %>% 
      rename(cost = totalcost)) 

## ---- save all individual merged files

walk2(scenarios_list, names(scenarios_list), ~ {
  file_name <- paste0("resources_troph-cost/economic/optimisations/SCH/scenarios",
                      "/merged_scenario_unconstrained-", .y, ".csv")
  write_csv(.x, file_name)
})


## ---- combine all files into one

optis_scenarios <-
  scenarios_list %>%
  list_rbind() %>%
  # add a unique identifier to each row
  unite(
    optimisation,
    epid,
    col = "id",
    sep = "_",
    remove = FALSE,
    na.rm = FALSE
  ) %>%
  relocate(id, .before = region) %>% 
  filter(ecovalue != 0) %>% 
  mutate(budget = sum(cost, na.rm = TRUE) %>% 
           ceiling(.),
         .by = optimisation) 

# save the combined long file 

write_csv(optis_scenarios, "data/processed/joint_ecol_sch.csv")



### 15.2.5 Optimisations scenarios for a 2505€ budget --------------------------

## ---- read each optimization file 

# list of paths for unconstrained optimisations

paths_scenarios_2505 <-
  file.path(
    "resources_troph-cost/economic/optimisations/SCH/scenarios_budget2505",
    list.files(
      "resources_troph-cost/economic/optimisations/SCH/scenarios_budget2505",
      pattern = "^Result"
    )
  )

# create a list with all optimisations

scenarios_list_2505 <-
  paths_scenarios_2505 %>%
  set_names(
    basename(.) %>%
      str_remove_all(., "Result_|Result(\\d*)_") %>%
      str_remove_all(., ".csv") %>%
      str_remove(., "_(\\d*)$|^(\\d*)_") %>%
      str_replace_all(., " \\* ", "_") %>% 
      str_replace_all(., " \\+ ", "_")
  ) %>% 
  
  # loop over each file
  map(
    ~ read.csv(.x, sep = ";", dec = ",") %>%
      as_tibble() %>%
      select(-X) %>%
      
      # clean names
      rename_with(.fn = tolower, .cols = everything()) %>%
      rename_with(~ gsub("x.", "", .x, fixed = TRUE)) %>%
      rename_with(~ gsub(".", "", .x, fixed = TRUE)) %>%
      rename(measure_id = matches("MID"),
             epid = matches("FID")) %>%
      
      # add the file name as a col name and extract budget and indicator name
      mutate(optimisation =
               basename(.x) %>%
               str_remove(., "Result_|Result(\\d*)_") %>%
               str_remove(., ".csv") %>%
               str_remove(., "^\\d*_") %>% 
               str_remove(., "_(\\d*)$") %>% 
               str_replace_all(., " \\+ ", "_"), 
             .before = epid) %>%
      
      # create a variable with the budget (= sum of costs across all sites)
      mutate(budget = sum(totalcost, na.rm = TRUE) %>% 
               ceiling(.), .after = totalcost) %>% 
      
      # merge the files with all measures with each optimisation in the list
      inner_join(all_measures_sch, .,
                 by = c("epid", "measure_id")) %>% 
      rename(cost = totalcost)) 


## ---- save all individual merged files

walk2(scenarios_list_2505, names(scenarios_list_2505), ~ {
  file_name <- paste0("resources_troph-cost/economic/optimisations/SCH/scenarios_budget2505", "/merged_scenario_2505-", .y, ".csv")
  write_csv(.x, file_name)
})


## ---- combine all files into one

optis_scenarios_2505 <-
  scenarios_list_2505 %>%
  list_rbind() %>%
  # add a unique identifier to each row
  unite(
    optimisation,
    epid,
    col = "id",
    sep = "_",
    remove = FALSE,
    na.rm = FALSE
  ) %>%
  relocate(id, .before = region) %>% 
  filter(ecovalue != 0) %>% 
  mutate(budget = sum(cost, na.rm = TRUE) %>% 
           ceiling(.),
         .by = optimisation) 

# save the combined long file 

write_csv(optis_scenarios_2505, "data/processed/joint_ecol_econ_2505_sch.csv")




### 15.2.6 Optimisations scenarios for a 2942 € budget -----------------------

## ---- read each optimization file 

# list of paths for unconstrained optimisations

paths_scenarios_2942 <-
  file.path(
    "resources_troph-cost/economic/optimisations/SCH/scenarios_budget2942",
    list.files(
      "resources_troph-cost/economic/optimisations/SCH/scenarios_budget2942",
      pattern = "^Result"
    )
  )

# create a list with all optimisations

scenarios_list_2942 <-
  paths_scenarios_2942 %>%
  set_names(
    basename(.) %>%
      str_remove_all(., "Result_|Result(\\d*)_") %>%
      str_remove_all(., ".csv") %>%
      str_remove(., "_(\\d*)$|^(\\d*)_") %>%
      str_replace_all(., " \\* ", "_") %>% 
      str_replace_all(., " \\+ ", "_")
  ) %>% 
  
  # loop over each file
  map(
    ~ read.csv(.x, sep = ";", dec = ",") %>%
      as_tibble() %>%
      select(-X) %>%
      
      # clean names
      rename_with(.fn = tolower, .cols = everything()) %>%
      rename_with(~ gsub("x.", "", .x, fixed = TRUE)) %>%
      rename_with(~ gsub(".", "", .x, fixed = TRUE)) %>%
      rename(measure_id = matches("MID"),
             epid = matches("FID")) %>%
      
      # add the file name as a col name and extract budget and indicator name
      mutate(optimisation =
               basename(.x) %>%
               str_remove(., "Result_|Result(\\d*)_") %>%
               str_remove(., ".csv") %>%
               str_remove(., "^\\d*_") %>% 
               str_remove(., "_(\\d*)$") %>% 
               str_replace_all(., " \\+ ", "_"), 
             .before = epid) %>%
      
      # create a variable with the budget (= sum of costs across all sites)
      mutate(budget = sum(totalcost, na.rm = TRUE) %>% 
               ceiling(.), .after = totalcost) %>% 
      
      # merge the files with all measures with each optimisation in the list
      inner_join(all_measures_sch, .,
                 by = c("epid", "measure_id")) %>% 
      rename(cost = totalcost)) 


## ---- save all individual merged files

walk2(scenarios_list_2942, names(scenarios_list_2942), ~ {
  file_name <- paste0("resources_troph-cost/economic/optimisations/SCH/scenarios_budget2942", "/merged_scenario_2942-", .y, ".csv")
  write_csv(.x, file_name)
})


## ---- combine all files into one

optis_scenarios_2942 <-
  scenarios_list_2942 %>%
  list_rbind() %>%
  # add a unique identifier to each row
  unite(
    optimisation,
    epid,
    col = "id",
    sep = "_",
    remove = FALSE,
    na.rm = FALSE
  ) %>%
  relocate(id, .before = region) %>% 
  filter(ecovalue != 0) %>% 
  mutate(budget = sum(cost, na.rm = TRUE) %>% 
           ceiling(.),
         .by = optimisation) 

# save the combined long file 

write_csv(optis_scenarios_2942, "data/processed/joint_ecol_econ_2942_sch.csv")




### 15.2.7 Merging single files -----------------------------------------------

# this section can be read just one time because after reading it, we add two cols which are then used to wrangle the files afterwards 

## ---- Read in files

# individual optimisation files for merging into a single file as in ID 31798

list(
  optis_sch = read_csv("data/processed/single_ecol_sch.csv"),
  optis_2942_sch = read_csv("data/processed/single_ecol_econ_2942_sch.csv"),
  optis_2505_sch = read_csv("data/processed/single_ecol_econ_2505_sch.csv"),
  optis_scenarios_sch = read_csv("data/processed/joint_ecol_sch.csv"),
  optis_scenarios_2942_sch = read_csv("data/processed/joint_ecol_econ_2942_sch.csv"),
  optis_scenarios_2505_sch = read_csv("data/processed/joint_ecol_econ_2505_sch.csv")
) %>% 
  map(
    ~ .x %>%
      # set col names to lower case 
      rename_with(., ~tolower(.)) %>% 
      # set names of list elements 
      set_names(
        names(.)) %>% 
      as_tibble()) %>% 
  # assign to global environment 
  list2env(., envir = .GlobalEnv)


## ---- data wrangling

# List of dataframes

data_list <- list(
  optis_sch,
  optis_2942_sch,
  optis_2505_sch,
  optis_scenarios_sch,
  optis_scenarios_2942_sch,
  optis_scenarios_2505_sch
)

# Vector of opti_type values to recycle

opti_types <- c("ecol", "ecol_econ_2942", "ecol_econ_2505")

# Piped statement to add columns and bind rows

final_df <- data_list %>%
  imap(~ .x %>% 
         # add goal type determining the value based on the index of list elements
         mutate(goal_type = ifelse(.y <= 3, "single", "joint"), 
                .before = region) %>%
         # add opti_type, recycle values based on the index of list elements: ".y" is the index of the current dataframe in the list; "(.y - 1)" adjusts the index to start from 0 (because R is 1-indexed, but modulo operation is easier to understand with 0-indexing); "%% length(opti_types)" calculates the remainder when the adjusted index is divided by the length of opti_types (which is 3); "+ 1": Adjusts the result back to 1-indexing for correct vector indexing in R
         mutate(opti_type = opti_types[(.y - 1) %% length(opti_types) + 1],
                .after = goal_type)) %>%
  bind_rows() 

# Inspect the final dataframe

final_df %>% View()

# test that the operation is correct

all.equal(
  final_df %>% 
    filter(goal_type == "single" & opti_type == "ecol_econ_2942") %>% 
    select(-c(goal_type, opti_type)),
  optis_2942_sch
)

all.equal(
  final_df %>% 
    filter(goal_type == "joint" & opti_type == "ecol_econ_2505") %>% 
    select(-c(goal_type, opti_type)),
  optis_scenarios_2505_sch
)

## ---- save the final dataset 

write_excel_csv(final_df, 'data/processed/all_optimisations_sch.csv')


## ---- UPDATE: add a column with the EPIDs without leading zero 

final_df <- read_csv('data/processed/all_optimisations_sch.csv')

final_df <-
  final_df %>%
  mutate(site_id = epid %>%
           str_remove(., "(?<=G)0"),
         .after = epid) %>% 
  relocate(site_id, .after = epid) %>% 
  mutate(across(c(n_lepi:sum_troph_int_pa, connectance_pa:sum_crops_pa, ecovalue), 
         \(x) round(x, digits = 5)))

write_excel_csv(final_df, 'data/processed/all_optimisations_sch.csv')


 ## ---- UPDATE: add a column with the EPIDs without leading zero. 

# df <- read_csv('data/processed/EP_all_measures_scores_SCH_cost_indicators.csv')
# size <- read_delim('resources_troph-cost/338774_Vogt_et_at_2019_Landuse-OpenData/338774_Vogt_et_at_2019.csv')
# slope <- read_delim("resources_troph-cost/EPs/20826_6_Dataset_EPs_info/20826_6_data.csv")
# 
# df <-
#   df %>%
#   mutate(site_id = epid %>%
#            str_remove(., "(?<=G)0"),
#          .after = epid) %>% 
#     left_join(
#       size %>% 
#         filter(Year == 2008) %>% 
#         select(PlotID, SizeManagementUnit_ha), 
#       by = c("site_id" = "PlotID")
#     ) %>% 
#     relocate(SizeManagementUnit_ha, .after = n_crop_pests_pa) %>% 
#     left_join(
#       slope %>% 
#         select(EP_PlotID, Slope),
#       by = c("site_id" = "EP_PlotID")
#     ) %>% 
#     relocate(Slope, .after = SizeManagementUnit_ha)
# 
# write_excel_csv(df, 'data/processed/EP_all_measures_scores_SCH_cost_indicators.csv')


