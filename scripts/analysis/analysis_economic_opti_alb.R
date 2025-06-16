#' ----
#' title: analysing cost-effective optimisation results - ALB
#' author: Esteban Menares
#' date: 15.11.2023
#' ----


#' **AIM**: 
#' to analyse and visualize results of the cost-effective optimisation. Visualise trade-off and synergies between each individual scores and also for conservation scenarios. 

#' **NOTE**:
#' ecological optimisation = optimisation for individual goals without budget limit. 
# ecol-econ optimisation = optimisation for individual and scenarios goals with budget limitation

#' **Steps**: 
# 1) without budget limit. What is the cost for maximising each independent indicator values? Check this by ranking the plots by eco value.

# 2) using the minimum budget of all of the budgets as the comparison point. Which ecological value do we reach? we use single indicators, not scenarios. We can do also other values. Do optimisation for constrained budget. 

# 3) using the all_measures table, per indicator, rank the ecological value reached at each optimisation. Use mean, not sum to have values from 0-1, add SD. Create a radar chart graph for each optimisation. In each radar chart show: 
# 3.1) the sum of ecovalue reached in ecol-econ optimization as a % of the maximum sum reachable per indicator
# 3.2) the budget in the ecol-econ optimization as a % of the budget in the ecological optimization per indicator

# 4) look at sets of indicators (scenarios). Run optimisation for scenarios using step 1 and 2. Add to the big table. And visualise using step 3. 

# also...
# - calculate the sum and mean (and median) of each ecological score and the sum of total costs across all EPID for each optimisation. For each score we see how the mean or the sum of score changes in each optimisation. We do the same for the costs separately.
# - calculate and visualize the ratios of the sum of ecological value to the sum of costs per optimisation and region

#' **NOTE**:
#' we ignored the results for crop_pest_potential for the 1344 budget. We optimized all indicators systematically for the two budgets by cost/benefit but if the budget is high enough to optimize only by benefit, that is the result we need to take. So for crop_pest_potential 1344,- that is exactly the budget needed to optimize only the indicator, therefore the cost benefit optimizations makes no sense and gives only a strange result. So we take the ecovalue from the ecol optimization (unconstrained budget) results here and not the cost benefit results (constrained budget). The same holds for unique_trophic_interactions, also here, we just use the ecovalue results from ecol optimization, as the only budget needed is 991,-



# 1. Set up  -------------------------------------------------------------

library(tidyverse)
conflicted::conflicts_prefer(dplyr::filter())
library(ggpubr) # for extra ggplot functionalities
library(GGally) # parallel coordinates plot
library(ggthemes) # extra themes 
library(fmsb) # radar chart graphs

## ---- load source code

source('scripts/source_script.R') # my personal collection of funtions

devtools::source_gist("2a1bb0133ff568cbe28d", 
                      filename = "geom_flat_violin.R")
# sourced from github "dgrtwo/geom_flat_violin.R

## ---- set plotting theme
theme_set(theme_clean(8))
par(family = "Helvetica")

## ---- Read in data

# original (unscaled) indicators (scores)

scores <-
  read_csv("data/processed/scores_economic.csv") %>% 
  as_tibble() %>% 
  # remove the "_pa" ending within the optimisation col
  rename_with(.fn = ~str_replace_all(., "_pa$", ""), 
              .cols = everything()) %>% 
    # Replace names of columns for specific indicators
    rename_with(.fn = ~ str_replace_all(
      .,
      c(
        "\\bconnectance\\b" = "connectance",
        "\\bn_crop_pests\\b" = "crop_pest_potential",
        "\\bn_red_list_plant\\b" = "red_list_plants",
        "\\bsum_crops\\b" = "pollination_potential",
        "\\bz_score_nest\\b" = "nestedness",
        "\\bn_lepi\\b" = "butterfly_richness",
        "\\bav_reg_dist_lepi\\b" = "regional_distribution_butterflies",
        "\\bsum_troph_int\\b" = "trophic_interactions",
        "\\bav_reg_dist_plant\\b" = "regional_distribution_plants",
        "\\bn_red_list_lepi\\b" = "red_list_butterflies",
        "\\bn_plant\\b" = "plant_richness",
        "\\bn_unique_int\\b" = "unique_trophic_interactions"
      )
    ), .cols = everything())

# read in the unscaled table of indicators to test our undo_minmax() function 

# scores_scaled <- 
#   read_csv("data/processed/scores_economic_scaled.csv") %>% 
#   as_tibble() %>% 
#   # remove the "_pa" ending within the optimisation col
#   rename_with(.fn = ~str_replace_all(., "_pa$", ""), 
#               .cols = everything()) 

# optimisations

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
      rename_with(., ~tolower(str_replace_all(., "_pa$", ""))) %>% 
      # remove the "_pa" ending within the optimisation col
      mutate(
        optimisation = optimisation %>% 
          str_replace_all(., "_pa$", "") %>% 
          # add \\b...\\b to look whole word
          str_replace("\\bconnectance\\b", "connectance") %>% 
          str_replace("\\bn_crop_pests\\b", "crop_pest_potential") %>%
          str_replace("\\bn_red_list_plant\\b", "red_list_plants") %>%
          str_replace("\\bsum_crops\\b", "pollination_potential") %>%
          str_replace("\\bz_score_nest\\b", "nestedness") %>%
          str_replace("\\bn_lepi\\b", "butterfly_richness") %>%
          str_replace("\\bav_reg_dist_lepi\\b", "regional_distribution_butterflies") %>%
          str_replace("\\bsum_troph_int\\b", "trophic_interactions") %>%
          str_replace("\\bav_reg_dist_plant\\b", "regional_distribution_plants") %>%
          str_replace("\\bn_red_list_lepi\\b", "red_list_butterflies") %>%
          str_replace("\\bn_plant\\b", "plant_richness") %>% 
          str_replace("\\bn_unique_int\\b", "unique_trophic_interactions")) %>% 
      # arrange alphabetically, set locale to "en" to avoid grouping by case and sort by letter
      arrange(optimisation, .locale = "en") %>% 
      # replace also names of columns indicators
      rename_with(
        ~.x %>% 
          str_replace("\\bconnectance\\b", "connectance") %>%
          str_replace("\\bn_crop_pests\\b", "crop_pest_potential") %>% 
          str_replace("\\bn_red_list_plant\\b", "red_list_plants") %>%
          str_replace("\\bsum_crops\\b", "pollination_potential") %>%
          str_replace("\\bz_score_nest\\b", "nestedness") %>%
          str_replace("\\bn_lepi\\b", "butterfly_richness") %>%
          str_replace("\\bav_reg_dist_lepi\\b", "regional_distribution_butterflies") %>%
          str_replace("\\bsum_troph_int\\b", "trophic_interactions") %>%
          str_replace("\\bav_reg_dist_plant\\b", "regional_distribution_plants") %>%
          str_replace("\\bn_red_list_lepi\\b", "red_list_butterflies") %>%
          str_replace("\\bn_plant\\b", "plant_richness") %>% 
          str_replace("\\bn_unique_int\\b", "unique_trophic_interactions")) %>% 
      # set names of list elements 
      set_names(
        names(.))) %>% 
  # assign to global environment 
  list2env(., envir = .GlobalEnv)

# 2. Data wrangling and visualisation ------------------------------------

## 2.1 Costs -------------------------------------------------------------

## ---- Tbl. with mean + sd, sum, and median ecological value and costs per optimisation across sites

tbl_summary_costs_per_optis <-
  optis %>% 
  summarise(
    
    # ecological value
    mean_ecoval = mean(ecovalue, na.rm = TRUE),
    sd_ecoval = sd(ecovalue, na.rm = TRUE),
    sum_ecoval = sum(ecovalue, na.rm = TRUE),
    median_ecoval = median(ecovalue, na.rm = TRUE),
    
    # total costs
    mean_cost = mean(cost, na.rm = TRUE),
    sd_cost = sd(cost, na.rm = TRUE),
    sum_cost = sum(cost, na.rm = TRUE),
    median_cost = median(cost, na.rm = TRUE),
    
    # ratios 
    ratio_mean = mean_ecoval/mean_cost,
    ratio_sum = sum_ecoval/sum_cost,
    ratio_median = median_ecoval/median_cost,
    
    .by = optimisation
  ) %>%
  mutate(across(where(is.numeric), ~round(.x, digits = 3))) 

# save table 

write.table(tbl_summary_costs_per_optis, 
            "output/plots/economic_modelling/ALB/tbl_summary_costs_per_optis.txt",
            sep = ",",
            quote = FALSE,
            row.names = FALSE)


## ---- Fig. overall costs (i.e. budgets) per optimisation across all sites

optis %>% summarise(sum = sum(ecovalue, na.rm = TRUE), .by = optimisation) %>% 
  summarise(mean = mean(sum))

p_cost <-
  optis %>%
  summarise(sum_cost = sum(cost, na.rm = TRUE) %>% 
              ceiling(.),
            .by = optimisation) %>% 
  mutate(
    optimisation =
      optimisation %>%
      str_replace_all("_", " ") %>%
      str_to_sentence()
  ) %>%
  # reorder variables by sum of cost
  mutate(optimisation = fct_reorder(optimisation, sum_cost)) %>% 
  ggplot(aes(x = optimisation, y = sum_cost)) +
  geom_segment(aes(xend = optimisation, yend = 0)) + 
  geom_point(size = 2, color = "chocolate") +
  geom_hline(data = optis %>%
               summarise(sum_cost = sum(cost, na.rm = TRUE),
                         .by = optimisation),
             aes(yintercept = mean(sum_cost, na.rm = TRUE)),
             color = "black",
             linetype = "dotted") +
  # add labels for values
  geom_text(aes(label = round(sum_cost, digits = 0)),
            stat = "identity",
            nudge_y = 320,
            size = 2,
            colour = "black") +
  coord_flip() +
  labs(x = "Indicator", 
       y = "Overall cost across all sites (euro)",
       # title = "Overall cost across all sites for ecological optimisation of single indicators"
  )  +
  theme(plot.title.position = "plot")
p_cost

# save plot 

ggsave(
  filename = "cost_per_opti_across_all_sites_annotated.pdf",
  plot = last_plot(),
  path = "output/plots/economic_modelling/ALB/",
  dpi = "retina",
  height = 3.5,
  width = 3.5,
  units = "in",
  bg = "white",
  colormodel = "cmyk"
) 

## ---- Fig. total costs (i.e. budgets) per optimisation across all sites

# 1) without budget limit. What is the cost for maximising each independent indicator values? Check this by ranking the plots by eco value.

# ranked by the sum of ecological value 

p_cost_by_sum <- 
  optis %>%
  summarise(sum_cost = unique(budget, na.rm = TRUE),
            sum_ecovalue = sum(ecovalue, na.rm = TRUE),
            .by = optimisation) %>% 
  mutate(
    optimisation =
      optimisation %>%
      str_replace_all("_", " ") %>%
      str_to_sentence()
  ) %>%
  # rank plots by eco value
  mutate(optimisation = fct_reorder(optimisation, sum_ecovalue)) %>% 
  ggplot(aes(x = optimisation, y = sum_cost)) +
  geom_segment(aes(xend = optimisation, yend = 0)) +  
  geom_point(size = 2, color = "chocolate") +
  geom_text(aes(label = round(sum_ecovalue, digits = 1)), 
            stat = "identity", 
            nudge_y = 320,
            size = 2,
            colour = "black") + 
  geom_hline(data = optis %>% 
               summarise(sum_cost = sum(cost, na.rm = TRUE),
                         .by = optimisation), 
             aes(yintercept = mean(sum_cost, na.rm = TRUE)), 
             color = "black",
             linetype = "dotted") +
  coord_flip() +
  labs(x = "Indicators ranked by sum of ecological value", 
       y = "Overall cost across all sites (euro)",
       # title = "Overall cost across all sites for ecological optimisation of single indicators ranked by the sum of ecological value \nacross sites"
  )  +
  theme(plot.title.position = "plot")
p_cost_by_sum

# save plot 

ggsave(
  filename = "cost_per_opti_ranked_by_sum_ecoval.pdf",
  plot = last_plot(),
  path = "output/plots/economic_modelling/ALB/",
  dpi = "retina",
  height = 3.5,
  width = 3.5,
  units = "in",
  bg = "white",
  colormodel = "cmyk"
)


# print both plots together

ggarrange(
  p_cost,
  p_cost_by_sum,
  labels = c("(a)", "(b)"),
  hjust = -0.1,
  ncol = 1,
  nrow = 2,
  common.legend = TRUE,
  font.label = list(size = 10),
  legend = "top",
  align = "hv"
)

ggsave(
  filename = "costs_and_costs_ranked_by_sum_eco_val.pdf",
  plot = last_plot(),
  path = "output/plots/economic_modelling",
  dpi = "retina",
  height = 7,
  width = 3.5,
  units = "in",
  bg = "white",
  colormodel = "cmyk"
)


# ranked by the mean ecological value 

optis %>%
  summarise(sum_cost = unique(budget, na.rm = TRUE),
            mean_ecovalue = mean(ecovalue, na.rm = TRUE),
            .by = optimisation) %>% 
  mutate(
    optimisation =
      optimisation %>%
      str_replace_all("_", " ") %>%
      str_to_sentence() 
  ) %>%
  # rank plots by eco value
  mutate(optimisation = fct_reorder(optimisation, mean_ecovalue)) %>% 
  ggplot(aes(x = optimisation, y = sum_cost)) +
  geom_segment(aes(xend = optimisation, yend = 0)) +  
  geom_point(size = 2, color = "chocolate") +
  geom_text(aes(label = round(mean_ecovalue, digits = 2)), 
            stat = "identity", 
            nudge_y = 280,
            size = 1.8,
            colour = "black") + 
  geom_hline(data = optis %>% 
               summarise(sum_cost = sum(cost, na.rm = TRUE),
                         .by = optimisation), 
             aes(yintercept = mean(sum_cost, na.rm = TRUE)), 
             color = "black",
             linetype = "dotted") +
  coord_flip() +
  labs(x = "Indicator ranked by mean ecological value", 
       y = "Overall cost across all sites (euro)")  +
  theme(plot.title.position = "plot")

# save plot 

ggsave(
  filename = "cost_per_opti_ranked_by_mean_ecoval.pdf",
  plot = last_plot(),
  path = "output/plots/economic_modelling/ALB/",
  dpi = "retina",
  height = 7.5,
  width = 7.5,
  units = "cm",
  bg = "white",
  colormodel = "cmyk"
)


## ---- Fig. costs per plot across optimisations 

optis %>%
  mutate(optimisation = as_factor(optimisation)) %>%
  mutate(
    optimisation =
      optimisation %>%
      str_replace_all("_", " ") %>%
      str_to_sentence() 
  ) %>%
  ggplot(aes(x = optimisation,
             y = cost,
             fill = optimisation)) + 
  
  # add distribution
  geom_flat_violin(scale = "count",
                   trim = TRUE) +
  
  # add mean
  stat_summary(fun = mean,
               # fun.args = list(mult = 2), # mean +/- a constant times the SD
               geom = "pointrange",
               color = "black",
               size = 0.2,
               position = position_nudge(0.05)) + 
  
  # add median
  stat_summary(fun = median,
               color = "black",
               size = 0.2,
               shape = 9,
               position = position_nudge(0.05)) + 
  
  # add raw data
  geom_dotplot(binaxis = "y", 
               dotsize = 10,
               stackdir = "down", 
               binwidth = 0.1,
               position = position_nudge(-0.025)) + 
  coord_cartesian(ylim = c(0, 250), clip = "on") +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) + 
  labs(x = "Optimisation", 
       y = "Cost (euro/0.25 ha)")


# save plot 

ggsave(
  filename = "cost_per_plot_by_opti.pdf",
  plot = last_plot(),
  path = "output/plots/economic_modelling/ALB/",
  dpi = "retina",
  height = 14,
  width = 19,
  units = "cm"
) 


## 2.2 Ecological value ------------------------------------------------------

## ---- Tbl. sum of ecological value across sites per indicator and optimisation

tbl_sum_ecoval_per_indicator <- 
  optis %>% 
  summarise(
    
    # ecological value + total costs
    across(.cols = c(butterfly_richness:crop_pest_potential, cost), 
           .fns = ~ sum(.x),
           .names = "sum_{.col}"),
    .by = optimisation
  ) %>% 
  mutate(
    optimisation =
      optimisation %>%
      str_replace("_", " ") %>%
      str_to_sentence()
  ) %>%
  mutate(across(where(is.numeric), ~round(.x, digits = 3)))

# save table 

write.table(tbl_sum_ecoval_per_indicator, 
            "output/plots/economic_modelling/ALB/tbl_sum_ecoval_per_indicator.txt",
            sep = ",",
            quote = FALSE,
            row.names = FALSE)

## ---- Tbl. ratio of sum of ecological value to sum of costs across sites per indicator and optimisation

tbl_ratio_ecoval_per_indicator <- 
  optis %>% 
  summarise(
    # ecological value + total costs
    across(.cols = c(butterfly_richness:crop_pest_potential, cost), 
           .fns = ~ sum(.x),
           .names = "sum_{.col}"),
    .by = optimisation
  ) %>% 
  summarise(
    # ratio of sum_ecovalue:sum_costs
    across(.cols = c(sum_butterfly_richness:sum_crop_pest_potential), 
           .fns = ~ .x/sum_cost,
           .names = "ratio_{.col}"),
    
    .by = optimisation) %>% 
  mutate(
    optimisation =
      optimisation %>%
      str_to_sentence() %>%
      str_replace("_", " ") %>%
      str_to_sentence()
  ) %>%
  mutate(across(where(is.numeric), ~round(.x, digits = 3)))

# save table

write.table(tbl_ratio_ecoval_per_indicator, 
            "output/plots/economic_modelling/ALB/tbl_ratio_ecoval_per_indicator.txt",
            sep = ",",
            quote = FALSE,
            row.names = FALSE)

# 2) using the minimum budget of all of the budgets as the comparison point. Which ecological value do we reach? we use single indicators, not scenarios. We can do also other values. 

# what is the minimum budget?

optis %>% 
  summarise(
    n_sites = n(),
    total_cost = sum(cost) %>% ceiling(), 
    .by = optimisation) %>% 
  arrange(total_cost)

# concl: the smallest budget (= overall cost per optimisation across all sites) is 991 euros, but this budget allows us to optimise only 5 plots, so we decided to use also the second smallest one, crop_pest_potential = 1344 € >>> Do optimisation for constrained budget using 991 (rounded value) and 1344. This step is done separately in C# programming language and the results are cleaned and loaded in ~/Documents/BTU-PhD/troph-cost/scripts/wrangling/data_tidying.R in 15. ECONOMIC MODELLING. 

## ---- 3.1) the sum of ecovalue reached in ecol-econ optimization using 991€ compared to the max sum reachable per indicator in the ecol optimization

optis %>%
  summarise(
    sum_cost = sum(cost, na.rm = TRUE),
    sum_ecovalue = sum(ecovalue, na.rm = TRUE),
    .by = optimisation) %>% 
  left_join(
    optis_991 %>% 
      summarise(
        sum_cost_991 = sum(cost, na.rm = TRUE),
        sum_ecovalue_991 = sum(ecovalue, na.rm = TRUE),
        .by = optimisation)) %>% 
  mutate(
    optimisation =
      optimisation %>%
      str_to_sentence() %>%
      str_replace_all("_", " ")
  ) %>%
  # rank plots by eco value
  mutate(optimisation = fct_reorder(optimisation, sum_ecovalue)) %>%
  ggplot(.) +
  geom_segment(aes(x = optimisation, xend = optimisation, 
                   y = sum_ecovalue_991, yend = sum_ecovalue), 
               color = "grey") +
  geom_point(aes(x = optimisation, y = sum_ecovalue_991), 
             color = "gray25", size = 3) +
  geom_point(aes(x = optimisation, y = sum_ecovalue), 
             color = "black", size = 3, shape = 1) +
  geom_hline(data = optis %>%
               summarise(sum_ecovalue = sum(ecovalue, na.rm = TRUE),
                         .by = optimisation),
             aes(yintercept = mean(sum_ecovalue, na.rm = TRUE)),
             color = "gray75",
             linetype = "dotted") +
  geom_text(aes(x = optimisation, y = sum_ecovalue,
                label = round(sum_ecovalue, digits = 1)), 
            stat = "identity", 
            nudge_y = 2,
            size = 2,
            colour = "black") +
  geom_text(aes(x = optimisation, y = sum_ecovalue_991, 
                label = round(sum_ecovalue_991, digits = 1)), 
            stat = "identity", 
            nudge_y = -2,
            size = 2,
            colour = "black") +
  coord_flip() +
  labs(x = "Optimisations ranked by max ecological value", 
       y = "Sum of ecological value",
       title = "Total ecovalue across sites using ecol-econ optimisation with 991 euro budget compared to ecol optimisation") +
  theme(plot.title.position = "plot")

# save figure 

ggsave(
  filename = "ecovalue_budget_991.pdf",
  plot = last_plot(),
  path = "output/plots/economic_modelling/ALB/",
  dpi = "retina",
  height = 10,
  width = 7.5,
  units = "cm",
  device = "pdf"
)



## ---- 3.1) the sum of ecovalue reached in ecol-econ optimization using 1344€ compared to the max sum reachable per indicator in the ecol optimization

optis %>%
  summarise(
    sum_cost = sum(cost, na.rm = TRUE),
    sum_ecovalue = sum(ecovalue, na.rm = TRUE),
    .by = optimisation) %>% 
  left_join(
    optis_1344 %>% 
      summarise(
        sum_cost_1344 = sum(cost, na.rm = TRUE),
        sum_ecovalue_1344 = sum(ecovalue, na.rm = TRUE),
        .by = optimisation)) %>% 
  mutate(
    optimisation =
      optimisation %>%
      str_replace_all("_", " ") %>%
      str_to_sentence()
  ) %>%
  # rank plots by eco value
  mutate(optimisation = fct_reorder(optimisation, sum_ecovalue)) %>%
  ggplot(.) +
  geom_segment(aes(x = optimisation, xend = optimisation, 
                   y = sum_ecovalue_1344, yend = sum_ecovalue), 
               color = "grey") +
  geom_point(aes(x = optimisation, y = sum_ecovalue_1344), 
             color = "gray25", size = 3) +
  geom_point(aes(x = optimisation, y = sum_ecovalue), 
             color = "black", size = 3, shape = 1) +
  geom_hline(data = optis %>%
               summarise(sum_ecovalue = sum(ecovalue, na.rm = TRUE),
                         .by = optimisation),
             aes(yintercept = mean(sum_ecovalue, na.rm = TRUE)),
             color = "gray75",
             linetype = "dotted") +
  geom_text(aes(x = optimisation, y = sum_ecovalue,
                label = round(sum_ecovalue, digits = 1)), 
            stat = "identity", 
            nudge_y = 2,
            size = 2,
            colour = "black") +
  geom_text(aes(x = optimisation, y = sum_ecovalue_1344, 
                label = round(sum_ecovalue_1344, digits = 1)), 
            stat = "identity", 
            nudge_y = -2,
            size = 2,
            colour = "black") +
  coord_flip() +
  labs(x = "Optimisations ranked by max ecological value", 
       y = "Sum of ecological value",
       title = "Total ecovalue across sites using ecol-econ optimisation with 1344 euro budget compared to ecol optimisation") +
  theme(plot.title.position = "plot")

# save figure 

ggsave(
  filename = "ecovalue_budget_1344.pdf",
  plot = last_plot(),
  path = "output/plots/economic_modelling/ALB/",
  dpi = "retina",
  height = 10,
  width = 7.5,
  units = "cm"
)



## 2.3 Radar charts ----------------------------------------------------------

# Using the all_measures table, per indicator, rank the ecological value reached at each optimisation. Use mean + SD and sum and present values in a scale from 0-1. Create a radar chart graph for each optimisation. In each radar chart show: 
## a) the sum of ecovalue reached in ecol-econ optimization as a % of the maximum sum reachable in the ecol optimization per indicator 
## b) the budget in the ecol-econ optimization as a % of the budget in the ecological optimization per indicator 


### 2.3.1 Single indicators -------------------------------------------------

#' [NOTE]: This figure was not shown. 

## ---- budget 991

# lets work on a

sum_ecovalues_991 <-
  optis %>%
  
  # join ecovalues from both optimisations
  summarise(
    opti_type = "ecol",
    across(.cols = butterfly_richness:crop_pest_potential,
           .fns = ~ sum(., na.rm = TRUE)),
    .by = optimisation) %>% 
  bind_rows(
    optis_991 %>% 
      summarise(
        opti_type = "ecol-econ",
        across(.cols = butterfly_richness:crop_pest_potential,
               .fns = ~ sum(., na.rm = TRUE)),
        .by = optimisation)) %>% 
  relocate(opti_type, .before = optimisation) %>% 
  
  # turn into long format
  pivot_longer(
    cols = butterfly_richness:last_col(),
    names_to = "indicator",
    values_to = "ecovalue"
  ) %>% 
  
  # Calculate the percentage of ecovalues in df_opt1 relative to df_opt2
  group_by(optimisation, indicator) %>%
  summarise(
    sum_ecol = sum(ecovalue[opti_type == "ecol"]),
    sum_ecol_econ = sum(ecovalue[opti_type == "ecol-econ"]),
    perc_ecoval = (sum_ecol_econ / sum_ecol) * 100,
    .groups = "drop"
  ) %>% 
  select(-c(sum_ecol_econ, sum_ecol)) %>% 
  pivot_wider(
    names_from = "indicator",
    values_from = "perc_ecoval"
  )
sum_ecovalues_991

# work on b

sum_budgets_991 <-
  optis %>%
  summarise(
    opti_type = "ecol",
    budget = sum(cost, na.rm = TRUE),
    .by = optimisation
  ) %>%
  bind_rows(
    optis_991 %>%
      summarise(
        opti_type = "ecol-econ",
        budget = sum(cost, na.rm = TRUE),
        .by = optimisation)
  ) %>%
  relocate(opti_type, .before = optimisation) %>%
  # Calculate the percentage of ecovalues in df_opt1 relative to df_opt2
  summarise(
    sum_ecol = sum(budget[opti_type == "ecol"]),
    sum_ecol_econ = sum(budget[opti_type == "ecol-econ"]),
    perc_budget = (sum_ecol_econ / sum_ecol) * 100,
    .by = optimisation
  ) %>%
  select(-c(sum_ecol, sum_ecol_econ)) %>%
  pivot_wider(
    names_from = "optimisation",
    values_from = "perc_budget"
  ) %>%
  mutate(optimisation = "budget") %>%
  relocate(optimisation, .before = butterfly_richness)
sum_budgets_991

# paste both data frames together

radar_charts_991 <-
  sum_ecovalues_991 %>% 
  left_join(
    sum_budgets_991 %>% 
      select(-optimisation) %>% 
      pivot_longer(
        .,
        cols = butterfly_richness:last_col(),
        names_to = "optimisation",
        values_to = "budget"
      ),
    by = "optimisation"
  ) %>% 
  relocate(budget, .after = optimisation)
radar_charts_991

## ---- Budget 1344 

# lets work on a...

sum_ecovalues_1344 <-
  optis %>%
  
  # join ecovalues from both optimisations
  summarise(
    opti_type = "ecol",
    across(.cols = butterfly_richness:crop_pest_potential,
           .fns = ~ sum(., na.rm = TRUE)),
    .by = optimisation) %>% 
  bind_rows(
    optis_1344 %>% 
      summarise(
        opti_type = "ecol-econ",
        across(.cols = butterfly_richness:crop_pest_potential,
               .fns = ~ sum(., na.rm = TRUE)),
        .by = optimisation)) %>% 
  relocate(opti_type, .before = optimisation) %>% 
  # turn into long format
  pivot_longer(
    cols = butterfly_richness:last_col(),
    names_to = "indicator",
    values_to = "ecovalue"
  ) %>% 
  # Calculate the percentage of ecovalues in df_opt1 relative to df_opt2
  group_by(optimisation, indicator) %>%
  summarise(
    sum_ecol = sum(ecovalue[opti_type == "ecol"]),
    sum_ecol_econ = sum(ecovalue[opti_type == "ecol-econ"]),
    perc_ecoval = (sum_ecol_econ / sum_ecol) * 100,
    .groups = "drop"
  ) %>% 
  select(-c(sum_ecol_econ, sum_ecol)) %>% 
  pivot_wider(
    names_from = "indicator",
    values_from = "perc_ecoval"
  )
sum_ecovalues_1344

# work on b

sum_budgets_1344 <-
  optis %>%
  summarise(
    opti_type = "ecol",
    budget = sum(cost, na.rm = TRUE),
    .by = optimisation
  ) %>%
  bind_rows(
    optis_1344 %>%
      summarise(
        opti_type = "ecol-econ",
        budget = sum(cost, na.rm = TRUE),
        .by = optimisation)
  ) %>%
  relocate(opti_type, .before = optimisation) %>%
  summarise(
    sum_ecol = sum(budget[opti_type == "ecol"]),
    sum_ecol_econ = sum(budget[opti_type == "ecol-econ"]),
    perc_budget = (sum_ecol_econ / sum_ecol) * 100,
    .by = optimisation
  ) %>%
  select(-c(sum_ecol, sum_ecol_econ)) %>%
  pivot_wider(
    names_from = "optimisation",
    values_from = "perc_budget"
  ) %>%
  mutate(optimisation = "budget") %>%
  relocate(optimisation, .before = butterfly_richness)
sum_budgets_1344

# paste both dataframes together

radar_charts_1344 <-
  sum_ecovalues_1344 %>% 
  left_join(
    sum_budgets_1344 %>% 
      select(-optimisation) %>% 
      pivot_longer(
        .,
        cols = butterfly_richness:last_col(),
        names_to = "optimisation",
        values_to = "budget"
      ),
    by = "optimisation"
  ) %>% 
  relocate(budget, .after = optimisation)
radar_charts_1344

## ---- join all data for plotting

# paste both dataframes together

# first check the order of the rows 
radar_charts_991 %>% 
  column_to_rownames("optimisation") %>% 
  rownames() == 
  radar_charts_1344 %>% 
  column_to_rownames("optimisation") %>% 
  rownames()

# bind both datasets

radar_charts_991_1344 <-
  radar_charts_991 %>%
  mutate(optimisation = str_c(optimisation, "_991")) %>%
  bind_rows(radar_charts_1344 %>%
              mutate(optimisation = str_c(optimisation, "_1344"))) %>% 
  # relocate columns by groups of indicators
  relocate(
    optimisation, budget,
    # Traditional species conservation (TSC)
    butterfly_richness, plant_richness, red_list_butterflies, red_list_plants, 
    # Regional distribution range (RDR)
    regional_distribution_butterflies, regional_distribution_plants, 
    # Trophic interactions (TIN)      
    trophic_interactions, unique_trophic_interactions, 
    # Ecosystem resilience (ESR)
    connectance, nestedness,
    # Ecosystem services (ESS) 
    pollination_potential, crop_pest_potential
  )
radar_charts_991_1344

# keep only numeric vars in df for radar chart

radar_charts_num <-
  radar_charts_991_1344 %>% 
  column_to_rownames("optimisation")


## --- create radar chart plots 

# To use the fmsb package, I have to add 2 lines to the dataframe: the max and min of each topic to show on the plot!

# check the min and max 

radar_charts_num %>% 
  summarise(across(everything(), max)) %>%
  bind_rows(
    radar_charts_num %>% 
      summarise(across(everything(), min))) 

# Some goes beyond 100, but we keep anyway 0-100 to make radar charts comparable with each other

radar_charts_num <- rbind(rep(100, 13) , rep(0, 13) , radar_charts_num)
radar_charts_num

# Prepare file names

myfile_names <-
  radar_charts_991_1344 %>% 
  select(optimisation) %>% 
  filter(str_detect(optimisation, "_991"),
         !str_detect(optimisation, "budget")) %>% 
  pull() %>% 
  str_remove("_991")
myfile_names

# prepare plot titles

mytitle <-
  myfile_names %>% 
    str_replace_all("_", " ") %>% 
    str_to_sentence()
mytitle

# prepare variable labels 

# first we check if we could recycle this vector for all radar charts i.e. we check if the cols match between them 

optis %>% colnames()
optis_991 %>% colnames()
optis_1344 %>% colnames()
optis_scenarios %>% colnames()
optis_scenarios_991 %>% colnames()
optis_scenarios_1344 %>% colnames()

all.equal(
  optis %>% select(butterfly_richness:crop_pest_potential) %>% colnames(),
  optis_991 %>% select(butterfly_richness:crop_pest_potential) %>% colnames(),
  optis_1344 %>% select(butterfly_richness:crop_pest_potential) %>% colnames(),
  optis_scenarios %>% select(butterfly_richness:crop_pest_potential) %>% colnames(),
  optis_scenarios_991 %>% select(butterfly_richness:crop_pest_potential) %>% colnames(),
  optis_scenarios_1344 %>% select(butterfly_richness:crop_pest_potential) %>% colnames())

# concl: yes, we can recycle the vector...

vlabels_indicators <-
  radar_charts_num %>% 
  colnames() %>% 
  str_replace("budget", "Budget") %>% 
  str_replace("connectance", "Connectance") %>% 
  str_replace("crop_pest_potential", "Crop pest \npotential") %>% 
  str_replace("red_list_plants", "Red list \nplants") %>% 
  str_replace("pollination_potential", "Pollination \npotential") %>% 
  str_replace("nestedness", "Nestedness") %>% 
  str_replace("butterfly_richness", "Butterfly \nrichness") %>% 
  str_replace("regional_distribution_butterflies", "Regional \ndistribution butterflies") %>% 
  str_replace("\\btrophic_interactions\\b", "Trophic \ninteractions") %>% 
  str_replace("regional_distribution_plants", "Regional \ndistribution plants") %>% 
  str_replace("red_list_butterflies", "Red list \nbutterflies") %>% 
  str_replace("plant_richness", "Plant \nrichness") %>% 
  str_replace("unique_trophic_interactions", "Unique \ntrophic interactions") 
vlabels_indicators

# Loop for each plot
for(i in 1:12) {
  
  # genetate paths for each image
  mypath <-
    file.path("output/plots/economic_modelling/ALB/radar_charts_indicators", 
              paste("radar_chart_indicators_", myfile_names[i], ".pdf", sep = ""))
  
  pdf(file = mypath,
      width = 5.5, 
      height = 4)
  
  # Prepare the screen and set margins
  par(mar = c(1, 1, 2, 1))
  par(mfrow = c(1, 1))
  
  # Custom the radar chart
  radarchart(
    radar_charts_num[c(1:2,2+i, 14+i),], 
    axistype = 1,
    maxmin = TRUE, # row 1 = max, row 2 = min
    
    # custom polygon
    seg = 5, # n of segment per axis
    # pcol = c(hcl.colors(n = 1, palette = "viridis", alpha = 1, rev = T)), # color codes 
    pcol = "black",
    pfcol = NULL, # color codes for filling polygons
    plwd = 0.8, # line widths 
    plty = 1, # line types 
    pty = c(20, 13), # point symbol
    
    # custom the grid
    cglcol = "grey", # Line color for radar grids
    cglty = 1, # Line type for radar grids
    axislabcol = "grey50", # Color of axis label and numbers
    cglwd = 0.6, # Line width for radar grids
    
    # custom labels
    vlcex = 0.6, # font size vlabels
    calcex = 0.5, # font size for caxislabels
    vlabels = vlabels_indicators,
    
    #title
    title = mytitle[i]
  )
  
  dev.off()
}


# Prepare labels for collage

vlabels_indicators_collage <- 
  vlabels_indicators %>% 
  str_replace("Regional \ndistribution butterflies", "Reg. \ndist. butterflies") %>% 
  str_replace("\\bTrophic \ninteractions\\b", "Trophic \nints.") %>% 
  str_replace("Regional \ndistribution plants", "Reg. \ndist. plants") %>% 
  str_replace("Unique \ntrophic interactions", "Unique \ntrophic ints.") 
vlabels_indicators_collage

 ## ---- Radar chart with all 19 plots combined in one image

# assign paths for image
pdf(file = "output/plots/economic_modelling/ALB/radar_charts_indicators/radar_chart_indicators_collage.pdf",
    width = 7.5, 
    height = 9.5)

# Prepare the screen 
par(mar = c(0.5, 0.5, 3, 0.5))
par(mfrow = c(4, 3))

# Loop for each plot
for(i in 1:12) {
  
  # Custom the radar chart
  radarchart(
    radar_charts_num[c(1:2,2+i, 14+i),],
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
    
    # custom the grid
    cglcol = "grey", # Line color for radar grids
    cglty = 1, # Line type for radar grids
    axislabcol = "grey50", # Color of axis label and numbers
    cglwd = 0.6, # Line width for radar grids
    
    # custom labels
    vlcex = 0.85, # font size
    calcex = 0.6, # font size for caxislabels
    vlabels = vlabels_indicators_collage,
    
    #title
    title = mytitle[i]
  )
}

dev.off()


### 2.3.2 Summary radar chart -------------------------------------------------

#' [NOTE]: Figure 2a. 

# create a summary radar chart with all 12 indicators as nodes and values showing the proportions when optimising for that specific indicator. Also with 4 lines in just one radar chart 

# bind both datasets

radar_charts_summary <-
  sum_budgets_991 %>% 
  rename(opti = optimisation) %>%
  mutate(opti = str_c(opti, "_991")) %>%
  bind_rows(sum_budgets_1344 %>%
              rename(opti = optimisation) %>%
              mutate(opti = str_c(opti, "_1344"))
  ) %>% 
  bind_rows(
    radar_charts_991 %>%
      mutate(opti = "ecoval_991", .before = optimisation) %>%
      select(-budget) %>% 
      bind_rows(radar_charts_1344 %>%
                  mutate(opti = "ecoval_1344", .before = optimisation) %>% 
                  select(-budget)) %>% 
      pivot_longer(
        cols = butterfly_richness:last_col(),
        names_to = "indicator",
        values_to = "ecovalue"
      ) %>% 
      # keep ecovalues where the indicator is equal to the optimisation 
      filter(
        optimisation == indicator
      ) %>% 
      select(-optimisation) %>% 
      pivot_wider(
        names_from = "indicator",
        values_from = "ecovalue"
      ) 
  ) %>% 
  # relocate columns by groups of indicators
  relocate(
    opti, 
    # Traditional species conservation (TSC)
    butterfly_richness, plant_richness, red_list_butterflies, red_list_plants, 
    # Regional distribution range (RDR)
    regional_distribution_butterflies, regional_distribution_plants, 
    # Trophic interactions (TIN)      
    trophic_interactions, unique_trophic_interactions, 
    # Ecosystem resilience (ESR)
    connectance, nestedness,
    # Ecosystem services (ESS) 
    pollination_potential, crop_pest_potential
  )
radar_charts_summary

# keep only numeric vars in df for radar chart
radar_charts_num <-
  radar_charts_summary %>% 
  column_to_rownames("opti")

# prepare variable labels 

vlabels_indicators_sum <-
  vlabels_indicators[!vlabels_indicators == "Budget"] 
vlabels_indicators_sum

## --- create radar chart 

# To use the fmsb package, I have to add 2 lines to the dataframe: the max and min of each topic to show on the plot!
radar_charts_num <- rbind(rep(100, 12) , rep(0, 12) , radar_charts_num)
radar_charts_num

pdf(file = "output/plots/economic_modelling/ALB/radar_charts_indicators/radar_chart_summary_indicators.pdf",
    width = 5.5, 
    height = 4)

# Prepare the screen and set margins
par(mar = c(1, 1, 1, 1))
par(mfrow = c(1, 1))

# Custom the radar chart
radarchart(
  radar_charts_num,
  axistype = 1,
  maxmin = TRUE, # row 1 = max, row 2 = min
  
  # custom polygon
  seg = 5, # n of segment per axis
  pcol = c(hcl.colors(n = 2, palette = "viridis", alpha = 1, rev = T)), # color codes 
  pfcol = NULL, # color codes for filling polygons
  plwd = 0.8, # line widths 
  plty = 1 , # line types 
  pty = c(18, 18, 20, 20), # point symbol
  
  # custom the grid
  cglcol = "grey", # Line color for radar grids
  cglty = 1, # Line type for radar grids
  axislabcol = "grey50", # Color of axis label and numbers
  cglwd = 0.6, # Line width for radar grids
  
  # custom labels
  vlcex = 0.7, # font size
  calcex = 0.6, # font size for caxislabels
  vlabels = vlabels_indicators_sum,
)

dev.off()






### 2.3.3 Scenarios  -------------------------------------------------------

#' [NOTE]: This figure was not shown. 

# Create a radar chart graph for each optimisation. In each radar chart show: 
## a) the sum of ecovalue reached in ecol-econ optimization as a % of the maximum sum reachable in the ecol optimization per indicator 
## b) the budget in the ecol-econ optimization as a % of the budget in the ecological optimization per indicator 

# check that all dfs have the same numnber of optimisations 

nrow(optis_scenarios %>% distinct(optimisation))
nrow(optis_scenarios_991 %>% distinct(optimisation))
nrow(optis_scenarios_1344 %>% distinct(optimisation)) 

# OK all have 19 

## ---- prepare data

# lets work on a...

sum_ecovalues_scenarios <-
  optis_scenarios %>%
  # join ecovalues from both optimisations
  summarise(
    opti_type = "ecol",
    across(.cols = butterfly_richness:crop_pest_potential,
           .fns = ~ sum(., na.rm = TRUE)),
    .by = optimisation) %>% 
  bind_rows(
    optis_scenarios_991 %>% 
      summarise(
        opti_type = "ecol-econ-991",
        across(.cols = butterfly_richness:crop_pest_potential,
               .fns = ~ sum(., na.rm = TRUE)),
        .by = optimisation)) %>% 
  bind_rows(
    optis_scenarios_1344 %>% 
      summarise(
        opti_type = "ecol-econ-1344",
        across(.cols = butterfly_richness:crop_pest_potential,
               .fns = ~ sum(., na.rm = TRUE)),
        .by = optimisation)) %>% 
  relocate(opti_type, .before = optimisation) %>% 
  # turn into long format
  pivot_longer(
    cols = butterfly_richness:last_col(),
    names_to = "indicator",
    values_to = "ecovalue"
  ) %>% 
  # Calculate the percentage of ecovalue obtained in df2 and 3 relative to df1
  group_by(optimisation, indicator) %>%
  summarise(
    sum_ecol = sum(ecovalue[opti_type == "ecol"]),
    sum_ecol_econ_991 = sum(ecovalue[opti_type == "ecol-econ-991"]),
    sum_ecol_econ_1344 = sum(ecovalue[opti_type == "ecol-econ-1344"]),
    perc_ecoval_991 = (sum_ecol_econ_991 / sum_ecol) * 100,
    perc_ecoval_1344 = (sum_ecol_econ_1344 / sum_ecol) * 100,
    .groups = "drop"
  ) %>% 
  select(-c(sum_ecol_econ_991, sum_ecol_econ_1344, sum_ecol)) %>% 
  # arrange df for fitting to radar chart format order by budget 
  pivot_longer(
    cols = perc_ecoval_991:perc_ecoval_1344,
    names_to = "opti",
    names_prefix = "perc_ecoval_",
    values_to = "percentage"
  ) %>% 
  arrange(as.numeric(opti), optimisation, 
          # set locale to "en" to avoid grouping by case and sort by letter
          .locale = "en") %>%
  unite(optimisation, opti,
        col = "optimisation",
        sep = "_") %>% 
  pivot_wider(
    names_from = "indicator",
    values_from = "percentage"
  )
sum_ecovalues_scenarios

# work on b...

sum_budgets_scenarios <-
  optis_scenarios %>% 
  summarise(
    opti_type = "ecol",
    budget = sum(cost, na.rm = TRUE),
    .by = optimisation
  ) %>% 
  bind_rows(
    optis_scenarios_991 %>% 
      summarise(
        opti_type = "ecol-econ-991",
        budget = sum(cost, na.rm = TRUE),
        .by = optimisation)
  ) %>% 
  bind_rows(
    optis_scenarios_1344 %>% 
      summarise(
        opti_type = "ecol-econ-1344",
        budget = sum(cost, na.rm = TRUE),
        .by = optimisation)
  ) %>% 
  relocate(opti_type, .before = optimisation) %>% 
  # Calculate the percentage of budget needed in df2 and 3 relative to df1
  summarise(
    sum_ecol = sum(budget[opti_type == "ecol"]),
    sum_ecol_econ_991 = sum(budget[opti_type == "ecol-econ-991"]),
    sum_ecol_econ_1344 = sum(budget[opti_type == "ecol-econ-1344"]),
    perc_budget_991 = (sum_ecol_econ_991 / sum_ecol) * 100,
    perc_budget_1344 = (sum_ecol_econ_1344 / sum_ecol) * 100,
    .by = optimisation
  ) %>% 
  select(-c(sum_ecol_econ_991, sum_ecol_econ_1344, sum_ecol)) %>% 
  # arrange df for fitting to radar chart format order by budget 
  pivot_longer(
    cols = perc_budget_991:perc_budget_1344,
    names_to = "opti",
    names_prefix = "perc_budget_",
    values_to = "percentage"
  ) %>% 
  arrange(rev(opti)) %>% 
  pivot_wider(
    names_from = "optimisation",
    values_from = "percentage"
  ) %>% 
  mutate(opti = str_c("budget", opti, sep = "_"))
sum_budgets_scenarios

# paste both data frames together

radar_charts_scenarios <-
  sum_budgets_scenarios %>% 
  pivot_longer(
    cols = all_scores:last_col(),
    names_to = "optimisation",
    values_to = "percentage"
  ) %>% 
  unite(optimisation, opti, col = "optimisation", sep = "_") %>% 
  mutate(optimisation = str_remove(optimisation, "_budget")) %>% 
  rename(budget = percentage) %>% 
  left_join(., sum_ecovalues_scenarios, by = "optimisation") %>% 
  # relocate columns by groups of indicators
  relocate(
    optimisation, 
    budget, 
    # Traditional species conservation (TSC)
    butterfly_richness, plant_richness, red_list_butterflies, red_list_plants, 
    # Regional distribution range (RDR)
    regional_distribution_butterflies, regional_distribution_plants, 
    # Trophic interactions (TIN)      
    trophic_interactions, unique_trophic_interactions, 
    # Ecosystem resilience (ESR)
    connectance, nestedness,
    # Ecosystem services (ESS) 
    pollination_potential, crop_pest_potential
  )
radar_charts_scenarios

# keep only numeric vars in df for radar chart

radar_charts_num <-
  radar_charts_scenarios %>% 
  column_to_rownames("optimisation")

## --- create radar chart plots 

# Prepare file names

myfile_names <-
  radar_charts_scenarios %>% 
  select(optimisation) %>% 
  filter(str_detect(optimisation, "_991")) %>% 
  pull() %>% 
  str_remove("_991")
myfile_names

# prepare plot titles 

mytitle <-
  myfile_names %>% 
  str_replace_all("_", "-") %>% 
  str_replace_all("all-scores", "All scores") %>% 
  str_replace_all("All scores-ESS25", "All scores ESS 25%") %>% 
  str_replace_all("All scores-ESS50", "All scores ESS 50%")
mytitle

# To use the fmsb package, I have to add 2 lines to the dataframe: the max and min of each var to show on the plot!
radar_charts_num <- rbind(rep(100, 13) , rep(0, 13) , radar_charts_num)

# Loop for each plot
for(i in 1:19) {
  
  # genetate paths for each image
  mypath <-
    file.path("output/plots/economic_modelling/ALB/radar_charts_scenarios", 
              paste("radar_chart_scenarios_", myfile_names[i], ".pdf", sep = ""))
  
  pdf(file = mypath,
      width = 5.5, 
      height = 4)
  
  # Prepare the screen and set margins
  par(mar = c(1, 1, 2, 1))
  par(mfrow = c(1, 1))
  
  # Custom the radar chart
  radarchart(
    radar_charts_num[c(1:2, 2+i, 21+i),],
    axistype = 1,
    maxmin = TRUE, # row 1 = max, row 2 = min
    
    # custom polygon
    seg = 5, # n of segment per axis
    # pcol = c(hcl.colors(n = 2, palette = "viridis", alpha = 1, rev = T)), # color codes 
    pcol = "black",
    pfcol = NULL, # color codes for filling polygons
    plwd = 0.8, # line widths 
    plty = 1 , # line types 
    pty = c(20, 13), # point symbol
    
    # custom the grid 
    cglcol = "grey", # Line color for radar grids
    cglty = 1, # Line type for radar grids
    axislabcol = "grey50", # Color of axis label and numbers
    cglwd = 0.6, # Line width for radar grids
    
    # custom labels
    vlcex = 0.6, # font size
    calcex = 0.5, # font size for caxislabels
    vlabels = vlabels_indicators,
    
    #title
    title = mytitle[i]
  )
  
  dev.off()
}


## ---- Radar chart with all 19 plots combined in one image


# assign paths for image
pdf(file = "output/plots/economic_modelling/ALB/radar_charts_scenarios/radar_chart_scenarios_collage.pdf",
    width = 7.5, 
    height = 9.5)

# Prepare the screen 
par(mar = c(0, 0, 3, 0))
par(mfrow = c(5, 4))

# Loop for each plot
for(i in 1:19) {
  
  # Custom the radar chart
  radarchart(
    radar_charts_num[c(1:2, 2+i, 21+i),],
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
    vlcex = 0.68, # font size
    calcex = 0.6, # font size for caxislabels
    vlabels = vlabels_indicators_collage,
    
    #title
    title = mytitle[i]
  )
}

dev.off()




### 2.3.4 Summary radar chart scenarios  ----------------------------------

#' [NOTE]: Figure 2c. 

## ---- Prepare data

# ecovalues

sum_ecovalues_scenarios_991_1344 <- 
  # join all 3 dfs
  optis_scenarios %>%
  summarise(ecol = sum(ecovalue, na.rm = TRUE),
            .by = optimisation) %>%
  left_join(optis_scenarios_991 %>%
              summarise(
                ecol_econ_991 = sum(ecovalue, na.rm = TRUE),
                .by = optimisation
              )) %>%
  left_join(optis_scenarios_1344 %>%
              summarise(
                ecol_econ_1344 = sum(ecovalue, na.rm = TRUE),
                .by = optimisation
              )) %>%
  
  # calculate percentages
  mutate(
    perc_ecoval_991 = (ecol_econ_991 / ecol) * 100,
    perc_ecoval_1344 = (ecol_econ_1344 / ecol) * 100,
  ) %>%
  select(-c(ecol_econ_991, ecol_econ_1344, ecol)) %>%
  
  # arrange data for radar chart
  pivot_longer(cols = perc_ecoval_991:perc_ecoval_1344,
               names_to = "opti_type",
               values_to = "perc",
               names_prefix = "perc_ecoval_") %>% 
  pivot_wider(
    names_from = "optimisation",
    values_from = "perc"
  ) %>% 
  mutate(
    opti_type = str_c("ecovalue_", opti_type)
  )


sum_budget_scenarios_991_1344 <-
  # join all 3 dfs
  optis_scenarios %>%
  summarise(ecol = sum(cost, na.rm = TRUE),
            .by = optimisation) %>%
  left_join(optis_scenarios_991 %>%
              summarise(
                ecol_econ_991 = sum(cost, na.rm = TRUE),
                .by = optimisation
              )) %>%
  left_join(optis_scenarios_1344 %>%
              summarise(
                ecol_econ_1344 = sum(cost, na.rm = TRUE),
                .by = optimisation
              )) %>%
  
  # calculate percentages
  mutate(
    perc_ecoval_991 = (ecol_econ_991 / ecol) * 100,
    perc_ecoval_1344 = (ecol_econ_1344 / ecol) * 100,
  ) %>%
  select(-c(ecol_econ_991, ecol_econ_1344, ecol)) %>%
  
  # arrange data for radar chart
  pivot_longer(cols = perc_ecoval_991:perc_ecoval_1344,
               names_to = "opti_type",
               values_to = "perc",
               names_prefix = "perc_ecoval_") %>% 
  pivot_wider(
    names_from = "optimisation",
    values_from = "perc"
  ) %>% 
  mutate(
    opti_type = str_c("budget_", opti_type)
  )

# join both df

radar_charts_summary_scenarios <-
  sum_budget_scenarios_991_1344 %>% 
  bind_rows(sum_ecovalues_scenarios_991_1344)

# clean environment 

rm(sum_budget_scenarios_991_1344, sum_ecovalues_scenarios_991_1344)

# keep only numeric vars in df for radar chart

radar_charts_num <-
  radar_charts_summary_scenarios %>% 
  column_to_rownames("opti_type")


## ---- create radar chart 

# To use the fmsb package, I have to add 2 lines to the dataframe: the max and min of each topic to show on the plot!
radar_charts_num <- rbind(rep(100, 19) , rep(0, 19) , radar_charts_num)

# prepare variable labels 
vlabels_scenarios_sum <- 
  radar_charts_num %>% 
  colnames() %>% 
  str_replace_all("_", "-") %>% 
  str_replace_all("all-scores", "All scores") %>% 
  str_replace_all("All scores-ESS25", "All scores \nESS 25%") %>% 
  str_replace_all("All scores-ESS50", "All scores \nESS 50%")
vlabels_scenarios_sum

pdf(file = "output/plots/economic_modelling/ALB/radar_charts_scenarios/radar_chart_summary_scenarios.pdf",
    width = 5.5, 
    height = 4)

# Prepare the screen and set margins
par(mar = c(1, 1, 1, 1))
par(mfrow = c(1, 1))

# Custom the radar chart

radarchart(
  radar_charts_num,
  axistype = 1,
  maxmin = TRUE, # row 1 = max, row 2 = min
  
  # custom polygon
  seg = 5, # n of segment per axis
  pcol = c(hcl.colors(n = 2, palette = "viridis", alpha = 1, rev = T)), # color codes 
  pfcol = NULL, # color codes for filling polygons
  plwd = 0.8, # line widths 
  plty = 1 , # line types 
  pty = c(18, 18, 20, 20), # point symbol
  
  # custom the grid
  cglcol = "grey", # Line color for radar grids
  cglty = 1, # Line type for radar grids
  axislabcol = "grey50", # Color of axis label and numbers
  cglwd = 0.6, # Line width for radar grids
  
  # custom labels
  vlcex = 0.7, # font size
  calcex = 0.6, # font size for caxislabels
  vlabels = vlabels_scenarios_sum,
  
  #title
  # title = "Ecological benefit and overall cost of two budget-constrained optimisations \ncompared to a reference (100%) budget-unconstrained optimisation"
)

dev.off()




### 2.3.5 Synergies/trade-offs radar chart, indicators, budget 991 EUR -------

#' [NOTE]: Figure 3. 
 
# make a collage with trade-offs and synergies, one radar chart per optimization. Compare only ecovalue within one budget (991) of the ecological-economic optimization. As the max reference value (i.e. 100%), use the ecovalue of each indicator reached at the optimization of that specific indicator within the same ecol-econ opti and budget for each node. Add budget lines using the same principle as for ecovalue.

sum_ecovalues_991 <-
  optis_991 %>% 
  # calculate ecovalues for all indicators per optimisation
  summarise(
    across(.cols = butterfly_richness:crop_pest_potential,
           .fns = ~ sum(.)),
    .by = optimisation) %>% 
  bind_rows(
    optis_991 %>%
      summarise(
        maxvalue = sum(ecovalue),
        .by = optimisation) %>% 
      pivot_wider(names_from = optimisation,
                  values_from = maxvalue) %>% 
      mutate(optimisation = "maxvalue")) %>% 
  # sort alphabetically
  arrange(optimisation, 
          # set locale to "en" to avoid grouping by case and sort by letter
          .locale = "en")

# Extract the ecoval maxvalue row as a named vector
max_values <-
  sum_ecovalues_991 %>% 
  filter(optimisation == "maxvalue") %>% 
  select(-optimisation) %>% 
  unlist()

# Perform the division for each column by the corresponding maxvalue
sum_ecovalues_991 <-
  sum_ecovalues_991 %>%
  mutate(across(-optimisation, ~ (. / max_values[cur_column()])*100)) %>% 
  filter(optimisation != "maxvalue")

# check the sum and average ecoval per optimisation (i.e. across all columns)

summary_tbl_indicators <-
  sum_ecovalues_991 %>%
  rowwise() %>% 
  transmute(
    optimisation = optimisation,
    median_ecoval = median(c_across(butterfly_richness:last_col())),
    mean_ecoval = mean(c_across(butterfly_richness:crop_pest_potential))) %>% 
  mutate(across(where(is.numeric), ~round(., digits = 0)))
summary_tbl_indicators

summary_tbl_indicators_sd <-
  sum_ecovalues_991 %>%
  rowwise() %>% 
  transmute(
    optimisation = optimisation,
    median_ecoval = median(c_across(butterfly_richness:crop_pest_potential)),
    mean_ecoval = mean(c_across(butterfly_richness:crop_pest_potential)),
    sd_ecoval = sd(c_across(butterfly_richness:crop_pest_potential))) %>% 
  mutate(across(where(is.numeric), ~round(., digits = 0)))
summary_tbl_indicators_sd


## ---- work on the budgets 

# 1. Extract the maximum used budget per optimisation 
unique_budgets_indicators <- optis_991 %>% 
  distinct(optimisation, budget)

# 2. Create a new dataframe with nrow(unique_budgets) columns, each filled with the budget values

sum_budgets_991 <-
  as.data.frame(matrix(
    data = rep(unique_budgets_indicators$budget, each = nrow(unique_budgets_indicators)),
    nrow = nrow(unique_budgets_indicators),
    byrow = TRUE
  ))

# Name the columns according to optimisations
colnames(sum_budgets_991) <- unique_budgets_indicators$optimisation
rownames(sum_budgets_991) <- unique_budgets_indicators$optimisation

sum_budgets_991 <- sum_budgets_991 %>% 
  rownames_to_column(var = "optimisation") %>% 
  as_tibble()

# check data frame 
sum_budgets_991

# Perform the division of each column by the corresponding maxvalue

# unlist the max values to use in division and across 

vec_unique_budgets <- unique_budgets_indicators %>% 
  pivot_wider(names_from = optimisation, values_from = budget) %>% 
  unlist()

sum_budgets_991_perc <-
  sum_budgets_991 %>%
  mutate(across(-optimisation, ~ (. / vec_unique_budgets[cur_column()])*100)) %>% 
  mutate(optimisation = paste(optimisation, "budget", sep = "_"))
  
sum_budgets_991
sum_budgets_991_perc

# bind both datasets

radar_charts_summary <-
  sum_ecovalues_991 %>% 
  bind_rows(sum_budgets_991_perc) %>% 
  # relocate columns by groups of indicators
  relocate(
    optimisation,
    # Traditional species conservation (TSC)
    butterfly_richness, plant_richness, red_list_butterflies, red_list_plants, 
    # Regional distribution range (RDR)
    regional_distribution_butterflies, regional_distribution_plants, 
    # Trophic interactions (TIN)      
    trophic_interactions, unique_trophic_interactions, 
    # Ecosystem resilience (ESR)
    connectance, nestedness,
    # Ecosystem services (ESS) 
    pollination_potential, crop_pest_potential
  ) 

# keep only numeric vars in df for radar chart

radar_charts_num <-
  radar_charts_summary %>% 
  column_to_rownames("optimisation")


## --- create radar chart plots 

# To use the fmsb package, I have to add 2 lines to the dataframe: the max and min of each topic to show on the plot!

# check the min and max 

radar_charts_num %>% 
  summarise(across(everything(), max)) %>%
  bind_rows(
    radar_charts_num %>% 
      summarise(across(everything(), min))) 

# Some goes beyond 100, but we keep anyway 0-100 to make radar charts comparable with each other

radar_charts_num <- rbind(rep(100, 12) , rep(0, 12) , radar_charts_num)
radar_charts_num

# Prepare file names

myfile_names <-
  sum_ecovalues_991 %>% 
  select(optimisation) %>% 
  pull()
myfile_names

# prepare plot titles (this should match myfile_names)

mytitle <-
  myfile_names %>% 
  str_replace_all("_", " ") %>% 
  str_to_sentence()
mytitle

# Loop for each plot
for(i in 1:12) {
  
  # genetate paths for each image
  mypath <-
    file.path("output/plots/economic_modelling/ALB/radar_charts_indicators/tradeoffs", 
              paste("radar_chart_tradeoffs_indicators_", myfile_names[i], ".pdf", sep = ""))
  
  pdf(file = mypath,
      width = 5.5, 
      height = 4)
  
  # Prepare the screen and set margins
  par(mar = c(1, 1, 2, 1))
  par(mfrow = c(1, 1))
  
  # Custom the radar chart
  radarchart(
    radar_charts_num[c(1:2, 2+i, 14+i),], 
    axistype = 1,
    maxmin = TRUE, # row 1 = max, row 2 = min
    
    # custom polygon
    seg = 5, # n of segment per axis
    # pcol = c(hcl.colors(n = 1, palette = "viridis", alpha = 1, rev = T)), # color codes 
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
    vlcex = 0.6, # font size
    calcex = 0.6, # font size for caxislabels
    vlabels = vlabels_indicators_sum,
    
    #title
    title = mytitle[i]
  )
  
  dev.off()
}

# Prepare labels for trade-offs collage

vlabels_indicators_sum_collage <-
  vlabels_indicators_sum %>% 
  str_replace("Regional \ndistribution butterflies", "Reg. dist. \nbutterflies") %>% 
  str_replace("\\bTrophic \ninteractions\\b", "Trophic ints.") %>% 
  str_replace("Regional \ndistribution plants", "Reg. dist. \nplants") %>% 
  str_replace("Unique \ntrophic interactions", "Unique trophic \nints.") %>% 
  str_replace("Butterfly \nrichness", "Butterfly richness") 
vlabels_indicators_sum_collage

## ---- Radar chart with all 19 plots combined in one image

# assign paths for image
pdf(file = "output/plots/economic_modelling/ALB/radar_charts_indicators/tradeoffs/radar_chart_tradeoffs_indicators_collage.pdf",
    width = 7.5, 
    height = 9.5)

# Prepare the screen 
par(mar = c(0, 0, 2.5, 0))
par(mfrow = c(4, 3))

# Loop for each plot
for(i in 1:12) {
  
  # Custom the radar chart
  radarchart(
    radar_charts_num[c(1:2, 2+i, 14+i),],
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
    vlabels = vlabels_indicators_sum_collage,
    
    #title
    title = mytitle[i]
  )
}

dev.off()



### 2.3.6 Synergies/trade-offs radar chart, scenarios, budget 991 EUR ---------

#' [NOTE]: Figure 5a.

# use the ecovalue of each indicator reached at the optimization of that specific indicator within the same ecol-econ opti and budget as reference for each node. Add budget lines using the same principle as for ecovalue.

sum_ecovalues_scenarios_991 <-
  optis_scenarios_991 %>% 
  # calculate ecovalues for all indicators per optimisation
  summarise(
    across(.cols = butterfly_richness:crop_pest_potential,
           .fns = ~ sum(., na.rm = TRUE)),
    .by = optimisation)
sum_ecovalues_scenarios_991

# Perform the division for each column by the corresponding maxvalue of the single indicator

sum_ecovalues_scenarios_991 <-
  sum_ecovalues_scenarios_991 %>%
  mutate(across(-optimisation, ~ (. / max_values[cur_column()])*100)) %>% 
  filter(optimisation != "maxvalue")
sum_ecovalues_scenarios_991

# check median and average ecoval per optimisation (i.e. across all columns)

summary_tbl_scenarios_ref_single_indicators <- 
  sum_ecovalues_scenarios_991 %>%
  rowwise() %>% 
  transmute(
    optimisation = optimisation,
    median_ecoval = median(c_across(butterfly_richness:last_col())),
    mean_ecoval = mean(c_across(butterfly_richness:crop_pest_potential))) %>% 
  mutate(across(where(is.numeric), ~round(., digits = 0)))
summary_tbl_scenarios_ref_single_indicators

summary_tbl_scenarios_ref_single_indicators_sd <-
  sum_ecovalues_scenarios_991 %>%
  rowwise() %>% 
  transmute(
    optimisation = optimisation,
    median_ecoval = median(c_across(butterfly_richness:crop_pest_potential)),
    mean_ecoval = mean(c_across(butterfly_richness:crop_pest_potential)),
    sd_ecoval = sd(c_across(butterfly_richness:crop_pest_potential))) %>% 
  mutate(across(where(is.numeric), ~round(., digits = 0)))
summary_tbl_scenarios_ref_single_indicators_sd

## ---- work on the budgets 

# we should perform the following operation: a / b, where: 
# a = sum of costs per optimisation of scenarios e.g. all_scores 
# b = sum of costs per optimisation of indicators 

# 1. Extract the maximum used budget per optimisation 
unique_budgets <- optis_scenarios_991 %>% 
  distinct(optimisation, budget)

# 2. Create a new dataframe with rows = number of scenarios and cols = number of indicators, each filled with the budget values

sum_budgets_scenarios_991 <-
  as.data.frame(matrix(
    data = rep(unique_budgets$budget, each = nrow(unique_budgets_indicators)),
    nrow = nrow(unique_budgets),
    byrow = TRUE
  ))

# Name the columns according to optimisations
colnames(sum_budgets_scenarios_991) <- unique_budgets_indicators$optimisation
rownames(sum_budgets_scenarios_991) <- unique_budgets$optimisation

sum_budgets_scenarios_991 <- sum_budgets_scenarios_991 %>% 
  rownames_to_column(var = "optimisation") %>% 
  as_tibble()

# check data frame 
sum_budgets_scenarios_991

# Perform the division of each column by the budget of the single indicator optimisation (i.e. unique_budgets_indicators)

# unlist the max values to use in division and across 

vec_unique_budgets <- unique_budgets_indicators %>% 
  pivot_wider(names_from = optimisation, values_from = budget) %>% 
  unlist()

sum_budgets_scenarios_991_perc <-
  sum_budgets_scenarios_991 %>%
  mutate(across(-optimisation, ~ (. / vec_unique_budgets[cur_column()])*100)) %>% 
  mutate(optimisation = paste(optimisation, "budget", sep = "_"))

sum_budgets_scenarios_991
sum_budgets_scenarios_991_perc

# bind both datasets  and order columns of ecovalues dataset

radar_charts_summary <-
  sum_ecovalues_scenarios_991 %>% 
  bind_rows(sum_budgets_scenarios_991_perc) %>% 
  # relocate columns by groups of indicators
  relocate(
    optimisation,
    # Traditional species conservation (TSC)
    butterfly_richness, plant_richness, red_list_butterflies, red_list_plants, 
    # Regional distribution range (RDR)
    regional_distribution_butterflies, regional_distribution_plants, 
    # Trophic interactions (TIN)      
    trophic_interactions, unique_trophic_interactions, 
    # Ecosystem resilience (ESR)
    connectance, nestedness,
    # Ecosystem services (ESS) 
    pollination_potential, crop_pest_potential
  ) 

# Save dataset to create combined figure with both regions

trade_offs_scenarios_ALB <- radar_charts_summary

saveRDS(trade_offs_scenarios_ALB,
        file = 'data/processed/trade_offs_scenarios_ALB.rds')

# keep only numeric vars in df for radar chart

radar_charts_num <-
  radar_charts_summary %>% 
  column_to_rownames("optimisation")

## --- create radar chart plots 

# To use the fmsb package, I have to add 2 lines to the dataframe: the max and min of each topic to show on the plot!

# check the min and max 

radar_charts_num %>% 
  summarise(across(everything(), max)) %>%
  bind_rows(
    radar_charts_num %>% 
      summarise(across(everything(), min))) 

# add 0-100 to make radar charts comparable with each other

radar_charts_num <- rbind(rep(100, 12) , rep(0, 12) , radar_charts_num)
radar_charts_num

# Prepare file names

myfile_names <-
  sum_ecovalues_scenarios_991 %>% 
  select(optimisation) %>% 
  pull()
myfile_names

# prepare plot titles 

mytitle <-
  myfile_names %>% 
  str_replace_all("_", "-") %>% 
  str_replace_all("\\ball-scores\\b", "All scores") %>% 
  str_replace_all("All scores-ESS25", "All scores ESS 25%") %>% 
  str_replace_all("All scores-ESS50", "All scores ESS 50%")
mytitle

# Loop for each plot
for(i in 1:19) {
  
  # genetate paths for each image
  mypath <-
    file.path("output/plots/economic_modelling/ALB/radar_charts_scenarios/tradeoffs/", 
              paste("radar_chart_scenarios_tradeoffs_", myfile_names[i], ".pdf", sep = ""))
  
  pdf(file = mypath,
      width = 5.5, 
      height = 4)
  
  # Prepare the screen and set margins
  par(mar = c(1, 1, 2, 1))
  par(mfrow = c(1, 1))
  
  # Custom the radar chart
  radarchart(
    radar_charts_num[c(1:2, 2+i, 21+i),],
    axistype = 1,
    maxmin = TRUE, # row 1 = max, row 2 = min
    
    # custom polygon
    seg = 5, # n of segment per axis
    # pcol = c(hcl.colors(n = 2, palette = "viridis", alpha = 1, rev = T)), # color codes 
    pcol = "black",
    pfcol = NULL, # color codes for filling polygons
    plwd = 0.8, # line widths 
    plty = 1 , # line types 
    pty = c(20, 13), # point symbol
    cex = 5,
    
    # custom the grid 
    cglcol = "grey", # Line color for radar grids
    cglty = 1, # Line type for radar grids
    axislabcol = "grey50", # Color of axis label and numbers
    cglwd = 0.6, # Line width for radar grids
    
    # custom labels
    vlcex = 0.6, # font size
    calcex = 0.6, # font size for caxislabels
    vlabels = vlabels_indicators_sum %>% 
      str_replace("Regional \ndistribution butterflies", "Regional \ndistribution \nbutterflies") %>% 
      str_replace("Regional \ndistribution plants", "Regional \ndistribution \nplants"), 
    
    #title
    title = mytitle[i]
  )
  
  dev.off()
}



## ---- Radar chart with all 19 plots combined in one image

# assign paths for image
pdf(file = "output/plots/economic_modelling/ALB/radar_charts_scenarios/tradeoffs/radar_chart_tradeoffs_scenarios_collage.pdf",
    width = 7.5, 
    height = 9.5)

# Prepare the screen 
par(mar = c(0, 0, 2.5, 0))
par(mfrow = c(5, 4))

# Loop for each plot
for(i in 1:19) {
  
  # Custom the radar chart
  radarchart(
    radar_charts_num[c(1:2, 2+i, 21+i),],
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
    vlcex = 0.7, # font size
    calcex = 0.6, # font size for caxislabels
    vlabels = vlabels_indicators_sum_collage,
    
    #title
    title = mytitle[i]
  )
}

dev.off()



## 2.4 Appendix A13 ------------------------------------------------------

# summary table with the median and mean ecovalue reached per optimization across all indicators in the trade-offs/synergies analysis using the single indicators' ecovalue reached when optimizing cost-effectively for that same single indicator 

summary_tbl_indicators_sd %>% 
  bind_rows(summary_tbl_scenarios_ref_single_indicators_sd) %>% 
  mutate(
    optimisation = optimisation %>% 
      str_replace("crop_pest_potential", "Crop pests") %>% 
      str_replace("red_list_plants", "Red list plant") %>% 
      str_replace("pollination_potential", "Pollination potential") %>% 
      str_replace("nestedness", "Nestedness") %>% 
      str_replace("butterfly_richness", "Lepidoptera richness") %>% 
      str_replace("regional_distribution_butterflies", "Regional distribution Lepidoptera") %>% 
      str_replace("\\btrophic_interactions\\b", "Trophic interactions") %>% 
      str_replace("regional_distribution_plants", "Regional distribution plant") %>% 
      str_replace("red_list_butterflies", "Red list Lepidoptera") %>% 
      str_replace("plant_richness", "Plant richness") %>% 
      str_replace("\\bunique_trophic_interactions\\b", "Unique trophic interactions") %>% 
      str_replace_all("_", "-") %>% 
      str_replace_all("all-scores", "All scores") %>%  
      str_replace_all("All scores-ESS25", "All scores ESS 25%") %>% 
      str_replace_all("All scores-ESS50", "All scores ESS 50%")
  ) %>% 
  arrange(desc(mean_ecoval)) %>%
  # save table
  write.table(., 
              "output/plots/economic_modelling/ALB/tbl_mean_median_ecovalue_per_opti_tradeoffs_ALB.txt",
              sep = ",",
              quote = FALSE,
              row.names = FALSE)



## 2.5 Original scale indicator values ---------------------------------------

### 2.5.1 Testing undo min-max scaling function variables per region ---------

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
  
  # Check if column names match between scaled and unscaled data frames
  if (!identical(names(scaled), names(unscaled))) {
    stop("Error: Names in 'scaled' and 'unscaled' data frames do not match.")
  }
  
  # Proceed with the min-max unscaling if names match
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

#' [NOTE] Checking first inverse_minmax function and replacing values by the original ones. These test were run before to check the calculations run without mistakes, but now are commented out. 

## ---- TEST 1 - one variable

# scaled_var <- norm_minmax(scores[scores$region == "ALB",]$butterfly_richness)
# orig_var <- scores[scores$region == "ALB",]$butterfly_richness
# undo_scale <- undo_minmax(scaled = scaled_var, unscaled = orig_var)
# 
# # Prove two are identical
# all.equal(scores[scores$region == "ALB",]$butterfly_richness, undo_scale)
# 
# # check function
# undo_minmax(scaled = scaled_var, unscaled = orig_var)


## ---- TEST 2 - multiple variables

# we do not include variables for which we calculated reciprocal

# test <- scores_scaled %>% 
#   filter(region == "ALB") %>% 
#   mutate(
#     across(
#       c(butterfly_richness,
#         plant_richness,
#         red_list_butterflies,
#         red_list_plants,
#         trophic_interactions,
#         unique_trophic_interactions,
#         connectance,
#         nestedness,
#         pollination_potential),
#       ~ undo_minmax(scaled = .x, 
#                     unscaled = scores[scores$region == "ALB",][[cur_column()]], 
#                     na.rm = TRUE)))

# check results 

# all.equal(test %>% select(butterfly_richness:red_list_plants, trophic_interactions:pollination_potential), 
#           scores %>% filter(region == "ALB") %>% select(butterfly_richness:red_list_plants, trophic_interactions:pollination_potential))
# 
# rm(test)

## ---- TEST 3 - test undo with optimisation results

# test <-
#   optis %>%
#   filter(region == "ALB") %>%
#   mutate(
#     across(
#       c(butterfly_richness,
#         plant_richness,
#         red_list_butterflies,
#         red_list_plants,
#         trophic_interactions,
#         unique_trophic_interactions,
#         connectance,
#         nestedness,
#         pollination_potential),
#       ~ undo_minmax(scaled = .x,
#                     unscaled = scores[scores$region == "ALB",][[cur_column()]],
#                     na.rm = TRUE)))
# 
# all.equal(test %>% select(butterfly_richness:red_list_plants, trophic_interactions:pollination_potential),
#           scores %>% filter(region == "ALB") %>%
#             select(butterfly_richness:red_list_plants, trophic_interactions:pollination_potential))

# It works, only row.names differ due to longer df dims of test... all OK. The function works on the minmax scaled values, now we also first need to revert the indicators for which we calculated reciprocals (regional_distribution_butterflies, regional_distribution_plants) and manually reclassify values of crop_pest_potential. This will revert the changes done on ~/scripts/wrangling/pre-processing_economic_optimisation.R


## ---- TEST 4, undo minmax scaling and reciprocals

# For regional_distribution_butterflies and regional_distribution_plants, the input to the transformation isn't the raw values, but the inverse of the raw values. So we have to undo the transformation differently for those variables than the others.

# test <-
#   scores_scaled %>% 
#   filter(region == "ALB") %>% 
#   
#   # reclassify values of pests manually back to original values
#   mutate(
#     crop_pest_potential = case_when(
#       crop_pest_potential == 1 ~ 0,
#       crop_pest_potential == 0.5 ~ 1,
#       crop_pest_potential == 0 ~ 2)) %>% 
#   
#   # undo minmax scaling
#   mutate(
#     across(
#       c(butterfly_richness,
#         plant_richness,
#         red_list_butterflies,
#         red_list_plants,
#         trophic_interactions,
#         unique_trophic_interactions,
#         connectance,
#         nestedness,
#         pollination_potential,
#         ),
#       ~ undo_minmax(scaled = .x, 
#                     unscaled = scores %>% 
#                       filter(region == "ALB") %>%  
#                       select(cur_column()) %>% pull(), 
#                     na.rm = TRUE)),
#     across(
#       c(regional_distribution_butterflies, regional_distribution_plants), 
#       ~1/undo_minmax(scaled = .x, 
#                      unscaled = scores %>% 
#                        filter(region == "ALB") %>%  
#                        select(cur_column()) %>% 
#                        pull() %>% 
#                        inv(), 
#                      na.rm = TRUE)))

# # calculate reciprocal of reciprocals
# mutate(across(c(regional_distribution_butterflies, regional_distribution_plants), ~ 1/.)) %>%

# # reclassify inf values as 0 resulting from dividing 1/0
# mutate(across(c(regional_distribution_butterflies, regional_distribution_plants),
#               ~ if_else(is.infinite(.), 0, .)))

# all.equal(test, scores %>% filter(region == "ALB"))
# print(test)
# print(scores)

#' [END OF TESTING] ----



### 2.5.2 Undo transformations ------------------------------------------------

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
        crop_pest_potential = case_when(
          crop_pest_potential == 1 ~ 0,
          crop_pest_potential == 0.5 ~ 1,
          crop_pest_potential == 0 ~ 2
        )
      ) %>%
      
      # undo minmax scaling for specific columns
      mutate(
        across(
          c(
            butterfly_richness,
            plant_richness,
            red_list_butterflies,
            red_list_plants,
            trophic_interactions,
            unique_trophic_interactions,
            connectance,
            nestedness,
            pollination_potential 
          ),
        ~ undo_minmax(
          scaled = .x,
          unscaled = scores %>%
            filter(region == "ALB") %>%
            select(cur_column()) %>%
            pull(),
          na.rm = TRUE
        )
      ),
      
      # Apply inverse minmax scaling for specific columns
        across(
          c(regional_distribution_butterflies, regional_distribution_plants),
          ~ 1 / undo_minmax(
            scaled = .x,
            unscaled = scores %>%
              filter(region == "ALB") %>%
              select(cur_column()) %>%
              pull() %>%
              inv(),
            na.rm = TRUE
          )
        )
  )) %>%
  set_names(new_names)
  
# Assign the list elements to the global environment
list2env(my_new_list, envir = .GlobalEnv)



### 2.5.3 Do a table per indicator with original indicator values -------------
# add 100%, ecovalues, budget, n of sites...


tbl_summary_costs_and_ecovalue_per_optis_original_values <-
  new_optis %>%
  summarise(
    "Initial budget (EUR)" = "un",
    "Overall cost (EUR)" = sum(cost, na.rm = TRUE),
    "Number of sites" = n(),
    across(
      butterfly_richness:crop_pest_potential, ~ sum(., na.rm = TRUE)),
    .by = optimisation
  ) %>%
  bind_rows(
    new_optis_1344 %>%
      summarise(
        "Initial budget (EUR)" = "1344",
        "Overall cost (EUR)" = sum(cost, na.rm = TRUE),
        "Number of sites" = n(),
        across(
          butterfly_richness:crop_pest_potential, ~ sum(., na.rm = TRUE)),
        .by = optimisation
      ) 
  ) %>% 
  bind_rows(
    new_optis_991 %>%
      summarise(
        "Initial budget (EUR)" = "991",
        "Overall cost (EUR)" = sum(cost, na.rm = TRUE),
        "Number of sites" = n(),
        across(
          butterfly_richness:crop_pest_potential, ~ sum(., na.rm = TRUE)),
        .by = optimisation
      ) 
  ) %>% 
  bind_rows(
    new_optis_scenarios %>%
      summarise(
        "Initial budget (EUR)" = "un",
        "Overall cost (EUR)" = sum(cost, na.rm = TRUE),
        "Number of sites" = n(),
        across(
          butterfly_richness:crop_pest_potential, ~ sum(., na.rm = TRUE)),
        .by = optimisation
      ) 
  ) %>% 
  bind_rows(
    new_optis_scenarios_1344 %>%
      summarise(
        "Initial budget (EUR)" = "1344",
        "Overall cost (EUR)" = sum(cost, na.rm = TRUE),
        "Number of sites" = n(),
        across(
          butterfly_richness:crop_pest_potential, ~ sum(., na.rm = TRUE)),
        .by = optimisation
      ) 
  ) %>% 
  bind_rows(
    new_optis_scenarios_991 %>%
      summarise(
        "Initial budget (EUR)" = "991",
        "Overall cost (EUR)" = sum(cost, na.rm = TRUE),
        "Number of sites" = n(),
        across(
          butterfly_richness:crop_pest_potential, ~ sum(., na.rm = TRUE)),
        .by = optimisation
      ) 
  ) %>% 
  mutate(`Overall cost (EUR)` = ceiling(`Overall cost (EUR)`)) %>% 
  rename(Optimisation = optimisation,
         Connectance = connectance,
         `Crop pest potential` = crop_pest_potential,
         "Red list plants" = red_list_plants,
         "Pollination potential" = pollination_potential,
         Nestedness = nestedness,
         "Butterfly richness" = butterfly_richness,
         "Regional distribution butterflies" = regional_distribution_butterflies,
         "Trophic interactions" = trophic_interactions,
         "Regional distribution plants" = regional_distribution_plants,
         "Red list butterflies" = red_list_butterflies,
         "Plant richness" = plant_richness,
         "Unique trophic interactions" = unique_trophic_interactions
  ) %>% 
  mutate(across(c(`Butterfly richness`:last_col()), ~round(.x, digits = 0))) %>% 
  mutate(
    Optimisation = Optimisation %>% 
      str_replace("crop_pest_potential", "Crop pest potential") %>% 
      str_replace("connectance", "Connectance") %>% 
      str_replace("red_list_plants", "Red list plants") %>%
      str_replace("pollination_potential", "Pollination potential") %>% 
      str_replace("nestedness", "Nestedness") %>% 
      str_replace("butterfly_richness", "Butterfly richness") %>% 
      str_replace("regional_distribution_butterflies", "Regional distribution butterflies") %>% 
      str_replace("\\btrophic_interactions\\b", "Trophic interactions") %>% 
      str_replace("regional_distribution_plants", "Regional distribution plants") %>% 
      str_replace("red_list_butterflies", "Red list butterflies") %>%
      str_replace("plant_richness", "Plant richness") %>% 
      str_replace("\\bunique_trophic_interactions\\b", "Unique trophic interactions") %>% 
      str_replace_all("_", "-") %>% 
      str_replace_all("all-scores", "All scores") %>%  
      str_replace_all("All scores-ESS25", "All scores ESS 25%") %>% 
      str_replace_all("All scores-ESS50", "All scores ESS 50%")
  ) 

# save table 

write.table(tbl_summary_costs_and_ecovalue_per_optis_original_values, 
            "output/plots/economic_modelling/ALB/tbl_summary_costs_and_ecovalue_per_optis_original_values_ALB.txt",
            sep = ",",
            quote = FALSE,
            row.names = FALSE)

