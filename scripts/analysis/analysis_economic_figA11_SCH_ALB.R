#' ----
#' title: script to prepare Fig. A11 with costs of both regions
#' author: Esteban Menares
#' date: 09.10.2024
#' ----

#' [NOTE]: indicator = single conservation goal; scenario = joint conservation goal

# 1. Set up  -------------------------------------------------------------

library(tidyverse)
conflicted::conflicts_prefer(dplyr::filter())
library(ggpubr) # for extra ggplot functionalities
library(ggthemes) # extra themes 

## ---- set plotting theme
theme_set(theme_clean(12))
par(family = "Helvetica")

## ---- Read in data

# optimisations

list(
  optis_sch = read_csv("data/processed/single_ecol_sch.csv"),
  optis_scenarios_sch = read_csv("data/processed/joint_ecol_sch.csv"),
  optis_alb = read_csv("data/processed/single_ecol_alb.csv"),
  optis_scenarios_alb = read_csv("data/processed/joint_ecol_alb.csv")
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



# Fig. A11(a) - Indicators - Overall Cost -------------------------------------

# add panel A costs per plots (add SCH in other color). 

# Overall costs (i.e. budgets) per optimisation across all sites

p_figA11_A <-
  optis_alb %>%
  summarise(sum_cost_alb = sum(cost, na.rm = TRUE),
            .by = optimisation) %>% 
  left_join(
    optis_sch %>% 
      summarise(
        sum_cost_sch = sum(cost, na.rm = TRUE),
        .by = optimisation)) %>% 
  mutate(
    optimisation =
      optimisation %>%
      str_to_sentence() %>%
      str_replace("N_crop_pests", "Crop pest potential") %>%
      str_replace("N_red_list_plant", "Red list plants") %>%
      str_replace("Sum_crops", "Pollination potential") %>%
      str_replace("Z_score_nest", "Nestedness") %>%
      str_replace("N_lepi", "Butterfly richness") %>%
      str_replace("Av_reg_dist_lepi", "Regional distribution butterflies") %>%
      str_replace("Sum_troph_int", "Trophic interactions") %>%
      str_replace("Av_reg_dist_plant", "Regional distribution plants") %>%
      str_replace("N_red_list_lepi", "Red list butterflies") %>%
      str_replace("N_plant", "Plant richness") %>%
      str_replace("N_unique_int", "Unique trophic interactions")
  ) %>%
  # Reorder based on the appearance order in the dataset then reverse for coord_flip()
  arrange(optimisation) %>%
  mutate(optimisation = fct_rev(fct_inorder(optimisation))) %>%
  ggplot() +
  geom_segment(aes(x = optimisation, xend = optimisation, 
                   y = sum_cost_sch, yend = sum_cost_alb), 
               color = "grey25") +
  geom_point(aes(x = optimisation, y = sum_cost_sch), 
             size = 3, shape = 10, color = "steelblue") +
  geom_point(aes(x = optimisation, y = sum_cost_alb), 
             size = 3, shape = 1, color = "chocolate") +
  geom_hline(data = optis_sch %>%
               summarise(sum_cost_sch = sum(cost, na.rm = TRUE),
                         .by = optimisation),
             aes(yintercept = mean(sum_cost_sch, na.rm = TRUE)),
             color = "steelblue", linetype = "dashed") +
  geom_hline(data = optis_alb %>%
               summarise(sum_cost_alb = sum(cost, na.rm = TRUE),
                         .by = optimisation),
             aes(yintercept = mean(sum_cost_alb, na.rm = TRUE)),
             color = "chocolate", linetype = "dotted") +
  coord_flip() +
  labs(x = "Single conservation goal", 
       y = "Overall cost across all sites (EUR)")  +
  theme(plot.title.position = "plot",
        panel.grid.major.y = element_blank())
p_figA11_A

# save plot 

ggsave(
  filename = "figA11_cost_per_opti_across_all_sites.pdf",
  plot = last_plot(),
  path = "output/plots/economic_modelling/",
  dpi = "retina",
  height = 3.5,
  width = 3.5,
  units = "in",
  bg = "white",
  colormodel = "cmyk"
) 

# Fig. A11(b) - Indicators - Mean Cost per region --------------------------------

# ADD panel B: cost per region, divide per number of plots. Add SCH with other color.

p_figA11_B <-
  optis_alb %>%
  summarise(sum_cost_alb = sum(cost, na.rm = TRUE)/
              n(),
            .by = optimisation) %>% 
  left_join(
    optis_sch %>% 
      summarise(
        sum_cost_sch = sum(cost, na.rm = TRUE)/
          n(),
        .by = optimisation)) %>% 
  mutate(
    optimisation =
      optimisation %>%
      str_to_sentence() %>%
      str_replace("N_crop_pests", "Crop pest potential") %>%
      str_replace("N_red_list_plant", "Red list plants") %>%
      str_replace("Sum_crops", "Pollination potential") %>%
      str_replace("Z_score_nest", "Nestedness") %>%
      str_replace("N_lepi", "Butterfly richness") %>%
      str_replace("Av_reg_dist_lepi", "Regional distribution butterflies") %>%
      str_replace("Sum_troph_int", "Trophic interactions") %>%
      str_replace("Av_reg_dist_plant", "Regional distribution plants") %>%
      str_replace("N_red_list_lepi", "Red list butterflies") %>%
      str_replace("N_plant", "Plant richness") %>%
      str_replace("N_unique_int", "Unique trophic interactions")
  ) %>%
  # Reorder based on the appearance order in the dataset then reverse for coord_flip()
  arrange(optimisation) %>%
  mutate(optimisation = fct_rev(fct_inorder(optimisation))) %>%
  ggplot() +
  geom_segment(aes(x = optimisation, xend = optimisation, 
                   y = sum_cost_sch, yend = sum_cost_alb), 
               color = "grey25") +
  geom_point(aes(x = optimisation, y = sum_cost_sch), 
             size = 3, shape = 10, color = "steelblue") +
  geom_point(aes(x = optimisation, y = sum_cost_alb), 
             size = 3, shape = 1, color = "chocolate") +
  geom_hline(data = optis_sch %>%
               summarise(sum_cost_sch = sum(cost, na.rm = TRUE)/
                           n(),
                         .by = optimisation),
             aes(yintercept = mean(sum_cost_sch, na.rm = TRUE)),
             color = "steelblue", linetype = "dashed") +
  geom_hline(data = optis_alb %>%
               summarise(sum_cost_alb = sum(cost, na.rm = TRUE)/
                           n(),
                         .by = optimisation),
             aes(yintercept = mean(sum_cost_alb, na.rm = TRUE)),
             color = "chocolate", linetype = "dotted") +
coord_flip() +
  labs(x = "Single conservation goal", 
       y = "Mean cost per region (EUR/0.25ha)")  +
  theme(plot.title.position = "plot",
        panel.grid.major.y = element_blank())
p_figA11_B

# save plot 

ggsave(
  filename = "figA11B_mean_cost_per_opti_per_region.pdf",
  plot = last_plot(),
  path = "output/plots/economic_modelling/",
  dpi = "retina",
  height = 3.5,
  width = 3.5,
  units = "in",
  bg = "white",
  colormodel = "cmyk"
) 

# Fig. A11(c) - Scenarios - Overall Cost  ---------------------------------------

p_figA11_C <-
  optis_scenarios_alb %>%
  summarise(sum_cost_alb = sum(cost, na.rm = TRUE),
            .by = optimisation) %>% 
  left_join(
    optis_scenarios_sch %>% 
      summarise(
        sum_cost_sch = sum(cost, na.rm = TRUE),
        .by = optimisation)) %>% 
  mutate(
    optimisation =
      optimisation %>%
      str_replace_all("_", "-") %>% 
      str_replace_all("all-scores", "All scores") %>% 
      str_replace_all("low-crop-production", "Low crop production") %>% 
      str_replace_all("medium-crop-production", "Medium crop production")
  ) %>%
  # Reorder based on the appearance order in the dataset then reverse for coord_flip()
  arrange(optimisation) %>%
  mutate(optimisation = fct_rev(fct_inorder(optimisation))) %>%
  ggplot() +
  geom_segment(aes(x = optimisation, xend = optimisation, 
                   y = sum_cost_sch, yend = sum_cost_alb), 
               color = "grey25") +
  geom_point(aes(x = optimisation, y = sum_cost_sch), 
             size = 3, shape = 10, color = "steelblue") +
  geom_point(aes(x = optimisation, y = sum_cost_alb), 
             size = 3, shape = 1, color = "chocolate") +
  geom_hline(data = optis_scenarios_sch %>%
               summarise(sum_cost_sch = sum(cost, na.rm = TRUE),
                         .by = optimisation),
             aes(yintercept = mean(sum_cost_sch, na.rm = TRUE)),
             color = "steelblue", linetype = "dashed") +
  geom_hline(data = optis_scenarios_alb %>%
               summarise(sum_cost_alb = sum(cost, na.rm = TRUE),
                         .by = optimisation),
             aes(yintercept = mean(sum_cost_alb, na.rm = TRUE)),
             color = "chocolate", linetype = "dotted") +
  coord_flip() +
  labs(x = "Joint conservation goal", 
       y = "Overall cost across all sites (EUR)")  +
  theme(plot.title.position = "plot",
        panel.grid.major.y = element_blank())
p_figA11_C

# save plot 

ggsave(
  filename = "figA11C_cost_per_scenarios_across_all_sites.pdf",
  plot = last_plot(),
  path = "output/plots/economic_modelling/",
  dpi = "retina",
  height = 4.5,
  width = 3.5,
  units = "in",
  bg = "white",
  colormodel = "cmyk"
) 

# Fig. A11(d) - Scenarios - Mean Cost per region --------------------------------

p_figA11_D <-
  optis_scenarios_alb %>%
  summarise(sum_cost_alb = sum(cost, na.rm = TRUE)/
              n(),
            .by = optimisation) %>% 
  left_join(
    optis_scenarios_sch %>% 
      summarise(
        sum_cost_sch = sum(cost, na.rm = TRUE)/
          n(),
        .by = optimisation)) %>% 
  mutate(
    optimisation =
      optimisation %>%
      str_replace_all("_", "-") %>% 
      str_replace_all("all-scores", "All scores") %>% 
      str_replace_all("low-crop-production", "Low crop production") %>% 
      str_replace_all("medium-crop-production", "Medium crop production")
  ) %>%
  # Reorder based on the appearance order in the dataset then reverse for coord_flip()
  arrange(optimisation) %>%
  mutate(optimisation = fct_rev(fct_inorder(optimisation))) %>%
  ggplot() +
  geom_segment(aes(x = optimisation, xend = optimisation, 
                   y = sum_cost_sch, yend = sum_cost_alb), 
               color = "grey25") +
  geom_point(aes(x = optimisation, y = sum_cost_sch), 
             size = 3, shape = 10, color = "steelblue") +
  geom_point(aes(x = optimisation, y = sum_cost_alb), 
             size = 3, shape = 1, color = "chocolate") +
  geom_hline(data = optis_scenarios_sch %>%
               summarise(sum_cost_sch = sum(cost, na.rm = TRUE)/
                           n(),
                         .by = optimisation),
             aes(yintercept = mean(sum_cost_sch, na.rm = TRUE)),
             color = "steelblue", linetype = "dashed") +
  geom_hline(data = optis_scenarios_alb %>%
               summarise(sum_cost_alb = sum(cost, na.rm = TRUE)/
                           n(),
                         .by = optimisation),
             aes(yintercept = mean(sum_cost_alb, na.rm = TRUE)),
             color = "chocolate", linetype = "dotted") +
  coord_flip() +
  labs(x = "Joint conservation goal", 
       y = "Mean cost per region (EUR/0.25ha)")  +
  theme(plot.title.position = "plot",
        panel.grid.major.y = element_blank())
p_figA11_D

# save plot 

ggsave(
  filename = "figA11D_mean_cost_per_scenario_per_region.pdf",
  plot = last_plot(),
  path = "output/plots/economic_modelling/",
  dpi = "retina",
  height = 4.5,
  width = 3.5,
  units = "in",
  bg = "white",
  colormodel = "cmyk"
) 


# PLOT ALL TOGETHER

ggpubr::ggarrange(
  p_figA11_A,
  p_figA11_B,
  p_figA11_C,
  p_figA11_D,
  labels = c("(a)", "(b)", "(c)", "(d)"),
  font.label = list(size = 12),
  align = "hv",
  hjust = -0.2,
  vjust = 2.5,
  ncol = 2,
  nrow = 2
)

ggsave(
  filename = "figA11.pdf",
  plot = last_plot(),
  path = "output/plots/economic_modelling/",
  dpi = "retina",
  height = 8,
  width = 7.5,
  units = "in",
  bg = "white",
  colormodel = "cmyk"
) 

# END of Script 
