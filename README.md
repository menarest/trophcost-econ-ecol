# trophcost-econ-ecol
Supplementary code for analyses of Menares, E., Markova-Nenova N., Sturm A., Wätzold F., Birkhofer K. Cost-eﬀective Grassland Conservation for Individual
species, Trophic Interactions, Ecosystem Services, or Ecosystem Resilience: Trade-oﬀs and Synergies. Biological Conservation.
To be submitted.

This repository contains the analysis done for Menares et al. (2025), which adapted an existing optimization procedure to optimize individual and joint conservation goals for butterfly and plant communities in grasslands within two regions in Germany. For all grassland sites, we calculated the costs associated with each management measure and the ecological state measured by multiple attributes. For each region we performed three optimizations: one ecological (budget-unconstrained), and two cost-effectiveness optimizations (budget-constrained) with a low and a medium budget. We compared selected grassland management measures, their costs, and ecological effects for different goals obtained between optimization types and between different goals within the same optimization type.

# Requirements: 

> [!NOTES]
> check that the datasets are named the same as in the data reading steps in the scripts so you can run the analyses with no problem.
> The optimization results were obtained using a C# implementation of the heuristic greedy algorithm in Visual Studio 2019 and a MySQL database tailored to the project's datasets. While the optimization method is transferable, the implementation is tailored to the project. The C# code can be obtained by request from the authors.

- The Quarto document scores_exploration.qmd explores and selects the scores used the optimisations. 
- The scripts analysis_economic_opti_sch.R and analysis_economic_opti_alb.R generate the large bulk of graphs and tables
- The script analysis_economic_fig5_SCH_ALB.R genarates Fig. 5
- The script analysis_economic_figA11_SCH_ALB.R genarates the figure in appendix A11
- If you wish to run data wrangling and prepararion steps, you will need to download some data from BExIS (https://www.bexis.uni-jena.de/) using the IDs: 31780, 31791, 31796, 31798, 31808, 31816, 31930, 31931, 31932. Also see IDs 31817 and 31807 for R scripts to transform results. 
- Some cleaning, wrangling and data preparation steps were done before the analysis and can be found in the scripts:
  - pre-processing_economic_optimisation.R => pre-process scores selected from the exploratory analysis done in  ~/scores_exploration.qmd for economic (cost-benefit) optimisation. This is needed before the optimisation. 
  - undo_minmax_reciprocal_goals.R => undo the min-max scaling and the reciprocal of optimisation results resulting in the original scale values of ecological goals.
  - The script data_tidying.R uses the raw optimisation results to merge them into .csv files and be able to run all above mentioned steps.
