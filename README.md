# trophcost-econ-ecol
Supplementary code for analyses of Menares et al. 2025, "Cost-effective grassland conservation for individual species, trophic interactions, ecosystem services or ecosystem resilience: trade-offs and synergies."

This repository contains the analysis done for Menares et al. (2025), which adapted an existing optimization procedure to optimize individual and joint conservation goals for butterfly and plant communities in grasslands within two regions in Germany. For all grassland sites, we calculated the costs associated with each management measure and the ecological state measured by multiple attributes. For each region we performed three optimizations: one ecological (budget-unconstrained), and two cost-effectiveness optimizations (budget-constrained) with a low and a medium budget. We compared selected grassland management measures, their costs, and ecological effects for different goals obtained between optimization types and between different goals within the same optimization type.

# Requirements: 

To run the analyses, create graphs and tables use this dataset from # TODO [BExIS: 31738 (v.9)](https://doi.org/10.25829/bexis.31738-9)

To run data wrangling and prepararion steps, you will need to download the data from BExIS (https://www.bexis.uni-jena.de/) using the IDs: 31780, 31791, 31796, 31798, 31808, 31816, 31930, 31931, 31932. Also see IDs 31817 and 31807 for R scripts to transform results. The optimization results were obtained using a C# implementation of the heuristic greedy algorithm in Visual Studio 2019 and a MySQL database tailored to the project's datasets. While the optimization method is transferable, the implementation is tailored to the project. The C# code can be obtained by request from the authors.

> [!NOTES]
> check that the datasets are named the same as in the data reading steps in the scripts so you can run the analyses with no problem.

# TODO... Steps: 
1. run the [scores_exploration.qmd](scripts/wrangling/data_tidying.R) to clean and tidy the datasets
2. continue with the pairs files in order (pairs_1.., 2, 3, and 4)
3. run the files in the [scripts/analysis folder](scripts/analysis) in this order:
   - accuracy_co-occurrence
   - modeling_trophint_cooccur
   - troph_data_validation
   - matrices_cooccur

This should generate all files and plots of the paper and place them into the already folders.
