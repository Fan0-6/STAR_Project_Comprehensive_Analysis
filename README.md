# Project STAR Analysis: Grade 1 Math Scores

## Overview
This repository contains an extensive analysis of first-grade math scores from students involved in Project STAR, providing a critical review of the project's design and execution. The analysis identifies key limitations, including strict class size definitions, challenges in maintaining intervention consistency, ineffective randomization procedures, and inadequate control for confounding variables. These elements yield significant insights into the potential constraints of the study's findings.

## Caveats and Heterogeneity
The caveats section delves into missing data and heterogeneity, where biases associated with underrepresented minority groups are examined. Despite their small numbers, the decision to implement median imputation for math scores was made to address these biases. Furthermore, heterogeneity analysis exposes patterns linked to race, urbanicity, and free-lunch status, which inform adjustments to subsequent modeling efforts.

## Descriptive Analysis
The repository includes a descriptive analysis, utilizing univariate statistics, and visualizations of categorical variables to facilitate model selection. This section also investigates relationships between class types, math scores, and school characteristics, aggregating data across classes to discern patterns.

## Inferential Analysis
Two models form the basis of the inferential analysis:
- A mixed-effect two-way ANOVA, using school as a random effect and class type as a fixed effect, underscores the positive implications of smaller class sizes.
- A comprehensive linear model, introducing race, urbanicity, and free-lunch status as covariates, sheds light on the detrimental effects of free-lunch eligibility on math performance.

## Sensitivity Analysis
A sensitivity analysis evaluates the impact of different imputation and aggregation methods and contrasts the performance of the two proposed models. This analysis underscores the robustness of the findings and validates the selection of Model 2 for its superior representation of data and balanced complexity.

## Conclusion
The findings presented in this repository contribute valuable perspectives on how class size and school environment impact student academic performance, offering pointed evaluations for future improvement of the STAR Project.

## Repository Structure
- `RMD scripts/`: Code to compile the study report, documentation of methodology, and findings and outputs from the scripts, including figures and tables..
- `R scripts/`: pure codes.
- `README.md`: Overview and guide to the repository.

## Usage
To replicate the analysis, follow the instructions in the `scripts/` directory.

## License
This project is licensed under the terms of the MIT License. See `LICENSE.md` for details.

## Contact
For any queries or discussions regarding the findings, please open an issue in this repository.
