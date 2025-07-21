---
title: "Predicting Food Insecurity Among Seniors in Iowa PUMAs for WesleyLife (Meals on Wheels)"
author: "Eric Pfaffenbach, Anthony Esboldt, Riley Schultz and Jason Nguyen"
date: "12/12/2024"
output: html_document
---

## Introduction
This repository contains the code and data required to reproduce the results found in "Predicting Food Insecurity Among Seniors in Iowa PUMAs." Specifically, the code performs data cleaning, applies predictive modeling using selected predictor variables, and generates estimates of food insecurity. These predictions are visualized in a map of Iowa's PUMAs to provide actionable insights.

## Requirements
To install the required R packages, run the following code in R:


```r
install.packages(c("tidyverse", "pROC", "glmnet", "lubridate", "sf", 
                   "tigris", "ggplot2", "rmapshaper", "haven", 
                   "ggthemes", "logistf"))

```

## Data
We used three sources of data: CPS data containing household and food insecurity measures, ACS data with individual and household-level variables with PUMA identifiers but lacks food insecurity measures, and a dataset of senior population counts by PUMA in Iowa. These data files can be found in the following directories:


```r
list.files("data/")
```

```
## [1] "cps_00006.csv"
## [2] "spm_pu_2022.sas7bdat"
## [3] "total_iowa_seniors_by_puma.csv"
```


We will call all three data files above,  
CPS Data = "cps_00006.csv"  
ACS Data = "spm_pu_2022.sas7bdat" (this dataset is not in the repo, due to its size - over 1000MB)
Senior Population Data = "total_iowa_seniors_by_puma.csv"

## Reproduce
1. Run `ACS_FSBAL.R` to generate a choropleth map visualizing the predicted number of seniors in each PUMA across Iowa who are unable to afford balanced meals (FSBAL).

2. Run `ACS_FSWROUTY.R` to generate a choropleth map visualizing the predicted number of seniors in each PUMA across Iowa who are worrying that food would run out before they could afford more (FSWROUTY).

## References
IPUMS. Current Population Survey (CPS). "Food Security Supplement" variable group. Integrated Public Use Microdata Series, University of Minnesota. Available at https://cps.ipums.org/cps-action/variables/group?id=h-foodsec_foodsec.

U.S. Census Bureau. American Community Survey (ACS). Available at https://www.census.gov/programs-surveys/acs.
