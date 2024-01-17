# Cost-of-travel-delays

This repository contains the replication data and code for 

> Lian, T., Loo, B.P.Y. (2024) Cost of travel delays by traffic crashes

This study introduces a methodology using taxi GPS and open-source spatial data to measure travel delays caused by traffic crashes in Hong Kong. It quantifies delays by comparing typical and post-crash travel speeds on impacted road segments. 

A generalized linear model incorporating crash attributes, time, road network features, and urban environmental factors reveals that delays are poorly explained by crash characteristics alone. Integrating dynamic road conditions and built environment elements, like road segment centrality and pedestrian flow, significantly enhances model accuracy. 

# Instructions

## Data
The raw taxi GPS data utilized in our study are proprietary and were provided by an information technology company under a strict non-disclosure agreement. This agreement explicitly prohibits the sharing of the raw data with any third parties. To comply with this agreement and protect the confidentiality of the data, we are unable to share the raw dataset in its entirety. However, to support the reproducibility and verification of our results, we have prepared an aggregated dataset that includes: (1) aggregated speed estimates at affected road segments; (2) aggregated data used in the hourly traffic volume prediction model; and (3) other aggregated explanatory variables used in the regression analysis.




## Scripts
* estimate_speed.ipynb
  
   This script interprets the map-matching result and estimates traffic speed based on the speed information recorded in the GPS records
  
* plot_travel_delays_by_categorical_variables.R
  
   This script plots the estimated travel delays by different categorical variables (Figure 7)

* regression_analysis.R

   This script is the regression analysis
  

## Environment

Clone the repo
```
git clone https://github.com/LainyLian/Cost-of-travel-delays.git
```

Create a new python environment
```
conda create -n <env_name> python=3.9.12
```

Install all required packages
```
pip install -r requirements.txt
```
