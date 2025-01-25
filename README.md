# FSCI Data Pipeline

## Pipeline

The pipeline comprises a series of RMarkdown notebooks which process the data and save the necessary artifacts to the `output` directory. 

## Steps

1. `1_metrics_calculation.Rmd`: This notebook reads in the raw data, prepares it for analysis, generates a report on the data, and saves the output to the output directory. Two files are saved: 
  - `output/metrics_data.rds`
  - `output/metrics_combined_dataframe.rds`
  
  The first file contains the data for each metric along with meta-data, while the second file contains the metrics for all indicators in a single R data frame.


## Data Patches

To handle the small number of inconsistencies in the data (e.g., missing values, incorrect data types), the `data_patches.R` file contains a function that returns conditions for when to patch data. The `extract_indicator_data` function applies these patches to the data.

## Data Sources

The data used for this analysis can be downloaded at [Kate Schneider's FSCI 2024 Interactions Replication GitHub repository](https://github.com/KateSchneider-FoodPol/FSCI_2024Interactions_Replication/blob/main/Output%20data/FSCI_2024.csv.gz).

