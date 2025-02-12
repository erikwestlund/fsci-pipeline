# ======================================================================
# This file contains a collection of functions designed for the analysis,
# summarization, and visualization of indicator data. The functions are 
# alphabetized for ease of navigation. Comments follow the Roxygen style 
# for consistency, although this script is not part of a formal package.
# ======================================================================

#' calculate_distance_metrics
#'
#' @description Computes distance metrics for an indicator, including distances to global, regional, and income group milestones, 
#' as well as the distance to a specified target value (if provided). The calculations are based on the most recent data values.
#'
#' @param indicator_data A data frame containing the indicator data. Must include columns specified by `year_col`, `country_col`, and `value_col`.
#'   The `extract_indicator_data()` function should be used to extract the required columns from the raw data.
#' @param indicator_milestone_metrics A data frame containing milestone metrics for the indicator, including columns for global, regional, and income group milestones.
#'  The `get_milestone_metrics()` function should be used to extract the required columns from the raw data.
#' @param target_value (Optional) A numeric value representing the target to which distances are calculated. Default: `NA`.
#' @param variable_col The column name representing the variable/indicator (default: `"variable"`).
#' @param year_col The column name representing the year of the data (default: `"year"`).
#' @param country_col The column name representing the country (default: `"country"`).
#' @param value_col The column name representing the indicator value (default: `"value"`).
#'
#' @return A data frame containing:
#' - `variable`: The variable/indicator for which distances are calculated.
#' - `latest_year`: The most recent year of data for the country.
#' - `country`: The country name.
#' - `value`: The indicator value for the most recent year.
#' - `distance_to_milestone_global`: The distance to the global milestone.
#' - `distance_to_milestone_region`: The distance to the regional milestone.
#' - `distance_to_milestone_income_group`: The distance to the income group milestone.
#' - `distance_to_target`: The distance to the specified target value (if provided).
#'
#' @examples
#' # Example usage with indicator and milestone data
#' distance_metrics <- calculate_distance_metrics(
#'   indicator_data = extract_indicator_data(raw_analysis_data, "safeh20"),
#'   indicator_milestone_metrics = get_milestone_metrics(raw_analysis_data, "safeh20"),
#'   target_value = 100
#' )
calculate_distance_metrics <- function(
    indicator_data,
    indicator_milestone_metrics, 
    target_value = NA,
    variable_col = "variable",
    year_col = "year",
    country_col = "country",
    value_col = "value",
    region_col = "un_continental_region",
    income_group_col = "income_group"
) {
  distance_starting_data <- get_recent_data_values(indicator_data, year_col, country_col, value_col)
  
  indicator_data |> select(all_of(c(variable_col, year_col, country_col, region_col, income_group_col, value_col))) |> 
    mutate(
      target_value = target_value,
    ) |> 
    inner_join(
      distance_starting_data,
      by = c(c("year" = "latest_year"), country_col)
    ) |> 
    inner_join(
      indicator_milestone_metrics,
      by = c(country_col, variable_col)
    ) |> 
    mutate(
      latest_year = as.numeric(year),
      distance_to_milestone_global = milestone_global - value,
      distance_to_milestone_region = milestone_region - value,
      distance_to_milestone_income_group = milestone_income_group - value,
      distance_to_target = ifelse(is.na(target_value), NA, target_value - value)
    ) |> 
    select(
      all_of(c(variable_col, "latest_year", country_col, value_col, "distance_to_milestone_global", "distance_to_milestone_region", "distance_to_milestone_income_group", "distance_to_target"))
    ) |> 
    arrange(!!sym(country_col))
}


#' calculate_milestone_metrics
#'
#' @description Encapsulates the calculation of milestone data for global, regional, 
#' and income-group levels. It merges the results into a single dataset containing 
#' milestone values for each category.
#'
#' @param indicator_data A data frame containing the indicator data for calculations.
#' @param region_col The column name representing regions in the dataset (default: `"un_continental_region"`).
#'   Note: In the raw data, this column is named `UN Continental Region`. To avoid issues using awkward `backtick`
#'   syntax, before using this function, rename the column to `un_continental_region`.
#' @param income_group_col The column name representing income groups in the dataset (default: `"income_group"`).
#' @param variable_col The column name representing the variable/indicator (default: `"variable"`).
#' @param year_col The column name representing the year of the data (default: `"year"`).
#' @param country_col The column name representing the country (default: `"country"`).
#' @param value_col The column name representing the indicator value (default: `"value"`).
#'
#' @return A data frame with calculated milestone values for global, regional, and income-group levels,
#' including the merged data for further analysis.
#'
#' @examples
#' # Example usage:
#' milestone_metrics <- calculate_milestone_metrics(
#'   indicator_data = analysis_data,
#'   region_col = "un_continental_region",
#'   income_group_col = "income_group",
#'   variable_col = "variable",
#'   year_col = "year",
#'   country_col = "country",
#'   value_col = "value"
#' )
calculate_milestone_metrics <- function(
    indicator_data,
    region_col = "un_continental_region",
    income_group_col = "income_group",
    variable_col = "variable",
    year_col = "year",
    country_col = "country",
    value_col = "value"
) {
  milestone_global <- calculate_milestones(indicator_data, variable_col, year_col, value_col, country_col) |> pull(mean_value)
  
  milestone_region_data <- calculate_milestones(indicator_data, variable_col, year_col, value_col, country_col, region_col) |>
    rename(milestone_region = mean_value) |> 
    filter(!is.na(milestone_region)) |> 
    filter(!is.na(!!sym(region_col)))
  
  milestone_income_group_data <- calculate_milestones(indicator_data, variable_col, year_col, value_col, country_col, income_group_col) |>
    rename(milestone_income_group = mean_value) |> 
    filter(!is.na(milestone_income_group)) |> 
    filter(!is.na(!!sym(income_group_col)))
  
  indicator_data |> select(all_of(c(variable_col, year_col, country_col, region_col, income_group_col, value_col))) |> 
    mutate(
      milestone_global = milestone_global
    ) |>
    left_join(milestone_region_data, by = c(region_col)) |> 
    left_join(milestone_income_group_data, by = c(income_group_col)) |>
    filter(!is.na(value)) |>
    group_by(!!sym(country_col)) |> 
    mutate(
      latest_year = max(year)
    ) |>
    ungroup() |>
    filter(year == latest_year) |> 
    select(all_of(c(variable_col, "latest_year", country_col, "milestone_global", "milestone_region", "milestone_income_group")))
}


#' calculate_milestones
#'
#' @description Computes milestone values (e.g., percentiles) for indicators, either globally or within specific subgroups (e.g., regions, income groups). 
#' Supports dynamic grouping based on the provided subgroup column.
#'
#' @param indicator_data A data frame containing the data to calculate milestones for. Must include columns specified in other parameters.
#' @param variable_col The column name representing the variable/indicator (default: `"variable"`).
#' @param year_col The column name representing the year of the data (default: `"year"`).
#' @param value_col The column name representing the indicator value (default: `"value"`).
#' @param subgroup_col (Optional) The column name representing the subgroup for which milestones are calculated (e.g., `"region"` or `"income_group"`). 
#'   If `NULL`, calculates milestones globally (default: `NULL`).
#'
#' @return A data frame containing the milestone values for each variable, year, and optionally subgroup. Includes columns:
#' - `country`: The country associated with the milestone.
#' - `milestone`: The calculated milestone value based on the specified percentile (`milestone_pctile`).
#' - `variable`: The variable/indicator for which the milestone was calculated.
#' - `year`: The year of the milestone.
#'
#' @examples
#' # Calculate global milestones
#' global_milestones <- calculate_milestones(
#'   indicator_data = analysis_data,
#'   variable_col = "variable",
#'   year_col = "year",
#'   value_col = "value"
#' )
#'
#' # Calculate regional milestones
#' regional_milestones <- calculate_milestones(
#'   indicator_data = analysis_data,
#'   variable_col = "variable",
#'   year_col = "year",
#'   value_col = "value",
#'   subgroup_col = "region"
#' )
calculate_milestones <- function(
    indicator_data, 
    variable_col = "variable",
    year_col = "year",
    value_col = "value",
    country_col = "country",
    subgroup_col = NULL
) {
  group_data <- function(data) {
    if (!is.null(subgroup_col)) {
      group_by(data, !!sym(subgroup_col))
    } else {
      group_by(data)
    }
  }

  desirable_direction <- indicator_data |> 
    pull(desirable_direction) |> 
    unique()
  
  indicator_data |> 
    mutate(
      !!value_col := !!sym(value_col)
    ) |> 
    filter(!is.na(!!sym(value_col))) |> # Remove rows where value could not be converted
    group_by(!!sym(country_col)) |> # Group by country
    mutate(
      max_year = max(!!sym(year_col), na.rm = TRUE) 
    ) |> 
    ungroup() |> 
    filter(!!sym(year_col) == max_year) |> 
    group_data() |> # Apply conditional grouping
    mutate(
      milestone_cutoff = quantile(!!sym(value_col), milestone_pctile, na.rm = TRUE) # Calculate using correct milestone given desirability direction
    ) |> 
    filter(
      if_else(desirable_direction == 1, !!sym(value_col) >= milestone_cutoff, !!sym(value_col) <= milestone_cutoff)
    ) |>
    summarise(mean_value = mean(!!sym(value_col), na.rm = TRUE), .groups = "drop")
}

calculate_sdg_cagr_metrics <- function(
    indicator_data,
    desirable_direction = 1,
    target_value = NA,
    target_year = 2030,
    baseline_calc_year = 2015,
    country_col = "country",
    variable_col = "variable",
    year_col = "year",
    value_col = "value"
) {
  # First, calculate the CAGR from baseline year to latest hear
  cagr_data <- indicator_data |> 
    select(!!sym(country_col), !!sym(variable_col), !!sym(year_col), !!sym(value_col), desirable_direction) |> 
    filter(! is.na(!!sym(value_col)), ! is.na(!!sym(year_col))) |>
    group_by(!!sym(country_col)) |> 
    mutate(
      baseline_year = min(if_else(!!sym(year_col) >= baseline_calc_year, !!sym(year_col), Inf), na.rm = TRUE),
      baseline_year = if_else(baseline_year == Inf, NA_real_, baseline_year),
      baseline_year_value = if_else(!!sym(year_col) == baseline_year, !!sym(value_col), NA_real_),
      latest_year = max(!!sym(year_col), na.rm = TRUE),
      latest_year_value = if_else(!!sym(year_col) == latest_year, !!sym(value_col), NA_real_)
    ) |>
    # Filter to retain only the relevant rows with baseline and latest year values
    summarise(
      baseline_year_value = first(na.omit(baseline_year_value), default = NA_real_),
      latest_year_value = first(na.omit(latest_year_value), default = NA_real_),
      baseline_year = first(baseline_year),
      latest_year = first(latest_year)
    ) |> 
    filter(! is.na(baseline_year_value), ! is.na(latest_year_value)) |>
    rowwise() |> 
    mutate(
      cagr_sdg_baseline_to_latest = (latest_year_value / baseline_year_value) ^ (1 / (latest_year - baseline_year)) - 1
    )
  
  # Now, depending on whether there is a target value provided, we take different steps
  has_target <- ! is.na(target_value)
  
  # If we have a target
  result <- if (has_target) {
    cagr_data |> 
      mutate(
        value_ev = latest_year_value * (1 + cagr_sdg_baseline_to_latest) ^ (target_year - latest_year),
        ev_target_ratio = (value_ev - baseline_year_value) / (target_value - baseline_year_value),
        sdg_value = case_when(
          ev_target_ratio == 0 ~ case_when(
            desirable_direction == 1 && latest_year_value > target_value ~ "On track",
            desirable_direction == -1 && latest_year_value < target_value ~ "On track",
            TRUE ~ "Regression"
          ),
          ev_target_ratio >= 0.95 ~ "On track",
          ev_target_ratio >= 0.5 ~ "Moderate progress",
          ev_target_ratio >= 0.1 ~ "Marginal progress",
          ev_target_ratio >= 0 ~ "Stagnation", 
          ev_target_ratio < 0 ~ "Regression",
          TRUE ~ NA_character_
        )
      )
  } else {
    cagr_data |> 
      mutate(
        abs_cagr_sdg_baseline_to_latest = abs(cagr_sdg_baseline_to_latest),  # Take absolute value for comparison
        sdg_value = case_when(
          abs_cagr_sdg_baseline_to_latest >= 0.02 ~ "On track",
          abs_cagr_sdg_baseline_to_latest >= 0.0125 ~ "Moderate progress",
          abs_cagr_sdg_baseline_to_latest >= 0.005 ~ "Marginal progress",
          abs_cagr_sdg_baseline_to_latest >= 0 ~ "Stagnation",
          abs_cagr_sdg_baseline_to_latest < 0 ~ "Regression",
          TRUE ~ NA_character_
        )
      ) |> 
      select(-abs_cagr_sdg_baseline_to_latest)
  }
  
  result |> mutate(
    sdg_value = if_else(baseline_year == latest_year, NA_character_, sdg_value)
  )
}

#' calculate_standardized_distance_metrics
#'
#' @description Computes standardized distance metrics for indicators based on global, regional, and income group summaries. 
#' The function calculates weighted means, percentiles, and min-max scaled values for comparisons, allowing for desirable direction adjustments.
#'
#' @param indicator_data A data frame containing the data for analysis. Must include the columns specified in other parameters.
#' @param region_col The column name representing the region grouping (default: `"un_continental_region"`).
#' @param income_group_col The column name representing the income group (default: `"income_group"`).
#' @param weight_col The column name representing the weights for calculating weighted means (default: `"weight"`).
#' @param variable_col The column name representing the variable/indicator (default: `"variable"`).
#' @param year_col The column name representing the year (default: `"year"`).
#' @param value_col The column name representing the indicator value (default: `"value"`).
#'
#' @return A list of three data frames:
#' - `global`: Global summary metrics, including weighted means, percentiles, and min-max scaled values.
#' - `region`: Regional summary metrics, including weighted means and min-max scaled values for each region.
#' - `income_group`: Income group summary metrics, including weighted means and min-max scaled values for each income group.
#'
#' @examples
#' # Example: Calculate standardized metrics for global, regional, and income group comparisons
#' standardized_metrics <- calculate_standardized_distance_metrics(
#'   indicator_data = analysis_data,
#'   region_col = "un_continental_region",
#'   income_group_col = "income_group",
#'   weight_col = "weight",
#'   variable_col = "variable",
#'   year_col = "year",
#'   value_col = "value"
#' )
#'
#' # Access the global summary
#' global_metrics <- standardized_metrics$global
#'
#' # Access the regional summary
#' regional_metrics <- standardized_metrics$region
#'
#' # Access the income group summary
#' income_group_metrics <- standardized_metrics$income_group
calculate_standardized_distance_metrics <- function(
  indicator_data,
  region_col = "un_continental_region",
  income_group_col = "income_group",
  weight_col = "weight",
  variable_col = "variable",
  year_col = "year",
  value_col = "value"
) {
  desirable_direction <- indicator_data |> pull(desirable_direction) |> first() 
  
  # First, we need to calculate weighted means for global and grouped by region and income group
  # Then, we need global distance and regional/income group distance
  # Then we need a -1 to 1 standardization in the dirction of the desirable direction
  weighted_mean_data <- indicator_data  |> 
    filter(! is.na(!!sym(value_col)) & ! is.na(!!sym(weight_col))) |>
    # Calculate weighted means
    group_by(!!sym(year_col)) |>
    mutate(
      mean_global = mean(!!sym(value_col), na.rm = TRUE),
      p25_global = quantile(!!sym(value_col), 0.25, na.rm = TRUE),
      median_global = quantile(!!sym(value_col), 0.50, na.rm = TRUE),
      p75_global = quantile(!!sym(value_col), 0.75, na.rm = TRUE),
      weighted_mean_global = weighted.mean(!!sym(value_col), w = !!sym(weight_col), na.rm = TRUE),
      min_global = min(!!sym(value_col), na.rm = TRUE),
      max_global = max(!!sym(value_col), na.rm = TRUE)
    ) |> 
    ungroup() |> 
    group_by(!!sym(region_col), !!sym(year_col)) |>
    mutate(
      mean_region = mean(!!sym(value_col), na.rm = TRUE),
      p25_region = quantile(!!sym(value_col), 0.25, na.rm = TRUE),
      median_region = quantile(!!sym(value_col), 0.50, na.rm = TRUE),
      p75_region = quantile(!!sym(value_col), 0.75, na.rm = TRUE),
      weighted_mean_region = weighted.mean(!!sym(value_col), w = !!sym(weight_col), na.rm = TRUE),
      min_region = min(!!sym(value_col), na.rm = TRUE),
      max_region = max(!!sym(value_col), na.rm = TRUE)
    ) |>
    ungroup() |> 
    group_by(!!sym(income_group_col), !!sym(year_col)) |>
    mutate(
      mean_income_group = mean(!!sym(value_col), na.rm = TRUE),
      p25_income_group = quantile(!!sym(value_col), 0.25, na.rm = TRUE),
      median_income_group = quantile(!!sym(value_col), 0.50, na.rm = TRUE),
      p75_income_group = quantile(!!sym(value_col), 0.75, na.rm = TRUE),
      weighted_mean_income_group = weighted.mean(!!sym(value_col), w = !!sym(weight_col), na.rm = TRUE),
      min_income_group = min(!!sym(value_col), na.rm = TRUE),
      max_income_group = max(!!sym(value_col), na.rm = TRUE)
    ) |> 
    ungroup() |> 
    select(country, !!sym(year_col), mean_global, weighted_mean_global, p25_global, median_global, p75_global, min_global, max_global, mean_region, weighted_mean_region, median_region, p25_region, p75_region, min_region, max_region, mean_income_group, weighted_mean_income_group, p25_income_group, median_income_group, p75_income_group,  min_income_group, max_income_group)
  
  reference_data <- indicator_data |> 
    left_join(weighted_mean_data, by = c("country", year_col)) 
  
  global_summary <- reference_data |>
    filter(!is.na(weighted_mean_global) & !is.na(mean_global)) |>
    group_by(year) |> 
    mutate(
      min_max_scaled_wtd = (weighted_mean_global - min_global) / (max_global - min_global),
      min_max_mean_scaled_wtd = desirable_direction * (weighted_mean_global - weighted_mean_global) / (max_global - min_global)
    ) |> 
    summarise(
      min_max_scaled_wtd = mean(min_max_scaled_wtd, na.rm = TRUE),
      min_max_mean_scaled_wtd = mean(min_max_mean_scaled_wtd, na.rm = TRUE),
      mean_global = first(mean_global),
      weighted_mean_global = first(weighted_mean_global),
      min_global = first(min_global),
      max_global = first(max_global),
      p25_global = first(p25_global),
      median_global = first(median_global),
      p75_global = first(p75_global),
      .groups = "drop"
    ) |> 
    filter(
      !is.na(min_max_scaled_wtd & !is.na(min_max_mean_scaled_wtd)) & !is.nan(min_max_scaled_wtd) & !is.nan(min_max_mean_scaled_wtd)
    ) |> 
    select(
      year, min_max_scaled_wtd, everything()
    )
  
  region_summary <- reference_data |>
    filter(!is.na(weighted_mean_region) & !is.na(mean_global)) |> 
    group_by(!!sym(region_col), year) |>
    mutate(
      min_max_scaled_region_vs_global_wtd = (weighted_mean_region - min_global) / (max_global - min_global),
      min_max_mean_scaled_region_vs_global_wtd = desirable_direction * (weighted_mean_region - weighted_mean_global) / (max_global - min_global)
    ) |> 
    summarise(
      min_max_scaled_region_vs_global_wtd = first(min_max_scaled_region_vs_global_wtd),
      min_max_mean_scaled_region_vs_global_wtd = first(min_max_mean_scaled_region_vs_global_wtd),
      mean_global = first(mean_global),
      weighted_mean_global = first(weighted_mean_global),
      mean_region = first(mean_region),
      weighted_mean_region = first(weighted_mean_region),
      min_global = first(min_global),
      max_global = first(max_global),
      min_region = first(min_region),
      max_region = first(max_region),
      p25_region = first(p25_region),
      median_region = first(median_region),
      p75_region = first(p75_region),
      .groups = "drop"
    )|> 
    rename(
      region = !!sym(region_col)
    ) |> 
    filter(
      !is.na(min_max_scaled_region_vs_global_wtd & !is.na(min_max_mean_scaled_region_vs_global_wtd)) & !is.nan(min_max_scaled_region_vs_global_wtd) & !is.nan(min_max_mean_scaled_region_vs_global_wtd)
    ) |>
    select(
      year, region, min_max_scaled_region_vs_global_wtd, min_max_mean_scaled_region_vs_global_wtd, everything()
    )
  
  income_group_summary <- reference_data |>
    filter(!is.na(weighted_mean_income_group) & !is.na(mean_global)) |>
    group_by(!!sym(income_group_col), year) |>
    mutate(
      min_max_scaled_income_group_wtd = (weighted_mean_income_group - min_global) / (max_global - min_global),
      min_max_mean_scaled_income_group_wtd = desirable_direction * (weighted_mean_income_group - weighted_mean_global) / (max_global - min_global)
    ) |>
    summarise(
      min_max_scaled_income_group_wtd = first(min_max_scaled_income_group_wtd),
      min_max_mean_scaled_income_group_wtd = first(min_max_mean_scaled_income_group_wtd),
      mean_global = first(mean_global),
      weighted_mean_global = first(weighted_mean_global),
      mean_income_group = first(mean_income_group),
      weighted_mean_income_group = first(weighted_mean_income_group),
      min_global = first(min_global),
      max_global = first(max_global),
      min_income_group = first(min_income_group),
      max_income_group = first(max_income_group),
      p25_income_group = first(p25_income_group),
      median_income_group = first(median_income_group),
      p75_income_group = first(p75_income_group),
      .groups = "drop"
    )  |> 
    rename(
      income_group = !!sym(income_group_col)
    )  |> 
    filter(
      !is.na(min_max_scaled_income_group_wtd & !is.na(min_max_mean_scaled_income_group_wtd)) & !is.nan(min_max_scaled_income_group_wtd) & !is.nan(min_max_mean_scaled_income_group_wtd)
    ) |>
    select(
      year, income_group, min_max_scaled_income_group_wtd, min_max_mean_scaled_income_group_wtd, everything()
    )
  
  list(
    global = global_summary,
    region = region_summary,
    income_group = income_group_summary
  )
}


#' calculate_cagr
#'
#' @description Calculates the compound annual growth rate (CAGR) required to reach a target value from a starting value 
#' over a specified number of years. The result is expressed as a percentage.
#'
#' @param starting_value A numeric value representing the starting point of the calculation.
#'   Must be greater than zero to avoid mathematical errors.
#' @param starting_year An integer representing the year corresponding to the starting value.
#' @param target_value A numeric value representing the target to be achieved.
#'   Must be greater than zero to avoid mathematical errors.
#' @param target_year An integer representing the year by which the target value should be achieved.
#'
#' @return A numeric value representing the required CAGR (expressed as a percentage) to achieve the target value 
#' from the starting value by the target year. If `starting_year` equals `target_year`, the function returns `Inf` 
#' as no time exists for growth.
#'
#' @examples
#' # Example: Calculate the CAGR required to grow from 100 to 200 in 10 years
#' calculate_cagr(
#'   starting_value = 100, 
#'   starting_year = 2020, 
#'   target_value = 200, 
#'   target_year = 2030
#' )
#' 
#' # Example: When the target year equals the starting year
#' calculate_cagr(
#'   starting_value = 100, 
#'   starting_year = 2020, 
#'   target_value = 200, 
#'   target_year = 2020
#' ) # Returns `Inf`
calculate_cagr <- function(
    starting_value,
    starting_year,
    target_value, 
    target_year
) {
  if((!is.na(starting_value) && starting_value <= 0) || (!is.na(target_value) && target_value <= 0)) {
    NA
  } else {
    100 * ((target_value / starting_value) ^ (1 / (target_year - starting_year)) - 1)
  }
}


#' calculate_velocity_linear_growth_rate_required_to_hit_target_value_by_target_year
#'
#' @description Calculates the linear annual growth rate required to reach a target value from a starting value 
#' over a specified number of years. The result is expressed as a percentage per year.
#'
#' @param starting_value A numeric value representing the starting point of the calculation.
#' @param starting_year An integer representing the year corresponding to the starting value.
#' @param target_value A numeric value representing the target to be achieved.
#' @param target_year An integer representing the year by which the target value should be achieved.
#'
#' @return A numeric value representing the required linear growth rate (expressed as a percentage per year) 
#' to achieve the target value from the starting value by the target year. If `starting_year` equals `target_year`, 
#' the function returns `Inf` or `NaN` due to division by zero.
#'
#' @examples
#' # Example: Calculate the linear growth rate required to grow from 100 to 200 in 10 years
#' calculate_velocity_linear_growth_rate_required_to_hit_target_value_by_target_year(
#'   starting_value = 100, 
#'   starting_year = 2020, 
#'   target_value = 200, 
#'   target_year = 2030
#' )
calculate_velocity_linear_growth_rate_required_to_hit_target_value_by_target_year <- function(
    starting_value,
    starting_year,
    target_value, 
    target_year
) {
  100 * ((target_value - starting_value) / (target_year - starting_year))
}


#' calculate_velocity_metrics
#'
#' @description Computes velocity metrics for an indicator, including the growth rates required to achieve global, regional, 
#' and income group milestones, as well as a specified target value (if provided) by a target year. 
#' Calculations are based on the most recent data values and milestones. The function outsources calculating
#' the data points from which to calculate the growth rates to the `get_recent_data_values()` function.
#' It outsources the calculation of velocitry metrics to the `calculate_velocity_required_to_hit_milestone_metrics()` function.
#'
#' @param indicator_data A data frame containing the indicator data. Must include columns specified by year_col, country_col, and value_col.
#'   The extract_indicator_data() function should be used to extract the required columns from the raw data.
#' @param indicator_milestone_metrics A data frame containing milestone metrics for the indicator, including columns for global, regional, and income group milestones.
#'   The calculate_milestone_metrics() function should be used to compute the required milestone metrics.
#' @param target_value (Optional) A numeric value representing the target to which growth rates are calculated. Default: NA.
#' @param target_year A numeric value representing the year by which milestones and targets should be achieved. Default: 2030.
#' @param variable_col The column name representing the variable/indicator (default: "variable").
#' @param year_col The column name representing the year of the data (default: "year").
#' @param country_col The column name representing the country (default: "country").
#' @param value_col The column name representing the indicator value (default: "value").
#'
#' @return A data frame containing:
#' - variable: The variable/indicator for which velocities are calculated.
#' - country: The country name.
#' - growth rates to milestones and targets, including:
#'   - CAGR (Compound Annual Growth Rate) and linear growth rates to:
#'     - milestone_global: The global milestone.
#'     - milestone_region: The regional milestone.
#'     - milestone_income_group: The income group milestone.
#'     - target: The specified target value (if provided).
#'   - Linear growth rate to target_year.
#'    
#'
#' @examples
#' # Example usage with indicator and milestone data
#' velocity_metrics <- calculate_velocity_metrics(
#'   indicator_data = extract_indicator_data(raw_analysis_data, "safeh20"),
#'   indicator_milestone_metrics = calculate_milestone_metrics(raw_analysis_data, "safeh20"),
#'   target_value = 100,
#'   target_year = 2030
#' )
calculate_velocity_metrics <- function(
    indicator_data,
    indicator_milestone_metrics, 
    target_value = NA,
    target_year = 2030,
    variable_col = "variable",
    year_col = "year",
    country_col = "country",
    value_col = "value"
) {
  velocity_starting_data <- get_recent_data_values(indicator_data, year_col, country_col, value_col)
  by_year_velocities <- calculate_velocity_required_to_hit_milestone_metrics(velocity_starting_data, indicator_milestone_metrics, target_value, target_year)
  
  years_contributing <- indicator_data |> 
    group_by(!!sym(country_col)) |>
    summarise(years_contributing = n_distinct(!!sym(year_col))) |>
    rename(country = !!sym(country_col)) 
    
  by_year_velocities |> 
    left_join(years_contributing, by = country_col) |>
    group_by(country) |> 
    ungroup() |> 
    arrange(country)
}


#' calculate_velocity_required_to_hit_milestone_metrics
#'
#' @description Computes the growth rates (both compound annual growth rate (CAGR) and linear growth rate) required 
#' to reach global, regional, and income group milestones, as well as an optional target value, by a specified target year.
#' The calculations are performed using both the most recent value and the average value from the last three years of data.
#'
#' @param velocity_starting_data A data frame containing recent data values for each country. Must include:
#'   - `latest_year`: The most recent year with data.
#'   - `latest_value`: The most recent indicator value.
#'   - `avg_value_last_three_years`: The average indicator value from the last three years.
#' @param indicator_milestone_metrics A data frame containing milestone metrics for each country. Must include:
#'   - `milestone_global`: The global milestone value.
#'   - `milestone_region`: The regional milestone value.
#'   - `milestone_income_group`: The income group milestone value.
#' @param target_value (Optional) A numeric value representing the target to which growth rates are calculated. Default: NA.
#' @param target_year The year by which the milestones or target value should be achieved.
#'
#' @return A data frame containing 16 metrics:
#'   - 4 CAGR from latest year
#'   - 4 linear growth rate latest year
#'   - 4 CAGR from average of recent values from last three years
#'   - 4 linear growth rate from average of recent values from last three years
#' 
#' @examples
#' # Example usage
#' growth_metrics <- calculate_velocity_required_to_hit_milestone_metrics(
#'   velocity_starting_data = recent_values,
#'   indicator_milestone_metrics = milestone_metrics,
#'   target_value = 100,
#'   target_year = 2030
#' )
#'
#' @seealso calculate_cagr, calculate_velocity_linear_growth_rate_required_to_hit_target_value_by_target_year
calculate_velocity_required_to_hit_milestone_metrics <- function(
    velocity_starting_data,
    indicator_milestone_metrics,
    target_value,
    target_year
) {
  # Join starting data with milestone metrics by country
  velocity_starting_data <- velocity_starting_data |> 
    left_join(indicator_milestone_metrics, by = c("country", "latest_year"))
  
  # Calculate velocity metrics
  velocity_starting_data |> 
    rowwise() |> 
    mutate(
      # Using latest_value as starting point
      from_latest_year_cagr_required_to_hit_milestone_global = calculate_cagr(
        latest_value, latest_year, milestone_global, target_year),
      from_latest_year_cagr_required_to_hit_milestone_region = calculate_cagr(
        latest_value, latest_year, milestone_region, target_year),
      from_latest_year_cagr_required_to_hit_milestone_income_group = calculate_cagr(
        latest_value, latest_year, milestone_income_group, target_year),
      from_latest_year_cagr_required_to_hit_target = calculate_cagr(
        latest_value, latest_year, target_value, target_year),
      
      from_latest_year_linear_growth_rate_required_to_hit_milestone_global = calculate_velocity_linear_growth_rate_required_to_hit_target_value_by_target_year(
        latest_value, latest_year, milestone_global, target_year),
      from_latest_year_linear_growth_rate_required_to_hit_milestone_region = calculate_velocity_linear_growth_rate_required_to_hit_target_value_by_target_year(
        latest_value, latest_year, milestone_region, target_year),
      from_latest_year_linear_growth_rate_required_to_hit_milestone_income_group = calculate_velocity_linear_growth_rate_required_to_hit_target_value_by_target_year(
        latest_value, latest_year, milestone_income_group, target_year),
      from_latest_year_linear_growth_rate_required_to_hit_target = calculate_velocity_linear_growth_rate_required_to_hit_target_value_by_target_year(
        latest_value, latest_year, target_value, target_year),
      
      # Using avg_value_last_three_years as starting point
      from_mean_last_3_years_cagr_required_to_hit_milestone_global = calculate_cagr(
        avg_value_last_three_years, latest_year, milestone_global, target_year),
      from_mean_last_3_years_cagr_required_to_hit_milestone_region = calculate_cagr(
        avg_value_last_three_years, latest_year, milestone_region, target_year),
      from_mean_last_3_years_cagr_required_to_hit_milestone_income_group = calculate_cagr(
        avg_value_last_three_years, latest_year, milestone_income_group, target_year),
      from_mean_last_3_years_cagr_required_to_hit_target = calculate_cagr(
        avg_value_last_three_years, latest_year, target_value, target_year),
      
      from_mean_last_3_years_linear_growth_rate_required_to_hit_milestone_global = calculate_velocity_linear_growth_rate_required_to_hit_target_value_by_target_year(
        avg_value_last_three_years, latest_year, milestone_global, target_year),
      from_mean_last_3_years_linear_growth_rate_required_to_hit_milestone_region = calculate_velocity_linear_growth_rate_required_to_hit_target_value_by_target_year(
        avg_value_last_three_years, latest_year, milestone_region, target_year),
      from_mean_last_3_years_linear_growth_rate_required_to_hit_milestone_income_group = calculate_velocity_linear_growth_rate_required_to_hit_target_value_by_target_year(
        avg_value_last_three_years, latest_year, milestone_income_group, target_year),
      from_mean_last_3_years_linear_growth_rate_required_to_hit_target = calculate_velocity_linear_growth_rate_required_to_hit_target_value_by_target_year(
        avg_value_last_three_years, latest_year, target_value, target_year)
    ) |> 
    ungroup()
}


#' clean_analysis_data_variables
#'
#' @description Cleans variable names in the dataset by performing minimal adjustments, 
#' such as removing trailing carriage return and newline characters.
#'
#' @param variables A character vector of variable names to be cleaned.
#'
#' @return A character vector of cleaned variable names with trailing `\r\n` characters removed.
#'
#' @examples
#' # Clean variable names
#' cleaned_variables <- clean_analysis_data_variables(c("emint_beef\r\n", "emint_cerealsnorice\r\n"))
#' print(cleaned_variables)
#' # Output: "emint_beef" "emint_cerealsnorice"
clean_analysis_data_variables <- function(variables) {
  # we do some minimal cleaning here, namely, removing \r\n from any variable names
  variables |> stringr::str_replace_all("\r\n$", "")
}


#' determine_data_type
#'
#' @description Determines the data type of a vector of values. The function checks if the values are numeric,
#' boolean, or character, and returns the appropriate type as a string.
#'
#' @param values A vector of values to assess. The values can be of mixed types.
#'
#' @return A string indicating the data type of the input values. Possible values are:
#'   - `"boolean"`: If all numeric values are either 0 or 1.
#'   - `"numeric"`: If all values can be coerced to numeric and are not boolean.
#'   - `"character"`: If the values cannot be coerced to numeric.
#'
#' @examples
#' # Determine data type for numeric values
#' determine_data_type(c("1", "2", "3")) # Returns "numeric"
#'
#' # Determine data type for boolean values
#' determine_data_type(c("0", "1", "1")) # Returns "boolean"
#'
#' # Determine data type for character values
#' determine_data_type(c("a", "b", "c")) # Returns "character"
determine_data_type <- function(values) {
  values <- as.character(values) # Coerce to character
  
  numeric_values <- suppressWarnings(as.numeric(values))
  
  if (all(!is.na(numeric_values))) {
    if (all(numeric_values %in% c(0, 1))) {
      return("boolean")
    }
    return("numeric")
  }
  
  return("character")
}


#' extract_indicator_data
#'
#' @description Extracts data for a specified indicator variable from the analysis dataset 
#' and applies specified data patches for cleaning. Patches can include removing specific rows 
#' or setting specific values to `NA`. Ensures the `value` column is numeric and removes duplicate rows.
#' These patches are located in a list in `data_patches.R`.
#'
#' @param analysis_data A data frame containing the analysis data. 
#'   Expected to include columns such as `variable`, `country`, `year`, and the specified `value_col`.
#' @param indicator_variable A character string specifying the indicator variable to filter from the dataset.
#' @param value_col A character string specifying the column name containing the values. Defaults to `"value"`.
#' @param data_patches A list of patch objects specifying modifications to the data. Each patch should 
#'   include an `action` field (`"remove"` or `"set_na"`) and the relevant filtering criteria 
#'   (`indicator_variable`, `country`, `year`, and `value`).
#'
#' @return A cleaned data frame containing data for the specified indicator variable, with duplicates removed 
#'   and the `value` column coerced to numeric.
#'
#' @examples
#' data_patches <- list(
#'   list(action = "remove", indicator_variable = "var1", country = "Country1", year = 2020, value = "10"),
#'   list(action = "set_na", indicator_variable = "var2", country = "Country2", year = 2021, value = "15.5")
#' )
#'
#' # Extract and clean indicator data
#' cleaned_data <- extract_indicator_data(analysis_data, "var1", data_patches = data_patches)
extract_indicator_data <- function(analysis_data, indicator_variable, value_col = "value", data_patches = list()) {
  indicator_data <- analysis_data |>
    filter(variable == indicator_variable)
  
  # Apply patches
  for (patch in data_patches) {
    # Remove action
    if (patch$action == "remove") {
      indicator_data <- indicator_data |>
        filter(
          !(
            variable == patch$indicator_variable &
              country == patch$country &
              year == patch$year &
              value == patch$value
          )
        )
    }
    
    # Set NA
    if (patch$action == "set_na") {
      indicator_data <- indicator_data |> 
        mutate(
          value = case_when(
            variable == patch$indicator_variable & 
              country == patch$country & 
              year == patch$year & 
              value == patch$value ~ NA_character_, # Explicitly use NA_character_ for compatibility
            TRUE ~ as.character(value) # Ensure value is treated as character
          )
        )
    }
  }
  
  indicator_data |> 
    mutate(
      value = readr::parse_number(!!sym(value_col)),
      year = as.integer(year)
    ) |> 
    unique()  # Remove duplicates. We only remove duplicate rows so no information is lost when calculating metrics.
}


#' extract_target_data
#'
#' @description Filters the target data to extract information for a specific indicator variable 
#' that has a defined target.
#'
#' @param indicator_variable A character string specifying the indicator variable to filter from the target data.
#'
#' @return A data frame containing the target data for the specified indicator variable, 
#'   filtered to include only entries where a target is defined (`has_target == TRUE`).
#'
#' @examples
#' # Extract target data for "safeh20"
#' extract_target_data("safeh20")
extract_target_data <- function(indicator_variable) {
  target_data |> 
    filter(variable == indicator_variable) |> 
    filter(has_target == TRUE)
}


#' generate_distance_forest_plots
#'
#' @description Generates forest plots for visualizing milestone data by region or income group. 
#' Each plot displays countries within the specified grouping variable, along with annotations 
#' for global and group-specific milestone values.
#'
#' @param indicator_variable_metrics A data frame containing metrics for an indicator variable, 
#'   including country, region, income group, and the latest year values.
#' @param milestone_summary A data frame summarizing milestone information for global, regional, 
#'   and income group milestones.
#' @param label A character string specifying the topic being analyzed. 
#' @param unit A character string specifying the unit of measurement for the x-axis. Default is `"Value"`.
#' @param group_by A character string specifying the grouping variable for the plots. 
#'   Options are `"region"` or `"income_group"`. Default is `"region"`.
#'
#' @return A list of ggplot objects, where each plot corresponds to a specific region or income group.
#'
#' @examples
#' # Generate forest plots by region
#' plots_by_region <- generate_distance_forest_plots(indicator_variable_metrics, milestone_summary, unit = "Percentage", group_by = "region")
#'
#' # Generate forest plots by income group
#' plots_by_income <- generate_distance_forest_plots(indicator_variable_metrics, milestone_summary, unit = "Percentage", group_by = "income_group")
generate_distance_forest_plots <- function(indicator_variable_metrics, milestone_summary, label, unit = "Value", group_by = "region") {
  # Determine grouping variable
  if (group_by == "region") {
    group_prefix <- "Region: "
    group_var <- "region"
  } else if (group_by == "income_group") {
    group_prefix <- "Income Group: "
    group_var <- "income_group"
  } else {
    stop("Invalid group_by value. Use 'region' or 'income_group'.")
  }
  
  desirable_direction = indicator_variable_metrics$desirable_direction |> first()
  
  # Get target value if it's in milestone_summary
  target_value <- milestone_summary |> 
    filter(Group == "Target") |> 
    pull(Milestone) |> 
    as.numeric()
  
  if(length(target_value) == 0) {
    target_value <- NA
  }
  
  # Extract groups
  groups <- milestone_summary |>
    filter(str_detect(Group, group_prefix)) |>
    mutate(group = str_remove(Group, group_prefix)) |>
    pull(group)
  
  # Initialize plots list
  plots <- list()
  
  # Loop through each group
  # Debug: group <- 
  for (group in groups) {
    local({
      # Filter data for the group
      group_data <- indicator_variable_metrics |>
        filter(!!sym(group_var) == !!group, !is.na(latest_year_value)) |>
        mutate(
          country = factor(
            country,
            levels = country[order(desirable_direction * latest_year_value)]
          )
        )
      
      # Plot title
      plot_title <- paste0(label)
      plot_subtitle <- if (group_by == "region") {
        paste0("Region: ", group)
      } else {
        paste0("Income Group: ", group)
      }
      
      # Add milestones as data points
      global_milestone_value <- milestone_summary |> filter(Group == "Global") |> pull(Milestone)
      group_milestone_value <- milestone_summary |>
        filter(str_detect(Group, paste0(group_prefix, group))) |>
        pull(Milestone)
      
      y_milestone_label <- if (group_by == "region") {
        "Region Milestone"
      } else {
        "Income Group Milestone"
      }
      
      milestones_to_summarize <- if(!is.na(target_value)) {
        c("Target", "Global Milestone", y_milestone_label)
      } else {
        c("Global Milestone", y_milestone_label)
      }
      
      latest_year_values <- if(!is.na(target_value)) {
        c(target_value, global_milestone_value, group_milestone_value)
      } else {
        c(global_milestone_value, group_milestone_value)
      }
      
      milestone_data <- tibble(
        country = milestones_to_summarize,
        latest_year_value = latest_year_values,
        desirable_direction = rep(desirable_direction, length(milestones_to_summarize))
      )
      
      # Combine and sort data
      group_data <- bind_rows(group_data, milestone_data) |>
        mutate(
          country = factor(
            country,
            levels = unique(country[order(desirable_direction * latest_year_value)])
          )
        )
      
      # Calculate dynamic x-axis limits with padding
      x_padding <- diff(range(group_data$latest_year_value, na.rm = TRUE)) * 0.025 * sign(mean(group_data$latest_year_value, na.rm = TRUE))
                   
      plot <- ggplot(group_data, aes(x = latest_year_value, y = country)) +
        # ggtitle(plot_title) +
  
        geom_segment(aes(x = 0, xend = latest_year_value, y = country, yend = country,
                         color = case_when(
                           str_starts(country, "Global") ~ "blue",
                           str_starts(country, y_milestone_label) ~ "red",
                           str_starts(country, "Target") ~ "darkgreen",
                           TRUE ~ "gray90"
                         )),
                     linewidth = 0.3)
        
      if(!is.na(target_value)) {
        plot <- plot + annotate(
          "text",
          x = target_value + x_padding, 
          y = "Target",
          label = paste0(round(target_value, 1)),
          color = "darkgreen",
          hjust = 0,
          size = 3.5
        )
      }
        
      plot <- plot + annotate(
          "text",
          x = global_milestone_value + x_padding, 
          y = "Global Milestone",
          label = paste0(round(global_milestone_value, 1)),
          color = "blue",
          hjust = 0,
          size = 3.5
        ) +
        annotate(
          "text",
          x = group_milestone_value + x_padding, # Add dynamic padding
          y = paste0(y_milestone_label),
          label = paste0(round(group_milestone_value, 1)),
          color = "red",
          hjust = 0,
          size = 3.5
        ) +
        geom_point(aes(color = case_when(
          str_starts(country, "Global") ~ "blue",
          str_starts(country, y_milestone_label) ~ "red",
          str_starts(country, "Target") ~ "darkgreen",
          TRUE ~ "gray"
        )), size = 3) +
        scale_x_continuous(expand = expansion(mult = c(0.05, 0.1))) +
        scale_y_discrete(labels = function(x) {
          case_when(
            str_starts(x, "Global") ~ paste0("**<span style='color:blue;'>", x, "</span>**"),
            str_starts(x, y_milestone_label) ~ paste0("**<span style='color:red;'>", x, "</span>**"),
            str_starts(x, "Target") ~ paste0("**<span style='color:darkgreen;'>", x, "</span>**"),
            TRUE ~ x
          )
        }) +
        scale_color_identity(guide = "none") +
        labs(
          title = plot_title,
          subtitle = plot_subtitle,
          x = unit, 
          y = "Country"
        ) +
        theme_minimal() +
        theme(
          axis.text.y = element_markdown(size = 10),
          axis.text.x = element_text(size = 10),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          plot.title = element_text(face = "bold", size = 14)
        )
      
      # Add the plot to the list
      plots[[group]] <<- plot
    })
  }
  
  plots
}


# Debug
# milestone = "global_milestone"
# milestone = "region_milestone"
# milestone = "income_group_milestone"
# group_by = "income_group"
# group_by = "region"
# 
# unit = indicator_unit
# label = indicator_label
# group_by = "region"
# milestone = "global_target"
# velocity_metric="cagr"
generate_velocity_plots <- function(indicator_variable_metrics, velocity_summary, label, unit = "Value", milestone = "global_target", group_by = "region", velocity_metric="cagr") {
  # Determine grouping variable
  if (group_by == "region") {
    group_prefix <- "Region: "
    group_var <- "region"
  } else if (group_by == "income_group") {
    group_prefix <- "Income Group: "
    group_var <- "income_group"
  } else {
    stop("Invalid group_by value. Use 'region' or 'income_group'.")
  }
  
  if(velocity_metric %in% c("cagr", "linear_growth")) {
    velocity_data <- velocity_summary[[velocity_metric]]
  } else {
    stop("Invalid velocity_metric value. Use 'cagr' or 'linear_growth'")
  }
  
  if(!milestone %in% c("global_target", "global_milestone", "region_milestone", "income_group_milestone")) {
    stop("Invalid milestone value. Use 'global_target', 'global_milestone', 'region_milestone', or 'income_group_milestone'")
  }
  
  # Extract groups
  groups <- velocity_data |>
    filter(str_detect(Group, group_prefix)) |>
    mutate(group = str_remove(Group, group_prefix)) |>
    pull(group)
  
  # Initialize plots list
  plots <- list()
  
  # Loop through each group
  # Debug:
  # label = indicator_label
  # unit = indicator_unit
  # group <- groups[1]
  for (group in groups) {
    local({
      # Filter data for the group
      group_data <- indicator_variable_metrics |>
        filter(!!sym(group_var) == !!group, !is.na(latest_year_value)) |>
        mutate(
          country = factor(
            country,
            levels = country[order(desirable_direction * latest_year_value)]
          )
        )
      
      # Milestone info
      metric_value <- velocity_data |> 
        filter(case_when(
          milestone == "global_target" ~ Group == "Target",
          milestone == "global_milestone" ~ Group == "Global",
          milestone == "region_milestone" ~ str_detect(Group, paste0("Region: ", group)),
          TRUE ~ str_detect(Group, paste0("Income Group: ", group))
        )) |> 
        pull(Milestone)
     
      metric_label <- case_when(
        milestone == "global_target" ~ "Global Target",
        milestone == "global_milestone" ~ "Global Milestone",
        milestone == "region_milestone" ~ "Region Milestone",
        milestone == "income_group_milestone" ~ "Income Group Milestone"
      )
      
      metric_label_title <- case_when(
        milestone == "global_target" ~ paste0(metric_label, " (", metric_value, "", unit, ")"),
        milestone == "global_milestone" ~ paste0(metric_label, " (",metric_value,  "", unit, ")"),
        milestone == "region_milestone" ~ paste0(metric_label, " (", metric_value,  "", unit, ")"),
        milestone == "income_group_milestone" ~ paste0(metric_label, " (", metric_value,  "", unit, ")"),
      )
      
      # Plot titles
      plot_title <- paste0("CAGR Required To Reach ", metric_label, " by Country")
      
      plot_subtitle <- if (group_by == "region") {
        paste0("Region: ", group)
      } else {
        paste0("Income Group: ", group)
      }
      
      y_milestone_label <- metric_label_title
  
      group_data <- group_data |> 
        mutate(cagr = case_when(
          milestone == "global_target" ~ from_latest_year_cagr_required_to_hit_target,
          milestone == "global_milestone" ~ from_latest_year_cagr_required_to_hit_milestone_global,
          milestone == "region_milestone" ~ from_latest_year_cagr_required_to_hit_milestone_region,
          TRUE ~ from_latest_year_cagr_required_to_hit_milestone_income_group
        )) |> 
        filter(!is.na(cagr)) |>
        select(country, cagr)
      
      
      metric_global_avg <- velocity_data |> 
        filter(case_when(
          milestone == "global_target" ~ Group == "Target",
          TRUE ~ Group == "Global"
        )) |> 
        pull(`Avg Growth Rate Required (From Latest Value)`) |> 
        as.numeric()
      
      metric_group_avg <- group_data  |> pull(cagr) |> mean() |> as.numeric()
      
      global_average_label <- "Average (Global)"
      group_average_label <- paste0("Average (", group, ")")
      
      global_avg_data <- tibble(
        country = global_average_label,
        cagr = metric_global_avg
      )
      
      group_avg_data <- tibble(
        country = group_average_label,
        cagr = metric_group_avg
      )
      
      group_data <- bind_rows(global_avg_data, group_avg_data, group_data) |> 
      mutate(
        country = factor(
          country,
          levels = unique(country[order(cagr)])
        )
      )
      
      global_x_padding <- ifelse(metric_global_avg < 0, -20, 5)
      group_x_padding <- ifelse(metric_group_avg < 0, -20, 5)
      
      plot <- 
        ggplot(group_data, aes(x = cagr, y = country)) +
        
        # Line segments colored based on the label
        geom_segment(aes(x = 0, xend = cagr, y = country, yend = country,
                         color = case_when(
                           .data$country == global_average_label ~ "blue",
                           .data$country == group_average_label ~ "red",
                           TRUE ~ "gray"
                         )),
                     linewidth = 0.3) +
        
        # Points colored for global and group averages
        geom_point(aes(color = case_when(
          .data$country == global_average_label ~ "blue",
          .data$country == group_average_label ~ "red",
          TRUE ~ "gray"
        )), size = 3) +
        
        # Annotation for Global Average
        geom_text(data = group_data %>% filter(country == global_average_label),
                  aes(x = cagr + global_x_padding, y = country, label = paste0(formatC(metric_global_avg, format="f", digits=1), '%')),
                  color = "blue", hjust = 0, size = 3.5) +
        
        # Annotation for Group Average
        geom_text(data = group_data %>% filter(country == group_average_label),
                  aes(x = cagr + group_x_padding, y = country, label = paste0(formatC(metric_group_avg, format="f", digits=1), '%')),
                  color = "red", hjust = 0, size = 3.5) +
        
        scale_color_identity(guide = "none") +
        
        # Color the y-axis labels for both averages
        scale_y_discrete(labels = function(y) {
          ifelse(y == global_average_label, paste0("<span style='color:blue;'><strong>", y, "</strong></span>"),
                 ifelse(y == group_average_label, paste0("<span style='color:red;'><strong>", y, "</strong></span>"), y))
        }) +
        
        labs(
          title = str_wrap(plot_title, width = 50) %>% gsub("\n", " \n", .),
          subtitle = plot_subtitle,
          x = paste0("CAGR from Latest Data Point to ", metric_label),
          y = element_blank()
        ) +

        scale_x_continuous(limits = c(
          min(0, min(group_data$cagr)) - ifelse(min(0, min(group_data$cagr)) < -10, 38, 18),
          max(100, max(group_data$cagr)) + 20
          ),
          labels = function(x) paste0(x, "%") 
        ) +
        
        theme_minimal() +
        theme(
          axis.text.y = element_markdown(size = 10),  # Enable markdown to render colored text
          axis.text.x = element_text(size = 10),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          plot.title = element_text(face = "bold", size = 14)
        )
        
      # Add the plot to the list
      plots[[group]] <<- plot
    })
  }
  
  plots
}


#' get_analysis_data
#'
#' @description Processes raw input data for analysis by cleaning variable names, adding relevant columns, 
#' and selecting only the necessary fields. This function prepares the data for downstream analysis.
#'
#' @param raw_analysis_data A data frame containing the raw data to be processed. The data must include columns for:
#'   - `country`: The country associated with the data point.
#'   - `variable`: Variable names.
#'   - `indicator`: A description of the indicator associated with the variable.
#'   - `short_label`: Short label for the variable.
#'   - `value`: The value of the data point.
#'   - `UN Continental Region`: The UN continental region of the data points.
#'   - `income_group`: The income group classification for the country.
#'   - `desirable_direction`: Indicates the desired direction for milestone analysis (e.g., -1 or 1).
#'   - Other necessary columns for analysis.
#'
#' @return A cleaned and processed data frame with the following columns:
#'   - `country`: The country associated with the data point.
#'   - `ISO3`: The ISO3 code for the country.
#'   - `variable`: Cleaned variable names.
#'   - `indicator`: Indicator associated with the variable.
#'   - `short_label`: Short label for the variable.
#'   - `year`: Year of the data point.
#'   - `value`: Value of the data point.
#'   - `un_continental_region`: UN continental region (renamed for clarity).
#'   - `income_group`: Income group classification for the country.
#'   - `desirable_direction`: Indicates whether increasing or decreasing is desirable.
#'   - `milestone_pctile`: Percentile used for milestone calculation (0.2 if decreasing is desirable, otherwise 0.8).
#'   - `weight`: Weight for the data point.
#'
#' @examples
#' # Prepare data for analysis
#' analysis_data <- get_analysis_data(raw_data)
get_analysis_data <- function(raw_analysis_data) {
  raw_analysis_data |> 
    mutate(
      variable = clean_analysis_data_variables(variable), # Cleans up a handful of minor issues from raw data
      un_continental_region = `UN Continental Region`,
      desirable_direction = as.integer(as.character(desirable_direction)),
      milestone_pctile = if_else(desirable_direction == -1, 0.2, 0.8)
    ) |> 
    select(
      country, ISO3, variable, indicator, short_label, year, value, un_continental_region, income_group, desirable_direction, milestone_pctile, weight, unit
    )
}


#' get_country_indicator_summary
#'
#' @description Summarizes the data availability for each country over time, identifying the first and latest years with data,
#' and includes additional regional and income group metadata.
#'
#' @param data A data frame containing the following columns:
#'   - `country`: The name of the country.
#'   - `year`: The year of observation.
#'   - `value`: The observed value (can include missing values).
#'   - `UN Continental Region`: The UN-defined continental region for the country.
#'   - `income_group`: The income group classification of the country.
#'
#' @return A data frame summarizing data availability for each country, with the following columns:
#'   - `country`: The name of the country.
#'   - `region`: The UN-defined continental region for the country.
#'   - `income_group`: The income group classification of the country.
#'   - `first_year_with_data`: The earliest year with data available for the country.
#'   - `latest_year_with_data`: The most recent year with data available for the country.
#'   - Columns for each year within the range (2000 to 2024), showing the number of non-missing values.
#'
#' @examples
#' # Get a summary of country indicator data
#' get_country_indicator_summary(indicator_data)
get_country_indicator_summary <- function(data) {
  min_year <- min(data$year)
  max_year <- max(data$year)
  
  min_year <- 2000
  max_year <- 2024
  
  year_data <- data |> 
    mutate(
      value = ifelse(is.na(value) | value == "", NA, value)
    ) |> 
    group_by(country, year) |> 
    summarise(non_missing_count = sum(!is.na(value)), .groups = "drop") |> 
    pivot_wider(
      names_from = year, 
      values_from = non_missing_count, 
      values_fill = 0
    ) |> 
    rowwise() |> 
    mutate(
      first_year_with_data = min(
        as.numeric(
          names(pick(as.character(min_year):as.character(max_year)))[which(c_across(as.character(min_year):as.character(max_year)) > 0)]
        )
      ),
      latest_year_with_data = max(
        as.numeric(
          names(pick(as.character(min_year):as.character(max_year)))[which(c_across(as.character(min_year):as.character(max_year)) > 0)]
        )
      )
    ) |> 
    ungroup() 
  
  year_data |> 
    left_join(
      data |> 
        mutate(
          region = `UN Continental Region`,
        ) |> 
        select(country, region, income_group) |> 
        distinct()
    ) |> 
    select(country, region, income_group, first_year_with_data, latest_year_with_data, everything()) |> 
    arrange(country)
}


#' get_income_groups
#'
#' @description Extracts and returns a sorted, unique list of income groups from the provided dataset.
#' The sorting is based on a predefined prefix order: "Low", "Lower", "Upper", "High".
#'
#' @param data A data frame containing a column named `income_group`.
#'   This column should include income group classifications as character strings.
#'
#' @return A character vector of sorted, unique income group names, ordered by the predefined prefix order.
#'
#' @examples
#' # Get unique income groups
#' get_income_groups(example_data)
get_income_groups <- function(data) {
  groups <- data |> filter(
    !is.na(income_group)
  ) |> 
    select("income_group") |>
    mutate(
      income_group = as.character(income_group)
    ) |> 
    unique()
  
  prefix_order <- c("Low", "Lower", "Upper", "High")
  
  groups |> 
    mutate(
      prefix_rank = match(
        sub(" .*", "", income_group), 
        prefix_order
      )
    ) |> 
    arrange(prefix_rank) |> 
    select(-prefix_rank) |> 
    pull(income_group)
}


#' get_income_group_col
#'
#' @description Returns the column name representing income groups in the provided dataset. 
#' Currently, this function assumes the column name is always `income_group`.
#'
#' @param data A data frame that contains a column representing income groups.
#'   The column is expected to be named `income_group`.
#'
#' @return A character string specifying the name of the income group column, i.e., `"income_group"`.
#'
#' @examples
#' # Example usage:
#' income_group_col <- get_income_group_col(data)
get_income_group_col <- function(data) {
  return("income_group")
}


#' get_income_group_summary
#'
#' @description Summarizes the dataset by income groups, providing the number of distinct countries and regions for each income group. The function also ensures income groups are arranged in a meaningful order based on their prefixes.
#'
#' @param data A data frame containing columns `income_group`, `country`, and `UN Continental Region`.
#'
#' @return A data frame with one row per income group, containing:
#'   - `income_group`: The income group name.
#'   - `n_countries`: The number of distinct countries in the income group.
#'   - `n_regions`: The number of distinct UN continental regions in the income group.
#'   The result is arranged by a meaningful order based on the prefixes of the income group (`Low`, `Lower`, `Upper`, `High`).
#'
#' @examples
#' # Example usage:
#' get_income_group_summary(data)
get_income_group_summary <- function(data) {
  region_col <- get_region_col(data)
  summary_data <- data |> 
    mutate(
      region = !!sym(region_col)
    ) |> 
    group_by(income_group) |>
    summarise(
      n_countries = n_distinct(country),
      n_regions = n_distinct(region)
    ) |> 
    filter(!is.na(income_group))
  
  
  prefix_order <- c("Low", "Lower", "Upper", "High")
  
  summary_data |> 
    mutate(
      prefix_rank = match(
        sub(" .*", "", income_group), 
        prefix_order
      )
    ) |> 
    arrange(prefix_rank) |> 
    select(-prefix_rank)
}


#' get_indicator_summary
#'
#' @description Summarizes key metrics for each indicator in the dataset, including data type, distinct values, reporting countries, and temporal data coverage.
#'
#' @param indicator_data A data frame containing the columns `variable`, `indicator`, `value`, and `year`. 
#'   The `extract_indicator_data` function can be used to extract this data from the raw dataset.
#'
#' @return A data frame summarizing each indicator, including:
#'   - `variable`: The indicator variable name.
#'   - `indicator`: The descriptive label for the indicator.
#'   - `data_type`: The data type of the indicator (`boolean`, `numeric`, or `character`).
#'   - `distinct_values`: The number of distinct values observed for the indicator across all years.
#'   - `years_of_data`: The number of years with data for the indicator.
#'   - `first_year`: The earliest year with data.
#'   - `first_year_reporting_countries`: The number of countries reporting in the first year.
#'   - `latest_year`: The most recent year with data.
#'   - `latest_year_reporting_countries`: The number of countries reporting in the most recent year.
#'   - `lowest_reporting_year`: The year with the fewest countries reporting data.
#'   - `lowest_year_reporting_countries`: The number of countries in the year with the fewest reports.
#'   - `highest_reporting_year`: The year with the most countries reporting data.
#'   - `highest_year_reporting_countries`: The count of countries in the year with the most reports.
#'   - `avg_countries_reporting_per_year`: The average number of countries reporting per year.
#'
#' @examples
#' # Example usage:
#' get_indicator_summary(data)
get_indicator_summary <- function(analysis_data) {
  data_type_distinct_values <- analysis_data |> 
    mutate(
      value = ifelse(is.na(value) | value == "", NA, value)  # Handle missing or empty values
    ) |> 
    filter(!is.na(value)) |>  # Keep only rows with valid data
    group_by(variable) |> 
    summarise(
      data_type = determine_data_type(value),  # Determine data type for each variable
      distinct_values = n_distinct(value, na.rm = TRUE),  # Total distinct values across all years
      .groups = "drop"
    )
  
  summary_metrics <- analysis_data |> 
    mutate(
      value = ifelse(is.na(value) | value == "", NA, value)  # Handle missing or empty values
    ) |> 
    filter(!is.na(value)) |>  # Keep only rows with valid data
    group_by(variable, year) |> 
    summarise(
      country_count = n(), 
      .groups = "drop"
    ) |> 
    group_by(variable) |> 
    summarise(
      years_of_data = n_distinct(year),  # Number of years with data
      first_year = min(year),  # First year with data
      first_year_reporting_countries = country_count[year == min(year)],  # Countries in the first year
      latest_year = max(year), # Latest year with data
      latest_year_reporting_countries = country_count[year == max(year)],  # Countries in the latest year
      lowest_reporting_year = year[which.min(country_count)],  # Year with the smallest count
      lowest_year_reporting_countries = min(country_count),  # Smallest count
      highest_reporting_year = year[which.max(country_count)],  # Year with the largest count
      highest_year_reporting_countries = max(country_count),  # Largest count
      avg_countries_reporting_per_year = round(mean(country_count), 1),  # Average reporting countries
      .groups = "drop"
    )
  
  summary_metrics |> 
    left_join(data_type_distinct_values, by = "variable") |> 
    left_join(analysis_data |> select(variable, indicator) |> distinct(), by = "variable") |>
    select(variable, indicator,  data_type, distinct_values, everything())
}


#' get_indicator_variable_metrics
#'
#' @description Processes a single indicator variable to compute metrics such as milestones, distances, and velocities. 
#' This function handles missing or non-numeric data and returns a detailed summary for the indicator.
#'
#' @param analysis_data A data frame containing the long-form data for all indicators, including `variable`, `value`, `country`, and `year`.
#'   The `get_analysis_data` function can be used to generate this data.
#' @param indicator_summary A data frame summarizing metadata for all indicators, including `variable` and `data_type`.
#'   The `get_indicator_summary` function can be used to generate this summary.
#' @param target_data A data frame containing target values for each indicator, including `variable` and `target`.
#'   The `get_target_data` function can be used to generate this data.
#' @param indicator_variable A string representing the name of the indicator variable to process.
#' @param target_year An integer representing the target year for velocity calculations.
#' @param data_patches A list of patches for correcting or adjusting indicator data, applied before calculations.
#'   These are located in `data_patches.R`
#'
#' @return A list containing:
#'   - `variable`: The name of the indicator variable.
#'   - `indicator`: A descriptive label for the indicator.
#'   - `short_label`: A short label for the indicator.
#'   - `message`: A status message indicating success, warnings, or failure.
#'   - `data`: A tibble with calculated metrics for each country, including milestones, distances, and velocities.
#'   - `meta`: A list of metadata, including:
#'       - `data_type`: The data type of the indicator variable.
#'       - `variable`: The indicator variable name.
#'       - `failing`: A logical flag indicating whether processing failed.
#'       - `warnings`: A logical flag indicating whether warnings occurred.
#'       - `failing_variables`: A list of variables that failed processing.
#'       - `warning_variables`: A list of variables with warnings.
#'
#' @examples
#' # Example usage
#' get_indicator_variable_metrics(
#'   analysis_data = analysis_data,
#'   indicator_summary = indicator_summary,
#'   target_data = target_data,
#'   indicator_variable = "safeh20",
#'   target_year = 2030,
#'   data_patches = data_patches
#' )
get_indicator_variable_metrics <- function(
  analysis_data, 
  indicator_summary,
  target_data,
  indicator_variable, 
  target_year,
  data_patches
) {
  # Debug:
  # indicator_variable <- "safeh20"
  # indicator_variable <- "childlabor"
  # indicator_variable <- "emint_cerealsnorice"
  # indicator_variable <- "aginGDP"
  # indicator_variable <- "agwaterdraw"
  # indicator_variable <- "accountability"
  region_col <- get_region_col(analysis_data)
  income_group_col <- get_income_group_col(analysis_data)
  weight_col <- get_weight_col()
  indicator_variable_metrics <- tibble()
  indicator_standardized_distance_metrics <- tibble()
  milestone_summary <- tibble()
  velocity_summary <- tibble()
  sdg_summary <- tibble()
  distance_region_plots <- NA
  distance_income_group_plots <- NA
  velocity_global_target_plots <- NA
  velocity_global_milestone_plots <- NA
  velocity_region_milestone_plots <- NA
  velocity_income_group_milestone_plots <- NA
  
  indicator_summary_data <- indicator_summary |> filter(variable == indicator_variable)
  failing_variables <- list()
  warning_variables <- list()
  
  # if indicator_summary$data_type for this variable is not numeric, return list with info
  data_type <- indicator_summary |> filter(variable == indicator_variable) |> pull(data_type)
  indicator_label <- analysis_data |> filter(variable == indicator_variable) |> pull(indicator) |> unique() |> as.character()
  indicator_short_label <- analysis_data |> filter(variable == indicator_variable) |> pull(short_label) |> unique() |> as.character()
  indicator_unit <- analysis_data |> filter(variable == indicator_variable) |> pull(unit) |> unique() |> as.character()
  desirable_direction <- analysis_data |> filter(variable == indicator_variable) |> pull(desirable_direction) |> unique() |> as.numeric()
  
  if(length(data_type) == 0 || data_type != "numeric") {
    message <- paste0("Skipped: indicator variable ", indicator_variable, " is not numeric.")
  } else{
    tryCatch(
      {
        indicator_data <- extract_indicator_data(analysis_data, indicator_variable, data_patches = data_patches)
        target_value <- get_target_value(indicator_variable, target_data)
        
        indicator_milestone_metrics <- calculate_milestone_metrics(indicator_data)
        indicator_distance_metrics <- calculate_distance_metrics(indicator_data, indicator_milestone_metrics, target_value)
        indicator_standardized_distance_metrics <- calculate_standardized_distance_metrics(indicator_data, region_col, income_group_col, weight_col)
        indicator_velocity_metrics <- calculate_velocity_metrics(indicator_data, indicator_milestone_metrics, target_value, target_year)
        indicator_sdg_cagr_metrics <- calculate_sdg_cagr_metrics(indicator_data, desirable_direction, target_value, target_year, baseline_calc_year=2015)
        
        indicator_metrics_countries_with_data <- indicator_milestone_metrics |> 
          mutate(
            target_value = target_value
          ) |> 
          inner_join(indicator_distance_metrics, by = c("variable", "country", "latest_year")) |>  # Narrows to latest year 
          rename(
            latest_year_value = value,
          ) |> 
          select(variable, country, latest_year, latest_year_value, everything()) |> # Rearrange
          inner_join(indicator_velocity_metrics |> select(-milestone_global, -milestone_region, -milestone_income_group), by = c("variable", "country", "latest_year")) |> 
          arrange(country)
        
        # Fill in empty countries
        countries_without_data <- analysis_data |> 
          filter(variable == indicator_variable) |> 
          select(country) |> 
          anti_join(indicator_metrics_countries_with_data, by = "country") |> 
          mutate(
            variable = indicator_variable,
          ) |>
          unique() |> 
          arrange(country)
        
        country_region_income_group <- analysis_data |>
          filter(variable == indicator_variable) |>
          select(country, ISO3, desirable_direction, milestone_pctile, all_of(c(region_col, income_group_col))) |>
          rename(
            region = !!region_col,
            income_group = !!income_group_col
          ) |>
          unique()

        indicator_sdg_cagr_metrics_to_merge <- indicator_sdg_cagr_metrics |>
          rename(
            sdg_baseline_year = baseline_year,
            sdg_baseline_value = baseline_year_value,
            sdg_latest_year = latest_year,
            sdg_latest_value = latest_year_value,
            sdg_cagr = cagr_sdg_baseline_to_latest,
            sdg_rating = sdg_value
          ) 
        
        indicator_variable_metrics <- indicator_metrics_countries_with_data |> 
          mutate(unit = indicator_unit) |>
          bind_rows(countries_without_data) |> 
          left_join(country_region_income_group, by = "country") |> 
          left_join(indicator_sdg_cagr_metrics_to_merge, by = c("country")) |> 
          select(variable, country, ISO3,  desirable_direction, unit, milestone_pctile, region, income_group, everything()) |>
          arrange(country)

        # Get summaries of these metrics by global, region, income group
        milestone_summary <- get_summary_milestones(indicator_variable_metrics, indicator_unit, target_value, region_col, income_group_col)
        velocity_summary <- get_summary_velocities(indicator_variable_metrics, indicator_unit, target_value, region_col, income_group_col)
        sdg_summary <- get_summary_sdg(indicator_variable_metrics)
        
        # Visualize: this will be extracted
        # Prepare the data
        distance_region_plots <- generate_distance_forest_plots(indicator_variable_metrics, milestone_summary, label = indicator_label, unit = indicator_unit, group_by = "region")
        distance_income_group_plots <- generate_distance_forest_plots(indicator_variable_metrics, milestone_summary, label = indicator_label, unit = indicator_unit, group_by = "income_group")
        
        # velocity viz
        velocity_global_target_plots <- if(!is.na(target_value)) {
          generate_velocity_plots(indicator_variable_metrics, velocity_summary, label = indicator_label, unit = indicator_unit, milestone = "global_target", group_by = "region", velocity_metric="cagr")
        } else {
          NULL
        }
        velocity_global_milestone_plots <- generate_velocity_plots(indicator_variable_metrics, velocity_summary, label = indicator_label, unit = indicator_unit, milestone = "global_milestone", group_by = "region", velocity_metric="cagr")
        velocity_region_milestone_plots <- generate_velocity_plots(indicator_variable_metrics, velocity_summary, label = indicator_label, unit = indicator_unit, milestone = "region_milestone", group_by = "region", velocity_metric="cagr")
        velocity_income_group_milestone_plots <- generate_velocity_plots(indicator_variable_metrics, velocity_summary, label = indicator_label, unit = indicator_unit, milestone = "income_group_milestone", group_by = "income_group", velocity_metric="cagr")
        
        save_forest_plots(distance_region_plots, indicator_variable, "region")
        save_forest_plots(distance_income_group_plots, indicator_variable, "income_group")
        save_forest_plots(velocity_global_target_plots, indicator_variable, "global_target")
        save_forest_plots(velocity_global_milestone_plots, indicator_variable, "global_milestone")
        save_forest_plots(velocity_region_milestone_plots, indicator_variable, "region_milestone")
        save_forest_plots(velocity_income_group_milestone_plots, indicator_variable, "income_group_milestone")
        
        message <- "Indicator variable successfully processed."
      },
      warning = function(w) {
        warning_variables <- c(warning_variables, indicator_variable)
        cat(paste0("⚠️ Warning processing ", indicator_variable, ": ", w$message, "\n\n"))
        message <- "Indicator variable processed with warnings."
      },
      error = function(e) {
        failing_variables <- c(failing_variables, indicator_variable)
        cat(paste0("💣 Error processing ", indicator_variable, ": ", e$message, "\n\n"))
        print(e)
        
        message <- "Failed to process indicator variable."
      }
    )  
  }
  
  list(
    variable = indicator_variable,
    indicator = indicator_label,
    short_label = indicator_short_label,
    message = message,
    data = indicator_variable_metrics,
    visualizations = list(
      distance = list(
        region = distance_region_plots,
        income_group = distance_income_group_plots
      ),
      velocity = list(
        global_target = velocity_global_target_plots,
        global_milestone = velocity_global_milestone_plots,
        region_milestone = velocity_region_milestone_plots,
        income_group_milestone = velocity_income_group_milestone_plots
      )
    ),
    summaries = list(
      indicators = indicator_summary_data,
      milestones = milestone_summary,
      velocities = velocity_summary,
      sdg = sdg_summary
    ),
    standardized_distance_metrics = indicator_standardized_distance_metrics,
    meta = list(
      data_type = data_type,
      indicator_unit = indicator_unit,
      desirable_direction = desirable_direction,
      milestone_summary = milestone_summary,
      variable = indicator_variable,
      failing = length(failing_variables) > 0,
      warnings = length(warning_variables) > 0,
      failing_variables = failing_variables,
      warning_variables = warning_variables
    )
  )
}


#' get_indicator_variables
#'
#' @description Extracts and returns a sorted, unique list of indicator variables from the provided dataset,
#' excluding any specified variables.
#'
#' @param data A data frame containing a column named `variable`.
#'   This column should include indicator variable names as character strings.
#' @param exclude A character vector of variable names to exclude from the output. Default is an empty vector.
#'
#' @return A character vector of sorted, unique indicator variable names, excluding any specified in `exclude`.
#'
#' @examples
#' # Get indicator variables excluding specific ones
#' get_indicator_variables(example_data, exclude = c("var1", "var2"))
get_indicator_variables <- function(data, exclude = c()) {
  data |>
    filter(
      !variable %in% exclude
    ) |>
    pull(variable) |>
    as.character() |>
    sort() |>
    unique()
}


#' get_metrics_data
#'
#' @description Generates a list of metrics data for all indicators in the provided dataset. 
#' The function processes raw data, applies data patches, computes milestones, distances, and velocities, 
#' and organizes the results by indicator.
#'
#' @param raw_analysis_data A data frame containing the raw input data for analysis.  This should be read in from a
#'   pre-processed CSV or RDS file from the FSCI 2024 repository: https://github.com/KateSchneider-FoodPol/FSCI_2024Interactions_Replication/tree/main/Output%20data
#' @param indicator_summary A data frame summarizing key characteristics of each indicator, including 
#'   data types and descriptive information.
#' @param raw_target_data A data frame containing target values for specific indicators. It must include 
#'   columns such as `variable` and `target`.
#' @param target_year An integer specifying the target year for calculating metrics (default is 2030).
#' @param data_patches A list of data patches to apply to specific indicators. Each patch should specify 
#'   changes to the raw data (default is an empty list).
#'
#' @return A named list where each element corresponds to an indicator. Each element is a list containing:
#'   - `variable`: The indicator variable name.
#'   - `indicator`: The descriptive name of the indicator.
#'   - `short_label`: A short label for the indicator.
#'   - `message`: A message summarizing the processing status (e.g., success, warning, or error).
#'   - `data`: A data frame with metrics for the indicator, including milestones, distances, and velocities.
#'   - `meta`: Metadata about the processing, such as data type, warnings, and failures.
#'
#' @examples
#' # Generate metrics data for all indicators
#' metrics_data <- get_metrics_data(
#'   raw_analysis_data = raw_analysis_data,
#'   indicator_summary = indicator_summary,
#'   raw_target_data = raw_target_data,
#'   target_year = 2030,
#'   data_patches = list()
#' )
get_metrics_data <- function(
    raw_analysis_data,
    indicator_summary,
    raw_target_data,
    target_year = 2030,
    data_patches = list()
) {
  analysis_data <- get_analysis_data(raw_analysis_data)
  target_data <- get_target_data(raw_target_data)
  indicator_variables <- get_indicator_variables(analysis_data)
  
  map(
    indicator_variables, 
    ~ get_indicator_variable_metrics(
        analysis_data,
        indicator_summary,
        target_data,
        .x,
        target_year,
        data_patches
    )
  ) |> 
    set_names(indicator_variables)
}


#' get_milestone_summaries_from_indicator_variable_metrics
#'
#' @description Extracts milestone summaries for global, regional, and income group levels from indicator variable metrics.
#' Returns a list containing global, regional, and income group milestone values.
#'
#' @param indicator_variable_metrics A data frame containing metrics for an indicator, including milestone values.
#' Must contain columns `milestone_global`, `milestone_region`, and `milestone_income_group`.
#' @param target_value (Optional) A numeric value representing the target milestone. Default: NA.
#'
#' @return A list with the a summary of global, regional, and income group milestones.
#'
#' @details
#' - The global milestone is extracted as the first non-NA value from the `milestone_global` column.
#' - Regional milestones are summarized by `region`, taking the first non-NA value per region.
#' - Income group milestones are summarized by `income_group`, taking the first non-NA value per group and sorting them
#'   by predefined levels: "Low income", "Lower middle income", "Upper middle income", "High income".
#'
#' @examples
#' # Example usage:
#' milestone_summaries <- get_milestone_summaries_from_indicator_variable_metrics(indicator_variable_metrics, target_value = 100)
#' milestone_summaries$global  # Global milestone
#' milestone_summaries$region  # Regional milestones
#' milestone_summaries$income_group  # Income group milestones
get_milestone_summaries_from_indicator_variable_metrics <- function(indicator_variable_metrics, target_value) {
  global_milestone <- indicator_variable_metrics |> filter(!is.na(milestone_global)) |> pull(milestone_global) |> first()
  region_milestones <- indicator_variable_metrics |> filter(!is.na(milestone_region)) |> group_by(region) |> summarise(milestone = first(milestone_region)) |> filter(!is.na(region))
  income_group_milestones <- indicator_variable_metrics |> 
    filter(!is.na(milestone_income_group)) |> 
    group_by(income_group) |> 
    summarise(milestone = first(milestone_income_group)) |> 
    filter(!is.na(income_group)) |> 
    mutate(
      income_group = factor(income_group, levels = c("Low income", "Lower middle income", "Upper middle income", "High income")) # Define factor levels for sorting
    ) |>
    arrange(income_group)

  list(
    target = target_value,
    global = global_milestone,
    region = region_milestones,
    income_group = income_group_milestones
  )
}


#' get_recent_data_values
#'
#' @description Extracts recent data values for each country, including the most recent year and value, 
#' as well as average values and years from the last three years of available data.
#'
#' @param indicator_data A data frame containing the indicator data. Must include columns specified by year_col, country_col, and value_col.
#' @param year_col The column name representing the year of the data (default: "year").
#' @param country_col The column name representing the country (default: "country").
#' @param value_col The column name representing the indicator value (default: "value").
#'
#' @return A data frame containing:
#' - latest_year: The most recent year with data for each country.
#' - latest_value: The indicator value for the most recent year.
#' - last_three_years: A comma-separated string of the last three years with data for each country.
#' - avg_value_last_three_years: The average of the indicator values for the last three years with data.
#'
#' @examples
#' # Example usage with indicator data
#' recent_values <- get_recent_data_values(
#'   indicator_data = analysis_data,
#'   year_col = "year",
#'   country_col = "country",
#'   value_col = "value"
#' )
get_recent_data_values <- function(
    indicator_data,
    year_col = "year",
    country_col = "country",
    value_col = "value"
) {
  indicator_data |> 
    filter(!is.na(!!sym(value_col))) |>  # Remove rows with NA values in the target column
    group_by(!!sym(country_col)) |> 
    reframe(
      latest_year = max(!!sym(year_col), na.rm = TRUE),
      latest_value = (!!sym(value_col))[which(!!sym(year_col) == max(!!sym(year_col), na.rm = TRUE))],
      last_three_years = paste(
        (!!sym(year_col))[!!sym(year_col) >= max(!!sym(year_col), na.rm = TRUE) - 2],
        collapse = ","
      ),
      avg_value_last_three_years = mean(
        (!!sym(value_col))[!!sym(year_col) %in% (!!sym(year_col))[!!sym(year_col) >= max(!!sym(year_col), na.rm = TRUE) - 2]],
        na.rm = TRUE
      )
    ) |> 
    unique()   # Remove duplicates. We only remove duplicate rows so no information is lost when calculating metrics.
}


#' get_region_summary
#'
#' @description Summarizes the dataset by UN continental regions, providing the number of distinct countries and income groups for each region.
#'
#' @param data A data frame containing columns `UN Continental Region`, `country`, and `income_group`.
#'
#' @return A data frame with one row per region, containing:
#'   - `region`: The UN continental region name.
#'   - `n_countries`: The number of distinct countries in the region.
#'   - `n_income_groups`: The number of distinct income groups in the region.
#'
#' @examples
#' # Example usage:
#' get_region_summary(data)
get_region_summary <- function(data) {
  region_col <- get_region_col(data)
  
  data |> 
    mutate(
      region = !!sym(region_col),
    ) |> 
    group_by(region) |>
    summarise(
      n_countries = n_distinct(country),
      n_income_groups = n_distinct(income_group)
    ) |> 
    arrange(region)
}

#'
#' @description Determines the correct column name for the UN continental region in a given dataset. 
#' This function checks for the presence of the column `UN Continental Region` or `un_continental_region` 
#' and returns the appropriate column name.
#'
#' @param data A data frame that may contain a column representing the UN continental region.
#'
#' @return A character string indicating the column name for the UN continental region. 
#' Returns `"UN Continental Region"` if it exists in the data, otherwise `"un_continental_region"`.
#'
#' @examples
#' # Example usage:
#' region_col <- get_region_col(data)
get_region_col <- function(data) {
  ifelse("UN Continental Region" %in% colnames(data), "UN Continental Region", "un_continental_region")
}


#' get_regions
#'
#' @description Extracts and returns a sorted, unique list of UN continental regions from the provided dataset. 
#' The function dynamically determines the correct column name for the region using `get_region_col()`.
#'
#' @param data A data frame that contains a column representing the UN continental regions.
#'   The column name can be either `UN Continental Region` or `un_continental_region`.
#'
#' @return A character vector of sorted, unique UN continental region names from the specified column.
#'
#' @examples
#' # Example usage:
#' regions <- get_regions(data)
get_regions <- function(data) {
  data |> pull(get_region_col(data)) |> as.character() |> sort() |> unique()
}


#' get_summary_milestones
#'
#' @description Generates a summary of milestone values for a given indicator, including global, regional, and income group milestones. 
#' The output is a standardized table with two columns: `Group` (e.g., "Global", "Region: [Region]", "Income Group: [Group]") and `Milestone`.
#'
#' @param indicator_variable_metrics A data frame containing metrics for an indicator, including milestone columns 
#'   (`milestone_global`, `milestone_region`, `milestone_income_group`).
#' @param indicator_unit The unit of the indicator (e.g., "USD", "people", "percent").
#' @param target_value The target value for the indicator.
#' @param region_col The column name representing the region.
#' @param income_group_col The column name representing the income group.
#'
#' @return A tibble with two columns:
#' - `Group`: The group type (Global, Region: [Region], or Income Group: [Group]).
#' - `Milestone`: The milestone value for each group.
#'
#' @examples
#' # Example usage with indicator metrics
#' summary_milestones <- get_summary_milestones(
#'   indicator_variable_metrics = indicator_variable_metrics,
#'   region_col = "region",
#'   income_group_col = "income_group"
#' )
get_summary_milestones <- function(indicator_variable_metrics, indicator_unit, target_value = NA, region_col = "un_continental_group", income_group_col = "income_group") {
  milestone_summaries <- get_milestone_summaries_from_indicator_variable_metrics(indicator_variable_metrics, target_value)
    
  global_milestone <- milestone_summaries$global
  region_milestones <- milestone_summaries$region
  income_group_milestones <- milestone_summaries$income_group
  
  # Scaffold out the summary table  
  if(!is.na(target_value)) {
    summary <- tibble(Group = "Target", Milestone = target_value)
  } else {
    summary <- tibble()
  }
 # Get average values
 global_avg_value <- indicator_variable_metrics |>
   filter(!is.na(latest_year_value)) |>
   summarise(avg_value = mean(latest_year_value, na.rm = TRUE)) |>
   pull(avg_value)
 
 region_avg_values <- indicator_variable_metrics |>
   filter(!is.na(latest_year_value)) |>
   group_by(region) |>
   summarise(avg_value = mean(latest_year_value, na.rm = TRUE)) |>
   filter(!is.na(region)) |>
   mutate(Group = paste0("Region: ", region)) |>
   select(Group, avg_values_latest_year = avg_value)
 
 income_group_avg_values <- indicator_variable_metrics |>
   filter(!is.na(latest_year_value)) |>
   group_by(income_group) |>
   summarise(avg_value = mean(latest_year_value, na.rm = TRUE)) |>
   filter(!is.na(income_group)) |>
   mutate(
     income_group = factor(income_group, levels = c("Low income", "Lower middle income", "Upper middle income", "High income")),
     Group = paste0("Income Group: ", income_group)
   ) |>
   arrange(income_group) |>
   select(Group, avg_values_latest_year = avg_value)
 
 target_avg_value <- if (!is.na(target_value)) {
   indicator_variable_metrics |>
     filter(!is.na(latest_year_value)) |>
     summarise(avg_value = mean(latest_year_value, na.rm = TRUE)) |>
     pull(avg_value)
 } else {
   NA
 }
 
 # Initial summary table
 summary <- bind_rows(
   summary,
   tibble(Group = "Global", Milestone = global_milestone),
   region_milestones |> 
     mutate(Group = paste0("Region: ", region)) |> 
     select(Group, Milestone = milestone),
   income_group_milestones |> 
     mutate(Group = paste0("Income Group: ", income_group)) |> 
     select(Group, Milestone = milestone)
 ) |> 
 left_join(
   bind_rows(
     tibble(Group = "Target", "avg_values_latest_year" = target_avg_value),
     tibble(Group = "Global", "avg_values_latest_year" = global_avg_value),
     region_avg_values,
     income_group_avg_values
   ),
   by = "Group"
 )
 
 ## Summarize distances
 # Global Average Distance
 global_metrics <- indicator_variable_metrics |>
   summarise(
     avg_distance = mean(distance_to_milestone_global, na.rm = TRUE),
     countries_total = n_distinct(country),
     countries_missing = sum(is.na(distance_to_milestone_global)),
     countries_contributing = countries_total - countries_missing
   ) |> 
   select(avg_distance, countries_total, countries_contributing, countries_missing)
 
 # Target Average Distance
 target_metrics <- if (!is.na(target_value)) {
   indicator_variable_metrics |>
     summarise(
       avg_distance = mean(distance_to_target, na.rm = TRUE),
       countries_total = n_distinct(country),
       countries_missing = sum(is.na(distance_to_target)),
       countries_contributing = countries_total - countries_missing
     ) |>
     select(avg_distance, countries_total, countries_contributing, countries_missing)
 } else {
   tibble(
     avg_distance = NA,
     countries_total = n_distinct(indicator_variable_metrics$country),
     countries_contributing = 0,
     countries_missing = n_distinct(indicator_variable_metrics$country)
   )
 }
 
 # Regional Average Distance
 region_avg_distances <- indicator_variable_metrics |>
   group_by(region) |>
   summarise(
     avg_distance = mean(distance_to_milestone_region, na.rm = TRUE),
     countries_total = n_distinct(country),
     countries_missing = sum(is.na(distance_to_milestone_region)),
     countries_contributing = countries_total - countries_missing
   ) |>
   mutate(Group = paste0("Region: ", region)) |>
   select(Group, avg_distance, countries_total, countries_contributing, countries_missing)
 
 # Income Group Average Distance
 income_group_avg_distances <- indicator_variable_metrics |>
   group_by(income_group) |>
   summarise(
     avg_distance = mean(distance_to_milestone_income_group, na.rm = TRUE),
     countries_total = n_distinct(country),
     countries_missing = sum(is.na(distance_to_milestone_income_group)),
     countries_contributing = countries_total - countries_missing
   ) |>
   mutate(Group = paste0("Income Group: ", income_group)) |>
   filter(!is.na(income_group)) |>
   select(Group, avg_distance, countries_total, countries_contributing, countries_missing)
 
 # Add back in distances
 summary |>
   left_join(
     bind_rows(
       tibble(
         Group = "Target",
         avg_distance = target_metrics$avg_distance,
         countries_total = target_metrics$countries_total,
         countries_contributing = target_metrics$countries_contributing,
         countries_missing = target_metrics$countries_missing
       ),
       tibble(
         Group = "Global",
         avg_distance = global_metrics$avg_distance,
         countries_total = global_metrics$countries_total,
         countries_contributing = global_metrics$countries_contributing,
         countries_missing = global_metrics$countries_missing
       ),
       region_avg_distances |>
         select(Group, avg_distance, countries_total, countries_contributing, countries_missing),
       income_group_avg_distances |>
         select(Group, avg_distance, countries_total, countries_contributing, countries_missing)
     ),
     by = "Group"
   ) |> 
   mutate(
     Milestone = round(Milestone, 2),
     Unit = indicator_unit,
     avg_values_latest_year = round(avg_values_latest_year, 2),
     avg_distance = round(avg_distance, 2),
     missing_pct = round(100 * countries_missing / countries_total, 1),
   ) |> 
   rename(
     `Average Value (Latest Year)` = avg_values_latest_year,
     `Average Distance To Target/Milestone` = avg_distance,
     `Countries Total` = countries_total,
     `Countries Contributing` = countries_contributing,
     `Countries Missing` = countries_missing,
     `Missing (%)` = missing_pct
   )
}

get_summary_sdg <- function(indicator_variable_metrics, cagr_version = "latest") {
  desired_order <- c("On track", "Moderate progress", "Marginal progress", "Stagnation", "Regression")
  
  indicator_variable_metrics |> 
    mutate(
      sdg_rating = factor(sdg_rating, levels = desired_order),
      sdg_rating = replace_na(as.character(sdg_rating), "No data") 
    ) |>
    group_by(sdg_rating) |> 
    summarize(
      n = n(),
      .groups = "drop"
    ) |> 
    complete(
      sdg_rating = desired_order, fill = list(n = 0)
    ) |> 
    arrange(factor(sdg_rating, levels = desired_order)) |> 
    rename(
      `SDG Progress Assessment` = sdg_rating,
      `Count` = n
    )
  
}


#' get_summary_velocities
#'
#' @description Computes and returns velocity summaries (CAGR and Linear Growth) for target, global, 
#' regional, and income group milestones based on indicator metrics.
#'
#' @param indicator_variable_metrics A data frame containing metrics for an indicator, including milestone and velocity values.
#' @param indicator_unit The unit of the indicator (e.g., "USD").
#' @param target_value (Optional) A numeric value representing the target to include in the velocity summaries. Default is `NA`.
#' @param region_col The column name representing the region grouping (default: "un_continental_group").
#' @param income_group_col The column name representing the income group grouping (default: "income_group").
#'
#' @return A list containing two tibbles:
#' - `cagr`: Summary data for CAGR (Compound Annual Growth Rate) metrics.
#' - `linear_growth`: Summary data for Linear Growth metrics.
#'
#' Each tibble contains the following columns:
#' - `Group`: The category (Target, Global, Region, or Income Group).
#' - `Milestone`: The milestone value for the group.
#' - `Countries Included`: The number of countries included in the calculation.
#' - `Avg Latest Value`: The average of the `latest_year_value`.
#' - `Avg Growth Rate Required (From Latest Value)`: The average CAGR or Linear Growth to reach the milestone by 2030 using the latest year value.
#' - `Avg Latest Value (3-Year Avg)`: The average of the `avg_value_last_three_years`.
#' - `Avg Growth Rate (From 3-Year Avg)`: The average CAGR or Linear Growth to reach the milestone by 2030 using the 3-year average.
#'
#' @examples
#' velocity_summaries <- get_summary_velocities(
#'   indicator_variable_metrics,
#'   target_value = 100,
#'   region_col = "region",
#'   income_group_col = "income_group"
#' )
get_summary_velocities <- function(indicator_variable_metrics, indicator_unit, target_value = NA, region_col = "un_continental_group", income_group_col = "income_group") {
  milestone_summaries <- get_milestone_summaries_from_indicator_variable_metrics(indicator_variable_metrics, target_value)
  
  global_milestone <- milestone_summaries$global
  region_milestones <- milestone_summaries$region
  income_group_milestones <- milestone_summaries$income_group
  
  cagr_summaries <- get_velocity_summary_data(indicator_variable_metrics, indicator_unit, milestone_summaries, "cagr")
  linear_growth_summaries <- get_velocity_summary_data(indicator_variable_metrics, indicator_unit, milestone_summaries, "linear")
  
  list(
    cagr = cagr_summaries,
    linear_growth = linear_growth_summaries
  )
}


#' get_target_data
#'
#' @description Processes raw data to extract and rename target-related columns, and formats the data for further analysis.
#'
#' @param raw_data A data frame containing target-related information with specific column names.
#'   Expected columns include:
#'   - `Variable name`
#'   - `Indicator`
#'   - `Unit`
#'   - `Thematic Grouping`
#'   - `Target (yes/no)`
#'   - `Target Global (value)`
#'
#' @return A processed data frame with the following columns:
#'   - `variable`: The name of the variable.
#'   - `has_target`: A logical value indicating whether a target exists.
#'   - `target`: The global target value.
#'
#' @examples
#' # Process raw target data
#' get_target_data(raw_target_data)
get_target_data <- function(raw_data) {
  target_data <- raw_data |> 
    rename(
      variable = `Variable name`,
      indicator = `Indicator`,
      unit = `Unit`,
      thematic_grouping = `Thematic Grouping`,
      has_target = `Target (yes/no)`,
      target = `Target Global (value)`,
    ) |> 
    mutate(
      has_target = if_else(has_target == "Yes", TRUE, FALSE)
    ) |> 
    select(
      variable, has_target, target
    )
}


#' get_target_value
#'
#' @description Retrieves the target value for a specific indicator variable from the target data.
#'
#' @param indicator_variable A character string specifying the indicator variable to search for in the target data.
#' @param target_data A data frame containing target-related information, including the `variable` and `target` columns.
#'
#' @return A numeric value representing the target for the specified indicator variable.
#'   If no target is found, the function returns `NA`.
#'
#' @examples
#' get_target_value("safeh20", target_data)
get_target_value <- function(indicator_variable, target_data) {
  value <- target_data |> 
    filter(variable == indicator_variable) |> 
    pull(target) |> 
    as.numeric()
  
  if(length(value) == 0) {
    NA
  } else {
    value
  }
}


#' get_un_regions
#'
#' @description Extracts and returns a sorted, unique list of UN continental regions from the provided dataset. 
#' The function dynamically determines the column name for the region based on the dataset's column names.
#'
#' @param data A data frame containing a column for UN continental regions. 
#'   The column can be named either `un_continental_region` or `UN Continental Region`.
#'
#' @return A character vector of sorted, unique UN continental region names from the specified column.
#'
#' @examples
#' # Get unique UN regions
#' get_un_regions(example_data)
get_un_regions <- function(data) {
  region_col <- region_col <- get_region_col(data)
  data |> pull(!!sym(region_col)) |> as.character() |> sort() |> unique()
}


#' get_velocity_summary_data
#'
#' @description Prepares the summary data for velocity metrics (CAGR or Linear Growth) grouped by target, global,
#' region, and income group milestones.
#'
#' @param indicator_variable_metrics A data frame containing metrics for an indicator, including milestone values.
#' @param indicator_unit A character string specifying the unit of the indicator.
#' @param milestone_summaries A list of milestone summaries, including global, region, and income group milestones.
#' @param growth_type A character string indicating the type of growth metric to calculate. Options are "cagr" or "linear".
#'
#' @return A tibble summarizing the growth metrics, including:
#' - `Group`: The category (Target, Global, Region, or Income Group).
#' - `Milestone`: The milestone value for the group.
#' - `Countries Included`: The number of countries included in the calculation.
#' - `Avg Latest Value`: The average of the `latest_year_value`.
#' - `Avg Growth Rate Required (From Latest Value)`: The average growth rate (CAGR or Linear Growth) to reach the milestone by 2030 using the latest year value.
#' - `Avg Latest Value (3-Year Avg)`: The average of the `avg_value_last_three_years`.
#' - `Avg Growth Rate (From 3-Year Avg)`: The average growth rate (CAGR or Linear Growth) to reach the milestone by 2030 using the 3-year average.
#'
#' @examples
#' summary_data <- get_velocity_summary_data(
#'   indicator_variable_metrics,
#'   milestone_summaries,
#'   growth_type = "cagr"
#' )
get_velocity_summary_data <- function(indicator_variable_metrics,  indicator_unit, milestone_summaries, growth_type = "cagr") {
  if (!growth_type %in% c("cagr", "linear")) {
    stop("Invalid growth_type. Options are 'cagr' or 'linear'.")
  }
  
  target <- milestone_summaries$target
  
  if (growth_type == "cagr") {
    latest_year_col <- "from_latest_year_cagr_required_to_hit"
    three_year_avg_col <- "from_mean_last_3_years_cagr_required_to_hit"
  } else {
    latest_year_col <- "from_latest_year_linear_growth_rate_required_to_hit"
    three_year_avg_col <- "from_mean_last_3_years_linear_growth_rate_required_to_hit"
  }
  
  # Target Data
  target_data <- if (!is.na(target)) {
    tibble(
      Group = "Target",
      Milestone = milestone_summaries$target,
      `Countries Included` = indicator_variable_metrics |> 
        filter(!is.na(!!sym(paste0(latest_year_col, "_target")))) |> 
        summarise(countries_included = n_distinct(country)) |> 
        pull(countries_included),
      `Avg Latest Value` = indicator_variable_metrics |> 
        summarise(avg_value = mean(latest_year_value, na.rm = TRUE)) |> 
        pull(avg_value),
      `Avg Growth Rate Required (From Latest Value)` = indicator_variable_metrics |> 
        summarise(avg_growth = mean(!!sym(paste0(latest_year_col, "_target")), na.rm = TRUE)) |> 
        pull(avg_growth),
      `Avg Latest Value (3-Year Avg)` = indicator_variable_metrics |> 
        summarise(avg_value = mean(avg_value_last_three_years, na.rm = TRUE)) |> 
        pull(avg_value),
      `Avg Growth Rate (From 3-Year Avg)` = indicator_variable_metrics |> 
        summarise(avg_growth = mean(!!sym(paste0(three_year_avg_col, "_target")), na.rm = TRUE)) |> 
        pull(avg_growth)
    )
  } else {
    tibble()
  }
  
  # Global Data
  global_data <- tibble(
    Group = "Global",
    Milestone = milestone_summaries$global,
    `Countries Included` = indicator_variable_metrics |> 
      filter(!is.na(!!sym(paste0(latest_year_col, "_milestone_global")))) |> 
      summarise(countries_included = n_distinct(country)) |> 
      pull(countries_included),
    `Avg Latest Value` = indicator_variable_metrics |> 
      summarise(avg_value = mean(latest_year_value, na.rm = TRUE)) |> 
      pull(avg_value),
    `Avg Growth Rate Required (From Latest Value)` = indicator_variable_metrics |> 
      summarise(avg_growth = mean(!!sym(paste0(latest_year_col, "_milestone_global")), na.rm = TRUE)) |> 
      pull(avg_growth),
    `Avg Latest Value (3-Year Avg)` = indicator_variable_metrics |> 
      summarise(avg_value = mean(avg_value_last_three_years, na.rm = TRUE)) |> 
      pull(avg_value),
    `Avg Growth Rate (From 3-Year Avg)` = indicator_variable_metrics |> 
      summarise(avg_growth = mean(!!sym(paste0(three_year_avg_col, "_milestone_global")), na.rm = TRUE)) |> 
      pull(avg_growth)
  )
  
  # Region data
  region_data <- milestone_summaries$region |> 
    mutate(region = as.character(region)) |>
    mutate(
      Group = paste0("Region: ", region),
      Milestone = milestone,
      `Avg Latest Value` = map_dbl(region, ~ {
        filtered_data <- indicator_variable_metrics |> 
          mutate(region = as.character(region)) |> 
          filter(region == .x)
        mean(filtered_data$latest_year_value, na.rm = TRUE)
      }),
      `Avg Growth Rate Required (From Latest Value)` = map_dbl(region, ~ {
        filtered_data <- indicator_variable_metrics |> 
          mutate(region = as.character(region)) |> 
          filter(region == .x)
        mean(filtered_data$from_latest_year_cagr_required_to_hit_milestone_region, na.rm = TRUE)
      }),
      `Avg Latest Value (3-Year Avg)` = map_dbl(region, ~ {
        filtered_data <- indicator_variable_metrics |> 
          mutate(region = as.character(region)) |> 
          filter(region == .x)
        mean(filtered_data$avg_value_last_three_years, na.rm = TRUE)
      }),
      `Avg Growth Rate (From 3-Year Avg)` = map_dbl(region, ~ {
        filtered_data <- indicator_variable_metrics |> 
          mutate(region = as.character(region)) |> 
          filter(region == .x)
        mean(filtered_data$from_mean_last_3_years_cagr_required_to_hit_milestone_region, na.rm = TRUE)
      }),
      `Countries Included` = map_dbl(region, ~ {
        filtered_data <- indicator_variable_metrics |> 
          mutate(region = as.character(region)) |> 
          filter(region == .x)
        n_distinct(filtered_data$country)
      })
    ) |> 
    select(Group,
       Milestone, 
       `Countries Included`, 
       `Avg Latest Value`, 
       `Avg Growth Rate Required (From Latest Value)`, 
       `Avg Latest Value (3-Year Avg)`, 
       `Avg Growth Rate (From 3-Year Avg)`)
  
  # Income group
  income_group_data <- milestone_summaries$income_group |> 
    mutate(income_group = as.character(income_group)) |> 
    mutate(
      Group = paste0("Income Group: ", income_group),
      Milestone = milestone,
      `Avg Latest Value` = map_dbl(income_group, ~ {
        filtered_data <- indicator_variable_metrics |> 
          mutate(income_group = as.character(income_group)) |> 
          filter(!is.na(income_group) & income_group == .x)
        if (nrow(filtered_data) == 0) {
          return(NA)
        }
        mean(filtered_data$latest_year_value, na.rm = TRUE)
      }),
      `Avg Growth Rate Required (From Latest Value)` = map_dbl(income_group, ~ {
        filtered_data <- indicator_variable_metrics |> 
          mutate(income_group = as.character(income_group)) |> 
          filter(!is.na(income_group) & income_group == .x)
        if (nrow(filtered_data) == 0) {
          return(NA)
        }
        mean(filtered_data$from_latest_year_cagr_required_to_hit_milestone_income_group, na.rm = TRUE)
      }),
      `Avg Latest Value (3-Year Avg)` = map_dbl(income_group, ~ {
        filtered_data <- indicator_variable_metrics |> 
          mutate(income_group = as.character(income_group)) |> 
          filter(!is.na(income_group) & income_group == .x)
        if (nrow(filtered_data) == 0) {
          return(NA)
        }
        mean(filtered_data$avg_value_last_three_years, na.rm = TRUE)
      }),
      `Avg Growth Rate (From 3-Year Avg)` = map_dbl(income_group, ~ {
        filtered_data <- indicator_variable_metrics |> 
          mutate(income_group = as.character(income_group)) |> 
          filter(!is.na(income_group) & income_group == .x)
        if (nrow(filtered_data) == 0) {
          return(NA)
        }
        mean(filtered_data$from_mean_last_3_years_cagr_required_to_hit_milestone_income_group, na.rm = TRUE)
      }),
      `Countries Included` = map_dbl(income_group, ~ {
        filtered_data <- indicator_variable_metrics |> 
          mutate(income_group = as.character(income_group)) |> 
          filter(!is.na(income_group) & income_group == .x)
        n_distinct(filtered_data$country)
      })
    ) |> 
    select(Group,
           Milestone,
           `Countries Included`,
           `Avg Latest Value`,
           `Avg Growth Rate Required (From Latest Value)`,
           `Avg Latest Value (3-Year Avg)`,
           `Avg Growth Rate (From 3-Year Avg)`)
  
  bind_rows(target_data, global_data, region_data, income_group_data) |> 
    mutate(
      `Milestone` = round(`Milestone`, 2),
      `Unit` = indicator_unit,
      `Avg Latest Value` = round(`Avg Latest Value`, 2),
      `Avg Growth Rate Required (From Latest Value)` = round(`Avg Growth Rate Required (From Latest Value)`, 2),
      `Avg Latest Value (3-Year Avg)` = round(`Avg Latest Value (3-Year Avg)`, 2),
      `Avg Growth Rate (From 3-Year Avg)` = round(`Avg Growth Rate (From 3-Year Avg)`, 2)
    ) |>
    mutate(
      across(
        c(
          `Milestone`,
          `Avg Latest Value`,
          `Avg Growth Rate Required (From Latest Value)`,
          `Avg Latest Value (3-Year Avg)`,
          `Avg Growth Rate (From 3-Year Avg)`
        ),
        ~ ifelse(is.na(.x) | is.nan(.x), "", as.character(.x))
      )
    )
}

#' get_weight_col
#' 
#' @description Returns the name of the column in the data frame that contains weights. This is defensive so we can change the column name in one place.
get_weight_col <- function() {
  'weight'
}

#' save_forest_plots
#'
#' @description Saves a list of forest plots to PNG files, organized by variable names and grouping type (e.g., region or income group).
#'
#' @param plots A named list of ggplot objects, where each name corresponds to a region or income group.
#' @param variable_name A character string specifying the name of the variable being plotted (used in file naming).
#' @param group_by A character string indicating the grouping type, either `"region"` or `"income_group"`, used in file naming.
#' @param output_dir A character string specifying the directory to save the plots. Default is `"output/images"`.
#' @param width Numeric value specifying the width of the saved plot in inches. Default is `8`.
#' @param height Numeric value specifying the height of the saved plot in inches. Default is `6`.
#'
#' @return None. Saves PNG files to the specified output directory.
#'
#' @examples
#' # Save forest plots for "safeh20" by region
#' save_forest_plots(plots_by_region, variable_name = "safeh20", group_by = "region")
#'
#' # Save forest plots for "safeh20" by income group
#' save_forest_plots(plots_by_income, variable_name = "safeh20", group_by = "income_group")
save_forest_plots <- function(plots, variable_name, group_by, output_dir = "output/images", width = 10, height = 8) {
  # Create the output directory for the variable if it doesn't exist
  variable_dir <- file.path(output_dir, variable_name)
  if (!dir.exists(variable_dir)) {
    dir.create(variable_dir, recursive = TRUE)
  }
  
  # Save each plot
  for (plot_name in names(plots)) {
    file_name <- paste0(group_by, "_plot_", plot_name, ".png")
    file_path <- file.path(variable_dir, file_name)
    
    ggsave(
      filename = file_path,
      plot = plots[[plot_name]],
      width = width,
      height = height,
      units = "in"
    )
  }
}


#' safe_summary_value
#'
#' @description Safely converts a value to a character string, replacing `NA`, `NULL`, or zero-length values with an empty string (`""`).
#'
#' @param value The input value to be checked and converted. It can be of any type.
#'
#' @return A character string. If the input value is `NA`, `NULL`, or has a length of `0`, it returns an empty string (`""`). Otherwise, it returns the character representation of the input value.
#'
#' @examples
#' # Example usage of safe_summary_value
#' safe_summary_value(NA)          # Returns ""
#' safe_summary_value(NULL)        # Returns ""
#' safe_summary_value("")          # Returns ""
#' safe_summary_value("example")   # Returns "example"
#' safe_summary_value(42)          # Returns "42"
#'
safe_summary_value <- function(value) {
  if (is.null(value) || length(value) == 0 || is.na(value)) {
    return("")
  }
  return(as.character(value))
}


#' summarize_indicator_summary
#'
#' @description Formats an `indicator_summary` object into a key-value pair tibble for improved readability. 
#' The output includes details such as variable name, data type, number of distinct values, reporting years, 
#' and other relevant metrics from the `indicator_summary` data.
#'
#' @param indicator_summary A data frame containing a single row summarizing an indicator.
#'   This data can be extracted using the `get_indicator_summary` function.`
#'
#' @return A tibble with two columns:
#' - `Key`: A description of each metric.
#' - `Value`: The corresponding value from the `indicator_summary`.
#'
#' @examples
#' indicator_summary <- get_indicator_summary(extract_indicator_data(analysis_data, "emint_beef"))
#' summarize_indicator_summary(indicator_summary)
summarize_indicator_summary <- function(indicator_summary) {
  indicator_summary <- indicator_summary |> as.list()
  
  tibble(
    Key = c(
      "Unique Values Count",
      "Years of Data",
      "First Year",
      "Countries Reporting (First Year)",
      "Latest Year",
      "Countries Reporting (Latest Year)",
      "Year with Fewest Reporting Countries",
      "Countries Reporting (Lowest Year)",
      "Year with Most Reporting Countries",
      "Countries Reporting (Highest Year)",
      "Average Reporting Countries Per Year"
    ),
    Value = c(
      safe_summary_value(indicator_summary$distinct_values),
      safe_summary_value(indicator_summary$years_of_data),
      safe_summary_value(indicator_summary$first_year),
      safe_summary_value(indicator_summary$first_year_reporting_countries),
      safe_summary_value(indicator_summary$latest_year),
      safe_summary_value(indicator_summary$latest_year_reporting_countries),
      safe_summary_value(indicator_summary$lowest_reporting_year),
      safe_summary_value(indicator_summary$lowest_year_reporting_countries),
      safe_summary_value(indicator_summary$highest_reporting_year),
      safe_summary_value(indicator_summary$highest_year_reporting_countries),
      safe_summary_value(round(indicator_summary$avg_countries_reporting_per_year, 1))
    )
  )
}


#' summarize_metrics_collection
#'
#' @description Summarizes the metadata and key attributes of a single indicator's metrics collection into a tibble.
#' This function extracts the variable name, data type, indicator description, and short label for presentation or further analysis.
#'
#' @param indicator_variable_metrics A list containing the metadata and metrics for a single indicator variable. 
#'   This object should have been created by a function like `get_indicator_variable_metrics`.
#'
#' @return A tibble with the following columns:
#' - `variable`: The name of the indicator variable.
#' - `data_type`: The data type of the indicator variable (e.g., "numeric", "boolean").
#' - `indicator`: The description of the indicator.
#' - `short_label`: A short label for the indicator.
#'
#' @examples
#' # Example usage
#' indicator_metrics <- get_indicator_variable_metrics(
#'   analysis_data = analysis_data, 
#'   indicator_summary = indicator_summary, 
#'   target_data = target_data, 
#'   indicator_variable = "safeh20", 
#'   target_year = 2030, 
#'   data_patches = list()
#' )
#'
#' summary <- summarize_metrics_collection(indicator_metrics)
#' print(summary)
summarize_metrics_collection <- function(indicator_variable_metrics) {
  
  
  tibble(
    Key = c("Variable", "Data Type", "Indicator", "Short Label"),
    Value = c(
      safe_summary_value(indicator_variable_metrics$variable), 
      safe_summary_value(indicator_variable_metrics$meta$data_type), 
      safe_summary_value(indicator_variable_metrics$indicator), 
      safe_summary_value(indicator_variable_metrics$short_label)
    )
  )
}

#' summarize_metrics_milestones
#'
#' @description Summarizes key milestone metrics for a given indicator, region, and income group. 
#' Extracts milestone values and compiles them into a structured tibble for reporting or further analysis.
#'
#' @param indicator_variable_metrics A list or data frame containing metrics for the specified indicator. 
#' Must include a `milestones` element with milestone values.
#' @param region_summary (Optional) A summary of regional metrics, used for context or additional milestone calculations.
#' @param income_group_summary (Optional) A summary of income group metrics, used for context or additional milestone calculations.
#'
#' @return A tibble containing two columns:
#' - `Key`: The label of the milestone (e.g., "Global Milestone", "Milestone 2", etc.).
#' - `Value`: The milestone value extracted from `indicator_variable_metrics$milestones`.
#'
#' @examples
#' # Example usage
#' milestone_summary <- summarize_metrics_milestones(
#'   indicator_variable_metrics = list(
#'     milestones = c(0.1, 0.25, 0.5, 0.75, 0.9)
#'   ),
#'   region_summary = region_metrics,
#'   income_group_summary = income_group_metrics
#' )
#'
#' print(milestone_summary)
summarize_metrics_milestones <- function(indicator_variable_metrics, region_summary, income_group_summary) {
  tibble(
    Key = c("Global Milestone", "Milestone 2", "Milestone 3", "Milestone 4", "Milestone 5"),
    Value = c(
      safe_summary_value(indicator_variable_metrics$milestones[1]), 
      safe_summary_value(indicator_variable_metrics$milestones[2]), 
      safe_summary_value(indicator_variable_metrics$milestones[3]), 
      safe_summary_value(indicator_variable_metrics$milestones[4]), 
      safe_summary_value(indicator_variable_metrics$milestones[5])
    )
  )
}


#' summarize_standardized_distance_metrics
#'
#' @description Summarizes standardized distance metrics for an indicator by combining global, regional, and income group-level metrics. 
#' Provides a consolidated dataset and highlights the metrics for the latest year.
#'
#' @param indicator_standardized_distance_metrics A list containing standardized distance metrics for global, region, and income group levels.
#' Each element of the list should be a data frame with standardized distance metrics.
#'
#' @return A list containing:
#' - `all`: A tibble combining global, regional, and income group metrics with columns:
#'   - `year`: The year of the metric.
#'   - `group`: The grouping level ("Global", "Region", or "Income Group").
#'   - `min_max_scaled_wtd`: The scaled standardized distance metric.
#' - `latest_year`: A subset of the `all` tibble, filtered to only include metrics from the latest year.
#'
#' @examples
#' # Example usage
#' standardized_metrics <- summarize_standardized_distance_metrics(
#'   indicator_standardized_distance_metrics = list(
#'     global = tibble(year = 2021:2022, min_max_scaled_wtd = c(0.75, 0.8)),
#'     region = tibble(year = 2021:2022, min_max_scaled_region_wtd = c(0.7, 0.78)),
#'     income_group = tibble(year = 2021:2022, min_max_scaled_income_group_wtd = c(0.65, 0.72))
#'   )
#' )
#'
summarize_standardized_distance_metrics <- function(indicator_standardized_distance_metrics) {
  global <- indicator_standardized_distance_metrics$global |>
    mutate(group = "Global") |>
    rename(
      min_max_scaled_vs_global_wtd = min_max_scaled_wtd,
      min_max_mean_scaled_vs_global_wtd = min_max_mean_scaled_wtd
    ) |>
    select(year, group, min_max_scaled_vs_global_wtd, min_max_mean_scaled_vs_global_wtd) 
  
  region <- indicator_standardized_distance_metrics$region |> 
    mutate(group = region) |>
    rename(
      min_max_scaled_vs_global_wtd = min_max_scaled_region_vs_global_wtd,
      min_max_mean_scaled_vs_global_wtd = min_max_mean_scaled_region_vs_global_wtd
    ) |> 
    select(year, group, min_max_scaled_vs_global_wtd, min_max_mean_scaled_vs_global_wtd)
  
  income_group <- indicator_standardized_distance_metrics$income_group |>
    mutate(group = income_group) |>
    rename(
      min_max_scaled_vs_global_wtd = min_max_scaled_income_group_wtd,
      min_max_mean_scaled_vs_global_wtd = min_max_mean_scaled_income_group_wtd
    ) |> 
    select(year, group, min_max_scaled_vs_global_wtd, min_max_mean_scaled_vs_global_wtd) 
  
  # order by Low, lower middle income, upper middle income, high income
  income_group <- income_group |>
    mutate(group = fct_relevel(group, "Low income", "Lower middle income", "Upper middle income", "High income")) |> 
    arrange(group)
  
  all <- bind_rows(global, region, income_group)  |>
    mutate(
      year = as.integer(year),
      min_max_scaled_vs_global_wtd = round(min_max_scaled_vs_global_wtd, 2),
      min_max_mean_scaled_vs_global_wtd = round(min_max_mean_scaled_vs_global_wtd, 2)
    ) |> 
    rename(
      `Latest Year` = year,
      `Group` = group,
      `Min-Max` = min_max_scaled_vs_global_wtd,
      `Min-Max Mean-Scaled` = min_max_mean_scaled_vs_global_wtd
    )
  
  latest_year <- all |> 
    group_by(Group) |>
    filter(`Latest Year` == max(`Latest Year`)) |>
    ungroup()
  
  list(
    all = all,
    latest_year = latest_year
  )
}
