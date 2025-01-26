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
    value_col = "value"
) {
  distance_starting_data <- get_recent_data_values(indicator_data, year_col, country_col, value_col)
  
  indicator_milestone_metrics |>
    left_join(
      indicator_data |> select(all_of(c(variable_col, year_col, country_col, value_col))),
      by = c("variable", "year", "country")
    ) |> 
    inner_join(
      distance_starting_data,
      by = c(c("year" = "latest_year"), country_col)
    ) |>
    rename(latest_year = year) |> 
    mutate(
      distance_to_milestone_global = value - milestone_global,
      distance_to_milestone_region = value - milestone_region,
      distance_to_milestone_income_group = value - milestone_income_group,
      distance_to_target = ifelse(is.na(target_value), NA, value - target_value)
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
#' @param regions_col The column name representing regions in the dataset (default: `"un_continental_region"`).
#'   Note: In the raw data, this column is named `UN Continental Region`. To avoid issues using awkward `backtick`
#'   syntax, before using this function, rename the column to `un_continental_region`.
#' @param income_groups_col The column name representing income groups in the dataset (default: `"income_group"`).
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
#'   regions_col = "un_continental_region",
#'   income_groups_col = "income_group",
#'   variable_col = "variable",
#'   year_col = "year",
#'   country_col = "country",
#'   value_col = "value"
#' )
calculate_milestone_metrics <- function(
    indicator_data,
    regions_col = "un_continental_region",
    income_groups_col = "income_group",
    variable_col = "variable",
    year_col = "year",
    country_col = "country",
    value_col = "value"
) {
  milestone_global_data <- calculate_milestones(indicator_data, variable_col, year_col, value_col) |>
    rename(milestone_global = milestone)
  
  milestone_region_data <- calculate_milestones(indicator_data, variable_col, year_col, value_col, regions_col) |>
    rename(milestone_region = milestone) |>
    select(-all_of(regions_col))
  
  milestone_income_group_data <- calculate_milestones(indicator_data, variable_col, year_col, value_col, income_groups_col) |>
    rename(milestone_income_group = milestone) |>
    select(-all_of(income_groups_col))
  
  milestone_global_data |>
    left_join(milestone_region_data, by = c(variable_col, year_col, country_col)) |>
    left_join(milestone_income_group_data, by = c(variable_col, year_col, country_col)) |>
    arrange(country_col, -!!sym(year_col))
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
    subgroup_col = NULL
) {
  group_data <- function(data) {
    if (!is.null(subgroup_col)) {
      group_by(data, !!sym(subgroup_col), !!sym(variable_col), !!sym(year_col))
    } else {
      group_by(data, !!sym(variable_col), !!sym(year_col))
    }
  }
  
  indicator_data |> 
    mutate(
      !!value_col := !!sym(value_col)
    ) |> 
    filter(!is.na(!!sym(value_col))) |> # Remove rows where value could not be converted
    group_data() |> # Apply conditional grouping
    reframe(
      country = country,
      milestone = quantile(!!sym(value_col), milestone_pctile, na.rm = TRUE, type = 2) # Calculate using correct milestone given desirability direction
    ) |> 
    ungroup() |> 
    rename(variable = !!sym(variable_col), year = !!sym(year_col)) |> 
    unique()  # Remove duplicates. We only remove duplicate rows so no information is lost when calculating metrics.
}


#' calculate_velocity_cagr_required_to_hit_target_value_by_target_year
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
#' calculate_velocity_cagr_required_to_hit_target_value_by_target_year(
#'   starting_value = 100, 
#'   starting_year = 2020, 
#'   target_value = 200, 
#'   target_year = 2030
#' )
#' 
#' # Example: When the target year equals the starting year
#' calculate_velocity_cagr_required_to_hit_target_value_by_target_year(
#'   starting_value = 100, 
#'   starting_year = 2020, 
#'   target_value = 200, 
#'   target_year = 2020
#' ) # Returns `Inf`
calculate_velocity_cagr_required_to_hit_target_value_by_target_year <- function(
    starting_value,
    starting_year,
    target_value, 
    target_year
) {
  100 * ((target_value / starting_value) ^ (1 / (target_year - starting_year)) - 1)
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
  
  by_year_velocities |> 
    group_by(country) |> 
    filter(year == latest_year) |>
    ungroup() |> 
    select(-year) |> 
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
#' @seealso calculate_velocity_cagr_required_to_hit_target_value_by_target_year, calculate_velocity_linear_growth_rate_required_to_hit_target_value_by_target_year
calculate_velocity_required_to_hit_milestone_metrics <- function(
    velocity_starting_data,
    indicator_milestone_metrics,
    target_value,
    target_year
) {
  # Join starting data with milestone metrics by country
  velocity_starting_data <- velocity_starting_data |> 
    left_join(indicator_milestone_metrics, by = c("country"))
  
  # Calculate velocity metrics
  velocity_starting_data |> 
    rowwise() |> 
    mutate(
      # Using latest_value as starting point
      from_latest_year_cagr_required_to_hit_milestone_global = calculate_velocity_cagr_required_to_hit_target_value_by_target_year(
        latest_value, latest_year, milestone_global, target_year),
      from_latest_year_cagr_required_to_hit_milestone_region = calculate_velocity_cagr_required_to_hit_target_value_by_target_year(
        latest_value, latest_year, milestone_region, target_year),
      from_latest_year_cagr_required_to_hit_milestone_income_group = calculate_velocity_cagr_required_to_hit_target_value_by_target_year(
        latest_value, latest_year, milestone_income_group, target_year),
      from_latest_year_cagr_required_to_hit_target = calculate_velocity_cagr_required_to_hit_target_value_by_target_year(
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
      from_mean_last_3_years_cagr_required_to_hit_milestone_global = calculate_velocity_cagr_required_to_hit_target_value_by_target_year(
        avg_value_last_three_years, latest_year, milestone_global, target_year),
      from_mean_last_3_years_cagr_required_to_hit_milestone_region = calculate_velocity_cagr_required_to_hit_target_value_by_target_year(
        avg_value_last_three_years, latest_year, milestone_region, target_year),
      from_mean_last_3_years_cagr_required_to_hit_milestone_income_group = calculate_velocity_cagr_required_to_hit_target_value_by_target_year(
        avg_value_last_three_years, latest_year, milestone_income_group, target_year),
      from_mean_last_3_years_cagr_required_to_hit_target = calculate_velocity_cagr_required_to_hit_target_value_by_target_year(
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


#' get_analysis_data
#'
#' @description Processes raw input data for analysis by cleaning variable names, adding relevant columns, 
#' and selecting only the necessary fields. This function prepares the data for downstream analysis.
#'
#' @param raw_data A data frame containing the raw data to be processed. The data must include columns for:
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
#'   - `variable`: Cleaned variable names.
#'   - `indicator`: Indicator associated with the variable.
#'   - `short_label`: Short label for the variable.
#'   - `year`: Year of the data point.
#'   - `value`: Value of the data point.
#'   - `un_continental_region`: UN continental region (renamed for clarity).
#'   - `income_group`: Income group classification for the country.
#'   - `desirable_direction`: Indicates whether increasing or decreasing is desirable.
#'   - `milestone_pctile`: Percentile used for milestone calculation (0.2 if decreasing is desirable, otherwise 0.8).
#'
#' @examples
#' # Prepare data for analysis
#' analysis_data <- get_analysis_data(raw_data)
get_analysis_data <- function(raw_data) {
  raw_data |> 
    mutate(
      variable = clean_analysis_data_variables(variable), # Cleans up a handful of minor issues from raw data
      un_continental_region = `UN Continental Region`,
      milestone_pctile = if_else(desirable_direction == -1, 0.2, 0.8)
    ) |> 
    select(
      country, variable, indicator, short_label, year, value, un_continental_region, income_group, desirable_direction, milestone_pctile
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
  summary_data <- data |> 
    mutate(
      region = `UN Continental Region`
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
get_indicator_variable_metrics <- function(analysis_data, indicator_summary, target_data, indicator_variable, target_year, data_patches) {
  indicator_variable_metrics <- tibble()
  failing_variables <- list()
  warning_variables <- list()
  
  # if indicator_summary$data_type for this variable is not numeric, return list with info
  data_type <- indicator_summary |> filter(variable == indicator_variable) |> pull(data_type)
  indicator_label <- analysis_data |> filter(variable == indicator_variable) |> pull(indicator) |> unique() |> as.character()
  indicator_short_label <- analysis_data |> filter(variable == indicator_variable) |> pull(short_label) |> unique() |> as.character()
  
  if(length(data_type) == 0 || data_type != "numeric") {
    message <- paste0("Skipped: indicator variable ", indicator_variable, " is not numeric.")
  } else{
    tryCatch(
      {
        indicator_data <- extract_indicator_data(analysis_data, indicator_variable, data_patches = data_patches)
        target_value <- get_target_value(indicator_variable, target_data)
        indicator_milestone_metrics <- calculate_milestone_metrics(indicator_data) # All years for milestones
        indicator_distance_metrics <- calculate_distance_metrics(indicator_data, indicator_milestone_metrics, target_value)
        indicator_velocity_metrics <- calculate_velocity_metrics(indicator_data, indicator_milestone_metrics, target_value, target_year)
        
        indicator_metrics_countries_with_data <- indicator_milestone_metrics |> 
          mutate(
            target_value = target_value
          ) |> 
          inner_join(indicator_distance_metrics, by = c("variable", "country", c("year" = "latest_year"))) |>  # Narrows to latest year 
          rename(
            latest_year = year,
            latest_year_value = value,
          ) |> 
          select(variable, country, latest_year,  latest_year_value, everything()) |> # Rearrange
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
        
        indicator_variable_metrics <- indicator_metrics_countries_with_data |> 
          bind_rows(countries_without_data) |> 
          arrange(country)
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
    meta = list(
      data_type = data_type,
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
#' @param raw_data A data frame containing the raw input data for analysis.  This should be read in from a
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
#'   raw_data = raw_data,
#'   indicator_summary = indicator_summary,
#'   raw_target_data = raw_target_data,
#'   target_year = 2030,
#'   data_patches = list()
#' )
get_metrics_data <- function(
    raw_data,
    indicator_summary,
    raw_target_data,
    target_year = 2030,
    data_patches = list()
) {
  analysis_data <- get_analysis_data(raw_data)
  regions <- get_un_regions(analysis_data)
  income_groups <- get_income_groups(analysis_data)
  target_data <- get_target_data(raw_target_data)
  indicator_variables <- get_indicator_variables(analysis_data)
  
  map(
    indicator_variables, 
    ~ get_indicator_variable_metrics(analysis_data, indicator_summary, target_data, .x, target_year, data_patches)
  ) |> 
    set_names(indicator_variables)
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
  data |> 
    mutate(
      region = `UN Continental Region`
    ) |> 
    group_by(region) |>
    summarise(
      n_countries = n_distinct(country),
      n_income_groups = n_distinct(income_group)
    ) |> 
    arrange(region)
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
  region_col <- if_else("un_continental_region" %in% names(data), "un_continental_region", "UN Continental Region")
  data |> pull(!!sym(region_col)) |> as.character() |> sort() |> unique()
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