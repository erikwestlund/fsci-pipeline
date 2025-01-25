
get_un_regions <- function(data) {
  region_col <- if_else("un_continental_region" %in% names(data), "un_continental_region", "UN Continental Region")
  data |> pull(!!sym(region_col)) |> as.character() |> sort() |> unique()
}

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

get_indicator_summary <- function(data) {
  data_type_distinct_values <- data |> 
    mutate(
      value = ifelse(is.na(value) | value == "", NA, value) # Handle missing or empty values
    ) |> 
    filter(!is.na(value)) |> # Keep only rows with valid data
    group_by(variable) |> 
    summarise(
      data_type = determine_data_type(value), # Determine data type for each variable
      distinct_values = n_distinct(value, na.rm = TRUE), # Total distinct values across all years
      .groups = "drop"
    )
  
  summary_metrics <- data |> 
    mutate(
      value = ifelse(is.na(value) | value == "", NA, value) # Handle missing or empty values
    ) |> 
    filter(!is.na(value)) |> # Keep only rows with valid data
    group_by(variable, year) |> 
    summarise(
      country_count = n(), 
      .groups = "drop"
    ) |> 
    group_by(variable) |> 
    summarise(
      years_of_data = n_distinct(year), # Number of years with data
      first_year = min(year), # First year with data
      first_year_reporting_countries = country_count[year == min(year)], # Countries in the first year
      latest_year = max(year), # Latest year with data
      latest_year_reporting_countries = country_count[year == max(year)], # Countries in the latest year
      lowest_reporting_year = year[which.min(country_count)], # Year with the smallest count
      lowest_year_reporting_countries = min(country_count), # Smallest count
      highest_reporting_year = year[which.max(country_count)], # Year with the largest count
      highest_year_reporting_countries = max(country_count), # Largest count
      avg_countries_reporting_per_year = round(mean(country_count), 1), # Average reporting countries
      .groups = "drop"
    )
  
  summary_metrics |> 
    left_join(data_type_distinct_values, by = "variable") |> 
    left_join(data |> select(variable, indicator) |> distinct(), by = "variable") |>
    select(variable, indicator,  data_type, distinct_values, everything())
}

determine_data_type <- function(values) {
  values <- as.character(values) # Coerce to character
  
  # Attempt numeric coercion
  numeric_values <- suppressWarnings(as.numeric(values))
  
  if (all(!is.na(numeric_values))) {
    # Check for boolean
    if (all(numeric_values %in% c(0, 1))) {
      return("boolean")
    }
    return("numeric")
  }
  
  # If not numeric, it's character
  return("character")
}

get_metrics_data <- function(
  raw_data,
  indicator_summary,
  raw_target_data,
  target_year = 2030
) {
  analysis_data <- get_analysis_data(raw_data)
  regions <- get_un_regions(analysis_data)
  income_groups <- get_income_groups(analysis_data)
  target_data <- get_target_data(raw_target_data)
  indicator_variables <- get_indicator_variables(analysis_data)
  
  map(
    indicator_variables, 
    ~ get_indicator_variable_metrics(analysis_data, indicator_summary, target_data, .x, target_year)
  ) |> 
    set_names(indicator_variables)
}

# Returns a list of `data` and `meta`
get_indicator_variable_metrics <- function(analysis_data, indicator_summary, target_data, indicator_variable, target_year) {
  indicator_variable_metrics <- tibble()
  failing_variables <- list()
  warning_variables <- list()
  
  # if indicator_summary$data_type for this variable is not numeric, return list with info
  data_type <- indicator_summary |> filter(variable == indicator_variable) |> pull(data_type)
  
  if(length(data_type) == 0 || data_type != "numeric") {
    message <- paste0("Skipped: indicator variable ", indicator_variable, " is not numeric.")
  } else{
    tryCatch(
      {
        indicator_data <- extract_indicator_data(analysis_data, indicator_variable)
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


clean_analysis_data_variables <- function(variables) {
  # we do some minimal cleaning here, namely, removing \r\n from any variable names
  variables |> stringr::str_replace_all("\r\n$", "")
}

# Classify values in a vector as numeric or character. Used for data type analysis.
classify_values_individually <- function(values, n = 5) {
  unique_vals <- unique(values)
  
  # Filter NA
  unique_vals <- unique_vals[!is.na(unique_vals)]
  
  numeric_vals <- suppressWarnings(as.numeric(unique_vals))
  numeric_examples <- unique_vals[!is.na(numeric_vals)] |> sort()
  
  char_vals <- unique_vals[is.na(numeric_vals)] |> sort()
  
  list(
    numeric_examples = head(numeric_examples, n),
    char_examples = head(char_vals, n),
    numeric_full = numeric_examples,
    char_full = char_vals
  )
}

# Extract a specific indicator variable from the analysis data.
extract_indicator_data <- function(analysis_data, variable_name, value_col = "value") {
  analysis_data |>
    filter(variable == variable_name) |> 
    mutate(
      value = readr::parse_number(!!sym(value_col)),
      year = as.integer(year)
    )
}

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

# Extracts target information for an indicator variable
extract_target_data <- function(indicator_variable) {
  target_data |> 
    filter(variable == indicator_variable) |> 
    filter(has_target == TRUE)
}

# Encapsulate the calculation of milestone data and distance/progress metrics
# into a single function.
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
    rename(variable = !!sym(variable_col), year = !!sym(year_col))
}

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
  by_year_velocities <- calculate_growth_required_metrics(velocity_starting_data, indicator_milestone_metrics, target_value, target_year)
  
  by_year_velocities |> 
    group_by(country) |> 
    filter(year == latest_year) |>
    ungroup() |> 
    select(-year) |> 
    arrange(country)
}

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
    )
}

calculate_growth_required_metrics <- function(
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
      from_latest_year_cagr_required_to_hit_milestone_global = calculate_cagr_required_to_hit_target_value_by_target_year(
        latest_value, latest_year, milestone_global, target_year),
      from_latest_year_cagr_required_to_hit_milestone_region = calculate_cagr_required_to_hit_target_value_by_target_year(
        latest_value, latest_year, milestone_region, target_year),
      from_latest_year_cagr_required_to_hit_milestone_income_group = calculate_cagr_required_to_hit_target_value_by_target_year(
        latest_value, latest_year, milestone_income_group, target_year),
      from_latest_year_cagr_required_to_hit_target = calculate_cagr_required_to_hit_target_value_by_target_year(
        latest_value, latest_year, target_value, target_year),
      
      from_latest_year_linear_growth_rate_required_to_hit_milestone_global = calculate_linear_growth_rate_required_to_hit_target_value_by_target_year(
        latest_value, latest_year, milestone_global, target_year),
      from_latest_year_linear_growth_rate_required_to_hit_milestone_region = calculate_linear_growth_rate_required_to_hit_target_value_by_target_year(
        latest_value, latest_year, milestone_region, target_year),
      from_latest_year_linear_growth_rate_required_to_hit_milestone_income_group = calculate_linear_growth_rate_required_to_hit_target_value_by_target_year(
        latest_value, latest_year, milestone_income_group, target_year),
      from_latest_year_linear_growth_rate_required_to_hit_target = calculate_linear_growth_rate_required_to_hit_target_value_by_target_year(
        latest_value, latest_year, target_value, target_year),
      
      # Using avg_value_last_three_years as starting point
      from_mean_last_3_years_cagr_required_to_hit_milestone_global = calculate_cagr_required_to_hit_target_value_by_target_year(
        avg_value_last_three_years, latest_year, milestone_global, target_year),
      from_mean_last_3_years_cagr_required_to_hit_milestone_region = calculate_cagr_required_to_hit_target_value_by_target_year(
        avg_value_last_three_years, latest_year, milestone_region, target_year),
      from_mean_last_3_years_cagr_required_to_hit_milestone_income_group = calculate_cagr_required_to_hit_target_value_by_target_year(
        avg_value_last_three_years, latest_year, milestone_income_group, target_year),
      from_mean_last_3_years_cagr_required_to_hit_target = calculate_cagr_required_to_hit_target_value_by_target_year(
        avg_value_last_three_years, latest_year, target_value, target_year),
      
      from_mean_last_3_years_linear_growth_rate_required_to_hit_milestone_global = calculate_linear_growth_rate_required_to_hit_target_value_by_target_year(
        avg_value_last_three_years, latest_year, milestone_global, target_year),
      from_mean_last_3_years_linear_growth_rate_required_to_hit_milestone_region = calculate_linear_growth_rate_required_to_hit_target_value_by_target_year(
        avg_value_last_three_years, latest_year, milestone_region, target_year),
      from_mean_last_3_years_linear_growth_rate_required_to_hit_milestone_income_group = calculate_linear_growth_rate_required_to_hit_target_value_by_target_year(
        avg_value_last_three_years, latest_year, milestone_income_group, target_year),
      from_mean_last_3_years_linear_growth_rate_required_to_hit_target = calculate_linear_growth_rate_required_to_hit_target_value_by_target_year(
        avg_value_last_three_years, latest_year, target_value, target_year)
    ) |> 
    ungroup()
}

calculate_cagr_required_to_hit_target_value_by_target_year <- function(
    starting_value,
    starting_year,
    target_value, 
    target_year
) {
  100 * ((target_value / starting_value) ^ (1 / (target_year - starting_year)) - 1)
}

calculate_linear_growth_rate_required_to_hit_target_value_by_target_year <- function(
    starting_value,
    starting_year,
    target_value, 
    target_year
) {
  100 * ((target_value - starting_value) / (target_year - starting_year))
}


# Calculate distance from milestone and progress metrics.
calculate_distance_and_progress <- function(indicator_data,
                                            milestone_data, 
                                            target_data,
                                            variable_col = "variable", 
                                            year_col = "year", 
                                            value_col = "value", 
                                            country_col = "country") {
  # Ensure milestone_data has proper names
  # processed_milestone_data <- milestone_data |>
  #   rename(
  #     milestone_year = !!sym(year_col)
  #     ) |>
  #   select(!!sym(variable_col), milestone_year, milestone_global, milestone_region, milestone_income_group)
  # 
  # Perform the join
  data_with_milestones <- indicator_data |>
    left_join(
      milestone_data,
      by = c(variable_col, year_col, country_col)
    )
  
  # Convert value to numeric, calculate distances and progress
  data_with_metrics <- data_with_milestone |>
    mutate(
      !!value_col := parse_number(!!sym(value_col)), # Convert to numeric
      distance_to_milestone = !!sym(value_col) - milestone_value,
      progress_to_milestone = !!sym(value_col) / milestone_value
    ) |>
    select(
      !!sym(country_col),
      !!sym(year_col),
      !!sym(variable_col),
      value = !!sym(value_col),
      milestone_value,
      distance_to_milestone,
      progress_to_milestone
    )
  
  # Add target information if it is vailable
  target_data_available <- target_data |> filter(has_target) |> nrow() > 0
  
  if (target_data_available) {
    data_with_metrics <- data_with_metrics |> 
      left_join(
        target_data |> select(variable, target),
        by = variable_col
      ) |> 
      mutate(
        distance_to_target = value - target,
        progress_to_target = value / target
      )
  } else {
    data_with_metrics <- data_with_metrics |> mutate(target_value = NA_real_) 
  }
  
  return(data_with_metrics)
}

# Calculate growth rates for each country and variable.
# Note: The data required here is the milestone data frame returned from 
# `calculate_distance_and_progress`
calculate_growth_rates <- function(milestone_data, 
                                   variable_col = "variable", 
                                   year_col = "year", 
                                   value_col = "value", 
                                   country_col = "country") {
  milestone_data |>
    filter(!is.na(!!sym(value_col))) |> # Remove rows with NA values
    group_by(!!sym(country_col), !!sym(variable_col)) |> # Group by country and variable
    arrange(!!sym(year_col)) |> # Ensure data is ordered by year
    summarize(
      start_year = first(!!sym(year_col)),
      end_year = last(!!sym(year_col)),
      start_value = first(!!sym(value_col)),
      end_value = last(!!sym(value_col)),
      years = end_year - start_year,
      cagr = ifelse(
        years > 0,
        (end_value / start_value)^(1 / years) - 1,
        NA_real_
      ),
      linear_growth_rate = ifelse(
        years > 0,
        (end_value - start_value) / years,
        NA_real_
      ),
      .groups = "drop"
    )
}