get_data_patches <- function() {
  list(
    list(
      indicator_variable = "functionalintegrity",
      country = "India",
      year = 2015,
      value = "0",
      action = "remove"
    ),
    list(
      indicator_variable = "mufppurbshare",
      country = "Canada",
      year = 2023,
      value = Inf,
      action = "set_na"
    ),
    list(
      indicator_variable = "mufppurbshare",
      country = "New Zealand",
      year = 2023,
      value = Inf,
      action = "set_na"
    ),
    list(
      indicator_variable = "mufppurbshare",
      country = "Russian Federation",
      year = 2023,
      value = Inf,
      action = "set_na"
    ),
    list(
      indicator_variable = "mufppurbshare",
      country = "United States of America",
      year = 2023,
      value = Inf,
      action = "set_na"
    )
  )
}