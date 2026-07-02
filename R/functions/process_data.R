source("R/functions/helper_functions.R")
source("R/functions/recode_lad_boundaries.R")
source("R/functions/get_geo_lookup.R")

read_births_data <- function(births_data_path, sheet, skip, ...) {
  raw_births <- readxl::read_excel(
    births_data_path,
    sheet = sheet,
    skip = skip,
    ...
  ) |>
    rename_cols_wrapper()

  return(raw_births)
}

process_births_data <- function(
  births_data_path,
  lookup_by_lad,
  skip,
  period_col,
  recode_from_year,
  recode_to_year,
  ...
) {
  raw_births_lad <- read_births_data(
    births_data_path = births_data_path,
    sheet = "1",
    skip = 5,
    ...
  )
  raw_births_msoa <- read_births_data(
    births_data_path = births_data_path,
    sheet = "2",
    skip = 5,
    ...
  )

  common_period <- get_common_period(raw_births_lad, raw_births_msoa)

  births_lad <- raw_births_lad |>
    dplyr::filter({{ period_col }} %in% common_period) |>
    recode_lad_boundaries(
      from_year = recode_from_year,
      to_year = recode_to_year
    )

  births_msoa <- raw_births_msoa |>
    dplyr::filter({{ period_col }} %in% common_period)

  period_col <- rlang::as_name(rlang::ensym(period_col))

  births_lad_by_year <- split(births_lad, births_lad[[period_col]])
  births_msoas_by_year <- split(births_msoa, births_msoa[[period_col]])

  births_data <- list(lad = births_lad_by_year, msoa = births_msoas_by_year)
  missing_msoas <- get_missing_msoas(
    births_data = births_data,
    lookup_by_lad = lookup_by_lad
  )

  births_data[["msoa"]] <- add_missing_msoas(
    births_msoa_data = births_data[["msoa"]],
    missing_msoas = missing_msoas
  )

  return(births_data)
}

process_population_at_risk <- function(
  parquet_path,
  split_col
) {
  parquet_data <- arrow::open_dataset(parquet_path) |>
    dplyr::collect() |>
    tidyr::pivot_wider(names_from = c_age_name, values_from = obs_value) |>
    janitor::clean_names(case = "snake")

  population_at_risk_by_year <- split(parquet_data, parquet_data[[split_col]])
  return(population_at_risk_by_year)
}

process_lookup <- function(
  lookup_path,
  split_col,
  ...
) {
  lad_to_msoa_lookup <- read.csv(lookup_path, ...)
  lookup_by_lad <- split(lad_to_msoa_lookup, lad_to_msoa_lookup[[split_col]])

  return(lookup_by_lad)
}
