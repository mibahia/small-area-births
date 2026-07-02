#' Make a seed table
#'
#' @description
#' `make_seed` creates a seed table for IPF using population estimates.
#'
#' @details
#' This is a specific function that requires parquet files saved locally
#' from Nomis & a LAD to MSOA look up from Open Geography portal.
#' Because births and population estimates are mid-year figures,
#' we average estimates over a two year period.
#'
#' @param parquet_data_by_year A list.
#' @param age_band A character.
#' @param lad_code A character.
#' @param msoas A character vector.
#' @return A matrix / double.
make_seed <- function(
  parquet_data_by_year, # data across both years
  age_band,
  lad_code,
  msoas ## this is being passed as character vector
) {
  checkmate::assert_list(parquet_data_by_year)
  checkmate::assert_character(age_band)
  checkmate::assert_character(lad_code, len = 1)
  checkmate::assert_character(msoas, any.missing = FALSE)

  sya_cols <- get_sya_cols_from_age_band(age_band)

  lad_population_at_risk <- purrr::map(parquet_data_by_year, \(x) {
    filter_population_at_risk(x, msoas, sya_cols, geography_code)
  })

  result <- purrr::reduce(lad_population_at_risk, `+`) /
    length(lad_population_at_risk)

  seed_matrix <- matrix(
    unlist(result),
    nrow = length(msoas),
    ncol = 5,
    dimnames = list(c(), c(sya_cols))
  )

  return(seed_matrix)
}


#' Filter population at risk by geography and age
#'
#' @description
#' `filter_population_at_risk` filter population for the `make_seed` function.
#'
#' @param population_at_risk_data A list.
#' @param msoas A character vector.
#' @param ages A character.
#' @param geography_code_col An object.
#' @return A data frame.
filter_population_at_risk <- function(
  population_at_risk_data,
  msoas,
  ages,
  geography_code_col
) {
  filtered_population <- population_at_risk_data |>
    dplyr::filter({{ geography_code_col }} %in% msoas) |>
    dplyr::arrange({{ geography_code_col }})

  msoas_list <- filtered_population |>
    dplyr::pull({{ geography_code_col }})

  # Making sure that the order of MSOAs are consistent to avoid bugs.
  if (!identical(msoas_list, msoas)) {
    warning("MSOA order inconsistent...")
  }

  output <- filtered_population |>
    dplyr::select(dplyr::all_of(ages))

  return(output)
}
