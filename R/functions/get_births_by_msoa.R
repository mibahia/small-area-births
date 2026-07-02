source("R/functions/ipf_wrapper.R")
source("R/functions/constraints.R")
source("R/functions/make_seed.R")

#' Create births by MSOA by single age of mother
#'
#' @description
#' `get_births_by_msoa` runs ipf for all age bands, LADs and years.
#'
#' @details
#' This function is a mipfp::Ipfp wrapper. Creates seeds and constraints using
#' helper functions and applies Iterative Proportional Fitting to inputs.
#' Handles all age_bands and local authority districts in a single year.
#'
#' @param births_lad_by_year A list.
#' @param births_msoas_by_year A list.
#' @param lad_to_msoa_lookup A list.
#' @param population_at_risk_data A list.
#' @param local_authorities A character vector.
#' @param age_bands A character vector.
#' @param period A character vector.
#' @return A list.
get_births_by_msoa <- function(
  births_lad_by_year,
  births_msoas_by_year,
  lad_to_msoa_lookup,
  population_at_risk_data,
  msoa_code_col,
  period = NULL
) {
  checkmate::assert_list(births_lad_by_year)
  checkmate::assert_list(births_msoas_by_year)
  checkmate::assert_list(lad_to_msoa_lookup)
  checkmate::assert_list(population_at_risk_data)
  checkmate::assert(
    checkmate::check_null(period),
    checkmate::check_character(period),
    combine = "or"
  )
  checkmate::assert_character(msoa_code_col)

  ### Get unique years and LA
  if (is.null(period)) {
    period <- names(births_msoas_by_year)
  } else if (!all(period %in% names(births_msoas_by_year))) {
    stop("Period inconsistent with data. Check years and params.")
  }

  local_authorities <- names(lad_to_msoa_lookup)

  temp_las <- list()
  final <- list()
  number_of_las <- length(local_authorities)

  for (y in period) {
    age_bands <- births_msoas_by_year[[y]] |>
      names() |>
      stringr::str_subset("age_\\d{2}_\\d{2}")
    years <- unlist(stringr::str_split(y, "-")[[1]])
    current_lad_data <- births_lad_by_year[[y]]
    current_msoa_data <- births_msoas_by_year[[y]]
    current_population_at_risk <- population_at_risk_data[c(years)]

    print(glue::glue("-------------- Running IPF for year {y} --------------"))

    for (i in seq_along(local_authorities)) {
      result <- ipf_wrapper(
        births_lad_data = current_lad_data,
        births_msoa_data = current_msoa_data,
        population_at_risk_data = current_population_at_risk,
        msoas = sort(lad_to_msoa_lookup[[local_authorities[[i]]]][[
          msoa_code_col
        ]]), # Making sure that the order of MSOAs are consistent to avoid bugs.
        age_bands = age_bands,
        lad_code = local_authorities[[i]]
      )

      if (i %% 10 == 0) {
        print(glue::glue("Running LA number {i} of {number_of_las} for {y}"))
      }

      temp_las[[local_authorities[[i]]]] <- result
    }
    final[y] <- list(temp_las)
  }
  return(final)
}
