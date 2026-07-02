source("../fertility-rate-estimation/R/functions/reprofile_combined_rates.R")
source("../fertility-rate-estimation/R/functions/smooth_fertility_curve.R")
source(
  "../fertility-rate-estimation/R/functions/estimate_fertility_rates_sya.R"
)

#' Smooth mid-year age-specific fertility rates
#'
#' @description
#' `smooth_my_asfr_msoas` is a `smooth_fertility_curve` wrapper
#'
#' @details
#' This function splits the uppper / lower age bands and reprofile these rates
#' by using functions from the fertility-rate-estimation process. It then
#' applies the `smooth_fertility_curve` to mid-year ASFR for small areas.
#'
#' @param raw_asfr_data A dataframe.
#' @param geo_col A character vector.
#' @return A dataframe.
smooth_my_asfr_small_areas <- function(raw_asfr_data, geography_col) {
  message("Transforming upper / lower age bands into single year of age...")

  asfr_data <- raw_asfr_data |>
    transform_combined_into_sye(
      start_age = 20,
      end_age = 44,
      combined_start_age = "Under 19",
      combined_end_age = "Over 45",
      asfr_min_age = 15,
      asfr_max_age = 49
    ) |>
    reprofile_combined_rates()

  raw_list <- split(
    asfr_data,
    ~ asfr_data[[geography_col]] + asfr_data[["year"]]
  )[order(names(split(
    asfr_data,
    ~ asfr_data[[geography_col]] + asfr_data[["year"]]
  )))]
  smooth_list <- sapply(names(raw_list), function(x) NULL)

  # Set initial year
  first_year <- min(asfr_data[["year"]])

  message("Smoothing small area fertility rates...")

  for (i in seq_along(raw_list)) {
    current_year <- unique(raw_list[[i]][["year"]])

    if (current_year == first_year) {
      params <- list(
        m = 0.424,
        a = 0.574,
        b1 = 3.536,
        c1 = 24.858,
        b2 = 4.815,
        c2 = 33.218
      )
    } else {
      params <- smooth_rates[["coefs"]]
    }

    smooth_rates <- smooth_fertility_curve(
      raw_rates = raw_list[[i]],
      params
    )

    smooth_list[[i]] <- smooth_rates[["rates"]]

    if (i %% 100 == 0) message(paste0("Running ", i, " of ", length(raw_list)))
  }

  smooth_rates_dataframe <- smooth_list |>
    dplyr::bind_rows()

  return(smooth_rates_dataframe)
}
