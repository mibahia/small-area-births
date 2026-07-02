#' Runs IPF
#'
#' @description
#' `ipf_wrapper` runs ipf for all age bands for a given year and LAD.
#'
#' @details
#' This function is a mipfp::Ipfp wrapper. Creates seed and constraints using
#' helper functions and apply Iterative Proportional Fitting to inputs.
#' Handles all age_bands in a single year and local authority district.
#' Returns a nested list with births, rates and adjusted population estimates.
#'
#' @param births_lad_data A data frame.
#' @param births_msoa_data A data frame.
#' @param lad_to_msoa_lookup A data frame.
#' @param age_bands A character vector.
#' @param lad_code A character.
#' @param year A character.
#' @return A list
ipf_wrapper <- function(
  births_lad_data,
  births_msoa_data,
  population_at_risk_data,
  msoas,
  age_bands,
  lad_code
) {
  births_temp <- list()
  rates_temp <- list()
  pop_temp <- list()

  # Adding the first / last age_band
  pop_under_20 <- make_seed(population_at_risk_data, "a15_19", lad_code, msoas)
  pop_over_44 <- make_seed(population_at_risk_data, "a45_49", lad_code, msoas)

  births_under_20_over_44 <- births_msoa_data |>
    dplyr::filter(msoa_2021_code %in% msoas) |>
    dplyr::select(age_19_and_under, age_45_and_over)

  rates_under_20 <- births_under_20_over_44[1] / sum(pop_under_20)
  rates_over_44 <- births_under_20_over_44[2] / sum(pop_over_44)

  for (age in age_bands) {
    tryCatch(
      {
        # Get seed
        seed <- make_seed(population_at_risk_data, age, lad_code, msoas)
        pop_temp[age] <- list(seed)

        if (is.null(seed)) {
          break
        }

        # Create row and col constraints
        target.row <- make_row_constrainst(
          births_msoa_data,
          msoas,
          age,
          lad_code
        )
        target.col <- make_col_constrainst(births_lad_data, age, lad_code)

        target.data = list(
          row = target.row,
          col = target.col
        )

        target.list <- list(
          row = 1,
          col = 2
        )

        if (length(target.row) != nrow(seed)) {
          warning(
            glue::glue("Number of MSOAs inconsistent for {lad_code}.")
          )
        }

        if (sum(target.row) != sum(target.col)) {
          warning(
            glue::glue("Constraints' sums don't match for {lad_code}.")
          )
        }

        # Apply IPF
        res <- mipfp::Ipfp(seed, target.list, target.data)
        if (any(is.nan(res$p.hat))) {
          births_temp[age] <- list(res$x.hat)
          rates_temp[age] <- list((res$x.hat) / seed)
        } else {
          births_temp[age] <- list(res$p.hat * sum(target.col))
          rates_temp[age] <- list(res$p.hat * sum(target.col) / seed)
        }
      },
      error = function(e) {
        cat("Error:", conditionMessage(e), "\n")
      }
    )
  }

  births_temp <- do.call(cbind, births_temp)
  rates_temp <- do.call(cbind, rates_temp)
  pop_temp <- do.call(cbind, pop_temp)

  births <- cbind(
    msoas,
    births_under_20_over_44[1],
    births_temp,
    births_under_20_over_44[2]
  )
  population_estimates <- cbind(msoas, pop_under_20, pop_temp, pop_over_44)
  rates <- cbind(msoas, rates_under_20, rates_temp, rates_over_44)

  return(list(
    births = births,
    fertility_rate = rates,
    population_estimate = population_estimates
  ))
}
