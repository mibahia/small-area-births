#' Calculates ASFR for rolling sum
#'
#' @description
#' `calculate_rolling_sum_asfr` calculates rolling sums ASFR.
#'
#' @details
#' This function creates a dataframe for births and population at risk.
#' It then applies the `rollsum` function from the `zoo` packages.
#' After merging both datasets, it calculates age-specific fertility rates
#' for the period, labelling the new period according to the `k` param.
#'
#' @param births_by_msoa A list.
#' @param lookup_by_lad A list.
#' @param k A integer, 3 or 5.
#' @return A data frame.
calculate_rolling_sum_asfr <- function(
  births_by_msoa,
  lookup_by_lad,
  msoa_names_lookup,
  k = 3,
  ...
) {
  checkmate::assert_list(births_by_msoa)
  checkmate::assert_list(lookup_by_lad)
  checkmate::assert_choice(k, c(3, 5))

  if (k == 3) {
    label_adj <- 1
  } else if (k == 5) {
    label_adj <- 2
  }

  print("Creating births dataframe...")
  input_births <- create_dataframe(
    births_by_msoa = births_by_msoa,
    lookup_by_lad = lookup_by_lad,
    msoa_names_lookup = msoa_names_lookup,
    output_type = "births"
  )

  print("Creating population at risk dataframe...")
  input_population_at_risk <- create_dataframe(
    births_by_msoa = births_by_msoa,
    lookup_by_lad = lookup_by_lad,
    msoa_names_lookup = msoa_names_lookup,
    output_type = "population_estimate"
  )

  if (length(unique(input_births$year)) < 2) {
    stop("Check unique years in the input data...")
  }

  print("Applying rolling sum for births...")
  births <- apply_rollsum(
    df = input_births,
    target_col = births,
    label_adj = label_adj,
    k = k
  )

  print("Applying rolling sum for population at risk...")
  population_at_risk <- apply_rollsum(
    df = input_population_at_risk,
    target_col = population_estimate,
    label_adj = label_adj,
    k = k
  )

  output <- births |>
    dplyr::left_join(population_at_risk, by = NULL) |>
    dplyr::mutate(
      fertility_rate = as.numeric(births) / as.numeric(population_estimate),
      ...
    )

  print("Done!")

  return(output)
}

apply_rollsum <- function(df, target_col, label_adj, k) {
  output <- df |>
    dplyr::mutate(
      label_year = paste0(year - label_adj, "-", year + label_adj),
      .after = year
    ) |>
    dplyr::group_by(msoa21_code, age_of_mother) |>
    dplyr::mutate(
      {{ target_col }} := zoo::rollsum(
        {{ target_col }},
        k = k,
        align = "center",
        fill = c(NA)
      )
    ) |>
    dplyr::ungroup() |>
    na.omit()

  return(output)
}
