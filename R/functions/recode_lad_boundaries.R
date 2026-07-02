#' Recodes Get the number of observations
#'
#' @description
#' `recode_lad_boundaries` is just a wrapper of `gsscoder::recode_gss`.
#'
#' @param births_lad_data A dataframe.
#' @param from_year A integer.
#' @param to_year A integer.
#' @return A dataframe.
recode_lad_boundaries <- function(
  births_lad_data,
  from_year = 2021,
  to_year = 2023
) {
  births_for_gss_coder <- births_lad_data |>
    tidyr::pivot_longer(
      cols = colnames(births_lad_data)[4:length(colnames(births_lad_data))],
      names_to = "age_of_mother",
      values_to = "value"
    ) |>
    dplyr::rename(gss_code = local_authority_code) |>
    dplyr::select(!local_authority_name)

  recoded_births_lad <- suppressWarnings(
    gsscoder::recode_gss(
      df_in = births_for_gss_coder,
      recode_from_year = from_year,
      recode_to_year = to_year
    ) |>
      gsscoder::add_gss_names(
        col_name = "local_authority_name",
        gss_year = to_year
      )
  )

  output <- recoded_births_lad |>
    tidyr::pivot_wider(names_from = "age_of_mother", values_from = "value") |>
    dplyr::rename(local_authority_code = gss_code)

  return(output)
}
