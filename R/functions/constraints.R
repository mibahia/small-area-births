#' Make a row constraint
#'
#' @description
#' `make_row_constrainst` creates row constraints for IPF.
#'
#' @details
#' This function simply selects the MSOAs for that a local authority and
#' takes the column for a particular age band.
#'
#' @param births_msoa_data A data frame, filtered by year.
#' @param lad_to_msoa_lookup A data frame.
#' @param age_band A character.
#' @param lad_code A character.
#' @return A numeric vector.
make_row_constrainst <- function(
  births_msoa_data,
  msoas,
  age_band,
  lad_code,
  msoa_col = msoa_2021_code
) {
  # We apply arrange to make sure that the order of MSOAs are consistent.

  row_constraint <- births_msoa_data |>
    dplyr::filter({{ msoa_col }} %in% msoas) |>
    dplyr::arrange({{ msoa_col }}) |>
    dplyr::select(!!age_band) |>
    unlist()

  return(as.numeric(row_constraint))
}

#' Make a column constraint
#'
#' @description
#' `make_col_constrainst` creates column constraints for IPF.
#'
#' @details
#' This function index into all the single-year of age columns in the
#' local authority data for a particular age band.
#'
#' @param births_lad_data A data frame, filtered by year.
#' @param age_band A character.
#' @param lad_code A character.
#' @return A numeric vector.
make_col_constrainst <- function(births_lad_data, age_band, lad_code) {
  sya_cols <- get_sya_cols_from_age_band(age_band)

  col_constraint <- births_lad_data |>
    dplyr::filter(local_authority_code == !!lad_code) |>
    dplyr::select(dplyr::all_of(c(sya_cols)))

  return(as.numeric(col_constraint))
}
