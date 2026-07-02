#' Creates a dataframe from ipfp wrapper outputs
#'
#' @description
#' `create_dataframe` creates a dataframe for a particular type of output.
#'
#' @details
#' This function constructs a dataframe from the ipfp wrapper function called
#' recursively using `get_births_by_msoa`.
#'
#' @param births_by_msoa A list.
#' @param output_type A character.
#' @param output_by_lad A list.
#' @param lad_col A character.
#' @param msoa_col A character.
#' @return A data frame.
create_dataframe <- function(
  births_by_msoa,
  output_type,
  lookup_by_lad,
  msoa_names_lookup,
  lad_col = "LAD23NM",
  msoa_col = "MSOA21CD"
) {
  checkmate::assert_list(births_by_msoa)
  checkmate::assert_choice(
    output_type,
    c("fertility_rate", "births", "population_estimate")
  )
  checkmate::assert_character(lad_col, pattern = "^LAD")
  checkmate::assert_character(msoa_col, pattern = "^MSOA")

  df <- list()
  for (yr in names(births_by_msoa)) {
    print(glue::glue("Creating dataframe for year {yr}"))

    for (current_lad_code in names(births_by_msoa[[yr]])) {
      current_lad_name <- unique(lookup_by_lad[[current_lad_code]][[lad_col]])
      current_msoas <- unlist(lookup_by_lad[[current_lad_code]][[msoa_col]])

      output <- purrr::pluck(births_by_msoa, yr, current_lad_code, output_type)

      current_output <- output |>
        tibble::as_tibble() |>
        dplyr::mutate(
          local_authority_code = current_lad_code,
          .after = msoas
        ) |>
        dplyr::mutate(
          local_authority_name = current_lad_name,
          .after = local_authority_code
        ) |>
        dplyr::mutate(label_year = yr, .after = local_authority_name) |>
        dplyr::left_join(msoa_names_lookup, by = c("msoas" = "msoa21cd"))

      df <- rbind(df, current_output)
    }
  }

  if (output_type == "population_estimate") {
    df <- df |>
      dplyr::mutate(dplyr::across(age_15:age_49, as.numeric)) |>
      dplyr::mutate(
        age_19_and_under = age_15 + age_16 + age_17 + age_18 + age_19,
        .keep = "unused",
        .after = label_year
      ) |>
      dplyr::mutate(
        age_45_and_over = age_45 + age_46 + age_47 + age_48 + age_49,
        .keep = "unused",
        .after = age_44
      )
  }

  df <- df |>
    tidyr::pivot_longer(
      cols = dplyr::starts_with("age"),
      names_to = "age_of_mother",
      values_to = "value"
    ) |>
    dplyr::mutate(
      age_of_mother = dplyr::case_when(
        age_of_mother == "age_19_and_under" ~ "Under 19",
        age_of_mother == "age_45_and_over" ~ "Over 45",
        .default = stringr::str_extract(age_of_mother, "\\d{2}")
      ),
      year = as.numeric(
        stringr::str_extract(label_year, "(?<=-)(\\d{4})")
      ),
      .after = label_year
    )

  # Making DF compatible with fertility functions
  output <- df |>
    dplyr::rename(
      gss_code = local_authority_code,
      gss_name = local_authority_name,
      {{ output_type }} := value,
      msoa21_code = msoas,
      msoa21_name = msoa21hclnm
    ) |>
    dplyr::relocate(msoa21_name, .after = msoa21_code)

  return(output)
}
