#' Create single year age bands from period band
#'
#' @description
#' `get_sya_cols_from_age_band` takes a 5-year band string and returns
#' single year of age strings.
#'
#' @param age_band A character.
#' @return A character vector.
get_sya_cols_from_age_band <- function(age_band) {
  digit_ages <- stringr::str_extract_all(age_band, "\\d{2}")[[1]]

  age_range <- digit_ages[1]:digit_ages[2]

  sya_cols <- unlist(lapply(age_range, function(x) paste0("age_", x)))

  return(sya_cols)
}


#' Find common period
#'
#' @description
#' `get_common_period` finds the common period for all three datasets.
#' single year of age strings.
#'
#' @param births_lad_data A dataset.
#' @param births_msoa_data A dataset.
#' @param population_at_risk_path A character.
#' @return A numeric vector.
get_common_period <- function(
  births_lad_data,
  births_msoa_data,
  population_at_risk_path = "data/raw/population_at_risk/"
) {
  parquet_years <- as.numeric(
    unlist(
      lapply(
        list.dirs(
          path = population_at_risk_path
        ),
        function(x) stringr::str_extract_all(x, "\\d{4}")
      )
    )
  )

  parquet_period_years <- unlist(
    parquet_years |>
      purrr::map(
        \(x) paste0(x, "-", x + 1)
      )
  )

  return(base::intersect(
    c(
      births_lad_data$period,
      births_msoa_data$period
    ),
    parquet_period_years
  ))
}

get_missing_msoas <- function(
  births_data,
  lookup_by_lad,
  lad_col = "local_authority_code",
  msoa_col = "MSOA21CD",
  verbose = FALSE
) {
  if (verbose) {
    print("Data str() by year...")
    print(str(births_data[[2]], 1))
  }

  number_of_msoas <- purrr::map_int(lookup_by_lad, nrow) |> sum()

  years_with_diff_msoa <- c()
  for (msoa_data in births_data[[2]]) {
    year <- unique(msoa_data$period)

    if (nrow(msoa_data) != number_of_msoas) {
      years_with_diff_msoa <- append(years_with_diff_msoa, year)
    }
  }

  #' This loop prints the missing MSOAs for each local authority
  missing_msoa <- c()
  for (y in years_with_diff_msoa) {
    temp <- c()
    birth_msoa_col <- grep(
      "msoa",
      colnames(births_data[[2]][[y]]),
      value = TRUE
    )

    for (la in unique(births_data[[1]][[y]][[lad_col]])) {
      current_msoas <- lookup_by_lad[[la]][[msoa_col]]

      births_msoas_by_la <- births_data[[2]][[y]] |>
        dplyr::filter(.data[[birth_msoa_col]] %in% current_msoas) |>
        dplyr::pull(.data[[birth_msoa_col]])

      if (length(current_msoas) != length(births_msoas_by_la)) {
        diff <- setdiff(sort(current_msoas), sort(births_msoas_by_la))
        if (verbose) {
          print(glue::glue("MSOA {diff} missing for LA {la} year {y}"))
        }
        temp <- append(temp, diff)
      }
      missing_msoa[[y]] <- temp
    }
  }

  return(missing_msoa)
}


add_missing_msoas <- function(
  births_msoa_data,
  missing_msoas
) {
  #' Handle cases where there isn't a discrepancy between number of MSOAs
  if (is.null(missing_msoas)) {
    return(births_msoa_data)
  }

  output <- births_msoa_data
  for (y in names(missing_msoas)) {
    for (msoa in missing_msoas[[y]]) {
      add_msoa <- list(y, msoa, 0, 0, 0, 0, 0, 0, 0)
      output[[y]] <- rbind(output[[y]], add_msoa)
    }
  }
  return(output)
}


#' Rename columns
#'
#' @description
#' `rename_cols_wrapper` rename cols to facilitate data wrangling.
#'
#' @param raw_births_data A data frame.
#' @return A data frame.
rename_cols_wrapper <- function(raw_births_data) {
  births_data <- raw_births_data |>
    dplyr::rename_with(~ tolower(gsub(" |-", "_", .x))) |>
    dplyr::rename_with(
      ~ paste0("age_", .x, recycle0 = TRUE),
      dplyr::matches("^[0-9]")
    )

  return(births_data)
}
