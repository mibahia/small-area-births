#' Construct URLs for API call
#'
#' @description
#' `construct_urls` creates URLs based on the number of IDs.
#'
#' @details
#' This function constructs URLs to handle multiple API calls,
#' as the current limit for the Open Geography portal is 1,000 IDs.
#'
#' @param base_url A character.
#' @param params A list.
#' @param id_count A integer.
#' @return A list.
download_lookup <- function(urls) {
  lookup_data <- data.frame()

  for (open_geo_url in urls) {
    req <- httr2::request(open_geo_url)

    tryCatch(
      {
        req |> httr2::req_perform()
      },
      error = function(e) {
        cat("Check URL:", conditionMessage(e))
      }
    )

    cat("Downloading query:", sep = "\n\n")
    cat(strsplit(open_geo_url[[1]], "query?")[[1]][2], sep = "\n\n")
    cat(" ", sep = "\n\n")

    json_body <- req |>
      httr2::req_perform() |>
      httr2::resp_body_json()

    temp_list <- lapply(json_body$features, function(x) x$properties)
    temp_df <- dplyr::bind_rows(temp_list)
    lookup_data <- rbind(lookup_data, temp_df)
  }
  return(lookup_data)
}
