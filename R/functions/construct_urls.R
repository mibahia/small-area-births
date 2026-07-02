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
construct_urls <- function(base_url, params, id_count) {
  # Get sequence, currently API call handles 1,000 rows per request
  limit_seq <- seq(from = 1, to = id_count, by = 1000)

  urls <-
    lapply(
      limit_seq,
      function(x) {
        paste0(base_url, params$where, x, "&", params$outFields, params$f)
      }
    )

  encoded_urls <- lapply(urls, utils::URLencode)

  return(encoded_urls)
}
