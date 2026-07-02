#' Get the number of observations
#'
#' @description
#' `get_observation_count` retrieves the number of observation IDs.
#'
#' @param base_url A character.
#' @param params A named list.
#' @return A integer.
get_observation_count <- function(base_url, params) {
  req <- httr2::request(base_url) |>
    httr2::req_url_query(
      where = params$where,
      returnIdsOnly = params$returnIdsOnly,
      returnCountOnly = params$returnCountOnly,
      cacheHint = params$cacheHint,
      f = params$f
    )

  tryCatch(
    {
      resp <- httr2::req_perform(req)
    },
    error = function(e) {
      cat("Check URL:", conditionMessage(e))
    }
  )

  observation_count <- resp |>
    httr2::resp_body_json()

  return(observation_count[[1]])
}
