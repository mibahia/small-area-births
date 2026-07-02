source("R/functions/get_observation_count.R")
source("R/functions/construct_urls.R")
source("R/functions/download_lookup.R")

#' Downloads a lookup from Open Geography Portal
#'
#' @description
#' `get_geo_lookup` downloads a lookup from Open Geography Portal API.
#'
#' @details
#' This function takes a base url and a list of certain parameters
#' to construct urls, as the API max is 1,000 rows. For more
#' information on the datasets available and query please visit:
#' https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services
#'
#' @param base_url A character.
#' @param params A named list. Only accepts: where, outFields, cacheHint, f
#' @return A data frame.
get_geo_lookup <- function(base_url, params) {
  count_params <- list(
    where = "1=1",
    returnIdsOnly = "true",
    returnCountOnly = "true",
    cacheHint = "false",
    f = "pjson"
  )

  id_count <- get_observation_count(base_url = base_url, params = count_params)

  urls <- construct_urls(
    base_url = base_url,
    params = params,
    id_count = id_count
  )

  lad_to_msoa_lookup <- download_lookup(urls = urls)

  return(lad_to_msoa_lookup)
}
