#' This is the script used to create OA fertility rates for 2026 projections
#' You'll need to create ASFR by MSOA `R/msoa_births.R`

source("R/functions/get_geo_lookup.R")

###### Step 1: Get ASFR by MSOA  ######

smooth_asfr_msoa_my <- readRDS(
  "data/processed/smooth_rolling_sum_asfr_msoa_from_2011_to_2021.rds"
) |>
  dplyr::filter(year == max(year)) |>
  dplyr::select(age, msoa21_code, year, fertility_rate)

###### Step 2: Get OA21 to MSOA21 and MSOA21 to WARD22 lookups ######

oa21_msoa21_lookup <- get_geo_lookup(
  base_url = paste0(
    "https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services",
    "/OA_LSOA_MSOA_EW_DEC_2021_LU_v3/FeatureServer/0/query?"
  ),
  params = list(
    where = "where=1=1 AND objectid>=",
    outFields = "outFields=OA21CD, MSOA21CD&",
    cacheHint = "false",
    f = "f=geojson"
  )
) |>
  dplyr::rename_with(tolower)

oa21_ward22_lookup <- get_geo_lookup(
  base_url = paste0(
    "https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services",
    "/OA21_WD22_LTLA22_UTLA22_RGN22_CTRY22_EW_LU_v2/FeatureServer/0/query?"
  ),
  params = list(
    where = "where=1=1 AND objectid>=",
    outFields = "outFields=OA21CD, WD22CD, WD22NM, LTLA22CD, LTLA22NM&",
    cacheHint = "false",
    f = "f=geojson"
  )
) |>
  dplyr::rename_with(tolower)

###### Step 3: Get births by OA and calculate the mean ######

if (!file.exists("data/raw/birthsanddeathsmidyearfinal.xlsx")) {
  download.file(
    paste0(
      "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/",
      "birthsdeathsandmarriages/livebirths/adhocs/2798livebirthsandnumberof",
      "deathoccurencesby2021censusoutputareasandsexforenglandandwalesforperiod",
      "smidyear2022to2024/birthsanddeathsmidyearfinal.xlsx"
    ),
    "data/raw/birthsanddeathsmidyearfinal.xlsx"
  )
}

raw_oa_births <- readxl::read_excel(
  "data/raw/birthsanddeathsmidyearfinal.xlsx",
  sheet = "births",
  skip = 3
) |>
  janitor::clean_names()

oa_births <- raw_oa_births |>
  dplyr::group_by(output_area) |>
  dplyr::summarise(
    dplyr::across(dplyr::starts_with("x"), sum)
  ) |>
  dplyr::rowwise() |>
  dplyr::mutate(oa_births = mean(c(x2022, x2023, x2024))) |>
  dplyr::select(output_area, oa_births)

oa_weights <- oa_births |>
  dplyr::left_join(oa21_msoa21_lookup, by = c("output_area" = "oa21cd")) |>
  dplyr::left_join(
    smooth_asfr_msoa_my,
    by = c("msoa21cd" = "msoa21_code"),
    relationship = "many-to-many"
  ) |>
  dplyr::mutate(weighted_fertility = fertility_rate * oa_births)

###### Step 4: Get ward fertility rates ######

ward_asfr <- oa_weights |>
  dplyr::left_join(oa21_ward22_lookup, by = c("output_area" = "oa21cd")) |>
  dplyr::group_by(age, wd22cd, wd22nm) |>
  dplyr::summarise(
    rates_sum = sum(weighted_fertility),
    weights_sum = sum(oa_births),
    .groups = "drop"
  ) |>
  dplyr::mutate(fertility_rate = rates_sum / weights_sum)

ward_asfr_output <- ward_asfr |>
  dplyr::left_join(
    oa21_ward22_lookup |>
      dplyr::distinct(wd22cd, wd22nm, ltla22cd, ltla22nm),
    by = c("wd22cd", "wd22nm"),
    relationship = "many-to-many"
  ) |>
  dplyr::rename(gss_code = ltla22cd, gss_name = ltla22nm) |>
  dplyr::select(age, wd22cd, wd22nm, gss_code, gss_name, fertility_rate)

saveRDS(ward_asfr_output, "data/processed/ward_asfr.rds")

ward_tfr <- ward_asfr_output |>
  dplyr::group_by(wd22cd, wd22nm) |>
  dplyr::summarise(tfr = sum(fertility_rate), .groups = "drop_last") |>
  dplyr::left_join(
    oa21_ward22_lookup |>
      dplyr::distinct(wd22cd, wd22nm, ltla22cd, ltla22nm),
    by = c("wd22cd", "wd22nm"),
  ) |>
  dplyr::rename(gss_code = ltla22cd, gss_name = ltla22nm) |>
  dplyr::select(wd22cd, wd22nm, gss_code, gss_name, tfr)

saveRDS(ward_tfr, "data/processed/ward_tfr.rds")
