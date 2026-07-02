###### Step 1: Download and clean births, population at risk and lookup ######

source("R/functions/get_births_by_msoa.R")
source("R/functions/process_data.R")
source("R/functions/helper_functions.R")
source("R/functions/create_dataframe.R")
source("R/functions/calculate_rolling_sum_asfr.R")
source("R/functions/smooth_my_asfr_small_areas.R")
source("R/functions/get_geo_lookup.R")


if (!dir.exists("data/raw")) {
  dir.create("data/raw", recursive = TRUE)
}

file_paths <-
  list(
    raw_births_lad_msoa = "data/raw/raw_births_lad_msoa.xlsx",
    lad_to_msoa_lookup = "data/raw/lad23_to_msoa21_lookup.csv",
    population_at_risk = "data/raw/population_at_risk",
    msoa_names_lookup = "data/raw/msoa_2021_names.csv"
  )

if (!file.exists(file_paths$raw_births_lad_msoa)) {
  download.file(
    paste0(
      "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/",
      "birthsdeathsandmarriages/livebirths/adhocs/1391numbersoflivebirthsby",
      "localauthorityandmsoaenglandandwalesmidyear1992tomidyear2021/",
      "livebirthsfinal.xlsx"
    ),
    file_paths$raw_births_lad_msoa,
    mode = "wb"
  )
}

if (!file.exists(file_paths$lad_to_msoa)) {
  base_url <- paste0(
    "https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services",
    "/MSOA21_LAD23_EW_LU/FeatureServer/0/query?"
  )

  params <- list(
    where = "where=1=1 AND objectid>=",
    outFields = "outFields=MSOA21CD, MSOA21NM, LAD23CD, LAD23NM&",
    cacheHint = "false",
    f = "f=geojson"
  )
  lad_to_msoa_lookup <- get_geo_lookup(base_url = base_url, params = params)
  write.csv(
    x = lad_to_msoa_lookup,
    file = "data/raw/lad23_to_msoa21_lookup.csv",
    row.names = FALSE
  )
}

if (!file.exists(file_paths$msoa_names_lookup)) {
  download.file(
    "https://houseofcommonslibrary.github.io/msoanames/MSOA-Names-2.2.csv",
    "data/raw/msoa_2021_names.csv",
    mode = "wb"
  )
}

if (!dir.exists(file_paths$population_at_risk)) {
  print(glue::glue(
    "Downloading population at risk from Nomis. ",
    "It'll takes a few minutes...grab some ☕️"
  ))

  population_at_risk <- nomisr::nomis_get_data(
    id = "NM_2014_1", # MSOA population estimates
    geography = "TYPE152",
    gender = 2, # Female
    measures = 20100,
    c_age = c(116:151), # 15 - 50
    tidy = TRUE,
    select = c(
      "date",
      "geography_name",
      "geography_code",
      "gender_name",
      "c_age_name",
      "obs_value"
    )
  )

  parquet_path <- "data/raw/population_at_risk/"

  population_at_risk |>
    dplyr::group_by(date) |>
    arrow::write_dataset(path = parquet_path, format = "parquet")

  print(glue::glue("Population at risk estimates saved in {parquet_path}"))

  rm(population_at_risk, parquet_path)
}

###### Step 2: Clean and process all files ######

#' *Lookup*: reads in csv and split data by year
#' Currently set up for MSOA21 to LAD23
lookup_by_lad <- process_lookup(
  lookup_path = file_paths$lad_to_msoa_lookup,
  split_col = "LAD23CD"
)
msoa_names_lookup <- read.csv("data/raw/msoa_2021_names.csv") |>
  dplyr::select(msoa21cd, msoa21hclnm)

#' *Births data*: LAD and MSOA level data from the same raw file.
#' Output is named list nested list containing both datasets
#' The function below
#' i. reads in the excel file
#' ii. renames columns
#' iii. filters by common year
#' vi. recodes LAD boundaries from 2021 to 2023 (ONS guidance is incorrect)
#' v. adds rows for missing MSOAs - Births data has less 2 MSOAs for 2 years.
births_data <- process_births_data(
  births_data_path = file_paths$raw_births_lad_msoa,
  lookup_by_lad = lookup_by_lad,
  skip = 5,
  period_col = period,
  recode_from_year = 2021,
  recode_to_year = 2023
)

#' Quick look of the MSOA & LAD data:
par(mfrow = c(2, 5))
for (yr in names(births_data[["msoa"]])) {
  hist(
    births_data[["msoa"]][[yr]] |>
      dplyr::select(where(is.numeric)) |>
      rowSums(),
    breaks = 100,
    main = yr,
    xlab = "Row sum"
  )
}

for (yr in names(births_data[["lad"]])) {
  hist(
    births_data[["lad"]][[yr]] |>
      dplyr::select(where(is.numeric)) |>
      rowSums(),
    breaks = 100,
    main = yr,
    xlab = "Row sum"
  )
}

#' *Population at risk*: reads in parquet, pivot wider and split by year.
population_at_risk_by_year <- process_population_at_risk(
  parquet_path = file_paths$population_at_risk,
  split_col = "date"
)

###### Step 3: Run IPF for all years / local authorities ######
births_by_msoa <- get_births_by_msoa(
  births_lad_by_year = births_data[["lad"]],
  births_msoas_by_year = births_data[["msoa"]],
  lad_to_msoa_lookup = lookup_by_lad,
  population_at_risk_data = population_at_risk_by_year,
  msoa_code_col = "MSOA21CD",
  period = NULL
)

###### Step 4: Set single year or rolling sum ######
rolling_sum <- TRUE

if (rolling_sum) {
  final_output <- calculate_rolling_sum_asfr(
    births_by_msoa = births_by_msoa,
    lookup_by_lad = lookup_by_lad,
    msoa_names_lookup = msoa_names_lookup,
    k = 3,
    .keep = "unused"
  )
  file_name <- glue::glue(
    "{substitute(rolling_sum)}_asfr_msoa_from_2011_to_2021.rds"
  )
} else {
  final_output <- create_dataframe(
    births_by_msoa = births_by_msoa,
    output_type = "fertility_rate",
    msoa_names_lookup = msoa_names_lookup,
    lookup_by_lad = lookup_by_lad
  )
  file_name <- "asfr_msoa_from_2011_to_2021.rds"
}

processed_dir <- "data/processed"
if (!dir.exists(processed_dir)) {
  dir.create(processed_dir, recursive = TRUE)
}

saveRDS(final_output, glue::glue("{processed_dir}/{file_name}"))

###### Step 4: Smooth rates ######

smooth_asfr <- smooth_my_asfr_small_areas(
  raw_asfr_data = final_output,
  geography_col = "msoa21_code"
)

# Check fitting & saving output
prop.table(table(smooth_asfr$fitting_status))

saveRDS(
  smooth_asfr,
  glue::glue("{processed_dir}/smooth_{file_name}")
)

# Creating total fertility rates timeseries
total_fertility_rates <- smooth_asfr |>
  dplyr::group_by(msoa21_code, msoa21_name, year) |>
  dplyr::summarise(tfr = sum(fertility_rate), .groups = "drop_last") |>
  dplyr::left_join(
    smooth_asfr |>
      dplyr::distinct(msoa21_code, msoa21_name, gss_code, gss_name),
    by = c("msoa21_code", "msoa21_name")
  )

saveRDS(
  total_fertility_rates,
  glue::glue("{processed_dir}/msoa_tfr_timeseries.rds")
)
