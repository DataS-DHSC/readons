test_that("valid url generates right output", {
  ons_url <- paste0(
    "https://www.ons.gov.uk/peoplepopulationandcommunity/",
    "healthandsocialcare/conditionsanddiseases/datasets/coronaviruscovid19",
    "infectionsurveydata"
  )

  Sys.sleep(1)

  df <- get_latest_ons_data_url(ons_url)

  result <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/healthandsocialcare/conditionsanddiseases/datasets/coronaviruscovid19infectionsurveydata/2023/20230310covid19infectionsurveydatasetsengland.xlsx"

  expect_equal(df, result)
})


test_that("invalid url generates error", {
  ons_url <- paste0(
    "https://ww.ons.gov.uk/peoplpopulationandcommunity/",
    "healthandsocialcare/conditionsanddiseases/datasets/coronaviruscovid19",
    "infectionsurveydata"
  )

  Sys.sleep(1)

  expect_error(get_latest_ons_data_url(ons_url), "Invalid URL")
})


test_that("number parsed as ons_url generates error", {
  ons_url <- 999

  Sys.sleep(1)

  expect_error(get_latest_ons_data_url(ons_url), "Invalid input")
})


test_that("url with no links to data generates error", {
  ons_url <- "https://www.ons.gov.uk/peoplepopulationandcommunity"

  Sys.sleep(1)

  expect_error(get_latest_ons_data_url(ons_url), "No data links available")

})


test_that("parsing a vector containing multiple urls generates error", {
  ons_url <- c("https://www.ons.gov.uk/peoplepopulationandcommunity/healthandsocialcare/conditionsanddiseases/datasets/coronaviruscovid19infectionsurveydata",
               "https://www.ons.gov.uk/peoplepopulationandcommunity/healthandsocialcare/childhealth/datasets/coronaviruscasesinschoolpupilsengland",
               "https://www.ons.gov.uk/peoplepopulationandcommunity/healthandsocialcare/childhealth/datasets/birthsandinfantmortalitybyethnicityenglandandwales")

  Sys.sleep(1)

  expect_error(get_latest_ons_data_url(ons_url), "the condition has length > 1")

})


test_that("destfile directory is created if did not previously exist", {
  ons_url <- paste0(
    "https://www.ons.gov.uk/peoplepopulationandcommunity/",
    "healthandsocialcare/conditionsanddiseases/datasets/coronaviruscovid19",
    "infectionsurveydata")

  destfilepath <- "data"

  Sys.sleep(1)

  expect_true(dir.exists(dirname(download_latest_ons_data(ons_url, destfilepath))))

  unlink("data", recursive = TRUE)
})



test_that("valid URL input results in file downloaded to newly created destfile directory", {
  ons_url <- paste0(
    "https://www.ons.gov.uk/peoplepopulationandcommunity/",
    "healthandsocialcare/conditionsanddiseases/datasets/coronaviruscovid19",
    "infectionsurveydata")

  destfilepath <- "data"

  Sys.sleep(1)

  download_latest_ons_data(ons_url, destfilepath)

  expect_true(file.exists(destfilepath))

  unlink("data", recursive = TRUE)
})


