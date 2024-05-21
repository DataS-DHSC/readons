test_that("valid url generates right output", {
  ons_url <- paste0(
    "https://www.ons.gov.uk/peoplepopulationandcommunity/",
    "healthandsocialcare/conditionsanddiseases/datasets/coronaviruscovid19",
    "infectionsurveydata"
  )

  ons_url2 <- paste0(
    "https://www.ons.gov.uk/employmentandlabourmarket/peopleinwork/",
    "labourproductivity/datasets/unitlabourcostandunitwagecosttimeseries"
  )

  ons_url3 <- paste0(
    "https://www.ons.gov.uk/peoplepopulationandcommunity/",
    "birthsdeathsandmarriages/deaths/datasets/",
    "childmortalitystatisticschildhoodinfantandperinatalchildhoodinfantand",
    "perinatalmortalityinenglandandwales"
  )

  ons_url4 <- paste0(
    "https://www.ons.gov.uk/economy/nationalaccounts/balanceofpayments/datasets/",
    "tradeingoodsmretsallbopeu2013timeseriesspreadsheet"
  )

  Sys.sleep(4)

  df <- get_latest_ons_data_url(ons_url)

  Sys.sleep(4)

  df2 <- get_latest_ons_data_url(ons_url2)

  Sys.sleep(5)

  df3 <- get_latest_ons_data_url(ons_url3)

  Sys.sleep(5)

  df4 <- get_latest_ons_data_url(ons_url4)

  Sys.sleep(5)

  result <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/healthandsocialcare/conditionsanddiseases/datasets/coronaviruscovid19infectionsurveydata/2023/20230310covid19infectionsurveydatasetsengland.xlsx"
  result2 <- "https://www.ons.gov.uk/file?uri=/employmentandlabourmarket/peopleinwork/labourproductivity/datasets/unitlabourcostandunitwagecosttimeseries/current/ucst.csv"
  result3 <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/datasets/childmortalitystatisticschildhoodinfantandperinatalchildhoodinfantandperinatalmortalityinenglandandwales/2021/cim2021deathcohortworkbook.xlsx"
  result4 <- "https://www.ons.gov.uk/file?uri=/economy/nationalaccounts/balanceofpayments/datasets/tradeingoodsmretsallbopeu2013timeseriesspreadsheet/current/mret.csv"

  expect_equal(df, result)
  expect_equal(df2, result2)
  expect_equal(df3, result3)
  expect_equal(df4, result4)
})


test_that("invalid url generates error", {
  ons_url <- paste0(
    "https://ww.ons.gov.uk/peoplpopulationandcommunity/",
    "healthandsocialcare/conditionsanddiseases/datasets/coronaviruscovid19",
    "infectionsurveydata"
  )

  Sys.sleep(4)

  expect_error(get_latest_ons_data_url(ons_url), "Invalid URL")

  Sys.sleep(4)

})


test_that("number parsed as ons_url generates error", {
  ons_url <- 999

  Sys.sleep(4)

  expect_error(get_latest_ons_data_url(ons_url), "Invalid input")

  Sys.sleep(4)
})


test_that("url with no links to data generates error", {
  ons_url <- "https://www.ons.gov.uk/peoplepopulationandcommunity"

  Sys.sleep(4)

  expect_error(get_latest_ons_data_url(ons_url), "No data links available")

  Sys.sleep(4)

})


test_that("parsing a vector containing multiple urls generates error", {
  ons_url <- c("https://www.ons.gov.uk/peoplepopulationandcommunity/healthandsocialcare/conditionsanddiseases/datasets/coronaviruscovid19infectionsurveydata",
               "https://www.ons.gov.uk/peoplepopulationandcommunity/healthandsocialcare/childhealth/datasets/coronaviruscasesinschoolpupilsengland",
               "https://www.ons.gov.uk/peoplepopulationandcommunity/healthandsocialcare/childhealth/datasets/birthsandinfantmortalitybyethnicityenglandandwales")

  Sys.sleep(4)

  expect_error(get_latest_ons_data_url(ons_url), "the condition has length > 1")

  Sys.sleep(4)

})


test_that("destfile directory is created if did not previously exist", {
  ons_url <- paste0(
    "https://www.ons.gov.uk/peoplepopulationandcommunity/",
    "healthandsocialcare/conditionsanddiseases/datasets/coronaviruscovid19",
    "infectionsurveydata")

  destfilepath <- "data"

  Sys.sleep(4)

  expect_true(dir.exists(dirname(download_latest_ons_data(ons_url, destfilepath))))

  Sys.sleep(4)

  unlink("data", recursive = TRUE)
})



test_that("valid URL input results in file downloaded to newly created destfile directory", {
  ons_url <- paste0(
    "https://www.ons.gov.uk/peoplepopulationandcommunity/",
    "healthandsocialcare/conditionsanddiseases/datasets/coronaviruscovid19",
    "infectionsurveydata")

  destfilepath <- "data"

  Sys.sleep(4)

  download_latest_ons_data(ons_url, destfilepath)

  Sys.sleep(4)

  expect_true(file.exists(destfilepath))

  unlink("data", recursive = TRUE)
})


