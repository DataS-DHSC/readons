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

  element_info <- "h1.page-intro__title"

  Sys.sleep(4)

  df <- get_html_text(ons_url, element_info)

  Sys.sleep(4)

  df2 <- get_html_text(ons_url2, element_info)

  Sys.sleep(4)

  df3 <- get_html_text(ons_url3, element_info)

  Sys.sleep(4)

  df4 <- get_html_text(ons_url4, element_info)

  Sys.sleep(4)

  result <- "Dataset Coronavirus (COVID-19) Infection Survey: England"
  result2 <- "Dataset Unit labour cost and unit wage cost time series"
  result3 <- "Dataset Child mortality (death cohort) tables in England and Wales"
  result4 <- "Dataset UK trade time series"

  expect_equal(df, result)
  expect_equal(df2, result2)
  expect_equal(df3, result3)
  expect_equal(df4, result4)

})

test_that("broken url generates error", {
  ons_url <- paste0(
    "https://ww.ons.gov.uk/peoplpopulationandcommunity/",
    "healthandsocialcare/conditionsanddiseases/datasets/coronaviruscovid19",
    "infectionsurveydata"
  )

  element_info <- "h1.page-intro__title"

  Sys.sleep(1)

  expect_error(get_html_text(ons_url, element_info), "Invalid URL")
})


test_that("number parsed as ons_url generates error", {
  ons_url <- 999

  element_info <- "h1.page-intro__title"

  Sys.sleep(1)

  expect_error(get_html_text(ons_url, element_info), "Invalid input")
})

test_that("html element_info is a single string otherwise generates error", {
  ons_url <- paste0(
    "https://www.ons.gov.uk/peoplepopulationandcommunity/",
    "healthandsocialcare/conditionsanddiseases/datasets/coronaviruscovid19",
    "infectionsurveydata"
  )

  element_info <- c("h1.page-intro__title", "button class", "higher")

  Sys.sleep(1)

  expect_error(get_html_text(ons_url, element_info), "Invalid element information input")
})




