# function to extract url of latest ods/xls/csv/xlsx file on that webpage
# Checks URL is valid, then searches the ons_url for any xlsx links, turns relative links to absolute,
# then filters to the first (assuming most recent) link

#' Extracts available data file extensions (ods/xls/csv/xlsx/csdb) on an ONS webpage
#'
#' @param ons_url The URL for the ONS webpage which contains embedded dataset link/s.
#'
#' @return file_extensions - unique available data file extensions
#'
#' @importFrom RCurl url.exists
#' @importFrom rvest read_html
#' @importFrom rvest html_element
#' @importFrom rvest html_attr
#' @importFrom stringr str_subset
#' @importFrom stringr str_extract
#' @importFrom xml2 url_absolute
#'
#' @export
#'
#' @examples
#' ons_url <- paste("https://www.ons.gov.uk/peoplepopulationandcommunity/",
#' "healthandsocialcare/conditionsanddiseases/datasets/coronaviruscovid19",
#' "infectionsurveydata", sep="")
#'
#' extract_file_extensions(ons_url)
#'
extract_file_extensions <- function(ons_url) {

  # Tests it is a character string, if not stops
  if(!is.character(ons_url)) {
    stop("Invalid input")
  }

  # Tests if URL exists, if not stops
  if(!url.exists(ons_url, useragent="curl/5.2 Rcurl/1.98")) {
    stop("Invalid URL")
  }

  data_urls <- read_html(ons_url) %>%
    html_elements("a") %>%
    html_attr("href") %>%
    str_subset("\\.ods$|\\.xls$|\\.csv$|\\.xlsx$|\\.csdb$") %>%
    url_absolute(ons_url)

  # test that data links are available, if not stops
  if(length(data_urls) == 0) {
    stop("No data links available")
  }

  # Use str_extract to capture the substring after the last "."
  file_extensions <- str_extract(data_urls, "\\.\\w+$")

  # Remove the leading "." from the extracted substring
  file_extensions <- sub("^\\.", "", file_extensions)

  # filter to unique file extension type
  # TO FIX - need to find a way to filter to most recent data file types - see https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/datasets/childmortalitystatisticschildhoodinfantandperinatalchildhoodinfantandperinatalmortalityinenglandandwales
  file_extensions <- unique(file_extensions)

  return(file_extensions)

}


# TO FIX - need to enable user to specify the file extension they want to download
#' Finds the first (assuming most recent) data (ods/xls/csv/xlsx) URL on an ONS webpage
#'
#' @param ons_url The URL for the ONS webpage which contains embedded dataset link/s.
#'
#' @return data_url - the first, assuming most recent, data URL on the webpage
#'
#' @importFrom RCurl url.exists
#' @importFrom dplyr first
#' @importFrom rvest read_html
#' @importFrom rvest html_element
#' @importFrom rvest html_attr
#' @importFrom stringr str_subset
#' @importFrom xml2 url_absolute
#'
#' @export
#'
#' @examples
#' ons_url <- paste("https://www.ons.gov.uk/peoplepopulationandcommunity/",
#' "healthandsocialcare/conditionsanddiseases/datasets/coronaviruscovid19",
#' "infectionsurveydata", sep="")
#'
#' get_latest_ons_data_url(ons_url)
#'
get_latest_ons_data_url <- function(ons_url) {

  # Tests it is a character string, if not stops
  if(!is.character(ons_url)) {
    stop("Invalid input")
  }

  # Tests if URL exists, if not stops
  if(!url.exists(ons_url, useragent="curl/5.2 Rcurl/1.98")) {
    stop("Invalid URL")
  }

  data_urls <- read_html(ons_url) %>%
    html_elements("a") %>%
    html_attr("href") %>%
    str_subset("\\.ods$|\\.xls$|\\.csv$|\\.xlsx$|\\.csdb$") %>%
    url_absolute(ons_url)

  # test that data links are available, if not stops
  if(length(data_urls) == 0) {
    stop("No data links available")
  }

  data_url <- first(data_urls)

  return(data_url)
}

# function to download latest data to destination location
# (need to specify mode as wb(write binary) otherwise it will try to make the
# file compatible with Windows)

#' Downloads first (assuming most recent) dataset link embedded on ONS webpage
#' and saves in the user specified destination location (and will create new
#' folder directories if required)
#'
#' @param ons_url The URL for the ONS webpage which contains embedded dataset link/s.
#' @param destfilepath The destination location where you want to save the file.
#'
#' @return The destination file path folder
#' @importFrom RCurl url.exists
#' @importFrom utils download.file
#' @importFrom stringr str_locate_all
#' @export
#'
#' @examples
#' ons_url <- paste("https://www.ons.gov.uk/peoplepopulationandcommunity/",
#' "healthandsocialcare/conditionsanddiseases/datasets/coronaviruscovid19",
#' "infectionsurveydata", sep="")
#'
#'
#' destfilepath <- "data"
#'
#' download_latest_ons_data(ons_url, destfilepath)
#'
download_latest_ons_data <- function(ons_url, destfilepath) {

  data_url <- get_latest_ons_data_url(ons_url)

  ons_file_name <- substr(data_url,
                          max(str_locate_all(data_url,"\\/")[[1]]),
                          nchar(data_url))

  destfilepathwithext <- paste0(destfilepath, ons_file_name)

  #checks if destfile directory exists and if not creates it
  absolute_directory <- normalizePath(dirname(destfilepathwithext), mustWork = FALSE)


  if(!dir.exists(absolute_directory)) {
    dir.create(absolute_directory)
    print(paste0("Directory did not exist and has been created: ", absolute_directory))
  }


  download.file(data_url, destfilepathwithext, mode="wb")
  return(normalizePath(destfilepath))
}
