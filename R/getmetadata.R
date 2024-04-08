# functions to extract relevant metadata - title, release date, next updated, about this dataset, editions available

# generic function for retrieving html text
#' Retrieves html text
#'
#' @param ons_url The ONS URL from which you want to read text.
#' @param element_info The html element you want to read.
#'
#' @return html_text_script The text from the element you have specified.
#'
#' @importFrom rvest read_html
#' @importFrom rvest html_elements
#' @importFrom rvest html_text2
#' @importFrom RCurl url.exists
#' @export
#'
#' @examples
#' ons_url <- paste("https://www.ons.gov.uk/peoplepopulationandcommunity/",
#' "healthandsocialcare/conditionsanddiseases/datasets/coronaviruscovid19",
#' "infectionsurveydata", sep="")
#'
#' element_info <- "h1.page-intro__title"
#'
#' get_html_text(ons_url, element_info)

get_html_text <- function(ons_url, element_info){

  # Tests ons_url is a character object, if not stops
  if(!is.character(ons_url)) {
    stop("Invalid input")
  }

  # URL validation test
  if(!url.exists(ons_url, useragent="curl/5.2 Rcurl/1.98")) {
    stop("Invalid URL")
  }

  # Tests element_info is a single character string, if not stops
  if(!is.character(element_info) | length(element_info) != 1) {
    stop("Invalid element information input")
  }

  html_text_script <- read_html(ons_url) %>%
    html_elements(element_info) %>%
    html_text2()
  return(html_text_script)
}

# use generic function for retrieving html text within other get metadata functions
#' Retrieves dataset title from ONS webpage
#'
#' @param ons_url The ONS URL which contains the embedded dataset link.
#'
#' @return dataset_title The title of the dataset.
#' @importFrom stringr str_replace
#' @export
#'
#' @examples
#' ons_url <- paste("https://www.ons.gov.uk/peoplepopulationandcommunity/",
#' "healthandsocialcare/conditionsanddiseases/datasets/coronaviruscovid19",
#' "infectionsurveydata", sep="")
#'
#' get_dataset_title(ons_url)
#'
get_dataset_title <- function(ons_url){
  dataset_title <- get_html_text(ons_url,"h1.page-intro__title") %>%
    str_replace("Dataset ","")
  return(dataset_title)
}

#' Retrieves the release date of the latest dataset available
#'
#' @param ons_url The ONS URL which contains the embedded dataset link.
#'
#' @return release_date The release date of the latest dataset available.
#' @export
#'
#' @examples
#' ons_url <- paste("https://www.ons.gov.uk/peoplepopulationandcommunity/",
#' "healthandsocialcare/conditionsanddiseases/datasets/coronaviruscovid19",
#' "infectionsurveydata", sep="")
#'
#' get_release_date(ons_url)

get_release_date <- function(ons_url) {
  release_date <- get_html_text(ons_url,".meta__item:nth-child(2) .meta__term+ div")
  return(release_date)
}

#' Retrieves the date the next version of the dataset will be made available
#'
#' @param ons_url The ONS URL which contains the embedded dataset link.
#'
#' @return next_update The date the next version of the dataset will be made available
#' @export
#'
#' @examples
#' ons_url <- paste("https://www.ons.gov.uk/peoplepopulationandcommunity/",
#' "healthandsocialcare/conditionsanddiseases/datasets/coronaviruscovid19",
#' "infectionsurveydata", sep="")
#'
#' get_next_updated(ons_url)

get_next_updated <- function(ons_url) {
  next_update <- get_html_text(ons_url,".meta__item~ .meta__item+ .meta__item .meta__term+ div")
  return(next_update)
}

#' Retrieves additional information about this dataset
#'
#' @param ons_url The ONS URL which contains the embedded dataset link.
#'
#' @return about_this dataset Additional information provided by ONS about this dataset.
#' @importFrom stringr str_replace
#' @importFrom stringr str_detect
#' @importFrom dplyr as_tibble
#' @importFrom dplyr mutate
#' @importFrom dplyr filter
#' @importFrom dplyr pull
#' @importFrom dplyr row_number
#' @importFrom tidyr separate
#' @export
#'
#' @examples
#' ons_url <- paste("https://www.ons.gov.uk/peoplepopulationandcommunity/",
#' "healthandsocialcare/conditionsanddiseases/datasets/coronaviruscovid19",
#' "infectionsurveydata", sep="")
#'
#' get_about_this_dataset(ons_url)

get_about_this_dataset <- function(ons_url){

  about_this_dataset <- get_html_text(ons_url, "section") %>%
    as_tibble() %>%
    mutate(id = row_number()) %>%
    filter(str_detect(value,"About this Dataset")) %>%
    separate(col = value,
             sep = "\n",
             into = c("title","about")) %>%
    pull(about)

  return(about_this_dataset)
}


#' Retrieves a list of all available editions of the dataset
#'
#' @param ons_url The ONS URL which contains the embedded dataset link.
#'
#' @return available_editions A list of all available editions of the dataset.
#' @importFrom stringr str_subset
#' @export
#'
#' @examples
#' ons_url <- paste("https://www.ons.gov.uk/peoplepopulationandcommunity/",
#' "healthandsocialcare/conditionsanddiseases/datasets/coronaviruscovid19",
#' "infectionsurveydata", sep="")
#'
#' get_available_editions(ons_url)

get_available_editions <- function(ons_url){

  available_editions <- get_html_text(ons_url, "h3") %>%
    str_subset("edition")

  if(length(available_editions) == 0){
    available_editions <- "1 or fewer releases detected"}

  return(available_editions)
}

#' Retrieves the latest(first listed) edition name of the dataset
#'
#' @param ons_url The ONS URL which contains the embedded dataset link.
#'
#' @return latest_edition The latest edition name of the dataset
#' @importFrom dplyr first
#' @export
#'
#' @examples
#' ons_url <- paste("https://www.ons.gov.uk/peoplepopulationandcommunity/",
#' "healthandsocialcare/conditionsanddiseases/datasets/coronaviruscovid19",
#' "infectionsurveydata", sep="")
#'
#' get_latest_edition(ons_url)

get_latest_edition <- function(ons_url){

  latest_edition <- get_available_editions(ons_url) %>%
    first()

  return(latest_edition)
}

# Higher level function that runs all functions above to get relevant metadata and return metadata as list with each element named properly
#' Higher level function that retrieves relevant metadata for ONS datasets
#'
#' @param ons_url The ONS URL which contains the embedded dataset link.
#'
#' @return metadata A list containing the dataset title, latest edition, release date, when it will next be updated and additional information about the dataset.
#' @export
#'
#' @examples
#' ons_url <- paste("https://www.ons.gov.uk/peoplepopulationandcommunity/",
#' "healthandsocialcare/conditionsanddiseases/datasets/coronaviruscovid19",
#' "infectionsurveydata", sep="")
#'
#' get_metadata(ons_url)

get_metadata <- function(ons_url){

  metadata <- list(
    dataset_title = get_dataset_title(ons_url),
    about_this_dataset = get_about_this_dataset(ons_url),
    latest_edition = get_latest_edition(ons_url),
    release_date = get_release_date(ons_url),
    next_updated = get_next_updated(ons_url)
  )

  return(metadata)
}

