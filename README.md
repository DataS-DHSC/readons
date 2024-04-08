
<!-- README.md is generated from README.Rmd. Please edit that file -->

# readons

<!-- badges: start -->
<!-- badges: end -->

readons aims to facilitate webscraping of datasets from the UK Office
for National Statistics (ONS) website. It’s goal is to remove the manual
steps involved (and time taken) in navigating to an ONS webpage,
searching for the most recent dataset of interest and downloading a
local copy. It will be particularly useful for incorporating into
routine analysis that needs to be regularly updated based on the latest
ONS data.

Note that users are advised to first check whether the ONS data required
is available through API. If so, onsr package
(<https://cran.r-project.org/web/packages/onsr/readme/README.html>)
provides a client for the ‘Office of National Statistics’ (‘ONS’) API
<https://api.beta.ons.gov.uk/v1> and this currently covers data such as
wellbeing, deaths, trade, tax and benefits, suicides, gdp, population
estimates, life expectancy, labour market, GDP, and ASHE. See README
file for full list of data available through API. If the required ONS
data is not available through API, then the readons package can be used
to webscrape the data.

## Installation

You can install the readons package as follows:

``` r
if (!requireNamespace("librarian")) install.packages("librarian")
librarian::shelf(DataS-DHSC/readons)
```

## Usage and Example

To use this package you will need the URL for the ONS webpage that
contains the embedded data you are trying to download and will need to
specify an end destination for where you would like to save the
downloaded data.

``` r
# Assign the URL of the ONS webpage that contains required data to ons_url

ons_url <- "https://www.ons.gov.uk/peoplepopulationandcommunity/healthandsocialcare/conditionsanddiseases/datasets/coronaviruscovid19infectionsurveydata"

# Assign the destination you would like to save the downloaded data into as destfilepath (note that any new folder directories will be created if they do not already exist)

destfilepath <- "data"

# Download data to user specified destination

download_latest_ons_data(ons_url, destfilepath)

# Retrieve relevant metadata (dataset title, latest edition, release date, when it will next be updated and additional informaiton about the dataset)

get_metadata(ons_url)
```

## Notes

This package assumes that the first dataset link embedded on the ONS
webpage is the most recent. If ONS change the structure or formatting of
their webpages then this package will need to be updated to reflect any
changes.
