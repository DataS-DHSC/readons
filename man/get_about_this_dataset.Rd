% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getmetadata.R
\name{get_about_this_dataset}
\alias{get_about_this_dataset}
\title{Retrieves additional information about this dataset}
\usage{
get_about_this_dataset(ons_url)
}
\arguments{
\item{ons_url}{The ONS URL which contains the embedded dataset link.}
}
\value{
about_this dataset Additional information provided by ONS about this dataset.
}
\description{
Retrieves additional information about this dataset
}
\examples{
ons_url <- paste("https://www.ons.gov.uk/peoplepopulationandcommunity/",
"healthandsocialcare/conditionsanddiseases/datasets/coronaviruscovid19",
"infectionsurveydata", sep="")

get_about_this_dataset(ons_url)
}
