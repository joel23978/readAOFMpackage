% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/individual_table_functions.R
\name{read_eofy}
\alias{read_eofy}
\title{Reads in and cleans the EOFY Positions - Executive Summary	data from the AOFM, returns cleaned in a long format}
\usage{
read_eofy(aofm_table, csv = F)
}
\arguments{
\item{aofm_table}{object to download, "summary" is the only correct input, typically called from read_aofm()}

\item{csv}{do you want to export the cleaned data as a csv to /data}
}
\value{
AOFM eofy data as a dataframe
}
\description{
Reads in and cleans the EOFY Positions - Executive Summary	data from the AOFM, returns cleaned in a long format
}
\examples{
\dontrun{read_eofy("summary")}
# downloads EOFY Positions - Executive Summary	data from the AOFM, returns cleaned in a long format

}
