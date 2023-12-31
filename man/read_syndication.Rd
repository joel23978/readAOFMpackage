% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/individual_table_functions.R
\name{read_syndication}
\alias{read_syndication}
\title{Reads in and cleans Syndication details data under "Transaction Details" from the AOFM, returns cleaned in a long format}
\usage{
read_syndication(aofm_table, csv = F)
}
\arguments{
\item{aofm_table}{object to download, typically called from read_aofm()}

\item{csv}{do you want to export the cleaned data as a csv to /data}
}
\value{
AOFM Transactional data as a dataframe
}
\description{
Reads in and cleans Syndication details data under "Transaction Details" from the AOFM, returns cleaned in a long format
}
\examples{
\dontrun{read_transactional("tb_syndication")}
# downloads Treasury Bond Syndication details data from the AOFM, returns cleaned in a long format

}
