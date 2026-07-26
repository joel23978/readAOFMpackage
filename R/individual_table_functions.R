#' Reads in and cleans the EOFY Positions - Executive Summary	data from the AOFM, returns cleaned in a long format
#'
#' @param aofm_table object to download, "summary" is the only correct input, typically called from read_aofm()
#' @param csv if `TRUE`, also writes the parsed output to `output/`
#' @param timeout Per-attempt workbook transport timeout in seconds.
#' @param retries Retries after the first workbook transport attempt.
#' @param max_bytes Maximum accepted workbook size.
#' @returns AOFM eofy data as a dataframe
#' @examples
#' \dontrun{read_eofy("summary")}
#' # downloads EOFY Positions - Executive Summary	data from the AOFM, returns cleaned in a long format
#'
#' @importFrom zoo na.locf
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr filter "%>%" pull mutate distinct mutate_at vars
#' @importFrom stats na.omit
#'
#' @export

read_eofy <- function(aofm_table
                      , csv = FALSE
                      , timeout = getOption("readAOFM.timeout", 30)
                      , retries = getOption("readAOFM.retries", 1L)
                      , max_bytes = getOption("readAOFM.max_bytes", 100 * 1024^2)
) {
  tmp0 <- download_aofm_table_workbook(aofm_table, timeout, retries, max_bytes)
  aofm_parse_eofy_workbook(tmp0, csv = csv)
}








#' Reads in and cleans the "End of Month Positions" data from the AOFM, returns a list
#'
#' @param aofm_table object to download, typically called from read_aofm()
#' @param csv if `TRUE`, also writes the parsed output to `output/`
#' @param timeout Per-attempt workbook transport timeout in seconds.
#' @param retries Retries after the first workbook transport attempt.
#' @param max_bytes Maximum accepted workbook size.
#' @returns AOFM eom data is returned as a list with data frame elements for
#' FaceValue, MarketValue, Delta, Duration and Tenor
#' @examples
#'\dontrun{ read_eom("tb_position_dealt")}
#' # downloads Treasury Bond EOM data on a dealt basis from the AOFM
#' # returns cleaned in a long format
#' # data is returned as a list with data frame elements for
#' # FaceValue, MarketValue, Delta, Duration and Tenor
#' \dontrun{read_eom("tb_position_settlement")}
#' # downloads Treasury Bond EOM data on a settlement basis from the
#' # AOFM, returns cleaned in a long format as a list with elements for each of
#' # FaceValue, MarketValue, Delta, Duration and Tenor
#'
#' @importFrom zoo na.locf
#' @importFrom tidyr pivot_longer replace_na
#' @importFrom dplyr filter "%>%" pull mutate distinct mutate_at vars
#' @importFrom stringr str_detect
#' @importFrom janitor row_to_names
#' @importFrom readr write_csv
#' @importFrom stats na.omit
#'
#' @export

read_eom <- function(aofm_table
                     , csv = FALSE
                     , timeout = getOption("readAOFM.timeout", 30)
                     , retries = getOption("readAOFM.retries", 1L)
                     , max_bytes = getOption("readAOFM.max_bytes", 100 * 1024^2)
) {
  tmp0 <- download_aofm_table_workbook(aofm_table, timeout, retries, max_bytes)
  aofm_parse_eom_workbook(tmp0, aofm_table = aofm_table, csv = csv)
}














#' Reads in and cleans data under "Transaction Details" from the AOFM, returns cleaned in a long format (excl. issunace via conversion and syndication details)
#'
#' @param aofm_table object to download, typically called from read_aofm()
#' @param csv if `TRUE`, also writes the parsed output to `output/`
#' @param timeout Per-attempt workbook transport timeout in seconds.
#' @param retries Retries after the first workbook transport attempt.
#' @param max_bytes Maximum accepted workbook size.
#' @returns AOFM Transactional data as a dataframe
#' @examples
#' \dontrun{read_transactional("tb_issunace")}
#' # downloads Treasury Bond Issunace data from the AOFM, returns cleaned in a long format
#' \dontrun{read_transactional("slf")}
#' # downloads Securities Lending Facility data from the AOFM, returns cleaned in a long format
#'
#' @importFrom zoo na.locf
#' @importFrom tidyr pivot_longer replace_na
#' @importFrom dplyr filter "%>%" pull mutate distinct mutate_at vars mutate_if select where select_if
#' @importFrom stringr str_detect
#' @importFrom janitor row_to_names clean_names
#' @importFrom stats na.omit
#' @importFrom readr write_csv
#'
#' @export

read_transactional <- function(aofm_table
                               , csv = FALSE
                               , timeout = getOption("readAOFM.timeout", 30)
                               , retries = getOption("readAOFM.retries", 1L)
                               , max_bytes = getOption("readAOFM.max_bytes", 100 * 1024^2)
) {
  tmp0 <- download_aofm_table_workbook(aofm_table, timeout, retries, max_bytes)
  aofm_parse_transactional_workbook(tmp0, aofm_table = aofm_table, csv = csv)
}









#' Reads in and cleans Syndication details data under "Transaction Details" from the AOFM, returns cleaned in a long format
#'
#' @param aofm_table object to download, typically called from read_aofm()
#' @param csv if `TRUE`, also writes the parsed output to `output/`
#' @param timeout Per-attempt workbook transport timeout in seconds.
#' @param retries Retries after the first workbook transport attempt.
#' @param max_bytes Maximum accepted workbook size.
#' @returns AOFM Transactional data as a dataframe
#' @examples
#' \dontrun{read_transactional("tb_syndication")}
#' # downloads Treasury Bond Syndication details data from the AOFM, returns cleaned in a long format
#'
#' @importFrom zoo na.locf
#' @importFrom tidyr pivot_longer replace_na
#' @importFrom dplyr filter "%>%" pull mutate distinct mutate_at vars mutate_if select where select_if
#' @importFrom stringr str_detect
#' @importFrom janitor row_to_names clean_names
#' @importFrom readr write_csv
#' @importFrom stats na.omit
#'
#' @export


read_syndication <- function(aofm_table
                             , csv = FALSE
                             , timeout = getOption("readAOFM.timeout", 30)
                             , retries = getOption("readAOFM.retries", 1L)
                             , max_bytes = getOption("readAOFM.max_bytes", 100 * 1024^2)
) {
  tmp0 <- download_aofm_table_workbook(aofm_table, timeout, retries, max_bytes)
  aofm_parse_syndication_workbook(tmp0, aofm_table = aofm_table, csv = csv)
}









#' Reads in and cleans data under "AGS Secondary Market Turnover" from the AOFM, returns cleaned in a long format
#'
#' @param aofm_table object to download, typically called from read_aofm()
#' @param csv if `TRUE`, also writes the parsed output to `output/`
#' @param timeout Per-attempt workbook transport timeout in seconds.
#' @param retries Retries after the first workbook transport attempt.
#' @param max_bytes Maximum accepted workbook size.
#' @returns AOFM Transactional data as a dataframe
#' @examples
#' \dontrun{read_secondary("tb_turnover")}
#' # downloads Treasury Bond Turnover data from the AOFM, returns cleaned in a long format
#'
#' @importFrom zoo na.locf
#' @importFrom tidyr pivot_longer replace_na
#' @importFrom dplyr filter "%>%" pull mutate distinct mutate_at vars mutate_if select where select_if
#' @importFrom stringr str_detect
#' @importFrom janitor row_to_names clean_names
#' @importFrom readr write_csv
#'
#' @export

read_secondary <- function(aofm_table
                           , csv = FALSE
                           , timeout = getOption("readAOFM.timeout", 30)
                           , retries = getOption("readAOFM.retries", 1L)
                           , max_bytes = getOption("readAOFM.max_bytes", 100 * 1024^2)
) {
  tmp0 <- download_aofm_table_workbook(aofm_table, timeout, retries, max_bytes)
  aofm_parse_secondary_workbook(tmp0, aofm_table = aofm_table, csv = csv)
}







#' Reads in and cleans data under "Term Premium Estimates" from the AOFM, returns cleaned in a long format
#'
#' @param aofm_table object to download, typically called from read_aofm()
#' @param csv if `TRUE`, also writes the parsed output to `output/`
#' @param timeout Per-attempt workbook transport timeout in seconds.
#' @param retries Retries after the first workbook transport attempt.
#' @param max_bytes Maximum accepted workbook size.
#' @returns AOFM Transactional data as a dataframe
#' @examples
#' \dontrun{read_premium("termpremium")}
#' # downloads Term Premium data from the AOFM, returns cleaned in a long format
#'
#' @importFrom zoo na.locf
#' @importFrom tidyr pivot_longer replace_na
#' @importFrom dplyr filter "%>%" pull mutate distinct mutate_at vars mutate_if select where select_if arrange
#' @importFrom stringr str_detect
#' @importFrom janitor row_to_names clean_names
#' @importFrom readr write_csv
#' @importFrom readxl cell_rows
#'
#' @export

read_premium <- function(aofm_table
                         , csv = FALSE
                         , timeout = getOption("readAOFM.timeout", 30)
                         , retries = getOption("readAOFM.retries", 1L)
                         , max_bytes = getOption("readAOFM.max_bytes", 100 * 1024^2)
) {
  tmp0 <- download_aofm_table_workbook(aofm_table, timeout, retries, max_bytes)
  aofm_parse_premium_workbook(tmp0, aofm_table = aofm_table, csv = csv)
}









#' Reads in and cleans data under "Ownership of Australia Government Securities" from the AOFM, returns cleaned in a long format
#'
#' @param aofm_table object to download, typically called from read_aofm()
#' @param csv if `TRUE`, also writes the parsed output to `output/`
#' @param timeout Per-attempt workbook transport timeout in seconds.
#' @param retries Retries after the first workbook transport attempt.
#' @param max_bytes Maximum accepted workbook size.
#' @returns AOFM ownership data as a list, with elements relating to sheets in the original file
#' @examples
#' \dontrun{read_ownership("ownership_nonresident")}
#'
#' @importFrom zoo na.locf
#' @importFrom tidyr pivot_longer replace_na
#' @importFrom dplyr filter "%>%" pull mutate distinct mutate_at vars mutate_if select where select_if arrange
#' @importFrom stringr str_detect
#' @importFrom janitor row_to_names clean_names
#' @importFrom readr write_csv
#' @importFrom readxl cell_rows
#'
#' @export
#

read_ownership <- function(aofm_table
                           , csv = FALSE
                           , timeout = getOption("readAOFM.timeout", 30)
                           , retries = getOption("readAOFM.retries", 1L)
                           , max_bytes = getOption("readAOFM.max_bytes", 100 * 1024^2)
) {
  tmp0 <- download_aofm_table_workbook(aofm_table, timeout, retries, max_bytes)
  aofm_parse_ownership_workbook(tmp0, aofm_table = aofm_table, csv = csv)
}
