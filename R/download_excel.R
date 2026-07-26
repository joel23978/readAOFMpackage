#' Download xlsx.xls files from AOFM DataHub
#'
#' @param security object you want data on, options include:
#' tb, tib, tn, slf, summary, aggregate, ownership, retail, termpremium
#' @param type the specific type of data you want, occasionally optional:
#' dealt, settlement, issuance, syndication, buyback, turnover,
#' @returns a list of downloaded files
#' @param timeout Per-attempt workbook transport timeout in seconds.
#' @param retries Retries after the first workbook transport attempt.
#' @param max_bytes Maximum accepted workbook size.
#' @examples
#' \dontrun{download_aofm_xlsx("tb", "issuance")}
#' # downloads AOFM Treasury Bond Issuance data to data/tb_issuance.xlsx
#' \dontrun{download_aofm_xlsx("tb")}
#' # downloads all Treasury Bond data from the AOFM Datahub incl
#' # issunace, syndication details, position data etc to data/
#' \dontrun{download_aofm_xlsx()}
#' # downloads all data from the AOFM Datahub to data/
#'
#' @importFrom dplyr filter "%>%" pull
#'
#' @export


download_aofm_xlsx <- function(security = NULL ## options include; tb, tib, tn, slf, summary, aggregate, ownership, retail, term.premium
                               , type =  NULL ## options include;
                               , timeout = getOption("readAOFM.timeout", 30)
                               , retries = getOption("readAOFM.retries", 1L)
                               , max_bytes = getOption(
                                 "readAOFM.max_bytes",
                                 100 * 1024^2
                               )
) {

  # run find_file function to determine which file to download
  aofm_table <- find_file(security, type)

  # check if /data sub folder exists and create if not
  if (dir.exists("data") == F) {
    dir.create("data")
  }

  if (is.null(aofm_table)==T){
    print(aofm_table)

  } else {
    for (table_id in aofm_table){
      file.row <- aofm_table_row(table_id)
      file.name <- file.row %>%
        pull(file.save)

      download_aofm_workbook(
        file.row$file.path[[1]],
        file.path("data", file.name),
        timeout = timeout,
        retries = retries,
        max_bytes = max_bytes,
        official_only = TRUE
      )
    }
    print("The following files have been downloaded to: data")
    print(aofm_table)
  }
}
