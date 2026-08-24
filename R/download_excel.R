#' Download raw AOFM Data Hub workbooks
#'
#' `download_aofm_xlsx()` resolves exact `security` and `type` values against
#' the package's local AOFM catalogue, then downloads the matching `.xls` or
#' `.xlsx` workbooks over HTTPS. Raw files are written beneath `data/` in the
#' current working directory. The function does not require credentials and
#' does not maintain a package cache. Use [read_aofm()] for the usual
#' download-and-parse workflow; use [search_aofm()] to discover valid tables
#' without a network request.
#' Transport is bounded internally with a 15-second connect timeout, a
#' 120-second overall transfer limit, a 30-second low-speed abort below
#' 1 KiB/s, and a 100 MiB workbook-size limit. These safeguards are not public
#' function arguments.
#'
#' The catalogue contains seven historical rows without a parser. Their
#' selector fields are not populated, so they cannot be selected individually;
#' an unfiltered `download_aofm_xlsx()` call includes them together with every
#' other catalogue workbook. They cannot be read by [read_aofm()] until a
#' parser exists. A changed AOFM URL, non-workbook response, or changed workbook
#' layout causes an error from the downloader or the subsequent parser.
#'
#' @param security Optional exact security family (default `NULL`). Supported values are
#'   `summary`, `aggregate`, `tb`, `tib`, `tn`, `slf`, `ownership`, `retail`,
#'   and `termpremium`.
#' @param type Optional exact table type (default `NULL`). Supported values are `dealt`,
#'   `settlement`, `issuance`, `syndication`, `buyback`, `turnover`, `public`,
#'   and `nonresident`. If either argument is omitted, every matching catalogue
#'   row is selected.
#' @returns A character vector of matched table IDs, returned invisibly after
#'   the files have been downloaded. If no row matches, `NULL` is returned and
#'   an explanatory message is printed. The files themselves are written to
#'   `data/` and are not returned as R objects. The function creates `data/`
#'   before checking whether a selector matched, so a no-match call can create
#'   an empty directory.
#' @examples
#' # Catalogue discovery is offline and does not create files.
#' search_aofm("tb issuance")[, c("id", "read_call")]
#'
#' # Downloading is opt-in in examples because it requires the live AOFM site.
#' if (interactive()) {
#'   download_in_temporary_directory <- function() {
#'     old <- getwd()
#'     on.exit(setwd(old), add = TRUE)
#'     setwd(tempdir())
#'     download_aofm_xlsx("tb", "issuance")
#'   }
#'   download_in_temporary_directory()
#' }
#'
#' @export


download_aofm_xlsx <- function(security = NULL ## options include; tb, tib, tn, slf, summary, aggregate, ownership, retail, term.premium
                               , type =  NULL ## options include;
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

      download_aofm_workbook(file.row$file.path[[1]], file.path("data", file.name))
    }
    print("The following files have been downloaded to: data")
    print(aofm_table)
  }
}
