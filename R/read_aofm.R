aofm_dispatch_read <- function(aofm_table, child_fn, csv = FALSE) {
  switch(
    child_fn,
    read_eofy = read_eofy(aofm_table = aofm_table, csv = csv),
    read_eom = read_eom(aofm_table = aofm_table, csv = csv),
    read_transactional = read_transactional(aofm_table = aofm_table, csv = csv),
    read_syndication = read_syndication(aofm_table = aofm_table, csv = csv),
    read_ownership = read_ownership(aofm_table = aofm_table, csv = csv),
    read_secondary = read_secondary(aofm_table = aofm_table, csv = csv),
    read_premium = read_premium(aofm_table = aofm_table, csv = csv),
    stop(sprintf("No reader exists for table id '%s'.", aofm_table), call. = FALSE)
  )
}

#' Read and tidy one or more AOFM Data Hub tables
#'
#' `read_aofm()` is the preferred high-level interface to readAOFM. It maps
#' the optional `security` and `type` filters to the package's supported AOFM
#' table catalogue, downloads each selected workbook over HTTPS, and dispatches
#' to the appropriate family reader. The package does not require credentials
#' and the reader stages each workbook in a temporary file rather than
#' maintaining a persistent cache. Transport is bounded internally with a
#' 15-second connect timeout, a 120-second overall transfer limit, a 30-second
#' low-speed abort below 1 KiB/s, and a 100 MiB workbook-size limit; these are
#' implementation safeguards rather than public arguments.
#'
#' Supported table IDs cover end-of-financial-year positions, end-of-month
#' positions, Treasury Bond, Treasury Indexed Bond and Treasury Note
#' transactions, syndications, buybacks, retail and securities-lending data,
#' public and non-resident ownership, secondary-market turnover, and term
#' premium estimates. The catalogue also contains seven historical files that
#' do not have parsers; those rows are excluded here. Because their selector
#' fields are empty, [download_aofm_xlsx()] includes them only in an unfiltered
#' raw download of the full catalogue, not as individually selectable tables.
#'
#' @param security Optional exact security family (default `NULL`). Supported values are
#'   `summary`, `aggregate`, `tb`, `tib`, `tn`, `slf`, `ownership`, `retail`,
#'   and `termpremium`. If omitted, all supported families are considered.
#' @param type Optional exact table type (default `NULL`). Supported values are `dealt`,
#'   `settlement`, `issuance`, `syndication`, `buyback`, `turnover`, `public`,
#'   and `nonresident`. If omitted, all supported types for `security` are
#'   considered.
#' @param csv Logical scalar (default `FALSE`). If `TRUE`, pass the option to each family reader;
#'   parsed output is also written beneath `output/` in the current working
#'   directory. For a list result, one CSV is written per component.
#' @returns If the filters identify one table, the corresponding reader result:
#'   usually a long-form tibble/data frame with identifier columns, `date` or
#'   `period`, `name`, and `value`, or a named list of such data frames for
#'   multi-sheet workbook families. If the filters identify multiple tables,
#'   returns a named list keyed by stable table ID. With no filters, the list
#'   has one element for each of the 23 parser-supported tables.
#'
#'   Transactional and syndication results preserve workbook identifier fields
#'   and pivot measures to `name`/`value`; parsed dates are `Date` objects where
#'   the source provides dates. End-of-month and ownership readers return named
#'   lists of component data frames. Exact measure columns follow the current
#'   AOFM workbook and may change if AOFM changes its source layout.
#'
#' @details
#' A selection that matches no supported table throws an error. Transport
#' failures, non-workbook responses, empty files, missing sheets, missing
#' required columns, and incompatible upstream layouts also throw errors. The
#' AOFM source is external, so examples and tests should use local fixtures or
#' mocks when deterministic, offline execution is required.
#'
#' @seealso [search_aofm()] for offline discovery, [download_aofm_xlsx()] for
#'   raw workbook downloads, and the family readers [read_eom()],
#'   [read_transactional()], [read_syndication()], [read_ownership()],
#'   [read_secondary()], [read_premium()], and [read_eofy()].
#' @examples
#' # Discovery is local and does not contact AOFM.
#' search_aofm("tb issuance")[, c("id", "reader", "read_call")]
#'
#' # Exercise the real transactional parser without a network request when the
#' # package's tb_issuance fixture is installed.
#' if (requireNamespace("testthat", quietly = TRUE)) {
#'   fixture <- system.file("extdata", "tb_issuance.xlsx", package = "readAOFM")
#'   if (nzchar(fixture)) {
#'     result <- suppressMessages(testthat::with_mocked_bindings(
#'       read_aofm("tb", "issuance"),
#'       download_aofm_table_workbook = function(...) fixture,
#'       .package = "readAOFM"
#'     ))
#'     c(rows = nrow(result), columns = ncol(result))
#'   }
#' }
#'
#' @export

read_aofm <- function(security = NULL
                      , type = NULL
                      , csv = FALSE
) {
  matches <- aofm_index
  matches <- matches[matches$fn != "no function exists", , drop = FALSE]

  if (!is.null(security)) {
    matches <- matches[!is.na(matches$p.security) & matches$p.security == security, , drop = FALSE]
  }

  if (!is.null(type)) {
    matches <- matches[!is.na(matches$p.type) & matches$p.type == type, , drop = FALSE]
  }

  if (nrow(matches) == 0) {
    stop(
      sprintf(
        "No supported AOFM table matched security = %s and type = %s.",
        if (is.null(security)) "NULL" else dQuote(security),
        if (is.null(type)) "NULL" else dQuote(type)
      ),
      call. = FALSE
    )
  }

  if (nrow(matches) == 1) {
    return(aofm_dispatch_read(matches$id[[1]], matches$fn[[1]], csv = csv))
  }

  results <- lapply(seq_len(nrow(matches)), function(i) {
    aofm_dispatch_read(matches$id[[i]], matches$fn[[i]], csv = csv)
  })
  names(results) <- matches$id

  results
}
