#' Read the AOFM end-of-financial-year executive summary
#'
#' `read_eofy()` downloads the `summary` workbook from the AOFM Data Hub and
#' tidies its first worksheet into long form. The source is fetched over HTTPS
#' without credentials and staged in a temporary file; the package
#' does not maintain a persistent cache. Transport is bounded internally with a
#' 15-second connect timeout, a 120-second overall transfer limit, a 30-second
#' low-speed abort below 1 KiB/s, and a 100 MiB workbook-size limit; these are
#' not public arguments. A changed workbook layout or missing required columns
#' causes an error.
#'
#' @param aofm_table Must be the catalogue ID `summary`. It is normally selected
#'   through [read_aofm()] rather than called directly.
#' @param csv Logical scalar (default `FALSE`). If `TRUE`, also write the parsed data to
#'   `output/eofy_executive_summary.csv` beneath the current working directory.
#' @returns A tibble/data frame in long form. It contains the source identifier
#'   columns plus `date` (a `Date`) and `value`; duplicate and missing
#'   observations are removed by the parser. Exact identifier columns follow
#'   the current AOFM workbook.
#' @seealso [read_aofm()] for the preferred interface and [search_aofm()] for
#'   offline catalogue discovery.
#' @examples
#' search_aofm("executive summary")
#'
#' # A live workbook read is opt-in so package examples remain offline.
#' if (interactive()) {
#'   read_eofy("summary")
#' }
#'
#' @export

read_eofy <- function(aofm_table
                      , csv = FALSE
) {
  tmp0 <- download_aofm_table_workbook(aofm_table)
  aofm_parse_eofy_workbook(tmp0, csv = csv)
}








#' Read an AOFM end-of-month positions workbook
#'
#' `read_eom()` downloads and tidies one of the eight end-of-month position
#' workbooks. The current parser returns the four data worksheets after the
#' notes sheet (`FaceValue`, `MarketValue`, `Delta`, and `Duration`); although
#' some source workbooks also contain a `Tenor` worksheet, it is not currently
#' included in the returned list. Dates are normalised to `Date` and measures
#' to numeric values.
#'
#' @param aofm_table One of `aggregate_position_dealt`,
#'   `aggregate_position_settlement`, `tb_position_dealt`,
#'   `tb_position_settlement`, `tib_position_dealt`,
#'   `tib_position_settlement`, `tn_position_dealt`, or
#'   `tn_position_settlement`. It is normally selected through [read_aofm()].
#' @param csv Logical scalar (default `FALSE`). If `TRUE`, write one CSV per returned component
#'   to `output/` beneath the current working directory.
#' @returns A named list of four tibble/data-frame components. Each component
#'   preserves workbook identity fields, adds `date` as a `Date`, and contains
#'   numeric `value` observations in long form. Component names are the stable
#'   table ID followed by the worksheet name, for example
#'   `tb_position_dealt_FaceValue`.
#' @details The workbook is fetched over HTTPS without credentials and staged
#'   in a temporary file. Missing worksheets, rows, columns, or an incompatible
#'   AOFM layout cause an error; no persistent package cache is used. Transport
#'   has the package-wide bounded timeout and size safeguards described above.
#' @seealso [read_aofm()] for the preferred interface and [search_aofm()] for
#'   offline catalogue discovery.
#' @examples
#' search_aofm("tb dealt")
#'
#' # This is a complete official snapshot with four long data worksheets;
#' # run the offline parse interactively because it can take several seconds.
#' fixture <- system.file("extdata", "tb_position_dealt.xlsx", package = "readAOFM")
#' if (interactive() && requireNamespace("testthat", quietly = TRUE) && nzchar(fixture)) {
#'   result <- suppressMessages(testthat::with_mocked_bindings(
#'     read_eom("tb_position_dealt"),
#'     download_aofm_table_workbook = function(...) fixture,
#'     .package = "readAOFM"
#'   ))
#'   names(result)
#' }
#'
#' @export

read_eom <- function(aofm_table
                     , csv = FALSE
) {
  tmp0 <- download_aofm_table_workbook(aofm_table)
  aofm_parse_eom_workbook(tmp0, aofm_table = aofm_table, csv = csv)
}














#' Read a transactional AOFM workbook
#'
#' `read_transactional()` handles tender, buyback, retail-facility, and
#' securities-lending workbooks. It downloads the selected workbook over HTTPS
#' without credentials, stages it in a temporary file, and pivots numeric
#' measures into long form. The package has no persistent cache and
#' applies the package-wide bounded timeout and size safeguards described in
#' [read_aofm()].
#'
#' @param aofm_table One of `tb_issuance`, `tb_buyback`, `tib_issuance`,
#'   `tib_buyback`, `tn_issuance`, `retail`, or `slf`. It is normally selected
#'   through [read_aofm()].
#' @param csv Logical scalar (default `FALSE`). If `TRUE`, write the parsed result to
#'   `output/<aofm_table>.csv` beneath the current working directory.
#' @returns A tibble/data frame in long form. Workbook identifier columns are
#'   retained; numeric measures are represented by `name` and `value`. Known
#'   date fields such as `date_held`, `date_settled`, `maturity`, `settle_date`,
#'   `start_date`, `end_date`, and `security_maturity_date` are normalised to
#'   `Date` where present. Exact measures and identifier columns follow the
#'   current source workbook.
#' @details Empty files, missing required columns, non-workbook responses, and
#'   changed AOFM layouts cause an error. Missing measure values are omitted by
#'   the parser.
#' @seealso [read_aofm()] for the preferred interface and [search_aofm()] for
#'   offline catalogue discovery.
#' @examples
#' search_aofm("tb issuance")
#'
#' fixture <- system.file("extdata", "tb_issuance.xlsx", package = "readAOFM")
#' if (requireNamespace("testthat", quietly = TRUE) && nzchar(fixture)) {
#'   result <- suppressMessages(testthat::with_mocked_bindings(
#'     read_transactional("tb_issuance"),
#'     download_aofm_table_workbook = function(...) fixture,
#'     .package = "readAOFM"
#'   ))
#'   head(result[c("date_held", "name", "value")])
#' }
#'
#' @export

read_transactional <- function(aofm_table
                               , csv = FALSE
) {
  tmp0 <- download_aofm_table_workbook(aofm_table)
  aofm_parse_transactional_workbook(tmp0, aofm_table = aofm_table, csv = csv)
}









#' Read AOFM syndication details
#'
#' `read_syndication()` reads the syndicated-issue workbooks for Treasury Bonds
#' or Treasury Indexed Bonds. It downloads over HTTPS without credentials,
#' stages the workbook in a temporary file, and combines the source sheets into
#' a single long-form result. No persistent package cache is used; the package-
#' wide bounded timeout and size safeguards are applied.
#'
#' @param aofm_table Either `tb_syndication` or `tib_syndication`. It is
#'   normally selected through [read_aofm()].
#' @param csv Logical scalar (default `FALSE`). If `TRUE`, write the parsed result to
#'   `output/<aofm_table>.csv` beneath the current working directory.
#' @returns A tibble/data frame with source identifier columns, `pricing_date`
#'   and `settlement_date` as `Date` values where present, a `type` identifying
#'   `new_bond` or `tap`, and long-form `name` and `value` columns. Exact source
#'   fields follow the current AOFM workbook.
#' @details Notes worksheets are excluded. Empty files, missing required date
#'   fields or value columns, and changed source layouts cause an error.
#' @seealso [read_aofm()] for the preferred interface and [search_aofm()] for
#'   offline catalogue discovery.
#' @examples
#' search_aofm("tb syndication")
#'
#' fixture <- system.file("extdata", "tb_syndication.xlsx", package = "readAOFM")
#' if (requireNamespace("testthat", quietly = TRUE) && nzchar(fixture)) {
#'   result <- suppressMessages(testthat::with_mocked_bindings(
#'     read_syndication("tb_syndication"),
#'     download_aofm_table_workbook = function(...) fixture,
#'     .package = "readAOFM"
#'   ))
#'   head(result[c("pricing_date", "type", "name", "value")])
#' }
#'
#' @export


read_syndication <- function(aofm_table
                             , csv = FALSE
) {
  tmp0 <- download_aofm_table_workbook(aofm_table)
  aofm_parse_syndication_workbook(tmp0, aofm_table = aofm_table, csv = csv)
}









#' Read AOFM secondary-market turnover
#'
#' `read_secondary()` combines the tenor and investor-type worksheets from a
#' Treasury Bond or Treasury Indexed Bond turnover workbook. The workbook is
#' fetched over HTTPS without credentials and staged in a temporary file; the
#' package does not maintain a persistent cache. The package-wide
#' bounded timeout and size safeguards are applied.
#'
#' @param aofm_table Either `tb_turnover` or `tib_turnover`. It is normally
#'   selected through [read_aofm()].
#' @param csv Logical scalar (default `FALSE`). If `TRUE`, write the parsed result to
#'   `output/<aofm_table>.csv` beneath the current working directory.
#' @returns A tibble/data frame in long form with `period` as a `Date`, `group`
#'   equal to `tenor` or `investor_type`, and `name`/`value` columns for the
#'   turnover measures. Exact measure columns follow the current workbook.
#' @details AOFM publishes turnover quarterly with a reporting lag. Missing
#'   sheets, periods, or changed workbook layouts cause an error.
#' @seealso [read_aofm()] for the preferred interface and [search_aofm()] for
#'   offline catalogue discovery.
#' @examples
#' search_aofm("secondary market turnover")
#'
#' if (interactive()) {
#'   read_secondary("tb_turnover")
#' }
#'
#' @export

read_secondary <- function(aofm_table
                           , csv = FALSE
) {
  tmp0 <- download_aofm_table_workbook(aofm_table)
  aofm_parse_secondary_workbook(tmp0, aofm_table = aofm_table, csv = csv)
}







#' Read AOFM term-premium estimates
#'
#' `read_premium()` downloads the `termpremium` workbook and combines its two
#' source worksheets into a date-sorted long-form result. The source is fetched
#' over HTTPS without credentials and staged in a temporary file; no persistent
#' package cache is used. The package-wide bounded timeout and size safeguards
#' are applied.
#'
#' @param aofm_table Must be the catalogue ID `termpremium`. It is normally
#'   selected through [read_aofm()].
#' @param csv Logical scalar (default `FALSE`). If `TRUE`, write the parsed result to
#'   `output/termpremium.csv` beneath the current working directory.
#' @returns A tibble/data frame sorted by `date`, with `date` as a `Date`,
#'   `type` identifying the source worksheet, and long-form `name` and `value`
#'   columns. Exact measures follow the current AOFM workbook.
#' @details Missing date fields, empty workbooks, and changed worksheet layouts
#'   cause an error.
#' @seealso [read_aofm()] for the preferred interface and [search_aofm()] for
#'   offline catalogue discovery.
#' @examples
#' search_aofm("term premium")
#'
#' if (interactive()) {
#'   read_premium("termpremium")
#' }
#'
#' @export

read_premium <- function(aofm_table
                         , csv = FALSE
) {
  tmp0 <- download_aofm_table_workbook(aofm_table)
  aofm_parse_premium_workbook(tmp0, aofm_table = aofm_table, csv = csv)
}









#' Read AOFM ownership of Australian Government Securities
#'
#' `read_ownership()` reads the public register or non-resident holdings
#' workbook and returns one long-form data frame per source worksheet. It
#' downloads over HTTPS without credentials and stages the workbook in a
#' temporary file; the package does not maintain a persistent cache.
#' The package-wide bounded timeout and size safeguards are applied.
#'
#' @param aofm_table Either `ownership_public` or `ownership_nonresident`. It is
#'   normally selected through [read_aofm()].
#' @param csv Logical scalar (default `FALSE`). If `TRUE`, write one CSV per returned worksheet
#'   beneath `output/` in the current working directory.
#' @returns A named list of data frames. Public ownership returns the first two
#'   source worksheets; non-resident ownership returns source worksheets two
#'   through four. Each component preserves its source identity columns and
#'   contains `date` as a `Date` plus numeric `value` observations in long form.
#'   Component names combine the table ID and source worksheet name.
#' @details Missing sheets, rows, columns, or changed source layouts cause an
#'   error. The exact identity columns follow the current AOFM workbook.
#' @seealso [read_aofm()] for the preferred interface and [search_aofm()] for
#'   offline catalogue discovery.
#' @examples
#' search_aofm("foreign ownership")
#'
#' if (interactive()) {
#'   read_ownership("ownership_nonresident")
#' }
#'
#' @export
#

read_ownership <- function(aofm_table
                           , csv = FALSE
) {
  tmp0 <- download_aofm_table_workbook(aofm_table)
  aofm_parse_ownership_workbook(tmp0, aofm_table = aofm_table, csv = csv)
}
