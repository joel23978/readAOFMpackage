# Read and tidy one or more AOFM Data Hub tables

`read_aofm()` is the preferred high-level interface to readAOFM. It maps
the optional `security` and `type` filters to the package's supported
AOFM table catalogue, downloads each selected workbook over HTTPS, and
dispatches to the appropriate family reader. The package does not
require credentials and the reader stages each workbook in a temporary
file rather than maintaining a persistent cache. Transport is bounded
internally with a 15-second connect timeout, a 120-second overall
transfer limit, a 30-second low-speed abort below 1 KiB/s, and a 100 MiB
workbook-size limit; these are implementation safeguards rather than
public arguments.

## Usage

``` r
read_aofm(security = NULL, type = NULL, csv = FALSE)
```

## Arguments

- security:

  Optional exact security family (default `NULL`). Supported values are
  `summary`, `aggregate`, `tb`, `tib`, `tn`, `slf`, `ownership`,
  `retail`, and `termpremium`. If omitted, all supported families are
  considered.

- type:

  Optional exact table type (default `NULL`). Supported values are
  `dealt`, `settlement`, `issuance`, `syndication`, `buyback`,
  `turnover`, `public`, and `nonresident`. If omitted, all supported
  types for `security` are considered.

- csv:

  Logical scalar (default `FALSE`). If `TRUE`, pass the option to each
  family reader; parsed output is also written beneath `output/` in the
  current working directory. For a list result, one CSV is written per
  component.

## Value

If the filters identify one table, the corresponding reader result:
usually a long-form tibble/data frame with identifier columns, `date` or
`period`, `name`, and `value`, or a named list of such data frames for
multi-sheet workbook families. If the filters identify multiple tables,
returns a named list keyed by stable table ID. With no filters, the list
has one element for each of the 23 parser-supported tables.

Transactional and syndication results preserve workbook identifier
fields and pivot measures to `name`/`value`; parsed dates are `Date`
objects where the source provides dates. End-of-month and ownership
readers return named lists of component data frames. Exact measure
columns follow the current AOFM workbook and may change if AOFM changes
its source layout.

## Details

Supported table IDs cover end-of-financial-year positions, end-of-month
positions, Treasury Bond, Treasury Indexed Bond and Treasury Note
transactions, syndications, buybacks, retail and securities-lending
data, public and non-resident ownership, secondary-market turnover, and
term premium estimates. The catalogue also contains seven historical
files that do not have parsers; those rows are excluded here. Because
their selector fields are empty,
[`download_aofm_xlsx()`](https://joel23978.github.io/readAOFM/reference/download_aofm_xlsx.md)
includes them only in an unfiltered raw download of the full catalogue,
not as individually selectable tables.

A selection that matches no supported table throws an error. Transport
failures, non-workbook responses, empty files, missing sheets, missing
required columns, and incompatible upstream layouts also throw errors.
The AOFM source is external, so examples and tests should use local
fixtures or mocks when deterministic, offline execution is required.

## See also

[`search_aofm()`](https://joel23978.github.io/readAOFM/reference/search_aofm.md)
for offline discovery,
[`download_aofm_xlsx()`](https://joel23978.github.io/readAOFM/reference/download_aofm_xlsx.md)
for raw workbook downloads, and the family readers
[`read_eom()`](https://joel23978.github.io/readAOFM/reference/read_eom.md),
[`read_transactional()`](https://joel23978.github.io/readAOFM/reference/read_transactional.md),
[`read_syndication()`](https://joel23978.github.io/readAOFM/reference/read_syndication.md),
[`read_ownership()`](https://joel23978.github.io/readAOFM/reference/read_ownership.md),
[`read_secondary()`](https://joel23978.github.io/readAOFM/reference/read_secondary.md),
[`read_premium()`](https://joel23978.github.io/readAOFM/reference/read_premium.md),
and
[`read_eofy()`](https://joel23978.github.io/readAOFM/reference/read_eofy.md).

## Examples

``` r
# Discovery is local and does not contact AOFM.
search_aofm("tb issuance")[, c("id", "reader", "read_call")]
#>            id             reader                   read_call
#> 1 tb_issuance read_transactional read_aofm("tb", "issuance")

# Exercise the real transactional parser without a network request when the
# package's tb_issuance fixture is installed.
if (requireNamespace("testthat", quietly = TRUE)) {
  fixture <- system.file("extdata", "tb_issuance.xlsx", package = "readAOFM")
  if (nzchar(fixture)) {
    result <- suppressMessages(testthat::with_mocked_bindings(
      read_aofm("tb", "issuance"),
      download_aofm_table_workbook = function(...) fixture,
      .package = "readAOFM"
    ))
    c(rows = nrow(result), columns = ncol(result))
  }
}
#>    rows columns 
#>   26268       7 
```
