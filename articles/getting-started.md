# Getting started with readAOFM

`readAOFM` reads workbook data published by the Australian Office of
Financial Management (AOFM) Data Hub and returns R-friendly data frames
or named lists. The examples in this article separate the two parts of a
normal workflow: discover a table locally, then read a selected
workbook. The first successful read uses a packaged workbook snapshot,
so it is deterministic and does not contact AOFM.

## Find a table without downloading anything

[`search_aofm()`](https://joel23978.github.io/readAOFM/reference/search_aofm.md)
searches the package’s local catalogue. By default it returns metadata
and ready-to-run calls; it does not make a network request.

``` r

readAOFM::search_aofm("tb issuance")[, c("id", "reader", "read_call")]
#>            id             reader                   read_call
#> 1 tb_issuance read_transactional read_aofm("tb", "issuance")
```

The search accepts ordinary descriptions as well as exact terminology.
For example, these searches identify the indexed-bond and non-resident
ownership families:

``` r

readAOFM::search_aofm("inflation")
#>   security        type                      id             reader
#> 1      tib     buyback             tib_buyback read_transactional
#> 2      tib       dealt      tib_position_dealt           read_eom
#> 3      tib    issuance            tib_issuance read_transactional
#> 4      tib  settlement tib_position_settlement           read_eom
#> 5      tib syndication         tib_syndication   read_syndication
#> 6      tib    turnover            tib_turnover     read_secondary
#>                         read_call
#> 1     read_aofm("tib", "buyback")
#> 2       read_aofm("tib", "dealt")
#> 3    read_aofm("tib", "issuance")
#> 4  read_aofm("tib", "settlement")
#> 5 read_aofm("tib", "syndication")
#> 6    read_aofm("tib", "turnover")
readAOFM::search_aofm("foreign ownership")
#>    security        type                    id         reader
#> 1 ownership nonresident ownership_nonresident read_ownership
#>                               read_call
#> 1 read_aofm("ownership", "nonresident")
```

The `read_call` column is useful when a query returns more than one
table. For example, `search_aofm("issuance")` finds Treasury Bond,
Treasury Indexed Bond, and Treasury Note issuance; reading all three
would perform three workbook downloads.

## Read the first table using a local fixture

The public entry point is `read_aofm(security, type, csv = FALSE)`. The
`security` and `type` values are exact selectors. A single match returns
that table’s parsed object. The package’s tests and vignettes use
[`testthat::with_mocked_bindings()`](https://testthat.r-lib.org/reference/local_mocked_bindings.html)
only to replace the internal downloader with the packaged workbook; the
call below still exercises the public dispatcher and the real parser.

``` r

tb_issuance <- testthat::with_mocked_bindings(
  readAOFM::read_aofm("tb", "issuance"),
  download_aofm_table_workbook = function(aofm_table) {
    stopifnot(identical(aofm_table, "tb_issuance"))
    tb_issuance_fixture
  },
  .package = "readAOFM"
)

stopifnot(
  is.data.frame(tb_issuance),
  all(c("date_held", "tender_number", "maturity", "name", "value") %in%
        names(tb_issuance)),
  inherits(tb_issuance$date_held, "Date")
)

data.frame(
  object_class = paste(class(tb_issuance), collapse = ", "),
  rows = nrow(tb_issuance),
  columns = ncol(tb_issuance),
  date_class = paste(class(tb_issuance$date_held), collapse = ", "),
  stringsAsFactors = FALSE
)
#>              object_class  rows columns date_class
#> 1 tbl_df, tbl, data.frame 26268       7       Date
```

The transactional reader preserves workbook identifiers and pivots
numeric measures into the long-form `name`/`value` columns. Date fields
handled by the reader are returned as `Date` objects. Exact source
columns can change if AOFM changes its workbook, so inspect names before
building a downstream analysis.

``` r

utils::head(tb_issuance[c("date_held", "maturity", "name", "value")], 3)
#> # A tibble: 3 × 4
#>   date_held  maturity   name                value
#>   <date>     <date>     <chr>               <dbl>
#> 1 1982-08-05 1984-04-15 coupon                 16
#> 2 1982-08-05 1984-04-15 amount_offered  200000000
#> 3 1982-08-05 1984-04-15 amount_allotted 200000000
```

For a single table,
[`read_aofm()`](https://joel23978.github.io/readAOFM/reference/read_aofm.md)
returns the parsed data frame directly. When selectors match several
supported tables, it returns a named list keyed by table ID; omitting
both selectors requests all 23 parser-supported tables and can therefore
involve many downloads. `csv = TRUE` keeps the same return value and
additionally writes the reader’s CSV output under `output/` in the
current working directory; see the network and outputs article for an
isolated example.

## Optional live read

The normal live call is intentionally guarded. Set
`READAOFM_RUN_LIVE_EXAMPLES=true` in an interactive or development
session to opt in after checking that the AOFM site is reachable. The
default vignette build path never makes a network request.

``` r

if (interactive() && identical(Sys.getenv("READAOFM_RUN_LIVE_EXAMPLES"), "true")) {
  live_tb_issuance <- readAOFM::read_aofm("tb", "issuance")
  stopifnot(is.data.frame(live_tb_issuance))
  utils::head(live_tb_issuance, 3)
} else {
  cat("Live read skipped; set READAOFM_RUN_LIVE_EXAMPLES=true to opt in.\n")
}
#> Live read skipped; set READAOFM_RUN_LIVE_EXAMPLES=true to opt in.
```

Live readers fetch public workbooks over HTTPS and do not require AOFM
credentials. They stage each workbook in a temporary file rather than
using a persistent package download cache. Upstream availability and
workbook layout remain outside the package’s control.
