# Network, outputs, and troubleshooting

`readAOFM` has two kinds of I/O: readers fetch public AOFM Data Hub
workbooks and optionally write parsed CSV files;
[`search_aofm()`](https://joel23978.github.io/readAOFM/reference/search_aofm.md)
is local and has no I/O. The examples below keep all writes in an
isolated temporary directory and replace the network downloader with a
packaged workbook snapshot.

## Network and staging behavior

The reader path resolves a table from the local catalogue, requests its
AOFM workbook over HTTPS, validates that the response looks like an
Excel workbook, and stages the file in a temporary location for parsing.
It does not require credentials and does not maintain a persistent
package cache. Repeating a live read can therefore observe a newer AOFM
workbook. Transport failures, HTTP errors, empty files, non-workbook
responses, and changed source layouts are reported with table context.

Use the offline search before a live call:

``` r

readAOFM::search_aofm("tb issuance")[, c("id", "read_call")]
#>            id                   read_call
#> 1 tb_issuance read_aofm("tb", "issuance")
```

## CSV output is opt-in

`csv = TRUE` preserves the parsed return value and asks the family
reader to write a CSV beneath `output/`. The following example uses the
real public
[`read_aofm()`](https://joel23978.github.io/readAOFM/reference/read_aofm.md)
and parser, but mocks only the internal downloader so that the result is
repeatable and no network request is made.

``` r

csv_probe <- with_temp_working_directory({
  parsed <- testthat::with_mocked_bindings(
    readAOFM::read_aofm("tb", "issuance", csv = TRUE),
    download_aofm_table_workbook = function(aofm_table) {
      stopifnot(identical(aofm_table, "tb_issuance"))
      tb_issuance_fixture
    },
    .package = "readAOFM"
  )

  output_file <- file.path("output", "tb_issuance.csv")
  stopifnot(file.exists(output_file))

  list(
    parsed = parsed,
    files = list.files(".", recursive = TRUE, all.files = FALSE),
    csv = data.frame(
      path = output_file,
      bytes = unname(file.info(output_file)$size),
      stringsAsFactors = FALSE
    )
  )
})

csv_probe$files
#> [1] "output/tb_issuance.csv"
csv_probe$csv
#>                     path   bytes
#> 1 output/tb_issuance.csv 2064015
utils::head(csv_probe$parsed[c("date_held", "name", "value")], 3)
#> # A tibble: 3 × 3
#>   date_held  name                value
#>   <date>     <chr>               <dbl>
#> 1 1982-08-05 coupon                 16
#> 2 1982-08-05 amount_offered  200000000
#> 3 1982-08-05 amount_allotted 200000000
```

Leaving `csv = FALSE` (the default) does not create an `output/`
directory. The raw helper
[`download_aofm_xlsx()`](https://joel23978.github.io/readAOFM/reference/download_aofm_xlsx.md)
has different behavior: it saves source workbooks beneath `data/` and is
intended for an explicitly writable project directory. Neither output
path is a package cache.

## Multi-result calls and cost

Selectors can intentionally identify several tables. For example,
`read_aofm(type = "issuance")` returns a named list for Treasury Bond,
Treasury Indexed Bond, and Treasury Note issuance, and it downloads each
workbook. A security-only call such as `read_aofm("tb")` similarly reads
every supported Treasury Bond table. Search first to see the exact set
and generated calls:

``` r

issuance <- readAOFM::search_aofm("issuance")
issuance[, c("security", "type", "id", "read_call")]
#>   security     type           id                    read_call
#> 1       tb issuance  tb_issuance  read_aofm("tb", "issuance")
#> 2      tib issuance tib_issuance read_aofm("tib", "issuance")
#> 3       tn issuance  tn_issuance  read_aofm("tn", "issuance")
```

Start with a single `security`/`type` pair when you are exploring, then
make a multi-result call once the additional network and parsing cost is
intended.

## Common errors and recovery

An invalid selector is detected locally and does not contact AOFM:

``` r

tryCatch(
  readAOFM::read_aofm("not-a-security"),
  error = function(error) conditionMessage(error)
)
#> [1] "No supported AOFM table matched security = \"not-a-security\" and type = NULL."
```

For a live download or parse failure, check the following in order:

1.  Run
    [`search_aofm()`](https://joel23978.github.io/readAOFM/reference/search_aofm.md)
    and copy the generated `read_call` so that selectors are exact.
2.  Check network/proxy/SSL access to the AOFM source URL and retry
    once.
3.  If the error mentions HTML, HTTP status, or workbook signature, AOFM
    may have returned an error page or redirect instead of an Excel
    workbook.
4.  If the error mentions sheets or required columns, the upstream
    workbook layout may have changed. Report the table ID, full error,
    and access date.
5.  If a CSV or raw download cannot be written, switch to a writable
    project directory or leave `csv = FALSE`.

The package does not accept a local workbook path through the public
readers. For deterministic checks, use the packaged snapshots as above
and keep any temporary working directory isolated from analysis outputs.

## Optional live troubleshooting probe

This normal live call is opt-in. It is disabled during ordinary vignette
and CRAN builds, so rendering remains deterministic and offline.

``` r

if (interactive() && identical(Sys.getenv("READAOFM_RUN_LIVE_EXAMPLES"), "true")) {
  live <- readAOFM::read_aofm("tb", "issuance")
  utils::head(live, 3)
} else {
  cat("Live probe skipped; set READAOFM_RUN_LIVE_EXAMPLES=true to opt in.\n")
}
#> Live probe skipped; set READAOFM_RUN_LIVE_EXAMPLES=true to opt in.
```
