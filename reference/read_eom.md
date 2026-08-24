# Read an AOFM end-of-month positions workbook

`read_eom()` downloads and tidies one of the eight end-of-month position
workbooks. The current parser returns the four data worksheets after the
notes sheet (`FaceValue`, `MarketValue`, `Delta`, and `Duration`);
although some source workbooks also contain a `Tenor` worksheet, it is
not currently included in the returned list. Dates are normalised to
`Date` and measures to numeric values.

## Usage

``` r
read_eom(aofm_table, csv = FALSE)
```

## Arguments

- aofm_table:

  One of `aggregate_position_dealt`, `aggregate_position_settlement`,
  `tb_position_dealt`, `tb_position_settlement`, `tib_position_dealt`,
  `tib_position_settlement`, `tn_position_dealt`, or
  `tn_position_settlement`. It is normally selected through
  [`read_aofm()`](https://joel23978.github.io/readAOFM/reference/read_aofm.md).

- csv:

  Logical scalar (default `FALSE`). If `TRUE`, write one CSV per
  returned component to `output/` beneath the current working directory.

## Value

A named list of four tibble/data-frame components. Each component
preserves workbook identity fields, adds `date` as a `Date`, and
contains numeric `value` observations in long form. Component names are
the stable table ID followed by the worksheet name, for example
`tb_position_dealt_FaceValue`.

## Details

The workbook is fetched over HTTPS without credentials and staged in a
temporary file. Missing worksheets, rows, columns, or an incompatible
AOFM layout cause an error; no persistent package cache is used.
Transport has the package-wide bounded timeout and size safeguards
described above.

## See also

[`read_aofm()`](https://joel23978.github.io/readAOFM/reference/read_aofm.md)
for the preferred interface and
[`search_aofm()`](https://joel23978.github.io/readAOFM/reference/search_aofm.md)
for offline catalogue discovery.

## Examples

``` r
search_aofm("tb dealt")
#>   security  type                id   reader                read_call
#> 1       tb dealt tb_position_dealt read_eom read_aofm("tb", "dealt")

# This is a complete official snapshot with four long data worksheets;
# run the offline parse interactively because it can take several seconds.
fixture <- system.file("extdata", "tb_position_dealt.xlsx", package = "readAOFM")
if (interactive() && requireNamespace("testthat", quietly = TRUE) && nzchar(fixture)) {
  result <- suppressMessages(testthat::with_mocked_bindings(
    read_eom("tb_position_dealt"),
    download_aofm_table_workbook = function(...) fixture,
    .package = "readAOFM"
  ))
  names(result)
}
```
