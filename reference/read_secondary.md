# Read AOFM secondary-market turnover

`read_secondary()` combines the tenor and investor-type worksheets from
a Treasury Bond or Treasury Indexed Bond turnover workbook. The workbook
is fetched over HTTPS without credentials and staged in a temporary
file; the package does not maintain a persistent cache. The package-wide
bounded timeout and size safeguards are applied.

## Usage

``` r
read_secondary(aofm_table, csv = FALSE)
```

## Arguments

- aofm_table:

  Either `tb_turnover` or `tib_turnover`. It is normally selected
  through
  [`read_aofm()`](https://joel23978.github.io/readAOFM/reference/read_aofm.md).

- csv:

  Logical scalar (default `FALSE`). If `TRUE`, write the parsed result
  to `output/<aofm_table>.csv` beneath the current working directory.

## Value

A tibble/data frame in long form with `period` as a `Date`, `group`
equal to `tenor` or `investor_type`, and `name`/`value` columns for the
turnover measures. Exact measure columns follow the current workbook.

## Details

AOFM publishes turnover quarterly with a reporting lag. Missing sheets,
periods, or changed workbook layouts cause an error.

## See also

[`read_aofm()`](https://joel23978.github.io/readAOFM/reference/read_aofm.md)
for the preferred interface and
[`search_aofm()`](https://joel23978.github.io/readAOFM/reference/search_aofm.md)
for offline catalogue discovery.

## Examples

``` r
search_aofm("secondary market turnover")
#>   security     type           id         reader                    read_call
#> 1       tb turnover  tb_turnover read_secondary  read_aofm("tb", "turnover")
#> 2      tib turnover tib_turnover read_secondary read_aofm("tib", "turnover")

if (interactive()) {
  read_secondary("tb_turnover")
}
```
