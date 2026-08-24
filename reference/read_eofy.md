# Read the AOFM end-of-financial-year executive summary

`read_eofy()` downloads the `summary` workbook from the AOFM Data Hub
and tidies its first worksheet into long form. The source is fetched
over HTTPS without credentials and staged in a temporary file; the
package does not maintain a persistent cache. Transport is bounded
internally with a 15-second connect timeout, a 120-second overall
transfer limit, a 30-second low-speed abort below 1 KiB/s, and a 100 MiB
workbook-size limit; these are not public arguments. A changed workbook
layout or missing required columns causes an error.

## Usage

``` r
read_eofy(aofm_table, csv = FALSE)
```

## Arguments

- aofm_table:

  Must be the catalogue ID `summary`. It is normally selected through
  [`read_aofm()`](https://joel23978.github.io/readAOFM/reference/read_aofm.md)
  rather than called directly.

- csv:

  Logical scalar (default `FALSE`). If `TRUE`, also write the parsed
  data to `output/eofy_executive_summary.csv` beneath the current
  working directory.

## Value

A tibble/data frame in long form. It contains the source identifier
columns plus `date` (a `Date`) and `value`; duplicate and missing
observations are removed by the parser. Exact identifier columns follow
the current AOFM workbook.

## See also

[`read_aofm()`](https://joel23978.github.io/readAOFM/reference/read_aofm.md)
for the preferred interface and
[`search_aofm()`](https://joel23978.github.io/readAOFM/reference/search_aofm.md)
for offline catalogue discovery.

## Examples

``` r
search_aofm("executive summary")
#>   security type      id    reader            read_call
#> 1  summary <NA> summary read_eofy read_aofm("summary")

# A live workbook read is opt-in so package examples remain offline.
if (interactive()) {
  read_eofy("summary")
}
```
