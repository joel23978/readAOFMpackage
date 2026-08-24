# Read AOFM ownership of Australian Government Securities

`read_ownership()` reads the public register or non-resident holdings
workbook and returns one long-form data frame per source worksheet. It
downloads over HTTPS without credentials and stages the workbook in a
temporary file; the package does not maintain a persistent cache. The
package-wide bounded timeout and size safeguards are applied.

## Usage

``` r
read_ownership(aofm_table, csv = FALSE)
```

## Arguments

- aofm_table:

  Either `ownership_public` or `ownership_nonresident`. It is normally
  selected through
  [`read_aofm()`](https://joel23978.github.io/readAOFM/reference/read_aofm.md).

- csv:

  Logical scalar (default `FALSE`). If `TRUE`, write one CSV per
  returned worksheet beneath `output/` in the current working directory.

## Value

A named list of data frames. Public ownership returns the first two
source worksheets; non-resident ownership returns source worksheets two
through four. Each component preserves its source identity columns and
contains `date` as a `Date` plus numeric `value` observations in long
form. Component names combine the table ID and source worksheet name.

## Details

Missing sheets, rows, columns, or changed source layouts cause an error.
The exact identity columns follow the current AOFM workbook.

## See also

[`read_aofm()`](https://joel23978.github.io/readAOFM/reference/read_aofm.md)
for the preferred interface and
[`search_aofm()`](https://joel23978.github.io/readAOFM/reference/search_aofm.md)
for offline catalogue discovery.

## Examples

``` r
search_aofm("foreign ownership")
#>    security        type                    id         reader
#> 1 ownership nonresident ownership_nonresident read_ownership
#>                               read_call
#> 1 read_aofm("ownership", "nonresident")

if (interactive()) {
  read_ownership("ownership_nonresident")
}
```
