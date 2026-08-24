# Download raw AOFM Data Hub workbooks

`download_aofm_xlsx()` resolves exact `security` and `type` values
against the package's local AOFM catalogue, then downloads the matching
`.xls` or `.xlsx` workbooks over HTTPS. Raw files are written beneath
`data/` in the current working directory. The function does not require
credentials and does not maintain a package cache. Use
[`read_aofm()`](https://joel23978.github.io/readAOFM/reference/read_aofm.md)
for the usual download-and-parse workflow; use
[`search_aofm()`](https://joel23978.github.io/readAOFM/reference/search_aofm.md)
to discover valid tables without a network request. Transport is bounded
internally with a 15-second connect timeout, a 120-second overall
transfer limit, a 30-second low-speed abort below 1 KiB/s, and a 100 MiB
workbook-size limit. These safeguards are not public function arguments.

## Usage

``` r
download_aofm_xlsx(security = NULL, type = NULL)
```

## Arguments

- security:

  Optional exact security family (default `NULL`). Supported values are
  `summary`, `aggregate`, `tb`, `tib`, `tn`, `slf`, `ownership`,
  `retail`, and `termpremium`.

- type:

  Optional exact table type (default `NULL`). Supported values are
  `dealt`, `settlement`, `issuance`, `syndication`, `buyback`,
  `turnover`, `public`, and `nonresident`. If either argument is
  omitted, every matching catalogue row is selected.

## Value

A character vector of matched table IDs, returned invisibly after the
files have been downloaded. If no row matches, `NULL` is returned and an
explanatory message is printed. The files themselves are written to
`data/` and are not returned as R objects. The function creates `data/`
before checking whether a selector matched, so a no-match call can
create an empty directory.

## Details

The catalogue contains seven historical rows without a parser. Their
selector fields are not populated, so they cannot be selected
individually; an unfiltered `download_aofm_xlsx()` call includes them
together with every other catalogue workbook. They cannot be read by
[`read_aofm()`](https://joel23978.github.io/readAOFM/reference/read_aofm.md)
until a parser exists. A changed AOFM URL, non-workbook response, or
changed workbook layout causes an error from the downloader or the
subsequent parser.

## Examples

``` r
# Catalogue discovery is offline and does not create files.
search_aofm("tb issuance")[, c("id", "read_call")]
#>            id                   read_call
#> 1 tb_issuance read_aofm("tb", "issuance")

# Downloading is opt-in in examples because it requires the live AOFM site.
if (interactive()) {
  download_in_temporary_directory <- function() {
    old <- getwd()
    on.exit(setwd(old), add = TRUE)
    setwd(tempdir())
    download_aofm_xlsx("tb", "issuance")
  }
  download_in_temporary_directory()
}
```
