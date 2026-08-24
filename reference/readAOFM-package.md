# Read AOFM Data Hub workbooks in R

readAOFM provides a small, table-oriented interface to workbooks
published by the Australian Office of Financial Management (AOFM). Users
can discover supported tables with
[`search_aofm()`](https://joel23978.github.io/readAOFM/reference/search_aofm.md),
read one or more tables with
[`read_aofm()`](https://joel23978.github.io/readAOFM/reference/read_aofm.md),
use a family-specific reader when needed, or save raw workbooks with
[`download_aofm_xlsx()`](https://joel23978.github.io/readAOFM/reference/download_aofm_xlsx.md).
Parsed results are returned in long-form data frames or named lists of
data frames, depending on the workbook family.

## Details

A read or raw download contacts the AOFM Data Hub over HTTPS and does
not require credentials. Readers stage workbooks in temporary files and
do not maintain a persistent package cache. Parsed CSV output is an
explicit opt-in side effect under `output/`; raw workbook downloads are
an explicit operation under `data/` in the current working directory.
Upstream URL or workbook-layout changes can therefore require a package
update.

## See also

[`search_aofm()`](https://joel23978.github.io/readAOFM/reference/search_aofm.md),
[`read_aofm()`](https://joel23978.github.io/readAOFM/reference/read_aofm.md),
and
[`download_aofm_xlsx()`](https://joel23978.github.io/readAOFM/reference/download_aofm_xlsx.md)

## Author

**Maintainer**: Joel Findlay <joel.findlay@gmail.com> \[copyright
holder\]
