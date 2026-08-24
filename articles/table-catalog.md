# Table catalog and data sources

`readAOFM` ships a local catalogue that maps AOFM Data Hub workbooks to
search terms, public selectors, and parser families. This article
records the 23 parser-supported entries and the seven historical entries
available through raw workbook download. Reader calls retrieve current
source workbooks; the catalogue itself supplies stable discovery
metadata.

## The 23 parsed tables

The public selectors are exact. The table below is deliberately explicit
so that the table ID, family, and corresponding
[`read_aofm()`](https://joel23978.github.io/readAOFM/reference/read_aofm.md)
call are visible in one place. The validation column is computed with
[`search_aofm()`](https://joel23978.github.io/readAOFM/reference/search_aofm.md),
which is an offline catalogue operation.

``` r

parsed_catalog <- data.frame(
  family = c(
    "End-of-financial-year summary",
    rep("End-of-month positions", 8),
    rep("Transactional issuance and buybacks", 7),
    rep("Syndication details", 2),
    rep("Ownership", 2),
    rep("Secondary-market turnover", 2),
    "Term premium"
  ),
  security = c(
    "summary",
    "aggregate", "aggregate", "tb", "tb", "tib", "tib", "tn", "tn",
    "tb", "tb", "tib", "tib", "tn", "retail", "slf",
    "tb", "tib",
    "ownership", "ownership",
    "tb", "tib",
    "termpremium"
  ),
  type = c(
    NA_character_,
    "dealt", "settlement", "dealt", "settlement", "dealt", "settlement",
    "dealt", "settlement",
    "issuance", "buyback", "issuance", "buyback", "issuance", NA, NA,
    "syndication", "syndication",
    "public", "nonresident",
    "turnover", "turnover",
       NA_character_
  ),
  id = c(
    "summary",
    "aggregate_position_dealt", "aggregate_position_settlement",
    "tb_position_dealt", "tb_position_settlement",
    "tib_position_dealt", "tib_position_settlement",
    "tn_position_dealt", "tn_position_settlement",
    "tb_issuance", "tb_buyback", "tib_issuance", "tib_buyback",
    "tn_issuance", "retail", "slf",
    "tb_syndication", "tib_syndication",
    "ownership_public", "ownership_nonresident",
    "tb_turnover", "tib_turnover",
    "termpremium"
  ),
  stringsAsFactors = FALSE
)

parsed_catalog$selector <- ifelse(
  is.na(parsed_catalog$type),
  sprintf('read_aofm("%s")', parsed_catalog$security),
  sprintf(
    'read_aofm("%s", "%s")',
    parsed_catalog$security,
    parsed_catalog$type
  )
)

parsed_catalog$catalog_match <- vapply(seq_len(nrow(parsed_catalog)), function(i) {
  query <- if (is.na(parsed_catalog$type[[i]])) {
    parsed_catalog$security[[i]]
  } else {
    paste(parsed_catalog$security[[i]], parsed_catalog$type[[i]])
  }
  parsed_catalog$id[[i]] %in% readAOFM::search_aofm(query)$id
}, logical(1))

stopifnot(nrow(parsed_catalog) == 23L, all(parsed_catalog$catalog_match))
parsed_catalog[, c("family", "id", "selector")]
#>                                 family                            id
#> 1        End-of-financial-year summary                       summary
#> 2               End-of-month positions      aggregate_position_dealt
#> 3               End-of-month positions aggregate_position_settlement
#> 4               End-of-month positions             tb_position_dealt
#> 5               End-of-month positions        tb_position_settlement
#> 6               End-of-month positions            tib_position_dealt
#> 7               End-of-month positions       tib_position_settlement
#> 8               End-of-month positions             tn_position_dealt
#> 9               End-of-month positions        tn_position_settlement
#> 10 Transactional issuance and buybacks                   tb_issuance
#> 11 Transactional issuance and buybacks                    tb_buyback
#> 12 Transactional issuance and buybacks                  tib_issuance
#> 13 Transactional issuance and buybacks                   tib_buyback
#> 14 Transactional issuance and buybacks                   tn_issuance
#> 15 Transactional issuance and buybacks                        retail
#> 16 Transactional issuance and buybacks                           slf
#> 17                 Syndication details                tb_syndication
#> 18                 Syndication details               tib_syndication
#> 19                           Ownership              ownership_public
#> 20                           Ownership         ownership_nonresident
#> 21           Secondary-market turnover                   tb_turnover
#> 22           Secondary-market turnover                  tib_turnover
#> 23                        Term premium                   termpremium
#>                                 selector
#> 1                   read_aofm("summary")
#> 2        read_aofm("aggregate", "dealt")
#> 3   read_aofm("aggregate", "settlement")
#> 4               read_aofm("tb", "dealt")
#> 5          read_aofm("tb", "settlement")
#> 6              read_aofm("tib", "dealt")
#> 7         read_aofm("tib", "settlement")
#> 8               read_aofm("tn", "dealt")
#> 9          read_aofm("tn", "settlement")
#> 10           read_aofm("tb", "issuance")
#> 11            read_aofm("tb", "buyback")
#> 12          read_aofm("tib", "issuance")
#> 13           read_aofm("tib", "buyback")
#> 14           read_aofm("tn", "issuance")
#> 15                   read_aofm("retail")
#> 16                      read_aofm("slf")
#> 17        read_aofm("tb", "syndication")
#> 18       read_aofm("tib", "syndication")
#> 19      read_aofm("ownership", "public")
#> 20 read_aofm("ownership", "nonresident")
#> 21           read_aofm("tb", "turnover")
#> 22          read_aofm("tib", "turnover")
#> 23              read_aofm("termpremium")
```

The same catalogue can be searched with user-facing terms. These calls
are local and return metadata only because `read = FALSE` is the
default:

``` r

readAOFM::search_aofm("treasury bond")[, c("id", "read_call")]
#>                       id                      read_call
#> 1             tb_buyback     read_aofm("tb", "buyback")
#> 2      tb_position_dealt       read_aofm("tb", "dealt")
#> 3            tb_issuance    read_aofm("tb", "issuance")
#> 4 tb_position_settlement  read_aofm("tb", "settlement")
#> 5         tb_syndication read_aofm("tb", "syndication")
#> 6            tb_turnover    read_aofm("tb", "turnover")
readAOFM::search_aofm("inflation")[, c("id", "read_call")]
#>                        id                       read_call
#> 1             tib_buyback     read_aofm("tib", "buyback")
#> 2      tib_position_dealt       read_aofm("tib", "dealt")
#> 3            tib_issuance    read_aofm("tib", "issuance")
#> 4 tib_position_settlement  read_aofm("tib", "settlement")
#> 5         tib_syndication read_aofm("tib", "syndication")
#> 6            tib_turnover    read_aofm("tib", "turnover")
readAOFM::search_aofm("secondary market")[, c("id", "read_call")]
#>             id                    read_call
#> 1  tb_turnover  read_aofm("tb", "turnover")
#> 2 tib_turnover read_aofm("tib", "turnover")
```

## Raw-workbook catalogue entries

The following seven catalogue IDs are available through the raw workbook
download path while the parser-supported workflow focuses on the 23
entries above:

``` r

raw_only_ids <- c(
  "tb_issuance_conversion",
  "indexation_factors",
  "rmbs_transactions",
  "rmbs_auctions",
  "interest_rate_swaps",
  "cross_currency_swaps",
  "portfolio_overview"
)

raw_only <- data.frame(
  id = raw_only_ids,
  parser = NA_character_,
  access = "download_aofm_xlsx()",
  stringsAsFactors = FALSE
)

# This check reads package metadata only. It does not download a workbook;
# users should use search_aofm() rather than this internal object.
catalog_metadata <- get("aofm_index", envir = asNamespace("readAOFM"))
stopifnot(
  setequal(raw_only_ids, catalog_metadata$id[catalog_metadata$fn == "no function exists"]),
  all(catalog_metadata$fn[catalog_metadata$id %in% raw_only_ids] == "no function exists")
)
raw_only
#>                       id parser               access
#> 1 tb_issuance_conversion   <NA> download_aofm_xlsx()
#> 2     indexation_factors   <NA> download_aofm_xlsx()
#> 3      rmbs_transactions   <NA> download_aofm_xlsx()
#> 4          rmbs_auctions   <NA> download_aofm_xlsx()
#> 5    interest_rate_swaps   <NA> download_aofm_xlsx()
#> 6   cross_currency_swaps   <NA> download_aofm_xlsx()
#> 7     portfolio_overview   <NA> download_aofm_xlsx()
```

[`download_aofm_xlsx()`](https://joel23978.github.io/readAOFM/reference/download_aofm_xlsx.md)
is the public raw-workbook helper. Its selectors are the `security` and
`type` values shown for parsed rows. The raw-only rows have no parser
and no public
[`read_aofm()`](https://joel23978.github.io/readAOFM/reference/read_aofm.md)
call; because their selector fields are not populated, an unfiltered raw
download is the available public route and will also select other
catalogue workbooks. The function writes files beneath `data/` in the
working directory and performs a live HTTPS request, so this article
does not run it automatically.

## Source access and scope

The public AOFM Data Hub provides these workbooks without a package
username, password, or API key.
[`search_aofm()`](https://joel23978.github.io/readAOFM/reference/search_aofm.md)
performs local discovery. Reader and raw download calls request the
workbook URL recorded in the package catalogue over HTTPS, stage it in a
temporary file (readers), or save it beneath `data/` (raw downloads).
Each call retrieves the current source rather than using a persistent
package cache.

AOFM controls workbook URLs, publication cadence, sheet names, columns,
and historical coverage. For example, turnover is published quarterly
with a reporting lag, while several raw-only datasets are historical. A
changed URL, HTML error page, unavailable source, or changed workbook
layout can therefore make a read fail. Use the table ID, selector, error
text, and access date when reporting a source change. The packaged
workbook snapshots used by tests and vignettes are deterministic
examples, not a replacement for the live AOFM source or a runtime cache.
