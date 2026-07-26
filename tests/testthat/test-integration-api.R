test_that("aofm_catalog exposes stable supported and unsupported rows", {
  supported <- aofm_catalog()
  complete <- aofm_catalog(include_unsupported = TRUE)

  expect_equal(nrow(supported), 23L)
  expect_equal(nrow(complete), 30L)
  expect_true(all(supported$supported))
  expect_equal(sum(!complete$supported), 7L)
  expect_true(all(c(
    "security", "type", "table_id", "reader", "category", "title",
    "description", "source_url", "file_name", "supported"
  ) %in% names(complete)))
  expect_setequal(
    complete$table_id[!complete$supported],
    c(
      "tb_issuance_conversion",
      "indexation_factors",
      "rmbs_transactions",
      "rmbs_auctions",
      "interest_rate_swaps",
      "cross_currency_swaps",
      "portfolio_overview"
    )
  )
})

test_that("download_aofm_file targets one stable table and preserves metadata", {
  destination_dir <- tempfile("readAOFM-download-")
  source_fixture <- fixture_path("tb_issuance.xlsx")
  seen <- list()

  testthat::local_mocked_bindings(
    download_aofm_workbook = function(url, destfile) {
      seen[[length(seen) + 1L]] <<- list(url = url, destfile = destfile)
      dir.create(dirname(destfile), recursive = TRUE, showWarnings = FALSE)
      file.copy(source_fixture, destfile, overwrite = TRUE)
      invisible(destfile)
    },
    .package = "readAOFM"
  )

  path <- download_aofm_file("tb_issuance", destination_dir)

  expect_true(file.exists(path))
  expect_match(path, "tb_issuance\\.xlsx$")
  expect_identical(attr(path, "table_id"), "tb_issuance")
  expect_match(attr(path, "source_url"), "aofm\\.gov\\.au")
  expect_equal(length(seen), 1L)

  download_aofm_file("tb_issuance", destination_dir, overwrite = FALSE)
  expect_equal(length(seen), 1L)
})

test_that("current production URL overrides are exposed consistently", {
  catalog <- aofm_catalog()
  term <- catalog[catalog$table_id == "termpremium", , drop = FALSE]
  public <- catalog[catalog$table_id == "ownership_public", , drop = FALSE]

  expect_match(term$source_url, "2025-06-06/term%20premium\\.xlsx$")
  expect_match(public$source_url, "2025-05-02/register_of_government_borrowing\\.xlsx$")
})

test_that("downloads are retried and installed without partial files", {
  destination_dir <- tempfile("readAOFM-retry-")
  destination <- file.path(destination_dir, "retry.xlsx")
  fixture <- fixture_path("tb_issuance.xlsx")
  attempts <- 0L

  testthat::local_mocked_bindings(
    curl_fetch_disk = function(url, destfile, handle) {
      attempts <<- attempts + 1L
      if (attempts == 1L) {
        stop("temporary failure")
      }
      file.copy(fixture, destfile, overwrite = TRUE)
      list(status_code = 200L, type = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet")
    },
    .package = "curl"
  )

  readAOFM:::download_aofm_workbook(
    "https://example.invalid/retry.xlsx",
    destination,
    retries = 1L
  )

  expect_equal(attempts, 2L)
  expect_true(file.exists(destination))
  expect_false(any(grepl("\\.part$", list.files(destination_dir))))
})

test_that("current AOFM production workbook shapes parse completely", {
  live_files <- c(
    eom = Sys.getenv("READAOFM_TEST_EOM_FILE"),
    ownership = Sys.getenv("READAOFM_TEST_OWNERSHIP_FILE"),
    premium = Sys.getenv("READAOFM_TEST_PREMIUM_FILE")
  )
  skip_if(any(!nzchar(live_files)), "Set READAOFM_TEST_*_FILE for production-shape tests")

  eom <- read_aofm_file(live_files[["eom"]], "tb_position_dealt")
  ownership <- read_aofm_file(live_files[["ownership"]], "ownership_public")
  premium <- read_aofm_file(live_files[["premium"]], "termpremium")

  expect_setequal(
    sub("^tb_position_dealt_", "", names(eom)),
    c("FaceValue", "MarketValue", "Delta", "Duration", "Tenor")
  )
  eom_keys <- lapply(eom, function(item) item[setdiff(names(item), "value")])
  expect_true(all(vapply(eom_keys, anyDuplicated, integer(1)) == 0L))
  expect_false(any(vapply(eom, function(item) all(item$Maturity == "total"), logical(1))))
  expect_true(all(vapply(ownership, nrow, integer(1)) > 0L))
  expect_setequal(unique(premium$type), c("TermPremiumOLS", "TermPremiumBC"))
  expect_true(all(!is.na(premium$date)))
})

test_that("read_aofm_file parses representative local fixtures", {
  issuance <- read_aofm_file(
    fixture_path("tb_issuance.xlsx"),
    "tb_issuance"
  )
  position <- read_aofm_file(
    fixture_path("tb_position_dealt.xlsx"),
    "tb_position_dealt"
  )
  syndication <- read_aofm_file(
    fixture_path("tb_syndication.xlsx"),
    "tb_syndication"
  )

  expect_true(is.data.frame(issuance))
  expect_true(all(c("date_held", "name", "value") %in% names(issuance)))
  expect_type(position, "list")
  expect_true(all(vapply(position, is.data.frame, logical(1))))
  expect_true(is.data.frame(syndication))
  expect_true(all(c("pricing_date", "name", "value") %in% names(syndication)))
})

test_that("local-file dispatch covers all 23 supported catalogue routes", {
  catalog <- aofm_catalog()
  called <- character()
  parser_stub <- function(...) {
    arguments <- list(...)
    table_id <- arguments$aofm_table
    if (is.null(table_id) || !nzchar(table_id)) {
      table_id <- "summary"
    }
    called <<- c(called, table_id)
    data.frame(date = as.Date("2026-01-01"), value = 1)
  }

  testthat::local_mocked_bindings(
    aofm_parse_eofy_workbook = parser_stub,
    aofm_parse_eom_workbook = parser_stub,
    aofm_parse_transactional_workbook = parser_stub,
    aofm_parse_syndication_workbook = parser_stub,
    aofm_parse_ownership_workbook = parser_stub,
    aofm_parse_secondary_workbook = parser_stub,
    aofm_parse_premium_workbook = parser_stub,
    .package = "readAOFM"
  )

  fixture <- fixture_path("tb_issuance.xlsx")
  results <- lapply(catalog$table_id, function(table_id) {
    read_aofm_file(fixture, table_id)
  })

  expect_equal(length(results), 23L)
  expect_true(all(vapply(results, is.data.frame, logical(1))))
  expect_equal(length(called), 23L)
})

test_that("unsupported and invalid local-file requests fail clearly", {
  fixture <- fixture_path("tb_issuance.xlsx")

  expect_error(
    read_aofm_file(fixture, "portfolio_overview"),
    "does not yet have a parser"
  )
  expect_error(
    download_aofm_file("not_a_table"),
    "Could not resolve a unique table row"
  )
  expect_error(
    read_aofm_file("missing.xlsx", "tb_issuance"),
    "does not exist"
  )
})
