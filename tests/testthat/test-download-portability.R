test_that("workbook downloads use bounded transport settings", {
  captured <- new.env(parent = emptyenv())
  fixture <- tempfile(fileext = ".xlsx")

  testthat::local_mocked_bindings(
    new_handle = function(...) {
      captured$handle <- list(...)
      captured$handle
    },
    curl_fetch_disk = function(url, path, handle) {
      captured$url <- url
      captured$path <- path
      captured$handle_received <- handle
      writeBin(as.raw(c(0x50, 0x4b, 0x03, 0x04)), path)
      list(
        status_code = 200L,
        type = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
      )
    },
    .package = "curl"
  )

  result <- readAOFM:::download_aofm_workbook(
    "https://www.aofm.gov.au/media/591",
    fixture
  )

  expect_identical(result, fixture)
  expect_identical(captured$url, "https://www.aofm.gov.au/media/591")
  expect_identical(captured$path, fixture)
  expect_identical(captured$handle_received, captured$handle)
  expect_true(captured$handle$followlocation)
  expect_equal(captured$handle$maxredirs, 10L)
  expect_identical(captured$handle$useragent, "readAOFM/0.1.0")
  expect_equal(captured$handle$connecttimeout, 15)
  expect_equal(captured$handle$timeout, 120)
  expect_equal(captured$handle$low_speed_time, 30)
  expect_equal(captured$handle$low_speed_limit, 1024)
  expect_equal(captured$handle$maxfilesize, 100 * 1024^2)
})

test_that("workbook downloads retain HTTP and content-type errors", {
  fixture <- tempfile(fileext = ".xlsx")

  testthat::local_mocked_bindings(
    curl_fetch_disk = function(url, path, handle) {
      writeBin(charToRaw("not-found"), path)
      list(status_code = 404L, type = "text/html")
    },
    .package = "curl"
  )

  expect_error(
    readAOFM:::download_aofm_workbook("https://www.aofm.gov.au/media/404", fixture),
    "HTTP 404"
  )

  testthat::local_mocked_bindings(
    curl_fetch_disk = function(url, path, handle) {
      writeBin(charToRaw("error page"), path)
      list(status_code = 200L, type = "text/html")
    },
    .package = "curl"
  )

  expect_error(
    readAOFM:::download_aofm_workbook("https://www.aofm.gov.au/media/error", fixture),
    "instead of a workbook"
  )
})

test_that("workbook downloads retain transport and signature errors", {
  fixture <- tempfile(fileext = ".xlsx")

  testthat::local_mocked_bindings(
    curl_fetch_disk = function(url, path, handle) {
      stop("simulated connection failure")
    },
    .package = "curl"
  )

  expect_error(
    readAOFM:::download_aofm_workbook("https://www.aofm.gov.au/media/failure", fixture),
    "Failed to download workbook.*simulated connection failure"
  )

  testthat::local_mocked_bindings(
    curl_fetch_disk = function(url, path, handle) {
      writeBin(charToRaw("plain text"), path)
      list(status_code = 200L, type = "application/octet-stream")
    },
    .package = "curl"
  )

  expect_error(
    readAOFM:::download_aofm_workbook("https://www.aofm.gov.au/media/plain", fixture),
    "did not look like a valid .xlsx workbook"
  )
})
