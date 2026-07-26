aofm_catalog_overrides <- function() {
  configured <- getOption("readAOFM.url_overrides", NULL)
  defaults <- c(
    ownership_public = paste0(
      "https://www.aofm.gov.au/sites/default/files/2025-05-02/",
      "register_of_government_borrowing.xlsx"
    ),
    termpremium = paste0(
      "https://www.aofm.gov.au/sites/default/files/2025-06-06/",
      "term%20premium.xlsx"
    )
  )

  if (is.null(configured)) {
    return(defaults)
  }
  if (!is.character(configured) || is.null(names(configured))) {
    stop(
      "`options(readAOFM.url_overrides = ...)` must be a named character vector.",
      call. = FALSE
    )
  }

  defaults[names(configured)] <- configured
  defaults
}

aofm_apply_catalog_overrides <- function(catalog) {
  overrides <- aofm_catalog_overrides()
  matched <- match(catalog$id, names(overrides))
  replace <- !is.na(matched)
  catalog$file.path[replace] <- unname(overrides[matched[replace]])
  catalog
}

aofm_table_row <- function(aofm_table) {
  row <- aofm_index[aofm_index$id == aofm_table, , drop = FALSE]

  if (nrow(row) != 1L) {
    stop(sprintf("Could not resolve a unique table row for '%s'.", aofm_table), call. = FALSE)
  }

  aofm_apply_catalog_overrides(row)
}

download_aofm_workbook <- function(
    url,
    destfile,
    timeout = getOption("readAOFM.timeout", 60),
    retries = getOption("readAOFM.retries", 2L)) {
  if (!is.character(url) || length(url) != 1L || is.na(url) || !nzchar(url)) {
    stop("`url` must be a single non-empty string.", call. = FALSE)
  }

  if (!is.character(destfile) || length(destfile) != 1L || is.na(destfile) || !nzchar(destfile)) {
    stop("`destfile` must be a single non-empty string.", call. = FALSE)
  }

  ext <- tolower(tools::file_ext(destfile))
  if (!ext %in% c("xls", "xlsx")) {
    stop("`destfile` must end in .xls or .xlsx.", call. = FALSE)
  }

  dir.create(dirname(destfile), recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(dirname(destfile))) {
    stop(sprintf("Could not create download directory '%s'.", dirname(destfile)), call. = FALSE)
  }

  timeout <- suppressWarnings(as.numeric(timeout))
  retries <- suppressWarnings(as.integer(retries))
  if (length(timeout) != 1L || is.na(timeout) || timeout <= 0) {
    stop("`timeout` must be one positive number of seconds.", call. = FALSE)
  }
  if (length(retries) != 1L || is.na(retries) || retries < 0L) {
    stop("`retries` must be one non-negative integer.", call. = FALSE)
  }

  handle <- curl::new_handle(
    followlocation = TRUE,
    maxredirs = 10L,
    connecttimeout = min(timeout, 15),
    timeout = timeout,
    useragent = paste0("readAOFM/", utils::packageVersion("readAOFM"))
  )

  temporary <- tempfile(
    pattern = paste0(".", basename(destfile), "-"),
    tmpdir = dirname(destfile),
    fileext = ".part"
  )
  on.exit(unlink(temporary, force = TRUE), add = TRUE)

  response <- NULL
  last_error <- NULL
  for (attempt in seq_len(retries + 1L)) {
    unlink(temporary, force = TRUE)
    response <- tryCatch(
      curl::curl_fetch_disk(url, temporary, handle = handle),
      error = function(e) {
        last_error <<- conditionMessage(e)
        NULL
      }
    )

    retryable_server_error <- is.list(response) &&
      !is.null(response$status_code) &&
      response$status_code >= 500L
    if (!is.null(response) && !retryable_server_error) {
      break
    }
  }

  if (is.null(response)) {
    stop(
      sprintf(
        "Failed to download workbook from '%s' after %d attempt(s): %s",
        url,
        retries + 1L,
        if (is.null(last_error)) "unknown download error" else last_error
      ),
      call. = FALSE
    )
  }

  if (is.list(response) && !is.null(response$status_code) && response$status_code >= 400L) {
    stop(
      sprintf(
        "Failed to download workbook from '%s' (HTTP %s).",
        url,
        response$status_code
      ),
      call. = FALSE
    )
  }

  content_type <- tolower(if (is.list(response) && !is.null(response$type)) response$type else "")
  if (nzchar(content_type) && grepl("^(text/|application/xhtml\\+xml|application/xml|text/html)", content_type)) {
    stop(
      sprintf(
        "Download from '%s' returned '%s' instead of a workbook.",
        url,
        response$type
      ),
      call. = FALSE
    )
  }

  info <- file.info(temporary)
  if (!file.exists(temporary) || is.na(info$size) || info$size <= 0) {
    stop(sprintf("Download from '%s' produced an empty file.", url), call. = FALSE)
  }

  signature <- readBin(temporary, what = "raw", n = 8L)
  if (ext == "xlsx") {
    if (length(signature) < 2L || !identical(signature[1:2], charToRaw("PK"))) {
      stop(
        sprintf("Download from '%s' did not look like a valid .xlsx workbook.", url),
        call. = FALSE
      )
    }
  } else if (ext == "xls") {
    expected <- as.raw(c(0xd0, 0xcf, 0x11, 0xe0, 0xa1, 0xb1, 0x1a, 0xe1))
    if (length(signature) < length(expected) || !identical(signature[seq_along(expected)], expected)) {
      stop(
        sprintf("Download from '%s' did not look like a valid .xls workbook.", url),
        call. = FALSE
      )
    }
  }

  installed <- file.rename(temporary, destfile)
  if (!installed) {
    installed <- file.copy(temporary, destfile, overwrite = TRUE)
  }
  if (!installed || !file.exists(destfile)) {
    stop(sprintf("Could not atomically install downloaded workbook at '%s'.", destfile), call. = FALSE)
  }

  invisible(destfile)
}

download_aofm_table_workbook <- function(aofm_table) {
  row <- aofm_table_row(aofm_table)
  file_name <- row$file.save[[1]]
  ext <- tools::file_ext(file_name)

  if (!nzchar(ext)) {
    stop(sprintf("Could not determine a workbook extension for '%s'.", aofm_table), call. = FALSE)
  }

  tmp <- tempfile(fileext = paste0(".", ext))
  download_aofm_workbook(row$file.path[[1]], tmp)
}
