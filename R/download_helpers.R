aofm_table_row <- function(aofm_table) {
  row <- aofm_index[aofm_index$id == aofm_table, , drop = FALSE]

  if (nrow(row) != 1L) {
    stop(sprintf("Could not resolve a unique table row for '%s'.", aofm_table), call. = FALSE)
  }

  row
}

download_aofm_workbook <- function(url, destfile) {
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

  handle <- curl::new_handle(
    followlocation = TRUE,
    maxredirs = 10L,
    useragent = "readAOFM/0.1.0",
    connecttimeout = 15,
    timeout = 120,
    low_speed_time = 30,
    low_speed_limit = 1024,
    maxfilesize = 100 * 1024^2
  )

  response <- tryCatch(
    curl::curl_fetch_disk(url, destfile, handle = handle),
    error = function(e) {
      stop(sprintf("Failed to download workbook from '%s': %s", url, conditionMessage(e)), call. = FALSE)
    }
  )

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

  info <- file.info(destfile)
  if (!file.exists(destfile) || is.na(info$size) || info$size <= 0) {
    stop(sprintf("Download from '%s' produced an empty file.", url), call. = FALSE)
  }

  signature <- readBin(destfile, what = "raw", n = 8L)
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
