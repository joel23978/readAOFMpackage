#' List AOFM data tables known to the package
#'
#' Returns the package's table catalogue without downloading any workbooks.
#' Stable table identifiers can be passed to [download_aofm_file()] and
#' [read_aofm_file()].
#'
#' @param include_unsupported Include catalogue rows that do not yet have a
#'   parser.
#' @returns A data frame describing AOFM tables, source files, and parser
#'   support.
#' @export
aofm_catalog <- function(include_unsupported = FALSE) {
  catalog <- aofm_apply_catalog_overrides(aofm_index)
  supported <- catalog$fn != "no function exists"

  if (!isTRUE(include_unsupported)) {
    catalog <- catalog[supported, , drop = FALSE]
    supported <- supported[supported]
  }

  data.frame(
    security = as.character(catalog$p.security),
    type = as.character(catalog$p.type),
    table_id = as.character(catalog$id),
    reader = as.character(catalog$fn),
    category = as.character(catalog$category),
    title = as.character(catalog$title),
    description = as.character(catalog$despription),
    source_url = as.character(catalog$file.path),
    file_name = as.character(catalog$file.save),
    supported = unname(supported),
    stringsAsFactors = FALSE
  )
}

aofm_supported_table_row <- function(table_id) {
  if (!is.character(table_id) ||
      length(table_id) != 1L ||
      is.na(table_id) ||
      !nzchar(trimws(table_id))) {
    stop("`table_id` must be a single non-empty string.", call. = FALSE)
  }

  row <- aofm_table_row(trimws(table_id))
  if (identical(row$fn[[1]], "no function exists")) {
    stop(
      sprintf("AOFM table '%s' does not yet have a parser.", table_id),
      call. = FALSE
    )
  }

  row
}

#' Download one AOFM workbook by stable table ID
#'
#' @param table_id A stable `table_id` returned by [aofm_catalog()].
#' @param path Directory in which to save the workbook.
#' @param overwrite Replace an existing file when `TRUE`.
#' @returns The normalized path to the downloaded workbook. The path has
#'   `table_id` and `source_url` attributes.
#' @export
download_aofm_file <- function(
    table_id,
    path = tempdir(),
    overwrite = TRUE) {
  row <- aofm_supported_table_row(table_id)

  if (!is.character(path) ||
      length(path) != 1L ||
      is.na(path) ||
      !nzchar(trimws(path))) {
    stop("`path` must be a single non-empty directory.", call. = FALSE)
  }

  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(path)) {
    stop(sprintf("Could not create download directory '%s'.", path), call. = FALSE)
  }

  destination <- file.path(path, row$file.save[[1]])
  if (!file.exists(destination) || isTRUE(overwrite)) {
    download_aofm_workbook(row$file.path[[1]], destination)
  }

  if (!file.exists(destination)) {
    stop(
      sprintf("AOFM download did not create '%s'.", destination),
      call. = FALSE
    )
  }

  result <- normalizePath(destination, winslash = "/", mustWork = TRUE)
  attr(result, "table_id") <- row$id[[1]]
  attr(result, "source_url") <- row$file.path[[1]]
  result
}

aofm_dispatch_parse <- function(file_path, table_id, reader, csv = FALSE) {
  switch(
    reader,
    read_eofy = aofm_parse_eofy_workbook(file_path, csv = csv),
    read_eom = aofm_parse_eom_workbook(
      file_path,
      aofm_table = table_id,
      csv = csv
    ),
    read_transactional = aofm_parse_transactional_workbook(
      file_path,
      aofm_table = table_id,
      csv = csv
    ),
    read_syndication = aofm_parse_syndication_workbook(
      file_path,
      aofm_table = table_id,
      csv = csv
    ),
    read_ownership = aofm_parse_ownership_workbook(
      file_path,
      aofm_table = table_id,
      csv = csv
    ),
    read_secondary = aofm_parse_secondary_workbook(
      file_path,
      aofm_table = table_id,
      csv = csv
    ),
    read_premium = aofm_parse_premium_workbook(
      file_path,
      aofm_table = table_id,
      csv = csv
    ),
    stop(
      sprintf("No local-file parser exists for AOFM table '%s'.", table_id),
      call. = FALSE
    )
  )
}

#' Parse a previously downloaded AOFM workbook
#'
#' This function separates downloading from parsing so callers can retain and
#' hash an immutable raw workbook before normalising its observations.
#'
#' @param file_path Path to an AOFM `.xls` or `.xlsx` workbook.
#' @param table_id A stable `table_id` returned by [aofm_catalog()].
#' @param csv If `TRUE`, also writes the parser's legacy CSV output.
#' @returns The same data-frame or named-list result as [read_aofm()].
#' @export
read_aofm_file <- function(file_path, table_id, csv = FALSE) {
  row <- aofm_supported_table_row(table_id)

  if (!is.character(file_path) ||
      length(file_path) != 1L ||
      is.na(file_path) ||
      !nzchar(trimws(file_path))) {
    stop("`file_path` must be a single non-empty string.", call. = FALSE)
  }
  if (!file.exists(file_path)) {
    stop(sprintf("AOFM workbook does not exist at '%s'.", file_path), call. = FALSE)
  }

  aofm_dispatch_parse(
    file_path = file_path,
    table_id = row$id[[1]],
    reader = row$fn[[1]],
    csv = csv
  )
}
