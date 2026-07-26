aofm_dispatch_read <- function(
    aofm_table,
    child_fn,
    csv = FALSE,
    timeout = getOption("readAOFM.timeout", 30),
    retries = getOption("readAOFM.retries", 1L),
    max_bytes = getOption("readAOFM.max_bytes", 100 * 1024^2)) {
  switch(
    child_fn,
    read_eofy = read_eofy(aofm_table, csv, timeout, retries, max_bytes),
    read_eom = read_eom(aofm_table, csv, timeout, retries, max_bytes),
    read_transactional =
      read_transactional(aofm_table, csv, timeout, retries, max_bytes),
    read_syndication =
      read_syndication(aofm_table, csv, timeout, retries, max_bytes),
    read_ownership =
      read_ownership(aofm_table, csv, timeout, retries, max_bytes),
    read_secondary =
      read_secondary(aofm_table, csv, timeout, retries, max_bytes),
    read_premium =
      read_premium(aofm_table, csv, timeout, retries, max_bytes),
    stop(sprintf("No reader exists for table id '%s'.", aofm_table), call. = FALSE)
  )
}

#' Reads in and cleans any	data from the AOFM, returns cleaned in a long format
#'
#' @param security object you want data on, options include:
#' tb, tib, tn, slf, summary, aggregate, ownership, retail, termpremium.
#' Optional; when omitted, all supported tables are returned.
#' @param type the specific type of data you want, optional when the inputs
#'   resolve to one or more supported tables:
#' dealt, settlement, issuance, syndication, buyback, turnover,
#' @param csv if `TRUE`, also writes the parsed output to a CSV file via the
#'   underlying reader
#' @param timeout Per-attempt workbook transport timeout in seconds.
#' @param retries Retries after the first workbook transport attempt.
#' @param max_bytes Maximum accepted workbook size.
#' @returns AOFM selected AOFM data as a dataframe, list, or named list of
#'   table results when the inputs match multiple supported tables
#' @examples
#' \dontrun{read_aofm("tb", "issuance")}
#' # downloads Treasury Bond Issunace data from the AOFM
#' # returns cleaned in a long format
#'
#' @export

read_aofm <- function(security = NULL
                      , type = NULL
                      , csv = FALSE
                      , timeout = getOption("readAOFM.timeout", 30)
                      , retries = getOption("readAOFM.retries", 1L)
                      , max_bytes = getOption(
                        "readAOFM.max_bytes",
                        100 * 1024^2
                      )
) {
  for (name in c("security", "type")) {
    value <- get(name)
    if (
      !is.null(value) &&
        (
          !is.character(value) ||
            length(value) != 1L ||
            is.na(value) ||
            !nzchar(trimws(value))
        )
    ) {
      stop(
        sprintf("`%s` must be NULL or one non-empty string.", name),
        call. = FALSE
      )
    }
  }
  if (!is.logical(csv) || length(csv) != 1L || is.na(csv)) {
    stop("`csv` must be one non-missing logical value.", call. = FALSE)
  }
  aofm_validate_transport_bounds(timeout, retries, max_bytes)

  matches <- aofm_index
  matches <- matches[matches$fn != "no function exists", , drop = FALSE]

  if (!is.null(security)) {
    matches <- matches[!is.na(matches$p.security) & matches$p.security == security, , drop = FALSE]
  }

  if (!is.null(type)) {
    matches <- matches[!is.na(matches$p.type) & matches$p.type == type, , drop = FALSE]
  }

  if (nrow(matches) == 0) {
    stop(
      sprintf(
        "No supported AOFM table matched security = %s and type = %s.",
        if (is.null(security)) "NULL" else dQuote(security),
        if (is.null(type)) "NULL" else dQuote(type)
      ),
      call. = FALSE
    )
  }

  if (nrow(matches) == 1) {
    return(aofm_dispatch_read(
      matches$id[[1]],
      matches$fn[[1]],
      csv = csv,
      timeout = timeout,
      retries = retries,
      max_bytes = max_bytes
    ))
  }

  results <- lapply(seq_len(nrow(matches)), function(i) {
    aofm_dispatch_read(
      matches$id[[i]],
      matches$fn[[i]],
      csv = csv,
      timeout = timeout,
      retries = retries,
      max_bytes = max_bytes
    )
  })
  names(results) <- matches$id

  results
}
