aofm_validate_workbook <- function(path, min_sheets, context) {
  if (!file.exists(path)) {
    stop(sprintf("%s: workbook does not exist at '%s'", context, path), call. = FALSE)
  }

  info <- file.info(path)
  if (is.na(info$size) || info$size <= 0) {
    stop(sprintf("%s: workbook is empty at '%s'", context, path), call. = FALSE)
  }

  sheets <- readxl::excel_sheets(path)
  if (length(sheets) < min_sheets) {
    stop(
      sprintf(
        "%s: expected at least %d sheet(s) but found %d in '%s'",
        context,
        min_sheets,
        length(sheets),
        path
      ),
      call. = FALSE
    )
  }

  sheets
}

aofm_excel_date <- function(x, origin = "1899-12-30") {
  if (inherits(x, "Date")) {
    return(x)
  }

  if (inherits(x, "POSIXt")) {
    return(as.Date(x))
  }

  if (is.factor(x)) {
    x <- as.character(x)
  }

  if (is.numeric(x)) {
    return(as.Date(x, origin = origin))
  }

  if (is.character(x)) {
    numeric_x <- suppressWarnings(as.numeric(x))
    if (all(is.na(x) | !is.na(numeric_x))) {
      return(as.Date(numeric_x, origin = origin))
    }

    parsed_x <- tryCatch(
      suppressWarnings(as.Date(x)),
      error = function(e) rep(as.Date(NA), length(x))
    )
    if (all(is.na(parsed_x) == is.na(x) | is.na(parsed_x))) {
      return(parsed_x)
    }

    return(x)
  }

  as.Date(x)
}

aofm_require_columns <- function(data, required, context) {
  missing <- setdiff(required, names(data))
  if (length(missing) > 0) {
    stop(
      sprintf(
        "%s: missing required column(s): %s",
        context,
        paste(missing, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  invisible(data)
}

aofm_require_rows <- function(data, min_rows, context) {
  if (nrow(data) < min_rows) {
    stop(
      sprintf(
        "%s: expected at least %d row(s) but found %d",
        context,
        min_rows,
        nrow(data)
      ),
      call. = FALSE
    )
  }

  invisible(data)
}

aofm_write_csv_if_requested <- function(data, csv, path) {
  if (isTRUE(csv)) {
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    readr::write_csv(data, path)
  }

  invisible(data)
}

aofm_transactional_required_columns <- function(aofm_table) {
  if (grepl("retail", aofm_table)) {
    return(c("settle_date", "security_maturity_date"))
  }

  if (grepl("slf", aofm_table)) {
    return(c("start_date", "end_date", "security_maturity_date"))
  }

  if (aofm_table == "tb_issuance") {
    return(c("date_held", "date_settled", "maturity"))
  }

  c("maturity", "date_settled", "date_held")
}

aofm_parse_eofy_workbook <- function(path, csv = FALSE) {
  context <- "read_eofy()"
  aofm_validate_workbook(path, 1, context)

  tmp1 <- readxl::read_excel(path, skip = 1, sheet = 1)
  aofm_require_rows(tmp1, 1, context)
  if (ncol(tmp1) < 4) {
    stop(sprintf("%s: expected at least 4 columns in the first sheet", context), call. = FALSE)
  }

  for (i in c(3:1, 4)) {
    if (i > 1) {
      tmp1[i] <- ifelse(
        is.na(tmp1[[i - 1]]) == FALSE & is.na(tmp1[[i]]) == TRUE,
        tmp1[[i - 1]],
        tmp1[[i]]
      )
    }
    tmp1[i] <- zoo::na.locf(tmp1[i], na.rm = FALSE)
  }

  eofy_executive_summary <- tmp1 %>%
    tidyr::pivot_longer(!c(1:4), names_to = "date") %>%
    dplyr::mutate(date = aofm_excel_date(date, origin = "1899-12-30")) %>%
    dplyr::distinct() %>%
    dplyr::mutate(value = tidyr::replace_na(value, 0)) %>%
    stats::na.omit()

  aofm_require_columns(eofy_executive_summary, c("date", "value"), context)
  aofm_write_csv_if_requested(
    eofy_executive_summary,
    csv,
    file.path("output", "eofy_executive_summary.csv")
  )

  eofy_executive_summary
}

aofm_parse_eom_workbook <- function(path, aofm_table, csv = FALSE) {
  context <- sprintf("read_eom(%s)", aofm_table)
  aofm_validate_workbook(path, 5, context)

  tmp1 <- read_excel_allsheets(path)
  data_indices <- which(!grepl("^notes\\b", names(tmp1), ignore.case = TRUE))
  if (length(data_indices) < 4) {
    stop(
      sprintf(
        "%s: expected at least FaceValue, MarketValue, Delta and Duration sheets",
        context
      ),
      call. = FALSE
    )
  }

  return.data <- list()
  for (i in data_indices) {
    output.name <- paste0(aofm_table, "_", names(tmp1)[i])
    output.name.csv <- paste0(output.name, ".csv")

    m <- if (grepl("tb|tib", aofm_table)) 5 else 4
    tmp_sheet <- tmp1[[i]]
    aofm_require_rows(tmp_sheet, m + 1, context)

    tmp2 <- tmp_sheet[1:m, ] %>%
      data.table::transpose()

    for (j in c(3:1, (4:m))) {
      if (j > 1) {
        tmp2[j] <- ifelse(
          is.na(tmp2[[j - 1]]) == FALSE & is.na(tmp2[[j]]) == TRUE,
          tmp2[[j - 1]],
          tmp2[[j]]
        )
      }
      tmp2[j] <- zoo::na.locf(tmp2[j], na.rm = FALSE)
    }

    tmp3 <- tmp2 %>%
      data.table::transpose()

    tmp4 <- tmp3 %>%
      rbind(tmp_sheet[(m + 1):(nrow(tmp_sheet)), ] %>%
              `colnames<-`(names(tmp3))) %>%
      dplyr::mutate(V1 = ifelse(is.na(V1), V2, V1)) %>%
      data.table::transpose() %>%
      `colnames<-`(.[1,]) %>%
      janitor::row_to_names(row_number = 1)

    eom_id_cols <- names(tmp4)[seq_len(m)]
    if (grepl("tb|tib|tn", aofm_table)) {
      security_key <- do.call(
        paste,
        c(lapply(tmp4[eom_id_cols], as.character), sep = "\r")
      )
      tmp4$Series <- ave(
        seq_along(security_key),
        security_key,
        FUN = seq_along
      )
      eom_id_cols <- c(eom_id_cols, "Series")
    }
    tmp4 <- tmp4 %>%
      tidyr::pivot_longer(cols = -dplyr::all_of(eom_id_cols), names_to = "date")

    aofm_require_columns(tmp4, c("date", "value"), context)

    if (grepl("tb|tib|tn", aofm_table)) {
      aofm_require_columns(tmp4, "Maturity", context)
      maturity_raw <- trimws(as.character(tmp4$Maturity))
      maturity_number <- suppressWarnings(as.numeric(maturity_raw))
      maturity_date <- as.Date(maturity_number, origin = "1899-12-30")
      tmp4$Maturity <- ifelse(
        !is.na(maturity_date),
        format(maturity_date, "%Y-%m-%d"),
        maturity_raw
      )
      tmp4$Maturity[is.na(tmp4$Maturity) | !nzchar(tmp4$Maturity)] <- "total"
    }

    tmp4 <- tmp4 %>%
      dplyr::mutate(date = aofm_excel_date(date, origin = "1899-12-30")) %>%
      dplyr::distinct() %>%
      dplyr::mutate(value = as.numeric(value)) %>%
      stats::na.omit()

    return.data[[length(return.data) + 1]] <- tmp4
    names(return.data)[length(return.data)] <- output.name

    aofm_write_csv_if_requested(tmp4, csv, file.path("output", output.name.csv))
  }

  return.data
}

aofm_parse_transactional_workbook <- function(path, aofm_table, csv = FALSE) {
  context <- sprintf("read_transactional(%s)", aofm_table)
  aofm_validate_workbook(path, 1, context)

  tmp1 <- readxl::read_excel(
    path,
    skip = 3,
    sheet = 1,
    col_names = as.character(readxl::read_excel(path)[1, ]),
    guess_max = 10000
  ) %>%
    janitor::clean_names()

  required <- aofm_transactional_required_columns(aofm_table)
  aofm_require_columns(tmp1, required, context)

  if (grepl("retail", aofm_table)) {
    tmp2 <- tmp1 %>%
      dplyr::mutate(
        settle_date = aofm_excel_date(settle_date, origin = "1899-12-30"),
        security_maturity_date = aofm_excel_date(security_maturity_date, origin = "1899-12-30")
      )
  } else if (grepl("slf", aofm_table)) {
    tmp2 <- tmp1 %>%
      dplyr::mutate(
        start_date = aofm_excel_date(start_date, origin = "1899-12-30"),
        end_date = aofm_excel_date(end_date, origin = "1899-12-30"),
        security_maturity_date = aofm_excel_date(security_maturity_date, origin = "1899-12-30")
      )
  } else if (aofm_table == "tb_issuance") {
    tmp2 <- tmp1 %>%
      dplyr::mutate(
        date_held = aofm_excel_date(date_held, origin = "1899-12-30"),
        date_settled = aofm_excel_date(date_settled, origin = "1899-12-30"),
        maturity = aofm_excel_date(maturity, origin = "1899-12-30")
      ) %>%
      dplyr::mutate_if(is.logical, as.numeric)
  } else {
    tmp2 <- tmp1 %>%
      dplyr::mutate(
        maturity = aofm_excel_date(maturity, origin = "1899-12-31"),
        date_settled = aofm_excel_date(date_settled, origin = "1899-12-31"),
        date_held = aofm_excel_date(date_held, origin = "1899-12-31")
      )
  }

  tmp3 <- tmp2 %>%
    dplyr::select(where(not_all_na)) %>%
    tidyr::pivot_longer(!colnames((.) %>% dplyr::select_if(~!is.numeric(.)))) %>%
    dplyr::filter(!is.na(value))

  aofm_require_columns(tmp3, c("name", "value"), context)
  aofm_write_csv_if_requested(tmp3, csv, file.path("output", paste0(aofm_table, ".csv")))

  tmp3
}

aofm_parse_syndication_workbook <- function(path, aofm_table, csv = FALSE) {
  context <- sprintf("read_syndication(%s)", aofm_table)
  sheet_names <- aofm_validate_workbook(path, 2, context)
  data_sheets <- sheet_names[!grepl("^notes\\b", sheet_names, ignore.case = TRUE)]

  if (length(data_sheets) < 2) {
    stop(
      sprintf("%s: expected at least 2 syndication data sheets after excluding notes", context),
      call. = FALSE
    )
  }

  sheet_list <- lapply(data_sheets, function(sheet_name) {
    tmp1 <- readxl::read_excel(path, sheet = sheet_name, col_names = FALSE)
    aofm_require_rows(tmp1, 1, context)
    tmp1 <- tmp1[rowSums(!is.na(tmp1)) > 0, colSums(!is.na(tmp1)) > 0, drop = FALSE]

    field_names <- janitor::make_clean_names(as.character(tmp1[[1]]))
    tmp2 <- as.data.frame(data.table::transpose(tmp1[, -1, drop = FALSE]), stringsAsFactors = FALSE)
    colnames(tmp2) <- field_names

    tmp2 <- tmp2 %>%
      janitor::clean_names() %>%
      dplyr::mutate(
        pricing_date = aofm_excel_date(pricing_date, origin = "1899-12-30"),
        settlement_date = aofm_excel_date(settlement_date, origin = "1899-12-30"),
        type = ifelse(grepl("tap", sheet_name, ignore.case = TRUE), "tap", "new_bond")
      )

    aofm_require_columns(tmp2, c("pricing_date", "settlement_date"), context)
    id_cols <- intersect(
      c(
        "bond_line",
        "pricing_date",
        "settlement_date",
        "pricing_reference",
        "initial_price_guidance_bp",
        "final_spread_bp",
        "curve_extension",
        "joint_lead_managers",
        "type"
      ),
      names(tmp2)
    )
    measure_cols <- setdiff(names(tmp2), id_cols)

    if (length(measure_cols) == 0) {
      stop(sprintf("%s: no syndication value columns found in sheet '%s'", context, sheet_name), call. = FALSE)
    }

    tmp2 %>%
      tidyr::pivot_longer(dplyr::all_of(measure_cols)) %>%
      dplyr::filter(!is.na(value))
  })

  tmp5 <- do.call(rbind, sheet_list)
  aofm_write_csv_if_requested(tmp5, csv, file.path("output", paste0(aofm_table, ".csv")))

  tmp5
}

aofm_parse_secondary_workbook <- function(path, aofm_table, csv = FALSE) {
  context <- sprintf("read_secondary(%s)", aofm_table)
  aofm_validate_workbook(path, 3, context)

  sheet_list <- lapply(2:3, function(i) {
    tmp1 <- readxl::read_excel(
      path,
      sheet = i,
      skip = ifelse(grepl("tib", aofm_table), 3, 1)
    ) %>%
      `colnames<-`(c("period", colnames(.)[2:ncol(.)])) %>%
      dplyr::mutate(
        period = aofm_excel_date(period, origin = "1899-12-30"),
        group = ifelse(i == 2, "tenor", "investor_type")
      )

    aofm_require_columns(tmp1, "period", context)

    tmp1 %>%
      tidyr::pivot_longer(!colnames(tmp1 %>% dplyr::select_if(~!is.numeric(.))))
  })

  tmp5 <- do.call(rbind, sheet_list)
  aofm_write_csv_if_requested(tmp5, csv, file.path("output", paste0(aofm_table, ".csv")))

  tmp5
}

aofm_parse_premium_workbook <- function(path, aofm_table, csv = FALSE) {
  context <- sprintf("read_premium(%s)", aofm_table)
  sheet_names <- aofm_validate_workbook(path, 3, context)
  data_sheets <- sheet_names[!grepl("^notes\\b", sheet_names, ignore.case = TRUE)]
  if (length(data_sheets) < 2) {
    stop(sprintf("%s: expected at least two term-premium data sheets", context), call. = FALSE)
  }

  sheet_list <- lapply(data_sheets, function(sheet_name) {
    tmp1 <- readxl::read_excel(path, sheet = sheet_name, skip = 1) %>%
      janitor::clean_names() %>%
      dplyr::mutate(
        date = aofm_excel_date(date, origin = "1899-12-30"),
        type = sheet_name
      )

    aofm_require_columns(tmp1, "date", context)

    tmp1 %>%
      tidyr::pivot_longer(!colnames(tmp1 %>% dplyr::select_if(~!is.numeric(.)))) %>%
      dplyr::filter(!is.na(value))
  })

  tmp5 <- do.call(rbind, sheet_list) %>%
    dplyr::arrange(date)

  aofm_write_csv_if_requested(tmp5, csv, file.path("output", paste0(aofm_table, ".csv")))

  tmp5
}

aofm_parse_ownership_workbook <- function(path, aofm_table, csv = FALSE) {
  context <- sprintf("read_ownership(%s)", aofm_table)
  min_sheets <- if (grepl("public", aofm_table)) 2 else 4
  sheets <- aofm_validate_workbook(path, min_sheets, context)

  tmp1 <- read_excel_allsheets(path)
  if (length(tmp1) < min_sheets) {
    stop(sprintf("%s: expected at least %d sheets but found %d", context, min_sheets, length(tmp1)), call. = FALSE)
  }

  if (grepl("public", aofm_table)) {
    sheet_indices <- 1:2
    top_rows <- 1:4
    n <- max(top_rows) + 1
  } else {
    sheet_indices <- 2:4
    top_rows <- 1:4
    n <- max(top_rows) + 1
  }
  m <- max(top_rows)

  return.data <- list()
  for (i in sheet_indices) {
    output.name <- paste0(aofm_table, "_", names(tmp1)[i])
    output.name.csv <- paste0(output.name, ".csv")

    tmp_sheet <- tmp1[[i]]
    aofm_require_rows(tmp_sheet, m + 1, context)

    tmp2 <- tmp_sheet[top_rows, ] %>%
      data.table::transpose()

    for (j in c(3:1, (4:m))) {
      if (j > 1) {
        tmp2[j] <- ifelse(
          is.na(tmp2[[j - 1]]) == FALSE & is.na(tmp2[[j]]) == TRUE,
          tmp2[[j - 1]],
          tmp2[[j]]
        )
      }
      tmp2[j] <- zoo::na.locf(tmp2[j], na.rm = FALSE)
    }

    tmp3 <- tmp2 %>%
      data.table::transpose()

    tmp4 <- tmp3 %>%
      rbind(tmp_sheet[(m + 1):(nrow(tmp_sheet)), ] %>%
              `colnames<-`(names(tmp3))) %>%
      dplyr::mutate(V1 = ifelse(is.na(V1), V2, V1)) %>%
      data.table::transpose() %>%
      `colnames<-`(.[1,]) %>%
      janitor::row_to_names(row_number = 1)

    names(tmp4) <- make.unique(names(tmp4), sep = "_")

    ownership_id_cols <- names(tmp4)[seq_len(n - 1)]
    tmp4 <- tmp4 %>%
      tidyr::pivot_longer(cols = -dplyr::all_of(ownership_id_cols), names_to = "date")

    aofm_require_columns(tmp4, c("date", "value"), context)

    if (grepl("public", aofm_table)) {
      tmp5 <- tmp4 %>%
        dplyr::mutate(date = aofm_excel_date(date, origin = "1899-12-30")) %>%
        dplyr::distinct() %>%
        dplyr::mutate(value = as.numeric(value)) %>%
        stats::na.omit()
    } else {
      tmp5 <- tmp4 %>%
        dplyr::mutate(date = aofm_excel_date(date, origin = "1899-12-30")) %>%
        dplyr::distinct() %>%
        dplyr::mutate(value = as.numeric(value)) %>%
        stats::na.omit()
    }

    return.data[[length(return.data) + 1]] <- tmp5
    names(return.data)[length(return.data)] <- output.name

    aofm_write_csv_if_requested(tmp5, csv, file.path("output", output.name.csv))
  }

  return.data
}
