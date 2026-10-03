# Read and tidy the sheets of the international arrivals workbook. Both sheets
# share a layout: rows 2 to 20 are regional summaries; countries begin at
# row 21.

# Monthly arrival counts
read_arrivals <- function(path, sheet = "Monthly") {
  read_monthly_sheet(path, sheet, values_to = "arrivals")
}

# Monthly year-over-year change in arrivals, as a proportion
read_arrivals_yoy <- function(path, sheet = "Monthly Y-o-Y % Change") {
  read_monthly_sheet(path, sheet, values_to = "yoy_change")
}

read_monthly_sheet <- function(path, sheet, values_to, first_country_row = 21) {
  readxl::read_excel(
    path,
    sheet = sheet,
    col_types = "text",
    na = c("", "-", "- - -"),
    .name_repair = "unique_quiet"
  ) |>
    dplyr::select(country = 2, region = 3, tidyselect::matches("^\\d{4}")) |>
    dplyr::slice(-seq_len(first_country_row - 2)) |>
    dplyr::filter(!is.na(country)) |>
    tidyr::pivot_longer(
      !c(country, region),
      names_to = "month",
      values_to = values_to
    ) |>
    dplyr::mutate(
      country = stringr::str_squish(country),
      preliminary = stringr::str_detect(month, "Preliminary"),
      date = parse_month_header(month),
      dplyr::across(tidyselect::all_of(values_to), as.numeric)
    ) |>
    dplyr::select(
      country,
      region,
      date,
      tidyselect::all_of(values_to),
      preliminary
    )
}

# Month headers are a mix of Excel date serials ("36527") and text labels
# ("2019-09", "2022-1", "2026-07\r\nPreliminary"). The serials do not always
# fall on the first of the month, so every date is floored to its month.
parse_month_header <- function(x) {
  is_serial <- stringr::str_detect(x, "^\\d+$")
  serial <- as.Date(as.numeric(x[is_serial]), origin = "1899-12-30")
  label <- lubridate::ym(stringr::str_extract(
    x[!is_serial],
    "^\\d{4}-\\d{1,2}"
  ))

  out <- as.Date(rep(NA, length(x)))
  out[is_serial] <- lubridate::floor_date(serial, "month")
  out[!is_serial] <- label
  out
}
