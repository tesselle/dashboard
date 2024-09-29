## Get RStudio CRAN mirror data
## Set CRAN package list
tesselle <- c("aion", "arkhe", "alkahest", "dimensio", "folio",
              "isopleuros", "kairos", "khroma", "nexus", "tabula", "tesselle")

## Get RStudio CRAN mirror data
if (!dir.exists("data")) dir.create("data")
data_daily <- file.path("data", "downloads_daily.csv")
data_monthly <- file.path("data", "downloads_monthly.csv")

if (!file.exists(data_daily)) {
  downloads_daily <- NULL
  from <- as.Date("2018-10-18")
} else {
  downloads_daily <- read.csv(data_daily)
  from <- as.Date(max(downloads_daily$date)) + 1
}

if (Sys.Date() - from >= 0) {
  last <- try(adjustedcranlogs::adj_cran_downloads(
    packages = tesselle,
    from = as.character(from),
    to = "last-day"
  ))

  if (!inherits(last, "try-error")) {
    ## Write data
    downloads_daily <- rbind(downloads_daily, last) |>
      dplyr::filter( # One day before the date of the first release
        (package == "khroma" & date >= "2018-10-18") |
          (package == "tabula" & date >= "2018-12-02") |
          (package == "arkhe" & date >= "2019-12-17") |
          (package == "folio" & date >= "2021-02-11") |
          (package == "dimensio" & date >= "2021-04-21") |
          (package == "kairos" & date >= "2021-11-07") |
          (package == "tesselle" & date >= "2022-04-27") |
          (package == "alkahest" & date >= "2022-09-14") |
          (package == "isopleuros" & date >= "2023-05-15") |
          (package == "aion" & date >= "2023-06-12") |
          (package == "nexus" & date >= "2023-11-28")
      )

    ## Downloads per month
    downloads_monthly <- downloads_daily |>
      dplyr::mutate(
        month = lubridate::floor_date(date, "month")
      ) |>
      dplyr::group_by(package, month) |>
      dplyr::summarize(
        count = sum(count),
        adjusted_downloads = sum(adjusted_downloads),
        .groups = "drop_last"
      ) |>
      dplyr::ungroup() |>
      dplyr::group_by(package) |>
      dplyr::mutate(
        total_count = cumsum(count),
        total_downloads = cumsum(adjusted_downloads)
      ) |>
      dplyr::ungroup()

    write.csv(downloads_daily, file = data_daily)
    write.csv(downloads_monthly, file = data_monthly)
  }
}
