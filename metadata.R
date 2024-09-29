# Helpers =====
## CRAN API
cran_info <- function(packages, what = c("Package", "Version", "Title",
                                         "Description", "Publication")) {
  db <- tools::CRAN_package_db()
  items <- db[db$Package %in% packages, , drop = FALSE]
  items$Publication <- as.Date(items$`Date/Publication`)
  items <- items[, what, drop = FALSE]

  check <- tools::CRAN_check_results()
  status <- check[check$Package %in% packages, , drop = FALSE]
  status <- split(status, f = status$Package)
  items$Status <- vapply(
    X = status,
    FUN = function(x) as.character(max(x$Status)),
    FUN.VALUE = character(1)
  )

  items
}

## R-universe API
ru_info <- function(org, what = c("Package", "Version", "Title", "Description")) {
  api <- httr2::request(sprintf("https://%s.r-universe.dev", org))
  req <- httr2::req_url_path(api, "/api/packages/")
  resp <- httr2::req_perform(req)
  json <- httr2::resp_body_json(resp)

  items <- lapply(X = json, `[`, what)
  items <- lapply(items, as.data.frame)
  items <- do.call(rbind, items)
  items
}

## GitHub API
gh_info <- function(org, what = c("stargazers_count", "forks", "open_issues")) {
  user_repo <- gh::gh(
    endpoint = "/search/repositories",
    q = sprintf("user:%s topic:r-package", org),
    .limit = Inf
  )
  items <- user_repo$items
  if (length(items) < 1) return(NULL)
  if (length(items) == 1 && items == "") return(NULL)

  items <- lapply(items, `[`, c("name", what))
  items <- lapply(items, as.data.frame)
  items <- do.call(rbind, items)
  items
}

# Get =====
## Get R-universe data
universe <- try(ru_info("tesselle"))
## Get GitHub data
github <- try(gh_info("tesselle"))
## Get CRAN data
cran <- try(cran_info(universe$Package))

ok <- !inherits(universe, "try-error") &&
  !inherits(github, "try-error") &&
  !inherits(cran, "try-error")

if (ok) {
  ## Merge metadata
  pkg <- merge(cran, universe, by.x = "Package", by.y = "Package",
               all.x = TRUE, all.y = TRUE, sort = FALSE,
               suffixes = c("", "_universe"))
  pkg <- merge(pkg, github, by.x = "Package", by.y = "name",
               all.x = FALSE, all.y = FALSE, sort = FALSE,
               suffixes = c("", "_github"))

  pkg <- pkg[order(pkg$Package, decreasing = FALSE), ]
  colnames(pkg) <- tolower(colnames(pkg))

  ## Write CSV
  if (!dir.exists("data")) dir.create("data")
  write.csv(pkg, file = file.path("data", "metadata.csv"), row.names = FALSE)
}
