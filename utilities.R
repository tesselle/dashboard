## Helpers =====
fail <- function(x) {
  is.null(x) || inherits(x, "try-error")
}

# CRAN API =====
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

# R-universe API =====
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

# GitHub API =====
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
gh_issues <- function(org) {
  user_issues <- gh::gh(
    endpoint = "/search/issues",
    q = sprintf("user:%s", org),
    .limit = Inf
  )
  items <- user_issues$items
  if (length(items) < 1) return(NULL)
  if (length(items) == 1 && items == "") return(NULL)

  items
}
gh_clean_url <- function(x) {
  x <- sub("api.", "", x)
  x <- sub("repos/", "", x)
  x
}
gh_style_label <- function(x) {
  href <- gh_clean_url(x$url)
  style <- "background: #%s; padding: 0 7px; color: #000000; text-decoration: none; border-radius: 2em;"
  style <- sprintf(style, x$color)
  sprintf("<a href='%s' style='%s'>%s</a>", href, style, x$name)
}
gh_style_labels <- function(x) {
  if (length(x) == 0) return(NA)
  paste0(vapply(X = x, FUN = gh_style_label, FUN.VALUE = character(1)))
}
gh_summary <- function(x, what = c("number", "title",
                                   "created_at", "updated_at", "comments",
                                   "labels", "state",
                                   "repository_url", "html_url")) {
  x <- lapply(x, `[`, what)
  x <- lapply(
    X = x,
    FUN = function(x) {
      x$labels <- gh_style_labels(x$labels)
      x
    }
  )
  x <- lapply(x, as.data.frame)
  x <- do.call(rbind, x)

  x$repository_url <- gh_clean_url(x$repository_url)
  x$repository <- sub("https://github.com/", "", x$repository_url)
  x$created_at <- as.Date(x$created_at)
  x$updated_at <- as.Date(x$updated_at)
  x$title <- sprintf("<a href='%s'>%s</a> #%d", x$html_url, x$title, x$number)

  x <- x[x$state == "open", , drop = FALSE]
  x
}

# OpenAlex =====
oa_summary <- function(x, what = c("title", "so", "url", "publication_year")) {
  lapply(
    X = x,
    FUN = function(x) {
      x <- lapply(x, `[`, what)
      x <- lapply(x, as.data.frame)
      x <- do.call(rbind, x)
      x$title <- sprintf("<a href='%s'>%s</a>", x$url, x$title)
      x
    }
  )
}
