## GitHub API
gh_issues <- function(org, what = c("repository_url", "html_url", "number", "title", "state")) {
  user_issues <- gh::gh(
    endpoint = "/search/issues",
    q = sprintf("user:%s", org),
    .limit = Inf
  )
  items <- user_issues$items
  if (length(items) < 1) return(NULL)
  if (length(items) == 1 && items == "") return(NULL)

  items <- lapply(items, `[`, what)
  items <- lapply(items, as.data.frame)
  items <- do.call(rbind, items)

  items$repository_url <- sub("api.", "", items$repository_url)
  items$repository_url <- sub("repos/", "", items$repository_url)
  items$repository <- sub("https://github.com/", "", items$repository_url)
  items <- items[items$state == "open", , drop = FALSE]
  items
}

# Get =====
## Get GitHub data
github <- try(gh_issues("tesselle"))

if (!inherits(github, "try-error")) {
  ## Write CSV
  if (!dir.exists("data")) dir.create("data")
  write.csv(github, file = file.path("data", "issues.csv"), row.names = FALSE)
}
