## Helpers
source("utilities.R")

## Get GitHub data
issues <- try(gh_issues("tesselle"))

## Write yaml
if (!fail(issues)) {
  cat(
    yaml::as.yaml(issues, column.major = FALSE),
    file = file.path("data", "issues.yml"),
    sep = "\n",
    append = FALSE
  )
}
