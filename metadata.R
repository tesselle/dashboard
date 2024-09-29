## Helpers
source("utilities.R")

## Get R-universe data
universe <- try(ru_info("tesselle"))

## Get GitHub data
github <- try(gh_info("tesselle"))

## Get CRAN data
cran <- try(cran_info(universe$Package))

if (!fail(universe) && !fail(github) && !fail(cran)) {
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
