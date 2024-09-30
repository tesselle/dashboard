## Helpers
source("utilities.R")

## Be polite
options(openalexR.mailto = "packages@tesselle.org")

## Get works
doi <- read.csv(file.path("data", "doi.csv"))
doi$doi <- sprintf("https://doi.org/%s", doi$doi)

works <- try(openalexR::oa_fetch(
  entity = "works",
  doi = doi$doi,
  verbose = TRUE
))

## Get citations
if (!fail(works)) {
  works <- merge(x = doi, y = works, by = "doi", all.x = FALSE, all.y = TRUE)
  works <- split(works, f = works$package)

  cites <- lapply(
    X = works,
    FUN = function(x) {
      oa <- try(openalexR::oa_fetch(
        entity = "works",
        cites = x$id,
        verbose = TRUE
      ))
      if (fail(oa)) return(NULL)

      self <- oa$doi %in% doi$doi
      oa <- oa[!self, , drop = FALSE]
      if (nrow(oa) == 0) return(NULL)
      oa
    }
  )
}

## Write yaml
cites <- Filter(f = Negate(fail), x = cites)
cat(
  yaml::as.yaml(cites, column.major = FALSE),
  file = file.path("data", "citations.yml"),
  sep = "\n",
  append = FALSE
)
