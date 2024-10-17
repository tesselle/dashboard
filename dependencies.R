## Helpers
source("utilities.R")

dep <- miniCRAN::makeDepGraph(
  pkg = tesselle(meta = FALSE),
  repos = c("https://cloud.r-project.org", "https://tesselle.r-universe.dev/"),
  suggests = FALSE,
  enhances = FALSE,
  includeBasePkgs = FALSE
)

mat <- dep |>
  igraph::as_data_frame() |>
  subset(select = c("from", "to")) |>
  relations::endorelation(graph = _) |>
  relations::transitive_reduction() |>
  relations::relation_incidence()

## Write CSV
if (!dir.exists("data")) dir.create("data")
write.csv(mat, file = file.path("data", "dependencies.csv"), row.names = TRUE)
