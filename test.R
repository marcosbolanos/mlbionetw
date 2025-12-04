library(igraph)
library(miic)

data <- read.csv("/home/rstudio/data/MI.csv")

data <- data[, colSums(is.na(data)) <= 0.2 * nrow(data)]

data <- na.omit(data)

miic_obj <- miic(
  input_data = data, latent = "yes",
  n_shuffles = 10, conf_threshold = 0.001
)

# plot graph with igraph
if(require(igraph)) {
  plot(miic_obj, method="igraph")
}
