library(devtools)
library(igraph)
if (!require(devtools, quietly = TRUE))
  install.packages("devtools")

devtools::install_github("tyler-lovelace1/rCausalMGM")

library(rCausalMGM)

data <- read.csv("/home/rstudio/data/MI.csv")

data <- data[, colSums(is.na(data)) <= 0.2 * nrow(data)]

data <- na.omit(data)

nzv <- sapply(data, function(x) length(unique(x)) > 1)
data <- data[, nzv]


ig <- mgm(data, lambda=0.05, verbose=T)


print(ig)


library(igraph)

# Extract edge strings
edge_strings <- ig$edges

# Split "A --- B" into two columns
edge_list <- do.call(
  rbind,
  strsplit(edge_strings, " --- ", fixed = TRUE)
)

edge_df <- as.data.frame(edge_list, stringsAsFactors = FALSE)
colnames(edge_df) <- c("from", "to")

g_igraph <- graph_from_data_frame(
  d = edge_df,
  vertices = ig$nodes,
  directed = TRUE
)

g <- delete_vertices(g_igraph, degree(g) == 0)

plot(
  g,
  layout = layout_with_fr(g),
  
  # vertices
  vertex.size = 4,
  vertex.color = "#D6ECFA",        # light blue fill
  vertex.frame.color = "#4A90C2",  # slightly darker blue outline
  vertex.label.color = "black",
  vertex.label.cex = 0.6,
  vertex.label.font = 2,      # bold
  vertex.label.family = "Nimbus Sans",
  vertex.label.dist = 0.5,
  
  # edges
  edge.width = 2,
  edge.arrow.size = 0.25,
  
  # global
  margin = 0
)

# now make a directed graph
# first code from binary to factor

# Identify numeric columns with < 10 unique values
low_cardinality <- sapply(data, function(x) {
  is.numeric(x) && length(unique(na.omit(x))) < 10
})

names(data)[low_cardinality]

# convert
data[low_cardinality] <- lapply(data[low_cardinality], factor)

# unbalanced columns
invalid_vars <- sapply(data, function(x) {
  if (is.numeric(x)) {
    length(unique(na.omit(x))) <= 1
  } else if (is.factor(x)) {
    any(table(x) < 5)
  } else {
    FALSE
  }
})

names(data)[invalid_vars]
data <- data[, !invalid_vars, drop = FALSE]

ig_directed = pcStable(data, ig)

print(ig_directed)

# Extract edge strings
edge_strings <- ig_directed$edges

# Split "A --- B" into two columns
edge_list <- do.call(
  rbind,
  strsplit(edge_strings, " --- ", fixed = TRUE)
)

edge_df <- as.data.frame(edge_list, stringsAsFactors = FALSE)
colnames(edge_df) <- c("from", "to")

g_igraph <- graph_from_data_frame(
  d = edge_df,
  directed = TRUE
)


g <- delete_vertices(g_igraph, degree(g) == 0)

plot(
  g,
  layout = layout_with_fr(g),
  
  # vertices
  vertex.size = 4,
  vertex.color = "#D6ECFA",        # light blue fill
  vertex.frame.color = "#4A90C2",  # slightly darker blue outline
  vertex.label.color = "black",
  vertex.label.cex = 0.6,
  vertex.label.font = 2,      # bold
  vertex.label.family = "Nimbus Sans",
  vertex.label.dist = 0.5,
  
  # edges
  edge.width = 2,
  edge.arrow.size = 0.25,
  
  # global
  margin = 0
)
