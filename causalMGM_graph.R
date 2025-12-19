library(devtools)
library(igraph)
if (!require(devtools, quietly = TRUE))
  install.packages("devtools")

devtools::install_github("tyler-lovelace1/rCausalMGM")
library(rCausalMGM)

data <- read.csv("/home/rstudio/data/MI.csv")
data <- data[, colSums(is.na(data)) <= 0.1 * nrow(data)]
data <- na.omit(data)
data$X <- NULL

# define the complications variables
complications <- list('FIBR_PREDS',
                      'PREDS_TAH',
                      'JELUD_TAH',
                      'FIBR_JELUD',
                      'A_V_BLOK',
                      'OTEK_LANC',
                      'RAZRIV',
                      'DRESSLER',
                      'ZSN',
                      'REC_IM',
                      'P_IM_STEN',
                      'LET_IS')

nzv <- sapply(data, function(x) length(unique(x)) > 1)
data <- data[, nzv]


## HELPER FUNCTION
parse_edges <- function(edge_strings) {
  edges <- data.frame(from = character(), to = character(), stringsAsFactors = FALSE)
  
  for (edge_str in edge_strings) {
    # Remove leading/trailing whitespace
    edge_str <- trimws(edge_str)
    
    if (grepl(" --> ", edge_str)) {
      # Directed edge: A --> B
      parts <- strsplit(edge_str, " --> ", fixed = TRUE)[[1]]
      edges <- rbind(edges, data.frame(from = parts[1], to = parts[2], stringsAsFactors = FALSE))
    } else if (grepl(" --- ", edge_str)) {
      # Undirected edge: A --- B (treat as bidirectional or as single undirected)
      parts <- strsplit(edge_str, " --- ", fixed = TRUE)[[1]]
      edges <- rbind(edges, data.frame(from = parts[1], to = parts[2], stringsAsFactors = FALSE))
      # For directed graph, you might want to add both directions:
      edges <- rbind(edges, data.frame(from = parts[2], to = parts[1], stringsAsFactors = FALSE))
    }
  }
  
  edges
}

# make undirected sceleton
ig <- mgm(data, lambda=0.05, verbose=T)
print(ig)

# now make a directed graph

ig_directed = pcStable(data, ig, alpha = 0.001)

print(ig_directed)

edge_df_dir <- parse_edges(ig_directed$edges)

vertex_names_dir <- sort(unique(c(edge_df_dir$from, edge_df_dir$to)))

g_pc <- graph_from_data_frame(
  d = edge_df_dir,
  vertices = data.frame(name = vertex_names_dir),
  directed = TRUE
)


# logical vector: TRUE if vertex is a complication
is_complication <- V(g_pc)$name %in% complications
complication_vertices <- V(g_pc)[is_complication]

# vertex fill colors
vertex_fill <- ifelse(
  is_complication,
  "#FF7F50",   # coral
  "#D6ECFA"    # light blue
)

# vertex border (frame) colors
vertex_border <- ifelse(
  is_complication,
  "#B22222",   # dark red
  "#4A90C2"    # blue outline
)


plot(
  g_pc,
  layout = layout_with_dh(g_pc),
  
  # vertices
  vertex.size = 4,
  vertex.color = vertex_fill,
  vertex.frame.color = vertex_border,
  vertex.label.color = "black",
  vertex.label.cex = 0.6,
  vertex.label.font = 2,
  vertex.label.family = "Nimbus Sans",
  vertex.label.dist = 0.5,
  
  # edges
  edge.width = 2,
  edge.arrow.size = 0.25,
  
  # global
  margin = 0
)

# neighbors of all complication nodes
connected_vertices <- unique(
  unlist(
    neighborhood(
      g_pc,
      order = 1,
      nodes = complication_vertices,
      mode = "all"
    )
  )
)

g_comp <- induced_subgraph(
  g_pc,
  vids = connected_vertices
)

is_complication_comp <- V(g_comp)$name %in% complications

vertex_fill_comp <- ifelse(
  is_complication_comp,
  "#FF7F50",   # coral
  "#D6ECFA"
)

vertex_border_comp <- ifelse(
  is_complication_comp,
  "#B22222",   # red border
  "#4A90C2"
)

plot(
  g_comp,
  layout = layout_with_fr(g_comp),
  
  # vertices
  vertex.size = 5,
  vertex.color = vertex_fill_comp,
  vertex.frame.color = vertex_border_comp,
  vertex.label.color = "black",
  vertex.label.cex = 0.65,
  vertex.label.font = 2,
  vertex.label.family = "Nimbus Sans",
  
  # edges
  edge.width = 2,
  edge.arrow.size = 0.3,
  
  margin = 0
)

print(ig_directed$edges)
