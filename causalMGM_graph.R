library(devtools)
library(igraph)
if (!require(devtools, quietly = TRUE))
  install.packages("devtools")

devtools::install_github("tyler-lovelace1/rCausalMGM")
library(rCausalMGM)

data <- read.csv("/home/rstudio/data/MI.csv")
data <- data[, colSums(is.na(data)) <= 0.2 * nrow(data)]
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
  edges <- do.call(
    rbind,
    strsplit(edge_strings, "\\s+-+>|\\s+<-+\\s+|\\s+--+\\s+|\\s+o-o\\s+|\\s+o->\\s+")
  )
  colnames(edges) <- c("from", "to")
  as.data.frame(edges, stringsAsFactors = FALSE)
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

