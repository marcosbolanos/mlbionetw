library(igraph)
library(miic)

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

miic_obj <- miic(
  input_data = data, latent = "yes",
  n_shuffles = 50, conf_threshold = 0.001
)


g <- export(
  miic_obj,
  method = "igraph",
  pcor_palette = NULL,
  #display = "compact",
  show_self_loops = TRUE
)

g <- delete.vertices(g, degree(g) == 0)
# logical vector: TRUE if vertex is a complication
is_complication <- V(g)$name %in% complications
complication_vertices <- V(g)[is_complication]

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
  g,
  layout = layout_with_dh(g),
  
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
      g,
      order = 1,
      nodes = complication_vertices,
      mode = "all"
    )
  )
)

g_comp <- induced_subgraph(
  g,
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


# plot 'SEX' vs 'DLIT_AG' variable
plot(data$SEX, data$DLIT_AG)

boxplot(DLIT_AG ~ SEX, data = data)

library(ggplot2)
library(gghalves)


data$SEX <- factor(data$SEX, levels = c(0, 1), labels = c("Male", "Female"))

ggplot(data, aes(x = SEX, y = DLIT_AG, fill = SEX, color = SEX)) +
  
  geom_jitter(width = 0.12, alpha = 0.4, size = 1.4) +
  geom_half_boxplot(side = "r", width = 0.15, alpha = 0.8, outlier.shape = NA) +
  scale_fill_manual(values = c(
    "Male"   = "#6BB6FF",  # light blue
    "Female" = "#FF79A8"   # soft pink
  )) +
  scale_color_manual(values = c(
    "Male"   = "#2C79D3",
    "Female" = "#CC3E83"
  )) +
  
  labs(
    title = "Distribution of 'absence of hypertension in years'",
    x = "Sex",
    y = "DLIT_AG"
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 16)
  )

