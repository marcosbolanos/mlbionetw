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

g <- export(
  miic_obj,
  method = "igraph",
  pcor_palette = NULL,
  display = "compact",
  show_self_loops = TRUE
)

g <- delete.vertices(g, degree(g) == 0)

plot(g)

# plot 'SEX' vs 'DLIT_AG' variable
plot(data$SEX, data$DLIT_AG)

boxplot(DLIT_AG ~ SEX, data = data)

library(ggplot2)
library(gghalves)

# install if needed:
# install.packages("gghalves")

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

