library(ggplot2)
library(gghalves)
library(dplyr)
library(tidyr)
library(patchwork)

# Read data
data <- read.csv("/home/rstudio/data/MI.csv")
data <- data[, colSums(is.na(data)) <= 0.1 * nrow(data)]
data <- na.omit(data)
data$X <- NULL

# Define the complications to plot
complications_to_plot <- c('REC_IM', 'NITR_S', 'FIBR_JELUD', 'RAZRIV')

# Convert binary variables to factors with meaningful labels
data_plot <- data %>%
  mutate(
    REC_IM = factor(REC_IM, levels = c(0, 1), labels = c("No", "Yes")),
    NITR_S = factor(NITR_S, levels = c(0, 1), labels = c("No", "Yes")),
    FIBR_JELUD = factor(FIBR_JELUD, levels = c(0, 1), labels = c("No", "Yes")),
    RAZRIV = factor(RAZRIV, levels = c(0, 1), labels = c("No", "Yes"))
  )

# Define color palette (coral/red scheme matching your graph)
color_no <- "#6BB6FF"   # light blue for "No"
color_yes <- "#FF7F50"  # coral for "Yes"
border_no <- "#2C79D3"  # darker blue
border_yes <- "#B22222" # dark red

# Create individual plots for each complication
plot_list <- list()

for (comp in complications_to_plot) {
  p <- ggplot(data_plot, aes(x = .data[[comp]], y = LET_IS, 
                             fill = .data[[comp]], color = .data[[comp]])) +
    geom_jitter(width = 0.12, alpha = 0.4, size = 1.4) +
    geom_half_boxplot(side = "r", width = 0.15, alpha = 0.8, outlier.shape = NA) +
    scale_fill_manual(values = c("No" = color_no, "Yes" = color_yes)) +
    scale_color_manual(values = c("No" = border_no, "Yes" = border_yes)) +
    labs(
      title = comp,
      x = "",
      y = "LET_IS"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "none",
      panel.grid.minor = element_blank(),
      plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
      axis.text.x = element_text(size = 11)
    )
  
  plot_list[[comp]] <- p
}

# Combine all plots into a 2x2 grid
combined_plot <- (plot_list[[1]] + plot_list[[2]]) / 
  (plot_list[[3]] + plot_list[[4]]) +
  plot_annotation(
    title = 'Distribution of LET_IS by Complication Status',
    theme = theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5))
  )

# Display the combined plot
print(combined_plot)

# Optional: Create a single plot with all complications in facets
data_long <- data_plot %>%
  select(LET_IS, all_of(complications_to_plot)) %>%
  pivot_longer(
    cols = all_of(complications_to_plot),
    names_to = "Complication",
    values_to = "Status"
  )

facet_plot <- ggplot(data_long, aes(x = Status, y = LET_IS, fill = Status, color = Status)) +
  geom_jitter(width = 0.12, alpha = 0.4, size = 1.2) +
  geom_half_boxplot(side = "r", width = 0.15, alpha = 0.8, outlier.shape = NA) +
  scale_fill_manual(values = c("No" = color_no, "Yes" = color_yes)) +
  scale_color_manual(values = c("No" = border_no, "Yes" = border_yes)) +
  facet_wrap(~ Complication, nrow = 2, scales = "free_x") +
  labs(
    title = 'Distribution of LET_IS by Complication Status',
    x = "Complication Present",
    y = "LET_IS"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    strip.text = element_text(face = "bold", size = 12),
    strip.background = element_rect(fill = "#F0F0F0", color = NA)
  )

# Display the faceted plot
print(facet_plot)

# Separate plot: REC_IM → OTEK_LANC
data_rec_otek <- data %>%
  mutate(
    REC_IM = factor(REC_IM, levels = c(0, 1), labels = c("No REC_IM", "Yes REC_IM")),
    OTEK_LANC = factor(OTEK_LANC, levels = c(0, 1), labels = c("No", "Yes"))
  )

rec_otek_plot <- ggplot(data_rec_otek, aes(x = REC_IM, fill = OTEK_LANC)) +
  geom_bar(position = "fill", alpha = 0.85, color = "white", linewidth = 0.5) +
  scale_fill_manual(
    values = c("No" = color_no, "Yes" = color_yes),
    name = "OTEK_LANC"
  ) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "REC_IM → OTEK_LANC Relationship",
    subtitle = "Proportion of OTEK_LANC by REC_IM status",
    x = "REC_IM (Recurrent MI)",
    y = "Proportion"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "right",
    legend.title = element_text(face = "bold", size = 12),
    legend.text = element_text(size = 11),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.title = element_text(face = "bold", size = 15, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray40"),
    axis.text = element_text(size = 11)
  )

print(rec_otek_plot)

library(ggplot2)
library(gghalves)
library(dplyr)
library(tidyr)
library(patchwork)

# Read data
data <- read.csv("/home/rstudio/data/MI.csv")
data <- data[, colSums(is.na(data)) <= 0.1 * nrow(data)]
data <- na.omit(data)
data$X <- NULL

# Define the complications to plot
complications_to_plot <- c('REC_IM', 'NITR_S', 'FIBR_JELUD', 'RAZ_RIV')

# Convert binary variables to factors with meaningful labels
data_plot <- data %>%
  mutate(
    REC_IM = factor(REC_IM, levels = c(0, 1), labels = c("No", "Yes")),
    NITR_S = factor(NITR_S, levels = c(0, 1), labels = c("No", "Yes")),
    FIBR_JELUD = factor(FIBR_JELUD, levels = c(0, 1), labels = c("No", "Yes")),
    RAZ_RIV = factor(RAZ_RIV, levels = c(0, 1), labels = c("No", "Yes"))
  )

# Define color palette (coral/red scheme matching your graph)
color_no <- "#6BB6FF"   # light blue for "No"
color_yes <- "#FF7F50"  # coral for "Yes"
border_no <- "#2C79D3"  # darker blue
border_yes <- "#B22222" # dark red

# Create individual plots for each complication
plot_list <- list()

for (comp in complications_to_plot) {
  p <- ggplot(data_plot, aes(x = .data[[comp]], y = LET_IS, 
                             fill = .data[[comp]], color = .data[[comp]])) +
    geom_jitter(width = 0.12, alpha = 0.4, size = 1.4) +
    geom_half_boxplot(side = "r", width = 0.15, alpha = 0.8, outlier.shape = NA) +
    scale_fill_manual(values = c("No" = color_no, "Yes" = color_yes)) +
    scale_color_manual(values = c("No" = border_no, "Yes" = border_yes)) +
    labs(
      title = comp,
      x = "",
      y = "LET_IS"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "none",
      panel.grid.minor = element_blank(),
      plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
      axis.text.x = element_text(size = 11)
    )
  
  plot_list[[comp]] <- p
}

# Combine all plots into a 2x2 grid
combined_plot <- (plot_list[[1]] + plot_list[[2]]) / 
  (plot_list[[3]] + plot_list[[4]]) +
  plot_annotation(
    title = 'Distribution of LET_IS by Complication Status',
    theme = theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5))
  )

# Display the combined plot
print(combined_plot)

# Optional: Create a single plot with all complications in facets
data_long <- data_plot %>%
  select(LET_IS, all_of(complications_to_plot)) %>%
  pivot_longer(
    cols = all_of(complications_to_plot),
    names_to = "Complication",
    values_to = "Status"
  )

facet_plot <- ggplot(data_long, aes(x = Status, y = LET_IS, fill = Status, color = Status)) +
  geom_jitter(width = 0.12, alpha = 0.4, size = 1.2) +
  geom_half_boxplot(side = "r", width = 0.15, alpha = 0.8, outlier.shape = NA) +
  scale_fill_manual(values = c("No" = color_no, "Yes" = color_yes)) +
  scale_color_manual(values = c("No" = border_no, "Yes" = border_yes)) +
  facet_wrap(~ Complication, nrow = 2, scales = "free_x") +
  labs(
    title = 'Distribution of LET_IS by Complication Status',
    x = "Complication Present",
    y = "LET_IS"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    strip.text = element_text(face = "bold", size = 12),
    strip.background = element_rect(fill = "#F0F0F0", color = NA)
  )

# Display the faceted plot
print(facet_plot)

# Separate plot: REC_IM → OTEK_LANC
data_rec_otek <- data %>%
  mutate(
    REC_IM = factor(REC_IM, levels = c(0, 1), labels = c("No REC_IM", "Yes REC_IM")),
    OTEK_LANC = factor(OTEK_LANC, levels = c(0, 1), labels = c("No", "Yes"))
  )

rec_otek_plot <- ggplot(data_rec_otek, aes(x = REC_IM, fill = OTEK_LANC)) +
  geom_bar(position = "fill", alpha = 0.85, color = "white", linewidth = 0.5) +
  scale_fill_manual(
    values = c("No" = color_no, "Yes" = color_yes),
    name = "OTEK_LANC"
  ) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "REC_IM → OTEK_LANC Relationship",
    subtitle = "Proportion of OTEK_LANC by REC_IM status",
    x = "REC_IM (Recurrent MI)",
    y = "Proportion"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "right",
    legend.title = element_text(face = "bold", size = 12),
    legend.text = element_text(size = 11),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.title = element_text(face = "bold", size = 15, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray40"),
    axis.text = element_text(size = 11)
  )

print(rec_otek_plot)

# Alternative: Side-by-side count plot
rec_otek_count <- ggplot(data_rec_otek, aes(x = REC_IM, fill = OTEK_LANC)) +
  geom_bar(position = "dodge", alpha = 0.85, color = "white", linewidth = 0.5) +
  scale_fill_manual(
    values = c("No" = color_no, "Yes" = color_yes),
    name = "OTEK_LANC"
  ) +
  labs(
    title = "REC_IM → OTEK_LANC Relationship (Counts)",
    x = "REC_IM (Recurrent MI)",
    y = "Count"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "right",
    legend.title = element_text(face = "bold", size = 12),
    legend.text = element_text(size = 11),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.title = element_text(face = "bold", size = 15, hjust = 0.5),
    axis.text = element_text(size = 11)
  )

print(rec_otek_count)

# Separate plot: OTEK_LANC → NA_R_1_n
data_otek_na <- data %>%
  mutate(
    OTEK_LANC = factor(OTEK_LANC, levels = c(0, 1), labels = c("No OTEK_LANC", "Yes OTEK_LANC"))
  )

otek_na_plot <- ggplot(data_otek_na, aes(x = OTEK_LANC, y = NA_R_1_n, 
                                         fill = OTEK_LANC, color = OTEK_LANC)) +
  geom_jitter(width = 0.12, alpha = 0.4, size = 1.4) +
  geom_half_boxplot(side = "r", width = 0.15, alpha = 0.8, outlier.shape = NA) +
  scale_fill_manual(values = c("No OTEK_LANC" = color_no, "Yes OTEK_LANC" = color_yes)) +
  scale_color_manual(values = c("No OTEK_LANC" = border_no, "Yes OTEK_LANC" = border_yes)) +
  labs(
    title = "OTEK_LANC → NA_R_1_n Relationship",
    subtitle = "Distribution of NA_R_1_n by OTEK_LANC status",
    x = "OTEK_LANC (Pulmonary Edema)",
    y = "NA_R_1_n"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 15, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray40"),
    axis.text = element_text(size = 11)
  )

print(otek_na_plot)
