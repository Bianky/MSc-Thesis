
# forest factors graph
# ------------------------------------------------------------------------------

# DATA
# forest factors
forest_factors_wf <- read.csv("MSc-Thesis/Analysis/data/forest/wf/11_forest_factors.csv")
forest_factors_df <- read.csv("MSc-Thesis/Analysis/data/forest/df/11_forest_factors.csv")

forest_data <- full_join(forest_factors_df, forest_factors_wf) %>% 
  dplyr::select(-X) %>% 
  filter(!(ID == "MX_DR_FC6")) %>% 
  mutate(forest_cover = forest_cover * 100,
         forest_late_ss = forest_late_ss * 100) %>% 
  rename(`Forest cover` = forest_cover,
         `Late successional forest` = forest_late_ss, 
         `Forest connectivity` = forest_connectivity) %>% 
  pivot_longer(cols = c(`Forest cover`, `Late successional forest`, `Forest connectivity`), names_to = "var", values_to = "value")

forest_data$forest_type[forest_data$forest_type == "wet"] <- "Wet forest"
forest_data$forest_type[forest_data$forest_type == "dry"] <- "Dry forest"


# GRAPH

forest_data$var <- factor(
  forest_data$var,
  levels = c(
    "Forest cover",
    "Late successional forest",
    "Forest connectivity"
  )
)

forest_data$forest_type <- factor(
  forest_data$forest_type,
  levels = c("Wet forest", "Dry forest")
)


fp <- ggplot(forest_data, aes(forest_type, value, fill = forest_type)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.3) +
  facet_wrap(~var , scales = "free_y", ncol = 1) +
  theme_minimal() +
  theme() +
  labs(y = "Percentage (%)",
       x = "") +
  #scale_fill_manual(values = c(
  #  "dry" = "#D55E00",
  #  "wet" = "#009E73")) +
  scale_fill_manual(values = c(
    "Dry forest" = "#D55E00",
    "Wet forest" = "#009E73")) +
  theme(
    legend.position = "none",
    axis.text = element_text(size = 14),      # numbers/tick labels
    axis.title = element_text(size = 16),
    strip.text = element_text(size = 18)) +      # axis titles
  coord_flip()


