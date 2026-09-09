
# seed factors graph
# ------------------------------------------------------------------------------

# DATA
# seed factors
seed_factors_wf <- read.csv("MSc-Thesis/Analysis/data/seed/wf/11_seed_factors.csv")
seed_factors_df <- read.csv("MSc-Thesis/Analysis/data/seed/df/11_seed_factors.csv")

seed_factors_all <- full_join(seed_factors_df, seed_factors_wf) %>% 
  dplyr::select(-X)

# forest factors
forest_factors_wf <- read.csv("MSc-Thesis/Analysis/data/forest/wf/11_forest_factors.csv")
forest_factors_df <- read.csv("MSc-Thesis/Analysis/data/forest/df/11_forest_factors.csv")

forest_factors_all <- full_join(forest_factors_df, forest_factors_wf) %>% 
  dplyr::select(-X)

all_factors <- full_join(seed_factors_all, forest_factors_all) %>% 
  filter(!(ID == "MX_DR_FC6")) 

seed_data_year <- all_factors %>% 
  dplyr::select(abundance, richness, dispersal_biotic, dispersal_abiotic, guild_pioneer, guild_generalist, guild_shadetolerant, forest_type) %>%
  mutate(dispersal_biotic = dispersal_biotic * 100,
         dispersal_abiotic = dispersal_abiotic * 100,
         guild_generalist = guild_generalist * 100,
         guild_pioneer = guild_pioneer * 100,
         guild_shadetolerant = guild_shadetolerant * 100,
         abundance = log10(abundance)) %>% 
  rename("Biotic dispersal (%)" = dispersal_biotic,
         "Abiotic dispersal (%)" = dispersal_abiotic, 
         "Pioneer (%)" = guild_pioneer,
         "Generalist (%)" = guild_generalist,
         "Shade-tolerant (%)" = guild_shadetolerant,
         "log10(Abundance) (n)" = abundance, 
         "Richness (n)" = richness) %>% 
  pivot_longer(cols = c("Richness (n)", "log10(Abundance) (n)", "Biotic dispersal (%)", "Shade-tolerant (%)"), names_to = "var", values_to = "value") 

seed_data_year$var <- factor(seed_data_year$var,
                        levels = c("log10(Abundance) (n)", "Richness (n)", "Biotic dispersal (%)", "Abiotic dispersal (%)", "Pioneer (%)",  "Generalist (%)", "Shade-tolerant (%)"))
seed_data_year$forest_type[seed_data_year$forest_type == "dry"] <- "Dry forest"
seed_data_year$forest_type[seed_data_year$forest_type == "wet"] <- "Wet forest"



seed_data_season <- all_factors_season %>% 
  dplyr::select(season, abundance, richness, dispersal_biotic, dispersal_abiotic, guild_pioneer, guild_generalist, guild_shadetolerant, forest_type) %>%
  mutate(dispersal_biotic = dispersal_biotic * 100,
         dispersal_abiotic = dispersal_abiotic * 100,
         guild_generalist = guild_generalist * 100,
         guild_pioneer = guild_pioneer * 100,
         guild_shadetolerant = guild_shadetolerant * 100,
         abundance = log10(abundance)) %>% 
  rename("Biotic dispersal (%)" = dispersal_biotic,
         "Abiotic dispersal (%)" = dispersal_abiotic, 
         "Pioneer (%)" = guild_pioneer,
         "Generalist (%)" = guild_generalist,
         "Shade-tolerant (%)" = guild_shadetolerant,
         "log10(Abundance) (n)" = abundance, 
         "Richness (n)" = richness) %>% 
  pivot_longer(cols = c("Richness (n)", "log10(Abundance) (n)", "Biotic dispersal (%)", "Shade-tolerant (%)"), names_to = "var", values_to = "value") 

seed_data_season$var <- factor(seed_data_season$var,
                        levels = c("log10(Abundance) (n)", "Richness (n)", "Biotic dispersal (%)", "Abiotic dispersal (%)", "Pioneer (%)",  "Generalist (%)", "Shade-tolerant (%)"))
seed_data_season$forest_type[seed_data_season$forest_type == "dry"] <- "Dry forest"
seed_data_season$forest_type[seed_data_season$forest_type == "wet"] <- "Wet forest"


# GRAPH

year <- ggplot(seed_data_year, aes(forest_type, value, fill = forest_type)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.3) +
  facet_wrap(~ var, scale = "free_y", nrow = 1,
             strip.position = "left", 
             labeller = as_labeller(c(`log10(Abundance) (n)` = "log10(Seed abundance) (nr)", `Richness (n)` = "Seed richness (nr)", `Biotic dispersal (%)` = "Biotic seed dispersal (%)",`Shade-tolerant (%)` = "Shade-tolerant seed guild (%)") ) ) +
  scale_fill_manual(values = c(
    "Dry forest" = "gray",
    "Wet forest" = "gray")) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(y = "", x = "") +
  theme(
    strip.placement = "outside",
    strip.text.y.left = element_text(
      angle = 90,
      size = 16
    ),
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 18),
    strip.text = element_text(size = 16),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    legend.key.size = unit(1.2, "cm"))

season <- ggplot(seed_data_season, aes(forest_type, value, fill = season)) +
  geom_boxplot(
    position = position_dodge(width = 0.75)
  ) +
  geom_jitter(
    aes(fill = season),
    position = position_jitterdodge(
      jitter.width = 0.15,
      dodge.width = 0.75
    ),
    alpha = 0.5,
    show.legend = FALSE,
  ) +
  facet_wrap(~ var, scale = "free_y", nrow = 1,
             strip.position = "left", 
             labeller = as_labeller(c(`log10(Abundance) (n)` = "log10(Seed abundance) (nr)", `Richness (n)` = "Seed richness (nr)", `Biotic dispersal (%)` = "Biotic seed dispersal (%)",`Shade-tolerant (%)` = "Shade-tolerant seed guild (%)") ) ) +
  scale_fill_manual(values = c(
    "dry" = "#D55E00",
    "wet" = "#009E73")) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(y = "", x = "", fill = "Season") +
  theme(
    strip.placement = "outside",
    strip.text.y.left = element_text(
      angle = 90,
      size = 16
    ),
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 18),
    strip.text = element_text(size = 16),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    legend.key.size = unit(1.2, "cm")) 

year + season +  plot_layout(nrow = 2)

# whiskers <- seed_data_year %>%
#   group_by(var, forest_type) %>%
#   summarise(
#     Q1 = quantile(value, 0.25, na.rm = TRUE),
#     Q3 = quantile(value, 0.75, na.rm = TRUE),
#     IQR = IQR(value, na.rm = TRUE),
#     upper_whisker = min(
#       max(value, na.rm = TRUE),
#       Q3 + 1.5 * IQR
#     ),
#     .groups = "drop"
#   )
# 
# letters <- data.frame(
#   var = c("log10(Abundance) (n)", "Richness (n)", "Biotic dispersal (%)",  "Shadetolerant (%)"),           # replace with the exact facet name
#   forest_type = rep(c("dry", "wet"), 4),
#   value = Inf,
#   label = c("a", "a",
#             "a", "a",
#             "a", "b",
#             "a", "a",
#             "a", "a",
#             "a", "b",
#             "a", "b"))
# 
# letters <- letters %>%
#   dplyr::select(-value) %>%
#   left_join(
#     whiskers,
#     by = c("var", "forest_type")
#   ) %>%
#   mutate(
#     value = upper_whisker + 0.05 * IQR
#   )
# 
# seed_data_year$var <- factor(seed_data_year$var,
#                         levels = c("log10(Abundance) (n)", "Richness (n)", "Biotic dispersal (%)", "Abiotic dispersal (%)", "Pioneer (%)",  "Generalist (%)", "Shadetolerant (%)"))
# letters$var   <- factor(letters$var, levels = c("log10(Abundance) (n)", "Richness (n)", "Biotic dispersal (%)", "Abiotic dispersal (%)", "Pioneer (%)",  "Generalist (%)", "Shadetolerant (%)"))
# library(ggh4x)
# 
# 
# 
# 
# year + season +  plot_layout(nrow = 2)
# 
# 
# 
# seed_data_year$forest_type[seed_data_year$forest_type == "dry"] <- "Dry forest"
# seed_data_season$forest_type[seed_data_season$forest_type == "dry"] <- "Dry forest"
# 
# seed_data_year$forest_type[seed_data_year$forest_type == "wet"] <- "Wet forest"
# seed_data_season$forest_type[seed_data_season$forest_type == "wet"] <- "Wet forest"
# 
# letters <- data.frame(
#   var = rep(c("log10(Abundance) (n)", "Richness (n)", "Biotic dispersal (%)", "Shadetolerant (%)"), each = 2),           # replace with the exact facet name
#   forest_type = c("Dry forest", "Wet forest", "Dry forest", "Wet forest", "Dry forest", "Wet forest", "Dry forest", "Wet forest"),
#   value = Inf,
#   label = c("a", "a",
#             "a", "a",
#             "a", "a",
#             "a", "b"))
# 
# seed_data_year$var <- factor(seed_data_year$var,
#                         levels = c("log10(Abundance) (n)", "Richness (n)", "Biotic dispersal (%)", "Abiotic dispersal (%)", "Pioneer (%)",  "Generalist (%)", "Shadetolerant (%)"))
# letters$var   <- factor(letters$var, levels = c("log10(Abundance) (n)", "Richness (n)", "Biotic dispersal (%)", "Abiotic dispersal (%)", "Pioneer (%)",  "Generalist (%)", "Shadetolerant (%)"))
# 
# letters <- seed_data_year %>%
#   group_by(var, forest_type) %>%
#   summarise(
#     value = boxplot.stats(value)$stats[5],   # upper whisker
#     .groups = "drop"
#   ) %>%
#   group_by(var) %>%
#   mutate(
#     # add 5% of the y-range in each facet
#     value = value + 0.05 * diff(range(value))
#   ) %>%
#   ungroup() %>%
#   mutate(
#     label = c("a", "a",
#               "a", "a",
#               "a", "a",
#               "a", "b")
#   )
# 
# ab <- data.frame(
#   var = c("log10(Abundance) (n)"),
#   forest_type = c("Dry forest", "Wet forest"),
#   y = c(3.2, 3.7),
#   label = c("a", "a")
# )
# 
# ri <- data.frame(
#   var = c("Richness (n)"),
#   forest_type = c("Dry forest", "Wet forest"),
#   y = c(13, 24),
#   label = c("a", "a")
# )
# 
# 
# sh <- data.frame(
#   var = c("Shadetolerant (%)"),
#   forest_type = c("Dry forest", "Wet forest"),
#   y = c(30, 20),
#   label = c("a", "b")
# )
# 
# 
# bd <- data.frame(
#   var = c("Biotic dispersal (%)"),
#   forest_type = c("Dry forest", "Wet forest"),
#   y = c(5, 47),
#   label = c("a", "a")
# )
# 
# ab$var <- factor(ab$var, levels = levels(seed_data_year$var))
# ri$var <- factor(ri$var, levels = levels(seed_data_year$var))
# bd$var <- factor(bd$var, levels = levels(seed_data_year$var))
# sh$var <- factor(sh$var, levels = levels(seed_data_year$var))
# 
# seed_data_year$var <- factor(seed_data_year$var,
#                              levels = c("log10(Abundance) (n)", "Richness (n)", "Biotic dispersal (%)", "Shadetolerant (%)"))
# seed_data_season$var <- factor(seed_data_season$var,
#                                levels = c("log10(Abundance) (n)", "Richness (n)", "Biotic dispersal (%)", "Shadetolerant (%)"))
# 
# 
# ggplot() +
#   
#   ## Overall boxplot (background)
#   geom_boxplot(data = seed_data_year,
#     aes(x = forest_type, y = value, fill = forest_type),
#     width = 0.7,
#     alpha = 0.3,
#     outlier.shape = NA,
#     color = "black", fill = "darkgrey"
#   ) +
#   ## Points
#   geom_jitter(data = seed_data_year,
#               aes(x = forest_type, y = value, fill = forest_type),
#               position = position_jitterdodge(
#                 jitter.width = 0.1,
#                 dodge.width = 0.6
#               ),
#               alpha = 0.3, 
#               color = "darkgrey"
#   ) +
#   
#   ## Season-specific boxplots (foreground)
#   geom_boxplot(data = seed_data_season,
#     aes(x = forest_type, y = value, fill = season, group = interaction(forest_type, season)),
#     position = position_dodge(width = 0.6),
#     width = 0.25,
#     outlier.shape = NA
#   ) +
#   
#   ## Points
#   geom_jitter(data = seed_data_season,
#     aes(x = forest_type, y = value, color = season),
#     position = position_jitterdodge(
#       jitter.width = 0.1,
#       dodge.width = 0.6
#     ),
#     alpha = 0.8
#   ) +
#   
#   facet_wrap(~var, scales = "free_y", nrow = 1) +
#   scale_fill_manual(
#     name = "Season",
#     values = c(
#     "dry" = "#D55E00",
#     "wet" = "#009E73")) +
#   scale_color_manual(
#     name = "Season",
#     values = c(
#     "dry" = "#D55E00",
#     "wet" = "#009E73")) +
#   theme_minimal() +
#   theme(legend.position = "bottom") +
#   labs(y = "", x = "", fill = "Season") +
#   theme(
#     axis.text = element_text(size = 14),
#     axis.title = element_text(size = 18),
#     strip.text = element_text(size = 16),
#     legend.title = element_text(size = 16),
#     legend.text = element_text(size = 14),
#     legend.key.size = unit(1.2, "cm")) +
#   # geom_text(
#   #   data = letters,
#   #   aes(x = forest_type, y = value, label = label),
#   #   inherit.aes = FALSE,
#   #   vjust = 0,
#   #   fontface = "italic",
#   #   size = 6, 
#   #   color = "grey2") +
#   geom_text(
#     data = ab,
#     aes(forest_type, y, label = label),
#     inherit.aes = FALSE, size = 6) +
#   geom_text(
#     data = ri,
#     aes(forest_type, y, label = label),
#     inherit.aes = FALSE, size = 6) +
#   geom_text(
#     data = sh,
#     aes(forest_type, y, label = label),
#     inherit.aes = FALSE, size = 6) +
#   geom_text(
#     data = bd,
#     aes(forest_type, y, label = label),
#     inherit.aes = FALSE, size = 6)
# 
# 
# 
# plot_year <- seed_data_year %>%
#   mutate(period = "Year",
#          group_var = forest_type)
# 
# plot_season <- seed_data_season %>%
#   mutate(period = "Season",
#          group_var = interaction(forest_type, season))
# 
# plot_data <- bind_rows(
#   plot_year %>% dplyr::select(var, forest_type, value, period, group_var),
#   plot_season %>% dplyr::select(var, forest_type, season, value, period, group_var)
# )
# 
# ggplot() +
#   
#   ## Year row
#   geom_boxplot(
#     data = filter(plot_data, period == "Year"),
#     aes(x = forest_type, y = value),
#     width = 0.7,
#     alpha = 0.3,
#     outlier.shape = NA,
#     color = "black",
#     fill = "darkgrey"
#   ) +
#   
#   ## Year points
#   geom_jitter(
#     data = filter(plot_data, period == "Year"),
#     aes(x = forest_type, y = value),
#     width = 0.1,
#     alpha = 0.3,
#     color = "darkgrey"
#   ) +
#   
#   ## Season row
#   geom_boxplot(
#     data = filter(plot_data, period == "Season"),
#     aes(
#       x = forest_type,
#       y = value,
#       fill = season,
#       group = interaction(forest_type, season)
#     ),
#     position = position_dodge(width = 0.6),
#     width = 0.25,
#     outlier.shape = NA
#   ) +
#   
#   ## Season points
#   geom_jitter(
#     data = filter(plot_data, period == "Season"),
#     aes(
#       x = forest_type,
#       y = value,
#       color = season
#     ),
#     position = position_jitterdodge(
#       jitter.width = 0.1,
#       dodge.width = 0.6
#     ),
#     alpha = 0.8
#   ) +
#   
#   ## Two rows: Year / Season
#   facet_grid(
#     period ~ var,
#     scales = "free_y"
#   ) +
#   
#   scale_fill_manual(
#     name = "Season",
#     values = c(
#       "dry" = "#D55E00",
#       "wet" = "#009E73"
#     )
#   ) +
#   
#   scale_color_manual(
#     name = "Season",
#     values = c(
#       "dry" = "#D55E00",
#       "wet" = "#009E73"
#     )
#   ) +
#   
#   theme_minimal() +
#   
#   theme(
#     legend.position = "bottom",
#     axis.text = element_text(size = 14),
#     axis.title = element_text(size = 18),
#     strip.text = element_text(size = 16),
#     legend.title = element_text(size = 16),
#     legend.text = element_text(size = 14),
#     legend.key.size = unit(1.2, "cm")
#   ) +
#   
#   labs(
#     y = "",
#     x = "",
#     fill = "Season"
#   )
#   
# 
# 
# 
# 
# 
