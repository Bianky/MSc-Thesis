

# LANDSCAPE ATTRIBUTES GRAPH ---------------------------------------------------
# ------------------------------------------------------------------------------

# DATA
# forest attri
forest_attri_wf <- read.csv("MSc-Thesis/Analysis/data/forest/wf/11_forest_attri.csv")
forest_attri_df <- read.csv("MSc-Thesis/Analysis/data/forest/df/11_forest_attri.csv")

forest_attri_all <- full_join(forest_attri_df, forest_attri_wf) %>% 
  dplyr::select(-X)

forest_data <- full_join(forest_attri_df, forest_attri_wf) %>% 
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


ggplot(forest_data, aes(forest_type, value, fill = forest_type)) +
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


# SEED ATTRIBUTES GRAPHS -------------------------------------------------------
# ------------------------------------------------------------------------------

# DATA
# seed attributes
seed_attri_wf <- read.csv("MSc-Thesis/Analysis/data/seed/wf/11_seed_attri.csv")
seed_attri_df <- read.csv("MSc-Thesis/Analysis/data/seed/df/11_seed_attri.csv")

seed_attri_all <- full_join(seed_attri_df, seed_attri_wf) %>% 
  dplyr::select(-X)

all_attri <- full_join(seed_attri_all, forest_attri_all) %>% 
  filter(!(ID == "MX_DR_FC6")) 

# seed attributes - season
seed_attri_wf_season <- read.csv("MSc-Thesis/Analysis/data/seed/wf/11_seed_attri_season.csv")
seed_attri_df_season <- read.csv("MSc-Thesis/Analysis/data/seed/df/11_seed_attri_season.csv")

seed_attri_all_season <- full_join(seed_attri_df_season, seed_attri_wf_season) %>% 
  dplyr::select(-X)

all_attri_season <- full_join(seed_attri_all_season, forest_attri_all) %>% 
  filter(!(ID == "MX_DR_FC6")) 


seed_data <- all_attri %>% 
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

seed_data$var <- factor(seed_data$var,
                             levels = c("log10(Abundance) (n)", "Richness (n)", "Biotic dispersal (%)", "Abiotic dispersal (%)", "Pioneer (%)",  "Generalist (%)", "Shade-tolerant (%)"))
seed_data$forest_type[seed_data$forest_type == "dry"] <- "Dry forest"
seed_data$forest_type[seed_data$forest_type == "wet"] <- "Wet forest"



seed_data_season <- all_attri_season %>% 
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
year <- ggplot(seed_data, aes(forest_type, value, fill = forest_type)) +
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

# SCATTERPLOTS-------------- ---------------------------------------------------
# ------------------------------------------------------------------------------

data_generalist <- all_attri %>% 
  mutate(forest_cover = forest_cover * 100,
        forest_late_ss = forest_late_ss * 100,
       dispersal_biotic = dispersal_biotic * 100,
      guild_generalist = guild_generalist * 100,
     guild_shadetolerant = guild_shadetolerant * 100) %>%
  rename(`Forest cover (%)` = forest_cover,
         `Late successional forest (%)` = forest_late_ss) %>% 
  pivot_longer(
    cols = c(`Forest cover (%)`, `Late successional forest (%)`),
    names_to = "predictor",
    values_to = "cover")

p1 <- ggplot(data_generalist, aes(cover, guild_generalist, color = predictor)) +
  geom_smooth(method = "lm", alpha = 0.18) +
  geom_point(alpha = 0.4, size = 2, position = position_jitter(width = 1, height = 0.2)) +
  theme_classic() +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 14)) +
  scale_color_manual(
    values = c(
      `Late successional forest (%)` = "#0072B2",
      `Forest cover (%)` = "#E69F00"
      
    ),
    labels = c(
      "Forest cover",
      "Late successional forest"
    )) +
  labs(x = "Percentage (%)", y = "Generalist seed guild (%)") +
  theme(
    axis.text = element_text(size = 14),      
    axis.title = element_text(size = 16),
    legend.title = element_blank(),
    legend.position = c(1, 1),
    legend.justification = c(1, 0.9)) +    
  annotate(
    "text", x = 30, y = 33, label = "β = -0.07", size = 6, color = "#0072B2") +
  annotate(
    "text", x = 30.5, y = 30, label = "p = 0.006", size = 6, color = "#0072B2") +
  annotate(
    "text", x = 70, y = 10, label = "β = -0.07", size = 6, color = "#E69F00") +
  annotate(
    "text", x = 70.5, y = 7, label = "p = 0.029", size = 6, color = "#E69F00") 


data_shadetolerant <- all_attri %>% 
  mutate(forest_late_ss = forest_late_ss * 100,
         guild_shadetolerant = guild_shadetolerant * 100) 

data_shadetolerant$forest_type[data_shadetolerant$forest_type == "dry"] <- "Dry forest"
data_shadetolerant$forest_type[data_shadetolerant$forest_type == "wet"] <- "Wet forest"

p2 <- data_shadetolerant %>% 
  ggplot(aes(forest_late_ss, guild_shadetolerant)) +
  geom_smooth(method = "lm", alpha = 0.18, color = "black") +
  geom_point(alpha = 1, size = 2, position = position_jitter(width = 1, height = 0.2), aes(color = forest_type)) +
  scale_color_manual(values = c(
    "Dry forest" = "#D55E00",
    "Wet forest" = "#009E73"
  )) +
  theme_classic() +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 14)) +
  scale_y_continuous(
    breaks = seq(0, 30, by = 10)) +
  scale_x_continuous(
    breaks = seq(30, 99, by = 10)) +
  labs(x = "Late successional forest (%)", y = "Shade-tolerant seed guild (%)") +
  theme(
    axis.text = element_text(size = 14),   
    axis.title = element_text(size = 16),
    legend.title = element_blank(),
    legend.position = c(0.9, 0.9),
    legend.justification = c(1, 1),
  )  +
  annotate(
    "text", x = 27, y = 12.5, label = "β = 0.06", size = 6) +
  annotate(
    "text", x = 28.2, y = 10.5, label = "p = 0.031", size = 6)

p1 + p2
