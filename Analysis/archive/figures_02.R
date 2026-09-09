
data_m <- data %>% 
  mutate(forest_cover = forest_cover * 100,
         forest_late_ss = forest_late_ss * 100)

p1 <- ggplot(data_m, aes(forest_cover, abundance, color = forest_type)) +
  geom_smooth(method = "lm", se = F) +
  geom_point(alpha = 0.3, position = position_jitter(width = 0.5, height = 0)) +
  theme_minimal() +
  coord_cartesian(ylim = c(0, 750)) +
  theme(legend.position = "bottom") +
  scale_x_continuous(
    breaks = seq(30, 99, by = 10)) +
  scale_color_manual(values = c(
    "dry" = "#D55E00",
    "wet" = "#009E73"
  )) +
  labs(x = "forest cover (%)", y = "abundance (n)") +
  theme(
    axis.text = element_text(size = 14),      # numbers/tick labels
    axis.title = element_text(size = 16)      # axis titles
  )

p2 <- ggplot(data_m, aes(forest_late_ss, abundance, color = forest_type)) +
  geom_smooth(method = "lm", se = F) +
  geom_point(alpha = 0.3, position = position_jitter(width = 0.5, height = 0)) +
  theme_minimal() +
  coord_cartesian(ylim = c(0, 750)) +
  theme(legend.position = "bottom") +
  scale_x_continuous(
    breaks = seq(30, 99, by = 10)) +
  scale_color_manual(values = c(
    "dry" = "#D55E00",
    "wet" = "#009E73"
  )) +
  labs(x = "late successional forest cover (%)", y = "abundance (n)") +
  theme(
    axis.text = element_text(size = 14),      # numbers/tick labels
    axis.title = element_text(size = 16)      # axis titles
  )

data_s <- data %>% 
  mutate(forest_cover = forest_cover * 100,
         forest_late_ss = forest_late_ss * 100,
         dispersal_abiotic = dispersal_abiotic * 100,
         guild_generalist = guild_generalist * 100)

ps <- ggplot(data_ws, aes(forest_cover, guild_generalist)) +
  geom_smooth(method = "lm", se = F) +
  geom_point(alpha = 0.3, position = position_jitter(width = 0.5, height = 0)) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_x_continuous(
    breaks = seq(30, 99, by = 10)) +
  labs(x = "forest cover (%)", y = "richness (n)") +
  theme(
    axis.text = element_text(size = 14),      # numbers/tick labels
    axis.title = element_text(size = 16)      # axis titles
  )

data_ws <- data %>% 
  filter(season == "wet") %>% 
  mutate(forest_cover = forest_cover * 100,
         forest_late_ss = forest_late_ss * 100,
         dispersal_abiotic = dispersal_abiotic * 100,
         guild_generalist = guild_generalist * 100)

p3 <- ggplot(data_ws, aes(forest_cover, richness, color = forest_type)) +
  geom_smooth(method = "lm", se = F) +
  geom_point(alpha = 0.3, position = position_jitter(width = 0.5, height = 0)) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_x_continuous(
    breaks = seq(30, 99, by = 10)) +
  scale_color_manual(values = c(
    "dry" = "#D55E00",
    "wet" = "#009E73"
  )) +
  labs(x = "forest cover (%)", y = "richness (n)") +
  theme(
    axis.text = element_text(size = 14),      # numbers/tick labels
    axis.title = element_text(size = 16)      # axis titles
  )

p4 <- ggplot(data_ws, aes(forest_late_ss, richness, color = forest_type)) +
  geom_smooth(method = "lm", se = F) +
  geom_point(alpha = 0.3, position = position_jitter(width = 0.5, height = 0)) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_x_continuous(
    breaks = seq(30, 99, by = 10)) +
  scale_color_manual(values = c(
    "dry" = "#D55E00",
    "wet" = "#009E73"
  )) +
  labs(x = "late successional forest cover (%)", y = "richness (n)") +
  theme(
    axis.text = element_text(size = 14),      # numbers/tick labels
    axis.title = element_text(size = 16)      # axis titles
  )

p5 <- ggplot(data_ws, aes(forest_connectivity, richness, color = forest_type)) +
  geom_smooth(method = "lm", se = F) +
  geom_point(alpha = 0.3, position = position_jitter(width = 0.5, height = 0)) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_x_continuous(
    breaks = seq(30, 99, by = 10)) +
  scale_color_manual(values = c(
    "dry" = "#D55E00",
    "wet" = "#009E73"
  )) +
  labs(x = "forest connectivity (%)", y = "richness (n)") +
  theme(
    axis.text = element_text(size = 14),      # numbers/tick labels
    axis.title = element_text(size = 16)      # axis titles
  )

p6 <- ggplot(data_ws, aes(forest_cover, dispersal_biotic, color = forest_type)) +
  geom_smooth(method = "lm", se = F) +
  geom_point(alpha = 0.3, position = position_jitter(width = 0.5, height = 0)) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_x_continuous(
    breaks = seq(30, 99, by = 10)) +
  scale_color_manual(values = c(
    "dry" = "#D55E00",
    "wet" = "#009E73"
  )) +
  labs(x = "forest cover (%)", y = "biotic dispersal (%)") +
  theme(
    axis.text = element_text(size = 14),      # numbers/tick labels
    axis.title = element_text(size = 16)      # axis titles
  )

p7 <- ggplot(data_ws, aes(forest_cover, dispersal_abiotic, color = forest_type)) +
  geom_smooth(method = "lm", se = F) +
  geom_point(alpha = 0.3, position = position_jitter(width = 0.5, height = 0)) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_x_continuous(
    breaks = seq(30, 99, by = 10)) +
  scale_color_manual(values = c(
    "dry" = "#D55E00",
    "wet" = "#009E73"
  )) +
  labs(x = "forest cover (%)", y = "abiotic dispersal (%)") +
  theme(
    axis.text = element_text(size = 14),      # numbers/tick labels
    axis.title = element_text(size = 16)      # axis titles
  )

p8 <- ggplot(data_ws, aes(forest_cover, guild_generalist)) +
  geom_smooth(method = "lm", se = F, colour = "black") +
  geom_point(aes(color = forest_type), alpha = 0.3, position = position_jitter(width = 0.5, height = 0)) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_x_continuous(
    breaks = seq(30, 99, by = 10)) +
  scale_color_manual(values = c(
    "dry" = "#D55E00",
    "wet" = "#009E73"
  )) +
  labs(x = "forest cover (%)", y = "generalist guild (%)") +
  theme(
    axis.text = element_text(size = 14),      # numbers/tick labels
    axis.title = element_text(size = 16)      # axis titles
  )


p2 + p4 + p6 + p8



