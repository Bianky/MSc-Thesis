
# regular lm

data_generalist <- data %>% 
 # mutate(forest_cover = forest_cover * 100,
  #       forest_late_ss = forest_late_ss * 100,
   #      dispersal_biotic = dispersal_biotic * 100,
    #     guild_generalist = guild_generalist * 100,
     #    guild_shadetolerant = guild_shadetolerant * 100) %>% 
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
 # scale_x_continuous(
  # breaks = seq(0, 100, by = 10)) +
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
    axis.text = element_text(size = 14),      # numbers/tick labels
    axis.title = element_text(size = 16),
    legend.title = element_blank(),
    legend.position = c(1, 1),
    legend.justification = c(1, 0.9)) +      # axis titles
  annotate(
    "text", x = 30, y = 33, label = "β = -0.07", size = 6, color = "#0072B2") +
  annotate(
    "text", x = 30.5, y = 30, label = "p = 0.006", size = 6, color = "#0072B2") +
  annotate(
    "text", x = 70, y = 10, label = "β = -0.07", size = 6, color = "#E69F00") +
  annotate(
    "text", x = 70.5, y = 7, label = "p = 0.029", size = 6, color = "#E69F00") 

# p6 <- data %>% 
#   mutate(forest_cover = forest_cover * 100,
#          guild_generalist = guild_generalist * 100) %>% 
#   ggplot(aes(forest_cover, guild_generalist)) +
#   geom_smooth(method = "lm", alpha = 0.18, color = "black") +
#   geom_point(alpha = 0.4, size = 2, position = position_jitter(width = 1, height = 0.2)) +
#   theme_minimal() +
#   theme(legend.position = "bottom",
#         legend.text = element_text(size = 14)) +
#   scale_x_continuous(
#     breaks = seq(30, 99, by = 10)) +
#   labs(x = "Forest cover (%)", y = "Generalist guild (%)") +
#   theme(
#     axis.text = element_text(size = 14),      # numbers/tick labels
#     axis.title = element_text(size = 16),
#     legend.title = element_blank(),
#     legend.position = c(0.9, 0.9),
#     legend.justification = c(1, 1))      # axis titles
# 
# p7 <- data %>% 
#   mutate(forest_late_ss = forest_late_ss * 100,
#          guild_generalist = guild_generalist * 100) %>% 
#   ggplot(aes(forest_late_ss, guild_generalist)) +
#   geom_smooth(method = "lm", alpha = 0.18, color = "black") +
#   geom_point(alpha = 0.4, size = 2, position = position_jitter(width = 1, height = 0.2)) +
#   theme_minimal() +
#   theme(legend.position = "bottom",
#         legend.text = element_text(size = 14)) +
#   scale_x_continuous(
#     breaks = seq(30, 99, by = 10)) +
#   labs(x = "Late successional forest cover (%)", y = "Generalist guild (%)") +
#   theme(
#     axis.text = element_text(size = 14),      # numbers/tick labels
#     axis.title = element_text(size = 16),
#     legend.title = element_blank(),
#     legend.position = c(0.9, 0.9),
#     legend.justification = c(1, 1))      # axis titles

data_shadetolerant <- data %>% 
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
    axis.text = element_text(size = 14),      # numbers/tick labels
    axis.title = element_text(size = 16),
    legend.title = element_blank(),
    legend.position = c(0.9, 0.9),
    legend.justification = c(1, 1),
    )  +    # axis titles
  annotate(
    "text", x = 27, y = 12.5, label = "β = 0.06", size = 6) +
  annotate(
    "text", x = 28.2, y = 10.5, label = "p = 0.031", size = 6)


p1 + p2

# season lm
# data_ds <- data %>% 
#   filter(season == "dry") %>% 
#   mutate(dispersal_biotic = dispersal_biotic * 100)
# 
# mod <- lm(dispersal_biotic ~ forest_connectivity + forest_type, data = data_ds)
# 
# coef <- coef(summary(mod))[2, "Estimate"]
# pval <- coef(summary(mod))[2, "Pr(>|t|)"]
# 
# label <- sprintf("β = %.3f\np = %.3f", coef, pval)
# 
# p2 <- ggplot(data_ds, aes(forest_connectivity, dispersal_biotic)) +
#   geom_smooth(method = "lm", alpha = 0.18, color = "black") +
#   geom_point(alpha = 0.3, position = position_jitter(width = 0.5, height = 0)) +
#   theme_minimal() +
#   theme(legend.position = "bottom") +
#   scale_x_continuous(
#     breaks = seq(60, 100, by = 10)) +
#   scale_y_continuous(
#     breaks = seq(0, 100, by = 20)) +
#   labs(x = "Forest connectivity (%)", y = "Biotic dispersal (%)") +
#   theme(
#     axis.text = element_text(size = 14),      # numbers/tick labels
#     axis.title = element_text(size = 16)) +      # axis titles
#   annotate(
#     "text",
#     x = min(data_ds$forest_connectivity),
#     y = min(data_ds$dispersal_biotic),
#     label = label,
#     hjust = 0,
#     vjust = 0,
#     size = 5
#   )
# 
# data_ws <- data %>% 
#   filter(season == "wet") %>% 
#   mutate(dispersal_biotic = dispersal_biotic * 100,
#          guild_generalist = guild_generalist * 100, 
#          forest_cover = forest_cover * 100,
#          forest_late_ss = forest_late_ss * 100)
# 
# data_ws$forest_type[data_ws$forest_type == "dry"] <- "Dry forest"
# data_ws$forest_type[data_ws$forest_type == "wet"] <- "Wet forest"
# 
# p3 <- ggplot(data_ws, aes(forest_cover, dispersal_biotic, color = forest_type)) +
#   geom_smooth(method = "lm", alpha = 0.18) +
#   geom_point(alpha = 0.3, position = position_jitter(width = 0.5, height = 0)) +
#   theme_classic() +
#   theme(legend.position = "bottom") +
#   scale_color_manual(values = c(
#     "Dry forest" = "#D55E00",
#     "Wet forest" = "#009E73"
#   )) +
#   scale_x_continuous(
#     breaks = seq(0, 100, by = 10)) +
#   scale_y_continuous(
#     breaks = seq(0, 100, by = 20)) +
#   labs(x = "Forest cover (%)", y = "Biotic dispersal (%)", color = "Forest") +
#   theme(
#     axis.text = element_text(size = 14),      # numbers/tick labels
#     axis.title = element_text(size = 16),
#     legend.title = element_blank(),
#     legend.position = c(0.9, 0.9),
#     legend.justification = c(1, 1),
#     legend.text = element_text(size = 14)) +      # axis titles)      # axis titles
#   annotate(
#     "text", x = 30, y = -3, label = "β = 0.059", size = 6) +
#   annotate(
#     "text", x = 30, y = -8, label = "p = 0.064", size = 6) +
#   annotate(
#     "text", x = 18, y = 55, label = "c)", size = 6) 
# 
# p4 <- ggplot(data_ws, aes(forest_late_ss, guild_generalist)) +
#   geom_smooth(method = "lm", alpha = 0.18, color = "black") +
#   geom_point(alpha = 0.3, position = position_jitter(width = 0.5, height = 0)) +
#   theme_classic() +
#   theme(legend.position = "bottom") +
#   scale_x_continuous(
#     breaks = seq(0, 100, by = 10)) +
#   scale_y_continuous(
#     breaks = seq(0, 100, by = 20)) +
#   labs(x = "Late successional forest cover (%)", y = "Generalist guild (%)", color = "Forest type") +
#   theme(
#     axis.text = element_text(size = 14),      # numbers/tick labels
#     axis.title = element_text(size = 16),
#     legend.title = element_blank(),
#     legend.position = c(0.9, 0.9),
#     legend.justification = c(1, 1),
#     legend.text = element_text(size = 14)) +      # axis titles
#   annotate(
#     "text", x = 30, y = 3, label = "β = -0.061", size = 6) +
#   annotate(
#     "text", x = 29.5, y = 0, label = "p = 0.044", size = 6) +
#   annotate(
#     "text", x = 18, y = 40, label = "d)", size = 6) 
#   
# p1 + p2 + p3 + p4
