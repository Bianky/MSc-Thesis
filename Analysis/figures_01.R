
library(lubridate)

# data figures
data_boxplot <- data %>% 
  dplyr::select(abundance, richness, dispersal_biotic, dispersal_abiotic, guild_pioneer, guild_generalist, guild_shadetolerant, forest_type) %>%
  rename("biotic dispersal %" = dispersal_biotic,
         "abiotic dispersal %" = dispersal_abiotic, 
         "pioneer %" = guild_pioneer,
         "generalist %" = guild_generalist,
         "shadetolerant %" = guild_shadetolerant) %>% 
  pivot_longer(cols = c(1:7), names_to = "var", values_to = "value") 

ggplot(data_boxplot, aes(forest_type, value, fill = forest_type)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.3) +
  facet_wrap(~var , scales = "free_y", nrow = 1) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(y = "")

data_month <- data %>% 
  mutate(month = factor(Month, levels = month.name)) %>% 
  dplyr::select(abundance, richness, dispersal_biotic, dispersal_abiotic, guild_pioneer, guild_generalist, guild_shadetolerant, forest_type, forest_cover, forest_late_ss, month) #%>%
 # rename("biotic dispersal %" = dispersal_biotic,
 #        "abiotic dispersal %" = dispersal_abiotic, 
 #        "pioneer %" = guild_pioneer,
 #        "generalist %" = guild_generalist,
 #        "shadetolerant %" = guild_shadetolerant) %>% 
 # pivot_longer(cols = c(1:7), names_to = "var", values_to = "value") 

data_m <- data %>% 
  mutate(forest_cover = forest_cover * 100,
         forest_late_ss = forest_late_ss * 100)

p1 <- ggplot(data_m, aes(forest_cover, abundance, color = forest_type)) +
  geom_smooth(method = "lm", se = F) +
  geom_point(alpha = 0.3, position = position_jitter(width = 0.5, height = 0)) +
  theme_minimal() +
  coord_cartesian(ylim = c(0, 750)) +
  theme(legend.position = "none") +
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
  theme(legend.position = "none") +
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


  
data_season <- data %>% 
  dplyr::select(abundance, richness, dispersal_biotic, dispersal_abiotic, guild_pioneer, guild_generalist, guild_shadetolerant, forest_type, season) %>%
  rename("biotic dispersal %" = dispersal_biotic,
         "abiotic dispersal %" = dispersal_abiotic, 
         "pioneer %" = guild_pioneer,
         "generalist %" = guild_generalist,
         "shadetolerant %" = guild_shadetolerant) %>% 
  pivot_longer(cols = c(1:7), names_to = "var", values_to = "value") 
  
s <- ggplot(data_season, aes(season, value, fill = forest_type)) +
  geom_col(position = "dodge") +
  labs(x = "") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  facet_wrap(~var , scales = "free_y", ncol = 1) 


m + s

# forest

data_forest <- data %>% 
  pivot_longer(cols = c(forest_cover, forest_late_ss, forest_connectivity), names_to = "var", values_to = "value")

ggplot(data_forest, aes(forest_type, value, fill = forest_type)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.3) +
  facet_wrap(~var , scales = "free_y", ncol = 1) +
  theme_minimal() +
  labs(y = "") 


# most important
a <- ggplot(data_month, aes(forest_cover, abundance)) +
  geom_smooth(method = "lm") +
  geom_point(aes(color = forest_type)) +
  coord_cartesian(ylim = c(0, 1000)) +
  theme_minimal() +
  theme(legend.position = "bottom") 

b <- ggplot(data_month, aes(forest_late_ss, abundance)) +
  geom_smooth(method = "lm") +
  geom_point(aes(color = forest_type)) +
  coord_cartesian(ylim = c(0, 1000))+
  theme_minimal() +
  theme(legend.position = "bottom") 

a + b

c <- ggplot(data_month, aes(forest_late_ss, richness)) +
  geom_smooth(method = "lm") +
  geom_point(aes(color = forest_type)) +
  coord_cartesian(ylim = c(0, 12)) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(title = "month")

d <- ggplot(data, aes(forest_late_ss, richness)) +
  geom_smooth(method = "lm") +
  geom_point(aes(color = forest_type)) +
  coord_cartesian(ylim = c(0, 20))+
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(title = "regular")

e <- data_season %>% filter(season == "wet") %>% 
  ggplot(aes(forest_cover, richness)) +
  geom_smooth(method = "lm") +
  geom_point(aes(color = forest_type)) +
  coord_cartesian(ylim = c(0, 20)) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(title = "wet season")

f <- data_season %>% filter(season == "wet") %>% 
  ggplot(aes(forest_late_ss, richness)) +
  geom_smooth(method = "lm") +
  geom_point(aes(color = forest_type)) +
  coord_cartesian(ylim = c(0, 20)) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(title = "wet season")

c + d + e + f

g <- ggplot(data, aes(forest_cover, dispersal_abiotic)) +
  geom_smooth(method = "lm") +
  geom_point(aes(color = forest_type)) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(title = "regular")

h <- data_season %>% filter(season == "wet") %>% 
  ggplot(aes(forest_cover, dispersal_abiotic)) +
  geom_smooth(method = "lm") +
  geom_point(aes(color = forest_type)) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(title = "wet season")

g + h

i <- ggplot(data, aes(forest_cover, guild_generalist)) +
  geom_smooth(method = "lm") +
  geom_point(aes(color = forest_type)) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(title = "regular")

j <- data_season %>% filter(season == "wet") %>%  
  ggplot(aes(forest_cover, guild_generalist)) +
  geom_smooth(method = "lm") +
  geom_point(aes(color = forest_type)) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(title = "wet season")

k <- data_season %>% filter(season == "dry") %>%  
  ggplot(aes(forest_cover, guild_generalist)) +
  geom_smooth(method = "lm") +
  geom_point(aes(color = forest_type)) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(title = "dry season")

i + j + k



