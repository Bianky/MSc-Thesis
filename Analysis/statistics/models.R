
# DATA LOADING ------------------------------------------------------------------

# seed attributes
seed_attri_wf <- read.csv("MSc-Thesis/Analysis/data/seed/wf/11_seed_attri.csv")
seed_attri_df <- read.csv("MSc-Thesis/Analysis/data/seed/df/11_seed_attri.csv")

seed_attri_all <- full_join(seed_attri_df, seed_attri_wf) %>% 
  dplyr::select(-X)

# seed attributes - season
seed_attri_wf_season <- read.csv("MSc-Thesis/Analysis/data/seed/wf/11_seed_attri_season.csv")
seed_attri_df_season <- read.csv("MSc-Thesis/Analysis/data/seed/df/11_seed_attri_season.csv")

seed_attri_all_season <- full_join(seed_attri_df_season, seed_attri_wf_season) %>% 
  dplyr::select(-X)

# forest attributes
forest_attri_wf <- read.csv("MSc-Thesis/Analysis/data/forest/wf/11_forest_attri.csv")
forest_attri_df <- read.csv("MSc-Thesis/Analysis/data/forest/df/11_forest_attri.csv")

forest_attri_all <- full_join(forest_attri_df, forest_attri_wf) %>% 
  dplyr::select(-X)

# all attributes
all_attri <- full_join(seed_attri_all, forest_attri_all) %>% 
  filter(!(ID == "MX_DR_FC6")) 

# all attributes season
all_attri_season <- full_join(seed_attri_all_season, forest_attri_all) %>% 
  filter(!(ID == "MX_DR_FC6")) 


# DATA PREPARATION -------------------------------------------------------------
data <- all_attri %>% 
  dplyr::select(ID, 
                richness, abundance, dispersal_biotic, dispersal_abiotic, 
                guild_shadetolerant, guild_pioneer, guild_generalist, forest_cover, 
                forest_connectivity, forest_early_ss, forest_late_ss, forest_type)

# predictor variables
data$forest_cover        <- as.numeric(scale(data$forest_cover))
data$forest_early_ss     <- as.numeric(scale(data$forest_early_ss))
data$forest_late_ss      <- as.numeric(scale(data$forest_late_ss))
data$forest_connectivity <- as.numeric(scale(data$forest_connectivity))
data$forest_type         <- as.factor(data$forest_type)


# MODELS -----------------------------------------------------------------------
# predictor var prep
data$abundance <- log1p(data$abundance)

# define response var
pred_var <- data$guild_generalist

# lm 
# ------------------------------------------------------------------------------
lm1 <- lm(pred_var ~ forest_cover + forest_type, data = data)
summary(lm1)

lm2 <- lm(pred_var ~ forest_connectivity + forest_type, data = data)
summary(lm2)


lm3 <- lm(pred_var ~ forest_late_ss + forest_type, data = data)
summary(lm3)


# models check
check_model(lm1)
vif(lm1)


# lm per season
# ------------------------------------------------------------------------------
data_season <- all_attri_season %>% 
  dplyr::select(ID, 
                season,
                richness, abundance, dispersal_biotic, dispersal_abiotic, 
                guild_shadetolerant, guild_pioneer, guild_generalist, forest_cover, 
                forest_connectivity, forest_early_ss, forest_late_ss, forest_type)

# predictor variables
data_season$forest_cover        <- as.numeric(scale(data_season$forest_cover))
data_season$forest_early_ss     <- as.numeric(scale(data_season$forest_early_ss))
data_season$forest_late_ss      <- as.numeric(scale(data_season$forest_late_ss))
data_season$forest_connectivity <- as.numeric(scale(data_season$forest_connectivity))
data_season$forest_type         <- as.factor(data_season$forest_type)

data_season$season <- as.factor(data_season$season)

# subset of data for dry forest only
data_dry <- data_season %>% filter(season == "dry")

# define response variable
pred_var <- data_dry$richness

# lm models
lm_dry02 <- lm(pred_var ~ forest_cover + forest_type, data_dry)
summary(lm_dry02)
lm_dry03 <- lm(pred_var ~ forest_connectivity + forest_type, data_dry)
summary(lm_dry03)
lm_dry01 <- lm(pred_var ~ forest_late_ss + forest_type, data_dry)
summary(lm_dry01)

# subset of data for wet forest only
data_wet <- data_season %>% filter(season == "wet")

# define response variable
pred_var <- data_wet$richness

# lm models
lm_wet02 <- lm(pred_var ~ forest_cover + forest_type, data_wet)
summary(lm_wet02)
lm_wet03 <- lm(pred_var ~ forest_connectivity + forest_type, data_wet)
summary(lm_wet03)
lm_wet01 <- lm(pred_var ~ forest_late_ss + forest_type, data_wet)
summary(lm_wet01)






