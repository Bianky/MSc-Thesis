
# DATA LOADING ------------------------------------------------------------------

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

# all factors
all_factors <- full_join(seed_factors_all, forest_factors_all) %>% 
  filter(!(ID == "MX_DR_FC6")) 


# DATA PREPARATION -------------------------------------------------------------

data <- all_factors %>% 
  dplyr::select(ID, 
                season,
                #Month, 
                richness, abundance, dispersal_biotic, dispersal_abiotic, guild_shadetolerant, guild_pioneer, guild_generalist, forest_cover, forest_connectivity, forest_early_ss, forest_late_ss, forest_type)

# predictor variables
data$forest_cover        <- as.numeric(scale(data$forest_cover))
data$forest_early_ss     <- as.numeric(scale(data$forest_early_ss))
data$forest_late_ss      <- as.numeric(scale(data$forest_late_ss))
data$forest_connectivity <- as.numeric(scale(data$forest_connectivity))
data$forest_type         <- as.factor(data$forest_type)

data$season <- as.factor(data$season)
data$Month <- as.factor(data$Month)

# MODELS -----------------------------------------------------------------------
library(performance)
library(lmerTest)
library(MuMIn)
library(glmmTMB)

# predictor var prep
data$abundance <- log1p(data$abundance)

pred_var <- data$guild_shadetolerant

# regular lm# 
# ------------------------------------------------------------------------------
lm1 <- lm(pred_var ~ forest_late_ss + forest_connectivity + forest_type, data = data)
summary(lm1)
check_model(lm1)
vif(lm1)


n <- nrow(data) # shadetolerant
data$guild_shadetolerant_beta <-
  (data$guild_shadetolerant * (n - 1) + 0.5) / n

s1 <- glmmTMB(
  guild_shadetolerant_beta ~ forest_late_ss  + forest_type,
  ziformula = ~1,
  family = beta_family(),
  data = data
)
summary(s1)
check_model(s1)

# season lm# 
# ------------------------------------------------------------------------------
data_dry <- data %>% filter(season == "dry")
pred_var <- data_dry$dispersal_biotic
lm_dry <- lm(pred_var ~ forest_cover + forest_late_ss + forest_connectivity + forest_type, data_dry)
summary(lm_dry)

data_wet <- data %>% filter(season == "wet")
pred_var <- data_wet$dispersal_abiotic
lm_wet02 <- lm(pred_var ~ forest_cover + forest_connectivity + forest_late_ss + forest_type, data_wet)
summary(lm_wet02)
vif(lm_wet02)

# shadetolerant
n <- nrow(data_dry)
data_dry$guild_shadetolerant_beta <-
  (data_dry$guild_shadetolerant * (n - 1) + 0.5) / n

s1 <- glmmTMB(
  guild_shadetolerant_beta ~ forest_cover + forest_late_ss + forest_connectivity + forest_type,
  family = beta_family(),
  data = data_dry
)
summary(s1)
check_model(s1)

