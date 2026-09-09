
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
                #season,
                richness, abundance, dispersal_biotic, dispersal_abiotic, guild_shadetolerant, guild_pioneer, guild_generalist, forest_cover, forest_connectivity, forest_early_ss, forest_late_ss, forest_type)

# predictor variables
data$forest_cover        <- as.numeric(scale(data$forest_cover))
data$forest_early_ss     <- as.numeric(scale(data$forest_early_ss))
data$forest_late_ss      <- as.numeric(scale(data$forest_late_ss))
data$forest_connectivity <- as.numeric(scale(data$forest_connectivity))
data$forest_type         <- as.factor(data$forest_type)

data$season <- as.factor(data$season)

# MODELS -----------------------------------------------------------------------

# predictor var prep
data$abundance <- log1p(data$abundance)

pred_var <- data$guild_generalist

# lm 
# ------------------------------------------------------------------------------
lm1 <- lm(pred_var ~ forest_cover + forest_type , data = data)
summary(lm1)
check_model(lm1)
vif(lm1)

lm2 <- lm(pred_var ~ forest_connectivity + forest_type, data = data)
summary(lm2)
check_model(lm2)
vif(lm2)

lm3 <- lm(pred_var ~ forest_late_ss + forest_type, data = data)
summary(lm3)
check_model(lm3)
vif(lm3)

n <- nrow(data) # shadetolerant
data$dispersal_biotic_beta <-
  (data$dispersal_biotic * (n - 1) + 0.5) / n

s1 <- glmmTMB(
  dispersal_biotic_beta ~ forest_late_ss + forest_type,
  family = beta_family(),
  data = data
)
summary(s1)
check_model(s1)




# dredge function
# ------------------------------------------------------------------------------
options(na.action = "na.fail")
global_model <- lm(pred_var ~ forest_cover + forest_late_ss + forest_connectivity + forest_type, data = data)
dd <- dredge(global_model)
get.models(dd, subset = 1)[[1]]


# lmm per season
# ------------------------------------------------------------------------------
a <- lmer(pred_var ~ forest_cover + forest_type + season + (1|ID), data = data, REML = F)
b <- lmer(pred_var ~ forest_cover + forest_type + (1|season) + (1|ID), data = data, REML = F)
anova(a, b)

pred_var <- data$guild_generalist


lmm0 <- lmer(pred_var ~ 1 + (1|ID), data = data, REML = FALSE)

lmm1 <- lmer(pred_var ~ forest_cover + forest_type + season + (1|ID), data = data, REML = F)
summary(lmm1)
#print(check_model(lmm1)) # does not work
#isSingular(lmm1)

lmm2 <- lmer(pred_var ~ forest_connectivity + forest_type + season + (1|ID), data = data)
summary(lmm2)
#isSingular(lmm2)

lmm3 <- lmer(pred_var ~ forest_late_ss + forest_type + season + (1|ID), data = data)
#isSingular(lmm3)
summary(lmm3)


n <- nrow(data) #shadetolerant
data$guild_shadetolerant_beta <-
  (data$guild_shadetolerant * (n - 1) + 0.5) / n

s2 <- glmmTMB(
  guild_shadetolerant_beta ~ forest_connectivity + forest_type + season + (1 | ID),
  family = beta_family(),
  data = data
)
summary(s2)
check_model(s2)

# lmm per month
# ------------------------------------------------------------------------------
a <- lmer(pred_var ~ forest_cover + forest_type + Month + (1|ID), data = data, REML = F)
b <- lmer(pred_var ~ forest_cover + forest_type + (1|Month) + (1|ID), data = data, REML = F)
anova(a, b)

pred_var <- data$guild_pioneer


lmm0 <- lmer(pred_var ~ 1 + (1|ID), data = data, REML = FALSE)

lmm4 <- lmer(pred_var ~ forest_cover + forest_type + Month + (1|ID), data = data)
summary(lmm4)
#isSingular(lmm4)

lmm5 <- lmer(pred_var ~ forest_connectivity + forest_type + Month + (1|ID), data = data)
summary(lmm5)
#isSingular(lmm5)

lmm6 <- lmer(pred_var ~ forest_late_ss + forest_type + Month + (1|ID), data = data)
summary(lmm6)
#isSingular(lmm6)

n <- nrow(data) #shadetolerant
data$guild_shadetolerant_beta <-
  (data$guild_shadetolerant * (n - 1) + 0.5) / n

s3 <- glmmTMB(
  guild_shadetolerant_beta ~ forest_late_ss + forest_type + Month + (1 | ID),
  family = beta_family(),
  data = data
)
summary(s3)
check_model(s3)

# lm per season
# ------------------------------------------------------------------------------
data_dry <- data %>% filter(season == "dry")
pred_var <- data_dry$guild_shadetolerant
lm_dry02 <- lm(pred_var ~ forest_cover + forest_type, data_dry)
summary(lm_dry02)
lm_dry03 <- lm(pred_var ~ forest_connectivity + forest_type, data_dry)
summary(lm_dry03)
lm_dry01 <- lm(pred_var ~ forest_late_ss + forest_type, data_dry)
summary(lm_dry01)

check_model(lm_wet02)


data_wet <- data %>% filter(season == "wet")
data_wet$guild_generalist <- log1p(data_wet$guild_generalist)
pred_var <- data_wet$dispersal_biotic
lm_wet02 <- lm(pred_var ~ forest_cover + forest_type, data_wet)
summary(lm_wet02)
lm_wet03 <- lm(pred_var ~ forest_connectivity*forest_type, data_wet)
summary(lm_wet03)
lm_wet01 <- lm(pred_var ~ forest_late_ss + forest_type, data_wet)
summary(lm_wet01)
check_model(lm_wet01)

# shadetolerant
n <- nrow(data_wet)
data_wet$guild_generalist_beta <-
  (data_wet$guild_generalist * (n - 1) + 0.5) / n

s1 <- glmmTMB(
  guild_generalist_beta ~ forest_cover + forest_type,
  family = beta_family(),
  data = data_wet
)
summary(s1)
check_model(s1)




