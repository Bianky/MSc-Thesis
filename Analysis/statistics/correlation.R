
# CORRELATION ------------------------------------------------------------------
# ------------------------------------------------------------------------------

# DATA LOADING ------------------------------------------------------------------

# seed attributes
seed_attri_wf <- read.csv("MSc-Thesis/Analysis/data/seed/wf/11_seed_attri.csv")
seed_attri_df <- read.csv("MSc-Thesis/Analysis/data/seed/df/11_seed_attri.csv")

seed_attri_all <- full_join(seed_attri_df, seed_attri_wf) %>% 
  dplyr::select(-X)

# forest attributes
forest_attri_wf <- read.csv("MSc-Thesis/Analysis/data/forest/wf/11_forest_attri.csv")
forest_attri_df <- read.csv("MSc-Thesis/Analysis/data/forest/df/11_forest_attri.csv")

forest_attri_all <- full_join(forest_attri_df, forest_attri_wf) %>% 
  dplyr::select(-X)

# all attributes
all_attri <- full_join(seed_attri_all, forest_attri_all) %>% 
  filter(!(ID == "MX_DR_FC6")) 


# DATA PREPARATION -------------------------------------------------------------
data <- all_attri %>% 
  dplyr::select(ID, 
                richness, abundance, dispersal_biotic, dispersal_abiotic, 
                guild_shadetolerant, guild_pioneer, guild_generalist, forest_cover, 
                forest_connectivity, forest_early_ss, forest_late_ss, forest_type)


# correlation
num_data <- data[sapply(data, is.numeric)]
result <- rcorr(as.matrix(num_data), type = "spearman")

r <- result$r
p <- result$P

r_sig <- r
r_sig[p >= 0.05] <- NA

corrplot(
  r_sig,
  method = "color",
  type = "upper",
  addCoef.col = "black",
  na.label = " "
)

