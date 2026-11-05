
# TABLE FORMATION --------------------------------------------------------------
# ------------------------------------------------------------------------------

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

data_season <- all_attri_season %>% 
  dplyr::select(ID, 
                season,
                richness, abundance, dispersal_biotic, dispersal_abiotic, 
                guild_shadetolerant, guild_pioneer, guild_generalist, forest_cover, 
                forest_connectivity, forest_early_ss, forest_late_ss, forest_type)