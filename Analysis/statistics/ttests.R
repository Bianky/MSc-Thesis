
# T TESTS ----------------------------------------------------------------------
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

seed_attri_wf_season <- read.csv("MSc-Thesis/Analysis/data/seed/wf/11_seed_attri_season.csv")
seed_attri_df_season <- read.csv("MSc-Thesis/Analysis/data/seed/df/11_seed_attri_season.csv")

seed_attri_all_season <- full_join(seed_attri_df_season, seed_attri_wf_season) %>% 
  dplyr::select(-X)

all_attri_season <- full_join(seed_attri_all_season, forest_attri_all) %>% 
  filter(!(ID == "MX_DR_FC6")) 


data <- all_attri %>% 
  dplyr::select(ID, 
                richness, abundance, dispersal_biotic, dispersal_abiotic, 
                guild_shadetolerant, guild_pioneer, guild_generalist, forest_cover, 
                forest_connectivity, forest_early_ss, forest_late_ss, forest_type)

data_season <- all_attri_season %>% 
  dplyr::select(ID, season,
                richness, abundance, dispersal_biotic, dispersal_abiotic, 
                guild_shadetolerant, guild_pioneer, guild_generalist, forest_cover, 
                forest_connectivity, forest_early_ss, forest_late_ss, forest_type)

# t tests 

# t tests forest type
wilcox.test(abundance ~ forest_type, data = data) # yes
t.test(richness ~ forest_type, data = data) # yes
wilcox.test(dispersal_biotic ~ forest_type, data = data) # yes
wilcox.test(dispersal_abiotic ~ forest_type, data = data) # yes
wilcox.test(guild_pioneer ~ forest_type, data = data) # no
t.test(guild_generalist ~ forest_type, data = data) # no
wilcox.test(guild_shadetolerant ~ forest_type, data = data) # no

# t tests forest type
wilcox.test(forest_cover ~ forest_type, data = data) # yes
wilcox.test(forest_late_ss ~ forest_type, data = data) # yes
wilcox.test(forest_connectivity ~ forest_type, data = data) # yes

# t test season
data_wf <- data_season %>% filter(forest_type == "wet")
data_df <- data_season %>% filter(forest_type == "dry")

# t test for wet and dry season in wet forest
wilcox.test(abundance ~ season, data = data_wf) # yes
wilcox.test(richness ~ season, data = data_wf) # yes
wilcox.test(dispersal_biotic ~ season, data = data_wf) # no
wilcox.test(dispersal_abiotic ~ season, data = data_wf) # no
wilcox.test(guild_pioneer ~ season, data = data_wf) # no
wilcox.test(guild_generalist ~ season, data = data_wf) # no
wilcox.test(guild_shadetolerant ~ season, data = data_wf) # no

# t test for wet and dry season in dry forest
wilcox.test(abundance ~ season, data = data_df) # yes
wilcox.test(richness ~ season, data = data_df) # yes
wilcox.test(dispersal_biotic ~ season, data = data_df) # no
wilcox.test(dispersal_abiotic ~ season, data = data_df) # no
wilcox.test(guild_pioneer ~ season, data = data_df) # yes
wilcox.test(guild_generalist ~ season, data = data_df) # yes
wilcox.test(guild_shadetolerant ~ season, data = data_df) # yes

