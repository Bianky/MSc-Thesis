
# T TESTS ----------------------------------------------------------------------
# ------------------------------------------------------------------------------

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

