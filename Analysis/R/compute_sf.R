compute_sf <- function(data_folder){
  # function to compute the seed factors
  # data folder is the folder storing data
  
  # load in the seed data
  seeds <- read_excel(file.path(data_folder, "MX_FC_SEEDS_23_24.xlsx"), sheet = "Seed Data") %>% 
    filter(`Growth form` %in% c("Tree", "Shrub", "Tree/Shrub"))
  
  # calculate richness and abundance of seeds
  richness <- seeds %>%
    group_by(ID) %>%
    summarise(
      abundance = sum(as.numeric(Count_seeds), na.rm = T),
      richness = length(unique(Species)), 
      .groups = "drop")
  
  # calculate the percentage of each dispersal mode per researched plot
  dispersal <- seeds %>%
    distinct(ID, Species, .keep_all = T) %>% 
    count(ID, `Dispersal mode`, name = "dispersal_n") %>%
    group_by(ID) %>% 
    mutate(dispersal_sum = sum(dispersal_n),
           dispersal_n = (dispersal_n/dispersal_sum)) %>% 
    ungroup() %>% 
    pivot_wider(names_from = `Dispersal mode`, values_from = dispersal_n, values_fill = 0) %>% 
    rename(dispersal_abiotic = Abiotic, 
           dispersal_biotic = Biotic,
           `dispersal_a/biotic` = `Abiotic/biotic`,
           dispersal_NA = `NA`)
  
  # calculate the percentage of each guild per researched plot
  guild <- seeds %>%
    distinct(ID, Species, .keep_all = T) %>% 
    count(ID, Guild, name = "guild_n") %>%
    group_by(ID) %>% 
    mutate(guild_sum = sum(guild_n),
           guild_n = (guild_n/guild_sum)) %>% 
    ungroup() %>% 
    pivot_wider(names_from = Guild, values_from = guild_n, values_fill = 0) %>% 
    rename(guild_generalist = Generalist, 
           guild_pioneer = Pioneer, 
           guild_shadetolerant = `Shade-tolerant`,
           guild_NA = `NA`)
  
  # join all variables into one data frame
  seed_factors <- full_join(richness, dispersal)
  seed_factors <- full_join(seed_factors, guild)

  write.csv(seed_factors, file.path(data_folder, "seed_factors.csv"))
}
