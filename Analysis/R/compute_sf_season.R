compute_sf_season <- function(data_folder){
  # function to compute the seed attributes
  # data folder is the folder storing data
  
  # load in the seed data
  seed <- read_excel(file.path(data_folder, "MX_FC_SEEDS_23_24.xlsx"), sheet = "Seed Data") %>% 
    filter(`Growth form` %in% c("Tree", "Shrub", "Tree/Shrub"))
  
  seeds <- seed %>% 
    mutate(season = ifelse(`Forest type` == "Wet", 
                           ifelse(Month %in% c("February", "March", "April"), "dry", "wet"), 
                           ifelse(Month %in% c("May", "June", "July", "August", "September", "October"), "wet", "dry")))
  
  
  # calculate richness and abundance of seeds
  richness <- seeds %>%
    group_by(ID, season) %>%
    summarise(
      abundance = sum(as.numeric(Count_seeds), na.rm = T),
      richness = length(unique(Species)), 
      .groups = "drop")
  
  # calculate the percentage of each dispersal mode per researched plot
  dispersal <- seeds %>%
    distinct(ID, season, Species, .keep_all = T) %>% 
    count(ID, season, `Dispersal mode`, name = "dispersal_n") %>%
    group_by(ID, season) %>% 
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
    distinct(ID, season, Species, .keep_all = T) %>% 
    count(ID, season, Guild, name = "guild_n") %>%
    group_by(ID, season) %>% 
    mutate(guild_sum = sum(guild_n),
           guild_n = (guild_n/guild_sum)) %>% 
    ungroup() %>% 
    pivot_wider(names_from = Guild, values_from = guild_n, values_fill = 0) %>% 
    rename(guild_generalist = Generalist, 
           guild_pioneer = Pioneer, 
           guild_shadetolerant = `Shade-tolerant`,
           guild_NA = `NA`)
  
  # join all variables into one data frame
  seed_attri <- full_join(richness, dispersal, by = c("ID", "season"))
  seed_attri <- full_join(seed_attri, guild, by = c("ID", "season"))
  
  write.csv(seed_attri, file.path(data_folder, "11_seed_attri_season.csv"))
}
