
# DATA LOADING ------------------------------------------------------------------

# seed factors
seed_factors_wf <- read.csv("../../../04_data/seed/wf/11_seed_factors.csv")
seed_factors_df <- read.csv("../../../04_data/seed/df/11_seed_factors.csv")

seed_factors_all <- full_join(seed_factors_df, seed_factors_wf) %>% 
  dplyr::select(-X)

# forest factors
forest_factors_wf <- read.csv("../../../04_data/forest/wf/11_forest_factors.csv")
forest_factors_df <- read.csv("../../../04_data/forest/df/11_forest_factors.csv")

forest_factors_all <- full_join(forest_factors_df, forest_factors_wf) %>% 
  dplyr::select(-X)

# all factors
all_factors <- full_join(seed_factors_all, forest_factors_all) %>% 
  filter(!(ID == "MX_DR_FC6")) 


# DATA PREPARATION -------------------------------------------------------------

data <- all_factors %>% 
  dplyr::select(richness, dispersal_biotic, dispersal_abiotic, guild_shadetolerant, guild_pioneer, guild_generalist, forest_cover, forest_connectivity, forest_early_ss, forest_late_ss, forest_type)

# predictor variables
data$forest_cover        <- as.numeric(scale(data$forest_cover))
data$forest_early_ss     <- as.numeric(scale(data$forest_early_ss))
data$forest_late_ss      <- as.numeric(scale(data$forest_late_ss))
data$forest_connectivity <- as.numeric(scale(data$forest_connectivity))
data$forest_type         <- as.factor(data$forest_type)


# MODELS -----------------------------------------------------------------------


  ### Richness ----------------------------------------------------------------- 

  # model 1: forest cover + forest connectivity + forest type
  model1 <- lm(richness ~ forest_cover + forest_connectivity + forest_type, data = data)
  summary(model1)

  # residual diagnostics plot model 1
  par(mfrow = c(2, 2))
  plot(model1)
  mtext("richness ~ forest cover + forest connectivity + forest type", outer = TRUE, line = -1.5, cex = 1.5)
  
  # model 2: forest early succession stage + forest connectivity + forest type
  model2 <- lm(richness ~ forest_early_ss + forest_connectivity +  forest_type, data = data)
  summary(model2)

  # residual diagnostics plot model 2
  par(mfrow = c(2, 2))
  plot(model2)
  mtext("richness ~ forest early ss + forest connectivity + forest type", outer = TRUE, line = -1.5, cex = 1.5)
  
  # model 3: forest late succession stage + forest connectivity + forest type
  model3 <- lm(richness ~ forest_late_ss + forest_connectivity +  forest_type, data = data)
  summary(model3)
  
  # residual diagnostics plot model 3
  par(mfrow = c(2, 2))
  plot(model3)
  mtext("richness ~ forest late ss + forest connectivity + forest type", outer = TRUE, line = -1.5, cex = 1.5)

  
  # table of coefficients
  stargazer(model1, model2, model3, 
            type = "text",   # change to "latex" or "html" for papers
            title = "Regression results for richness",
            dep.var.labels = "Richness",
            covariate.labels = c("Forest cover", "Forest early successional stage", "Forest late successional stage", "Forest connectivity", "Forest type (wet)"),
            digits = 3)

  # Models and their predictors
  models <- list(model1 = model1, model2 = model2, model3 = model3)
  predictors <- list(
    model1 = c("forest_cover", "forest_connectivity", "forest_type"),
    model2 = c("forest_early_ss", "forest_connectivity", "forest_type"),
    model3 = c("forest_late_ss", "forest_connectivity", "forest_type")
  )
  
  # Original dataset for points (replace 'data1', 'data2', 'data3' with your datasets)
  data_list <- list(model1 = data, model2 = data, model3 = data)
  
  # Define nicer x-axis labels
  x_labels <- c(
    forest_cover       = "forest cover (%)",
    forest_connectivity = "forest connectivity",
    forest_type        = "forest type",
    forest_early_ss    = "forest early successional stage (%)",
    forest_late_ss     = "forest late successional stage (%)"
  )
  
  # Generate plots with raw data points and custom x-axis labels
  plots <- lapply(names(models), function(m) {
    lapply(predictors[[m]], function(p) {
      pred <- ggpredict(models[[m]], terms = p)
      ggplot(pred, aes(x = x, y = predicted)) +
        geom_line(color = "#CD5733", size = 1) +
        geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "#DCCB95") +
        geom_point(data = data_list[[m]], aes_string(x = p, y = "richness"), alpha = 0.5) +
        xlab(x_labels[p]) +  # use the custom label
        ylab("richness") +
        theme_minimal()
    })
  }) |> unlist(recursive = FALSE)
  
  # Combine plots in a grid
  wrap_plots(plots, ncol = 3)


  # Extract prediction for early successional cover from model2
  pred_early <- ggpredict(model2, terms = "forest_early_ss")
  
  # Plot with raw points and confidence ribbon
  r <- ggplot(pred_early, aes(x = x, y = predicted)) +
    geom_line(color = "#CD5733", size = 1) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
                alpha = 0.3, fill = "#E9E2CC") +
    geom_point(data = data,
               aes(x = forest_early_ss, y = richness, color = forest_type),
               alpha = 0.5) +
    scale_color_manual(values = c("wet" = "#869144", "dry" = "#FED789")) +
    xlab("forest early successional stage (%)") +
    ylab("species richness") +
    theme_minimal() +
    theme(legend.position = "none") 

  ### Biotic dispersal ---------------------------------------------------------

  # transformaiton of biotic dispersal to improve the model's fit
  data$dispersal_biotic <- sqrt(data$dispersal_biotic)
  
  # model 1: forest cover + forest connectivity + forest type
  model1 <- lm(dispersal_biotic ~ forest_cover + forest_connectivity +  forest_type, data = data)
  summary(model1)
  
  # residual diagnostics plot model 1
  par(mfrow = c(2, 2))
  plot(model1)
  mtext("biotic dispersal ~ forest cover + forest connectivity + forest type", outer = TRUE, line = -1.5, cex = 1.5)
  
  # model 2: early + forest connectivity + forest type
  model2 <- lm(dispersal_biotic ~ forest_early_ss + forest_connectivity +  forest_type, data = data)
  summary(model2)
  
  # residual diagnostics plot model 2
  par(mfrow = c(2, 2))
  plot(model2)
  mtext("biotic dispersal ~ forest early ss + forest connectivity + forest type", outer = TRUE, line = -1.5, cex = 1.5)
  
  # model 3: forest late successional stage + forest connectivity + forest type
  model3 <- lm(dispersal_biotic ~ forest_late_ss + forest_connectivity +  forest_type, data = data)
  summary(model3)
  
  # residual diagnostics plot model 3
  par(mfrow = c(2, 2))
  plot(model3)
  mtext("biotic dispersal ~ forest late ss + forest connectivity + forest type", outer = TRUE, line = -1.5, cex = 1.5)
  
  # table of coefficients
  stargazer(model1, model2, model3, 
            type = "text",   # change to "latex" or "html" for papers
            title = "Regression results for biotic dispersal",
            dep.var.labels = "Biotic dispersal",
            covariate.labels = c("Forest cover", "Forest early successional stage", "Forest late successional stage", "Forest connectivity", "Forest type (wet)"),
            digits = 3)
  
  # Models and their predictors
  models <- list(model1 = model1, model2 = model2, model3 = model3)
  predictors <- list(
    model1 = c("forest_cover", "forest_connectivity", "forest_type"),
    model2 = c("forest_early_ss", "forest_connectivity", "forest_type"),
    model3 = c("forest_late_ss", "forest_connectivity", "forest_type")
  )
  
  # Original dataset for points (replace 'data1', 'data2', 'data3' with your datasets)
  data_list <- list(model1 = data, model2 = data, model3 = data)
  
  # Define nicer x-axis labels
  x_labels <- c(
    forest_cover       = "forest cover (%)",
    forest_connectivity = "forest connectivity",
    forest_type        = "forest type",
    forest_early_ss    = "forest early successional stage (%)",
    forest_late_ss     = "forest late successional stage (%)"
  )
  
  # Generate plots with raw data points and custom x-axis labels
  plots <- lapply(names(models), function(m) {
    lapply(predictors[[m]], function(p) {
      pred <- ggpredict(models[[m]], terms = p)
      ggplot(pred, aes(x = x, y = predicted)) +
        geom_line(color = "#CD5733", size = 1) +
        geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "#DCCB95") +
        geom_point(data = data_list[[m]], aes_string(x = p, y = "dispersal_biotic"), alpha = 0.5) +
        xlab(x_labels[p]) +  # use the custom label
        ylab("biotic dispersal") +
        theme_minimal()
    })
  }) |> unlist(recursive = FALSE)
  
  # Combine plots in a grid
  wrap_plots(plots, ncol = 3)
  


  
  ### Abiotic dispersal --------------------------------------------------------
  
  # transformation of abiotic dispersal to improve the model's fit
  data$dispersal_abiotic <- (data$dispersal_abiotic)^2
  
  # model 1: forest cover + forest connectivity + forest type
  model1 <- lm(dispersal_abiotic ~ forest_cover + forest_connectivity +  forest_type, data = data)
  summary(model1)
  
  # residual diagnostics plot model 1
  par(mfrow = c(2, 2))
  plot(model1)
  mtext("abiotic dispersal ~ forest cover + forest connectivity + forest type", outer = TRUE, line = -1.5, cex = 1.5)
  
  # model 2: forest earlu successional stage + forest connectivity + forest type
  model2 <- lm(dispersal_abiotic ~ forest_early_ss + forest_connectivity +  forest_type, data = data)
  summary(model2)
  
  # residual diagnostics plot model 2
  par(mfrow = c(2, 2))
  plot(model2)
  mtext("abiotic dispersal ~ forest early ss + forest connectivity + forest type", outer = TRUE, line = -1.5, cex = 1.5)
  
  # model 3: forest late successional stage + forest connectivity + forest type
  model3 <- lm(dispersal_abiotic ~ forest_late_ss + forest_connectivity +  forest_type, data = data)
  summary(model3)
  
  # residual diagnostics plot model 3
  par(mfrow = c(2, 2))
  plot(model3)
  mtext("abiotic dispersal ~ forest late ss + forest connectivity + forest type", outer = TRUE, line = -1.5, cex = 1.5)
  
  # table of coefficients
  stargazer(model1, model2, model3, 
            type = "text",   # change to "latex" or "html" for papers
            title = "Regression results for abiotic dispersal",
            dep.var.labels = "Abiotic dispersal",
            covariate.labels = c("Forest cover", "Forest early successional stage", "Forest late successional stage", "Forest connectivity", "Forest type (wet)"),
            digits = 3)
  
  
  # Models and their predictors
  models <- list(model1 = model1, model2 = model2, model3 = model3)
  predictors <- list(
    model1 = c("forest_cover", "forest_connectivity", "forest_type"),
    model2 = c("forest_early_ss", "forest_connectivity", "forest_type"),
    model3 = c("forest_late_ss", "forest_connectivity", "forest_type")
  )
  
  # Original dataset for points (replace 'data1', 'data2', 'data3' with your datasets)
  data_list <- list(model1 = data, model2 = data, model3 = data)
  
  # Define nicer x-axis labels
  x_labels <- c(
    forest_cover       = "forest cover (%)",
    forest_connectivity = "forest connectivity",
    forest_type        = "forest type",
    forest_early_ss    = "forest early successional stage (%)",
    forest_late_ss     = "forest late successional stage (%)"
  )
  
  # Generate plots with raw data points and custom x-axis labels
  plots <- lapply(names(models), function(m) {
    lapply(predictors[[m]], function(p) {
      pred <- ggpredict(models[[m]], terms = p)
      ggplot(pred, aes(x = x, y = predicted)) +
        geom_line(color = "#CD5733", size = 1) +
        geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "#DCCB95") +
        geom_point(data = data_list[[m]], aes_string(x = p, y = "dispersal_abiotic"), alpha = 0.5) +
        xlab(x_labels[p]) +  # use the custom label
        ylab("abiotic dispersal") +
        theme_minimal()
    })
  }) |> unlist(recursive = FALSE)
  
  # Combine plots in a grid
  wrap_plots(plots, ncol = 3)
  
  
  ### Generalist guild ---------------------------------------------------------

  # model 1: forest cover + forest connectivity + forest type
  model1 <- lm(guild_generalist ~ forest_cover + forest_connectivity +  forest_type, data = data)
  summary(model1)
  
  # residual diagnostics plot model 1
  par(mfrow = c(2, 2))
  plot(model1)
  mtext("generalist guild ~ forest cover + forest connectivity + forest type", outer = TRUE, line = -1.5, cex = 1.5)
  
  # model 2: forest early successional stage + forest connectivity + forest type
  model2 <- lm(guild_generalist ~ forest_early_ss + forest_connectivity +  forest_type, data = data)
  summary(model2)
  
  # residual diagnostics plot model 2
  par(mfrow = c(2, 2))
  plot(model2)
  mtext("generalist guild ~ forest early ss + forest connectivity + forest type", outer = TRUE, line = -1.5, cex = 1.5)
  
  # model 3: forest late successional stage + forest connectivity + forest type
  model3 <- lm(guild_generalist ~ forest_late_ss + forest_connectivity +  forest_type, data = data)
  summary(model3)
  
  # residual diagnostics plot model 3
  par(mfrow = c(2, 2))
  plot(model3)
  mtext("generalist guild ~ forest late ss + forest connectivity + forest type", outer = TRUE, line = -1.5, cex = 1.5)

  # table of coefficients
  stargazer(model1, model2, model3, 
            type = "text",   # change to "latex" or "html" for papers
            title = "Regression results for generalist guild",
            dep.var.labels = "Generalist guild",
            covariate.labels = c("Forest cover", "Forest early successional stage", "Forest late successional stage", "Forest connectivity", "Forest type (wet)"),
            digits = 3)
  
  
  # Models and their predictors
  models <- list(model1 = model1, model2 = model2, model3 = model3)
  predictors <- list(
    model1 = c("forest_cover", "forest_connectivity", "forest_type"),
    model2 = c("forest_early_ss", "forest_connectivity", "forest_type"),
    model3 = c("forest_late_ss", "forest_connectivity", "forest_type")
  )
  
  # Original dataset for points (replace 'data1', 'data2', 'data3' with your datasets)
  data_list <- list(model1 = data, model2 = data, model3 = data)
  
  # Define nicer x-axis labels
  x_labels <- c(
    forest_cover       = "forest cover (%)",
    forest_connectivity = "forest connectivity",
    forest_type        = "forest type",
    forest_early_ss    = "forest early successional stage (%)",
    forest_late_ss     = "forest late successional stage (%)"
  )
  
  # Generate plots with raw data points and custom x-axis labels
  plots <- lapply(names(models), function(m) {
    lapply(predictors[[m]], function(p) {
      pred <- ggpredict(models[[m]], terms = p)
      ggplot(pred, aes(x = x, y = predicted)) +
        geom_line(color = "#CD5733", size = 1) +
        geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "#DCCB95") +
        geom_point(data = data_list[[m]], aes_string(x = p, y = "guild_generalist"), alpha = 0.5) +
        xlab(x_labels[p]) +  # use the custom label
        ylab("generalist guild") +
        theme_minimal()
    })
  }) |> unlist(recursive = FALSE)
  
  # Combine plots in a grid
  wrap_plots(plots, ncol = 3)
  
  
  ### Shadetolerant guild ------------------------------------------------------

  # transformation of shadetolerant guild to improve the model's fit
  n <- nrow(data)
  data$guild_shadetolerant <- (data$guild_shadetolerant * (n - 1) + 0.5) / n
  data$guild_shadetolerant <- data$guild_shadetolerant^(1/3) 

  # zero inflated model
  # model 1: forest cover + forest connectivity + forest type
  model1 <- betareg(guild_shadetolerant ~ forest_cover + forest_connectivity +  forest_type, data = data)
  summary(model1)
  
  # residual diagnostics plot model 1
  par(mfrow = c(2, 2))
  plot(model1)
  mtext("shadetolerant guild ~ forest cover + forest connectivity + forest type", outer = TRUE, line = -1.5, cex = 1.5)
  
  # model 2: early + forest connectivity + forest type
  model2 <- betareg(guild_shadetolerant ~ forest_early_ss + forest_connectivity +  forest_type, data = data)
  summary(model2)
  
  # residual diagnostics plot model 2
  par(mfrow = c(2, 2))
  plot(model2)
  mtext("shadetolerant guild ~ forest early ss + forest connectivity + forest type", outer = TRUE, line = -1.5, cex = 1.5)
  
  # model 3: late + forest connectivity + forest type
  model3 <- betareg(guild_shadetolerant ~ forest_late_ss + forest_connectivity +  forest_type, data = data)
  summary(model3)
  
  # residual diagnostics plot model 3
  par(mfrow = c(2, 2))
  plot(model3)
  mtext("shadetolerant guild ~ forest late ss + forest connectivity + forest type", outer = TRUE, line = -1.5, cex = 1.5)

  # table of coefficients
  stargazer(model1, model2, model3, 
            type = "text",   # change to "latex" or "html" for papers
            title = "Regression results for shadetolerant guild",
            dep.var.labels = "Shadetolerant guild",
            covariate.labels = c("Forest cover", "Forest early successional stage", "Forest late successional stage", "Forest connectivity", "Forest type (wet)"),
            digits = 3)
  

  
  # Models and their predictors
  models <- list(model1 = model1, model2 = model2, model3 = model3)
  predictors <- list(
    model1 = c("forest_cover", "forest_connectivity", "forest_type"),
    model2 = c("forest_early_ss", "forest_connectivity", "forest_type"),
    model3 = c("forest_late_ss", "forest_connectivity", "forest_type")
  )
  
  # Original dataset for points (replace 'data1', 'data2', 'data3' with your datasets)
  data_list <- list(model1 = data, model2 = data, model3 = data)
  
  # Define nicer x-axis labels
  x_labels <- c(
    forest_cover       = "forest cover (%)",
    forest_connectivity = "forest connectivity",
    forest_type        = "forest type",
    forest_early_ss    = "forest early successional stage (%)",
    forest_late_ss     = "forest late successional stage (%)"
  )
  
  # Generate plots with raw data points and custom x-axis labels
  plots <- lapply(names(models), function(m) {
    lapply(predictors[[m]], function(p) {
      pred <- ggpredict(models[[m]], terms = p)
      ggplot(pred, aes(x = x, y = predicted)) +
        geom_line(color = "#CD5733", size = 1) +
        geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "#DCCB95") +
        geom_point(data = data_list[[m]], aes_string(x = p, y = "guild_shadetolerant"), alpha = 0.5) +
        xlab(x_labels[p]) +  # use the custom label
        ylab("shadetolerant guild") +
        theme_minimal()
    })
  }) |> unlist(recursive = FALSE)
  
  # Combine plots in a grid
  wrap_plots(plots, ncol = 3)
  

  
  ### Pioneer guild ------------------------------------------------------------

  # transformation of pioneer guild to improve the model's fit
  data$guild_pioneer <- log(data$guild_pioneer + 1) 
  
  # model 1: forest cover + forest connectivity + forest type
  model1 <- lm(guild_pioneer ~ forest_cover + forest_connectivity +  forest_type, data = data)
  summary(model1)
  
  # residual diagnostics plot model 1
  par(mfrow = c(2, 2))
  plot(model1)
  mtext("pioneer guild ~ forest cover + forest connectivity + forest type", outer = TRUE, line = -1.5, cex = 1.5)
  
  # model 2: forest early successional stage + forest connectivity + forest type
  model2 <- lm(guild_pioneer ~ forest_early_ss + forest_connectivity +  forest_type, data = data)
  summary(model2)
  
  # residual diagnostics plot model 2
  par(mfrow = c(2, 2))
  plot(model2)
  mtext("pioneer guild ~ forest early ss + forest connectivity + forest type", outer = TRUE, line = -1.5, cex = 1.5)
  
  # model 3: forest late successional stage + forest connectivity + forest type
  model3 <- lm(guild_pioneer ~ forest_late_ss + forest_connectivity +  forest_type, data = data)
  summary(model3)
  
  y# residual diagnostics plot model 3
  par(mfrow = c(2, 2))
  plot(model3)
  mtext("pioneer ~ forest late ss + forest connectivity + forest type", outer = TRUE, line = -1.5, cex = 1.5)
  
  # table of coefficients
  stargazer(model1, model2, model3, 
            type = "text",   # change to "latex" or "html" for papers
            title = "Regression results for pioneer guild",
            dep.var.labels = "Pioneer guild",
            covariate.labels = c("Forest cover", "Forest early successional stage", "Forest late successional stage", "Forest connectivity", "Forest type (wet)"),
            digits = 3)
  

  
  # Models and their predictors
  models <- list(model1 = model1, model2 = model2, model3 = model3)
  predictors <- list(
    model1 = c("forest_cover", "forest_connectivity", "forest_type"),
    model2 = c("forest_early_ss", "forest_connectivity", "forest_type"),
    model3 = c("forest_late_ss", "forest_connectivity", "forest_type")
  )
  
  # Original dataset for points (replace 'data1', 'data2', 'data3' with your datasets)
  data_list <- list(model1 = data, model2 = data, model3 = data)
  
  # Define nicer x-axis labels
  x_labels <- c(
    forest_cover       = "forest cover (%)",
    forest_connectivity = "forest connectivity",
    forest_type        = "forest type",
    forest_early_ss    = "forest early successional stage (%)",
    forest_late_ss     = "forest late successional stage (%)"
  )
  
  # Generate plots with raw data points and custom x-axis labels
  plots <- lapply(names(models), function(m) {
    lapply(predictors[[m]], function(p) {
      pred <- ggpredict(models[[m]], terms = p)
      ggplot(pred, aes(x = x, y = predicted)) +
        geom_line(color = "#CD5733", size = 1) +
        geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "#DCCB95") +
        geom_point(data = data_list[[m]], aes_string(x = p, y = "guild_pioneer"), alpha = 0.5) +
        xlab(x_labels[p]) +  # use the custom label
        ylab("pioneer guild") +
        theme_minimal()
    })
  }) |> unlist(recursive = FALSE)
  
  # Combine plots in a grid
  wrap_plots(plots, ncol = 3)
  

#  PEARSON CORRELATION ---------------------------------------------------------
  
  df <- all_factors %>% rename("forest early successional stage (%)" = "forest_early_ss",
                               "forest late successional stage (%)" = "forest_late_ss",
                               "forest cover (%)" = "forest_cover",
                               "forest connectivity" = "forest_connectivity",
                               "richness" = "richness",
                               "abiotic dispersal (%)" = "dispersal_abiotic",
                               "biotic dispersal (%)" = "dispersal_biotic",
                               "generalist (%)" = "guild_generalist",
                               "pioneer (%)" = "guild_pioneer",
                               "shadetolerant (%)" = "guild_shadetolerant") 
  
  df <- df %>% dplyr::select("forest early successional stage (%)",
                             "forest late successional stage (%)",
                             "forest cover (%)",
                             "forest connectivity",
                             "richness",
                             "abiotic dispersal (%)",
                             "biotic dispersal (%)",
                             "generalist (%)",
                             "pioneer (%)",
                             "shadetolerant (%)",
                             "forest_type")
  
  exp <- c( "forest early successional stage (%)",
            "forest late successional stage (%)",
            "forest cover (%)",
            "forest connectivity")
  
  res <- c("richness",
           "abiotic dispersal (%)",
           "biotic dispersal (%)",
           "generalist (%)",
           "pioneer (%)",
           "shadetolerant (%)")
  
  
  wet_data <- df %>% filter(forest_type == "wet")
  dry_data <- df %>% filter(forest_type == "dry")
  
  # correlation matrix for all numeric variables within wet/dry
  my_order <- c("forest early successional stage (%)",
                "forest late successional stage (%)",
                "forest cover (%)",
                "forest connectivity",
                "richness",
                "biotic dispersal (%)",
                "abiotic dispersal (%)",
                "generalist (%)",
                "pioneer (%)",
                "shadetolerant (%)")
  
  
  get_cor_long_all <- function(df, forest_label, order_vars) {
    num_df <- df %>% dplyr::select(where(is.numeric))
    cor_mat <- cor(num_df, use = "pairwise.complete.obs")
    
    cor_long <- as.data.frame(as.table(cor_mat))
    colnames(cor_long) <- c("Var1", "Var2", "Correlation")
    
    # apply ordering
    cor_long$Var1 <- factor(cor_long$Var1, levels = order_vars)
    cor_long$Var2 <- factor(cor_long$Var2, levels = order_vars)
    
    cor_long$forest_type <- forest_label
    return(cor_long)
  }
  
  # apply to wet and dry
  cor_wet_all <- get_cor_long_all(wet_data, "wet", my_order)
  cor_dry_all <- get_cor_long_all(dry_data, "dry", my_order)
  
  cor_both_all <- bind_rows(cor_wet_all, cor_dry_all)
  
  # plot heatmap
  ggplot(cor_both_all, aes(x = Var2, y = Var1, fill = Correlation)) +
    geom_tile(color = "white") +
    geom_text(aes(label = round(Correlation, 2)), color = "black", size = 3) +
    scale_fill_gradient2(low = "#DCCB95", mid = "white", high = "#CD5733", midpoint = 0) +
    facet_wrap(~ forest_type) +
    theme_minimal() +
    labs(
      title = "Correlation matrix",
      fill = "Pearson r"
    ) +
    xlab("") +
    ylab("") +
    # color the axis labels
    scale_x_discrete(labels = setNames(
      paste0(my_order), my_order
    )) +
    scale_y_discrete(labels = setNames(
      paste0(my_order), my_order
    )) +
    theme(
      axis.text.x = element_text(color = ifelse(my_order %in% exp, "black", "brown4"), angle = 45, hjust = 1, vjust = 1),
      axis.text.y = element_text(color = ifelse(my_order %in% exp, "black", "brown4"))
    )
  
  
