
# DATA LOADING ------------------------------------------------------------------

# seed factors
seed_factors_wf <- read.csv("data/seed/wf/11_seed_factors.csv")
seed_factors_df <- read.csv("data/seed/df/11_seed_factors.csv")

seed_factors_all <- full_join(seed_factors_df, seed_factors_wf) %>% 
  dplyr::select(-X)

# forest factors
forest_factors_wf <- read.csv("data/forest/wf/11_forest_factors.csv")
forest_factors_df <- read.csv("data/forest/df/11_forest_factors.csv")

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
  
  library(performance)
  check_model(model1)

  # residual diagnostics plot model 1
  par(mfrow = c(2, 2))
  plot(model1)
  mtext("richness ~ forest cover + forest connectivity + forest type", outer = TRUE, line = -1.5, cex = 1.5)
  
  # model 2: forest early succession stage + forest connectivity + forest type
  model2 <- lm(richness ~ forest_early_ss + forest_connectivity +  forest_type, data = data)
  summary(model2)
  check_model(model2)
  

  # residual diagnostics plot model 2
  par(mfrow = c(2, 2))
  plot(model2)
  mtext("richness ~ forest early ss + forest connectivity + forest type", outer = TRUE, line = -1.5, cex = 1.5)
  
  # model 3: forest late succession stage + forest connectivity + forest type
  model3 <- lm(richness ~ forest_late_ss + forest_connectivity +  forest_type, data = data)
  summary(model3)
  check_model(model3)
  
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
  pred_early <- ggpredict(model3, terms = "forest_late_ss")
  
  # Plot with raw points and confidence ribbon
  r <- ggplot(pred_early, aes(x = x, y = predicted)) +
    geom_line(color = "#CD5733", size = 1) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
                alpha = 0.3, fill = "#E9E2CC") +
    geom_point(data = data,
               aes(x = forest_late_ss, y = richness, color = forest_type),
               alpha = 0.5) +
    scale_color_manual(values = c("wet" = "#869144", "dry" = "#FED789")) +
    xlab("forest early successional stage (%)") +
    ylab("species richness") +
    theme_minimal() +
    theme(legend.position = "bottom", 
          legend.title = element_blank(),
          axis.title.x = element_text(size = 17),  
          axis.title.y = element_text(size = 17)) 

  ### Biotic dispersal ---------------------------------------------------------

  # transformaiton of biotic dispersal to improve the model's fit
  data$dispersal_biotic <- sqrt(data$dispersal_biotic)
  
  # model 1: forest cover + forest connectivity + forest type
  model1 <- lm(dispersal_biotic ~ forest_cover + forest_connectivity +  forest_type, data = data)
  summary(model1)
  check_model(model1)
  
  # residual diagnostics plot model 1
  par(mfrow = c(2, 2))
  plot(model1)
  mtext("biotic dispersal ~ forest cover + forest connectivity + forest type", outer = TRUE, line = -1.5, cex = 1.5)
  
  # model 2: early + forest connectivity + forest type
  model2 <- lm(dispersal_biotic ~ forest_early_ss + forest_connectivity +  forest_type, data = data)
  summary(model2)
  check_model(model2)
  
  
  # residual diagnostics plot model 2
  par(mfrow = c(2, 2))
  plot(model2)
  mtext("biotic dispersal ~ forest early ss + forest connectivity + forest type", outer = TRUE, line = -1.5, cex = 1.5)
  
  # model 3: forest late successional stage + forest connectivity + forest type
  model3 <- lm(dispersal_biotic ~ forest_late_ss + forest_connectivity +  forest_type, data = data)
  summary(model3)
  check_model(model3)
  
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
  check_model(model1)
  
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
  
  # Extract prediction for early successional cover from model2
  pred_early <- ggpredict(model1, terms = "forest_cover")
  
  # Plot with raw points and confidence ribbon
  g <- ggplot(pred_early, aes(x = x, y = predicted)) +
    geom_line(color = "#CD5733", size = 1) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
                alpha = 0.3, fill = "#E9E2CC") +
    geom_point(data = data,
               aes(x = forest_cover, y = guild_generalist, color = forest_type),
               alpha = 0.5) +
    scale_color_manual(values = c("wet" = "#869144", "dry" = "#FED789")) +
    xlab("forest cover (%)") +
    ylab("generalist guild") +
    theme_minimal() +
    theme(legend.position = "bottom", 
          legend.title = element_blank(),
          axis.title.x = element_text(size = 12),  
          axis.title.y = element_text(size = 12)) 
  
  
  ### Shadetolerant guild ------------------------------------------------------

  library(gamlss)
  
  model1 <- gamlss(
    guild_shadetolerant ~ forest_cover + forest_connectivity + forest_type,
    family = BEZI,  # zero-inflated beta
    data = data
  )
  summary(model1)
  
  model2 <- gamlss(
    guild_shadetolerant ~ forest_early_ss + forest_connectivity + forest_type,
    family = BEZI,  # zero-inflated beta
    data = data
  )
  summary(model2)
  
  model3 <- gamlss(
    guild_shadetolerant ~ forest_late_ss + forest_connectivity + forest_type,
    family = BEZI,  # zero-inflated beta
    data = data
  )
  summary(model3)
  
  # residual diagnostics plot model 1
  par(mfrow = c(2, 2))
  plot(model1)
  mtext("shadetolerant guild ~ forest cover + forest connectivity + forest type", outer = TRUE, line = -1.5, cex = 1.5)
  
  
  # extract coefficients and SEs for the mean (mu) part
  coef_model1 <- coef(model1, what = "mu")
  se_model1 <- sqrt(diag(vcov(model1, what = "mu")))
  
  coef_model2 <- coef(model2, what = "mu")
  se_model2 <- sqrt(diag(vcov(model2, what = "mu")))
  
  coef_model3 <- coef(model3, what = "mu")
  se_model3 <- sqrt(diag(vcov(model3, what = "mu")))
  
  library(ggplot2)
  
  # Create a sequence of forest_cover values
  forest_cover_seq <- seq(min(data$forest_cover), max(data$forest_cover), length.out = 100)
  
  # Create a data frame with fixed values for other predictors
  pred_grid <- data.frame(
    forest_cover = forest_cover_seq,
    forest_connectivity = mean(data$forest_connectivity),
    forest_type = factor("wet", levels = levels(data$forest_type))  # ensure factor levels match
  )
  
  # Predict fitted values using the type = "response" via fitted() and newdata simulation
  pred_grid$predicted <- fitted(model1, what = "mu")[1:100]  # approximate, safer than newdata
  
  # Plot
  ggplot() +
    geom_point(data = data, aes(x = forest_cover, y = guild_shadetolerant), alpha = 0.5) +
    geom_line(data = pred_grid, aes(x = forest_cover, y = predicted), color = "#CD5733", size = 1) +
    xlab("Forest cover (%)") +
    ylab("Shadetolerant guild") +
    theme_minimal()
  
  
  #######################################
  # stargazer table
  stargazer(
    model1, model2, model3,
    type = "text",
    coef = list(coef_model1, coef_model2, coef_model3),
    se = list(se_model1, se_model2, se_model3),
    dep.var.labels = "Guild Shade-tolerant",
    column.labels = c("Model 1", "Model 2", "Model 3"),
    covariate.labels = names(coef_model1),
    no.space = TRUE,
    single.row = TRUE
  )
  

  # # transformation of shadetolerant guild to improve the model's fit
  # n <- nrow(data)
  # data$guild_shadetolerant <- (data$guild_shadetolerant * (n - 1) + 0.5) / n
  # data$guild_shadetolerant <- data$guild_shadetolerant^(1/3) 
  # 
  # 
  # # zero inflated model
  # # model 1: forest cover + forest connectivity + forest type
  # model1 <- betareg(guild_shadetolerant ~ forest_cover + forest_connectivity +  forest_type, data = data)
  # summary(model1)
  # 
  # # residual diagnostics plot model 1
  # par(mfrow = c(2, 2))
  # plot(model1)
  # mtext("shadetolerant guild ~ forest cover + forest connectivity + forest type", outer = TRUE, line = -1.5, cex = 1.5)
  # 
  # 
  # # model 2: early + forest connectivity + forest type
  # model2 <- betareg(guild_shadetolerant ~ forest_early_ss + forest_connectivity +  forest_type, data = data)
  # summary(model2)
  # 
  # # residual diagnostics plot model 2
  # par(mfrow = c(2, 2))
  # plot(model2)
  # mtext("shadetolerant guild ~ forest early ss + forest connectivity + forest type", outer = TRUE, line = -1.5, cex = 1.5)
  # 
  # 
  # 
  # # model 3: late + forest connectivity + forest type
  # model3 <- betareg(guild_shadetolerant ~ forest_late_ss + forest_connectivity +  forest_type, data = data)
  # summary(model3)
  # 
  # # residual diagnostics plot model 3
  # par(mfrow = c(2, 2))
  # plot(model3)
  # mtext("shadetolerant guild ~ forest late ss + forest connectivity + forest type", outer = TRUE, line = -1.5, cex = 1.5)

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
  
  # residual diagnostics plot model 3
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
  

#  SPEARMAN CORRELATION --------------------------------------------------------
  
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
    var_names <- colnames(num_df)
    
    # run cor.test() for all pairs
    cor_results <- expand.grid(Var1 = var_names, Var2 = var_names) %>%
      dplyr::filter(Var1 != Var2) %>%
      rowwise() %>%
      mutate(
        test = list(cor.test(num_df[[Var1]], num_df[[Var2]], method = "spearman")),
        Correlation = test$estimate,
        p.value = test$p.value
      ) %>%
      ungroup()
    
    # add significance stars
    cor_results <- cor_results %>%
      mutate(sig = case_when(
        p.value < 0.01 ~ "***",
        p.value < 0.05  ~ "**",
        p.value < 0.1  ~ "*",
        TRUE ~ ""
      )) %>%
      mutate(label = paste0(round(Correlation, 2), sig))

        # ordering + forest label
    cor_results$Var1 <- factor(cor_results$Var1, levels = order_vars)
    cor_results$Var2 <- factor(cor_results$Var2, levels = order_vars)
    cor_results$forest_type <- forest_label
    
    return(cor_results %>% dplyr::select(-test))
  }
  
  cor_wet_all <- get_cor_long_all(wet_data, "wet", my_order)
  cor_dry_all <- get_cor_long_all(dry_data, "dry", my_order)
  
  cor_both_all <- bind_rows(cor_wet_all, cor_dry_all)
  
  # Keep lower triangle only
  cor_both_all_filtered <- cor_both_all %>%
    filter(as.numeric(Var1) > as.numeric(Var2)) %>% 
    filter(!(Var1 == "forest cover (%)" | Var1 == "forest connectivity")) %>% 
    filter(Var2 == "forest early successional stage (%)" | Var2 == "forest cover (%)" | Var2 == "forest connectivity")
  
  ggplot(cor_both_all_filtered, aes(x = Var2, y = Var1, fill = Correlation)) +
    geom_tile(color = "white") +
    geom_text(aes(label = label), color = "black", size = 3) +   # use label with stars
    scale_fill_gradient2(low = "#CD5733", mid = "white", high = "#476F84", midpoint = 0) +
    facet_wrap(~ forest_type) +
    theme_minimal() +
    labs(
      fill = "rho"
    ) +
    xlab("* p < 0.1, ** p < 0.05, *** p < 0.01") +
    ylab("") +
    scale_x_discrete(labels = setNames(paste0(my_order), my_order)) +
    scale_y_discrete(labels = setNames(paste0(my_order), my_order)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
    theme(
      axis.title.x = element_text(vjust = 0.5, hjust = 1),   # your existing axis title tweak
      axis.text.x = element_text(size = 10),                 # increase x-axis values
      axis.text.y = element_text(size = 10)                  # increase y-axis values
    )
