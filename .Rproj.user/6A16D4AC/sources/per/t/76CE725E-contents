source("Statcast Scraper.R")
library(mgcv)
library(xgboost)
library(foreach)
library(SHAPforxgboost)
library(parallel)


# Feature Engineering ----------------------------------------------------------------
feature_engineering <- function(df) {
  
  # Mapping system
  pitch_mapping <- list(
    
    # Fastballs
    "4-Seam Fastball" = "Fastball",
    "Sinker" = "Fastball",
    
    # Breaking Balls
    "Cutter" = "Breaking Ball",
    "Slider" = "Breaking Ball",
    "Sweeper" = "Breaking Ball",
    "Curveball" = "Breaking Ball",
    "Screwball" = "Breaking Ball",
    "Knuckle Curve" = "Breaking Ball",
    "Slurve" = "Breaking Ball",
    
    # Offspeed
    "Changeup" = "Offspeed",
    "Split-Finger" = "Offspeed",
    "Forkball" = "Offspeed",
    
    # Others
    #"Screwball" = "Other",
    "Eephus" = "Other",
    "Knuckleball" = "Other",
    "Other" = "Other",
    "Pitch Out" = "Other",
    "Slow Curve" = "Other"
  )
  
  
  # Map pitch_name to cleaned pitch type
  df <- df %>%
    mutate(
      pitch_class = recode(pitch_name, !!!pitch_mapping)
      ) %>%
    filter(pitch_class != "Other")
  
  
  df <- df %>%
    mutate(
      bat_side = as.factor(bat_side),
      pitch_hand = as.factor(pitch_hand),
      pitch_name = as.factor(pitch_name),
      pitch_type = as.factor(pitch_type),
      pitch_class = as.factor(pitch_class),
      arm_angle = as.numeric(arm_angle),
      effective_speed = as.numeric(effective_speed)
      )
  
  
  
  # Function to fit and predict effective speed
  fit_effective_speed <- function(data) {
    tryCatch({
      model <- gam(effective_speed ~ s(release_speed_mph, k = 5) + 
                     s(extension, k = 5), 
                   data = data)
      data %>%
        mutate(effective_speed = if_else(is.na(effective_speed), 
                                         predict(model, newdata = .), 
                                         effective_speed))
    }, error = function(e) data)
  }
  
  
  # Function to fit and predict spin rate
  fit_spin_rate <- function(data) {
    tryCatch({
      model <- gam(release_spin_rate ~ s(release_speed_mph, k = 5) + 
                     s(release_pos_x, k = 5) + s(release_pos_z, k = 5) + 
                     s(horz_break) + s(induced_vert_break), 
                   data = data)
      data %>%
        mutate(release_spin_rate = if_else(is.na(release_spin_rate), 
                                           predict(model, newdata = .), 
                                           release_spin_rate))
    }, error = function(e) data)
  }
  
  
  # Function to fit and predict arm angle
  fit_arm_angle <- function(data) {
    tryCatch({
      # Attempt to fit a GAM model
      model <- gam(arm_angle ~ s(pfx_x, k = 5) + s(pfx_z, k = 5), data = data)
      data %>%
        mutate(arm_angle = if_else(is.na(arm_angle), 
                                   predict(model, newdata = .), 
                                   arm_angle))
    }, error = function(e) data)
    
  }
  
  
  # Function to fit and predict spin axis
  fit_spin_axis <- function(data) {
    tryCatch({
      model <- gam(spin_axis ~ s(release_pos_x, k = 5) + s(release_pos_z, k = 5) +
                     s(pfx_x, k = 5) + s(pfx_z, k = 5), 
                   data = data)
      data %>%
        mutate(spin_axis = if_else(is.na(spin_axis), 
                                   predict(model, newdata = .), 
                                   spin_axis))
    }, error = function(e) data)
  }
  
  
  
  # Apply models within each group
  df <- df %>%
    group_by(pitcher_id, year, pitch_type) %>%
    group_split() %>%
    lapply(fit_effective_speed) %>%
    lapply(fit_spin_rate) %>%
    lapply(fit_arm_angle) %>%
    lapply(fit_spin_axis) %>%
    bind_rows() %>%
    as.data.frame() %>%
    group_by(pitcher_id, year) %>%
    group_split() %>%
    lapply(fit_effective_speed) %>%
    lapply(fit_spin_rate) %>%
    lapply(fit_arm_angle) %>%
    lapply(fit_spin_axis) %>%
    bind_rows() %>%
    as.data.frame() %>%
    group_by(pitch_type, year) %>%
    group_split() %>%
    lapply(fit_effective_speed) %>%
    lapply(fit_spin_rate) %>%
    lapply(fit_arm_angle) %>%
    lapply(fit_spin_axis) %>%
    bind_rows() %>%
    as.data.frame() %>%
    arrange(game_date, game_id, inning, at_bat_number, pitch_number)
  
  
  # Filter out rows with NA values in the specified columns
  columns_to_check <- c(
    "pitch_type", "pitch_name", "release_speed_mph", "effective_speed", 
    "release_spin_rate", "release_pos_x", "release_pos_y", "release_pos_z", 
    "extension", "arm_angle", "ax", "ay", "az", "vx0", "vy0", "vz0", 
    "spin_axis", "pfx_x", "pfx_z", "api_break_x_arm", 
    "api_break_x_batter_in", "api_break_z_with_gravity", 
    "plate_x", "plate_z", "VAA", "HAA", 
    "strike_zone_top", "strike_zone_bottom", "zone", "delta_run_exp"
  )
  
  df <- df %>%
    filter(if_all(all_of(columns_to_check), ~ !is.na(.)))
  
  
  # Adjusting VAA for Pitch Type and Pitch Height (VAAAA)
  df <- df %>%
    group_by(pitch_name) %>%
    group_modify(~ {
      vaa_model <- lm(VAA ~ plate_z, data = .x)
      .x %>%
        mutate(VAAAA = resid(vaa_model))
      }) %>%
    ungroup()
  
  
  # Adjusting HAA for Pitch Type, Location, Handedness and Release Position (HAAAA)
  df <- df %>%
    group_by(pitch_name) %>%
    group_modify(~ {
      # Exclude pitch_hand for pitch types with only 1 pitcher handedness
      if (n_distinct(.x$pitch_hand) < 2) {
        haa_model <- lm(HAA ~ plate_x + release_pos_x, data = .x)
      } else {
        haa_model <- lm(HAA ~ plate_x + pitch_hand + release_pos_x, data = .x)
      }
      .x %>% 
        mutate(HAAAA = resid(haa_model))
    }) %>%
    ungroup()
  
  
  # Mirror for left-handed pitchers
  df <- df %>%
    mutate(ax = ifelse(pitch_hand == "L", -ax, ax),
           pfx_x = ifelse(pitch_hand == "L", -pfx_x, pfx_x),
           release_pos_x = ifelse(pitch_hand == "L", -release_pos_x , release_pos_x))
  
  
  # Mean Run Expectancy for each pitch result by count (delta_run_exp_mean Variable)
  df <- df %>%
    group_by(pitch_result, strikes, balls) %>%
    mutate(delta_run_exp_mean = mean(delta_run_exp, na.rm = TRUE), 
           .groups = "drop")
  
  
  # Define primary pitch based on highest usage by batter handedness and year
  df_primary <- df %>%
    group_by(pitcher_id, year, bat_side, pitch_type) %>%
    summarise(
      pitch_count = n(),
      avg_release_speed = mean(release_speed_mph, na.rm = TRUE),
      avg_az = mean(az, na.rm = TRUE),
      avg_ax = mean(ax, na.rm = TRUE),
      avg_ivb = mean(pfx_z, na.rm = TRUE),
      avg_hb = mean(api_break_x_arm, na.rm = TRUE),
      avg_arm_angle = mean(arm_angle, na.rm = TRUE),
      .groups = "drop"
      ) %>%
    group_by(pitcher_id, year, bat_side) %>%
    mutate(
      usage = pitch_count / sum(pitch_count)
      ) %>%
    ungroup() %>%
    arrange(desc(usage)) %>%
    distinct(pitcher_id, year, bat_side, .keep_all = TRUE)
  
  
  # Merge primary pitch data back into the main DataFrame
  df <- df %>%
    left_join(df_primary, by = c("pitcher_id", "year", "bat_side")) %>%
    rename(pitch_type = pitch_type.x,
           primary_pitch_type = pitch_type.y)
  
  
  # Calculate pitch differentials compared to primary pitch
  df <- df %>%
    mutate(is_primary = if_else(primary_pitch_type == pitch_type, 1, 0)) %>%
    mutate(
      release_speed_diff = if_else(is_primary == 1, 0, 
                                   release_speed_mph - avg_release_speed),
      az_diff = if_else(is_primary == 1, 0, az - avg_az),
      ax_diff = if_else(is_primary == 1, 0, abs(ax - avg_ax)),
      ivb_diff = if_else(is_primary == 1, 0, pfx_z - avg_ivb),
      hb_diff = if_else(is_primary == 1, 0, abs(api_break_x_arm - avg_hb)),
      arm_angle_diff = if_else(is_primary == 1, 0, arm_angle - avg_arm_angle)
    )
  
  
  
  # Replace missing values for pitchers without a primary pitch defined
  df <- df %>%
    group_by(pitcher_id, year) %>%
    mutate(
      avg_release_speed = ifelse(is.na(avg_release_speed),
                                 max(start_speed, na.rm = TRUE), 
                                 avg_release_speed),
      avg_az = ifelse(is.na(avg_az), 
                      max(az, na.rm = TRUE), 
                      avg_az),
      avg_ax = ifelse(is.na(avg_ax), 
                      max(ax, na.rm = TRUE), 
                      avg_ax),
      avg_ivb = ifelse(is.na(avg_ivb), 
                       max(pfx_z, na.rm = TRUE), 
                       avg_ivb),
      avg_hb = ifelse(is.na(avg_hb), 
                      max(hb, na.rm = TRUE), 
                      avg_hb),
      avg_arm_angle = ifelse(is.na(avg_arm_angle), 
                             max(arm_angle, na.rm = TRUE), 
                             avg_arm_angle)) %>%
    ungroup() %>%
    as.data.frame() 
  
  
  # Final cleaning
  df <- df %>%
    relocate(pitch_name, .after = pitch_number) %>%
    relocate(pitch_class, .after = pitch_type) %>%
    relocate(VAAAA, .after = VAA) %>%
    relocate(HAAAA, .after = HAA) %>%
    arrange(game_date, game_id, inning, at_bat_number, pitch_number) %>%
    select(-.groups)
  
  return(df)
  
  }




data_2024 <- statcast_scraper(start_date = "2024-01-01", 
                              end_date = "2024-12-31") %>% feature_engineering()

data_2024 <- statcast_scraper(start_date = "2024-01-01", end_date = "2024-12-31")
data_2023 <- statcast_scraper(start_date = "2023-01-01", end_date = "2023-12-31")
data_2022 <- statcast_scraper(start_date = "2022-01-01", end_date = "2022-12-31")
data_2021 <- statcast_scraper(start_date = "2021-01-01", end_date = "2021-12-31")
data_2020 <- statcast_scraper(start_date = "2020-01-01", end_date = "2020-12-31")

data_all <- rbind(data_2020, data_2021, data_2022, data_2023, data_2024) %>%
  feature_engineering()

data_2019 <- statcast_scraper(start_date = "2019-01-01", end_date = "2019-12-31")
data_2018 <- statcast_scraper(start_date = "2018-01-01", end_date = "2018-12-31")
data_2017 <- statcast_scraper(start_date = "2017-01-01", end_date = "2017-12-31")
data_2016 <- statcast_scraper(start_date = "2016-01-01", end_date = "2016-12-31")
data_2015 <- statcast_scraper(start_date = "2015-01-01", end_date = "2015-12-31")



data_2024 %>%
  group_by(pitch_result) %>%
  summarise(n = n())

data_2024 %>%
  group_by(pitch_name) %>%
  summarise(n = n()) %>%
  as.data.frame()



# VAAAA and HAAAA Leaders
data_all %>%
  filter(primary_pitch_type != "EP", pitch_type == "CH", pitch_hand == "R") %>%
  group_by(pitcher_name, pitch_type, year) %>%
  summarise(HAA = mean(HAA),
            HAAAA = mean(HAAAA)) %>%
  arrange(desc(HAAAA)) %>%
  as.data.frame()

data_all %>%
  filter(primary_pitch_type != "EP", pitch_type == "FF", pitch_hand == "L") %>%
  group_by(pitcher_name, pitch_type, year) %>%
  summarise(VAA = mean(VAA),
            VAAAA = mean(VAAAA)) %>%
  arrange(desc(VAAAA)) %>%
  as.data.frame()




# Create the summary table
data_2024 %>%
  group_by(pitch_name, pitch_hand) %>%
  summarise(
    n = n(),
    across(all_of(c(
      "release_speed_mph", "effective_speed", "release_spin_rate",
      "release_pos_x", "release_pos_z", "extension", "arm_angle", 
      "ax", "ay", "az", "vx0", "vy0", "vz0", "spin_axis",
      "pfx_x", "pfx_z", "api_break_x_arm", "VAA", "HAA"#,
      #"release_speed_diff", "az_diff", "ax_diff", "ivb_diff", "hb_diff", 
      #"arm_angle_diff"
      )),
      list(mean = ~ mean(.x, na.rm = TRUE)
           #,
           #sd = ~ sd(.x, na.rm = TRUE)
           ),
      .names = "{.col}_{.fn}"),
    .groups = "drop") %>%
  mutate(
    across(ends_with("_mean"), ~ round(.x, 2))
    #,
    #across(ends_with("_sd"), ~ round(.x, 2)
    ) %>%
  as.data.frame() %>%
  write.csv("pitch_summary.csv")





filled_data %>%
  #filter(pitcher_name == "Kloffenstein, Adam", pitch_name == "Curveball") %>%
  filter(pitcher_name == "Montas, Frankie") %>%
  group_by(pitch_name, bat_side, fielding_team) %>%
  summarise(n = n(),
            avg_arm_angle = mean(arm_angle),
            avg_rpm = mean(release_spin_rate),
            avg_api_break_z = mean(api_break_z_with_gravity)*12,
            avg_pfx_z = mean(pfx_z)*12, #vertical movement w/o gravity
            avg_vert_break = mean(vert_break),
            avg_induced_vert_break = mean(induced_vert_break)) %>%
  arrange(pitch_name, fielding_team) %>%
  as.data.frame()
  #fit_arm_angle() %>%



data_all %>%
  group_by(pitch_class) %>%
  summarise(RV_per_100 = mean(delta_run_exp_mean)*100) %>%
  as.data.frame()




# Features to Include
# release_speed_mph
# release_spin_rate
# extension
# ax
# az
# ax_diff
# az_diff
# release_speed_diff
# arm_angle_diff
# VAAAA
# HAAAA
# is_primary

# Group by pitch_class






## Splitting Data --------------------------------------------------------------------

# Train
train_fb <- data_all %>%
  filter(year %in% c(2020, 2021, 2022)) %>%
  filter(pitch_class == "Fastball")

train_bb <- data_all %>%
  filter(year %in% c(2020, 2021, 2022)) %>%
  filter(pitch_class == "Breaking Ball")

train_off <- data_all %>%
  filter(year %in% c(2020, 2021, 2022)) %>%
  filter(pitch_class == "Offspeed")


# Test
test_fb <- data_all %>%
  filter(year %in% c(2023, 2024)) %>%
  filter(pitch_class == "Fastball")

test_bb <- data_all %>%
  filter(year %in% c(2023, 2024)) %>%
  filter(pitch_class == "Breaking Ball")

test_off <- data_all %>%
  filter(year %in% c(2023, 2024)) %>%
  filter(pitch_class == "Offspeed")


features <- c("release_speed_mph", "release_spin_rate", "extension",
              "ax", "az", "release_speed_diff", "ax_diff", "az_diff",
              "arm_angle_diff", "VAAAA", "HAAAA", "is_primary")


# Set XGBoost parameters
params <- list(
  booster = "gbtree",
  objective = "reg:squarederror",
  eval_metric = "rmse",   
  max_depth = 10,
  eta = 0.3,
  nthread = -1,
  subsample = 0.8,
  colsample_bytree = 0.8,
  min_child_weight = 0
  )










## Fastball Model --------------------------------------------------------------------

dtrain_fb <- xgb.DMatrix(data = model.matrix(~ . - 1, data = train_fb[, features]), 
                         label = train_fb$delta_run_exp_mean)


# Train XGBoost model
xgb_model_fb <- xgb.train(
  params = params,
  data = dtrain_fb,
  nrounds = 250,
  watchlist = list(train = dtrain_fb),
  print_every_n = 50
  )


# Feature Importance
importance_fb <- xgb.importance(feature_names = colnames(
  model.matrix(~ . - 1, data = train_fb[, features])),
  model = xgb_model_fb)
xgb.plot.importance(importance_fb)


# Hyperparameter Tuning
param_grid_fb <- expand.grid(
  nrounds = c(50, 100, 250),            # Reduced range for boosting rounds
  eta = c(0.3, 0.6, 0.9),               # Coarser learning rate grid
  max_depth = c(1, 6, 12),              # Key depths, avoiding very deep trees
  colsample_bytree = c(0.2, 0.6, 1.0),  # Coarser grid for feature subsampling
  subsample = c(0.2, 0.6, 1),           # Focused range for row subsampling
  min_child_weight = c(1, 10, 50),      # Coarse grid for minimum child weight
  lambda = c(1),                        # Fixing L2 regularization
  alpha = c(1)                          # Fixing L1 regularization
  )




# Detect number of CPU cores
num_cores <- parallel::detectCores()

# Create a cluster for parallel grid search
cluster <- parallel::makeCluster(num_cores)
clusterEvalQ(cluster, library(xgboost))  # Load XGBoost on all worker nodes

# Randomly select 50 combinations
sampled_grid <- param_grid_fb[sample(nrow(param_grid_fb), 600), ]

train_fb_sub <- train_fb[sample(1:nrow(train_fb), 100000), ]

# Export necessary objects to worker nodes
clusterExport(cluster, c("sampled_grid", "train_fb_sub", "num_cores", "features"))

# Define function to train each model in parallel
run_xgb_cv <- function(i) {
  params_tuned_fb <- as.list(sampled_grid[i, ])
  params_tuned_fb <- append(params_tuned_fb, list(
    booster = "gbtree",
    objective = "reg:squarederror",
    eval_metric = "rmse",
    nthread = num_cores  # Use all CPU cores for training each model
  ))
  
  # Re-create training data inside worker node
  dtrain_fb_sub <- xgb.DMatrix(
    data = model.matrix(~ . - 1, data = train_fb_sub[, features]),
    #data = model.matrix(~ . - 1, data = train_fb[, features]),
    label = train_fb_sub$delta_run_exp_mean)
  
  # Run XGBoost cross-validation
  cv_result <- xgb.cv(
    params = params_tuned_fb,
    data = dtrain_fb_sub,
    nrounds = params_tuned_fb$nrounds,
    nfold = 5,
    early_stopping_rounds = 10,
    verbose = 0
  )
  
  # Return results
  list(
    params = params_tuned_fb,
    rmse = min(cv_result$evaluation_log$test_rmse_mean)
  )
}

# Measure execution time
start_time <- Sys.time()

# Run grid search in parallel (one model per core)
results_fb <- parLapply(cluster, seq_len(nrow(sampled_grid)), run_xgb_cv)

# Stop parallel cluster
stopCluster(cluster)

end_time <- Sys.time()
print(paste("Total training time:", 
            round(difftime(end_time, start_time, units = "mins"), 2), 
            "minutes"))



# Get the best parameters
best_params_fb <- do.call(rbind, lapply(results_fb, function(res) {
  c(res$params, rmse = res$rmse)
  })) %>%
  as_tibble() %>%
  arrange(as.numeric(rmse)) %>%
  as.data.frame()


print(best_params_fb)


best_params_fb <- list(
  nrounds = best_params_fb$nrounds[[1]],
  eta = best_params_fb$eta[[1]],
  max_depth = best_params_fb$max_depth[[1]],
  colsample_bytree = best_params_fb$colsample_bytree[[1]],
  subsample = best_params_fb$subsample[[1]],
  min_child_weight = best_params_fb$min_child_weight[[1]],
  lambda = 1,
  alpha = 1
  )


# Re-run model with tuned parameters
xgb_model_fb_tuned <- xgb.train(
  params = best_params_fb,
  data = dtrain_fb,
  nrounds = best_params_fb$nrounds[[1]],
  watchlist = list(train = dtrain_fb),
  print_every_n = 50
  )


# Tuned Feature Importance
importance_fb_tuned <- xgb.importance(feature_names = colnames(
  model.matrix(~ . - 1, data = train_fb[, features])),
  model = xgb_model_fb_tuned)
xgb.plot.importance(importance_fb_tuned)



# Filter the dataframe to include only rows for the year 2023 and 2024
test_fb_2023 <- test_fb %>% 
  filter(year == 2023) %>%
  mutate(delta_run_exp_mean = predict(xgb_model_fb_tuned, 
                                      newdata = as.matrix(select(., all_of(features)))))

test_fb_2024 <- test_fb %>% 
  filter(year == 2024) %>%
  mutate(delta_run_exp_mean = predict(xgb_model_fb_tuned, 
                                      newdata = as.matrix(select(., all_of(features)))))


## 2023 Stuff+ ##
# Calculate the mean and standard deviation of the delta_run_exp_mean column for 2023
fb_target_mean <- mean(test_fb_2023$delta_run_exp_mean, na.rm = TRUE)
fb_target_std <- sd(test_fb_2023$delta_run_exp_mean, na.rm = TRUE)

# Standardize the delta_run_exp_mean column to create a z-score for 2023
test_fb_2023 <- test_fb_2023 %>%
  mutate(
    delta_run_exp_mean_zscore = (delta_run_exp_mean - fb_target_mean) / fb_target_std,
    stuff_plus = 100 - (delta_run_exp_mean_zscore * 10)
  )

# Aggregate stuff_plus by pitcher_id and year for 2023
fb_agg_2023 <- test_fb_2023 %>%
  group_by(pitcher_id, pitcher_name, pitch_name, year) %>%
  summarise(
    count = n(),
    mean_stuff_plus = mean(stuff_plus, na.rm = TRUE),
    .groups = "drop"
    ) %>%
  as.data.frame()


## 2024 Stuff+ ##
# Standardize the delta_run_exp_mean column to create a z-score 
# for 2024 using 2023 mean and std
test_fb_2024 <- test_fb_2024 %>%
  mutate(
    delta_run_exp_mean_zscore = (delta_run_exp_mean - fb_target_mean) / fb_target_std,
    stuff_plus = 100 - (delta_run_exp_mean_zscore * 10)
  )

# Aggregate stuff_plus by pitcher_id and year for 2024
fb_agg_2024 <- test_fb_2024 %>%
  group_by(pitcher_id, pitcher_name, pitch_name, year) %>%
  summarise(
    count = n(),
    mean_stuff_plus = mean(stuff_plus, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  as.data.frame()



fb_agg_2024 %>% 
  arrange(desc(mean_stuff_plus)) %>%
  head(n = 20)

fb_agg_2024 %>% 
  filter(pitcher_name == "Clase, Emmanuel") %>%
  head(n = 20)










## Breaking Ball Model ---------------------------------------------------------------

dtrain_bb <- xgb.DMatrix(data = model.matrix(~ . - 1, data = train_bb[, features]), 
                         label = train_bb$delta_run_exp_mean)


# Train XGBoost model
xgb_model_bb <- xgb.train(
  params = params,
  data = dtrain_bb,
  nrounds = 250,
  watchlist = list(train = dtrain_bb),
  print_every_n = 50
)


# Feature Importance
importance_bb <- xgb.importance(feature_names = colnames(
  model.matrix(~ . - 1, data = train_bb[, features])),
  model = xgb_model_bb)
xgb.plot.importance(importance_bb)


# Hyperparameter Tuning
param_grid_bb <- expand.grid(
  nrounds = c(50, 100, 250),            # Reduced range for boosting rounds
  eta = c(0.3, 0.6, 0.9),               # Coarser learning rate grid
  max_depth = c(1, 6, 12),              # Key depths, avoiding very deep trees
  colsample_bytree = c(0.2, 0.6, 1.0),  # Coarser grid for feature subsampling
  subsample = c(0.2, 0.6, 1),           # Focused range for row subsampling
  min_child_weight = c(1, 10, 50),      # Coarse grid for minimum child weight
  lambda = c(1),                        # Fixing L2 regularization
  alpha = c(1)                          # Fixing L1 regularization
)




# Detect number of CPU cores
num_cores <- parallel::detectCores()

# Create a cluster for parallel grid search
cluster <- parallel::makeCluster(num_cores)
clusterEvalQ(cluster, library(xgboost))  # Load XGBoost on all worker nodes

# Randomly select 50 combinations
sampled_grid <- param_grid_bb[sample(nrow(param_grid_bb), 600), ]

train_bb_sub <- train_bb[sample(1:nrow(train_bb), 100000), ]

# Export necessary objects to worker nodes
clusterExport(cluster, c("sampled_grid", "train_bb_sub", "num_cores", "features"))

# Define function to train each model in parallel
run_xgb_cv <- function(i) {
  params_tuned_bb <- as.list(sampled_grid[i, ])
  params_tuned_bb <- append(params_tuned_bb, list(
    booster = "gbtree",
    objective = "reg:squarederror",
    eval_metric = "rmse",
    nthread = num_cores  # Use all CPU cores for training each model
  ))
  
  # Re-create training data inside worker node
  dtrain_bb_sub <- xgb.DMatrix(
    data = model.matrix(~ . - 1, data = train_bb_sub[, features]),
    #data = model.matrix(~ . - 1, data = train_bb[, features]),
    label = train_bb_sub$delta_run_exp_mean)
  
  # Run XGBoost cross-validation
  cv_result <- xgb.cv(
    params = params_tuned_bb,
    data = dtrain_bb_sub,
    nrounds = params_tuned_bb$nrounds,
    nfold = 5,
    early_stopping_rounds = 10,
    verbose = 0
  )
  
  # Return results
  list(
    params = params_tuned_bb,
    rmse = min(cv_result$evaluation_log$test_rmse_mean)
  )
}

# Measure execution time
start_time <- Sys.time()

# Run grid search in parallel (one model per core)
results_bb <- parLapply(cluster, seq_len(nrow(sampled_grid)), run_xgb_cv)

# Stop parallel cluster
stopCluster(cluster)

end_time <- Sys.time()
print(paste("Total training time:", 
            round(difftime(end_time, start_time, units = "mins"), 2), 
            "minutes"))



# Get the best parameters
best_params_bb <- do.call(rbind, lapply(results_bb, function(res) {
  c(res$params, rmse = res$rmse)
})) %>%
  as_tibble() %>%
  arrange(as.numeric(rmse)) %>%
  as.data.frame()


print(best_params_bb)


best_params_bb <- list(
  nrounds = best_params_bb$nrounds[[1]],
  eta = best_params_bb$eta[[1]],
  max_depth = best_params_bb$max_depth[[1]],
  colsample_bytree = best_params_bb$colsample_bytree[[1]],
  subsample = best_params_bb$subsample[[1]],
  min_child_weight = best_params_bb$min_child_weight[[1]],
  lambda = 1,
  alpha = 1
)


# Re-run model with tuned parameters
xgb_model_bb_tuned <- xgb.train(
  params = best_params_bb,
  data = dtrain_bb,
  nrounds = best_params_bb$nrounds[[1]],
  watchlist = list(train = dtrain_bb),
  print_every_n = 50
)


# Tuned Feature Importance
importance_bb_tuned <- xgb.importance(feature_names = colnames(
  model.matrix(~ . - 1, data = train_bb[, features])),
  model = xgb_model_bb_tuned)
xgb.plot.importance(importance_bb_tuned)



# Filter the dataframe to include only rows for the year 2023 and 2024
test_bb_2023 <- test_bb %>% 
  filter(year == 2023) %>%
  mutate(delta_run_exp_mean = predict(xgb_model_bb_tuned, 
                                      newdata = as.matrix(select(., all_of(features)))))

test_bb_2024 <- test_bb %>% 
  filter(year == 2024) %>%
  mutate(delta_run_exp_mean = predict(xgb_model_bb_tuned, 
                                      newdata = as.matrix(select(., all_of(features)))))


## 2023 Stuff+ ##
# Calculate the mean and standard deviation of the delta_run_exp_mean column for 2023
bb_target_mean <- mean(test_bb_2023$delta_run_exp_mean, na.rm = TRUE)
bb_target_std <- sd(test_bb_2023$delta_run_exp_mean, na.rm = TRUE)

# Standardize the delta_run_exp_mean column to create a z-score for 2023
test_bb_2023 <- test_bb_2023 %>%
  mutate(
    delta_run_exp_mean_zscore = (delta_run_exp_mean - bb_target_mean) / bb_target_std,
    stuff_plus = 100 - (delta_run_exp_mean_zscore * 10)
  )

# Aggregate stuff_plus by pitcher_id and year for 2023
bb_agg_2023 <- test_bb_2023 %>%
  group_by(pitcher_id, pitcher_name, pitch_name, year) %>%
  summarise(
    count = n(),
    mean_stuff_plus = mean(stuff_plus, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  as.data.frame()


## 2024 Stuff+ ##
# Standardize the delta_run_exp_mean column to create a z-score 
# for 2024 using 2023 mean and std
test_bb_2024 <- test_bb_2024 %>%
  mutate(
    delta_run_exp_mean_zscore = (delta_run_exp_mean - bb_target_mean) / bb_target_std,
    stuff_plus = 100 - (delta_run_exp_mean_zscore * 10)
  )

# Aggregate stuff_plus by pitcher_id and year for 2024
bb_agg_2024 <- test_bb_2024 %>%
  group_by(pitcher_id, pitcher_name, pitch_name, year) %>%
  summarise(
    count = n(),
    mean_stuff_plus = mean(stuff_plus, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  as.data.frame()



bb_agg_2024 %>% 
  arrange(desc(mean_stuff_plus)) %>%
  head(n = 20)

bb_agg_2024 %>% 
  filter(pitcher_name == "Clase, Emmanuel") %>%
  head(n = 20)










## Offspeed Model --------------------------------------------------------------------

dtrain_off <- xgb.DMatrix(data = model.matrix(~ . - 1, data = train_off[, features]), 
                         label = train_off$delta_run_exp_mean)


# Train XGBoost model
xgb_model_off <- xgb.train(
  params = params,
  data = dtrain_off,
  nrounds = 250,
  watchlist = list(train = dtrain_off),
  print_every_n = 50
)


# Feature Importance
importance_off <- xgb.importance(feature_names = colnames(
  model.matrix(~ . - 1, data = train_off[, features])),
  model = xgb_model_off)
xgb.plot.importance(importance_off)


# Hyperparameter Tuning
param_grid_off <- expand.grid(
  nrounds = c(100, 250, 500),          # Reduced range for boosting rounds
  eta = c(0.1, 0.2, 0.3),              # Coarser learning rate grid
  max_depth = c(2, 6, 10),             # Key depths, avoiding very deep trees
  colsample_bytree = c(0.5, 0.75, 1),  # Coarser grid for feature subsampling
  subsample = c(0.6, 0.8, 1),          # Focused range for row subsampling
  min_child_weight = c(1, 10, 50),     # Coarse grid for minimum child weight
  lambda = c(1),                       # Fixing L2 regularization
  alpha = c(1)                         # Fixing L1 regularization
)


# Randomly select 50 combinations
sampled_grid <- param_grid_off[sample(nrow(param_grid_off), 50), ]


cluster <- parallel::makeCluster(parallel::detectCores())



# Initialize results list
results_off <- list()

dtrain_off_sub <- dtrain_off[sample(1:nrow(dtrain_off), 100000), ]

# Perform grid search for sampled parameters
for (i in seq_len(nrow(sampled_grid))) {
  params_tuned_off <- as.list(sampled_grid[i, ])
  params_tuned_off <- append(params_tuned_off, list(
    booster = "gbtree",
    objective = "reg:squarederror",
    eval_metric = "rmse"
  ))
  
  # Cross-validation with early stopping
  cv_result <- xgb.cv(
    params = params_tuned_off,
    data = dtrain_off_sub,
    nrounds = params_tuned_off$nrounds,
    nfold = 5,
    early_stopping_rounds = 10,
    verbose = 0
  )
  
  # Store results
  results_off[[i]] <- list(
    params = params_tuned_off,
    rmse = min(cv_result$evaluation_log$test_rmse_mean)
  )
}


parallel::stopCluster(cluster)


# Get the best parameters
best_params_off <- do.call(rbind, lapply(results_off, function(res) {
  c(res$params, rmse = res$rmse)
})) %>%
  as_tibble() %>%
  arrange(as.numeric(rmse)) %>%
  as.data.frame()


print(best_params_off)


best_params_off <- list(
  nrounds = best_params_off$nrounds[[1]],
  eta = best_params_off$eta[[1]],
  max_depth = best_params_off$max_depth[[1]],
  colsample_bytree = best_params_off$colsample_bytree[[1]],
  subsample = best_params_off$subsample[[1]],
  min_child_weight = best_params_off$min_child_weight[[1]],
  lambda = 1,
  alpha = 1
)


# Re-run model with tuned parameters
xgb_model_off_tuned <- xgb.train(
  params = best_params_off,
  data = dtrain_off,
  nrounds = best_params_off$nrounds[[1]],
  watchlist = list(train = dtrain_off),
  print_every_n = 50
)


# Tuned Feature Importance
importance_off_tuned <- xgb.importance(feature_names = colnames(
  model.matrix(~ . - 1, data = train_off[, features])),
  model = xgb_model_off_tuned)
xgb.plot.importance(importance_off_tuned)



# Filter the dataframe to include only rows for the year 2023 and 2024
test_off_2023 <- test_off %>% 
  filter(year == 2023) %>%
  mutate(delta_run_exp_mean = predict(xgb_model_off_tuned, 
                                      newdata = as.matrix(select(., all_of(features)))))

test_off_2024 <- test_off %>% 
  filter(year == 2024) %>%
  mutate(delta_run_exp_mean = predict(xgb_model_off_tuned, 
                                      newdata = as.matrix(select(., all_of(features)))))

## 2023 Stuff+ ##
# Calculate the mean and standard deviation of the delta_run_exp_mean column for 2023
off_target_mean <- mean(test_off_2023$delta_run_exp_mean, na.rm = TRUE)
off_target_std <- sd(test_off_2023$delta_run_exp_mean, na.rm = TRUE)

# Standardize the delta_run_exp_mean column to create a z-score for 2023
test_off_2023 <- test_off_2023 %>%
  mutate(
    delta_run_exp_mean_zscore = (delta_run_exp_mean - off_target_mean) / off_target_std,
    stuff_plus = 100 - (delta_run_exp_mean_zscore * 10)
  )


# Aggregate stuff_plus by pitcher_id and year for 2023
off_agg_2023 <- test_off_2023 %>%
  group_by(pitcher_id, pitcher_name, pitch_name, year) %>%
  summarise(
    count = n(),
    mean_stuff_plus = mean(stuff_plus, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  as.data.frame()

## 2024 Stuff+ ##
# Standardize the delta_run_exp_mean column to create a z-score 
# for 2024 using 2023 mean and std
test_off_2024 <- test_off_2024 %>%
  mutate(
    delta_run_exp_mean_zscore = (delta_run_exp_mean - off_target_mean) / off_target_std,
    stuff_plus = 100 - (delta_run_exp_mean_zscore * 10)
  )

# Aggregate stuff_plus by pitcher_id and year for 2024
off_agg_2024 <- test_off_2024 %>%
  group_by(pitcher_id, pitcher_name, pitch_name, year) %>%
  summarise(
    count = n(),
    mean_stuff_plus = mean(stuff_plus, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  as.data.frame()




