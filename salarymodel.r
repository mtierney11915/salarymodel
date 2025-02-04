library(tidyr)
library(dplyr)
library(baseballr)
#Data Preprocessing and Cleaning
{
#Contract Dataset
#Subset Data
{dfcons=select(MLB.Contracts,1:4,8,9,13)
dfcons=dfcons %>% filter(Years>0)
#Restrcture Data
str(dfcons)
dfcons$Guarantee <- gsub("[^0-9]", "", dfcons$Guarantee)  # Keep only digits
dfcons$Guarantee=as.integer(dfcons$Guarantee)
dfcons$AAV <- gsub("[^0-9]", "", dfcons$AAV)  # Keep only digits
dfcons$AAV=as.integer(dfcons$AAV)
dfcons <- dfcons %>%
  mutate(Player = sub("(.*),\\s*(.*)", "\\2 \\1", Player))
dfcons <- dfcons%>%
  mutate(Player = gsub("<e1>", "á", Player),
         Player = gsub("<e9>", "é", Player),
         Player = gsub("<ed>", "í", Player))
dfcons$stopyear=dfcons$Offseason.Year+dfcons$Years
}

#EDA
{str(dfcons)
summary(dfcons)}

#Split Data by Positions
{unique(dfcons$Pos.n)
pitcherpos=c("rhp-s", "lhp-c", "lhp-s", "rhp", "lhp", "rhp-c")
pos_df=dfcons %>% filter(!(Pos.n %in% pitcherpos))
pit_df=dfcons %>% filter((Pos.n %in% pitcherpos))
pos_df$contractyear=pos_df$Offseason.Year-1
pit_df$contractyear=pit_df$Offseason.Year-1
dfcons$contractyear=dfcons$Offseason.Year-1}

#Player Data

#Scrape FanGraphs Batting Data
{dfbat={fg_bat_leaders(
  age = "",
  pos = "np",
  stats = "bat",
  lg = "all",
  qual = "y",
  startseason = "2024",
  endseason = "2015",
  startdate = "",
  enddate = "",
  month = "0",
  hand = "",
  team = "0",
  pageitems = "100000",
  pagenum = "1",
  ind = "1",
  rost = "0",
  players = "",
  type = "8",
  postseason = "",
  sortdir = "default",
  sortstat = "playerid"
)}
dfbat=dfbat %>% select(1,5,8,12:31,42:49,59:64,70,74,75,85,116,138,141)
dfbat=dfbat %>% rename(Player = PlayerNameRoute)
#Scrape Fangraphs Pitching Data
dfpitch={fg_pitcher_leaders(
  age = "",
  pos = "all",
  stats = "pit",
  lg = "all",
  qual = "60",
  startseason = "2024",
  endseason = "2015",
  startdate = "",
  enddate = "",
  month = "0",
  hand = "",
  team = "0",
  pageitems = "100000",
  pagenum = "1",
  ind = "1",
  rost = "0",
  players = "",
  type = "8",
  postseason = "",
  sortdir = "default",
  sortstat = "playerid"
)}
dfpitch=dfpitch %>% select(1,3,6,8,12:22,24:34,45:50,51:54,57,68,75,76,122:124,131,132,133,142:150,151:161)
}

#Merge Data

#Position Players
#Identify players with multiple contracts
{poscontract_counts=pos_df %>%
  group_by(Player) %>%
  summarise(ContractCount = n())

# Split players into single and multiple contract groups
possingle_contracts <- pos_df %>%
  filter(Player %in% poscontract_counts$Player[poscontract_counts$ContractCount == 1])

posmultiple_contracts <- pos_df %>%
  filter(Player %in% poscontract_counts$Player[poscontract_counts$ContractCount > 1])
}

#Merge Single Contracts
{possingle_merged <- dfbat %>%
  left_join(possingle_contracts, by = "Player") %>%
  filter(Season < Offseason.Year)# Only include seasons before the contract year
}

#Test Run of Filtering and Merging Multiple Players
{av=filter(posmultiple_contracts,Player== "Matt Chapman")
av1=filter(dfbat,Player == "Matt Chapman")
result <- av1 %>%
  inner_join(av, by = "Player") %>%                               # Merge stats and contracts by Player
  filter((Season < Offseason.Year) | (Season == 2024 & Offseason.Year == 2024)) %>% # Include 2024 exception
  filter(Season <= stopyear) %>%                                  # Ensure the season is within the contract's validity period
  group_by(Player, Season) %>%                                    # Group by Player and Season for unique records
  slice_min(Offseason.Year, n = 1) %>%                             # Ensure only one contract per season
  ungroup() %>%                                                   # Ungroup to finalize
  select(1:5, 45:52)                                              # Select relevant columns for output
}

#Multiple Contracts Position Merge
{posmult_merged <- dfbat %>%
  inner_join(posmultiple_contracts, by = "Player") %>%                               # Merge stats and contracts by Player
  filter((Season < Offseason.Year) | (Season == 2024 & Offseason.Year == 2024)) %>% # Include 2024 exception
  filter(Season <= stopyear) %>%                                  # Ensure the season is within the contract's validity period
  group_by(Player, Season) %>%                                    # Group by Player and Season for unique records
  slice_min(Offseason.Year, n = 1) %>%                             # Ensure only one contract per season
  ungroup()
posmult_merged=posmult_merged %>% filter(Season!=2024)}

#Narrow Down Data To Relevant Columns for Modeling
{posmult_merged=posmult_merged %>% select(1:2,13,26,30,32:45,47,48,50)
possingle_merged=possingle_merged %>% select(1:2,13,26,30,32:45,47,48,50) }

#Aggregate and Weigh Seasons
# Step 1: Define hstats and metadata columns for weighing
{hstats <- c("RBI","BB_K","ISO","wOBA","wRAA","wRC","Batting","Fielding","Replacement","WAR","Spd","wRC_plus","Clutch","Contact_pct","OBP+","BABIP+","Offseason.Year","Age.7.1.24","Years","AAV") # Replace with actual statistical column names
metadata <- c("Player", "Offseason.Year", "Age.7.1.24", "Years", "AAV")
}

#Multiple Contracts
# Perform the calculation with linear decay weight and preserve the metadata columns
{contract_summary <- posmult_merged %>%
  mutate(weight = max(1, (Season - Offseason.Year + 4))) %>%  # Linear decay weight
  group_by(Player, Offseason.Year) %>%              # Group by player and contract year
  summarise(
    # Apply linear decay weighting to stats, excluding 'Offseason.Year'
    across(
      all_of(setdiff(hstats, "Offseason.Year")),  # Exclude 'Offseason.Year' from the weighting
      ~ sum(.x * weight, na.rm = TRUE) / sum(weight, na.rm = TRUE),  # Weighted average
      .names = "weighted_{.col}"  # Rename columns with "weighted_"
    ),
    # Preserve metadata (the most recent value for each)
    Age.7.1.24 = max(Age.7.1.24, na.rm = TRUE),
    Years = max(Years, na.rm = TRUE),
    AAV = max(AAV, na.rm = TRUE),
    Offseason.Year = max(Offseason.Year, na.rm = TRUE),  # Keep the most recent Offseason.Year
    .groups = "drop"  # Ungroup the data
  )}
#Single Contracts
{singlecontract_summary <- possingle_merged %>%
    mutate(weight = max(1, (Season - Offseason.Year + 4))) %>%  # Linear decay weight
    group_by(Player, Offseason.Year) %>%              # Group by player and contract year
    summarise(
      # Apply linear decay weighting to stats, excluding 'Offseason.Year'
      across(
        all_of(setdiff(hstats, "Offseason.Year")),  # Exclude 'Offseason.Year' from the weighting
        ~ sum(.x * weight, na.rm = TRUE) / sum(weight, na.rm = TRUE),  # Weighted average
        .names = "weighted_{.col}"  # Rename columns with "weighted_"
      ),
      # Preserve metadata (the most recent value for each)
      Age.7.1.24 = max(Age.7.1.24, na.rm = TRUE),
      Years = max(Years, na.rm = TRUE),
      AAV = max(AAV, na.rm = TRUE),
      Offseason.Year = max(Offseason.Year, na.rm = TRUE),  # Keep the most recent Offseason.Year
      .groups = "drop"  # Ungroup the data
    )}
#Merge Single and Multiple Final Dataframes
cdf=bind_rows(contract_summary,singlecontract_summary)

#Pitchers
{pitcontract_counts=pit_df %>%
    group_by(Player) %>%
    summarise(ContractCount = n())
  
  # Split players into single and multiple contract groups
  pitsingle_contracts <- pit_df %>%
    filter(Player %in% pitcontract_counts$Player[pitcontract_counts$ContractCount == 1])
  
  pitmultiple_contracts <- pit_df %>%
    filter(Player %in% pitcontract_counts$Player[pitcontract_counts$ContractCount > 1])
  dfpitch= dfpitch %>% rename(Player=PlayerName)
}

#Merge Single Contracts
{pitsingle_merged <- dfpitch %>%
    left_join(pitsingle_contracts, by = "Player") %>%
    filter(Season < Offseason.Year)# Only include seasons before the contract year
}

#Merge Multiple Contracts
{pitmult_merged <- dfpitch %>%
    inner_join(pitmultiple_contracts, by = "Player") %>%                               # Merge stats and contracts by Player
    filter((Season < Offseason.Year) | (Season == 2024 & Offseason.Year == 2024)) %>% # Include 2024 exception
    filter(Season <= stopyear) %>%                                  # Ensure the season is within the contract's validity period
    group_by(Player, Season) %>%                                    # Group by Player and Season for unique records
    slice_min(Offseason.Year, n = 1) %>%                             # Ensure only one contract per season
    ungroup()
  pitmult_merged=pitmult_merged %>% filter(Season!=2024)}

#Narrow Down Data To Relevant Columns for Modeling
{pitmult_merged=pitmult_merged %>% select(1:3,10,29,30,33:34,36:43,47:58,60,61,63,67,69:72)
pitsingle_merged=pitsingle_merged %>% select(1:3,10,29,30,33:34,36:43,47:58,60,61,63,67,69:72)
pitmult_merged <- pitmult_merged %>%
  mutate(Throws = if_else(Throws == "R", 1, 0))
pitsingle_merged <- pitsingle_merged %>%
  mutate(Throws = if_else(Throws == "R", 1, 0))}

#Aggregate and Weigh Seasons
# Step 1: Define pstats and pmetadata columns for weighing
{pstats <- c("QS",            
             "K_BB",          "H_9",            "WHIP",           "BABIP",         
              "FIP",            "GB_pct",         "WAR",            "xFIP",          
              "WPA",            "K_pct",          "BB_pct",         "K-BB_pct",      
              "K_9+",           "BB_9+",          "K_BB+",          "H_9+",          
              "HR_9+",          "AVG+",           "WHIP+",          "BABIP+",        
             "LOB_pct+",       "K_pct+",         "BB_pct+" ) # Replace with actual statistical column names
  pmetadata <- c("Throws","Player", "Offseason.Year", "Age.7.1.24", "Years", "AAV")
}

#Multiple Contracts
# Perform the calculation with linear decay weight and preserve the metadata columns
{pcontract_summary <- pitmult_merged %>%
    mutate(weight = max(1, (Season - Offseason.Year + 4))) %>%  # Linear decay weight
    group_by(Player, Offseason.Year) %>%              # Group by player and contract year
    summarise(
      # Apply linear decay weighting to stats, excluding 'Offseason.Year'
      across(
        all_of(setdiff(pstats, "Offseason.Year")),  # Exclude 'Offseason.Year' from the weighting
        ~ sum(.x * weight, na.rm = TRUE) / sum(weight, na.rm = TRUE),  # Weighted average
        .names = "weighted_{.col}"  # Rename columns with "weighted_"
      ),
      # Preserve metadata (the most recent value for each)
      Age.7.1.24 = max(Age.7.1.24, na.rm = TRUE),
      Years = max(Years, na.rm = TRUE),
      Throws= max(Throws,na.rm=TRUE),
      AAV = max(AAV, na.rm = TRUE),
      Offseason.Year = max(Offseason.Year, na.rm = TRUE),  # Keep the most recent Offseason.Year
      .groups = "drop"  # Ungroup the data
    )}
#Single Contracts
{psinglecontract_summary <- pitsingle_merged %>%
    mutate(weight = max(1, (Season - Offseason.Year + 4))) %>%  # Linear decay weight
    group_by(Player, Offseason.Year) %>%              # Group by player and contract year
    summarise(
      # Apply linear decay weighting to stats, excluding 'Offseason.Year'
      across(
        all_of(setdiff(pstats, "Offseason.Year")),  # Exclude 'Offseason.Year' from the weighting
        ~ sum(.x * weight, na.rm = TRUE) / sum(weight, na.rm = TRUE),  # Weighted average
        .names = "weighted_{.col}"  # Rename columns with "weighted_"
      ),
      # Preserve metadata (the most recent value for each)
      Age.7.1.24 = max(Age.7.1.24, na.rm = TRUE),
      Years = max(Years, na.rm = TRUE),
      AAV = max(AAV, na.rm = TRUE),
      Throws=max(Throws,na.rm = TRUE),
      Offseason.Year = max(Offseason.Year, na.rm = TRUE),  # Keep the most recent Offseason.Year
      .groups = "drop"  # Ungroup the data
    )}

#Merge Pitcher DFs
pdf=bind_rows(pcontract_summary,psinglecontract_summary)

#Get Ohtani To Hitters DF
{otaniH=dfbat %>% filter(Player== "Shohei Ohtani")
ohtaniC=pit_df %>% filter(Player== "Shohei Ohtani")
otani <- otaniH %>%
  left_join(ohtaniC, by = "Player") %>%
  filter(Season < Offseason.Year)
otani=otani %>% select(1:2,13,26,30,32:45,47,48,50)
{otani <- otani %>%
    mutate(weight = max(1, (Season - Offseason.Year + 4))) %>%  # Linear decay weight
    group_by(Player, Offseason.Year) %>%              # Group by player and contract year
    summarise(
      # Apply linear decay weighting to stats, excluding 'Offseason.Year'
      across(
        all_of(setdiff(hstats, "Offseason.Year")),  # Exclude 'Offseason.Year' from the weighting
        ~ sum(.x * weight, na.rm = TRUE) / sum(weight, na.rm = TRUE),  # Weighted average
        .names = "weighted_{.col}"  # Rename columns with "weighted_"
      ),
      # Preserve metadata (the most recent value for each)
      Age.7.1.24 = max(Age.7.1.24, na.rm = TRUE),
      Years = max(Years, na.rm = TRUE),
      AAV = max(AAV, na.rm = TRUE),
      Offseason.Year = max(Offseason.Year, na.rm = TRUE),  # Keep the most recent Offseason.Year
      .groups = "drop"  # Ungroup the data
    )}
cdf=bind_rows(cdf,otani)}
}

#Model Building
{
#Position AAV
{#Subset and Establish Train/Test + CV
{hAAV=cdf %>% select(3:18,22,24)
set.seed(123)  # For reproducibility
                     
                     # Split data (10% test set, 90% training set)
test_indices <- sample(1:nrow(hAAV), size = 0.1 * nrow(hAAV))
test_set <- hAAV[test_indices, ]
train_set <- hAAV[-test_indices, ]
library(caret)  # For cross-validation and model training

# Set up 5-fold cross-validation
cv_control <- trainControl(method = "cv", number = 10)  # 10-fold CV
}

#Model Loop
{model <- train(
  AAV ~ ., 
  data = train_set,
  method = "lm",  # Use different methods for other models
  trControl = cv_control
)

train_preds <- predict(model, newdata = train_set)
test_preds <- predict(model, newdata = test_set)

# Calculate evaluation metrics for training data
train_rmse <- sqrt(mean((train_preds - train_set$AAV)^2))
train_mae <- mean(abs(train_preds - train_set$AAV))
train_mse <- mean((train_preds - train_set$AAV)^2)
train_r2 <- cor(train_preds, train_set$AAV)^2

# Calculate evaluation metrics for test data
test_rmse <- sqrt(mean((test_preds - test_set$AAV)^2))
test_mae <- mean(abs(test_preds - test_set$AAV))
test_mse <- mean((test_preds - test_set$AAV)^2)
test_r2 <- cor(test_preds, test_set$AAV)^2

# Store all metrics in a dataframe
metrics_df <- data.frame(
  Model = "Linear Model",
  Train_RMSE = train_rmse,
  Train_MAE = train_mae,
  Train_MSE = train_mse,
  Train_R2 = train_r2,
  Test_RMSE = test_rmse,
  Test_MAE = test_mae,
  Test_MSE = test_mse,
  Test_R2 = test_r2
)
}
# List of models to loop over
models <- c("lm","rf", "xgbTree", "knn")

# Initialize an empty dataframe to store all metrics
metrics_df <- data.frame()

# Loop through each model
for (model_type in models) {
  tryCatch({
    # Fit the model using train() function
    model <- train(
      AAV ~ ., 
      data = train_set,
      method = model_type,  # Using the model from the loop
      trControl = cv_control
    )
    
    # Make predictions on train and test sets
    train_preds <- predict(model, newdata = train_set)
    test_preds <- predict(model, newdata = test_set)
    
    # Calculate evaluation metrics for training data
    train_rmse <- sqrt(mean((train_preds - train_set$AAV)^2))
    train_mae <- mean(abs(train_preds - train_set$AAV))
    train_mse <- mean((train_preds - train_set$AAV)^2)
    train_r2 <- cor(train_preds, train_set$AAV)^2
    
    # Calculate evaluation metrics for test data
    test_rmse <- sqrt(mean((test_preds - test_set$AAV)^2))
    test_mae <- mean(abs(test_preds - test_set$AAV))
    test_mse <- mean((test_preds - test_set$AAV)^2)
    test_r2 <- cor(test_preds, test_set$AAV)^2
    
    # Create a dataframe for the current model's metrics
    current_metrics <- data.frame(
      Model = model_type,
      Train_RMSE = train_rmse,
      Train_MAE = train_mae,
      Train_MSE = train_mse,
      Train_R2 = train_r2,
      Test_RMSE = test_rmse,
      Test_MAE = test_mae,
      Test_MSE = test_mse,
      Test_R2 = test_r2
    )
    
    # Append the current model's metrics to the main dataframe
    metrics_df <- rbind(metrics_df, current_metrics)
    
  }, error = function(e) {
    # Handle the error: Print which model failed
    message(paste("Error in model", model_type, ":", e$message))
  })
}


#Focus in on Random Forest
#Fine Tuned
colnames(test_set) <- make.names(colnames(test_set))
colnames(cdf) <- make.names(colnames(cdf))

haavmodel <- randomForest(AAV ~ ., data = train_set, mtry = 4,ntree = 500,       
  nodesize = 5       
)
haavmodel.pred=predict(haavmodel,newdata = test_set)
rftest_mae <- mean(abs(haavmodel.pred - test_set$AAV))
rftest_rmse <- sqrt(mean((haavmodel.pred - test_set$AAV)^2))
rf1 <- train(AAV ~ ., data = train_set, method = "rf", trControl = cv_control)
haavmodel.pred1=predict(rf1,newdata = test_set)
rf1test_mae <- mean(abs(haavmodel.pred1 - test_set$AAV))
rf1test_rmse <- sqrt(mean((haavmodel.pred1 - test_set$AAV)^2))

}
#Position Years
{
#Subset
{hYears=cdf %>% select(3:18,22,23)
  set.seed(1234)  # For reproducibility
  
  # Split data (10% test set, 90% training set)
  test_indices <- sample(1:nrow(hYears), size = 0.1 * nrow(hYears))
  test_set <- hYears[test_indices, ]
  train_set <- hYears[-test_indices, ]
  library(caret)  # For cross-validation and model training
  
  # Set up 5-fold cross-validation
  cv_control <- trainControl(method = "cv", number = 5)  # 10-fold CV
}
#Model Loop
{
library(caret)  # For training models
library(dplyr)  # For data manipulation

library(caret)  # For training models
library(dplyr)  # For data manipulation

# Define the models to evaluate
models <- c("lm",         # Linear regression
            "rf",         # Random forest
            "xgbTree") #Extreme Gradient Boosting Tree)    # Extreme Gradient Boosting Tree
#Normal Non-Log Scaled Models
{
# Initialize results storage
results <- list()

# Loop over caret-supported models
for (model in models) {
  # Train the model using train() from caret
  set.seed(1234)  # Ensure reproducibility for each model
  model_fit <- train(
    Years ~ .,                   # Target variable (dependent)
    data = train_set,            # Training data
    method = model,              # Model method
    trControl = cv_control       # Cross-validation setup
  )
  
  # Make predictions on the test set
  predictions <- predict(model_fit, newdata = test_set)
  
  # Calculate performance metrics
  mae <- mean(abs(predictions - test_set$Years))  # Mean Absolute Error
  rmse <- sqrt(mean((predictions - test_set$Years)^2))  # Root Mean Squared Error
  
  # Store results
  results[[model]] <- list(
    Model = model,
    MAE = mae,
    RMSE = rmse,
    Fit = model_fit  # Store the fitted model for further inspection
  )
}

# Add Poisson regression separately
set.seed(1234)
poisson_fit <- glm(
  Years ~ ., 
  data = train_set, 
  family = poisson
)

# Predict on the test set
poisson_predictions <- predict(poisson_fit, newdata = test_set, type = "response")

# Calculate Poisson performance metrics
poisson_mae <- mean(abs(poisson_predictions - test_set$Years))
poisson_rmse <- sqrt(mean((poisson_predictions - test_set$Years)^2))

# Store Poisson results
results[["poisson"]] <- list(
  Model = "poisson",
  MAE = poisson_mae,
  RMSE = poisson_rmse,
  Fit = poisson_fit
)
results_df <- do.call(rbind, lapply(results, function(res) {
  data.frame(Model = res$Model, MAE = res$MAE, RMSE = res$RMSE)
}))
}
#Log Scaled Models
{
train_set <- train_set %>%
  mutate(log_years = log(Years + 1))
train_set_modified <- train_set %>% select(-Years)
log_results <- list()

for (model in models) {
  # Train the model using train() from caret
  set.seed(1234)  # Ensure reproducibility for each model
  lmodel_fit <- train(
    log_years ~ .,                   # Target variable (dependent)
    data = train_set_modified,            # Training data
    method = model,              # Model method
    trControl = cv_control       # Cross-validation setup
  )
  
  # Make predictions on the test set
  lpredictions <- predict(lmodel_fit, newdata = test_set)
  years_predictions <- exp(lpredictions) - 1 
  # Calculate performance metrics
  logmae <- mean(abs(years_predictions - test_set$Years))  # Mean Absolute Error
  logrmse <- sqrt(mean((years_predictions - test_set$Years)^2))  # Root Mean Squared Error
  
  # Store results
  log_results[[model]] <- list(
    Model = model,
    MAE = logmae,
    RMSE = logrmse,
    Fit = lmodel_fit  # Store the fitted model for further inspection
  )
}
#Poisson
set.seed(1234)
poisson_fit <- glm(
  log_years ~ .,                   # Dependent variable
  family = poisson(link = "log"),  # Poisson family with log link
  data = train_set_modified
)
ppredictions <- predict(poisson_fit, newdata = cdf, type = "response")
pyears_predictions <- exp(ppredictions) - 1  # Transform predictions back to the original scale

# Evaluate performance
poisson_mae <- mean(abs(pyears_predictions - test_set$Years))  # Mean Absolute Error
poisson_rmse <- sqrt(mean((pyears_predictions - test_set$Years)^2))  # Root Mean Squared Error

# Store Poisson results
log_results[["poisson"]] <- list(
  Model = "poisson",
  MAE = poisson_mae,
  RMSE = poisson_rmse,
  Fit = poisson_fit
)
# Combine results into a summary data frame
logresults_df <- do.call(rbind, lapply(log_results, function(res) {
  data.frame(Model = res$Model, MAE = res$MAE, RMSE = res$RMSE)
}))
}
}
#Focus XGBoost Log Model
lmodel_fit <- train(
    log_years ~ .,                   # Target variable (dependent)
    data = train_set_modified,            # Training data
    method = "xgbTree",              # Model method
    trControl = cv_control       # Cross-validation setup
  )
xgpredtest <- predict(lmodel_fit, newdata = test_set)
xgyears_predictions <- exp(xgpredtest) - 1 
cdf$predictedYears=xgyears_predictions
cdf$Yearsdiff=cdf$Years-cdf$predictedYears
xgyears_maetest <- mean(abs(xgyears_predictions - test_set$Years))
xgyears_rmsetest <- sqrt(mean((xgyears_predictions - test_set$Years)^2))
xgpredreal <- predict(lmodel_fit, newdata = cdf)
xgyears_predictions <- exp(xgpredreal) - 1 
xgyears_maereal <- mean(abs(xgyears_predictions - cdf$Years))
xgyears_rmsereal <- sqrt(mean((xgyears_predictions - cdf$Years)^2))
cdf$predictedYears=xgyears_predictions
}
#Pitcher AAV
{#Subset and Establish Train/Test + CV
    {pAAV=pdf %>% select(4:22,27,29:30)
    set.seed(123)  # For reproducibility
    
    # Split data (10% test set, 90% training set)
    test_indices <- sample(1:nrow(pAAV), size = 0.1 * nrow(pAAV))
    test_set <- pAAV[test_indices, ]
    train_set <- pAAV[-test_indices, ]
    library(caret)  # For cross-validation and model training
    
    # Set up 5-fold cross-validation
    cv_control <- trainControl(method = "cv", number = 5)  # 10-fold CV
    }
    
    # List of models to loop over
    models <- c("lm","rf", "xgbTree", "knn")
    
    # Initialize an empty dataframe to store all metrics
    pAAVmetrics_df <- data.frame()
    train_set <- train_set %>% filter(
      !is.na(AAV) & !is.nan(AAV) & !is.infinite(AAV)
    )
    
    # Loop through each model
    for (model_type in models) {
      tryCatch({
        # Fit the model using train() function
        model <- train(
          AAV ~ ., 
          data = train_set,
          method = model_type,  # Using the model from the loop
          trControl = cv_control
        )
        
        # Make predictions on train and test sets
        train_preds <- predict(model, newdata = train_set)
        test_preds <- predict(model, newdata = test_set)
        
        # Calculate evaluation metrics for training data
        train_rmse <- sqrt(mean((train_preds - train_set$AAV)^2))
        train_mae <- mean(abs(train_preds - train_set$AAV))
        train_mse <- mean((train_preds - train_set$AAV)^2)
        train_r2 <- cor(train_preds, train_set$AAV)^2
        
        # Calculate evaluation metrics for test data
        test_rmse <- sqrt(mean((test_preds - test_set$AAV)^2))
        test_mae <- mean(abs(test_preds - test_set$AAV))
        test_mse <- mean((test_preds - test_set$AAV)^2)
        test_r2 <- cor(test_preds, test_set$AAV)^2
        
        # Create a dataframe for the current model's metrics
        current_metrics <- data.frame(
          Model = model_type,
          Train_RMSE = train_rmse,
          Train_MAE = train_mae,
          Train_MSE = train_mse,
          Train_R2 = train_r2,
          Test_RMSE = test_rmse,
          Test_MAE = test_mae,
          Test_MSE = test_mse,
          Test_R2 = test_r2
        )
        
        # Append the current model's metrics to the main dataframe
        pAAVmetrics_df <- rbind(pAAVmetrics_df, current_metrics)
        
      }, error = function(e) {
        # Handle the error: Print which model failed
        message(paste("Error in model", model_type, ":", e$message))
      })
    }
    test_set <- test_set %>%
      rename(weighted_K_BB_pct = `weighted_K-BB_pct`)
    colnames(pdf)=make.names(colnames(pdf))
    colnames(train_set)=make.names(colnames(train_set))
    paavmodel <- randomForest(`AAV` ~ ., data = train_set, mtry = 4, ntree = 500)
    paavmodel.pred=predict(paavmodel,newdata = test_set)
    prftest_mae <- mean(abs(paavmodel.pred - test_set$AAV))
    prftest_rmse <- sqrt(mean((paavmodel.pred - test_set$AAV)^2))
    paavmodel.pred=predict(paavmodel,newdata = pdf)
    prftest_maereal <- mean(abs(paavmodel.pred - pdf$AAV))
    prftest_rmsereal <- sqrt(mean((paavmodel.pred - pdf$AAV)^2))
    
   

}
#Pitcher Years
{  
  pYears=pdf %>% select(4:29)
  set.seed(1234)  # For reproducibility
  
  # Split data (10% test set, 90% training set)
  test_indices <- sample(1:nrow(pYears), size = 0.15 * nrow(pYears))
  test_set <- pYears[test_indices, ]
  train_set <- pYears[-test_indices, ]
  train_set <- train_set %>%
    filter(across(everything(), ~ !is.na(.) & is.finite(.)))
  
    train_set <- train_set %>%
      mutate(log_years = log(Years + 1))
    train_set_modified <- train_set %>% select(-Years)
    plog_results <- list()
    
    set.seed(1234)
    poisson_fit <- glm(
      log_years ~ .,                   # Dependent variable
      family = poisson(link = "log"),  # Poisson family with log link
      data = train_set_modified
    )
    ppredictions <- predict(poisson_fit, newdata = test_set, type = "response")
    pyears_predictions <- exp(ppredictions) - 1  # Transform predictions back to the original scale
    
    # Evaluate performance
    poisson_mae <- mean(abs(pyears_predictions - test_set$Years))  # Mean Absolute Error
    poisson_rmse <- sqrt(mean((pyears_predictions - test_set$Years)^2))  # Root Mean Squared Error
    
    # Store Poisson results
    plog_results[["poisson"]] <- list(
      Model = "poisson",
      MAE = poisson_mae,
      RMSE = poisson_rmse,
      Fit = poisson_fit
    )
    # Combine results into a summary data frame
    plogresults_df <- do.call(rbind, lapply(plog_results, function(res) {
      data.frame(Model = res$Model, MAE = res$MAE, RMSE = res$RMSE)
    }))
    

}
#Final Models
#Hitters AAV Final
{haavmodel <- randomForest(AAV ~ ., data = train_set, mtry = 4,ntree = 500,       
                            nodesize = 5       
  )
  haavmodel.pred=predict(haavmodel,newdata = test_set)
  rftest_mae <- mean(abs(haavmodel.pred - test_set$AAV))
  rftest_rmse <- sqrt(mean((haavmodel.pred - test_set$AAV)^2))
  haavmodel.pred=predict(haavmodel,newdata = cdf)
  rfmaereal<- mean(abs(haavmodel.pred - cdf$AAV))
  rfrmsereal <- sqrt(mean((haavmodel.pred - cdf$AAV)^2))
#Hitters Years
  lmodel_fit <- train(
    log_years ~ .,                   # Target variable (dependent)
    data = train_set_modified,            # Training data
    method = "xgbTree",              # Model method
    trControl = cv_control       # Cross-validation setup
  )
  xgpredtest <- predict(lmodel_fit, newdata = test_set)
  xgyears_predictions <- exp(xgpredtest) - 1 
  cdf$predictedYears=xgyears_predictions
  cdf$Yearsdiff=cdf$Years-cdf$predictedYears
  xgyears_maetest <- mean(abs(xgyears_predictions - test_set$Years))
  xgyears_rmsetest <- sqrt(mean((xgyears_predictions - test_set$Years)^2))
  xgpredreal <- predict(lmodel_fit, newdata = cdf)
  xgyears_predictions <- exp(xgpredreal) - 1 
  xgyears_maereal <- mean(abs(xgyears_predictions - cdf$Years))
  xgyears_rmsereal <- sqrt(mean((xgyears_predictions - cdf$Years)^2))
  cdf$predictedYears=xgyears_predictions
  # Final Hitters Predictions on Train
  cdf$Predicted_AAV=haavmodel.pred
  cdf$Predicted_Years=xgyears_predictions
  HittersOldPredDF=cdf%>% select(1:2,23:24,30:31)
  HittersOldPredDF$AAV_diff=HittersOldPredDF$AAV-HittersOldPredDF$Predicted_AAV
  HittersOldPredDF$Year_diff=HittersOldPredDF$Years-HittersOldPredDF$Predicted_Years
#Pitchers
  paavmodel <- randomForest(`AAV` ~ ., data = train_set, mtry = 4, ntree = 500)
  paavmodel.pred=predict(paavmodel,newdata = test_set)
  prftest_mae <- mean(abs(paavmodel.pred - test_set$AAV))
  prftest_rmse <- sqrt(mean((paavmodel.pred - test_set$AAV)^2))
  paavmodel.pred=predict(paavmodel,newdata = pdf)
  prftest_maereal <- mean(abs(paavmodel.pred - pdf$AAV))
  prftest_rmsereal <- sqrt(mean((paavmodel.pred - pdf$AAV)^2))
#Pitcher Years
  set.seed(1234)
  poisson_fit <- glm(
    log_years ~ .,                   # Dependent variable
    family = poisson(link = "log"),  # Poisson family with log link
    data = train_set_modified
  )
  ppredictions <- predict(poisson_fit, newdata = test_set, type = "response")
  pyears_predictions <- exp(ppredictions) - 1  # Transform predictions back to the original scale
  poisson_maetest <- mean(abs(pyears_predictions - test_set$Years))  # Mean Absolute Error
  poisson_rmsetest <- sqrt(mean((pyears_predictions - test_set$Years)^2))  # Root M
  ppredictions <- predict(poisson_fit, newdata = pdf, type = "response")
  pyears_predictions <- exp(ppredictions) - 1  # Transform predictions back to the original scale
  poisson_maereal <- mean(abs(pyears_predictions - pdf$Years))  # Mean Absolute Error
  poisson_rmsereal <- sqrt(mean((pyears_predictions - pdf$Years)^2))  # Root M
#Pitcher Final
pdf$Predicted_AAV=paavmodel.pred
pdf$Predicted_Years=pyears_predictions
PitchersOldPredDf= pdf %>% select(1,2,28,30,33:34)
PitchersOldPredDf$AAV_diff=PitchersOldPredDf$AAV-PitchersOldPredDf$Predicted_AAV
PitchersOldPredDf$Year_diff=PitchersOldPredDf$Years-PitchersOldPredDf$Predicted_Years

}
#Predictions
{
#Get Prediction Data Ready
fa=mlb_people_free_agents(season = 2024)
fa = select(fa,6,17)
fa=rename(fa,Player=player_full_name)
fapos <- fa %>% filter(!(position_abbreviation %in% c("RP", "P", "SP")))
fapos=fapos %>% select(1)
fapit <- fa %>% filter((position_abbreviation %in% c("RP", "P", "SP")))
dfbat_filtered <- dfbat %>% filter(Player %in% fapos$Player)
dfpitch_filtered= dfpitch %>% filter(Player %in%fapit$Player)
colnames(dfbat_filtered)=make.names(colnames(dfbat_filtered))
dfpitch_filtered <- dfpitch_filtered %>%
  mutate(Throw = if_else(Throws == "R", 1, 0))
dfpitch_filtered <- dfpitch_filtered %>%
  mutate(Throw = if_else(Throws == "R", 1, 0))
dfpitch_filtered= dfpitch_filtered %>% select(1,3:67)
dfpitch_filtered=dfpitch_filtered %>% rename(Throws=Throw)
#Weight years
hstats <- c("Age","RBI","BB_K","ISO","wOBA","wRAA","wRC","Batting","Fielding","Replacement","WAR","Spd","wRC_plus","Clutch","Contact_pct","OBP.","BABIP.") # Replace with actual statistical column names
fapos_sum <- dfbat_filtered %>%
  mutate(
    Age_2024 = Age + (2024 - Season),  # Calculate age as of 2024
    weight = pmax(1, (Season - 2025 + 4))  # Linear decay weight with Offseason.Year = 2025
  ) %>%
  group_by(Player) %>%  # Group by player
  summarise(
    # Apply linear decay weighting to stats
    across(
      all_of(hstats), 
      ~ sum(.x * weight, na.rm = TRUE) / sum(weight, na.rm = TRUE),  # Weighted average
      .names = "weighted_{.col}"  # Rename columns with "weighted_"
    ),
    # Calculate weighted average of 2024 age
    weighted_Age_2024 = sum(Age_2024 * weight, na.rm = TRUE) / sum(weight, na.rm = TRUE),
    .groups = "drop"  # Ungroup the data
  )
#Predict
#Hitters
colnames(fapos_sum)=make.names(colnames(fapos_sum))
fapos_sum=rename(fapos_sum, Age.7.1.24=weighted_Age_2024)
fapos_sum=select(fapos_sum,1,3:19)
aavpred=predict(haavmodel,fapos_sum)
fapos_sum$predAAV=aavpred
yearpred=predict(lmodel_fit,fapos_sum)
yearprednew=exp(yearpred)-1
fapos_sum$predYears=yearprednew
#Pitchers
pstats <- c("QS",            
            "K_BB",          "H_9",            "WHIP",           "BABIP",         
            "FIP",            "GB_pct",         "WAR",            "xFIP",          
            "WPA",            "K_pct",          "BB_pct",         "K-BB_pct",      
            "K_9+",           "BB_9+",          "K_BB+",          "H_9+",          
            "HR_9+",          "AVG+",           "WHIP+",          "BABIP+",        
            "LOB_pct+",       "K_pct+",         "BB_pct+","Throws" ) # Replace with actual statistical column names
pmetadata <- c("Throws","Player", "Offseason.Year", "Age.7.1.24", "Years", "AAV")
fapit_sum <- dfpitch_filtered %>%
  mutate(
    Age_2024 = Age + (2024 - Season),  # Calculate age as of 2024
    weight = pmax(1, (Season - 2025 + 4))  # Linear decay weight with Offseason.Year = 2025
  ) %>%
  group_by(Player) %>%  # Group by player
  summarise(
    # Apply linear decay weighting to stats
    across(
      all_of(pstats), 
      ~ sum(.x * weight, na.rm = TRUE) / sum(weight, na.rm = TRUE),  # Weighted average
      .names = "weighted_{.col}"  # Rename columns with "weighted_"
    ),
    # Calculate weighted average of 2024 age
    weighted_Age_2024 = sum(Age_2024 * weight, na.rm = TRUE) / sum(weight, na.rm = TRUE),
    .groups = "drop"  # Ungroup the data
  )
fapit_sum=fapit_sum %>% rename(Throws = weighted_Throws)
colnames(fapit_sum)=make.names(colnames(fapit_sum))
fapit_sum=rename(fapit_sum, Age.7.1.24=weighted_Age_2024)

paavpred=predict(paavmodel,fapit_sum)
fapit_sum$predAAV=paavpred
ppredictions <- predict(poisson_fit, newdata = fapit_sum, type = "response")
pyears_predictions <- exp(ppredictions) - 1
fapit_sum$predYears=pyears_predictions
pitres=fapit_sum%>%select(1,28,29)
posres=fapos_sum%>% select(1,19,20)
results=rbind(pitres,posres)
}
}

#Model Results
{
results_df #hitters years
logresults_df #hitters years log
metrics_df #hitters aav
pAAVmetrics_df #pitchers aav
plogresults_df

Model= c("Hitter AAV (Random Forest)","Hitter Years (XG Boost)","Pitcher AAV (Random Forest)","Pitcher Years (Poisson)")
MAE_Test=c(rftest_mae,xgyears_maetest,prftest_mae,poisson_mae)
MAE_ALL=c(rfmaereal,xgyears_maereal,prftest_maereal,poisson_maereal)
RMSE_Test=c(rftest_rmse,xgyears_rmsetest,prftest_rmse,poisson_rmse)
RMSE_ALL=c(rfrmsereal,xgyears_rmsereal,prftest_rmsereal,poisson_rmsereal)
ModelResults=data.frame(Model,MAE_Test,MAE_ALL,RMSE_Test,RMSE_ALL)
}

#Excel Exports
{write.csv(HittersOldPredDF,"Hitters Old.csv")
write.csv(PitchersOldPredDf,"Pitchers Old.csv")
write.csv(results,"FA Preds.csv")
write.csv(ModelResults,"Model Results.csv")}
