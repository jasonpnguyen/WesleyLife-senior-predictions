# Load necessary libraries
library(tidyverse)  
library(pROC)     
library(glmnet)    
library(lubridate)

# Load custom function to clean CPS data
source("code/clean_cps.R")

# Subset data: remove rows with missing FSWROUTY and irrelevant columns
cps_wrouty <- subset(cps_data, !is.na(cps_data$FSWROUTY),
                     select = -c(FSTOTXPNC_perpers, FSSTATUS, FSSTATUSMD, FSFOODS, 
                                 FSBAL, FSRAWSCRA, FSTOTXPNC, CPSID, COUNTY,FAMINC_numeric))

# Set random seed for reproducibility and prepare for training/test split
RNGkind(sample.kind = "default")
set.seed(23591)

# Create a 70% training dataset and 30% testing dataset
train.idx <- sample(x = 1:nrow(cps_wrouty), size = 0.7*nrow(cps_wrouty))
train.df <- cps_wrouty[train.idx,]
test.df <- cps_wrouty[-train.idx,]
w <- train.df$weight  # Store weight column for weighting during model fitting

# Remove the weight column from training and testing datasets
train.df <- subset(train.df, select = -c(weight))
test.df <- subset(test.df, select = -c(weight))

# Further prep data for lasso/ridge by making test/train MATRICES

# Prepare data for Lasso and Ridge regression by converting to matrices
x.train <- model.matrix(FSWROUTY ~ ., data = train.df)[, -1] # Training matrix (exclude intercept)
x.test <- model.matrix(FSWROUTY ~ ., data = test.df)[, -1] # Testing matrix (exclude intercept)

# Convert response variable to vectors
y.train <- as.vector(train.df$FSWROUTY)
y.test <- as.vector(test.df$FSWROUTY)


# USE cross validation to fit (LOTS OF) lasso and ridge regressions
str(y.train)
table(y.train)

# Fitting Lasso and Ridge models
lr_lasso_cv <- cv.glmnet(x.train,
                         y.train,
                         family = binomial(link = "logit"),
                         alpha = 1, # alpha = 1 for Lasso
                         weights = as.integer(w)) # Apply weights

lr_ridge_cv <- cv.glmnet(x.train,
                         y.train,
                         family = binomial(link = "logit"),
                         alpha = 0, # alpha = 0 for Ridge
                         weights = as.integer(w)) # Apply weights

# Extract the optimal lambda values from results
best_lasso_lambda <- lr_lasso_cv$lambda.min # Optimal lambda for Lasso
best_ridge_lambda <- lr_ridge_cv$lambda.min # Optimal lambda for Ridge


# Fit the final Lasso and Ridge models using the optimal lambda values
final_lasso <- glmnet(x.train,
                      y.train,
                      family = binomial,
                      alpha = 1,
                      lambda = best_lasso_lambda,
                      weights = as.integer(w))
final_ridge <- glmnet(x.train,
                      y.train,
                      family = binomial(link = "logit"),
                      alpha = 0,
                      lambda = best_ridge_lambda,
                      weights = as.integer(w))

# Create predictions for both models on the test dataset
test.df.preds <- test.df %>% 
  mutate(
    lasso_pred = predict(final_lasso, x.test, type = "response")[,1],
    ridge_pred = predict(final_ridge, x.test, type = "response")[,1]
  )

# Generate ROC curves to evaluate model performance
lasso_rocCurve <- roc(response = as.factor(test.df.preds$FSWROUTY),
                      predictor = test.df.preds$lasso_pred,  # Lasso predicted probabilities
                      levels = c("0","1")) # Levels of the outcome variable
ridge_rocCurve <- roc(response = as.factor(test.df.preds$FSWROUTY),
                      predictor = test.df.preds$ridge_pred, # Ridge predicted probabilities
                      levels = c("0","1")) # Levels of the outcome variable


# Create data frame with ROC curve information for Lasso model
lasso_data <- data.frame(
  Model = "Lasso",
  Specificity = lasso_rocCurve$specificities, # Specificity values
  Sensitivity = lasso_rocCurve$sensitivities, # Sensitivity values
  AUC = lasso_rocCurve$auc %>% as.numeric # AUC value for Lasso
)

# Create data frame with ROC curve information for Ridge model
ridge_data <- data.frame(
  Model = "Ridge",
  Specificity = ridge_rocCurve$specificities, # Specificity values
  Sensitivity = ridge_rocCurve$sensitivities, # Sensitivity values
  AUC = ridge_rocCurve$auc%>% as.numeric # AUC value for Ridge
)

# Combine the Lasso and Ridge ROC data frames into one
roc_data <- rbind(lasso_data, ridge_data)

# Plot ROC curves for Lasso and Ridge models
ggplot() +
  geom_line(aes(x = 1 - Specificity, y = Sensitivity, color = Model),data = roc_data) + # Plot ROC curve
  geom_text(data = roc_data %>% group_by(Model) %>% slice(1), 
            aes(x = 0.75, y = c(0.6875, 0.5625), colour = Model,
                label = paste0(Model, " AUC = ", round(AUC, 3)))) + # Add AUC labels
  scale_colour_brewer(palette = "Paired") + # Color palette for the plot
  labs(x = "1 - Specificity", y = "Sensitivity", color = "Model") + # Axis labels
  theme_minimal() # Minimal theme for the plot