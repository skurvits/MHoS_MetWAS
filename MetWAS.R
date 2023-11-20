library("tidyverse")
library("ggplot2")
library("sandwich", lib="~/local/R_libs/")


# List of variables to substitute for EEK2.depr.binary
variables_vector <- c("EEK2.depr.binary", "HevtDepression.deprDepressedEver.code", "HevtDepression.deprLossOfInterestEver.code", "HevtDepression.deprTired.code",
                     "HevtDepression.deprAppetite1.code", "HevtDepression.deprGainWeight.code", "HevtDepression.deprLoseWeight.code", "HevtDepression.deprSleep1.code",
                     "HevtDepression.deprAttention.code", "HevtDepression.deprWorthlessness.code", "HevtDepression.deprConfidence.code", "HevtDepression.deprHopelessness.code",
                     "HevtDepression.deprThoughtsOfDeath.code", "HevtDepression.deprSLE.code")  # Replace with your variable names

# Initialize the results data frame
results <- data.frame()

# Loop through variables and perform association analysis
for (variable in variables_vector) {
  # Perform association analysis using logistic regression for each feature
  association_results <- list()
  OR <- list()
  CI_low <- list()
  CI_high <- list()
  
  for (feature in feature_vector) {
    if (feature != variable && !(feature %in% covariates)) {
      formula <- as.formula(paste(variable, "~", feature, "+", paste(covariates, collapse = "+")))
      
      # Remove rows with missing values for the feature of interest
      model_data <- na.omit(analysis_df_F[, c(feature, variable, covariates), drop = FALSE])
      
      # Perform logistic regression with robust standard errors using the sandwich estimator
      model <- glm(formula, data = model_data, family = binomial)
      robust_model <- coeftest(model, vcov = vcovHC(model, type = "HC4"))  # Sandwich estimator
      
      # Check if there are valid results for this feature
      if (!is.null(robust_model)) {
        p_value <- summary(model)$coef[2, 4]  # Accessing the p-value for the coefficient
        odds_ratio <- exp(coef(robust_model)[2])
        conf_interval <- exp(confint(robust_model)[2, , drop = FALSE])
        lower_ci <- conf_interval[1, "2.5 %"]
        upper_ci <- conf_interval[1, "97.5 %"]
        
        association_results[[feature]] <- p_value
        OR[[feature]] <- odds_ratio
        CI_low[[feature]] <- lower_ci
        CI_high[[feature]] <- upper_ci
      }
    }
  }
  
  # Convert association results to a data frame
  association_df <- data.frame(
    feature = names(association_results),
    p_value = unlist(association_results),
    odds_ratio = unlist(OR),
    CI_low = unlist(CI_low),
    CI_high = unlist(CI_high)
  )
  
  # Compute adjusted p-values using Bonferroni correction
  alpha <- 0.05
  n <- length(association_df$p_value)
  association_df$adjusted_p_value <- p.adjust(association_df$p_value, method = "bonferroni")
  
  # Filter based on adjusted p-value
  significant_results <- subset(association_df, adjusted_p_value < discovery_threshold)
  
  # Sort the association results by adjusted p-value
  association_df <- association_df[order(association_df$adjusted_p_value), ]
  
  # Save association results to a CSV file
  csv_filename <- paste0("association_results_binary_F", variable, ".csv")
  write.csv(association_df, file = csv_filename)
  
  # Add significant results to the results data frame
  results <- rbind(results, significant_results)
}

# Save the results data frame
write.csv(results, file = "results_binary_F.csv", row.names = FALSE)
