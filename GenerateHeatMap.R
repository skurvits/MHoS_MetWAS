library("tidyverse")
library("ggplot2")

association_df <- read.csv("results_linear_M.csv", sep = ",", header = TRUE)

# Prepare the data for the heatmap
heatmap_data <- reshape2::dcast(association_df, variable_source ~ feature, value.var = "beta")

# Set the row names (variable_source)
rownames(heatmap_data) <- heatmap_data$variable_source
heatmap_data$variable_source <- NULL


# Convert odds ratios to a numeric matrix
heatmap_matrix <- as.matrix(heatmap_data)

# Replace any NA or NaN values with 0
heatmap_matrix[is.na(heatmap_matrix) | is.nan(heatmap_matrix)] <- 0

heatmap_matrix

options(repr.plot.width=20, repr.plot.height=20)

# Create the heatmap
heatmap(heatmap_matrix, 
        col = colorRampPalette(c("blue", "white", "red"))(20), 
        scale = "none",
        xlab = "Feature",
        ylab = "Variable Source",
        main = "Heatmap of Odds Ratios (Male)",
        cexRow = 1.5,# Adjust the size of row labels
        cexCol = 1.5,
        margins = c(10, 20),  # Adjust margins to fit long labels
        las = 2)  # Rotate y-axis labels
