

rm(list = ls())

library(grid)
library(ggplot2)
library(ggthemes)
library(scales)

setwd("/Users/davidrosenfeld/Documents/data_visualisation")

# Import data
wine <- read.csv("winequality-red.csv", sep = ";")

# Compute SVD on the data a create the dataframes necessary for PCA (we here multiply U by D)
wine.svd <- svd(scale(wine[, 1:11]))
wine.rpc <- data.frame(wine.svd$u %*% diag(wine.svd$d))
wine.csc <- data.frame(wine.svd$v)

# Extract variable names for chemical elements
variables <- colnames(wine[, 1:11])

# Extract the PC coordinates by filtering by wine quality
wine.rpc3 <- wine.rpc[which(wine$quality == 3),]
wine.rpc4 <- wine.rpc[which(wine$quality == 4),]
wine.rpc5 <- wine.rpc[which(wine$quality == 5),]
wine.rpc6 <- wine.rpc[which(wine$quality == 6),]
wine.rpc7 <- wine.rpc[which(wine$quality == 7),]
wine.rpc8 <- wine.rpc[which(wine$quality == 8),]


# Create variables for the coordinates, colours and labels for the legend
y_point_coords <- c(0.1, 0.25, 0.4, 0.55, 0.7, 0.85)
y_points_colours <-  c("#4575b4", "#91bfdb", "#e0f3f8", "#fee090", "#fc8d59", "#d73027")
y_points_labels <- c("3", "4", "5", "6", "7", "8")

# Regress quality on the first two principal components in order to then map it.
# Convert it to a dataframe and name the columns x and y.
qual_reg <- lm(scale(wine$quality) ~ scale(wine.rpc[,1]) + scale(wine.rpc[,2]))
qual_coords <- data.frame(t(c(summary(qual_reg)$coefficients[2,1], summary(qual_reg)$coefficients[3,1])))
colnames(qual_coords) <- c("x", "y")


# Create a PCA biplot. We use X1 and X2 as default variables for all data, and in the ggplot call have data = NULL
ggplot(NULL, aes(x = X1, y = X2)) +
  # Plot all the observations on the biplot, with colours for wine quality, and alpha transparancy
  geom_point(data = wine.rpc3, col = "#4575b4", alpha = 0.5) +
  geom_point(data = wine.rpc4, col = "#91bfdb", alpha = 0.5) +
  geom_point(data = wine.rpc5, col = "#e0f3f8", alpha = 0.5) +
  geom_point(data = wine.rpc6, col = "#fee090", alpha = 0.5) +
  geom_point(data = wine.rpc7, col = "#fc8d59", alpha = 0.5) +
  geom_point(data = wine.rpc8, col = "#d73027", alpha = 0.5) +
  # Add arrows and texts for the variables in relation to the principal components
  geom_segment(data = wine.csc, aes(x = 0, xend = 10*wine.csc[,1], y = 0, yend = 10*wine.csc[,2]), 
               arrow = arrow(length = unit(0.3,"cm"))) +
  geom_text(data = wine.csc, aes(x = 11*wine.csc[,1], y = 11*wine.csc[,2], label = variables)) +
  # Add a thick arrow and bold text for quality as pre-calculated via regression
  geom_segment(data = qual_coords, aes(x = 0, y = 0, xend = 10*x, yend = 10*y),
               arrow = arrow(length = unit(0.2,"cm")), size = 2) +
  geom_text(data = qual_coords, aes(x = 11*x, y = 11*y, label = "Quality", fontface = "bold")) +
  # Change theme to make the background white, get rid of grids, and make the x and y labels bold
  theme(panel.background = element_blank(), panel.grid = element_blank(), axis.line = element_line(color = "black"), 
        axis.ticks = element_line(color = "black"), axis.title = element_text(face = "bold"), 
        axis.text = element_text(face = "bold")) +
  # Add horizontal and vertical dotted lines at 0
  geom_hline(yintercept = 0, lty = 2, alpha = 0.5) +
  geom_vline(xintercept = 0, lty = 2, alpha = 0.5) +
  # Add centered title, x and y labels.
  ggtitle("PCA of wine by chemical components,
          with wine quality by colour") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  xlab("PC1 (29.2% of variance explained)") +
  ylab("PC2 (17.5% of variance explained)")
# Add a legend manually. Create a Viewport in the top right corner, which we outline with a rectangle
pushViewport(viewport(x = 0.9, y = 0.8, width = 0.1, height = 0.35))
grid.rect()
# Add the legend points, labels, and title
grid.points(x = rep(0.3, 6), y = y_point_coords - 0.02, pch = 19, 
            gp = gpar(col = y_points_colours, alpha = 0.5, cex = 0.7))
grid.text(x = rep(0.7, 6), y = y_point_coords - 0.02, label = y_points_labels)
grid.text(x = 0.5, y = 0.95, label  = "Quality")


pve <- 100*wine.svd$d^2/sum(wine.svd$d^2)

barplot(pve, main ="Proportion of variance explained", xlab = "Principal components from 1 to 11", 
        ylab = "Proportion of variance of explained, %")
