library(rstanarm)

df <- read.table("/Users/amelia/Documents/mssp/MA678/hw2/mesquite.txt", header = T)

#log(weight) ~ log(canopy_volume) + log(canopy_area) + log(canopy_shape) + log(total_height) + log(density) + group
df$canopy_volume <- df$diam1 * df$diam2 * df$canopy_height
df$canopy_area <- df$diam1 * df$diam2
df$canopy_shape <- df$diam1 / df$diam2

new_trees <- data.frame()
new_trees$canopy_volume <- new_trees$diam1 * new_trees$diam2 * new_trees$canopy_height
new_trees$canopy_area <- new_trees$diam1 * new_trees$diam2
new_trees$canopy_shape <- new_trees$diam1 / new_trees$diam2
new_trees$predicted <- exp(log(new_trees$canopy_volume) + log(new_trees$canopy_area) + log(new_trees$canopy_shape) + log(new_trees$total_height) + log(new_trees$density) + new_trees$group)

estimate <- mean(new_trees$predicted)
std <- sd(new_trees$predicted)



