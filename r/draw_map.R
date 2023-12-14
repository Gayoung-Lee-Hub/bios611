library(grid)
library(png)
library(gridExtra)
library(ggplot2)

pic1 <- readPNG("./data/plot_price.png")
pic2 <- readPNG("./data/map.png")

grid_image1 <- rasterGrob(pic1, interpolate = TRUE)
grid_image2 <- rasterGrob(pic2, interpolate = TRUE, width = unit(0.8, "npc"), height = unit(0.7, "npc"))
map <- grid.arrange(grid_image1, grid_image2, ncol = 2)
map

ggsave("./figure/FinalProject_Map.png", map, width = 10, height = 6, dpi = 300)