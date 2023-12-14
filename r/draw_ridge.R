library(dplyr)
library(tidyverse)
library(RColorBrewer)
library(ggplot2)
library(ggridges)
# import data
kc <- read_csv("./data/kc_house_data.csv") 
kc$log_price <- log(kc$price)

# Price 

kc$price1000 <- kc$price/1000
kc$price_scaled <- scale(kc$price)

kc2 <- kc[kc$price <= 2000000, ]
kc2 <- kc2[!is.na(kc2$yr_built), ]


# Create a new variable for grouped yr_built
kc2$yr_built_grouped <- cut(kc2$yr_built, breaks = seq(1930, 2020, by = 10), labels = FALSE)
kc2_filtered <- na.omit(kc2[, c("price", "yr_built_grouped")])

# Define breaks and labels for the desired range labels
breaks <- seq(1930, 2020, by = 10)
labels <- sprintf("%d-%d", breaks[-length(breaks)], breaks[-1] - 1)

# Convert yr_built_grouped to a factor with the specified labels
kc2$yr_built_grouped <- cut(kc2$yr_built, breaks = breaks, labels = labels, include.lowest = TRUE)

# Gradient color scheme for all groups (shades of light blue)
light_gradient_palette <- c("#F0F8FF", "#E0F4FF", "#D1EBFF", "#C1E1FF", "#B2D8FF", "#A3CEFF", "#94C4FF", "#85BAFF", "#76B0FF")

# Ridgeline plot for Price and grouped Year Built
pl4 <- ggplot(kc2_filtered, aes(x = price, y = as.factor(yr_built_grouped), group = yr_built_grouped, fill = as.factor(yr_built_grouped))) +
  geom_density_ridges() +
  scale_fill_manual(
    values = light_gradient_palette,
    breaks = levels(factor(kc2_filtered$yr_built_grouped)),
    labels = levels(factor(kc2_filtered$yr_built_grouped))
  ) +
  theme_ridges() +
  labs(x = "Price (US dollar)", y = "Year Built") +
  theme(legend.position = "none") +
  ggtitle("Ridgeline Plot: Price and Grouped Year Built") +
  theme(
    plot.title = element_text(size = 15),
    plot.background = element_rect(fill = "white"),
    panel.background = element_rect(fill = "white")
  )

ggsave("./figure/FinalProject_Ridge.png", pl4, width = 8, height = 6, dpi = 300)
