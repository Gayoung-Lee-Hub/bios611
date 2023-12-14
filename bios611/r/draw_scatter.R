
library(dplyr)
library(tidyverse)
library(RColorBrewer)
library(ggplot2)
library(ggridges)
library(cowplot)

# import data
kc <- read_csv("./data/kc_house_data.csv") 
kc$log_price <- log(kc$price)

kc$price1000 <- kc$price/1000
kc$price_scaled <- scale(kc$price)
# Define breaks and labels for the desired range labels
breaks <- seq(1930, 2020, by = 10)
labels <- sprintf("%d-%d", breaks[-length(breaks)], breaks[-1] - 1)

kc8 <- kc[kc$price <= 2000000, ]
kc8 <- kc8[!is.na(kc8$yr_built), ]
kc8$yr_built_grouped <- cut(kc8$yr_built, breaks = seq(1930, 2020, by = 10), labels = FALSE)
kc8$yr_built_grouped <- cut(kc8$yr_built, breaks = breaks, labels = labels, include.lowest = TRUE)


kc8 <- kc[kc$price <= 2000000, ]

scatter_plot <- ggplot(kc8, aes(x = sqft_living, y = price)) +
  geom_point(alpha = 0.5) +  # Use alpha to add transparency to points
  geom_smooth(method = "lm", se = FALSE, color = "blue") +  # Add linear regression line
  labs(x = "Square Feet(Living Room)", y = "Price (US Dollar)") +
  ggtitle("Scatter Plot with Regression Line: Price by Squared Feet(Living Room)") +
  theme_minimal()+
  theme(
    plot.title = element_text(size = 10),
    plot.background = element_rect(fill = "white"),
    panel.background = element_rect(fill = "white")
  )


scatter_plot2 <- ggplot(kc8, aes(x = sqft_above, y = price)) +
  geom_point(alpha = 0.5) +  # Use alpha to add transparency to points
  geom_smooth(method = "lm", se = FALSE, color = "blue") +  # Add linear regression line
  labs(x = "Square Feet(Above)", y = "Price (US Dollar)") +
  ggtitle("Scatter Plot with Regression Line: Price by Squared Feet(Above)") +
  theme_minimal()+
  theme(
    plot.title = element_text(size = 10),
    plot.background = element_rect(fill = "white"),
    panel.background = element_rect(fill = "white")
  )


scatter_plot3 <- ggplot(kc8, aes(x = sqft_basement, y = price)) +
  geom_point(alpha = 0.5) +  # Use alpha to add transparency to points
  geom_smooth(method = "lm", se = FALSE, color = "blue") +  # Add linear regression line
  labs(x = "Square Feet(Basement)", y = "Price (US Dollar)") +
  ggtitle("Scatter Plot with Regression Line: Price by Squared Feet(Basement)") +
  theme_minimal()+
  theme(
    plot.title = element_text(size = 10),
    plot.background = element_rect(fill = "white"),
    panel.background = element_rect(fill = "white")
  )


scatter_plot4 <- ggplot(kc8, aes(x = yr_built, y = price)) +
  geom_point(alpha = 0.5) +  # Use alpha to add transparency to points
  geom_smooth(method = "lm", se = FALSE, color = "blue") +  # Add linear regression line
  labs(x = "Year Built", y = "Price (US Dollar)") +
  ggtitle("Scatter Plot with Regression Line: Price and Year Built") +
  theme_minimal()+
  theme(
    plot.title = element_text(size = 10),
    plot.background = element_rect(fill = "white"),
    panel.background = element_rect(fill = "white")
  )

combined_plot <- plot_grid(
  plot_grid(scatter_plot, scatter_plot2, ncol = 1),
  plot_grid(scatter_plot3, scatter_plot4, ncol = 1),
  ncol = 2, align = "v", labels = c("A", "B")
)

ggsave("./figure/FinalProject_Scatter.png", combined_plot, width = 15, height = 10)