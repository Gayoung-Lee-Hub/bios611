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

# Create a box plot excluding rows with 15 or fewer bedrooms
box_plot <- ggplot(kc[kc$bedrooms < 15, ], aes(x = factor(bedrooms), y = price1000)) +
  geom_boxplot(fill = "#B2D8FF", color = "black", alpha = 0.7) +
  labs(x = "Number of bedrooms", y = "Price ($1,000)") +
  ggtitle("Box Plot: Bedrooms and Price") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 15),
    plot.background = element_rect(fill = "white"),
    panel.background = element_rect(fill = "white")
  )
box_plot2 <- ggplot(kc, aes(x = factor(bathrooms), y = price1000)) +
  geom_boxplot(fill = "#B2D8FF", color = "black", alpha = 0.7) +
  labs(x = "Number of bathrooms", y = "Price ($1,000)") +
  ggtitle("Box Plot: Bathrooms and Price") +
  theme_minimal()+
  theme(
    plot.title = element_text(size = 15),
    plot.background = element_rect(fill = "white"),
    panel.background = element_rect(fill = "white")
  )
box_plot3 <- ggplot(kc, aes(x = factor(grade), y = price1000)) +
  geom_boxplot(fill = "#94C4FF", color = "black", alpha = 0.7) +
  labs(x = "Grade", y = "Price ($1,000)") +
  ggtitle("Box Plot: Grade and Price") +
  theme_minimal()+
  theme(
    plot.title = element_text(size = 15),
    plot.background = element_rect(fill = "white"),
    panel.background = element_rect(fill = "white")
  )
box_plot4 <- ggplot(kc, aes(x = factor(condition), y = price1000)) +
  geom_boxplot(fill = "#94C4FF", color = "black", alpha = 0.7) +
  labs(x = "Condition", y = "Price ($1,000)") +
  ggtitle("Box Plot: Condition and Price") +
  theme_minimal()+
  theme(
    plot.title = element_text(size = 15),
    plot.background = element_rect(fill = "white"),
    panel.background = element_rect(fill = "white")
  )

combined_plot2 <- plot_grid(
  box_plot, box_plot2,
  box_plot3, box_plot4,
  ncol = 2, align = "h", labels = c("A", "B", "C", "D")
)

ggsave("./figure/FinalProject_Box.png", combined_plot2, width = 15, height = 10)