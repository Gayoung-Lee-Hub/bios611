
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


kc <- mutate(kc, pricecat = case_when(
  price < 321950 ~ "Cheep",
  between(price, 321950, 645000) ~ "Medium",
  price >= 645000 ~ "Expensive"
))

kc_summary <- kc %>%
  group_by(yr_built, pricecat) %>%
  summarize(count = n())

color_palette <- c("Medium"="#66c2a5", "Expensive"="#fc8d62","Cheep"="#8da0cb")


# Plotting
pl1 <- ggplot(kc_summary, aes(x = yr_built, y = count, fill = factor(pricecat, levels = pricecat_levels))) +
  geom_bar(stat = "identity", position = "stack") +
  labs(
    x = "Year built",
    y = "Count",
    fill = "Price Range",
    title = "Price and year built, grouped by price categories"
  ) +
  scale_fill_manual(values = color_palette) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 15),
    plot.background = element_rect(fill = "white"),
    panel.background = element_rect(fill = "white")
  )

kc_summary2 <- kc %>%
  filter(!is.na(yr_renovated), yr_renovated != 0) %>%
  group_by(yr_renovated, pricecat) %>%
  summarize(count = n())


# Specify the order of levels
pricecat_levels <- c("Cheep", "Medium", "Expensive")


# Plotting
pl2 <- ggplot(kc_summary2, aes(x = yr_renovated, y = count, fill = factor(pricecat, levels = pricecat_levels))) +
  geom_bar(stat = "identity", position = "stack") +
  labs(
    x = "Year Renovated",
    y = "Count",
    fill = "Price Range",
    title = "Price and year renovated, grouped by price categories"
  ) +
  scale_fill_manual(values = color_palette) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 15),
    plot.background = element_rect(fill = "white"),
    panel.background = element_rect(fill = "white")
  )

combined_pl <- plot_grid(pl1, pl2, ncol = 2)
combined_pl <- combined_pl + theme_grey(base_size = 11, base_family = "") +
  theme(
    text = element_text(size = 11),
    line = element_line(size = 11/22),
    rect = element_rect(size = 11/22)
  )
ggsave("./figure/FinalProject_PriceRange.png", combined_pl, width = 20, height = 8)