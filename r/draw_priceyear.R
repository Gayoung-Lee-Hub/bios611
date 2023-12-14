# Library 

library(dplyr)
library(tidyverse)
library(RColorBrewer)

# import data
kc <- read_csv("./data/kc_house_data.csv") 

kc2 <- kc
kc2$log_price <- log(kc$price)


kc2$price1000 <- kc$price/1000
kc2$price_scaled <- scale(kc$price)

kc2 <- mutate(kc2, pricecat = case_when(
  price < 321950 ~ "Cheep",
  between(price, 321950, 645000) ~ "Medium",
  price >= 645000 ~ "Expensive"
))

color_palette <- c("Medium"="#66c2a5", "Expensive"="#fc8d62","Cheep"="#8da0cb")
pricecat_levels <- c("Cheep", "Medium", "Expensive")

kc2_summary3 <- kc2 %>%
  group_by(sqft_living, pricecat) %>%
  summarize(count = n())

kc2_summary3_filtered <- kc2_summary3 %>%
  filter(sqft_living >= 0 & sqft_living <= 7500)

pl3 <- ggplot(kc2_summary3_filtered, aes(x = sqft_living, y = count, fill = factor(pricecat, levels = pricecat_levels))) +
  geom_bar(stat = "identity", position = "stack", width = 30) +  # Set bar width to 50
  labs(
    x = "Square Feet(Living room)",
    y = "Count",
    fill = "Price Range",
    title = "Price and Sqrt(Living room), grouped by price categories"
  ) +
  scale_x_continuous(breaks = seq(0, 7500, by = 1000)) + 
  scale_fill_manual(values = color_palette) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 15),
    plot.background = element_rect(fill = "white"),
    panel.background = element_rect(fill = "white")
  )

ggsave("./figure/FinalProject_PriceYear.png", pl3, width = 8, height = 6, dpi = 300)

