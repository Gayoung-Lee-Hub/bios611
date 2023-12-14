# Library 

library(dplyr)
library(tidyverse)

install.packages("RColorBrewer")
library(RColorBrewer)

# Directory
setwd("~/Desktop/BIOS611")
getwd()

# import data
kc <- read_csv("source_data/kc_house_data.csv") 
kc

price <-summary(kc$price)
print(price)

# price log scale: to adjust the effect of long tail (outlier)
kc2 <- kc
kc2$log_price <- log(kc$price)

# Price 

kc2$price1000 <- kc$price/1000
kc2$price_scaled <- scale(kc$price)

kc3 <- kc2 %>% 
  select(., -id)

# Group by price
layout(matrix(1:3, nrow = 1))

### Below 1st Quarter
cheepkc <- kc3[kc3[,2] <= 321950,]
summary(cheepkc$yr_built)
summary(cheepkc$yr_renovated)
hist(cheepkc$yr_built, main = "Histogram of Year Built - Bottom 25% of House Prices", xlab = "Year Built", col = "skyblue", border = "black")

### 1st - 3rd Quarter
avekc <- kc3[321950 < kc3[,2] & kc3[,2] <= 645000,]
summary(avekc$yr_built)
summary(avekc$yr_renovated)
hist(avekc$yr_built, main = "Histogram of Year Built - Interquartile range (IQR)", xlab = "Year Built", col = "skyblue", border = "black")

### upper 3rd Quarter 
expkc <- kc3[kc3[,2] > 645000,]
summary(expkc$yr_built)
summary(expkc$yr_renovated)
hist(expkc$yr_built, main = "Histogram of Year Built - Top 25% of House Prices", xlab = "Year Built", col = "skyblue", border = "black")

dev.print(png, file = "finalplot1.png", width = 800, height = 400)


## categorization by price (~25%/25%-75%/75%~)

kc2 <- mutate(kc2, pricecat = case_when(
  price < 321950 ~ "Cheep",
  between(price, 321950, 645000) ~ "Medium",
  price >= 645000 ~ "Expensive"
))

kc2_summary <- kc2 %>%
  group_by(yr_built, pricecat) %>%
  summarize(count = n())

color_palette <- c("Medium"="#66c2a5", "Expensive"="#fc8d62","Cheep"="#8da0cb")

# Specify the order of levels
pricecat_levels <- c("Cheep", "Medium", "Expensive")

# Plotting
pl1 <- ggplot(kc2_summary, aes(x = yr_built, y = count, fill = factor(pricecat, levels = pricecat_levels))) +
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

# Print the plot
print(pl1)


#### Renovated
kc2_summary2 <- kc2 %>%
  filter(!is.na(yr_renovated), yr_renovated != 0) %>%
  group_by(yr_renovated, pricecat) %>%
  summarize(count = n())


# Specify the order of levels
pricecat_levels <- c("Cheep", "Medium", "Expensive")


# Plotting
pl2 <- ggplot(kc2_summary2, aes(x = yr_renovated, y = count, fill = factor(pricecat, levels = pricecat_levels))) +
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

# Print the plot
print(pl2)


## plot combine
combined_pl <- plot_grid(pl1, pl2, ncol = 2)

# Apply the theme to the entire combined plot
combined_pl <- combined_pl + theme_grey(base_size = 11, base_family = "") +
  theme(
    text = element_text(size = 11),
    line = element_line(size = 11/22),
    rect = element_rect(size = 11/22)
  )
print(combined_pl)

# Save the plot
ggsave("combined_pl.png", combined_pl, width = 20, height = 8)



#### pl3

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

print(pl3)
# save
ggsave("FinalProject3.png", pl3, width = 8, height = 6, dpi = 300)

# Z-점수 계산
z_scores <- scale(kc2$price)

# Z-점수가 일정 기준을 넘는 경우를 이상치로 판단
outlier_indices <- which(abs(z_scores) > 3)

# 이상치 확인: 2000000 upper 
outlier_prices <- kc2$price[outlier_indices]

# 이상치 출력
cat("Outlier Prices:", outlier_prices, "\n")

kc8 <- kc2[kc2$price <= 2000000, ]
kc8 <- kc8[!is.na(kc8$yr_built), ]


# Create a new variable for grouped yr_built
kc8$yr_built_grouped <- cut(kc8$yr_built, breaks = seq(1930, 2020, by = 10), labels = FALSE)
kc8_filtered <- na.omit(kc8[, c("price", "yr_built_grouped")])

# Define breaks and labels for the desired range labels
breaks <- seq(1930, 2020, by = 10)
labels <- sprintf("%d-%d", breaks[-length(breaks)], breaks[-1] - 1)

# Convert yr_built_grouped to a factor with the specified labels
kc8$yr_built_grouped <- cut(kc8$yr_built, breaks = breaks, labels = labels, include.lowest = TRUE)

# Gradient color scheme for all groups (shades of light blue)
light_gradient_palette <- c("#F0F8FF", "#E0F4FF", "#D1EBFF", "#C1E1FF", "#B2D8FF", "#A3CEFF", "#94C4FF", "#85BAFF", "#76B0FF")

# Ridgeline plot for Price and grouped Year Built
pl4 <- ggplot(kc8_filtered, aes(x = price, y = as.factor(yr_built_grouped), group = yr_built_grouped, fill = as.factor(yr_built_grouped))) +
  geom_density_ridges() +
  scale_fill_manual(
    values = light_gradient_palette,
    breaks = levels(factor(kc8_filtered$yr_built_grouped)),
    labels = levels(factor(kc8_filtered$yr_built_grouped))
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

# Print the plot
print(pl4)
# save
ggsave("FinalProject4.png", pl4, width = 8, height = 6, dpi = 300)



#### scatter plot

kc8 <- kc2[kc2$price <= 2000000, ]
# Create a scatter plot with regression line
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


# Print the scatter plot
print(scatter_plot)

# Create a scatter plot with regression line
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

# Print the scatter plot
print(scatter_plot2)


# Create a scatter plot with regression line
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

# Print the scatter plot
print(scatter_plot3)


# Create a scatter plot with regression line
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

# Print the scatter plot
print(scatter_plot4)


# Combine plots
install.packages("cowplot")
library(cowplot)

# Set up the layout
combined_plot <- plot_grid(
  plot_grid(scatter_plot, scatter_plot2, ncol = 1),
  plot_grid(scatter_plot3, scatter_plot4, ncol = 1),
  ncol = 2, align = "v", labels = c("A", "B")
)

# Print the final combined plot
print(combined_plot)

# Specify the size of the plotting area
ggsave("FinalProject_Scatter.png", combined_plot, width = 15, height = 10)



# Create a box plot excluding rows with 15 or fewer bedrooms
box_plot_filtered <- ggplot(kc2[kc2$bedrooms < 15, ], aes(x = factor(bedrooms), y = price1000)) +
  geom_boxplot(fill = "#B2D8FF", color = "black", alpha = 0.7) +
  labs(x = "Number of bedrooms", y = "Price ($1,000)") +
  ggtitle("Box Plot: Bedrooms and Price") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 15),
    plot.background = element_rect(fill = "white"),
    panel.background = element_rect(fill = "white")
  )

# Print the filtered box plot
print(box_plot_filtered)
ggsave("FinalProject9.png", box_plot_filtered, width = 8, height = 6, dpi = 300)


box_plot2 <- ggplot(kc2, aes(x = factor(bathrooms), y = price1000)) +
  geom_boxplot(fill = "#B2D8FF", color = "black", alpha = 0.7) +
  labs(x = "Number of bathrooms", y = "Price ($1,000)") +
  ggtitle("Box Plot: Bathrooms and Price") +
  theme_minimal()+
  theme(
    plot.title = element_text(size = 15),
    plot.background = element_rect(fill = "white"),
    panel.background = element_rect(fill = "white")
  )

# Print the box plot
print(box_plot2)
ggsave("FinalProject10.png", box_plot2, width = 8, height = 6, dpi = 300)


box_plot3 <- ggplot(kc2, aes(x = factor(grade), y = price1000)) +
  geom_boxplot(fill = "#94C4FF", color = "black", alpha = 0.7) +
  labs(x = "Grade", y = "Price ($1,000)") +
  ggtitle("Box Plot: Grade and Price") +
  theme_minimal()+
  theme(
    plot.title = element_text(size = 15),
    plot.background = element_rect(fill = "white"),
    panel.background = element_rect(fill = "white")
  )

# Print the box plot
print(box_plot3)
ggsave("FinalProject11.png", box_plot3, width = 8, height = 6, dpi = 300)


box_plot4 <- ggplot(kc2, aes(x = factor(condition), y = price1000)) +
  geom_boxplot(fill = "#94C4FF", color = "black", alpha = 0.7) +
  labs(x = "Condition", y = "Price ($1,000)") +
  ggtitle("Box Plot: Condition and Price") +
  theme_minimal()+
  theme(
    plot.title = element_text(size = 15),
    plot.background = element_rect(fill = "white"),
    panel.background = element_rect(fill = "white")
  )

# Print the box plot
print(box_plot4)
ggsave("FinalProject12.png", box_plot5, width = 8, height = 6, dpi = 300)

# Set up the layout
combined_plot2 <- plot_grid(
  box_plot, box_plot2,
  box_plot3, box_plot4,
  ncol = 2, align = "h", labels = c("A", "B", "C", "D")
)
print(combined_plot2)

# Specify the size of the plotting area
ggsave("FinalProject_Box.png", combined_plot2, width = 15, height = 10)