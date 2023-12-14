# package
library(readr)
library(dplyr)
library(fields)
library(RColorBrewer)
library(lubridate)
library(ggplot2)


setwd("~/Desktop/BIOS611")
getwd()

# import data
kc <- read_csv("source_data/kc_house_data.csv") 
kc

price <-summary(kc$price)
print(price)


# price log scale: to adjust the effect of long tail (outlier)
kc$log_price <- log(kc$price)

# new dataset 
kc2 <-kc

# selling price by month
kc2$date <- paste(substr(kc$date, 1,6), "01", sep = "")
kc2$date <- ymd(kc$date)

# stores the median price of homes for each month
median_price_by_month <- kc2 %>% 
  group_by(date) %>% 
  summarise(median_price_month = median(price))

# price per square feet
kc2$price_sqft_living <- kc$price/kc$sqft_living
kc2$price_sqft_lot <- kc$price/kc$sqft_lot
kc2$price_sqft_lot15 <- kc$price/kc$sqft_lot15
kc2$price_sqft_living15 <- kc$price/kc$sqft_living15

# average price per square feet of living/lot

kc2 <- kc2 %>% 
  mutate(price_sqft_avg = 
           (price_sqft_living + price_sqft_lot + price_sqft_lot15 + price_sqft_living15)/4)

tmp <- kc2 %>% 
  group_by(date) %>% 
  summarise(mean_price = median(price_sqft_avg)) %>% as.data.frame()


# graph of mean price per sqft by date

library(ggplot2)

price_by_date <- kc2 %>%
  group_by(date) %>%
  summarise(mean_price = mean(price_sqft_avg))

ggplot(price_by_date, aes(x = date, y = mean_price)) +
  geom_line() +
  labs(x = "Date", y = "Mean Price per Sqft Avg") +
  ggtitle("Mean Price per Sqft Avg Over Time")

ggplot(kc2, aes(x = bathrooms, y = price_sqft_avg)) +
  geom_point() +
  labs(x = "Bathrooms", y = "Average Price per sqft") +
  ggtitle("Scatter Plot of price by the number of bathrooms")

# the association between year(renovated) and price per sqft
# except houses without renovation
subset_kc2 <- subset(kc2, yr_renovated != 0)

# linear regression w/o none-renovation houses
model_w_renov <- lm(price_sqft_avg ~ yr_renovated, data = subset_kc2)

par(cex.lab = 0.8, cex.axis = 0.6, cex.main = 1)
par(mar = c(5, 4, 4, 2) + 0.2)

p1 <- ggplot(subset_kc2, aes(x = yr_renovated, y = price_sqft_avg)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "blue") +
  labs(x = "Year renovated", y = "Price") +
  ggtitle("Price per sqft among houses with renovation in King County, WA") +
  scale_x_continuous(breaks = seq(min(subset_kc2$yr_renovated), max(subset_kc2$yr_renovated), by = 10))
p1

ggsave("plot1.png",p1)

summary(model_w_renov)



library(ggridges)


# Ridgeline plot: floors
par(cex.lab = 0.8, cex.axis = 0.6, cex.main = 1)
par(mar = c(5, 4, 4, 2) + 0.2)

p2 <- ggplot(kc2, aes(x = price_sqft_avg, y = floors, group = floors, fill = floors)) +
  geom_density_ridges() +
  scale_fill_gradient(low = "skyblue", high = "pink") +  # set color range 
  theme_ridges() + 
  labs(x = "Average price per sqft", y = "N of floors") +
  theme(legend.position = "none") +
  ggtitle("Ridgeline Plot: Price and floors") +
  theme(panel.background = element_rect(fill = "white"),  
        panel.grid.major.y = element_line(color = "gray"))
p2

ggsave("plot2.png",p2)