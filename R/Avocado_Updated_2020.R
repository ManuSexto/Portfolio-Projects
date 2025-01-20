library(readr)

avocado_updated_2020 <- read_csv("C:/Users/manus/Desktop/Data Analysis/Tasks//avocado-updated-2020.csv")
View(avocado_updated_2020)

# Create a copy to work with the data
copy <- avocado_updated_2020

##### 1. What types of variables does the dataset contain? Perform an exploratory analysis of the numerical variables (mean, variance, boxplots, etc). What conclusions can be drawn from the data sample?

# Apply functions to check the structure of the data
str(copy)
summary(copy)
ncol(copy)
nrow(copy)
dim(copy)
which(is.na(copy))
View(copy)

##### 2. Extract the sales price (the variable) of organic avocados sold in Albany and Boston

# Filter organic avocados sold in Albany and Boston
organic_albany_boston <- copy[copy$type == "organic" & (copy$geography == "Albany" | copy$geography == "Boston"), ]

price_organic_albany_boston <- organic_albany_boston$average_price
print(price_organic_albany_boston)
summary(price_organic_albany_boston)

##### 3. Before modeling, calculate the covariance and correlation matrix for the price of organic and conventional avocados and their sales volume. What conclusions can be drawn?

# Separate organic and conventional avocados
organic_avocados <- copy[copy$type == "organic", ]
conventional_avocados <- copy[copy$type == "conventional", ]

# Calculate covariance and correlation between prices and sales volume by avocado type
cov(conventional_avocados$average_price, conventional_avocados$total_volume)
## -122979.7
cov(organic_avocados$average_price, organic_avocados$total_volume)
## -3027.04
cor(conventional_avocados$average_price, conventional_avocados$total_volume)
## -0.09164995
cor(organic_avocados$average_price, organic_avocados$total_volume)
## -0.04665951

# Correlation matrix
conventional_numeric_matrix <- conventional_avocados[1:16521, c(2, 3)]
organic_numeric_matrix <- organic_avocados[1:16521, c(2, 3)]
cor(conventional_numeric_matrix)
cor(organic_numeric_matrix)

##### 4. Determine the possible relationship between prices and sales volume. If we take logarithms, what would that relationship look like?

lm(conventional_avocados$total_volume ~ conventional_avocados$average_price)
lm(log(conventional_avocados$total_volume) ~ log(conventional_avocados$average_price))

## total_volume =  4162762 - 2000275*average_price
## log_total_volume =  13.42 - 1.32*log_average_price

## Interpretation:
## If the price of conventional avocados increases by 10, total sales decrease by 20002750 (2000275*10).
## If the price increases by 10%, total sales decrease by 13.2% (-1.32*10).

lm(organic_avocados$total_volume ~ organic_avocados$average_price)
lm(log(organic_avocados$total_volume) ~ log(organic_avocados$average_price))

## total_volume =  106136 - 26299*average_price
## log_total_volume =  10.1407 - -0.7665*log_average_price

## Interpretation:
## If the price of organic avocados increases by 10, total sales decrease by 262990 (26299*10).
## If the price increases by 10%, total sales decrease by 7.665% (-0.7665*10).

##### Predict the sales price of organic avocados sold in Albany for the next 3 months #####

albany_avocados <- copy[copy$type == "organic" & copy$geography == "Albany", ]
albany_price <- albany_avocados$average_price
albany_price_ts <- ts(albany_price, start = c(2015, 1), frequency = 52)

print(albany_price_ts)
decompose(albany_price_ts)
plot(decompose(albany_price_ts))
plot(albany_price_ts)

library(forecast)
albany_price_model <- auto.arima(albany_price_ts)
albany_price_forecast <- forecast(albany_price_model, 12)

View(albany_price_forecast)
plot(albany_price_forecast)



