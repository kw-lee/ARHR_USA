library(tidyverse)

# load data
usa_data <- read.csv('data/data.csv')

# preprocess data
colnames(usa_data)
usa_data <- usa_data %>%
  select(Alpha.Code, race1, race2, race4, 
    med_age, income, vcrime, temperature, precipitation)

n <- nrow(usa_data)

# state
state <- usa_data$Alpha.Code

# response variable
Y <- usa_data %>%
  select(race1, race2, race4) %>%
  as.matrix()

# simplex
for (i in 1:n) {
  Y[i, ] = Y[i, ] / sum(Y[i, ])
}

# covariate
X <- usa_data[-(1:4)] %>%
  mutate_all(as.numeric) %>%
  as.matrix()
X

# normalize
for (j in 1:ncol(X)) {
  X[,j] = (X[,j]-min(X[,j])) / (max(X[,j])-min(X[,j]))
}

# Suffle data
set.seed(0)
shuffle = sample(1:n)
X <- X[shuffle,]
Y <- Y[shuffle, ]
state <- state[shuffle]

save(list = c('state', 'Y', 'X'), file = "src/usa_data.Rdata")