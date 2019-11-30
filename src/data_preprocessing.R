# preprocess data

# example output
usa_data = data.frame(
  state = c("DE", "PA"),
  pop_age00 = c(1/4, 1/3),
  pop_age01 = c(1/3, 1/4), 
  # ... pop_age85
  race_1 = c(1/4, 1/4),
  # ... race_2, 3, 4, 5
  # if can, sex_0, sex_1 (female, male)
  temp = c(23.0, 24.5),
  rain = c(34.0, 72.3),
  crime = c(0.3, 0.2), 
  income = c(23.4, 32.4)
)

head(usa_data)