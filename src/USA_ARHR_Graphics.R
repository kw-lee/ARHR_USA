library(ggtern)

load("C:/Users/audgns5222/Desktop//ARHR_error.Rdata")

dyn.load('C:/Users/audgns5222/Desktop/Add-Reg-Hilbert-Res/Dll files/CBS_continuous_simplex.dll') # path of the CBS_continuous_simplex.dll file
dyn.load('C:/Users/audgns5222/Desktop/Add-Reg-Hilbert-Res/Dll files/SBF_continuous_simplex.dll') # path of the CBS_continuous_simplex.dll file

summary(X)
mean_X <- apply(X,2,mean)
test <- as.data.frame(matrix(rep(mean_X, 100), nrow = 100, byrow = T))
colnames(test) <- colnames(X)

test_age <- test
test_age$med_age <- seq(0,1, length.out = 100)  
Y_pred <- SBF_simplex(as.matrix(test_age), X_training, Y_training, h = optimal_h[k,])$Y_hat
colnames(Y_pred) <- colnames(Y)
df_age <- as.data.frame(cbind(test_age,Y_pred))
p_age <- ggtern(data = df_age, aes(x = race1, y = race2, z = race4, color = med_age)) +
  geom_point(size=2) +
  scale_color_gradientn(colours = rainbow(3))+
  ggtitle("age") + 
  labs(fill = "age") + 
  theme(legend.position      = c(0,1), 
        legend.justification = c(0,1))
p_age


test_income <- test
test_income$income <- seq(0,1, length.out = 100)  
Y_pred <- SBF_simplex(as.matrix(test_income), X_training, Y_training, h = optimal_h[k,])$Y_hat
colnames(Y_pred) <- colnames(Y)
df_income <- as.data.frame(cbind(test_income,Y_pred))
p_income <- ggtern(data = df_income, aes(x = race1, y = race2, z = race4, color = income)) +
  geom_point(size=2) +
  scale_color_gradientn(colours = rainbow(3))+
  ggtitle("income") + 
  labs(fill = "income") + 
  theme(legend.position      = c(0,1), 
        legend.justification = c(0,1))
p_income


test_vcrime <- test
test_vcrime$vcrime <- seq(0,1, length.out = 100)  
Y_pred <- SBF_simplex(as.matrix(test_vcrime), X_training, Y_training, h = optimal_h[k,])$Y_hat
colnames(Y_pred) <- colnames(Y)
df_vcrime <- as.data.frame(cbind(test_vcrime,Y_pred))
p_vcrime <- ggtern(data = df_vcrime, aes(x = race1, y = race2, z = race4, color = vcrime)) +
  geom_point(size=2) +
  scale_color_gradientn(colours = rainbow(3))+
  ggtitle("vcrime") + 
  labs(fill = "vcrime") + 
  theme(legend.position      = c(0,1), 
        legend.justification = c(0,1))
p_vcrime


test_temperature <- test
test_temperature$temperature <- seq(0,1, length.out = 100)  
Y_pred <- SBF_simplex(as.matrix(test_temperature), X_training, Y_training, h = optimal_h[k,])$Y_hat
colnames(Y_pred) <- colnames(Y)
df_temperature <- as.data.frame(cbind(test_temperature,Y_pred))
p_temperature <- ggtern(data = df_temperature, aes(x = race1, y = race2, z = race4, color = temperature)) +
  geom_point(size=2) +
  scale_color_gradientn(colours = rainbow(3))+
  ggtitle("temperature") + 
  labs(fill = "temperature") + 
  theme(legend.position      = c(0,1), 
        legend.justification = c(0,1))
p_temperature


test_precipitation <- test
test_precipitation$precipitation <- seq(0,1, length.out = 100)  
Y_pred <- SBF_simplex(as.matrix(test_precipitation), X_training, Y_training, h = optimal_h[k,])$Y_hat
colnames(Y_pred) <- colnames(Y)
df_precipitation <- as.data.frame(cbind(test_precipitation,Y_pred))
p_precipitation <- ggtern(data = df_precipitation, aes(x = race1, y = race2, z = race4, color = precipitation)) +
  geom_point(size=2) +
  scale_color_gradientn(colours = rainbow(3))+
  ggtitle("precipitation") + 
  labs(fill = "precipitation") + 
  theme(legend.position      = c(0,1), 
        legend.justification = c(0,1))
p_precipitation

