install.packages("ggtern")
library(ggtern)

load("C:/Users/user/Desktop/ARHR_error.Rdata")

dyn.load('C:/Users/user/Desktop/Add-Reg-Hilbert-Res/Dll files/CBS_continuous_simplex.dll') # path of the CBS_continuous_simplex.dll file
dyn.load('C:/Users/user/Desktop/Add-Reg-Hilbert-Res/Dll files/SBF_continuous_simplex.dll') # path of the CBS_continuous_simplex.dll file

summary(X)
mean_X <- apply(X,2,mean)
test <- as.data.frame(matrix(rep(mean_X, 100), nrow = 100, byrow = T))
colnames(test) <- colnames(X)

test_age <- test
test_age$med_age <- seq(0,1, length.out = 100)  
Y_pred <- SBF_simplex(as.matrix(test_age), X_training, Y_training, h = optimal_h[k,])$Y_hat
colnames(Y_pred) <- c("Caucasian", "African_American", "Mongoloid")
df_age <- as.data.frame(cbind(test_age,Y_pred))
p_age <- ggtern(data = df_age, aes(x = Caucasian, y = African_American, z = Mongoloid, color = med_age)) +
  geom_point(size=2) +
  scale_color_gradientn(colours = rainbow(3))+
  ggtitle("age") + 
  theme_showarrows() +
  labs(fill = "age") + 
  theme(legend.position      = c(0,1), 
        legend.justification = c(0,1)) + 
  tern_limits(T=0.2, L=1, R=0.2)
p_age




test_income <- test
test_income$income <- seq(0,1, length.out = 100)  
Y_pred <- SBF_simplex(as.matrix(test_income), X_training, Y_training, h = optimal_h[k,])$Y_hat
colnames(Y_pred) <- c("Caucasian", "African_American", "Mongoloid")
df_income <- as.data.frame(cbind(test_income,Y_pred))
p_income <- ggtern(data = df_income, aes(x = Caucasian, y = African_American, z = Mongoloid, color = income)) +
  geom_point(size=2) +
  scale_color_gradientn(colours = rainbow(3))+
  ggtitle("income") + 
  labs(fill = "income") + 
  theme_showarrows() +
  theme(legend.position      = c(0,1), 
        legend.justification = c(0,1)) +
  tern_limits(T=0.4, L=1, R=0.4)
p_income


test_vcrime <- test
test_vcrime$vcrime <- seq(0,1, length.out = 100)  
Y_pred <- SBF_simplex(as.matrix(test_vcrime), X_training, Y_training, h = optimal_h[k,])$Y_hat
colnames(Y_pred) <- c("Caucasian", "African_American", "Mongoloid")
df_vcrime <- as.data.frame(cbind(test_vcrime,Y_pred))
p_vcrime <- ggtern(data = df_vcrime, aes(x = Caucasian, y = African_American, z = Mongoloid, color = vcrime)) +
  geom_point(size=2) +
  scale_color_gradientn(colours = rainbow(3))+
  ggtitle("vcrime") + 
  labs(fill = "vcrime") + 
  theme_showarrows() +
  theme(legend.position      = c(0,1), 
        legend.justification = c(0,1)) +
  tern_limits(T=0.2, L=1, R=0.2)
p_vcrime


test_temperature <- test
test_temperature$temperature <- seq(0,1, length.out = 100)  
Y_pred <- SBF_simplex(as.matrix(test_temperature), X_training, Y_training, h = optimal_h[k,])$Y_hat
colnames(Y_pred) <- c("Caucasian", "African_American", "Mongoloid")
df_temperature <- as.data.frame(cbind(test_temperature,Y_pred))
p_temperature <- ggtern(data = df_temperature, aes(x = Caucasian, y = African_American, z = Mongoloid, color = temperature)) +
  geom_point(size=2) +
  scale_color_gradientn(colours = rainbow(3))+
  ggtitle("temperature") + 
  labs(fill = "temperature") + 
  theme_showarrows() +
  theme(legend.position      = c(0,1), 
        legend.justification = c(0,1)) +
  tern_limits(T=0.25, L=1, R=0.25)
p_temperature


test_precipitation <- test
test_precipitation$precipitation <- seq(0,1, length.out = 100)  
Y_pred <- SBF_simplex(as.matrix(test_precipitation), X_training, Y_training, h = optimal_h[k,])$Y_hat
colnames(Y_pred) <- c("Caucasian", "African_American", "Mongoloid")
df_precipitation <- as.data.frame(cbind(test_precipitation,Y_pred))
p_precipitation <- ggtern(data = df_precipitation, aes(x = Caucasian, y = African_American, z = Mongoloid, color = precipitation)) +
  geom_point(size=2) +
  scale_color_gradientn(colours = rainbow(3))+
  ggtitle("precipitation") + 
  theme_showarrows() +
  labs(fill = "precipitation") + 
  theme(legend.position      = c(0,1), 
        legend.justification = c(0,1)) +
  tern_limits(T=0.2, L=1, R=0.2)
p_precipitation
