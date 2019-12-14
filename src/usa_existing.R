# load libraries
library(Compositional)

load('src/usa_data.Rdata')

# Function for computing square norm of y1 minus y2
# y1 and y2: compositional vectors
comp_distance = function(y1, y2) {
  D = length(y1)
  dist = matrix(NA, D, D)
  for (i in 1:D) {
    for (j in 1:D) {
        a1 = ifelse(y1[i] == y1[j], 0, log(y1[i]) - log(y1[j]))
        a2 = ifelse(y2[i] == y2[j], 0, log(y2[i]) - log(y2[j]))
        dist[i, j] = (a1 - a2)^2
    }
  }
  sum(dist)/2/D
}

# Index of 10 partitions of sample
nfolds = 10
n = nrow(X)
d = ncol(X)
folds <- cut(1:n, breaks = nfolds, labels = FALSE)

# Dirichlet regression
error_diri = c()
for (k in 1:nfolds) {
  s = which(folds==k)
  X_test = X[s,]
  Y_test = Y[s,]
  X_training = X[-s,]
  Y_training = Y[-s,]
  Y_hat = diri.reg(y=Y_training, x=X_training, xnew=X_test)$est
  temp_dist = c()
  for(i in 1:nrow(Y_hat)) {
    temp_dist[i] = comp_distance(as.vector(Y_test[i,]), as.vector(Y_hat[i,]))
  }
  error_diri[k] = sum(temp_dist)/nrow(Y_hat)
}

# Nonlinear regression; ols
error_ols = c()
for (k in 1:nfolds) {
  s = which(folds == k)
  X_test = X[s, ]
  Y_test = Y[s, ]
  X_training = X[-s, ]
  Y_training = Y[-s, ]
  Y_hat = ols.compreg(y = Y_training, x = X_training, xnew = X_test)$est
  temp_dist = c()
  for(i in 1:nrow(Y_hat)) {
    temp_dist[i] = comp_distance(as.vector(Y_test[i,]), as.vector(Y_hat[i,]))
  }
  error_ols[k] = sum(temp_dist)/nrow(Y_hat)
} 

# Kullback-Leibler-divergence-based regression
error_kl=c()
for(k in 1:nfolds)
{
  s=which(folds==k)
  X_test=X[s,]
  Y_test=Y[s,]
  X_training=X[-s,]
  Y_training=Y[-s,]
  Y_hat=kl.compreg(y=Y_training, x=X_training, xnew=X_test)$est
  temp_dist=c()
  for(i in 1:nrow(Y_hat))
  {
    temp_dist[i]=comp_distance(as.vector(Y_test[i,]),as.vector(Y_hat[i,]))
  }
  error_kl[k]=sum(temp_dist)/nrow(Y_hat)
}

# Alpha transformation method by Tsagris (2015)
optimal_alpha=c()
error_alpha=c()
for(k in 1:nfolds) {
  print(k)
  s=which(folds==k)
  X_test=X[s,]
  Y_test=Y[s,]
  X_training=X[-s,]
  Y_training=Y[-s,]
  optimal_alpha[k] = alfareg.tune(y=Y_training, x=X_training, a=seq(-1, 1, by = 0.2), nfolds = 5)$opt # K -> nfolds (updated); fold = 5
  Y_hat=alfa.reg(y=Y_training, x=X_training, a=optimal_alpha[k], xnew=X_test)$est
  temp_dist=c()
  for(i in 1:nrow(Y_hat))
  {
    temp_dist[i]=comp_distance(as.vector(Y_test[i,]),as.vector(Y_hat[i,]))
  }
  error_alpha[k]=sum(temp_dist)/nrow(Y_hat)
}

aspe_diri = mean(error_diri)
aspe_ols = mean(error_ols)
aspe_kl = mean(error_kl)
aspe_alpha = mean(error_alpha)

save(list = c('aspe_diri', 'aspe_ols', 'aspe_kl', 'aspe_alpha', 'optimal_alpha'), file = "src/aspe_existing.Rdata")

# > aspe_diri
# [1] 0.7118787
# > aspe_ols
# [1] 100.0437
# > aspe_kl
# [1] 0.7314985
# > aspe_alpha
# [1] 0.512279
# > optimal_alpha
#  [1] -1 -1 -1 -1 -1 -1 -1 -1 -1 -1
