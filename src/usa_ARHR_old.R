# codes from https://github.com/jeong-min-jeon/Add-Reg-Hilbert-Res

# Load necessary source file
source('Add-Reg-Hilbert-Res/Codes for election data/R_functions_for_proposed_method_for_simplex_responses.R') # path of the file

# Load necessary dll files
dyn.load('Add-Reg-Hilbert-Res/CBS_continuous_simplex.dll') # path of the CBS_continuous_simplex.dll file
dyn.load('Add-Reg-Hilbert-Res/SBF_continuous_simplex.dll') # path of the CBS_continuous_simplex.dll file

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

# For saving results
optimal_h = matrix(NA, nrow = nfolds, ncol = d)
Y_hat = array(NA, dim = c(nfolds, 25, T))
error = c()

# Get ASPE
for(k in 1:nfolds) {
  print(k)
  s = which(folds==k)
  X_test = X[s,]
  Y_test = Y[s,]
  X_training = X[-s,]
  Y_training = Y[-s,]
  optimal_h[k,] = CBS_simplex(X_training, Y_training, h_length = rep(41,d), h_add = rep(0.2, d))$optimal_h # Reduced bw grid for fast computation
  Y_hat[k, , ] = SBF_simplex(X_test, X_training, Y_training, h = optimal_h[k,])$Y_hat
  temp_dist=c()
  for(i in 1:nrow(Y_test)) {
    temp_dist[i] = comp_distance(as.vector(Y_test[i, ]), as.vector(Y_hat[k, i, ]))
  }
  error[k] = sum(temp_dist) / nrow(Y_test)
}

mean(error)