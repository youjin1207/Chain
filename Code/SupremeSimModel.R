library(Matrix)
library(MASS)
library(igraph)
source("Code/nine_hiton.cpp")
## learning main effect and interaction effect from the real data
data = read.csv("longdata.csv", sep = ",", header = TRUE)
data = as.data.frame(data)
input.Y = 2*data[,c(3:11)]-3 ; 
input.Y = as.data.frame(input.Y)
for(i in 1:9){
  input.Y[,i] = as.factor(input.Y[,i])
}

input = as.data.frame(input.Y)
names(input) = c("Y1", "Y2", "Y3", "Y4", "Y5", "Y6", "Y7", "Y8", "Y9")

## use the underlying network learned by hiton algorithm 
tab = xtabs(~ Y1 + Y2 + Y3 + Y4 + Y5 + Y6 + Y7 + Y8 + Y9, data = input)
main = loglm(~ Y1 + Y2 + Y3 + Y4 + Y5 + Y6 + Y7 + Y8 + Y9 + 
               Y1:Y3 + Y1:Y4 + Y1:Y5 + Y1:Y7 +
               Y2:Y6 + Y2:Y8 + Y2:Y9 +
               Y3:Y5 + Y3:Y6 + Y3:Y9 + 
               Y4:Y5 + Y4:Y7 +
               Y5:Y6 + Y5:Y7 + Y5:Y8 +
               Y6:Y8 + Y6:Y9 +
               Y8:Y9, data = tab)
weight.matrix = matrix(0, 9, 9)

diag(weight.matrix) = c(main$param$Y1[2], main$param$Y2[2], main$param$Y3[2], main$param$Y4[2],
                        main$param$Y5[2], main$param$Y6[2], main$param$Y7[2], main$param$Y8[2], main$param$Y9[2])

weight.matrix[1,c(3,4,5,7)] = c(main$param$Y1.Y3[1], main$param$Y1.Y4[1], main$param$Y1.Y5[1], main$param$Y1.Y7[1])
weight.matrix[2,c(6,8,9)] = c(main$param$Y2.Y6[1], main$param$Y2.Y8[1], main$param$Y2.Y9[1])
weight.matrix[3,c(5,7,9)] = c(main$param$Y3.Y5[1], main$param$Y3.Y6[1], main$param$Y3.Y9[1])
weight.matrix[4,c(5,7)] =  c(main$param$Y4.Y5[1], main$param$Y4.Y7[1])
weight.matrix[5,c(6,7,8)] = c(main$param$Y5.Y6[1], main$param$Y5.Y7[1], main$param$Y5.Y8[1])
weight.matrix[6,c(8,9)] = c(main$param$Y6.Y8[1], main$param$Y6.Y9[1])
weight.matrix[8,9] = main$param$Y8.Y9[1]
weight.matrix = Matrix::forceSymmetric(weight.matrix, uplo="U")
weight.matrix = as.matrix(weight.matrix)
Adj = matrix(0, 9, 9)
Adj[1,c(3,4,5,7)] = Adj[2,c(6,8,9)] = Adj[3,c(5,7,9)] = Adj[4, c(5,7)] = Adj[5, c(6,7,8)] = Adj[6,c(8,9)] = Adj[8,9] = 1
Adj = as.matrix(Matrix::forceSymmetric(Adj, uplo="U"))

n.obs = 2000
max.time = 100

## generate the simulated data using random gibbs sampler
Y = A = X = matrix(0, n.obs, 9)
alpha = beta = 1 # alpha, beta = 1,2
for(i in 1:n.obs){
  cov.out = cov = matrix(0, 9, max.time)
  treat.out = treat = matrix(0, 9, max.time)
  out = outcome = matrix(0, 9, max.time) 
  t = 1
  cov[,t] = rbinom(9, 1, 0.5)
  treat[,t] = rbinom(9, 1, 0.3)
  out[,t] = 1*diag(weight.matrix)
  outcome[,t] = 2*rbinom(9, 1, plogis(out[,t]))-1 # random initial value
  
  ## confounders 
  for(t in 2:max.time){
    for(j in 1:9){
      cov.out[j,t] =  -0.5 - 0.2*sum(cov[which(Adj[j,]==1),(t-1)])
      cov[j,t] = sample( c(cov[j,t-1], rbinom(1, 1, plogis(cov.out[j,t]))), 1,
                         prob = c(0.8, 0.2))
    }
  }
  X[i,] = cov[,max.time]
  ## treatment
  for(t in 2:max.time){
    for(j in 1:9){
      treat.out[j,t] = -0.5 - 0.2*sum(X[i, which(Adj[j,]==1)]) - 0.3*X[i,j] +
        0.1*sum(treat[which(Adj[j,]==1),(t-1)])
      treat[j,t] = sample( c(treat[j,t-1], rbinom(1, 1, plogis(treat.out[j,t]))), 1,
                           prob = c(0.8, 0.2))
    }
  }
  A[i,] = treat[,max.time]
  ##  outcomes
  for(t in 2:max.time){ 
    out[1,t] = alpha*weight.matrix[1,1] + beta*sum(weight.matrix[1,-1]*outcome[-1,(t-1)]) + 0.5*A[i,1] - 0.2*X[i,1] 
    # JPStevens
    out[2,t] = alpha*weight.matrix[2,2] + beta*sum(weight.matrix[2,-2]*outcome[-2,(t-1)]) + 0.5*A[i,2] - 0.2*X[i,2]   
    # DOConnor
    out[3,t] = alpha*weight.matrix[3,3] + beta*sum(weight.matrix[3,-3]*outcome[-3,(t-1)]) + 0.5*A[i,3] - 0.2*X[i,3] 
    # AScalia
    out[4,t] = alpha*weight.matrix[4,4] + beta*sum(weight.matrix[4,-4]*outcome[-4,(t-1)]) + 0.5*A[i,4] - 0.2*X[i,4] 
    # AMKenney
    out[5,t] = alpha*weight.matrix[5,5] + beta*sum(weight.matrix[5,-5]*outcome[-5,(t-1)]) + 0.5*A[i,5] - 0.2*X[i,5]                                    
    # DHSouter
    out[6,t] = alpha*weight.matrix[6,6] + beta*sum(weight.matrix[6,-6]*outcome[-6,(t-1)]) + 0.5*A[i,6] - 0.2*X[i,6] 
    # CThomas
    out[7,t] = alpha*weight.matrix[7,7] + beta*sum(weight.matrix[7,-7]*outcome[-7,(t-1)])+ 0.5*A[i,7] - 0.2*X[i,7] 
    # RBGinsburg
    out[8,t] = alpha*weight.matrix[8,8] + beta*sum(weight.matrix[8,-8]*outcome[-8,(t-1)]) + 0.5*A[i,8] - 0.2*X[i,8] 
    # SGBreyer
    out[9,t] = alpha*weight.matrix[9,9] + beta*sum(weight.matrix[9,-9]*outcome[-9,(t-1)]) + 0.5*A[i,9] - 0.2*X[i,9] 
    
    for(j in 1:9){
      outcome[j,t] = sample( c(outcome[j,t-1], 2*rbinom(1, 1, plogis(out[j,t]))-1), 1,
                             prob = c(0.8, 0.2))
    }
  }
  Y[i,] = outcome[,max.time]
}


## fit log-linear model for two conditional densities 
## dependent on "nine_hiton.cpp" function
nine.permute = permutations(n=2, r=9, c(-1,1), repeats.allowed=T)
input.Y = Y
input.A = A
input.X = X
main.param = optim(par = rep(0, 45), loglikeninecov, inputY = input.Y, 
                   inputA = input.A, inputX = input.X, permutetab = nine.permute, hessian = FALSE, method = "L-BFGS-B")  
