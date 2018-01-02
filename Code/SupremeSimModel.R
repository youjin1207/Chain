library(Matrix)
library(MASS)
## learning main effect and interaction effect from the real data
data = read.csv("Data/longdata.csv", sep = ",", header = TRUE)
data = as.data.frame(data)
input.Y = 2*data[,c(3:11)]-3 ; 
input.Y = as.data.frame(input.Y)
for(i in 1:9){
  input.Y[,i] = as.factor(input.Y[,i])
}

input = as.data.frame(input.Y)
names(input) = c("Y1", "Y2", "Y3", "Y4", "Y5", "Y6", "Y7", "Y8", "Y9")

## log-linear model without any intervention
tab = xtabs(~   Y1 + Y2 + Y3 + Y4 + Y5 + Y6 + Y7 + Y8 + Y9, data = input)
main = loglm(~  Y1 + Y2 + Y3 + Y4 + Y5 + Y6 + Y7 + Y8 + Y9 + 
               Y1:Y2 + Y1:Y3 + Y1:Y4 + Y1:Y5 +  Y1:Y7 + Y1:Y8 + Y1:Y9 +
               Y2:Y5 + Y2:Y6 + Y2:Y8 + Y2:Y9 +
               Y3:Y5 + Y3:Y6 + Y3:Y9 + 
               Y4:Y7 +
               Y6:Y8 + Y6:Y9 +
               Y7:Y9 +
               Y8:Y9, iter=2000, data = tab)
main.param = c(main$param[[1]], main$param$Y1[2], main$param$Y2[2], main$param$Y3[2], main$param$Y4[2],
               main$param$Y5[2], main$param$Y6[2], main$param$Y7[2], main$param$Y8[2], main$param$Y9[2],
               main$param$Y1.Y2[1], main$param$Y1.Y3[1], main$param$Y1.Y4[1], main$param$Y1.Y5[1],
               main$param$Y1.Y7[1], main$param$Y1.Y8[1], main$param$Y1.Y9[1],
               main$param$Y2.Y5[1],
               main$param$Y2.Y6[1], main$param$Y2.Y8[1], main$param$Y2.Y9[1],
               main$param$Y3.Y5[1],
               main$param$Y3.Y6[1], main$param$Y3.Y9[1],
               main$param$Y4.Y7[1], 
               main$param$Y6.Y8[1], main$param$Y6.Y9[1],
               main$param$Y7.Y9[1], main$param$Y8.Y9[1])

### generate model ###
set.seed(1)
Adj = matrix(0, 9, 9)
Adj[1, c(2,3,4,5,7,8,9)] = 1
Adj[2, c(5,6,8,9)] = 1
Adj[3, c(5,6,9)] = 1
Adj[4,7] = 1; Adj[6,c(8,9)] = 1; Adj[7,9] = 1; Adj[8,9] = 1

G = graph.adjacency(Adj, "undirected")
Adj = as.matrix(get.adjacency(G))
n.obs = 1000

logistic = function(x) exp(x) / (1 + exp(x))

############ 1. Generate Chain Graph using Gibbs-sampler ############
supreme_Gibbs = list()
for(ii in 1:1000){
  max.time = 10000
  n.obs = 1000
  stop.time = c()

  Y = matrix(0, n.obs, 9)
  A = matrix(0, n.obs, 9)
  for(k in 1:n.obs){
    
    A[k,] = rbinom(9, 1, 0.5)
  
    out = matrix(0, 9, max.time) # probability
    outcome = matrix(0, 9, max.time)
    
    t = 1
    out[,t] = main.param[2:10]
    outcome[,t] = 2*rbinom(9, 1, logistic(out[,t]))-1 # random initial value
    #outcome[,t] = ifelse(main.param[2:10] > 0, 1, -1)
    
    for(t in 2:max.time){
      random.justice = sample(1:9, 1)
      if(random.justice == 1){    
        # WHRehnquist
        out[1,t] = main$param$Y1[2] + 5*outcome[1, (t-1)] + 0.5*A[k,1] + main$param$Y1.Y2[1]*outcome[2,(t-1)] + 
          main$param$Y1.Y3[1]*outcome[3,(t-1)] + main$param$Y1.Y4[1]*outcome[4,(t-1)] + 
          main$param$Y1.Y5[1]*outcome[5,(t-1)] + 
          main$param$Y1.Y7[1]*outcome[7,(t-1)] + main$param$Y1.Y8[1]*outcome[8,(t-1)] + 
          main$param$Y1.Y9[1]*outcome[9,(t-1)] + rnorm(1, 0, 0.1)
      }else if(random.justice == 2){
        # JPStevens
        out[2,t] = main$param$Y2[2] + 5*outcome[2, (t-1)] + 0.5*A[k,2] + main$param$Y1.Y2[1]*outcome[1,(t-1)] + 
          main$param$Y2.Y5[1]*outcome[5,(t-1)] + main$param$Y2.Y6[1]*outcome[6,(t-1)] + 
          main$param$Y2.Y8[1]*outcome[8,(t-1)] + main$param$Y2.Y9[1]*outcome[9,(t-1)] + rnorm(1, 0, 0.1)            
      }else if(random.justice == 3){ 
        # DOConnor
        out[3,t] = main$param$Y3[2] + 5*outcome[3, (t-1)] + 0.5*A[k,3] + main$param$Y1.Y3[1]*outcome[1,(t-1)] + 
          main$param$Y3.Y5[1]*outcome[5,(t-1)] + main$param$Y3.Y6[1]*outcome[6,(t-1)] + 
          main$param$Y3.Y9[1]*outcome[9,(t-1)] + rnorm(1, 0, 0.1)
      }else if(random.justice == 4){
        # AScalia
        out[4,t] = main$param$Y4[2] + 5*outcome[4, (t-1)] + 0.5*A[k,4] + main$param$Y1.Y4[1]*outcome[1,(t-1)] + 
          main$param$Y4.Y7[1]*outcome[7,(t-1)] + rnorm(1, 0, 0.1)   
      }else if(random.justice == 5){
        # AMKenney
        out[5,t] = main$param$Y5[2] + 5*outcome[5, (t-1)] + 0.5*A[k,5] + main$param$Y1.Y5[1]*outcome[1,(t-1)] + 
          main$param$Y2.Y5[1]*outcome[2,(t-1)] + main$param$Y3.Y5[1]*outcome[3,(t-1)] + rnorm(1, 0, 0.1)                                        
      }else if(random.justice == 6){
        # DHSouter
        out[6,t] = main$param$Y6[2] + 5*outcome[6, (t-1)] + 0.5*A[k,6] + 
          main$param$Y2.Y6[1]*outcome[2,(t-1)] + main$param$Y3.Y6[1]*outcome[3,(t-1)] +  
          main$param$Y6.Y8[1]*outcome[8,(t-1)] + main$param$Y6.Y9[1]*outcome[9,(t-1)] + rnorm(1, 0, 0.1)  
      }else if(random.justice == 7){
        # CThomas
        out[7,t] = main$param$Y7[2] + 5*outcome[7, (t-1)] + 0.5*A[k,7] + main$param$Y1.Y7[1]*outcome[1,(t-1)] + 
          main$param$Y4.Y7[1]*outcome[4,(t-1)] + main$param$Y7.Y9[1]*outcome[9,(t-1)] + rnorm(1, 0, 0.1) 
      }else if(random.justice == 8){
        # RBGinsburg
        out[8,t] = main$param$Y8[2] + 5*outcome[8, (t-1)] + 0.5*A[k,8] + main$param$Y1.Y8[1]*outcome[1,(t-1)] + 
          main$param$Y2.Y8[1]*outcome[2,(t-1)] + main$param$Y6.Y8[1]*outcome[6,(t-1)] + 
          main$param$Y8.Y9[1]*outcome[9,(t-1)] + rnorm(1, 0, 0.1) 
      }else if(random.justice == 9){
        # SGBreyer
        out[9,t] = main$param$Y9[2] + 5*outcome[9, (t-1)] + 0.5*A[k,9] + main$param$Y1.Y9[1]*outcome[1,(t-1)] + 
          main$param$Y2.Y9[1]*outcome[2,(t-1)] + main$param$Y3.Y9[1]*outcome[3,(t-1)] + 
          main$param$Y6.Y9[1]*outcome[6,(t-1)] + main$param$Y7.Y9[1]*outcome[7,(t-1)] + 
          main$param$Y8.Y9[1]*outcome[8,(t-1)] + rnorm(1, 0, 0.1) 
      }                                                
      outcome[,t] = outcome[,(t-1)]                                        
      outcome[random.justice,t] = 2*rbinom(1, 1, logistic(out[random.justice,t]))- 1
      
      if(t > 1100){
        if(sum(rowMeans(outcome[,c( (t-100):t) ]) == outcome[,t])==9) break
      }
    }
    
    
    Y[k,] = outcome[,t]
    stop.time[k] = t
  }
  
  supreme_Gibbs[[ii]] = list(Y, A, stop.time)
}
save(supreme_Gibbs, file = "Data/supreme_Gibbs.RData")


############ 2. Generate Chain Graph using DAG Approximation ############
supreme_DAG = list()
for(ii in 1:1000){
  #### DAG ######
  Y = matrix(0, n.obs, 9)
  A = matrix(0, n.obs, 9)
  for(k in 1:n.obs){
    #set.seed(k)
    time = 100
    A[k,] = rbinom(9, 1, 0.5)
    #A[k,] = 2*A[k,] - 1
    out = matrix(0, 9, time) # probability
    outcome = matrix(0, 9, time)
    t = 1
    out[,t] = main.param[2:10]
    outcome[,t] = 2*rbinom(9, 1, logistic(out[,t]))-1
    for(t in 2:time){
      
      # WHRehnquist
      out[1,t] = main$param$Y1[2] + 5*outcome[1, (t-1)] + 0.5*A[k, 1] + main$param$Y1.Y2[1]*outcome[2,(t-1)] + 
        main$param$Y1.Y3[1]*outcome[3,(t-1)] + main$param$Y1.Y4[1]*outcome[4,(t-1)] + 
        main$param$Y1.Y5[1]*outcome[5,(t-1)] + 
        main$param$Y1.Y7[1]*outcome[7,(t-1)] + main$param$Y1.Y8[1]*outcome[8,(t-1)] + 
        main$param$Y1.Y9[1]*outcome[9,(t-1)] + rnorm(1, 0, 0.1)
      
      # JPStevens
      out[2,t] = main$param$Y2[2] + 5*outcome[2, (t-1)] + 0.5*A[k, 2] + main$param$Y1.Y2[1]*outcome[1,(t-1)] + 
        main$param$Y2.Y5[1]*outcome[5,(t-1)] + main$param$Y2.Y6[1]*outcome[6,(t-1)] + 
        main$param$Y2.Y8[1]*outcome[8,(t-1)] + main$param$Y2.Y9[1]*outcome[9,(t-1)] + rnorm(1, 0, 0.1)            
      
      # DOConnor
      out[3,t] = main$param$Y3[2] + 5*outcome[3, (t-1)] + 0.5*A[k, 3] + main$param$Y1.Y3[1]*outcome[1,(t-1)] + 
        main$param$Y3.Y5[1]*outcome[5,(t-1)] + main$param$Y3.Y6[1]*outcome[6,(t-1)] + 
        main$param$Y3.Y9[1]*outcome[9,(t-1)] + rnorm(1, 0, 0.1)
      
      # AScalia
      out[4,t] = main$param$Y4[2] + 5*outcome[4, (t-1)] + 0.5*A[k, 4] + main$param$Y1.Y4[1]*outcome[1,(t-1)] + 
        main$param$Y4.Y7[1]*outcome[7,(t-1)] + rnorm(1, 0, 0.1)   
      
      # AMKenney
      out[5,t] = main$param$Y5[2] + 5*outcome[5, (t-1)] + 0.5*A[k, 5] + main$param$Y1.Y5[1]*outcome[1,(t-1)] + 
        main$param$Y2.Y5[1]*outcome[2,(t-1)] + main$param$Y3.Y5[1]*outcome[3,(t-1)] + rnorm(1, 0, 0.1)                                        
      
      # DHSouter
      out[6,t] = main$param$Y6[2] + 5*outcome[6, (t-1)] + 0.5*A[k, 6] + 
        main$param$Y2.Y6[1]*outcome[2,(t-1)] + main$param$Y3.Y6[1]*outcome[3,(t-1)] +  
        main$param$Y6.Y8[1]*outcome[8,(t-1)] + main$param$Y6.Y9[1]*outcome[9,(t-1)] + rnorm(1, 0, 0.1)  
      
      # CThomas
      out[7,t] = main$param$Y7[2] + 5*outcome[7, (t-1)] + 0.5*A[k, 7] + main$param$Y1.Y7[1]*outcome[1,(t-1)] + 
        main$param$Y4.Y7[1]*outcome[4,(t-1)] + main$param$Y7.Y9[1]*outcome[9,(t-1)] + rnorm(1, 0, 0.1) 
      
      # RBGinsburg
      out[8,t] = main$param$Y8[2] + 5*outcome[8, (t-1)] + 0.5*A[k, 8] + main$param$Y1.Y8[1]*outcome[1,(t-1)] + 
        main$param$Y2.Y8[1]*outcome[2,(t-1)] + main$param$Y6.Y8[1]*outcome[6,(t-1)] + 
        main$param$Y8.Y9[1]*outcome[9,(t-1)] + rnorm(1, 0, 0.1) 
      
      # SGBreyer
      out[9,t] = main$param$Y9[2] + 5*outcome[9, (t-1)] + 0.5*A[k, 9] + main$param$Y1.Y9[1]*outcome[1,(t-1)] + 
        main$param$Y2.Y9[1]*outcome[2,(t-1)] + main$param$Y3.Y9[1]*outcome[3,(t-1)] + 
        main$param$Y6.Y9[1]*outcome[6,(t-1)] + main$param$Y7.Y9[1]*outcome[7,(t-1)] + 
        main$param$Y8.Y9[1]*outcome[8,(t-1)] + rnorm(1, 0, 0.1) 
      
      
      
      outcome[,t] = rbinom(9, 1, logistic(out[,t])) 
      outcome[,t] = 2*outcome[,t] - 1
    }
    Y[k,] = outcome[,t]
  }
  
  supreme_DAG[[ii]] =  list(Y, A)
}
save(supreme_DAG, file = "Data/supreme_DAG.RData")


###########################################################################
###########################################################################
######## 3. Fit log-linear model using data from 1.Gibbs sampler ##########
load("Data/supreme_Gibbs.RData")
supreme_loglin_Gibbs = list()
for(ii in 1:1000){
  set.seed(ii)
  input.Y = supreme_Gibbs[[ii]][[1]]
  input.A = supreme_Gibbs[[ii]][[2]]
  
  input.Y = as.data.frame(input.Y); input.A = as.data.frame(input.A)
  for(i in 1:9){
    input.Y[,i] = as.factor(input.Y[,i])
    input.A[,i] = as.factor(input.A[,i])
  }
  
  input = as.data.frame(cbind(input.Y, input.A))
  names(input) = c("Y1", "Y2", "Y3", "Y4", "Y5", "Y6", "Y7", "Y8", "Y9", 
                   "A1", "A2", "A3", "A4", "A5", "A6", "A7", "A8", "A9")
  
  tab = xtabs(~  Y1 + Y2 + Y3 + Y4 + Y5 + Y6 + Y7 + Y8 + Y9 + 
                A1 + A2 + A3 + A4 + A5 + A6 + A7 + A8 + A9 , data = input)
  main = loglm(~  Y1 + Y2 + Y3 + Y4 + Y5 + Y6 + Y7 + Y8 + Y9 + 
                 Y1:Y2 + Y1:Y3 + Y1:Y4 + Y1:Y5 +  Y1:Y7 + Y1:Y8 + Y1:Y9 +
                 Y2:Y5 + Y2:Y6 + Y2:Y8 + Y2:Y9 +
                 Y3:Y5 + Y3:Y6 + Y3:Y9 + 
                 Y4:Y7 +
                 Y6:Y8 + Y6:Y9 +
                 Y7:Y9 +
                 Y8:Y9 +  
                 A1:Y1 + A2:Y2 + A3:Y3 + A4:Y4 + A5:Y5 + A6:Y6 + A7:Y7 + A8:Y8 + A9:Y9, data = tab)
  main.param = c(main$param[[1]], main$param$Y1[2], main$param$Y2[2], main$param$Y3[2], main$param$Y4[2],
                 main$param$Y5[2], main$param$Y6[2], main$param$Y7[2], main$param$Y8[2], main$param$Y9[2],
                 main$param$Y1.Y2[1], main$param$Y1.Y3[1], main$param$Y1.Y4[1], main$param$Y1.Y5[1],
                 main$param$Y1.Y7[1], main$param$Y1.Y8[1], main$param$Y1.Y9[1],
                 main$param$Y2.Y5[1],
                 main$param$Y2.Y6[1], main$param$Y2.Y8[1], main$param$Y2.Y9[1],
                 main$param$Y3.Y5[1],
                 main$param$Y3.Y6[1], main$param$Y3.Y9[1],
                 main$param$Y4.Y7[1], 
                 main$param$Y6.Y8[1], main$param$Y6.Y9[1],
                 main$param$Y7.Y9[1], main$param$Y8.Y9[1],
                 main$param$Y1.A1[1], main$param$Y2.A2[1], main$param$Y3.A3[1], main$param$Y4.A4[1], main$param$Y5.A5[1],
                 main$param$Y6.A6[1], main$param$Y7.A7[1], main$param$Y8.A8[1], main$param$Y9.A9[1])
  
  supreme_loglin_Gibbs[[ii]]  = main.param
}

save(supreme_loglin_Gibbs, file = "Data/supreme_loglin_Gibbs.RData")


######## 3. Fit log-linear model using data from 1.Gibbs sampler ##########
load("Data/supreme_DAG.RData")
supreme_loglin_DAG = list()
for(ii in 1:1000){
  set.seed(ii)
  input.Y = supreme_DAG[[ii]][[1]]
  input.A = supreme_DAG[[ii]][[2]]
  
  input.Y = as.data.frame(input.Y); input.A = as.data.frame(input.A)
  for(i in 1:9){
    input.Y[,i] = as.factor(input.Y[,i])
    input.A[,i] = as.factor(input.A[,i])
  }
  
  input = as.data.frame(cbind(input.Y, input.A))
  names(input) = c("Y1", "Y2", "Y3", "Y4", "Y5", "Y6", "Y7", "Y8", "Y9", 
                   "A1", "A2", "A3", "A4", "A5", "A6", "A7", "A8", "A9")
  
  tab = xtabs(~  Y1 + Y2 + Y3 + Y4 + Y5 + Y6 + Y7 + Y8 + Y9 + 
                A1 + A2 + A3 + A4 + A5 + A6 + A7 + A8 + A9 , data = input)
  main = loglm(~  Y1 + Y2 + Y3 + Y4 + Y5 + Y6 + Y7 + Y8 + Y9 + 
                 Y1:Y2 + Y1:Y3 + Y1:Y4 + Y1:Y5 +  Y1:Y7 + Y1:Y8 + Y1:Y9 +
                 Y2:Y5 + Y2:Y6 + Y2:Y8 + Y2:Y9 +
                 Y3:Y5 + Y3:Y6 + Y3:Y9 + 
                 Y4:Y7 +
                 Y6:Y8 + Y6:Y9 +
                 Y7:Y9 +
                 Y8:Y9 +  
                 A1:Y1 + A2:Y2 + A3:Y3 + A4:Y4 + A5:Y5 + A6:Y6 + A7:Y7 + A8:Y8 + A9:Y9, data = tab)
  main.param = c(main$param[[1]], main$param$Y1[2], main$param$Y2[2], main$param$Y3[2], main$param$Y4[2],
                 main$param$Y5[2], main$param$Y6[2], main$param$Y7[2], main$param$Y8[2], main$param$Y9[2],
                 main$param$Y1.Y2[1], main$param$Y1.Y3[1], main$param$Y1.Y4[1], main$param$Y1.Y5[1],
                 main$param$Y1.Y7[1], main$param$Y1.Y8[1], main$param$Y1.Y9[1],
                 main$param$Y2.Y5[1],
                 main$param$Y2.Y6[1], main$param$Y2.Y8[1], main$param$Y2.Y9[1],
                 main$param$Y3.Y5[1],
                 main$param$Y3.Y6[1], main$param$Y3.Y9[1],
                 main$param$Y4.Y7[1], 
                 main$param$Y6.Y8[1], main$param$Y6.Y9[1],
                 main$param$Y7.Y9[1], main$param$Y8.Y9[1],
                 main$param$Y1.A1[1], main$param$Y2.A2[1], main$param$Y3.A3[1], main$param$Y4.A4[1], main$param$Y5.A5[1],
                 main$param$Y6.A6[1], main$param$Y7.A7[1], main$param$Y8.A8[1], main$param$Y9.A9[1])
  
  supreme_loglin_DAG[[ii]]  = main.param
}
save(supreme_loglin_DAG, file = "Data/supreme_loglin_DAG.RData")