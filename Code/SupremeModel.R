library(MASS)
### Network structure learning ###
findU = function(x) quantile(x, probs = 0.975)
findL = function(x) quantile(x, probs = 0.025)
# Without any intervention
load("data/supreme_loglin_network_null.RData")
supreme_loglin.adj = matrix(0, 9, 9)
boot.mat = matrix(0, nrow = length(supreme_loglin_network_null), ncol = 46)
for(i in 1:length(supreme_loglin_network_null)){
  boot.mat[i,] = supreme_loglin_network_null[[i]]
}
main = colMeans(boot.mat)
boot.lb = apply(boot.mat, 2, findL)
boot.ub = apply(boot.mat, 2, findU)
boot.sd = apply(boot.mat ,2 , sd)
ind = ( (main - 1.96*boot.sd )*((main + 1.96*boot.sd )) > 0)[c(11:46)]
oneway = ( (main - 1.96*boot.sd )*((main + 1.96*boot.sd )) > 0)[c(2:10)]
supreme_loglin.adj[lower.tri(supreme_loglin.adj, diag = FALSE)] = supreme_loglin.adj[lower.tri(supreme_loglin.adj, diag = FALSE)] + ind
supreme_loglin.adj[upper.tri(supreme_loglin.adj, diag = FALSE)] = t(supreme_loglin.adj)[upper.tri(supreme_loglin.adj, diag = FALSE)]

Adj.case1 = supreme_loglin.adj
print(Adj.case1)
#1 - 2,3,4,5,7,8,9
#2 - 5,6,8,9
#3 - 5,6,9
#4 - 7
#6 - 8,9
#7 - 9
#8 - 9



### Parameter learning ###
data = read.csv("Data/longdata.csv", sep = ",", header = TRUE)
data = as.data.frame(data)
Y = 2*data[,c(3:11)]-3 # -1 or 1

# criminal procedure
A = ifelse(data$case.issue == 1, 1, -1)
#A = ifelse(data$case.issue == 2, 1, -1) # civil rights
#A = ifelse(data$case.issue == 8, 1, -1) # economic activity
#A = ifelse(data$case.issue == 9, 1, -1) # judicial power

input.Y = Y; input.A = A; 
input.Y = as.data.frame(input.Y)
for(i in 1:9){
  input.Y[,i] = as.factor(input.Y[,i])
}

input = as.data.frame(cbind(input.Y, as.factor(input.A)))
names(input) = c("Y1", "Y2", "Y3", "Y4", "Y5", "Y6", "Y7", "Y8", "Y9", "case")

# create a contingency table
tab = xtabs(~ Y1 + Y2 + Y3 + Y4 + Y5 + Y6 + Y7 + Y8 + Y9 + case, data = input)
main = loglm(~ Y1 + Y2 + Y3 + Y4 + Y5 + Y6 + Y7 + Y8 + Y9 + 
               Y1:Y2 + Y1:Y3 + Y1:Y4 + Y1:Y5 +  Y1:Y7 + Y1:Y8 + Y1:Y9 +
               Y2:Y5 + Y2:Y6 + Y2:Y8 + Y2:Y9 +
               Y3:Y5 + Y3:Y6 + Y3:Y9 + 
               Y4:Y7 +
               Y6:Y8 + Y6:Y9 +
               Y7:Y9 +
               Y8:Y9 +  
               case:Y1 + case:Y2 + case:Y3 + case:Y4 + case:Y5 + case:Y6 + case:Y7 + case:Y8 + case:Y9, iter=2000, data = tab)
supreme_loglin_fit = c(main$param[[1]], main$param$Y1[2], main$param$Y2[2], main$param$Y3[2], main$param$Y4[2],
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
                        main$param$Y1.case[1], main$param$Y2.case[1], main$param$Y3.case[1], main$param$Y4.case[1], main$param$Y5.case[1],
                        main$param$Y6.case[1], main$param$Y7.case[1], main$param$Y8.case[1], main$param$Y9.case[1])
save(supreme_loglin_fit, file = "Data/supreme_loglin_fit1.RData")
# save(supreme_loglin_fit, file = "Data/supreme_loglin_fit2.RData")
# save(supreme_loglin_fit, file = "Data/supreme_loglin_fit8.RData")
# save(supreme_loglin_fit, file = "Data/supreme_loglin_fit9.RData")

# bootstrap sample (nb = 1000)
nb = 1000 # the number of bootstrap samples
supreme_loglin_boot2 =
for(ii in 1:nb){
  set.seed(ii)
  indbt = sample(1:nrow(Y), replace=TRUE)
  input.Y = Y[indbt,]; input.A = A[indbt]; 
  input.Y = as.data.frame(input.Y)
  for(i in 1:9){
    input.Y[,i] = as.factor(input.Y[,i])
  }
  
  input = as.data.frame(cbind(input.Y, as.factor(input.A)))
  names(input) = c("Y1", "Y2", "Y3", "Y4", "Y5", "Y6", "Y7", "Y8", "Y9", "case")
  
  tab = xtabs(~   Y1 + Y2 + Y3 + Y4 + Y5 + Y6 + Y7 + Y8 + Y9 + case, data = input)
  main = loglm(~  Y1 + Y2 + Y3 + Y4 + Y5 + Y6 + Y7 + Y8 + Y9 + 
                 Y1:Y2 + Y1:Y3 + Y1:Y4 + Y1:Y5 +  Y1:Y7 + Y1:Y8 + Y1:Y9 +
                 Y2:Y5 + Y2:Y6 + Y2:Y8 + Y2:Y9 +
                 Y3:Y5 + Y3:Y6 + Y3:Y9 + 
                 Y4:Y7 +
                 Y6:Y8 + Y6:Y9 +
                 Y7:Y9 +
                 Y8:Y9 +  
                 case:Y1 + case:Y2 + case:Y3 + case:Y4 + case:Y5 + case:Y6 + case:Y7 + case:Y8 + case:Y9, iter=2000, data = tab)
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
                 main$param$Y1.case[1], main$param$Y2.case[1], main$param$Y3.case[1], main$param$Y4.case[1], main$param$Y5.case[1],
                 main$param$Y6.case[1], main$param$Y7.case[1], main$param$Y8.case[1], main$param$Y9.case[1])
  
  supreme_loglin_boot1[[ii]] =  main.param
}

save(supreme_loglin_boot1, file = "Data/supreme_loglin_boot1.RData")
# save(supreme_loglin_boot2, file = "Data/supreme_loglin_boot2.RData")
# save(supreme_loglin_boot8, file = "Data/supreme_loglin_boot8.RData")
# save(supreme_loglin_boot9, file = "Data/supreme_loglin_boot9.RData")


save(supreme_loglin_boot2, file = "supreme_loglin_boot2.RData")