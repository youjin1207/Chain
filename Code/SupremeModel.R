library(MASS)
library(gtools)
library(bnlearn)
## learn chain component 
data = read.csv("Data/longdata.csv", sep = ",", header = TRUE)
data = as.data.frame(data)
Y = 2*data[,c(3:11)]-3 # 1 or -1
bnlearn.fit = si.hiton.pc(Y, cluster = NULL, whitelist = NULL, blacklist = NULL, test = NULL,
                   alpha = 0.05, B = NULL, max.sx = NULL, debug = FALSE, undirected = TRUE)
Adj = diag(9) # adjacent matrix
for(i in 1:9){
  Adj[i,] =  as.numeric(names(Y) %in% bnlearn.fit$nodes[[i]]$mb)
}

## estimate a chain graph model with a single treatment of case issue
## fit a separate model for four different issue areas
A = ifelse(data$case.issue == 1, 1, -1) # criminal procedure
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
               Y1:Y3 + Y1:Y4 + Y1:Y5 + Y1:Y7 +
               Y2:Y6 + Y2:Y8 + Y2:Y9 +
               Y3:Y5 + Y3:Y6 + Y3:Y9 + 
               Y4:Y5 + Y4:Y7 +
               Y5:Y6 + Y5:Y7 + Y5:Y8 +
               Y6:Y8 + Y6:Y9 +
               Y8:Y9 +  
               case:Y1 + case:Y2 + case:Y3 + case:Y4 + case:Y5 + case:Y6 + case:Y7 + case:Y8 + case:Y9,  data = tab)
supreme_hiton_fit1 = c(main$param[[1]], main$param$Y1[2], main$param$Y2[2], main$param$Y3[2], main$param$Y4[2],
                       main$param$Y5[2], main$param$Y6[2], main$param$Y7[2], main$param$Y8[2], main$param$Y9[2],
                       main$param$Y1.Y3[1], main$param$Y1.Y4[1], main$param$Y1.Y5[1], main$param$Y1.Y7[1], 
                       main$param$Y2.Y6[1], main$param$Y2.Y8[1], main$param$Y2.Y9[1],
                       main$param$Y3.Y5[1], main$param$Y3.Y6[1], main$param$Y3.Y9[1],
                       main$param$Y4.Y5[1], main$param$Y4.Y7[1], 
                       main$param$Y5.Y6[1], main$param$Y5.Y7[1], main$param$Y5.Y8[1],
                       main$param$Y6.Y8[1], main$param$Y6.Y9[1],
                       main$param$Y8.Y9[1],
                       main$param$Y1.case[1], main$param$Y2.case[1], main$param$Y3.case[1], main$param$Y4.case[1], main$param$Y5.case[1],
                       main$param$Y6.case[1], main$param$Y7.case[1], main$param$Y8.case[1], main$param$Y9.case[1])

save(supreme_hiton_fit1, file = "supreme_hiton_fit1.RData")
#save(supreme_hiton_fit1, file = "supreme_hiton_fit2.RData")
#save(supreme_hiton_fit1, file = "supreme_hiton_fit8.RData")
#save(supreme_hiton_fit1, file = "supreme_hiton_fit9.RData")


# bootstrap sample (nb = 1000)
nb = 1000 # the number of bootstrap samples
supreme_hiton_boot1 = supreme_hiton_boot2 = supreme_hiton_boot8 = supreme_hiton_boot9 = list()
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
  main = loglm(~ Y1 + Y2 + Y3 + Y4 + Y5 + Y6 + Y7 + Y8 + Y9 + 
                 Y1:Y3 + Y1:Y4 + Y1:Y5 + Y1:Y7 +
                 Y2:Y6 + Y2:Y8 + Y2:Y9 +
                 Y3:Y5 + Y3:Y6 + Y3:Y9 + 
                 Y4:Y5 + Y4:Y7 +
                 Y5:Y6 + Y5:Y7 + Y5:Y8 +
                 Y6:Y8 + Y6:Y9 +
                 Y8:Y9 +  
                 case:Y1 + case:Y2 + case:Y3 + case:Y4 + case:Y5 + case:Y6 + case:Y7 + case:Y8 + case:Y9,  data = tab)
  main.param = c(main$param[[1]], main$param$Y1[2], main$param$Y2[2], main$param$Y3[2], main$param$Y4[2],
                 main$param$Y5[2], main$param$Y6[2], main$param$Y7[2], main$param$Y8[2], main$param$Y9[2],
                 main$param$Y1.Y3[1], main$param$Y1.Y4[1], main$param$Y1.Y5[1], main$param$Y1.Y7[1], 
                 main$param$Y2.Y6[1], main$param$Y2.Y8[1], main$param$Y2.Y9[1],
                 main$param$Y3.Y5[1], main$param$Y3.Y6[1], main$param$Y3.Y9[1],
                 main$param$Y4.Y5[1], main$param$Y4.Y7[1], 
                 main$param$Y5.Y6[1], main$param$Y5.Y7[1], main$param$Y5.Y8[1],
                 main$param$Y6.Y8[1], main$param$Y6.Y9[1],
                 main$param$Y8.Y9[1],
                 main$param$Y1.case[1], main$param$Y2.case[1], main$param$Y3.case[1], main$param$Y4.case[1], main$param$Y5.case[1],
                 main$param$Y6.case[1], main$param$Y7.case[1], main$param$Y8.case[1], main$param$Y9.case[1])
  
  supreme_hiton_boot1[[ii]] =  main.param
}

save(supreme_hiton_boot1, file = "Data/supreme_hiton_boot1.RData")
# save(supreme_hiton_boot2, file = "Data/supreme_hiton_boot2.RData")
# save(supreme_hiton_boot8, file = "Data/supreme_hiton_boot8.RData")
# save(supreme_hiton_boot9, file = "Data/supreme_hiton_boot9.RData")


## three-way interactions
# clique sets
three.sets = c("Y6:Y8:Y9", "Y5:Y6:Y8", "Y1:Y5:Y7", "Y1:Y3:Y5",
               "Y3:Y6:Y9", "Y3:Y5:Y6", "Y1:Y4:Y7", "Y4:Y5:Y7",
               "Y1:Y4:Y5", "Y2:Y6:Y8", "Y2:Y8:Y9", "Y2:Y6:Y9")

# create a contingency table
tab = xtabs(~ Y1 + Y2 + Y3 + Y4 + Y5 + Y6 + Y7 + Y8 + Y9 + case, data = input)
supreme_hiton_fit2_three = c()
for(i in 1:length(three.sets)){
  frmla <- as.formula(paste("~ Y1 + Y2 + Y3 + Y4 + Y5 + Y6 + Y7 + Y8 + Y9 + 
                            Y1:Y3 + Y1:Y4 + Y1:Y5 + Y1:Y7 +
                            Y2:Y6 + Y2:Y8 + Y2:Y9 +
                            Y3:Y5 + Y3:Y6 + Y3:Y9 + 
                            Y4:Y5 + Y4:Y7 +
                            Y5:Y6 + Y5:Y7 + Y5:Y8 +
                            Y6:Y8 + Y6:Y9 +
                            Y8:Y9 +
                            case:Y1 + case:Y2 + case:Y3 + case:Y4 + case:Y5 + case:Y6 + case:Y7 + case:Y8 + case:Y9", 
                            three.sets[i], sep= "+"))
  main = loglm(frmla,  data = tab)
  supreme_hiton_fit1_three[i] = main$param[[length(main$param)]][1]
  #supreme_hiton_fit2_three[i] = main$param[[length(main$param)]][1]
  #supreme_hiton_fit8_three[i] = main$param[[length(main$param)]][1]
  #supreme_hiton_fit9_three[i] = main$param[[length(main$param)]][1]
  
}
save(supreme_hiton_fit1_three, file = "supreme_hiton_fit1_three.RData")
#save(supreme_hiton_fit2_three, file = "supreme_hiton_fit2_three.RData")
#save(supreme_hiton_fit8_three, file = "supreme_hiton_fit8_three.RData")
#save(supreme_hiton_fit9_three, file = "supreme_hiton_fit9_three.RData")

