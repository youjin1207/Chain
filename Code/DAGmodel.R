library(Matrix)
source("http://bioconductor.org/biocLite.R")
biocLite("graph")
biocLite("RBGL")
library(pcalg)
library(MASS)
### Data generating procedure ###
set.seed(1) # set.seed from 1-10.
G = rsparsematrix(9, 9, density = 0.3, symmetric = TRUE)
Adj = as.matrix(G)
Adj = ifelse(Adj != 0, 1, 0)
diag(Adj) = 0
n.obs = 1000

logistic = function(x) exp(x) / (1 + exp(x))
rg1 = list() # do the same for rg2-rg10, using set.seed(2)-set.seed(10).
for(ii in 1:1000){
  Y = matrix(0, n.obs, 9)
  A = matrix(0, n.obs, 9)
  for(k in 1:n.obs){
    time = 100
    A[k,] = rbinom(9, 1, 0.5)
    A[k,] = 2*A[k,] - 1
    out = matrix(0, 9, time) # probability
    outcome = matrix(0, 9, time)
    t = 1
    out[,t] = rep(0.5, 9) 
    outcome[,t] = 2*rbinom(9, 1, out[,t])-1
    for(t in 2:time){
      for(ind in 1:4){
        out[ind,t] = -0.5 + 5*outcome[ind, (t-1)] + 0.05*A[k,ind] + sum(0.3*outcome[which(Adj[ind,] == 1), (t-1)]) + rnorm(1, 0, 0.1)
      }
      out[5,t] = 0 + 5*outcome[5, (t-1)] + 0.2*A[k,5] + sum(0.3*outcome[which(Adj[5,] == 1), (t-1)]) + rnorm(1, 0, 0.1)
      for(ind in 6:9){
        out[ind,t] = 0.5 + 5*outcome[ind, (t-1)] + 0.05*A[k,ind] + sum(0.3*outcome[which(Adj[ind,] == 1), (t-1)]) + rnorm(1, 0, 0.1) 
      } 
      outcome[,t] = rbinom(9, 1, logistic(out[,t])) 
      outcome[,t] = 2*outcome[,t] - 1
    }
    Y[k,] = outcome[,t]
  }
  rg1[[ii]]  = list(Y, A)
}

save(rg1, file = "rg1.RData")
save(Adj, file = "adj1.RData")

### Independence Test ### 
load("Data/rg1.RData"); load("Data/adj1.RData"); rg = rg1
#load("Data/rg2.RData"); load("Data/adj2.RData"); rg = rg2
#load("Data/rg3.RData"); load("Data/adj3.RData"); rg = rg3
#load("Data/rg4.RData"); load("Data/adj4.RData"); rg = rg4
#load("Data/rg5.RData"); load("Data/adj5.RData"); rg = rg5
#load("Data/rg6.RData"); load("Data/adj6.RData"); rg = rg6
#load("Data/rg7.RData"); load("Data/adj7.RData"); rg = rg7
#load("Data/rg8.RData"); load("Data/adj8.RData"); rg = rg8
#load("Data/rg9.RData"); load("Data/adj9.RData"); rg = rg9
#load("Data/rg10.RData"); load("Data/adj10.RData"); rg = rg10
n = 1000; nsim = 1000
Y1 = Y2 = Y3 = Y4 = Y5 = Y6 = Y7 = Y8 = Y9 = matrix(0, nsim, n)
A1 = A2 = A3 = A4 = A5 = A6 = A7 = A8 = A9 = matrix(0, nsim, n)
for(i in 1:nsim){
  Y1[i,] = rg[[i]][[1]][,1]; Y2[i,] = rg[[i]][[1]][,2]; Y3[i,] = rg[[i]][[1]][,3]
  Y4[i,] = rg[[i]][[1]][,4]; Y5[i,] = rg[[i]][[1]][,5]; Y6[i,] = rg[[i]][[1]][,6]
  Y7[i,] = rg[[i]][[1]][,7]; Y8[i,] = rg[[i]][[1]][,8]; Y9[i,] = rg[[i]][[1]][,9]
  
  A1[i,] = rg[[i]][[2]][,1]; A2[i,] = rg[[i]][[2]][,2]; A3[i,] = rg[[i]][[2]][,3]
  A4[i,] = rg[[i]][[2]][,4]; A5[i,] = rg[[i]][[2]][,5]; A6[i,] = rg[[i]][[2]][,6]
  A7[i,] = rg[[i]][[2]][,7]; A8[i,] = rg[[i]][[2]][,8]; A9[i,] = rg[[i]][[2]][,9]
}

cond = list()
for(r in 1:nsim){
  data = cbind(Y1[r,], Y2[r,], Y3[r,], Y4[r,], Y5[r,], Y6[r,], Y7[r,], Y8[r,], Y9[r,],
               A1[r,], A2[r,], A3[r,], A4[r,], A5[r,], A6[r,], A7[r,], A8[r,], A9[r,])
  data = (data+1)/2
  data = ifelse(data == 0.5, 0, data)
  ###### ci.test
  cond[[r]] = matrix(0, 9, 9)
  for(i in 1:9){for(j in 1:9){
    cond[[r]][i,j] = gSquareBin(i, j, c(which(Adj[i,]==1),9+i), data, verbose = FALSE, adaptDF = FALSE)
  }}
}
cond.power = matrix(0, 9, 9)
for(r in 1:nsim){
  cond.power = cond.power + (cond[[r]] <= 0.05)/nsim
}

marg = list()
for(r in 1:nsim){
  data = cbind(Y1[r,], Y2[r,], Y3[r,], Y4[r,], Y5[r,], Y6[r,], Y7[r,], Y8[r,], Y9[r,],
               A1[r,], A2[r,], A3[r,], A4[r,], A5[r,], A6[r,], A7[r,], A8[r,], A9[r,])
  data = (data+1)/2
  data = ifelse(data == 0.5, 0, data)
  ###### ci.test
  marg[[r]] = matrix(0, 9, 9)
  for(i in 1:9){for(j in 1:9){
    marg[[r]][i,j] = gSquareBin(i, j, NULL, data, verbose = FALSE, adaptDF = FALSE)
  }}
}
marg.power = matrix(0, 9, 9)
for(r in 1:nsim){
  marg.power = marg.power + (marg[[r]] <= 0.05)/nsim
}


matrix.names = matrix(0, 9, 9)
for(i in 1:9){for(j in 1:9){
  matrix.names[i,j] = paste(i,",",j,sep="")
}}
## a list of non-adjacent pairs
pair.nonadj = matrix.names[upper.tri(matrix.names)][which(Adj[upper.tri(Adj)] == 0)]
cond.nonadj = cond.power[upper.tri(cond.power)][which(Adj[upper.tri(Adj)] == 0)]
marg.nonadj = marg.power[upper.tri(marg.power)][which(Adj[upper.tri(Adj)] == 0)]
mat = rbind(cond.nonadj, marg.nonadj)
colnames(mat) = pair.nonadj

pdf("Figure/network1.pdf", width = 15, height = 8)
#pdf("Figure/network2.pdf", width = 15, height = 8)
#pdf("Figure/network3.pdf", width = 15, height = 8)
#pdf("Figure/network4.pdf", width = 15, height = 8)
#pdf("Figure/network5.pdf", width = 15, height = 8)
#pdf("Figure/network6.pdf", width = 15, height = 8)
#pdf("Figure/network7.pdf", width = 15, height = 8)
#pdf("Figure/network8.pdf", width = 15, height = 8)
#pdf("Figure/network9.pdf", width = 15, height = 8)
#pdf("Figure/network10.pdf", width = 15, height = 8)
par(mfrow = c(1,1), mar = c(7,7,5,1), tcl = 0.5,
    xpd = TRUE, cex.axis = 1, cex.main = 2.5, oma = c(2,2,2,2))
barplot(mat, beside = TRUE, 
        col = c("firebrick1", "royalblue1"),
        ylim = c(0, 1), xpd = TRUE, cex.lab = 2, mgp = c(4,1,0),
        xlab = "Non-adjacent pairs",
        ylab = "Proportion of rejecting the null", main = "Random Network 1")
abline(h = 0.05, col = "black", xpd = FALSE)
#legend("topright",  c("Marginal", "Conditional"),
#       col = c("royalblue1", "firebrick1"), pch = 20, cex = 4,
#       bty ='n', xpd = TRUE, pt.cex = 4)
dev.off()