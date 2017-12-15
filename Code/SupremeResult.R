log.main.nine = function(pars, outcomes, treat){
  # This function prints out the log of clique factorizations before normalization.
  # Here cliques are base on our ad-hoc structural learning using log-linear model.
  # We are assuming 19 undirected edges between the justices. 
  
  pars[1] +
    sum(outcomes[1:9]*pars[2:10]) + 
    pars[11]*outcomes[1]*outcomes[2] + pars[12]*outcomes[1]*outcomes[3] +
    pars[13]*outcomes[1]*outcomes[4] + pars[14]*outcomes[1]*outcomes[5] +
    pars[15]*outcomes[1]*outcomes[7] +
    pars[16]*outcomes[1]*outcomes[8] + pars[17]*outcomes[1]*outcomes[9] +
    pars[18]*outcomes[2]*outcomes[5] + pars[19]*outcomes[2]*outcomes[6] +
    pars[20]*outcomes[2]*outcomes[8] +
    pars[21]*outcomes[2]*outcomes[9] +
    pars[22]*outcomes[3]*outcomes[5] + pars[23]*outcomes[3]*outcomes[6] +
    pars[24]*outcomes[3]*outcomes[9] + pars[25]*outcomes[4]*outcomes[7] +
    pars[26]*outcomes[6]*outcomes[8] +
    pars[27]*outcomes[6]*outcomes[9] + 
    pars[28]*outcomes[7]*outcomes[9] + pars[29]*outcomes[8]*outcomes[9] +
    pars[30]*outcomes[1]*treat + pars[31]*outcomes[2]*treat + 
    pars[32]*outcomes[3]*treat + pars[33]*outcomes[4]*treat +
    pars[34]*outcomes[5]*treat + pars[35]*outcomes[6]*treat +
    pars[36]*outcomes[7]*treat + pars[37]*outcomes[8]*treat + 
    pars[38]*outcomes[9]*treat 
}

print.prob.nine = function(pars, outcomes, treat){
  ## print out the probability of outcomes given interventions
  # par = estimated parameter with a length of 28.
  # outcomes = a set of Y
  # treat = a set of A
  main = log.main.nine(pars, outcomes, treat) 
  ver = c(-1,1); Z.part = 0
  for(i in 1:2){for(j in 1:2){for(l in 1:2){
    for(q in 1:2){for(w in 1:2){for(e in 1:2){
      for(a in 1:2){for(s in 1:2){for(d in 1:2){
        Z.part = Z.part + exp(log.main.nine(pars, outcomes = c(ver[i], ver[j], ver[l], ver[q], ver[w], ver[e], ver[a], ver[s], ver[d]), 
                                            treat))
      }}}}}}}}}
  prop = exp(main) /  Z.part 
  return(prop)
}

print.mu.nine = function(outcome.node, pars){
  # This function prints out 
  
  
  input.outcome = matrix(0, 2^9, 9)
  eight.possible = expand.grid(rep(list(0:1), 8))
  eight.possible = 2*eight.possible - 1
  for(i in 1:2^8){
    input.outcome[i, c(1:9)[-outcome.node]] = as.numeric(eight.possible[i,])
    input.outcome[(i+2^8), c(1:9)[-outcome.node]] = as.numeric(eight.possible[i,])
  }
  
  input.outcome[,outcome.node] = rep(1, 2^9)
  
  input.treat = rep(-1, 2^9)
  input.treat[c(1:2^8)] = rep(1, 2^8)
  
  tmp = rep(0, 2^9)
  for(i in 1:2^9){
    tmp[i] = print.prob.nine(pars, outcomes = input.outcome[i,], treat = input.treat[i])
  }
  
  # treatment effect 
  mu = sum(tmp[1:2^8]) -  sum(tmp[(2^8+1):2^9])
  return(mu)
}

print.tau.nine = function(node, pars){
  tau = 0
  for(i in 1:9){
    tau = tau + print.mu.nine(outcome.node = i, pars) 
  }
  tau = tau/9
  return(tau)
}


print.match.r = function(r, pars, treat){
  if(r==0){
    probs = print.prob.nine(pars, outcomes = rep(-1, 9), treat)
    return(probs)
  }
  # r : the number of 1 (liberal) in outcomes (> 0 )
  set1 = combinations(9, r, v = 1:9)
  input.outcome = matrix(-1, nrow(set1), 9)
  for(i in 1:nrow(set1)){
    input.outcome[i,set1[i,]] = rep(1,r)
  }
  
  probs = 0
  for(i in 1:nrow(input.outcome)){
    probs = probs + print.prob.nine(pars, outcomes = input.outcome[i,], treat)
  }
  return(probs)
}




####### 1. Criminal Procedure
load("Data/supreme_loglin_fit1.RData")
main = supreme_loglin_fit1
load("Data/supreme_loglin_boot1.RData")
boot.sample = matrix(0, nrow = length(supreme_loglin_boot1), ncol = 38)
for(i in 1:length(supreme_loglin_boot1)){
  boot.sample[i,] = supreme_loglin_boot1[[i]]
}


### P(Y = -1 | a = 1)
all0.under1 = print.prob.nine(pars = main, outcomes = rep(-1, 9), treat = 1)
all0.under1.boot = c()
for(i in 1:nrow(boot.sample)){
  all0.under1.boot[i] = print.prob.nine(pars = boot.sample[i,], outcomes = rep(-1, 9), treat = 1)
}
### P(Y = -1 | a = 0)
all0.under0 = print.prob.nine(pars = main, outcomes = rep(-1, 9), treat = -1)
all0.under0.boot = c()
for(i in 1:nrow(boot.sample)){
  all0.under0.boot[i] = print.prob.nine(pars = boot.sample[i,], outcomes = rep(-1, 9), treat = -1)
}

### P(Y = 1 | a = 1)
all1.under1 = print.prob.nine(pars = main, outcomes = rep(1, 9), treat = 1)
all1.under1.boot = c()
for(i in 1:nrow(boot.sample)){
  all1.under1.boot[i] = print.prob.nine(pars = boot.sample[i,], outcomes = rep(1, 9), treat = 1)
}

### P(Y = 1 | a = 0)
all1.under0 = print.prob.nine(pars = main, outcomes = rep(1, 9), treat = -1)
all1.under0.boot = c()
for(i in 1:nrow(boot.sample)){
  all1.under0.boot[i] = print.prob.nine(pars = boot.sample[i,], outcomes = rep(1, 9), treat = -1)
}

###
mat = matrix(0, nrow = 2, ncol = 3)
colnames(mat) = c("a=1", "a=0", "effect")
rownames(mat) = c("all conservative", "all liberal")
mat[1,] = c(paste(round(all0.under1, 3), " (", round(sd(all0.under1.boot),3),")", sep=""), 
            paste(round(all0.under0, 3), " (", round(sd(all0.under0.boot),3),")", sep=""),
            paste(round(all0.under1 - all0.under0, 3), " (", round(sd(all0.under1.boot - all0.under0.boot),3),")", sep=""))
mat[2,] = c(paste(round(all1.under1, 3), " (", round(sd(all1.under1.boot),3),")", sep=""), 
            paste(round(all1.under0, 3), " (", round(sd(all1.under0.boot),3),")", sep=""),
            paste(round(all1.under1 - all0.under0, 3), " (", round(sd(all1.under1.boot - all1.under0.boot),3),")", sep=""))
print(xtable(mat))

###
mat = matrix(0, nrow = 3, ncol = 10)
colnames(mat) = c(0:9)
rownames(mat) = c("a=1", "a=0", "effect")


### P(|Y| = r | a = 0)
print.match.boot1 = matrix(0, nrow = 10, ncol = nrow(boot.sample)) # 10 x 1000 matrix
print.match.boot0 = matrix(0, nrow = 10, ncol = nrow(boot.sample)) 

for(r in 1:10){
  print(r)
  for(i in 1:nrow(boot.sample)){
    print.match.boot1[r,i] = print.match.r((r-1), boot.sample[i,], 1)  
    print.match.boot0[r,i] = print.match.r((r-1), boot.sample[i,], -1) 
  }
}

## main effect
mat = matrix(0, nrow = 4, ncol = 5)
mat[1,] = c("constant", "WHRehnquist", "JPStevens", "SDOConnor", "AScalia")
mat[3,] = c("AMKennedy", "DHSouter", "CThomas", "RBGinsburg", "SGBreyer")
mat[2,] = c(paste(round(main[1],2), " [", round(findL(boot.sample[,1]), 2), " ,", round(findU(boot.sample[,1]), 2),"]", sep=""),
            paste(round(main[2],2), " [", round(findL(boot.sample[,2]), 2), " ,", round(findU(boot.sample[,2]), 2),"]", sep=""),
            paste(round(main[3],2), " [", round(findL(boot.sample[,3]), 2), " ,", round(findU(boot.sample[,3]), 2),"]", sep=""),
            paste(round(main[4],2), " [", round(findL(boot.sample[,4]), 2), " ,", round(findU(boot.sample[,4]), 2),"]", sep=""),
            paste(round(main[5],2), " [", round(findL(boot.sample[,5]), 2), " ,", round(findU(boot.sample[,5]), 2),"]", sep=""))

mat[4,] =  c( paste(round(main[6],2), " [", round(findL(boot.sample[,6]), 2), " ,", round(findU(boot.sample[,6]), 2),"]", sep=""),
              paste(round(main[7],2), " [", round(findL(boot.sample[,7]), 2), " ,", round(findU(boot.sample[,7]), 2),"]", sep=""),
              paste(round(main[8],2), " [", round(findL(boot.sample[,8]), 2), " ,", round(findU(boot.sample[,8]), 2),"]", sep=""),
              paste(round(main[9],2), " [", round(findL(boot.sample[,9]), 2), " ,", round(findU(boot.sample[,9]), 2),"]", sep=""),
              paste(round(main[10],2), " [", round(findL(boot.sample[,10]), 2), " ,", round(findU(boot.sample[,10]), 2),"]", sep=""))
print(xtable(mat))

## the number of vote
### P(|Y| = r | a = 0)
print.match.boot1 = matrix(0, nrow = 10, ncol = nrow(boot.sample)) # 10 x 1000 matrix
print.match.boot0 = matrix(0, nrow = 10, ncol = nrow(boot.sample)) 

for(r in 1:10){
  print(r)
  for(i in 1:nrow(boot.sample)){
    print.match.boot1[r,i] = print.match.r((r-1), boot.sample[i,], 1)  
    print.match.boot0[r,i] = print.match.r((r-1), boot.sample[i,], -1) 
  }
}

mat = matrix(0, nrow = 3, ncol = 10)
rownames(mat) = c("a=1", "a=0", "effect")
colnames(mat) = c(0:9)
mat[1,] = c(paste(round(print.match.r(0, main, 1), 2), " (", round(sd(print.match.boot1[1,]), 2), " )",  sdp=""),
            paste(round(print.match.r(1, main, 1), 2), " (", round(sd(print.match.boot1[2,]), 2), " )",  sdp=""),
            paste(round(print.match.r(2, main, 1), 2), " (", round(sd(print.match.boot1[3,]), 2), " )",  sdp=""),
            paste(round(print.match.r(3, main, 1), 2), " (", round(sd(print.match.boot1[4,]), 2), " )",  sdp=""),
            paste(round(print.match.r(4, main, 1), 2), " (", round(sd(print.match.boot1[5,]), 2), " )",  sdp=""),
            paste(round(print.match.r(5, main, 1), 2), " (", round(sd(print.match.boot1[6,]), 2), " )",  sdp=""),
            paste(round(print.match.r(6, main, 1), 2), " (", round(sd(print.match.boot1[7,]), 2), " )",  sdp=""),
            paste(round(print.match.r(7, main, 1), 2), " (", round(sd(print.match.boot1[8,]), 2), " )",  sdp=""),
            paste(round(print.match.r(8, main, 1), 2), " (", round(sd(print.match.boot1[9,]), 2), " )",  sdp=""),
            paste(round(print.match.r(9, main, 1), 2), " (", round(sd(print.match.boot1[10,]), 2), " )",  sdp=""))
mat[2,] = c(paste(round(print.match.r(0, main, -1), 2), " (", round(sd(print.match.boot0[1,]), 2), " )",  sdp=""),
            paste(round(print.match.r(1, main, -1), 2), " (", round(sd(print.match.boot0[2,]), 2), " )",  sdp=""),
            paste(round(print.match.r(2, main, -1), 2), " (", round(sd(print.match.boot0[3,]), 2), " )",  sdp=""),
            paste(round(print.match.r(3, main, -1), 2), " (", round(sd(print.match.boot0[4,]), 2), " )",  sdp=""),
            paste(round(print.match.r(4, main, -1), 2), " (", round(sd(print.match.boot0[5,]), 2), " )",  sdp=""),
            paste(round(print.match.r(5, main, -1), 2), " (", round(sd(print.match.boot0[6,]), 2), " )",  sdp=""),
            paste(round(print.match.r(6, main, -1), 2), " (", round(sd(print.match.boot0[7,]), 2), " )",  sdp=""),
            paste(round(print.match.r(7, main, -1), 2), " (", round(sd(print.match.boot0[8,]), 2), " )",  sdp=""),
            paste(round(print.match.r(8, main, -1), 2), " (", round(sd(print.match.boot0[9,]), 2), " )",  sdp=""),
            paste(round(print.match.r(9, main, -1), 2), " (", round(sd(print.match.boot0[10,]), 2), " )",  sdp=""))
mat[3,] =  c(paste(round(print.match.r(0, main, 1) - print.match.r(0, main, -1), 2), " (", round(sd(print.match.boot1[1,] - print.match.boot0[1,]), 2), " )",  sdp=""),
             paste(round(print.match.r(1, main, 1) - print.match.r(1, main, -1), 2), " (", round(sd(print.match.boot1[2,] - print.match.boot0[2,]), 2), " )",  sdp=""),
             paste(round(print.match.r(2, main, 1) - print.match.r(2, main, -1), 2), " (", round(sd(print.match.boot1[3,] - print.match.boot0[3,]), 2), " )",  sdp=""),
             paste(round(print.match.r(3, main, 1) - print.match.r(3, main, -1), 2), " (", round(sd(print.match.boot1[4,] - print.match.boot0[4,]), 2), " )",  sdp=""),
             paste(round(print.match.r(4, main, 1) - print.match.r(4, main, -1), 2), " (", round(sd(print.match.boot1[5,] - print.match.boot0[5,]), 2), " )",  sdp=""),
             paste(round(print.match.r(5, main, 1) - print.match.r(5, main, -1), 2), " (", round(sd(print.match.boot1[6,] - print.match.boot0[6,]), 2), " )",  sdp=""),
             paste(round(print.match.r(6, main, 1) - print.match.r(6, main, -1), 2), " (", round(sd(print.match.boot1[7,] - print.match.boot0[7,]), 2), " )",  sdp=""),
             paste(round(print.match.r(7, main, 1) - print.match.r(7, main, -1), 2), " (", round(sd(print.match.boot1[8,] - print.match.boot0[8,]), 2), " )",  sdp=""),
             paste(round(print.match.r(8, main, 1) - print.match.r(8, main, -1), 2), " (", round(sd(print.match.boot1[9,] - print.match.boot0[9,]), 2), " )",  sdp=""),
             paste(round(print.match.r(9, main, 1) - print.match.r(9, main, -1), 2), " (", round(sd(print.match.boot1[10,] - print.match.boot0[10,]), 2), " )",  sdp=""))
print(xtable(mat))


####### 2. Civil rights
load("Data/supreme_loglin_fit2.RData")
main = supreme_loglin_fit2
load("Data/supreme_loglin_boot2.RData")
boot.sample = matrix(0, nrow = length(supreme_loglin_boot2), ncol = 38)
for(i in 1:nrow(boot.sample)){
  boot.sample[i,] = supreme_loglin_boot2[[i]]
}

## main effect
mat = matrix(0, nrow = 4, ncol = 5)
mat[1,] = c("constant", "WHRehnquist", "JPStevens", "SDOConnor", "AScalia")
mat[3,] = c("AMKennedy", "DHSouter", "CThomas", "RBGinsburg", "SGBreyer")
mat[2,] = c(paste(round(main[1],2), " [", round(findL(boot.sample[,1]), 2), " ,", round(findU(boot.sample[,1]), 2),"]", sep=""),
            paste(round(main[2],2), " [", round(findL(boot.sample[,2]), 2), " ,", round(findU(boot.sample[,2]), 2),"]", sep=""),
            paste(round(main[3],2), " [", round(findL(boot.sample[,3]), 2), " ,", round(findU(boot.sample[,3]), 2),"]", sep=""),
            paste(round(main[4],2), " [", round(findL(boot.sample[,4]), 2), " ,", round(findU(boot.sample[,4]), 2),"]", sep=""),
            paste(round(main[5],2), " [", round(findL(boot.sample[,5]), 2), " ,", round(findU(boot.sample[,5]), 2),"]", sep=""))

mat[4,] =  c( paste(round(main[6],2), " [", round(findL(boot.sample[,6]), 2), " ,", round(findU(boot.sample[,6]), 2),"]", sep=""),
              paste(round(main[7],2), " [", round(findL(boot.sample[,7]), 2), " ,", round(findU(boot.sample[,7]), 2),"]", sep=""),
              paste(round(main[8],2), " [", round(findL(boot.sample[,8]), 2), " ,", round(findU(boot.sample[,8]), 2),"]", sep=""),
              paste(round(main[9],2), " [", round(findL(boot.sample[,9]), 2), " ,", round(findU(boot.sample[,9]), 2),"]", sep=""),
              paste(round(main[10],2), " [", round(findL(boot.sample[,10]), 2), " ,", round(findU(boot.sample[,10]), 2),"]", sep=""))
print(xtable(mat))

## the number of vote
### P(|Y| = r | a = 0)
print.match.boot1 = matrix(0, nrow = 10, ncol = nrow(boot.sample)) # 10 x 1000 matrix
print.match.boot0 = matrix(0, nrow = 10, ncol = nrow(boot.sample)) 

for(r in 1:10){
  print(r)
  for(i in 1:nrow(boot.sample)){
    print.match.boot1[r,i] = print.match.r((r-1), boot.sample[i,], 1)  
    print.match.boot0[r,i] = print.match.r((r-1), boot.sample[i,], -1) 
  }
}

mat = matrix(0, nrow = 3, ncol = 10)
rownames(mat) = c("a=1", "a=0", "effect")
colnames(mat) = c(0:9)
mat[1,] = c(paste(round(print.match.r(0, main, 1), 2), " (", round(sd(print.match.boot1[1,]), 2), " )",  sdp=""),
            paste(round(print.match.r(1, main, 1), 2), " (", round(sd(print.match.boot1[2,]), 2), " )",  sdp=""),
            paste(round(print.match.r(2, main, 1), 2), " (", round(sd(print.match.boot1[3,]), 2), " )",  sdp=""),
            paste(round(print.match.r(3, main, 1), 2), " (", round(sd(print.match.boot1[4,]), 2), " )",  sdp=""),
            paste(round(print.match.r(4, main, 1), 2), " (", round(sd(print.match.boot1[5,]), 2), " )",  sdp=""),
            paste(round(print.match.r(5, main, 1), 2), " (", round(sd(print.match.boot1[6,]), 2), " )",  sdp=""),
            paste(round(print.match.r(6, main, 1), 2), " (", round(sd(print.match.boot1[7,]), 2), " )",  sdp=""),
            paste(round(print.match.r(7, main, 1), 2), " (", round(sd(print.match.boot1[8,]), 2), " )",  sdp=""),
            paste(round(print.match.r(8, main, 1), 2), " (", round(sd(print.match.boot1[9,]), 2), " )",  sdp=""),
            paste(round(print.match.r(9, main, 1), 2), " (", round(sd(print.match.boot1[10,]), 2), " )",  sdp=""))
mat[2,] = c(paste(round(print.match.r(0, main, -1), 2), " (", round(sd(print.match.boot0[1,]), 2), " )",  sdp=""),
            paste(round(print.match.r(1, main, -1), 2), " (", round(sd(print.match.boot0[2,]), 2), " )",  sdp=""),
            paste(round(print.match.r(2, main, -1), 2), " (", round(sd(print.match.boot0[3,]), 2), " )",  sdp=""),
            paste(round(print.match.r(3, main, -1), 2), " (", round(sd(print.match.boot0[4,]), 2), " )",  sdp=""),
            paste(round(print.match.r(4, main, -1), 2), " (", round(sd(print.match.boot0[5,]), 2), " )",  sdp=""),
            paste(round(print.match.r(5, main, -1), 2), " (", round(sd(print.match.boot0[6,]), 2), " )",  sdp=""),
            paste(round(print.match.r(6, main, -1), 2), " (", round(sd(print.match.boot0[7,]), 2), " )",  sdp=""),
            paste(round(print.match.r(7, main, -1), 2), " (", round(sd(print.match.boot0[8,]), 2), " )",  sdp=""),
            paste(round(print.match.r(8, main, -1), 2), " (", round(sd(print.match.boot0[9,]), 2), " )",  sdp=""),
            paste(round(print.match.r(9, main, -1), 2), " (", round(sd(print.match.boot0[10,]), 2), " )",  sdp=""))
mat[3,] =  c(paste(round(print.match.r(0, main, 1) - print.match.r(0, main, -1), 2), " (", round(sd(print.match.boot1[1,] - print.match.boot0[1,]), 2), " )",  sdp=""),
             paste(round(print.match.r(1, main, 1) - print.match.r(1, main, -1), 2), " (", round(sd(print.match.boot1[2,] - print.match.boot0[2,]), 2), " )",  sdp=""),
             paste(round(print.match.r(2, main, 1) - print.match.r(2, main, -1), 2), " (", round(sd(print.match.boot1[3,] - print.match.boot0[3,]), 2), " )",  sdp=""),
             paste(round(print.match.r(3, main, 1) - print.match.r(3, main, -1), 2), " (", round(sd(print.match.boot1[4,] - print.match.boot0[4,]), 2), " )",  sdp=""),
             paste(round(print.match.r(4, main, 1) - print.match.r(4, main, -1), 2), " (", round(sd(print.match.boot1[5,] - print.match.boot0[5,]), 2), " )",  sdp=""),
             paste(round(print.match.r(5, main, 1) - print.match.r(5, main, -1), 2), " (", round(sd(print.match.boot1[6,] - print.match.boot0[6,]), 2), " )",  sdp=""),
             paste(round(print.match.r(6, main, 1) - print.match.r(6, main, -1), 2), " (", round(sd(print.match.boot1[7,] - print.match.boot0[7,]), 2), " )",  sdp=""),
             paste(round(print.match.r(7, main, 1) - print.match.r(7, main, -1), 2), " (", round(sd(print.match.boot1[8,] - print.match.boot0[8,]), 2), " )",  sdp=""),
             paste(round(print.match.r(8, main, 1) - print.match.r(8, main, -1), 2), " (", round(sd(print.match.boot1[9,] - print.match.boot0[9,]), 2), " )",  sdp=""),
             paste(round(print.match.r(9, main, 1) - print.match.r(9, main, -1), 2), " (", round(sd(print.match.boot1[10,] - print.match.boot0[10,]), 2), " )",  sdp=""))
print(xtable(mat))


####### 8. Economic Acticity
load("Data/supreme_loglin_fit8.RData")
main = supreme_loglin_fit8
load("data/supreme_loglin_boot8.RData")
boot.sample = matrix(0, nrow = length(supreme_loglin_boot8), ncol = 38)
for(i in 1:nrow(boot.sample)){
  boot.sample[i,] = supreme_loglin_boot8[[i]]
}

## main effect
mat = matrix(0, nrow = 4, ncol = 5)
mat[1,] = c("constant", "WHRehnquist", "JPStevens", "SDOConnor", "AScalia")
mat[3,] = c("AMKennedy", "DHSouter", "CThomas", "RBGinsburg", "SGBreyer")
mat[2,] = c(paste(round(main[1],2), " [", round(findL(boot.sample[,1]), 2), " ,", round(findU(boot.sample[,1]), 2),"]", sep=""),
            paste(round(main[2],2), " [", round(findL(boot.sample[,2]), 2), " ,", round(findU(boot.sample[,2]), 2),"]", sep=""),
            paste(round(main[3],2), " [", round(findL(boot.sample[,3]), 2), " ,", round(findU(boot.sample[,3]), 2),"]", sep=""),
            paste(round(main[4],2), " [", round(findL(boot.sample[,4]), 2), " ,", round(findU(boot.sample[,4]), 2),"]", sep=""),
            paste(round(main[5],2), " [", round(findL(boot.sample[,5]), 2), " ,", round(findU(boot.sample[,5]), 2),"]", sep=""))

mat[4,] =  c( paste(round(main[6],2), " [", round(findL(boot.sample[,6]), 2), " ,", round(findU(boot.sample[,6]), 2),"]", sep=""),
              paste(round(main[7],2), " [", round(findL(boot.sample[,7]), 2), " ,", round(findU(boot.sample[,7]), 2),"]", sep=""),
              paste(round(main[8],2), " [", round(findL(boot.sample[,8]), 2), " ,", round(findU(boot.sample[,8]), 2),"]", sep=""),
              paste(round(main[9],2), " [", round(findL(boot.sample[,9]), 2), " ,", round(findU(boot.sample[,9]), 2),"]", sep=""),
              paste(round(main[10],2), " [", round(findL(boot.sample[,10]), 2), " ,", round(findU(boot.sample[,10]), 2),"]", sep=""))
print(xtable(mat))

## the number of vote
### P(|Y| = r | a = 0)
print.match.boot1 = matrix(0, nrow = 10, ncol = nrow(boot.sample)) # 10 x 1000 matrix
print.match.boot0 = matrix(0, nrow = 10, ncol = nrow(boot.sample)) 

for(r in 1:10){
  print(r)
  for(i in 1:nrow(boot.sample)){
    print.match.boot1[r,i] = print.match.r((r-1), boot.sample[i,], 1)  
    print.match.boot0[r,i] = print.match.r((r-1), boot.sample[i,], -1) 
  }
}

mat = matrix(0, nrow = 3, ncol = 10)
rownames(mat) = c("a=1", "a=0", "effect")
colnames(mat) = c(0:9)
mat[1,] = c(paste(round(print.match.r(0, main, 1), 2), " (", round(sd(print.match.boot1[1,]), 2), " )",  sdp=""),
            paste(round(print.match.r(1, main, 1), 2), " (", round(sd(print.match.boot1[2,]), 2), " )",  sdp=""),
            paste(round(print.match.r(2, main, 1), 2), " (", round(sd(print.match.boot1[3,]), 2), " )",  sdp=""),
            paste(round(print.match.r(3, main, 1), 2), " (", round(sd(print.match.boot1[4,]), 2), " )",  sdp=""),
            paste(round(print.match.r(4, main, 1), 2), " (", round(sd(print.match.boot1[5,]), 2), " )",  sdp=""),
            paste(round(print.match.r(5, main, 1), 2), " (", round(sd(print.match.boot1[6,]), 2), " )",  sdp=""),
            paste(round(print.match.r(6, main, 1), 2), " (", round(sd(print.match.boot1[7,]), 2), " )",  sdp=""),
            paste(round(print.match.r(7, main, 1), 2), " (", round(sd(print.match.boot1[8,]), 2), " )",  sdp=""),
            paste(round(print.match.r(8, main, 1), 2), " (", round(sd(print.match.boot1[9,]), 2), " )",  sdp=""),
            paste(round(print.match.r(9, main, 1), 2), " (", round(sd(print.match.boot1[10,]), 2), " )",  sdp=""))
mat[2,] = c(paste(round(print.match.r(0, main, -1), 2), " (", round(sd(print.match.boot0[1,]), 2), " )",  sdp=""),
            paste(round(print.match.r(1, main, -1), 2), " (", round(sd(print.match.boot0[2,]), 2), " )",  sdp=""),
            paste(round(print.match.r(2, main, -1), 2), " (", round(sd(print.match.boot0[3,]), 2), " )",  sdp=""),
            paste(round(print.match.r(3, main, -1), 2), " (", round(sd(print.match.boot0[4,]), 2), " )",  sdp=""),
            paste(round(print.match.r(4, main, -1), 2), " (", round(sd(print.match.boot0[5,]), 2), " )",  sdp=""),
            paste(round(print.match.r(5, main, -1), 2), " (", round(sd(print.match.boot0[6,]), 2), " )",  sdp=""),
            paste(round(print.match.r(6, main, -1), 2), " (", round(sd(print.match.boot0[7,]), 2), " )",  sdp=""),
            paste(round(print.match.r(7, main, -1), 2), " (", round(sd(print.match.boot0[8,]), 2), " )",  sdp=""),
            paste(round(print.match.r(8, main, -1), 2), " (", round(sd(print.match.boot0[9,]), 2), " )",  sdp=""),
            paste(round(print.match.r(9, main, -1), 2), " (", round(sd(print.match.boot0[10,]), 2), " )",  sdp=""))
mat[3,] =  c(paste(round(print.match.r(0, main, 1) - print.match.r(0, main, -1), 2), " (", round(sd(print.match.boot1[1,] - print.match.boot0[1,]), 2), " )",  sdp=""),
             paste(round(print.match.r(1, main, 1) - print.match.r(1, main, -1), 2), " (", round(sd(print.match.boot1[2,] - print.match.boot0[2,]), 2), " )",  sdp=""),
             paste(round(print.match.r(2, main, 1) - print.match.r(2, main, -1), 2), " (", round(sd(print.match.boot1[3,] - print.match.boot0[3,]), 2), " )",  sdp=""),
             paste(round(print.match.r(3, main, 1) - print.match.r(3, main, -1), 2), " (", round(sd(print.match.boot1[4,] - print.match.boot0[4,]), 2), " )",  sdp=""),
             paste(round(print.match.r(4, main, 1) - print.match.r(4, main, -1), 2), " (", round(sd(print.match.boot1[5,] - print.match.boot0[5,]), 2), " )",  sdp=""),
             paste(round(print.match.r(5, main, 1) - print.match.r(5, main, -1), 2), " (", round(sd(print.match.boot1[6,] - print.match.boot0[6,]), 2), " )",  sdp=""),
             paste(round(print.match.r(6, main, 1) - print.match.r(6, main, -1), 2), " (", round(sd(print.match.boot1[7,] - print.match.boot0[7,]), 2), " )",  sdp=""),
             paste(round(print.match.r(7, main, 1) - print.match.r(7, main, -1), 2), " (", round(sd(print.match.boot1[8,] - print.match.boot0[8,]), 2), " )",  sdp=""),
             paste(round(print.match.r(8, main, 1) - print.match.r(8, main, -1), 2), " (", round(sd(print.match.boot1[9,] - print.match.boot0[9,]), 2), " )",  sdp=""),
             paste(round(print.match.r(9, main, 1) - print.match.r(9, main, -1), 2), " (", round(sd(print.match.boot1[10,] - print.match.boot0[10,]), 2), " )",  sdp=""))
print(xtable(mat))

####### 9. Judicial Power
load("Data/supreme_loglin_fit9.RData")
main = supreme_loglin_fit9
load("Data/supreme_loglin_boot9.RData")
boot.sample = matrix(0, nrow = length(supreme_loglin_boot9), ncol = 38)
for(i in 1:nrow(boot.sample)){
  boot.sample[i,] = supreme_loglin_boot9[[i]]
}

## main effect
mat = matrix(0, nrow = 4, ncol = 5)
mat[1,] = c("constant", "WHRehnquist", "JPStevens", "SDOConnor", "AScalia")
mat[3,] = c("AMKennedy", "DHSouter", "CThomas", "RBGinsburg", "SGBreyer")
mat[2,] = c(paste(round(main[1],2), " [", round(findL(boot.sample[,1]), 2), " ,", round(findU(boot.sample[,1]), 2),"]", sep=""),
            paste(round(main[2],2), " [", round(findL(boot.sample[,2]), 2), " ,", round(findU(boot.sample[,2]), 2),"]", sep=""),
            paste(round(main[3],2), " [", round(findL(boot.sample[,3]), 2), " ,", round(findU(boot.sample[,3]), 2),"]", sep=""),
            paste(round(main[4],2), " [", round(findL(boot.sample[,4]), 2), " ,", round(findU(boot.sample[,4]), 2),"]", sep=""),
            paste(round(main[5],2), " [", round(findL(boot.sample[,5]), 2), " ,", round(findU(boot.sample[,5]), 2),"]", sep=""))

mat[4,] =  c( paste(round(main[6],2), " [", round(findL(boot.sample[,6]), 2), " ,", round(findU(boot.sample[,6]), 2),"]", sep=""),
              paste(round(main[7],2), " [", round(findL(boot.sample[,7]), 2), " ,", round(findU(boot.sample[,7]), 2),"]", sep=""),
              paste(round(main[8],2), " [", round(findL(boot.sample[,8]), 2), " ,", round(findU(boot.sample[,8]), 2),"]", sep=""),
              paste(round(main[9],2), " [", round(findL(boot.sample[,9]), 2), " ,", round(findU(boot.sample[,9]), 2),"]", sep=""),
              paste(round(main[10],2), " [", round(findL(boot.sample[,10]), 2), " ,", round(findU(boot.sample[,10]), 2),"]", sep=""))
print(xtable(mat))

## the number of vote
### P(|Y| = r | a = 0)
print.match.boot1 = matrix(0, nrow = 10, ncol = nrow(boot.sample)) # 10 x 1000 matrix
print.match.boot0 = matrix(0, nrow = 10, ncol = nrow(boot.sample)) 

for(r in 1:10){
  print(r)
  for(i in 1:nrow(boot.sample)){
    print.match.boot1[r,i] = print.match.r((r-1), boot.sample[i,], 1)  
    print.match.boot0[r,i] = print.match.r((r-1), boot.sample[i,], -1) 
  }
}

mat = matrix(0, nrow = 3, ncol = 10)
rownames(mat) = c("a=1", "a=0", "effect")
colnames(mat) = c(0:9)
mat[1,] = c(paste(round(print.match.r(0, main, 1), 2), " (", round(sd(print.match.boot1[1,]), 2), " )",  sdp=""),
            paste(round(print.match.r(1, main, 1), 2), " (", round(sd(print.match.boot1[2,]), 2), " )",  sdp=""),
            paste(round(print.match.r(2, main, 1), 2), " (", round(sd(print.match.boot1[3,]), 2), " )",  sdp=""),
            paste(round(print.match.r(3, main, 1), 2), " (", round(sd(print.match.boot1[4,]), 2), " )",  sdp=""),
            paste(round(print.match.r(4, main, 1), 2), " (", round(sd(print.match.boot1[5,]), 2), " )",  sdp=""),
            paste(round(print.match.r(5, main, 1), 2), " (", round(sd(print.match.boot1[6,]), 2), " )",  sdp=""),
            paste(round(print.match.r(6, main, 1), 2), " (", round(sd(print.match.boot1[7,]), 2), " )",  sdp=""),
            paste(round(print.match.r(7, main, 1), 2), " (", round(sd(print.match.boot1[8,]), 2), " )",  sdp=""),
            paste(round(print.match.r(8, main, 1), 2), " (", round(sd(print.match.boot1[9,]), 2), " )",  sdp=""),
            paste(round(print.match.r(9, main, 1), 2), " (", round(sd(print.match.boot1[10,]), 2), " )",  sdp=""))
mat[2,] = c(paste(round(print.match.r(0, main, -1), 2), " (", round(sd(print.match.boot0[1,]), 2), " )",  sdp=""),
            paste(round(print.match.r(1, main, -1), 2), " (", round(sd(print.match.boot0[2,]), 2), " )",  sdp=""),
            paste(round(print.match.r(2, main, -1), 2), " (", round(sd(print.match.boot0[3,]), 2), " )",  sdp=""),
            paste(round(print.match.r(3, main, -1), 2), " (", round(sd(print.match.boot0[4,]), 2), " )",  sdp=""),
            paste(round(print.match.r(4, main, -1), 2), " (", round(sd(print.match.boot0[5,]), 2), " )",  sdp=""),
            paste(round(print.match.r(5, main, -1), 2), " (", round(sd(print.match.boot0[6,]), 2), " )",  sdp=""),
            paste(round(print.match.r(6, main, -1), 2), " (", round(sd(print.match.boot0[7,]), 2), " )",  sdp=""),
            paste(round(print.match.r(7, main, -1), 2), " (", round(sd(print.match.boot0[8,]), 2), " )",  sdp=""),
            paste(round(print.match.r(8, main, -1), 2), " (", round(sd(print.match.boot0[9,]), 2), " )",  sdp=""),
            paste(round(print.match.r(9, main, -1), 2), " (", round(sd(print.match.boot0[10,]), 2), " )",  sdp=""))
mat[3,] =  c(paste(round(print.match.r(0, main, 1) - print.match.r(0, main, -1), 2), " (", round(sd(print.match.boot1[1,] - print.match.boot0[1,]), 2), " )",  sdp=""),
             paste(round(print.match.r(1, main, 1) - print.match.r(1, main, -1), 2), " (", round(sd(print.match.boot1[2,] - print.match.boot0[2,]), 2), " )",  sdp=""),
             paste(round(print.match.r(2, main, 1) - print.match.r(2, main, -1), 2), " (", round(sd(print.match.boot1[3,] - print.match.boot0[3,]), 2), " )",  sdp=""),
             paste(round(print.match.r(3, main, 1) - print.match.r(3, main, -1), 2), " (", round(sd(print.match.boot1[4,] - print.match.boot0[4,]), 2), " )",  sdp=""),
             paste(round(print.match.r(4, main, 1) - print.match.r(4, main, -1), 2), " (", round(sd(print.match.boot1[5,] - print.match.boot0[5,]), 2), " )",  sdp=""),
             paste(round(print.match.r(5, main, 1) - print.match.r(5, main, -1), 2), " (", round(sd(print.match.boot1[6,] - print.match.boot0[6,]), 2), " )",  sdp=""),
             paste(round(print.match.r(6, main, 1) - print.match.r(6, main, -1), 2), " (", round(sd(print.match.boot1[7,] - print.match.boot0[7,]), 2), " )",  sdp=""),
             paste(round(print.match.r(7, main, 1) - print.match.r(7, main, -1), 2), " (", round(sd(print.match.boot1[8,] - print.match.boot0[8,]), 2), " )",  sdp=""),
             paste(round(print.match.r(8, main, 1) - print.match.r(8, main, -1), 2), " (", round(sd(print.match.boot1[9,] - print.match.boot0[9,]), 2), " )",  sdp=""),
             paste(round(print.match.r(9, main, 1) - print.match.r(9, main, -1), 2), " (", round(sd(print.match.boot1[10,] - print.match.boot0[10,]), 2), " )",  sdp=""))
print(xtable(mat))
############
coeff = rbind(supreme_loglin_fit1, supreme_loglin_fit2, supreme_loglin_fit3, 
              supreme_loglin_fit8, supreme_loglin_fit9)

print(xtable(coeff[,1:10], digits = 3))

effect.mat = matrix(0, 9, 9)
effect.mat[which(lower(Adj.final)[which(lower(Adj.final) == 1)]==1)] = colMeans(coeff)[11:29]

effect.mat[lower.tri(effect.mat)][which(lower(Adj.final) == 1)] = colMeans(coeff)[11:29]
effect.mat[upper.tri(effect.mat, diag = FALSE)] = t(effect.mat)[upper.tri(effect.mat, diag = FALSE) ]
