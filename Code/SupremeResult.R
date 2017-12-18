library(xtable)
library(gtools)
source(SupremeModel.R)
### auxiliary functions
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
  # This function prints out the probability of outcomes (outcomes) given the treatment assignment (treat)
  # using the 'pars' (coefficients of log-linear model) when we have nine subjects
  # par = estimated parameter with a length of 38.
  # outcomes = a set of Y
  # treat = treatment received
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

print.match.r = function(r, pars, treat){
  # This function prints out all the probability of having 
  # r-number of liberal-side votes
  
  if(r==0){
    probs = print.prob.nine(pars, outcomes = rep(-1, 9), treat)
    return(probs)
  }
  # r : the number of 1 (liberal) in outcomes (> 0 )
  # consider all possible cases (9 choose r)
  set1 = combinations(9, r, v = 1:9)
  input.outcome = matrix(-1, nrow(set1), 9)
  for(i in 1:nrow(set1)){
    input.outcome[i,set1[i,]] = rep(1,r)
  }
  # sum over all possible scenario to have r liberal-side votes under 'treat'.
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

## main effect
mat = matrix(0, nrow = 4, ncol = 5)
mat[1,] = c("constant", "WHRehnquist", "JPStevens", "SDOConnor", "AScalia")
mat[3,] = c("AMKennedy", "DHSouter", "CThomas", "RBGinsburg", "SGBreyer")
mat[2,] = c(paste(formatC(main[1],2, format = "f"), " [", formatC(findL(boot.sample[,1]), 2, format = "f"), " ,", formatC(findU(boot.sample[,1]), 2, format = "f"),"]", sep=""),
            paste(formatC(main[2],2, format = "f"), " [", formatC(findL(boot.sample[,2]), 2, format = "f"), " ,", formatC(findU(boot.sample[,2]), 2, format = "f"),"]", sep=""),
            paste(formatC(main[3],2, format = "f"), " [", formatC(findL(boot.sample[,3]), 2, format = "f"), " ,", formatC(findU(boot.sample[,3]), 2, format = "f"),"]", sep=""),
            paste(formatC(main[4],2, format = "f"), " [", formatC(findL(boot.sample[,4]), 2, format = "f"), " ,", formatC(findU(boot.sample[,4]), 2, format = "f"),"]", sep=""),
            paste(formatC(main[5],2, format = "f"), " [", formatC(findL(boot.sample[,5]), 2, format = "f"), " ,", formatC(findU(boot.sample[,5]), 2, format = "f"),"]", sep=""))

mat[4,] =  c( paste(formatC(main[6],2, format = "f"), " [", formatC(findL(boot.sample[,6]), 2, format = "f"), " ,", formatC(findU(boot.sample[,6]), 2, format = "f"),"]", sep=""),
              paste(formatC(main[7],2, format = "f"), " [", formatC(findL(boot.sample[,7]), 2, format = "f"), " ,", formatC(findU(boot.sample[,7]), 2, format = "f"),"]", sep=""),
              paste(formatC(main[8],2, format = "f"), " [", formatC(findL(boot.sample[,8]), 2, format = "f"), " ,", formatC(findU(boot.sample[,8]), 2, format = "f"),"]", sep=""),
              paste(formatC(main[9],2, format = "f"), " [", formatC(findL(boot.sample[,9]), 2, format = "f"), " ,", formatC(findU(boot.sample[,9]), 2, format = "f"),"]", sep=""),
              paste(formatC(main[10],2, format = "f"), " [", formatC(findL(boot.sample[,10]), 2, format = "f"), " ,", formatC(findU(boot.sample[,10]), 2, format = "f"),"]", sep=""))
case1.main = mat

## the number of vote
### P(|Y| = r | a = 0)
print.match.boot1 = matrix(0, nrow = 10, ncol = nrow(boot.sample)) # 10 x 1000 matrix
print.match.boot0 = matrix(0, nrow = 10, ncol = nrow(boot.sample)) 

for(r in 1:10){
  for(i in 1:nrow(boot.sample)){
    print.match.boot1[r,i] = print.match.r((r-1), boot.sample[i,], 1)  
    print.match.boot0[r,i] = print.match.r((r-1), boot.sample[i,], -1) 
  }
}

mat = matrix(0, nrow = 3, ncol = 10)
rownames(mat) = c("a=1", "a=0", "effect")
colnames(mat) = c(0:9)
mat[1,] = c(paste(formatC(print.match.r(0, main, 1), 2, format = "f"), " (", formatC(sd(print.match.boot1[1,], format = "f"), 2), " )",  sdp=""),
            paste(formatC(print.match.r(1, main, 1), 2, format = "f"), " (", formatC(sd(print.match.boot1[2,], format = "f"), 2), " )",  sdp=""),
            paste(formatC(print.match.r(2, main, 1), 2, format = "f"), " (", formatC(sd(print.match.boot1[3,], format = "f"), 2), " )",  sdp=""),
            paste(formatC(print.match.r(3, main, 1), 2, format = "f"), " (", formatC(sd(print.match.boot1[4,], format = "f"), 2), " )",  sdp=""),
            paste(formatC(print.match.r(4, main, 1), 2, format = "f"), " (", formatC(sd(print.match.boot1[5,], format = "f"), 2), " )",  sdp=""),
            paste(formatC(print.match.r(5, main, 1), 2, format = "f"), " (", formatC(sd(print.match.boot1[6,], format = "f"), 2), " )",  sdp=""),
            paste(formatC(print.match.r(6, main, 1), 2, format = "f"), " (", formatC(sd(print.match.boot1[7,], format = "f"), 2), " )",  sdp=""),
            paste(formatC(print.match.r(7, main, 1), 2, format = "f"), " (", formatC(sd(print.match.boot1[8,], format = "f"), 2), " )",  sdp=""),
            paste(formatC(print.match.r(8, main, 1), 2, format = "f"), " (", formatC(sd(print.match.boot1[9,], format = "f"), 2), " )",  sdp=""),
            paste(formatC(print.match.r(9, main, 1), 2, format = "f"), " (", formatC(sd(print.match.boot1[10,], format = "f"), 2), " )",  sdp=""))
mat[2,] = c(paste(formatC(print.match.r(0, main, -1), 2, format = "f"), " (", formatC(sd(print.match.boot0[1,], format = "f"), 2), " )",  sdp=""),
            paste(formatC(print.match.r(1, main, -1), 2, format = "f"), " (", formatC(sd(print.match.boot0[2,], format = "f"), 2), " )",  sdp=""),
            paste(formatC(print.match.r(2, main, -1), 2, format = "f"), " (", formatC(sd(print.match.boot0[3,], format = "f"), 2), " )",  sdp=""),
            paste(formatC(print.match.r(3, main, -1), 2, format = "f"), " (", formatC(sd(print.match.boot0[4,], format = "f"), 2), " )",  sdp=""),
            paste(formatC(print.match.r(4, main, -1), 2, format = "f"), " (", formatC(sd(print.match.boot0[5,], format = "f"), 2), " )",  sdp=""),
            paste(formatC(print.match.r(5, main, -1), 2, format = "f"), " (", formatC(sd(print.match.boot0[6,], format = "f"), 2), " )",  sdp=""),
            paste(formatC(print.match.r(6, main, -1), 2, format = "f"), " (", formatC(sd(print.match.boot0[7,], format = "f"), 2), " )",  sdp=""),
            paste(formatC(print.match.r(7, main, -1), 2, format = "f"), " (", formatC(sd(print.match.boot0[8,], format = "f"), 2), " )",  sdp=""),
            paste(formatC(print.match.r(8, main, -1), 2, format = "f"), " (", formatC(sd(print.match.boot0[9,], format = "f"), 2), " )",  sdp=""),
            paste(formatC(print.match.r(9, main, -1), 2, format = "f"), " (", formatC(sd(print.match.boot0[10,], format = "f"), 2), " )",  sdp=""))
mat[3,] =  c(paste(formatC(print.match.r(0, main, 1) - print.match.r(0, main, -1), 2, format = "f"), " (", formatC(sd(print.match.boot1[1,] - print.match.boot0[1,]), 2, format = "f"), " )",  sdp=""),
             paste(formatC(print.match.r(1, main, 1) - print.match.r(1, main, -1), 2, format = "f"), " (", formatC(sd(print.match.boot1[2,] - print.match.boot0[2,]), 2, format = "f"), " )",  sdp=""),
             paste(formatC(print.match.r(2, main, 1) - print.match.r(2, main, -1), 2, format = "f"), " (", formatC(sd(print.match.boot1[3,] - print.match.boot0[3,]), 2, format = "f"), " )",  sdp=""),
             paste(formatC(print.match.r(3, main, 1) - print.match.r(3, main, -1), 2, format = "f"), " (", formatC(sd(print.match.boot1[4,] - print.match.boot0[4,]), 2, format = "f"), " )",  sdp=""),
             paste(formatC(print.match.r(4, main, 1) - print.match.r(4, main, -1), 2, format = "f"), " (", formatC(sd(print.match.boot1[5,] - print.match.boot0[5,]), 2, format = "f"), " )",  sdp=""),
             paste(formatC(print.match.r(5, main, 1) - print.match.r(5, main, -1), 2, format = "f"), " (", formatC(sd(print.match.boot1[6,] - print.match.boot0[6,]), 2, format = "f"), " )",  sdp=""),
             paste(formatC(print.match.r(6, main, 1) - print.match.r(6, main, -1), 2, format = "f"), " (", formatC(sd(print.match.boot1[7,] - print.match.boot0[7,]), 2, format = "f"), " )",  sdp=""),
             paste(formatC(print.match.r(7, main, 1) - print.match.r(7, main, -1), 2, format = "f"), " (", formatC(sd(print.match.boot1[8,] - print.match.boot0[8,]), 2, format = "f"), " )",  sdp=""),
             paste(formatC(print.match.r(8, main, 1) - print.match.r(8, main, -1), 2, format = "f"), " (", formatC(sd(print.match.boot1[9,] - print.match.boot0[9,]), 2, format = "f"), " )",  sdp=""),
             paste(formatC(print.match.r(9, main, 1) - print.match.r(9, main, -1), 2, format = "f"), " (", formatC(sd(print.match.boot1[10,] - print.match.boot0[10,]), 2, format = "f"), " )",  sdp=""))
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
mat[2,] = c(paste(formatC(main[1],2, format = "f"), " [", formatC(findL(boot.sample[,1]), 2, format = "f"), " ,", formatC(findU(boot.sample[,1]), 2, format = "f"),"]", sep=""),
            paste(formatC(main[2],2, format = "f"), " [", formatC(findL(boot.sample[,2]), 2, format = "f"), " ,", formatC(findU(boot.sample[,2]), 2, format = "f"),"]", sep=""),
            paste(formatC(main[3],2, format = "f"), " [", formatC(findL(boot.sample[,3]), 2, format = "f"), " ,", formatC(findU(boot.sample[,3]), 2, format = "f"),"]", sep=""),
            paste(formatC(main[4],2, format = "f"), " [", formatC(findL(boot.sample[,4]), 2, format = "f"), " ,", formatC(findU(boot.sample[,4]), 2, format = "f"),"]", sep=""),
            paste(formatC(main[5],2, format = "f"), " [", formatC(findL(boot.sample[,5]), 2, format = "f"), " ,", formatC(findU(boot.sample[,5]), 2, format = "f"),"]", sep=""))

mat[4,] =  c( paste(formatC(main[6],2, format = "f"), " [", formatC(findL(boot.sample[,6]), 2, format = "f"), " ,", formatC(findU(boot.sample[,6]), 2, format = "f"),"]", sep=""),
              paste(formatC(main[7],2, format = "f"), " [", formatC(findL(boot.sample[,7]), 2, format = "f"), " ,", formatC(findU(boot.sample[,7]), 2, format = "f"),"]", sep=""),
              paste(formatC(main[8],2, format = "f"), " [", formatC(findL(boot.sample[,8]), 2, format = "f"), " ,", formatC(findU(boot.sample[,8]), 2, format = "f"),"]", sep=""),
              paste(formatC(main[9],2, format = "f"), " [", formatC(findL(boot.sample[,9]), 2, format = "f"), " ,", formatC(findU(boot.sample[,9]), 2, format = "f"),"]", sep=""),
              paste(formatC(main[10],2, format = "f"), " [", formatC(findL(boot.sample[,10]), 2, format = "f"), " ,", formatC(findU(boot.sample[,10]), 2, format = "f"),"]", sep=""))
case2.main = mat

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
mat[1,] = c(paste(formatC(print.match.r(0, main, 1), 2, format = "f"), " (", formatC(sd(print.match.boot1[1,], format = "f"), 2), " )",  sdp=""),
            paste(formatC(print.match.r(1, main, 1), 2, format = "f"), " (", formatC(sd(print.match.boot1[2,], format = "f"), 2), " )",  sdp=""),
            paste(formatC(print.match.r(2, main, 1), 2, format = "f"), " (", formatC(sd(print.match.boot1[3,], format = "f"), 2), " )",  sdp=""),
            paste(formatC(print.match.r(3, main, 1), 2, format = "f"), " (", formatC(sd(print.match.boot1[4,], format = "f"), 2), " )",  sdp=""),
            paste(formatC(print.match.r(4, main, 1), 2, format = "f"), " (", formatC(sd(print.match.boot1[5,], format = "f"), 2), " )",  sdp=""),
            paste(formatC(print.match.r(5, main, 1), 2, format = "f"), " (", formatC(sd(print.match.boot1[6,], format = "f"), 2), " )",  sdp=""),
            paste(formatC(print.match.r(6, main, 1), 2, format = "f"), " (", formatC(sd(print.match.boot1[7,], format = "f"), 2), " )",  sdp=""),
            paste(formatC(print.match.r(7, main, 1), 2, format = "f"), " (", formatC(sd(print.match.boot1[8,], format = "f"), 2), " )",  sdp=""),
            paste(formatC(print.match.r(8, main, 1), 2, format = "f"), " (", formatC(sd(print.match.boot1[9,], format = "f"), 2), " )",  sdp=""),
            paste(formatC(print.match.r(9, main, 1), 2, format = "f"), " (", formatC(sd(print.match.boot1[10,], format = "f"), 2), " )",  sdp=""))
mat[2,] = c(paste(formatC(print.match.r(0, main, -1), 2, format = "f"), " (", formatC(sd(print.match.boot0[1,], format = "f"), 2), " )",  sdp=""),
            paste(formatC(print.match.r(1, main, -1), 2, format = "f"), " (", formatC(sd(print.match.boot0[2,], format = "f"), 2), " )",  sdp=""),
            paste(formatC(print.match.r(2, main, -1), 2, format = "f"), " (", formatC(sd(print.match.boot0[3,], format = "f"), 2), " )",  sdp=""),
            paste(formatC(print.match.r(3, main, -1), 2, format = "f"), " (", formatC(sd(print.match.boot0[4,], format = "f"), 2), " )",  sdp=""),
            paste(formatC(print.match.r(4, main, -1), 2, format = "f"), " (", formatC(sd(print.match.boot0[5,], format = "f"), 2), " )",  sdp=""),
            paste(formatC(print.match.r(5, main, -1), 2, format = "f"), " (", formatC(sd(print.match.boot0[6,], format = "f"), 2), " )",  sdp=""),
            paste(formatC(print.match.r(6, main, -1), 2, format = "f"), " (", formatC(sd(print.match.boot0[7,], format = "f"), 2), " )",  sdp=""),
            paste(formatC(print.match.r(7, main, -1), 2, format = "f"), " (", formatC(sd(print.match.boot0[8,], format = "f"), 2), " )",  sdp=""),
            paste(formatC(print.match.r(8, main, -1), 2, format = "f"), " (", formatC(sd(print.match.boot0[9,], format = "f"), 2), " )",  sdp=""),
            paste(formatC(print.match.r(9, main, -1), 2, format = "f"), " (", formatC(sd(print.match.boot0[10,], format = "f"), 2), " )",  sdp=""))
mat[3,] =  c(paste(formatC(print.match.r(0, main, 1) - print.match.r(0, main, -1), 2, format = "f"), " (", formatC(sd(print.match.boot1[1,] - print.match.boot0[1,]), 2, format = "f"), " )",  sdp=""),
             paste(formatC(print.match.r(1, main, 1) - print.match.r(1, main, -1), 2, format = "f"), " (", formatC(sd(print.match.boot1[2,] - print.match.boot0[2,]), 2, format = "f"), " )",  sdp=""),
             paste(formatC(print.match.r(2, main, 1) - print.match.r(2, main, -1), 2, format = "f"), " (", formatC(sd(print.match.boot1[3,] - print.match.boot0[3,]), 2, format = "f"), " )",  sdp=""),
             paste(formatC(print.match.r(3, main, 1) - print.match.r(3, main, -1), 2, format = "f"), " (", formatC(sd(print.match.boot1[4,] - print.match.boot0[4,]), 2, format = "f"), " )",  sdp=""),
             paste(formatC(print.match.r(4, main, 1) - print.match.r(4, main, -1), 2, format = "f"), " (", formatC(sd(print.match.boot1[5,] - print.match.boot0[5,]), 2, format = "f"), " )",  sdp=""),
             paste(formatC(print.match.r(5, main, 1) - print.match.r(5, main, -1), 2, format = "f"), " (", formatC(sd(print.match.boot1[6,] - print.match.boot0[6,]), 2, format = "f"), " )",  sdp=""),
             paste(formatC(print.match.r(6, main, 1) - print.match.r(6, main, -1), 2, format = "f"), " (", formatC(sd(print.match.boot1[7,] - print.match.boot0[7,]), 2, format = "f"), " )",  sdp=""),
             paste(formatC(print.match.r(7, main, 1) - print.match.r(7, main, -1), 2, format = "f"), " (", formatC(sd(print.match.boot1[8,] - print.match.boot0[8,]), 2, format = "f"), " )",  sdp=""),
             paste(formatC(print.match.r(8, main, 1) - print.match.r(8, main, -1), 2, format = "f"), " (", formatC(sd(print.match.boot1[9,] - print.match.boot0[9,]), 2, format = "f"), " )",  sdp=""),
             paste(formatC(print.match.r(9, main, 1) - print.match.r(9, main, -1), 2, format = "f"), " (", formatC(sd(print.match.boot1[10,] - print.match.boot0[10,]), 2, format = "f"), " )",  sdp=""))
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
mat[2,] = c(paste(formatC(main[1],2, format = "f"), " [", formatC(findL(boot.sample[,1]), 2, format = "f"), " ,", formatC(findU(boot.sample[,1]), 2, format = "f"),"]", sep=""),
            paste(formatC(main[2], 2, format = "f"), " [", formatC(findL(boot.sample[,2]), 2, format = "f"), " ,", formatC(findU(boot.sample[,2]), 2, format = "f"),"]", sep=""),
            paste(formatC(main[3],2, format = "f"), " [", formatC(findL(boot.sample[,3]), 2, format = "f"), " ,", formatC(findU(boot.sample[,3]), 2, format = "f"),"]", sep=""),
            paste(formatC(main[4],2, format = "f"), " [", formatC(findL(boot.sample[,4]), 2, format = "f"), " ,", formatC(findU(boot.sample[,4]), 2, format = "f"),"]", sep=""),
            paste(formatC(main[5],2, format = "f"), " [", formatC(findL(boot.sample[,5]), 2, format = "f"), " ,", formatC(findU(boot.sample[,5]), 2, format = "f"),"]", sep=""))

mat[4,] =  c( paste(formatC(main[6],2, format = "f"), " [", formatC(findL(boot.sample[,6]), 2, format = "f"), " ,", formatC(findU(boot.sample[,6]), 2, format = "f"),"]", sep=""),
              paste(formatC(main[7],2, format = "f"), " [", formatC(findL(boot.sample[,7]), 2, format = "f"), " ,", formatC(findU(boot.sample[,7]), 2, format = "f"),"]", sep=""),
              paste(formatC(main[8],2, format = "f"), " [", formatC(findL(boot.sample[,8]), 2, format = "f"), " ,", formatC(findU(boot.sample[,8]), 2, format = "f"),"]", sep=""),
              paste(formatC(main[9],2, format = "f"), " [", formatC(findL(boot.sample[,9]), 2, format = "f"), " ,", formatC(findU(boot.sample[,9]), 2, format = "f"),"]", sep=""),
              paste(formatC(main[10],2, format = "f"), " [", formatC(findL(boot.sample[,10]), 2, format = "f"), " ,", formatC(findU(boot.sample[,10]), 2, format = "f"),"]", sep=""))
case8.main = mat

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
mat[1,] = c(paste(formatC(print.match.r(0, main, 1), 2, format = "f"), " (", formatC(sd(print.match.boot1[1,], format = "f"), 2), " )",  sdp=""),
            paste(formatC(print.match.r(1, main, 1), 2, format = "f"), " (", formatC(sd(print.match.boot1[2,], format = "f"), 2), " )",  sdp=""),
            paste(formatC(print.match.r(2, main, 1), 2, format = "f"), " (", formatC(sd(print.match.boot1[3,], format = "f"), 2), " )",  sdp=""),
            paste(formatC(print.match.r(3, main, 1), 2, format = "f"), " (", formatC(sd(print.match.boot1[4,], format = "f"), 2), " )",  sdp=""),
            paste(formatC(print.match.r(4, main, 1), 2, format = "f"), " (", formatC(sd(print.match.boot1[5,], format = "f"), 2), " )",  sdp=""),
            paste(formatC(print.match.r(5, main, 1), 2, format = "f"), " (", formatC(sd(print.match.boot1[6,], format = "f"), 2), " )",  sdp=""),
            paste(formatC(print.match.r(6, main, 1), 2, format = "f"), " (", formatC(sd(print.match.boot1[7,], format = "f"), 2), " )",  sdp=""),
            paste(formatC(print.match.r(7, main, 1), 2, format = "f"), " (", formatC(sd(print.match.boot1[8,], format = "f"), 2), " )",  sdp=""),
            paste(formatC(print.match.r(8, main, 1), 2, format = "f"), " (", formatC(sd(print.match.boot1[9,], format = "f"), 2), " )",  sdp=""),
            paste(formatC(print.match.r(9, main, 1), 2, format = "f"), " (", formatC(sd(print.match.boot1[10,], format = "f"), 2), " )",  sdp=""))
mat[2,] = c(paste(formatC(print.match.r(0, main, -1), 2, format = "f"), " (", formatC(sd(print.match.boot0[1,], format = "f"), 2), " )",  sdp=""),
            paste(formatC(print.match.r(1, main, -1), 2, format = "f"), " (", formatC(sd(print.match.boot0[2,], format = "f"), 2), " )",  sdp=""),
            paste(formatC(print.match.r(2, main, -1), 2, format = "f"), " (", formatC(sd(print.match.boot0[3,], format = "f"), 2), " )",  sdp=""),
            paste(formatC(print.match.r(3, main, -1), 2, format = "f"), " (", formatC(sd(print.match.boot0[4,], format = "f"), 2), " )",  sdp=""),
            paste(formatC(print.match.r(4, main, -1), 2, format = "f"), " (", formatC(sd(print.match.boot0[5,], format = "f"), 2), " )",  sdp=""),
            paste(formatC(print.match.r(5, main, -1), 2, format = "f"), " (", formatC(sd(print.match.boot0[6,], format = "f"), 2), " )",  sdp=""),
            paste(formatC(print.match.r(6, main, -1), 2, format = "f"), " (", formatC(sd(print.match.boot0[7,], format = "f"), 2), " )",  sdp=""),
            paste(formatC(print.match.r(7, main, -1), 2, format = "f"), " (", formatC(sd(print.match.boot0[8,], format = "f"), 2), " )",  sdp=""),
            paste(formatC(print.match.r(8, main, -1), 2, format = "f"), " (", formatC(sd(print.match.boot0[9,], format = "f"), 2), " )",  sdp=""),
            paste(formatC(print.match.r(9, main, -1), 2, format = "f"), " (", formatC(sd(print.match.boot0[10,], format = "f"), 2), " )",  sdp=""))
mat[3,] =  c(paste(formatC(print.match.r(0, main, 1) - print.match.r(0, main, -1), 2, format = "f"), " (", formatC(sd(print.match.boot1[1,] - print.match.boot0[1,]), 2, format = "f"), " )",  sdp=""),
             paste(formatC(print.match.r(1, main, 1) - print.match.r(1, main, -1), 2, format = "f"), " (", formatC(sd(print.match.boot1[2,] - print.match.boot0[2,]), 2, format = "f"), " )",  sdp=""),
             paste(formatC(print.match.r(2, main, 1) - print.match.r(2, main, -1), 2, format = "f"), " (", formatC(sd(print.match.boot1[3,] - print.match.boot0[3,]), 2, format = "f"), " )",  sdp=""),
             paste(formatC(print.match.r(3, main, 1) - print.match.r(3, main, -1), 2, format = "f"), " (", formatC(sd(print.match.boot1[4,] - print.match.boot0[4,]), 2, format = "f"), " )",  sdp=""),
             paste(formatC(print.match.r(4, main, 1) - print.match.r(4, main, -1), 2, format = "f"), " (", formatC(sd(print.match.boot1[5,] - print.match.boot0[5,]), 2, format = "f"), " )",  sdp=""),
             paste(formatC(print.match.r(5, main, 1) - print.match.r(5, main, -1), 2, format = "f"), " (", formatC(sd(print.match.boot1[6,] - print.match.boot0[6,]), 2, format = "f"), " )",  sdp=""),
             paste(formatC(print.match.r(6, main, 1) - print.match.r(6, main, -1), 2, format = "f"), " (", formatC(sd(print.match.boot1[7,] - print.match.boot0[7,]), 2, format = "f"), " )",  sdp=""),
             paste(formatC(print.match.r(7, main, 1) - print.match.r(7, main, -1), 2, format = "f"), " (", formatC(sd(print.match.boot1[8,] - print.match.boot0[8,]), 2, format = "f"), " )",  sdp=""),
             paste(formatC(print.match.r(8, main, 1) - print.match.r(8, main, -1), 2, format = "f"), " (", formatC(sd(print.match.boot1[9,] - print.match.boot0[9,]), 2, format = "f"), " )",  sdp=""),
             paste(formatC(print.match.r(9, main, 1) - print.match.r(9, main, -1), 2, format = "f"), " (", formatC(sd(print.match.boot1[10,] - print.match.boot0[10,]), 2, format = "f"), " )",  sdp=""))
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
mat[2,] = c(paste(formatC(main[1],2, format = "f"), " [", formatC(findL(boot.sample[,1]), 2, format = "f"), " ,", formatC(findU(boot.sample[,1]), 2, format = "f"),"]", sep=""),
            paste(formatC(main[2],2, format = "f"), " [", formatC(findL(boot.sample[,2]), 2, format = "f"), " ,", formatC(findU(boot.sample[,2]), 2, format = "f"),"]", sep=""),
            paste(formatC(main[3],2, format = "f"), " [", formatC(findL(boot.sample[,3]), 2, format = "f"), " ,", formatC(findU(boot.sample[,3]), 2, format = "f"),"]", sep=""),
            paste(formatC(main[4],2, format = "f"), " [", formatC(findL(boot.sample[,4]), 2, format = "f"), " ,", formatC(findU(boot.sample[,4]), 2, format = "f"),"]", sep=""),
            paste(formatC(main[5],2, format = "f"), " [", formatC(findL(boot.sample[,5]), 2, format = "f"), " ,", formatC(findU(boot.sample[,5]), 2, format = "f"),"]", sep=""))

mat[4,] =  c( paste(formatC(main[6],2, format = "f"), " [", formatC(findL(boot.sample[,6]), 2, format = "f"), " ,", formatC(findU(boot.sample[,6]), 2, format = "f"),"]", sep=""),
              paste(formatC(main[7],2, format = "f"), " [", formatC(findL(boot.sample[,7]), 2, format = "f"), " ,", formatC(findU(boot.sample[,7]), 2, format = "f"),"]", sep=""),
              paste(formatC(main[8],2, format = "f"), " [", formatC(findL(boot.sample[,8]), 2, format = "f"), " ,", formatC(findU(boot.sample[,8]), 2, format = "f"),"]", sep=""),
              paste(formatC(main[9],2, format = "f"), " [", formatC(findL(boot.sample[,9]), 2, format = "f"), " ,", formatC(findU(boot.sample[,9]), 2, format = "f"),"]", sep=""),
              paste(formatC(main[10],2, format = "f"), " [", formatC(findL(boot.sample[,10]), 2, format = "f"), " ,", formatC(findU(boot.sample[,10]), 2, format = "f"),"]", sep=""))
case9.main = mat

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
mat[1,] = c(paste(formatC(print.match.r(0, main, 1), 2, format = "f"), " (", formatC(sd(print.match.boot1[1,], format = "f"), 2), " )",  sdp=""),
            paste(formatC(print.match.r(1, main, 1), 2, format = "f"), " (", formatC(sd(print.match.boot1[2,], format = "f"), 2), " )",  sdp=""),
            paste(formatC(print.match.r(2, main, 1), 2, format = "f"), " (", formatC(sd(print.match.boot1[3,], format = "f"), 2), " )",  sdp=""),
            paste(formatC(print.match.r(3, main, 1), 2, format = "f"), " (", formatC(sd(print.match.boot1[4,], format = "f"), 2), " )",  sdp=""),
            paste(formatC(print.match.r(4, main, 1), 2, format = "f"), " (", formatC(sd(print.match.boot1[5,], format = "f"), 2), " )",  sdp=""),
            paste(formatC(print.match.r(5, main, 1), 2, format = "f"), " (", formatC(sd(print.match.boot1[6,], format = "f"), 2), " )",  sdp=""),
            paste(formatC(print.match.r(6, main, 1), 2, format = "f"), " (", formatC(sd(print.match.boot1[7,], format = "f"), 2), " )",  sdp=""),
            paste(formatC(print.match.r(7, main, 1), 2, format = "f"), " (", formatC(sd(print.match.boot1[8,], format = "f"), 2), " )",  sdp=""),
            paste(formatC(print.match.r(8, main, 1), 2, format = "f"), " (", formatC(sd(print.match.boot1[9,], format = "f"), 2), " )",  sdp=""),
            paste(formatC(print.match.r(9, main, 1), 2, format = "f"), " (", formatC(sd(print.match.boot1[10,], format = "f"), 2), " )",  sdp=""))
mat[2,] = c(paste(formatC(print.match.r(0, main, -1), 2, format = "f"), " (", formatC(sd(print.match.boot0[1,], format = "f"), 2), " )",  sdp=""),
            paste(formatC(print.match.r(1, main, -1), 2, format = "f"), " (", formatC(sd(print.match.boot0[2,], format = "f"), 2), " )",  sdp=""),
            paste(formatC(print.match.r(2, main, -1), 2, format = "f"), " (", formatC(sd(print.match.boot0[3,], format = "f"), 2), " )",  sdp=""),
            paste(formatC(print.match.r(3, main, -1), 2, format = "f"), " (", formatC(sd(print.match.boot0[4,], format = "f"), 2), " )",  sdp=""),
            paste(formatC(print.match.r(4, main, -1), 2, format = "f"), " (", formatC(sd(print.match.boot0[5,], format = "f"), 2), " )",  sdp=""),
            paste(formatC(print.match.r(5, main, -1), 2, format = "f"), " (", formatC(sd(print.match.boot0[6,], format = "f"), 2), " )",  sdp=""),
            paste(formatC(print.match.r(6, main, -1), 2, format = "f"), " (", formatC(sd(print.match.boot0[7,], format = "f"), 2), " )",  sdp=""),
            paste(formatC(print.match.r(7, main, -1), 2, format = "f"), " (", formatC(sd(print.match.boot0[8,], format = "f"), 2), " )",  sdp=""),
            paste(formatC(print.match.r(8, main, -1), 2, format = "f"), " (", formatC(sd(print.match.boot0[9,], format = "f"), 2), " )",  sdp=""),
            paste(formatC(print.match.r(9, main, -1), 2, format = "f"), " (", formatC(sd(print.match.boot0[10,], format = "f"), 2), " )",  sdp=""))
mat[3,] =  c(paste(formatC(print.match.r(0, main, 1) - print.match.r(0, main, -1), 2, format = "f"), " (", formatC(sd(print.match.boot1[1,] - print.match.boot0[1,]), 2, format = "f"), " )",  sdp=""),
             paste(formatC(print.match.r(1, main, 1) - print.match.r(1, main, -1), 2, format = "f"), " (", formatC(sd(print.match.boot1[2,] - print.match.boot0[2,]), 2, format = "f"), " )",  sdp=""),
             paste(formatC(print.match.r(2, main, 1) - print.match.r(2, main, -1), 2, format = "f"), " (", formatC(sd(print.match.boot1[3,] - print.match.boot0[3,]), 2, format = "f"), " )",  sdp=""),
             paste(formatC(print.match.r(3, main, 1) - print.match.r(3, main, -1), 2, format = "f"), " (", formatC(sd(print.match.boot1[4,] - print.match.boot0[4,]), 2, format = "f"), " )",  sdp=""),
             paste(formatC(print.match.r(4, main, 1) - print.match.r(4, main, -1), 2, format = "f"), " (", formatC(sd(print.match.boot1[5,] - print.match.boot0[5,]), 2, format = "f"), " )",  sdp=""),
             paste(formatC(print.match.r(5, main, 1) - print.match.r(5, main, -1), 2, format = "f"), " (", formatC(sd(print.match.boot1[6,] - print.match.boot0[6,]), 2, format = "f"), " )",  sdp=""),
             paste(formatC(print.match.r(6, main, 1) - print.match.r(6, main, -1), 2, format = "f"), " (", formatC(sd(print.match.boot1[7,] - print.match.boot0[7,]), 2, format = "f"), " )",  sdp=""),
             paste(formatC(print.match.r(7, main, 1) - print.match.r(7, main, -1), 2, format = "f"), " (", formatC(sd(print.match.boot1[8,] - print.match.boot0[8,]), 2, format = "f"), " )",  sdp=""),
             paste(formatC(print.match.r(8, main, 1) - print.match.r(8, main, -1), 2, format = "f"), " (", formatC(sd(print.match.boot1[9,] - print.match.boot0[9,]), 2, format = "f"), " )",  sdp=""),
             paste(formatC(print.match.r(9, main, 1) - print.match.r(9, main, -1), 2, format = "f"), " (", formatC(sd(print.match.boot1[10,] - print.match.boot0[10,]), 2, format = "f"), " )",  sdp=""))
print(xtable(mat))


## summarize the main effect
main.effect = rbind(case1.main[c(1:2),], case2.main[2,], case8.main[2,], case9.main[2,],
                    case1.main[c(3:4),], case2.main[4,], case8.main[4,], case9.main[4,])
rownames(main.effect) = make.names(rep(c("Issue", "Criminal procedure", "Civil rights",
                              "Economic activity", "Judicial power"),2), unique = TRUE)
print(xtable(main.effect), include.colnames = FALSE)


## edge weight for Figure 4 : proportional to the magnitude of two-way interations
# 1 - 2,3,4,5,7,8,9
# 2 - 5,6,8,9
# 3 - 5,6,9
# 4 - 7
# 6 - 8,9
# 7 - 9
# 8 - 9
abs(supreme_loglin_fit9[c(11:29)])*2