########################################
######### auxiliary functions ##########
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
    pars[30]*outcomes[1]*treat[1] + pars[31]*outcomes[2]*treat[2] + 
    pars[32]*outcomes[3]*treat[3] + pars[33]*outcomes[4]*treat[4] +
    pars[34]*outcomes[5]*treat[5] + pars[35]*outcomes[6]*treat[6] +
    pars[36]*outcomes[7]*treat[7] + pars[37]*outcomes[8]*treat[8] + 
    pars[38]*outcomes[9]*treat[9] 
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


################################
load("Data/supreme_loglin_Gibbs.RData")
boot.sample = matrix(0, nrow = length(supreme_loglin_Gibbs), ncol = 38)
for(i in 1:length(supreme_loglin_Gibbs)){
  boot.sample[i,] = supreme_loglin_Gibbs[[i]]
}

######
print.all1.treat.liberal = print.all1.treat.conserv = rep(0, length(supreme_loglin_Gibbs))
print.all0.treat.liberal = print.all0.treat.conserv = rep(0, length(supreme_loglin_Gibbs))
print.five.treat.liberal = print.five.treat.conserv = rep(0, length(supreme_loglin_Gibbs))
print.four.treat.liberal = print.four.treat.conserv = rep(0, length(supreme_loglin_Gibbs))
######
treat.liberal = rep(0,9); treat.liberal[c(2,6,8,9)] = rep(1,4)
treat.conserv = rep(0,9); treat.conserv[c(3,4,5,7)] = rep(1,4)
######
for(i in 1:nrow(boot.sample)){
  print.all1.treat.liberal[i] = print.match.r(9, boot.sample[i,], treat = treat.liberal)  
  print.all1.treat.conserv[i] = print.match.r(9, boot.sample[i,], treat = treat.conserv) 
  
  print.all0.treat.liberal[i] = print.match.r(0, boot.sample[i,], treat = treat.liberal)  
  print.all0.treat.conserv[i] = print.match.r(0, boot.sample[i,], treat = treat.conserv) 
  
  print.five.treat.liberal[i] = print.match.r(5, boot.sample[i,], treat = treat.liberal)  
  print.five.treat.conserv[i] = print.match.r(5, boot.sample[i,], treat = treat.conserv) 
  
  print.four.treat.liberal[i] = print.match.r(4, boot.sample[i,], treat = treat.liberal)  
  print.four.treat.conserv[i] = print.match.r(4, boot.sample[i,], treat = treat.conserv) 
}

######
tab = matrix(0, nrow = 4, ncol = 2)

tab[1,] = c(paste(formatC(mean(print.all1.treat.liberal), 4, format="f") , "(", formatC(sd(print.all1.treat.liberal), 4, format="f"), ")",sep=""),
            paste(formatC(mean(print.all1.treat.conserv), 4, format="f") , "(", formatC(sd(print.all1.treat.conserv), 4, format="f"), ")",sep=""))

tab[2,] = c(paste(formatC(mean(print.all0.treat.liberal), 4, format="f") , "(", formatC(sd(print.all0.treat.liberal), 4, format="f"), ")",sep=""),
            paste(formatC(mean(print.all0.treat.conserv), 4, format="f") , "(", formatC(sd(print.all0.treat.conserv), 4, format="f"), ")",sep=""))

tab[3,] = c(paste(formatC(mean(print.five.treat.liberal), 4, format="f") , "(", formatC(sd(print.five.treat.liberal), 4, format="f"), ")",sep=""),
            paste(formatC(mean(print.five.treat.conserv), 4, format="f") , "(", formatC(sd(print.five.treat.conserv), 4, format="f"), ")",sep=""))

tab[4,] = c(paste(formatC(mean(print.four.treat.liberal), 4, format="f") , "(", formatC(sd(print.four.treat.liberal), 4, format="f"), ")",sep=""),
            paste(formatC(mean(print.four.treat.conserv), 4, format="f") , "(", formatC(sd(print.four.treat.conserv), 4, format="f"), ")",sep=""))

print(xtable(tab))

########################################
print.all1.treat.Rehnquist = print.all1.treat.Thomas = print.all1.treat.Stevens = print.all1.treat.Scalia = rep(0, length(supreme_loglin_Gibbs))
print.all0.treat.Rehnquist = print.all0.treat.Thomas = print.all0.treat.Stevens = print.all0.treat.Scalia = rep(0, length(supreme_loglin_Gibbs))
print.five.treat.Rehnquist = print.five.treat.Thomas = print.five.treat.Stevens = print.five.treat.Scalia = rep(0, length(supreme_loglin_Gibbs))
print.four.treat.Rehnquist = print.four.treat.Thomas = print.four.treat.Stevens = print.four.treat.Scalia = rep(0, length(supreme_loglin_Gibbs))
######
for(i in 1:nrow(boot.sample)){
  print.all1.treat.Rehnquist[i] = print.match.r(9, boot.sample[i,], c(1,rep(0,8)))  
  print.all1.treat.Thomas[i] = print.match.r(9, boot.sample[i,], c(rep(0,6), 1, 0, 0)) 
  print.all1.treat.Stevens[i] = print.match.r(9, boot.sample[i,], c(0, 1,rep(0,7))) 
  print.all1.treat.Scalia[i] = print.match.r(9, boot.sample[i,], c(rep(0,3), 1, rep(0,5)))
  
  print.all0.treat.Rehnquist[i] = print.match.r(0, boot.sample[i,], c(1,rep(0,8)))  
  print.all0.treat.Thomas[i] = print.match.r(0, boot.sample[i,], c(rep(0,6), 1, 0, 0)) 
  print.all0.treat.Stevens[i] = print.match.r(0, boot.sample[i,],  c(0, 1,rep(0,7))) 
  print.all0.treat.Scalia[i] = print.match.r(0, boot.sample[i,], c(rep(0,3), 1, rep(0,5)))
  
  print.five.treat.Rehnquist[i] = print.match.r(5, boot.sample[i,], c(1,rep(0,8)))  
  print.five.treat.Thomas[i] = print.match.r(5, boot.sample[i,], c(rep(0,6), 1, 0, 0)) 
  print.five.treat.Stevens[i] = print.match.r(5, boot.sample[i,],  c(0, 1,rep(0,7))) 
  print.five.treat.Scalia[i] = print.match.r(5, boot.sample[i,], c(rep(0,3), 1, rep(0,5)))
  
  print.four.treat.Rehnquist[i] = print.match.r(4, boot.sample[i,], c(1,rep(0,8)))  
  print.four.treat.Thomas[i] = print.match.r(4, boot.sample[i,], c(rep(0,6), 1, 0, 0)) 
  print.four.treat.Stevens[i] = print.match.r(4, boot.sample[i,],  c(0, 1,rep(0,7))) 
  print.four.treat.Scalia[i] = print.match.r(4, boot.sample[i,], c(rep(0,3), 1, rep(0,5)))
}
######
tab = matrix(0, nrow = 4, ncol = 4)

tab[1,] = c(paste(formatC(mean(print.all1.treat.Rehnquist), 4, format="f") , "(", formatC(sd(print.all1.treat.Rehnquist), 4, format="f"), ")",sep=""),
            paste(formatC(mean(print.all1.treat.Thomas), 4, format="f") , "(", formatC(sd(print.all1.treat.Thomas), 4, format="f"), ")",sep=""),
            paste(formatC(mean(print.all1.treat.Stevens), 4, format="f") , "(", formatC(sd(print.all1.treat.Stevens), 4, format="f"), ")",sep=""),
            paste(formatC(mean(print.all1.treat.Scalia), 4, format="f") , "(", formatC(sd(print.all1.treat.Scalia), 4, format="f"), ")",sep=""))

tab[2,] = c(paste(formatC(mean(print.all0.treat.Rehnquist), 4, format="f") , "(", formatC(sd(print.all0.treat.Rehnquist), 4, format="f"), ")",sep=""),
            paste(formatC(mean(print.all0.treat.Thomas), 4, format="f") , "(", formatC(sd(print.all0.treat.Thomas), 4, format="f"), ")",sep=""),
            paste(formatC(mean(print.all0.treat.Stevens), 4, format="f") , "(", formatC(sd(print.all0.treat.Stevens), 4, format="f"), ")",sep=""),
            paste(formatC(mean(print.all0.treat.Scalia), 4, format="f") , "(", formatC(sd(print.all0.treat.Scalia), 4, format="f"), ")",sep=""))


tab[3,] = c(paste(formatC(mean(print.five.treat.Rehnquist), 4, format="f") , "(", formatC(sd(print.five.treat.Rehnquist), 4, format="f"), ")",sep=""),
            paste(formatC(mean(print.five.treat.Thomas), 4, format="f") , "(", formatC(sd(print.five.treat.Thomas), 4, format="f"), ")",sep=""),
            paste(formatC(mean(print.five.treat.Stevens), 4, format="f") , "(", formatC(sd(print.five.treat.Stevens), 4, format="f"), ")",sep=""),
            paste(formatC(mean(print.five.treat.Scalia), 4, format="f") , "(", formatC(sd(print.five.treat.Scalia), 4, format="f"), ")",sep=""))


tab[4,] = c(paste(formatC(mean(print.four.treat.Rehnquist), 4, format="f") , "(", formatC(sd(print.four.treat.Rehnquist), 4, format="f"), ")",sep=""),
            paste(formatC(mean(print.four.treat.Thomas), 4, format="f") , "(", formatC(sd(print.four.treat.Thomas), 4, format="f"), ")",sep=""),
            paste(formatC(mean(print.four.treat.Stevens), 4, format="f") , "(", formatC(sd(print.four.treat.Stevens), 4, format="f"), ")",sep=""),
            paste(formatC(mean(print.four.treat.Scalia), 4, format="f") , "(", formatC(sd(print.four.treat.Scalia), 4, format="f"), ")",sep=""))

print(xtable(tab))


##################################
################################
load("Data/supreme_loglin_DAG.RData")
boot.sample = matrix(0, nrow = length(supreme_loglin_DAG), ncol = 38)
for(i in 1:length(supreme_loglin_DAG)){
  boot.sample[i,] = supreme_loglin_DAG[[i]]
}

######
print.all1.treat.liberal = print.all1.treat.conserv = rep(0, length(supreme_loglin_DAG))
print.all0.treat.liberal = print.all0.treat.conserv = rep(0, length(supreme_loglin_DAG))
print.five.treat.liberal = print.five.treat.conserv = rep(0, length(supreme_loglin_DAG))
print.four.treat.liberal = print.four.treat.conserv = rep(0, length(supreme_loglin_DAG))
######
treat.liberal = rep(0,9); treat.liberal[c(2,6,8,9)] = rep(1,4)
treat.conserv = rep(0,9); treat.conserv[c(3,4,5,7)] = rep(1,4)
######
for(i in 1:nrow(boot.sample)){
  print.all1.treat.liberal[i] = print.match.r(9, boot.sample[i,], treat = treat.liberal)  
  print.all1.treat.conserv[i] = print.match.r(9, boot.sample[i,], treat = treat.conserv) 
  
  print.all0.treat.liberal[i] = print.match.r(0, boot.sample[i,], treat = treat.liberal)  
  print.all0.treat.conserv[i] = print.match.r(0, boot.sample[i,], treat = treat.conserv) 
  
  print.five.treat.liberal[i] = print.match.r(5, boot.sample[i,], treat = treat.liberal)  
  print.five.treat.conserv[i] = print.match.r(5, boot.sample[i,], treat = treat.conserv) 
  
  print.four.treat.liberal[i] = print.match.r(4, boot.sample[i,], treat = treat.liberal)  
  print.four.treat.conserv[i] = print.match.r(4, boot.sample[i,], treat = treat.conserv) 
}

######
tab = matrix(0, nrow = 4, ncol = 2)

tab[1,] = c(paste(formatC(mean(print.all1.treat.liberal), 4, format="f") , "(", formatC(sd(print.all1.treat.liberal), 4, format="f"), ")",sep=""),
            paste(formatC(mean(print.all1.treat.conserv), 4, format="f") , "(", formatC(sd(print.all1.treat.conserv), 4, format="f"), ")",sep=""))

tab[2,] = c(paste(formatC(mean(print.all0.treat.liberal), 4, format="f") , "(", formatC(sd(print.all0.treat.liberal), 4, format="f"), ")",sep=""),
            paste(formatC(mean(print.all0.treat.conserv), 4, format="f") , "(", formatC(sd(print.all0.treat.conserv), 4, format="f"), ")",sep=""))

tab[3,] = c(paste(formatC(mean(print.five.treat.liberal), 4, format="f") , "(", formatC(sd(print.five.treat.liberal), 4, format="f"), ")",sep=""),
            paste(formatC(mean(print.five.treat.conserv), 4, format="f") , "(", formatC(sd(print.five.treat.conserv), 4, format="f"), ")",sep=""))

tab[4,] = c(paste(formatC(mean(print.four.treat.liberal), 4, format="f") , "(", formatC(sd(print.four.treat.liberal), 4, format="f"), ")",sep=""),
            paste(formatC(mean(print.four.treat.conserv), 4, format="f") , "(", formatC(sd(print.four.treat.conserv), 4, format="f"), ")",sep=""))

print(xtable(tab))

########################################
print.all1.treat.Rehnquist = print.all1.treat.Thomas = print.all1.treat.Stevens = print.all1.treat.Scalia = rep(0, length(supreme_loglin_DAG))
print.all0.treat.Rehnquist = print.all0.treat.Thomas = print.all0.treat.Stevens = print.all0.treat.Scalia = rep(0, length(supreme_loglin_DAG))
print.five.treat.Rehnquist = print.five.treat.Thomas = print.five.treat.Stevens = print.five.treat.Scalia = rep(0, length(supreme_loglin_DAG))
print.four.treat.Rehnquist = print.four.treat.Thomas = print.four.treat.Stevens = print.four.treat.Scalia = rep(0, length(supreme_loglin_DAG))
######
for(i in 1:nrow(boot.sample)){
  print.all1.treat.Rehnquist[i] = print.match.r(9, boot.sample[i,], c(1,rep(0,8)))  
  print.all1.treat.Thomas[i] = print.match.r(9, boot.sample[i,], c(rep(0,6), 1, 0, 0)) 
  print.all1.treat.Stevens[i] = print.match.r(9, boot.sample[i,], c(0, 1,rep(0,7))) 
  print.all1.treat.Scalia[i] = print.match.r(9, boot.sample[i,], c(rep(0,3), 1, rep(0,5)))
  
  print.all0.treat.Rehnquist[i] = print.match.r(0, boot.sample[i,], c(1,rep(0,8)))  
  print.all0.treat.Thomas[i] = print.match.r(0, boot.sample[i,], c(rep(0,6), 1, 0, 0)) 
  print.all0.treat.Stevens[i] = print.match.r(0, boot.sample[i,],  c(0, 1,rep(0,7))) 
  print.all0.treat.Scalia[i] = print.match.r(0, boot.sample[i,], c(rep(0,3), 1, rep(0,5)))
  
  print.five.treat.Rehnquist[i] = print.match.r(5, boot.sample[i,], c(1,rep(0,8)))  
  print.five.treat.Thomas[i] = print.match.r(5, boot.sample[i,], c(rep(0,6), 1, 0, 0)) 
  print.five.treat.Stevens[i] = print.match.r(5, boot.sample[i,],  c(0, 1,rep(0,7))) 
  print.five.treat.Scalia[i] = print.match.r(5, boot.sample[i,], c(rep(0,3), 1, rep(0,5)))
  
  print.four.treat.Rehnquist[i] = print.match.r(4, boot.sample[i,], c(1,rep(0,8)))  
  print.four.treat.Thomas[i] = print.match.r(4, boot.sample[i,], c(rep(0,6), 1, 0, 0)) 
  print.four.treat.Stevens[i] = print.match.r(4, boot.sample[i,],  c(0, 1,rep(0,7))) 
  print.four.treat.Scalia[i] = print.match.r(4, boot.sample[i,], c(rep(0,3), 1, rep(0,5)))
}
######
tab = matrix(0, nrow = 4, ncol = 4)

tab[1,] = c(paste(formatC(mean(print.all1.treat.Rehnquist), 4, format="f") , "(", formatC(sd(print.all1.treat.Rehnquist), 4, format="f"), ")",sep=""),
            paste(formatC(mean(print.all1.treat.Thomas), 4, format="f") , "(", formatC(sd(print.all1.treat.Thomas), 4, format="f"), ")",sep=""),
            paste(formatC(mean(print.all1.treat.Stevens), 4, format="f") , "(", formatC(sd(print.all1.treat.Stevens), 4, format="f"), ")",sep=""),
            paste(formatC(mean(print.all1.treat.Scalia), 4, format="f") , "(", formatC(sd(print.all1.treat.Scalia), 4, format="f"), ")",sep=""))

tab[2,] = c(paste(formatC(mean(print.all0.treat.Rehnquist), 4, format="f") , "(", formatC(sd(print.all0.treat.Rehnquist), 4, format="f"), ")",sep=""),
            paste(formatC(mean(print.all0.treat.Thomas), 4, format="f") , "(", formatC(sd(print.all0.treat.Thomas), 4, format="f"), ")",sep=""),
            paste(formatC(mean(print.all0.treat.Stevens), 4, format="f") , "(", formatC(sd(print.all0.treat.Stevens), 4, format="f"), ")",sep=""),
            paste(formatC(mean(print.all0.treat.Scalia), 4, format="f") , "(", formatC(sd(print.all0.treat.Scalia), 4, format="f"), ")",sep=""))


tab[3,] = c(paste(formatC(mean(print.five.treat.Rehnquist), 4, format="f") , "(", formatC(sd(print.five.treat.Rehnquist), 4, format="f"), ")",sep=""),
            paste(formatC(mean(print.five.treat.Thomas), 4, format="f") , "(", formatC(sd(print.five.treat.Thomas), 4, format="f"), ")",sep=""),
            paste(formatC(mean(print.five.treat.Stevens), 4, format="f") , "(", formatC(sd(print.five.treat.Stevens), 4, format="f"), ")",sep=""),
            paste(formatC(mean(print.five.treat.Scalia), 4, format="f") , "(", formatC(sd(print.five.treat.Scalia), 4, format="f"), ")",sep=""))


tab[4,] = c(paste(formatC(mean(print.four.treat.Rehnquist), 4, format="f") , "(", formatC(sd(print.four.treat.Rehnquist), 4, format="f"), ")",sep=""),
            paste(formatC(mean(print.four.treat.Thomas), 4, format="f") , "(", formatC(sd(print.four.treat.Thomas), 4, format="f"), ")",sep=""),
            paste(formatC(mean(print.four.treat.Stevens), 4, format="f") , "(", formatC(sd(print.four.treat.Stevens), 4, format="f"), ")",sep=""),
            paste(formatC(mean(print.four.treat.Scalia), 4, format="f") , "(", formatC(sd(print.four.treat.Scalia), 4, format="f"), ")",sep=""))

print(xtable(tab))