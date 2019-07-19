### auxiliary functions
log.main.nine = function(pars, outcomes, treat){
  # This function prints out the log of clique factorizations before normalization.
  # Here cliques are base on our ad-hoc structural learning using log-linear model.
  # We are assuming 19 undirected edges between the justices. 
  
  pars[1] +
    sum(outcomes[1:9]*pars[2:10]) + 
    pars[11]*outcomes[1]*outcomes[3] + pars[12]*outcomes[1]*outcomes[4] +
    pars[13]*outcomes[1]*outcomes[5] + pars[14]*outcomes[1]*outcomes[7] +
   
    pars[15]*outcomes[2]*outcomes[6] + pars[16]*outcomes[2]*outcomes[8] +
    pars[17]*outcomes[2]*outcomes[9] +
 
    pars[18]*outcomes[3]*outcomes[5] + pars[19]*outcomes[3]*outcomes[6] +
    pars[20]*outcomes[3]*outcomes[9] +
    
    pars[21]*outcomes[4]*outcomes[5] + pars[22]*outcomes[4]*outcomes[7] + 
    
    pars[23]*outcomes[5]*outcomes[6] + pars[24]*outcomes[5]*outcomes[7] + 
    pars[25]*outcomes[5]*outcomes[8] + 
    
    pars[26]*outcomes[6]*outcomes[8] + pars[27]*outcomes[6]*outcomes[9] + 
    
    pars[28]*outcomes[8]*outcomes[9] + 
    
    pars[29]*outcomes[1]*treat + pars[30]*outcomes[2]*treat + 
    pars[31]*outcomes[3]*treat + pars[32]*outcomes[4]*treat +
    pars[33]*outcomes[5]*treat + pars[34]*outcomes[6]*treat +
    pars[35]*outcomes[7]*treat + pars[36]*outcomes[8]*treat + 
    pars[37]*outcomes[9]*treat 
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