max.time = 6000
### auxiliary functions
log.y = function(pars, Y, treat, cov, ind){
  
  if(ind == 1){
    oneway = pars[1]; twoway = pars[c(10,11,12,13)]; Aeffect=pars[28]; # 3,4,5,7
    main = oneway*Y[1] + sum(twoway*Y[1]*Y[c(3,4,5,7)]) + Aeffect*treat[1]*Y[1] + pars[37]*cov[1]*Y[1]
  }else if(ind == 2){
    oneway = pars[2]; twoway = pars[c(14,15,16)]; Aeffect = pars[29];  # 6,8,9
    main = oneway*Y[2] + sum(twoway*Y[2]*Y[c(6,8,9)]) + Aeffect*treat[2]*Y[2] + pars[38]*cov[2]*Y[2]
  }else if(ind == 3){
    oneway = pars[3]; twoway = pars[c(10,17,18,19)]; Aeffect = pars[30];  # 1,5,7,9
    main = oneway*Y[3] + sum(twoway*Y[3]*Y[c(1,5,7,9)]) + Aeffect*treat[3]*Y[3] + pars[39]*cov[3]*Y[3]
  }else if(ind == 4){
    oneway = pars[4]; twoway = pars[c(11,20,21)]; Aeffect = pars[31]; # 1,5,7
    main = oneway*Y[4] + sum(twoway*Y[4]*Y[c(1,5,7)]) + Aeffect*treat[4]*Y[4] + pars[40]*cov[4]*Y[4]
  }else if(ind == 5){
    oneway = pars[5]; twoway = pars[c(12,17,20,22,23,24)]; Aeffect = pars[32];  # 1,3,4,6,7,8
    main = oneway*Y[5] + sum(twoway*Y[5]*Y[c(1,3,4,6,7,8)]) + Aeffect*treat[5]*Y[5] + pars[41]*cov[5]*Y[5]
  }else if(ind == 6){
    oneway = pars[6]; twoway = pars[c(14,22,25,26)]; Aeffect = pars[33]; # 2,5,8,9
    main = oneway*Y[6] + sum(twoway*Y[6]*Y[c(2,5,8,9)]) + Aeffect*treat[6]*Y[6] + pars[42]*cov[6]*Y[6]
  }else if(ind == 7){
    oneway = pars[7]; twoway = pars[c(13,18,21,23)]; Aeffect = pars[34]; # 1,3,4,5
    main = oneway*Y[7] + sum(twoway*Y[7]*Y[c(1,3,4,5)]) + Aeffect*treat[7]*Y[7] + pars[43]*cov[7]*Y[7]
  }else if(ind == 8){
    oneway = pars[8]; twoway = pars[c(15,24,25,27)]; Aeffect = pars[35];  # 2,5,6,9
    main = oneway*Y[8] + sum(twoway*Y[8]*Y[c(2,5,6,9)]) + Aeffect*treat[8]*Y[8] + pars[44]*cov[8]*Y[8]
  }else if(ind == 9){
    oneway = pars[9]; twoway = pars[c(16,29,26,27)]; Aeffect = pars[36]; # 2,3,6,8
    main = oneway*Y[9] + sum(twoway*Y[9]*Y[c(2,3,6,8)]) + Aeffect*treat[9]*Y[9] + pars[45]*cov[9]*Y[9]
  }
  return(main)
}

print.prob.y = function(pars, all.Y, treat, cov, ind){
  y.list = all.Y; y.list[ind] = 1 
  main = log.y(pars, y.list, treat, cov, ind) 
  ver = c(-1,1); Z.part = 0
  for(j in 1:2){
    y.part = all.Y; y.part[ind] = ver[j]
    Z.part = Z.part + exp(log.y(pars, y.part, treat, cov, ind))
  }
  prop = exp(main) /  Z.part 
  return(prop)
}

print.gibbs.onecov.pars = function(pars, treatment, covariate){
  
  out = outcome = matrix(0, 9, max.time)
  t = 1
  out[,t] = rep(0.5,9)
  outcome[,t] = 2*rbinom(9, 1, out[,t])-1 # random initial value
  
  for(t in 2:max.time){
    
    random.justice = sample(1:9, 1)
    if(random.justice == 1){    
      out[1,t] = print.prob.y(pars, outcome[,t-1], treatment, covariate, ind = 1)
    }else if(random.justice == 2){
      out[2,t] = print.prob.y(pars, outcome[,t-1], treatment, covariate, ind = 2)    
    }else if(random.justice == 3){ 
      out[3,t] = print.prob.y(pars, outcome[,t-1], treatment, covariate, ind = 3)     
    }else if(random.justice == 4){
      out[4,t] = print.prob.y(pars, outcome[,t-1], treatment, covariate, ind = 4)  
    }else if(random.justice == 5){
      out[5,t] = print.prob.y(pars, outcome[,t-1], treatment, covariate, ind = 5)                                          
    }else if(random.justice == 6){
      out[6,t] = print.prob.y(pars, outcome[,t-1], treatment, covariate, ind = 6)  
    }else if(random.justice == 7){
      out[7,t] = print.prob.y(pars, outcome[,t-1], treatment, covariate, ind = 7)   
    }else if(random.justice == 8){
      out[8,t] = print.prob.y(pars, outcome[,t-1], treatment, covariate, ind = 8)   
    }else if(random.justice == 9){
      out[9,t] = print.prob.y(pars, outcome[,t-1], treatment, covariate, ind = 9)
    }
    outcome[,t] = outcome[,(t-1)]                                        
    outcome[random.justice,t] = 2*rbinom(1, 1, out[random.justice,t])- 1
  }
  
  Y.outcomes = t(outcome[,c(t-4999):t])  
  
  return(Y.outcomes) 
}


prob.set = list()
for(ii in 1:500){
  pars = onecov11_DAG_mle[[ii]][[1]]
  inputX = onecov11_DAG[[ii]][[3]]
  
  treat.conservative.prob = treat.liberal.prob = treat.reh.prob = treat.thomas.prob = treat.stevens.prob = treat.scalia.prob = rep(0,4)
  for(i in 1:nrow(inputX)){
    treat.conservative = (print.gibbs.onecov.pars(pars, c(0,0,1,1,1,0,1,0,0), inputX[i,])+1)/2
    treat.conservative.prob =  treat.conservative.prob + c(mean(rowSums(treat.conservative) == 9), mean(rowSums(treat.conservative) == 0),
                                                           mean(rowSums(treat.conservative) == 5), mean(rowSums(treat.conservative) == 4)) / nrow(inputX)
    
    treat.liberal = (print.gibbs.onecov.pars(pars,  c(0,1,0,0,0,1,0,1,1), inputX[i,])+1)/2
    treat.liberal.prob =  treat.liberal.prob + c(mean(rowSums(treat.liberal) == 9), mean(rowSums(treat.liberal) == 0),
                                                 mean(rowSums(treat.liberal) == 5), mean(rowSums(treat.liberal) == 4)) / nrow(inputX)
    
    treat.reh = (print.gibbs.onecov.pars(pars, c(1,rep(0,8)), inputX[i,])+1)/2
    treat.reh.prob =  treat.reh.prob + c(mean(rowSums(treat.reh) == 9), mean(rowSums(treat.reh) == 0),
                                         mean(rowSums(treat.reh) == 5), mean(rowSums(treat.reh) == 4)) / nrow(inputX)
    
    treat.thomas = (print.gibbs.onecov.pars(pars, c(rep(0,6), 1, 0, 0), inputX[i,])+1)/2
    treat.thomas.prob =  treat.thomas.prob + c(mean(rowSums(treat.thomas) == 9), mean(rowSums(treat.thomas) == 0),
                                               mean(rowSums(treat.thomas) == 5), mean(rowSums(treat.thomas) == 4)) / nrow(inputX)
    
    treat.stevens = (print.gibbs.onecov.pars(pars, c(0,1,rep(0,7)), inputX[i,])+1)/2
    treat.stevens.prob =  treat.stevens.prob + c(mean(rowSums(treat.stevens) == 9), mean(rowSums(treat.stevens) == 0),
                                                 mean(rowSums(treat.stevens) == 5), mean(rowSums(treat.stevens) == 4)) / nrow(inputX)
    
    treat.scalia = (print.gibbs.onecov.pars(pars, c(0,0,0,1,0,0,0,0,0), inputX[i,])+1)/2
    treat.scalia.prob =  treat.scalia.prob + c(mean(rowSums(treat.scalia) == 9), mean(rowSums(treat.scalia) == 0),
                                               mean(rowSums(treat.scalia) == 5), mean(rowSums(treat.scalia) == 4)) / nrow(inputX)
  }

  prob.set[[ii]] = list(treat.conservative.prob, treat.liberal.prob, treat.reh.prob, treat.thomas.prob,
              treat.stevens.prob, treat.scalia.prob))
}