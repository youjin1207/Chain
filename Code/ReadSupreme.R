################ reorganize a table ###################
dat.all = read.csv("Data/AllCases.csv", header  = TRUE, sep = ",")
id.types = unique(dat.all$caseId); case.name= c(); case.issue = c()
Justice = matrix(NA, nrow = length(id.types), ncol = length(unique(dat.all$justiceName)))
colnames(Justice) = as.character(unique(dat.all$justiceName))
Justice = as.data.frame(Justice)
for(k in 1:length(id.types)){
  print(k)
  case.name[k] = noquote(as.character(id.types[k]))
  case.issue[k] = (dat.all$issueArea[which(dat.all$caseId == id.types[k])][1])
  
  tmp.name = noquote(as.character(dat.all$justiceName[which(dat.all$caseId == id.types[k])]))
  tmp.vote = as.integer(dat.all$direction[which(dat.all$caseId == id.types[k])])
  for(i in 1:length(tmp.name)){
    Justice[k, colnames(Justice) %in% tmp.name[i] ] =  tmp.vote[i]
  }
}

new.data = as.data.frame(cbind(case.name, case.issue, Justice))
new.data$case.issue = as.factor(new.data$case.issue)

k = 1; case.count = rep(0, 100)
tmp = matrix(0, nrow = 100, ncol = 9)
tmp[1,] = colnames(new.data)[!is.na(new.data[1,])][-c(1,2)]
for(i in 1:nrow(new.data)){
  new = colnames(new.data)[!is.na(new.data[i,])][-c(1,2)]
  if(sum(!(new %in% tmp[k,])) == 0 | length(new) < 9){
    case.count[k] = case.count[k] + 1
  }else{
    k = k + 1
    tmp[k,] = new
  }
}

###########
which(dat.all$caseId == "1994-001")
which(dat.all$caseId == "2004-080")


# WHRehnquist : Chief Justice
#1: "WHRehnquist" 2:"JPStevens"   3:"SDOConnor"   
# 4:"AScalia"     5:"AMKennedy"   6:"DHSouter"    
# 7:"CThomas"    8:"RBGinsburg"  9:"SGBreyer"   
which(colnames(new.data) %in% tmp[which.max(case.count),])

long.data = new.data[c(1:2, which(colnames(new.data) %in% tmp[which.max(case.count),]))]
long.data = na.omit(long.data) # 893 without any NA

write.table(long.data, file = "data/longdata.csv", sep = ",")

data = read.csv("Data/longdata.csv", sep = ",", header = TRUE)

mean(rowSums(data[,c(3:11)] == 1) >= 5) # conservative
mean(rowSums(data[,c(3:11)] == 2) >= 5) # liberal
colSums(data[,c(3:11)] == 1) / nrow(data)

table(data$case.issue)
Y = 2*data[,c(3:11)]-3
A = ifelse(data$case.issue == 1, 1, -1)
