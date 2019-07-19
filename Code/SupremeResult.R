library(xtable)
library(gtools)
source("Code/printr.R")
Rcpp::sourceCpp("Code/nine_hiton.cpp")
## auxiliary functions
## auxiliary functions
findU = function(x) quantile(x,probs=0.975, na.rm = TRUE)
findL = function(x) quantile(x,probs=0.025, na.rm = TRUE)

load("Data/supreme_hiton_boot1.RData")
#load("Data/supreme_hiton_boot2.RData")
#load("Data/supreme_hiton_boot8.RData")
#load("Data/supreme_hiton_boot9.RData")

load("Data/supreme_hiton_fit1.RData")
#load("Data/supreme_hiton_fit2.RData")
#load("Data/supreme_hiton_fit8.RData")
#load("Data/supreme_hiton_fit9.RData")
## (22, 24)
hiton1 = hiton2 = hiton8 = hiton9 = matrix(0, 1000, 36)
for(i in 1:1000){
  hiton1[i,] = supreme_hiton_boot1[[i]][-1]
  hiton2[i,] = supreme_hiton_boot2[[i]][-1]
  hiton8[i,] = supreme_hiton_boot8[[i]][-1]
  hiton9[i,] = supreme_hiton_boot9[[i]][-1]
}

## main effect
main1 = supreme_hiton_fit1[-1]; main2 = supreme_hiton_fit2[-1]
main8 = supreme_hiton_fit8[-1]; main9 = supreme_hiton_fit9[-1]
mat = matrix(0, nrow = 15, ncol = 3)
mat[1,] = c( "WHRehnquist", "JPStevens", "SDOConnor")
mat[6,] = c( "AScalia", "AMKennedy", "DHSouter") 
mat[11,] = c( "CThomas", "RBGinsburg", "SGBreyer")
mat[2,] = c(paste(formatC(main1[1],2, format = "f"), " [", formatC(findL(hiton1[,1]), 2, format = "f"), " ,", formatC(findU(hiton1[,1]), 2, format = "f"),"]", sep=""),
            paste(formatC(main1[2],2, format = "f"), " [", formatC(findL(hiton1[,2]), 2, format = "f"), " ,", formatC(findU(hiton1[,2]), 2, format = "f"),"]", sep=""),
            paste(formatC(main1[3],2, format = "f"), " [", formatC(findL(hiton1[,3]), 2, format = "f"), " ,", formatC(findU(hiton1[,3]), 2, format = "f"),"]", sep=""))
mat[3,] = c(paste(formatC(main2[1],2, format = "f"), " [", formatC(findL(hiton2[,1]), 2, format = "f"), " ,", formatC(findU(hiton2[,1]), 2, format = "f"),"]", sep=""),
            paste(formatC(main2[2],2, format = "f"), " [", formatC(findL(hiton2[,2]), 2, format = "f"), " ,", formatC(findU(hiton2[,2]), 2, format = "f"),"]", sep=""),
            paste(formatC(main2[3],2, format = "f"), " [", formatC(findL(hiton2[,3]), 2, format = "f"), " ,", formatC(findU(hiton2[,3]), 2, format = "f"),"]", sep=""))
mat[4,] = c(paste(formatC(main8[1],2, format = "f"), " [", formatC(findL(hiton8[,1]), 2, format = "f"), " ,", formatC(findU(hiton8[,1]), 2, format = "f"),"]", sep=""),
            paste(formatC(main8[2],2, format = "f"), " [", formatC(findL(hiton8[,2]), 2, format = "f"), " ,", formatC(findU(hiton8[,2]), 2, format = "f"),"]", sep=""),
            paste(formatC(main8[3],2, format = "f"), " [", formatC(findL(hiton8[,3]), 2, format = "f"), " ,", formatC(findU(hiton8[,3]), 2, format = "f"),"]", sep=""))
mat[5,] = c(paste(formatC(main9[1],2, format = "f"), " [", formatC(findL(hiton9[,1]), 2, format = "f"), " ,", formatC(findU(hiton9[,1]), 2, format = "f"),"]", sep=""),
            paste(formatC(main9[2],2, format = "f"), " [", formatC(findL(hiton9[,2]), 2, format = "f"), " ,", formatC(findU(hiton9[,2]), 2, format = "f"),"]", sep=""),
            paste(formatC(main9[3],2, format = "f"), " [", formatC(findL(hiton9[,3]), 2, format = "f"), " ,", formatC(findU(hiton9[,3]), 2, format = "f"),"]", sep=""))

mat[7,] = c(paste(formatC(main1[4],2, format = "f"), " [", formatC(findL(hiton1[,4]), 2, format = "f"), " ,", formatC(findU(hiton1[,4]), 2, format = "f"),"]", sep=""),
            paste(formatC(main1[5],2, format = "f"), " [", formatC(findL(hiton1[,5]), 2, format = "f"), " ,", formatC(findU(hiton1[,5]), 2, format = "f"),"]", sep=""),
            paste(formatC(main1[6],2, format = "f"), " [", formatC(findL(hiton1[,6]), 2, format = "f"), " ,", formatC(findU(hiton1[,6]), 2, format = "f"),"]", sep=""))
mat[8,] = c(paste(formatC(main2[4],2, format = "f"), " [", formatC(findL(hiton2[,4]), 2, format = "f"), " ,", formatC(findU(hiton2[,4]), 2, format = "f"),"]", sep=""),
            paste(formatC(main2[5],2, format = "f"), " [", formatC(findL(hiton2[,5]), 2, format = "f"), " ,", formatC(findU(hiton2[,5]), 2, format = "f"),"]", sep=""),
            paste(formatC(main2[6],2, format = "f"), " [", formatC(findL(hiton2[,6]), 2, format = "f"), " ,", formatC(findU(hiton2[,6]), 2, format = "f"),"]", sep=""))
mat[9,] = c(paste(formatC(main8[4],2, format = "f"), " [", formatC(findL(hiton8[,4]), 2, format = "f"), " ,", formatC(findU(hiton8[,4]), 2, format = "f"),"]", sep=""),
            paste(formatC(main8[5],2, format = "f"), " [", formatC(findL(hiton8[,5]), 2, format = "f"), " ,", formatC(findU(hiton8[,5]), 2, format = "f"),"]", sep=""),
            paste(formatC(main8[6],2, format = "f"), " [", formatC(findL(hiton8[,6]), 2, format = "f"), " ,", formatC(findU(hiton8[,6]), 2, format = "f"),"]", sep=""))
mat[10,] = c(paste(formatC(main9[4],2, format = "f"), " [", formatC(findL(hiton9[,4]), 2, format = "f"), " ,", formatC(findU(hiton9[,4]), 2, format = "f"),"]", sep=""),
             paste(formatC(main9[5],2, format = "f"), " [", formatC(findL(hiton9[,5]), 2, format = "f"), " ,", formatC(findU(hiton9[,5]), 2, format = "f"),"]", sep=""),
             paste(formatC(main9[6],2, format = "f"), " [", formatC(findL(hiton9[,6]), 2, format = "f"), " ,", formatC(findU(hiton9[,6]), 2, format = "f"),"]", sep=""))


mat[12,] = c(paste(formatC(main1[7],2, format = "f"), " [", formatC(findL(hiton1[,7]), 2, format = "f"), " ,", formatC(findU(hiton1[,7]), 2, format = "f"),"]", sep=""),
             paste(formatC(main1[8],2, format = "f"), " [", formatC(findL(hiton1[,8]), 2, format = "f"), " ,", formatC(findU(hiton1[,8]), 2, format = "f"),"]", sep=""),
             paste(formatC(main1[9],2, format = "f"), " [", formatC(findL(hiton1[,9]), 2, format = "f"), " ,", formatC(findU(hiton1[,9]), 2, format = "f"),"]", sep=""))
mat[13,] = c(paste(formatC(main2[7],2, format = "f"), " [", formatC(findL(hiton2[,7]), 2, format = "f"), " ,", formatC(findU(hiton2[,7]), 2, format = "f"),"]", sep=""),
             paste(formatC(main2[8],2, format = "f"), " [", formatC(findL(hiton2[,8]), 2, format = "f"), " ,", formatC(findU(hiton2[,8]), 2, format = "f"),"]", sep=""),
             paste(formatC(main2[9],2, format = "f"), " [", formatC(findL(hiton2[,9]), 2, format = "f"), " ,", formatC(findU(hiton2[,9]), 2, format = "f"),"]", sep=""))
mat[14,] = c(paste(formatC(main8[7],2, format = "f"), " [", formatC(findL(hiton8[,7]), 2, format = "f"), " ,", formatC(findU(hiton8[,7]), 2, format = "f"),"]", sep=""),
             paste(formatC(main8[8],2, format = "f"), " [", formatC(findL(hiton8[,8]), 2, format = "f"), " ,", formatC(findU(hiton8[,8]), 2, format = "f"),"]", sep=""),
             paste(formatC(main8[9],2, format = "f"), " [", formatC(findL(hiton8[,9]), 2, format = "f"), " ,", formatC(findU(hiton8[,9]), 2, format = "f"),"]", sep=""))
mat[15,] = c(paste(formatC(main9[7],2, format = "f"), " [", formatC(findL(hiton9[,7]), 2, format = "f"), " ,", formatC(findU(hiton9[,7]), 2, format = "f"),"]", sep=""),
             paste(formatC(main9[8],2, format = "f"), " [", formatC(findL(hiton9[,8]), 2, format = "f"), " ,", formatC(findU(hiton9[,8]), 2, format = "f"),"]", sep=""),
             paste(formatC(main9[9],2, format = "f"), " [", formatC(findL(hiton9[,9]), 2, format = "f"), " ,", formatC(findU(hiton9[,9]), 2, format = "f"),"]", sep=""))

print(xtable(mat))

####### 1. Criminal Procedure
load("Data/supreme_hiton_fit1.RData")
main = supreme_hiton_fit1
load("Data/supreme_hiton_boot1.RData")
boot.sample = matrix(0, nrow = length(supreme_hiton_boot1), ncol = 37)
for(i in 1:length(supreme_hiton_boot1)){
  boot.sample[i,] = supreme_hiton_boot1[[i]]
}

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

### summarize three-way interaction
threes = cliques(graph.adjacency(A4, "undirected"), min = 3, max = 3)
three.sets = c("Y6:Y8:Y9", "Y5:Y6:Y8", "Y1:Y5:Y7", "Y1:Y3:Y5",
               "Y3:Y6:Y9", "Y3:Y5:Y6", "Y1:Y4:Y7", "Y4:Y5:Y7",
               "Y1:Y4:Y5", "Y2:Y6:Y8", "Y2:Y8:Y9", "Y2:Y6:Y9")
load("Data/supreme_hiton_boot1_three.Rdata")
load("Data/supreme_hiton_boot2_three.Rdata")
load("Data/supreme_hiton_boot8_three.Rdata")
load("Data/supreme_hiton_boot9_three.Rdata")
three1 = three2 = three8 = three9 = matrix(0, 1000, 12);
colnames(three1) = colnames(three2) = colnames(three8) = colnames(three9) = three.sets
for(i in 1:1000){
  three1[i,] = supreme_hiton_boot1_three[[i]]
  three2[i,] = supreme_hiton_boot2_three[[i]]
  three8[i,] = supreme_hiton_boot8_three[[i]]
  three9[i,] = supreme_hiton_boot9_three[[i]]
}
three1 = na.omit(three1); three2 = na.omit(three2)
three8 = na.omit(three8); three9 = na.omit(three9)
three_mat = matrix(0, 4, 12)
colnames(three_mat) = three.sets
rownames(three_mat) = c("Criminal procedure", "Civil rights",
                        "Economic activity", "Judicial power")
for(i in 1:12){
  three_mat[1,i] = paste("[",formatC(findL(three1[,i]), 2, format = "f"), ", ", formatC(findU(three1[,i]), 2, format = "f"), "]", sep="") 
  three_mat[2,i] = paste("[",formatC(findL(three2[,i]), 2, format = "f"), ", ", formatC(findU(three2[,i]), 2, format = "f"), "]", sep="") 
  three_mat[3,i] = paste("[",formatC(findL(three8[,i]), 2, format = "f"), ", ", formatC(findU(three8[,i]), 2, format = "f"), "]", sep="") 
  three_mat[4,i] = paste("[",formatC(findL(three9[,i]), 2, format = "f"), ", ", formatC(findU(three9[,i]), 2, format = "f"), "]", sep="") 
}
print(xtable(three_mat))
