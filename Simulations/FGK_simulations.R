#################################################
# Simulations from
# "Instrument Validity Tests with Causal Forests"
# Helmut Farbmacher et al 2020
# May 2020
#################################################

rm(list=ls())
#p.latetest <- c("grf", "rlearner", "rpart", "treeClust")
packages <- c("LATEtest","foreach", "doParallel", "mvtnorm")
invisible(lapply(packages, library, character.only = TRUE))

setwd("/Users/helmut/Documents/GitHub/LATEtest/Simulations")   # set working directory path
source("dgp_setup.R")

######################################################################################

seed=1234
set.seed(seed)

##### Setup DGP #####
#####################
setup <- "A"          # A: randomized experiment, B: easy confounding of Z and strong confounding of D
n <- 3000             # number of observations
R <- 8             # number of Monte Carlo replications

subsets <- 4
siglevel <- 0.05

##### Setup parallel computing #####
####################################
system <- Sys.info()['sysname']
if(system == 'Windows') {
  cl <- makeCluster(detectCores())
  clusterEvalQ(cl, c(library(grf),library(rlearner),library(rpart),library(treeClust),library(mvtnorm)))
} else if(system == 'Darwin') {
  cl <- makeForkCluster(detectCores())
}
registerDoParallel(cl)
showConnections()

sink(file=paste("FGK","_setup",setup,".txt",sep=""), split=TRUE)
print(Sys.time())
print("-----------------------")
print("setup, n, R, seed, subsets, siglevel")
print(paste(setup, n, R, seed, subsets, siglevel, sep=" / "))
print("--------------------------------------------")
print("--------------------------------------------")

##### Run simulation #####
##########################
for (dgp in 0:5) {         #loop over all dgps; see dgp_setup.R
  start_time = Sys.time()
  sim <- foreach(r=1:R, .combine=rbind) %dopar% {
    set.seed(r)
    data <- fct_datasim(setup=setup, dgp=dgp)

    test <- LATEtest(data=data, covars=paste0(colnames(data)[4:ncol(data)]), subsets=subsets, alpha=siglevel)
    return(c(r, test$results$Tmax,test$results$cv,test$results$reject,test$results$nu_ineq))
  }
  end_time = Sys.time()
  print(end_time - start_time)
  colnames(sim) <- c("round","Tmax0","Tmax1","Tmax2","cv0","cv1","cv2","reject0","reject1","reject2","dim0","dim1","dim2")
  sim <- as.data.frame(sim)

  ##### Display results #####
  ###########################
  print("dgp")
  print(dgp)
  options(digits=3); print(head(sim)); options(digits=4)
  print("summary(sim$Tmax0/1/2)")
  print(rbind(summary(sim$Tmax0),summary(sim$Tmax1),summary(sim$Tmax2)))
  print("summary(sim$cv0/1/2)")
  print(rbind(summary(sim$cv0),summary(sim$cv1),summary(sim$cv2)))
  print("mean(sim$reject0/1/2)")
  print(c(mean(sim$reject0),mean(sim$reject1),mean(sim$reject2)))
  print("")
  print("summary(sim$dim0/1/2)")
  print(rbind(summary(sim$dim0),summary(sim$dim1),summary(sim$dim2)))
  print("table(sim$dim0)")
  print(table(sim$dim0))
  print("table(sim$dim1)")
  print(table(sim$dim1))
  print("table(sim$dim2)")
  print(table(sim$dim2))
  save.image(file=paste("FGKtest","_setup",setup,"_dgp",dgp,".Rdata",sep=""))
  print("--------------------------------------")
}
sink(file=NULL)
stopCluster(cl)
registerDoParallel()
