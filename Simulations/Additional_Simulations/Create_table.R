

setwd("C:/Users/svenk/University/Github/LATEtest/Simulations/Additional_Simulations")

library(kableExtra)
library(knitr)

n <- 5000
setup  <- "B"

possible_subsets <- c(2,4,6)
possible_dgps <- 0:6

data_table <- data.frame("2" = double(),"4" = double(),"6" = double())
for (dgp in possible_dgps){
  p_mat <- matrix(NA,3,3)
  for (subsets in possible_subsets){
    try(remove(sim),silent = T)
    try(load(file=paste0("FGK","_setup",setup,"_n",n,"_subsets",subsets,"_dgp",dgp,".Rdata")),silent = T)
    try(p_mat[,match(subsets,possible_subsets)] <- c(mean(sim$reject_BHolm_2),
                                                     mean(sim$reject_BHoch_2),
                                                     mean(sim$reject_BY_2)),silent = T)
  }
  data_table <- rbind(data_table,p_mat)
}
colnames(data_table) <- c("2","4","6")
rownames(data_table) <- rep(c("Bonferroni","Bejamini-Hochberg","Benjamini-Yekutieli"),dim(data_table)[1]/3)

kable(data_table ,"latex", booktabs = T,align = "c") %>%
  kable_styling() %>%
  pack_rows("DGP0 (exog but uninformative)",1,3) %>%
  pack_rows("DGP1 (exog and relevant)",4,6) %>%
  pack_rows("DGP2 (local defiers)",7,9) %>%
  pack_rows("DGP3 (local direct effects)",10,12) %>%
  pack_rows("DGP4 (global direct effects)",13,15) %>%
  pack_rows("DGP5 (global direct effects, varying signs)",16,18) %>%
  pack_rows("DGP6 (local direct effects, but different subset)",19,21) %>%
  footnote(general = paste0("Based on 500 replications for significance level of 0.05 and n=",n,"."))
