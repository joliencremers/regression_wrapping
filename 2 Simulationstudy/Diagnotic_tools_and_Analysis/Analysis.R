#Information on how to obtain the Analysis files can be found in the Readme file
#This code was written by Inge Jansen

#Rcode to check the convergence of the first 100 samples in a regression analysis

#First read in the data. Put in the right filename below:
raw_data <- read.table("")

#Change the names of the columns. Adjust if there's only one predictor. 
names(raw_data) <- c("i","b0a","b0b","b1a","b1b","b2a", "b2b","ra","rb")

#This for loop will give the traceplot of b0 for the two chains (b0a in red and b0b in blue).
#Change chain name to get the trace plots of other parameters.
#Note that evaluation can take a while. 
for(i in 0:99){
  plot(raw_data$b0a[which(raw$i==i)], col="red", type="l", main=as.character(i))
  lines(raw_data$b0b[which(raw$i==i)], col="blue")
}

#Rcode to check the autocorrelation of the first 100 samples in a regression analysis

#First read in the data. Put in the right filename below:
autocors <- read.table("", header=F)

#Change the names of the columns. Adjust if there's only one predictor. 
names(autocors)<- c("lag", "b0", "b1","b2","rho")

#This for loop will give the autocorrelation plot of b2 (continuous predictor) for the two chains (b2a in red and b2b in blue).
#Change chain name to get the trace plots of other parameters.
#Note that evaluation can take a while. 
for(i in 0:99){
  plot(autocors$b2[((1:100)+(2*i*100))], col="red", type="l", ylim=c(0,1))
  lines(autocors$b2[(1:100)+((2*i+1)*100)], col="blue")
}



############################################################################################################
#################################Ancova output: Table 1#####################################################
############################################################################################################

#Read data:
WN     <- read.table("2 Simulationstudy/Simulation_output/Analysis_AncovaWN.dat")
WC     <- read.table("2 Simulationstudy/Simulation_output/Analysis_AncovaWC.dat")


names(WN) <- c("b0_true","b1_true","b2_true","rho_true","b_b0","b_b1","b_b2","b_rho","rb_b0","rb_b1","rb_b2","rb_rho","sd_b0","sd_b1","sd_b2","cp_b0","cp_b1","cp_b2","cp_rho","iw_b0","iw_b1","iw_b2","iw_rho","k")     
names(WC) <- names(WN)


#adjust for circularity
WN$b_b0 <- WN$b_b0 %% (2*pi)
WN$b_b0[WN$b_b0>pi] <- 2*pi - WN$b_b0[WN$b_b0>pi]
WN$rb_b0 <- WN$b_b0/WN$b0_true
WN$b_b1 <- WN$b_b1 %% (2*pi)
WN$b_b1[WN$b_b1>pi] <- 2*pi - WN$b_b1[WN$b_b1>pi]
WN$rb_b1 <- WN$b_b1/WN$b1_true
#
WC$b_b0 <- WC$b_b0 %% (2*pi)
WC$b_b0[WC$b_b0>pi] <- 2*pi - WC$b_b0[WC$b_b0>pi]
WC$rb_b0 <- WC$b_b0/WC$b0_true
WC$b_b1 <- WC$b_b1 %% (2*pi)
WC$b_b1[WC$b_b1>pi] <- 2*pi - WC$b_b1[WC$b_b1>pi]
WC$rb_b1 <- WC$b_b1/WC$b1_true


#Same for interval widths:
WN$iw_b0 <- WN$iw_b0 %% (2*pi)
WN$iw_b1 <- WN$iw_b1 %% (2*pi)
WC$iw_b0 <- WC$iw_b0 %% (2*pi)
WC$iw_b1 <- WC$iw_b1 %% (2*pi)

#The columns <parametername>_true contain the simulation values
#The columns b_<parametername> contain the absolute biases (reported for b2=0)
#The columns rb_<parametername> contain the relative biases
#The columns iw_<parametername> contain the interval widths
#The columns cp_<parametername> contain the coverage probabilities
#The convergence scores can be determined by running "Check trace plots.R"

################################################################################################################
#########################################LP output: Table 2#####################################################
################################################################################################################

#Read data:
WN     <- read.table("2 Simulationstudy/Simulation_output/Analysis_LPWN.dat")
WC     <- read.table("2 Simulationstudy/Simulation_output/Analysis_LPWC.dat")


names(WN) <- c("b0_true","b1_true","b2_true","rho_true","b_b0","b_b1","b_b2","b_rho","rb_b0","rb_b1","rb_b2","rb_rho","sd_b0","sd_b1","sd_b2","cp_b0","cp_b1","cp_b2","cp_rho","iw_b0","iw_b1","iw_b2","iw_rho","k")     
names(WC) <- names(WN)


#adjust for circularity
WN$b_b0 <- WN$b_b0 %% (2*pi)
WN$b_b0[WN$b_b0>pi] <- 2*pi - WN$b_b0[WN$b_b0>pi]
WN$rb_b0 <- WN$b_b0/WN$b0_true
#
WC$b_b0 <- WC$b_b0 %% (2*pi)
WC$b_b0[WC$b_b0>pi] <- 2*pi - WC$b_b0[WC$b_b0>pi]
WC$rb_b0 <- WC$b_b0/WC$b0_true


#Same for interval widths:
WN$iw_b0 <- WN$iw_b0 %% (2*pi)
WC$iw_b0 <- WC$iw_b0 %% (2*pi)

#The columns <parametername>_true contain the simulation values
#The columns b_<parametername> contain the absolute biases (reported for b2=0)
#The columns rb_<parametername> contain the relative biases
#The columns iw_<parametername> contain the interval widths
#The columns cp_<parametername> contain the coverage probabilities
#The convergence scores can be determined by running "Check trace plots.R"

################################################################################################################
#########################################Wrong Error output: Table 3############################################
################################################################################################################

#Read data:
WN     <- read.table("2 Simulationstudy/Simulation_output/Analysis_NWN.dat")
WC     <- read.table("2 Simulationstudy/Simulation_output/Analysis_NWC.dat")


names(WN) <- c("b0_true","b1_true","b2_true","rho_true","b_b0","b_b1","b_b2","b_rho","rb_b0","rb_b1","rb_b2","rb_rho","sd_b0","sd_b1","sd_b2","cp_b0","cp_b1","cp_b2","cp_rho","iw_b0","iw_b1","iw_b2","iw_rho","k")     
names(WC) <- names(WN)


#adjust for circularity
WN$b_b0 <- WN$b_b0 %% (2*pi)
WN$b_b0[WN$b_b0>pi] <- 2*pi - WN$b_b0[WN$b_b0>pi]
WN$rb_b0 <- WN$b_b0/WN$b0_true
WN$b_b1[WN$b1_true==3] <- WN$b_b1[WN$b1_true==3] %% (2*pi)
WN$b_b1[WN$b1_true==3][WN$b_b1[WN$b1_true==3]>pi] <- 2*pi - WN$b_b1[WN$b_b1[WN$b1_true==3]>pi]
WN$rb_b1 <- WN$b_b1/WN$b1_true
#
WC$b_b0 <- WC$b_b0 %% (2*pi)
WC$b_b0[WC$b_b0>pi] <- 2*pi - WC$b_b0[WC$b_b0>pi]
WC$rb_b0 <- WC$b_b0/WC$b0_true
WC$b_b1[WC$b1_true==3] <- WC$b_b1[WC$b1_true==3] %% (2*pi)
WC$b_b1[WC$b_b1[WC$b1_true==3]>pi] <- 2*pi - WC$b_b1[WC$b_b1[WC$b1_true==3]>pi]
WC$rb_b1 <- WC$b_b1/WC$b1_true


#Same for interval widths:
WN$iw_b0 <- WN$iw_b0 %% (2*pi)
WN$iw_b1[WN$b1_true==3] <- WN$iw_b1[WN$b1_true==3] %% (2*pi)
WC$iw_b0 <- WC$iw_b0 %% (2*pi)
WC$iw_b1[WC$b1_true==3] <- WC$iw_b1[WC$b1_true==3] %% (2*pi)



#The columns <parametername>_true contain the simulation values
#The columns b_<parametername> contain the absolute biases (reported for b2=0)
#The columns rb_<parametername> contain the relative biases
#The columns iw_<parametername> contain the interval widths
#The columns cp_<parametername> contain the coverage probabilities
#The convergence scores can be determined by running "Check trace plots.R"


################################################################################################################
#########################################N output: Figure 6#####################################################
################################################################################################################
#Read data:
WN     <- read.table("2 Simulationstudy/Simulation_output/Analysis_ErrorsWN.dat")
WC     <- read.table("2 Simulationstudy/Simulation_output/Analysis_ErrorsWC.dat")


names(WN) <- c("b0_true","b1_true","b2_true","rho_true","b_b0","b_b1","b_b2","b_rho","rb_b0","rb_b1","rb_b2","rb_rho","sd_b0","sd_b1","sd_b2","cp_b0","cp_b1","cp_b2","cp_rho","iw_b0","iw_b1","iw_b2","iw_rho","k")     
names(WC) <- names(WN)


#adjust for circularity
WN$b_b0 <- WN$b_b0 %% (2*pi)
WN$b_b0[WN$b_b0>pi] <- 2*pi - WN$b_b0[WN$b_b0>pi]
WN$rb_b0 <- WN$b_b0/WN$b0_true
WN$b_b1[WN$b1_true==3] <- WN$b_b1[WN$b1_true==3] %% (2*pi)
WN$b_b1[WN$b1_true==3][WN$b_b1[WN$b1_true==3]>pi] <- 2*pi - WN$b_b1[WN$b_b1[WN$b1_true==3]>pi]
WN$rb_b1 <- WN$b_b1/WN$b1_true
#
WC$b_b0 <- WC$b_b0 %% (2*pi)
WC$b_b0[WC$b_b0>pi] <- 2*pi - WC$b_b0[WC$b_b0>pi]
WC$rb_b0 <- WC$b_b0/WC$b0_true
WC$b_b1[WC$b1_true==3] <- WC$b_b1[WC$b1_true==3] %% (2*pi)
WC$b_b1[WC$b_b1[WC$b1_true==3]>pi] <- 2*pi - WC$b_b1[WC$b_b1[WC$b1_true==3]>pi]
WC$rb_b1 <- WC$b_b1/WC$b1_true


#Same for interval widths:
WN$iw_b0 <- WN$iw_b0 %% (2*pi)
WN$iw_b1[WN$b1_true==3] <- WN$iw_b1[WN$b1_true==3] %% (2*pi)
WC$iw_b0 <- WC$iw_b0 %% (2*pi)
WC$iw_b1[WC$b1_true==3] <- WC$iw_b1[WC$b1_true==3] %% (2*pi)


#First half of the table are Ancova results (b1_true=3), second half is LP results
#Results are ordered by sample size (e.g. first row of Ancova output N=10, etc.)
#The reported standard deviations can be readed from the column named sd_b2
#The reported bias can be obtained by determining the maximum values of the rb_<parametername> columns


###############################################################################################################
###########################################Multicollinearity: Figure 7#########################################
###############################################################################################################

#The data for Figure 7 can be directly read from the separate analysis files. Available in: 2Simulationstudy/Simulation_output/Multicollinearity_analysis.zip

###############################################################################################################
###########################################Group size: Figure 8################################################
###############################################################################################################

#The data for Figure 7 can be directly read from the separate analysis files. Available in: 2Simulationstudy/Simulation_output/Group_size_analysis.zip
