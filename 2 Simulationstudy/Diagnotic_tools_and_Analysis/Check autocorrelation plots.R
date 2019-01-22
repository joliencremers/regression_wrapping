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
