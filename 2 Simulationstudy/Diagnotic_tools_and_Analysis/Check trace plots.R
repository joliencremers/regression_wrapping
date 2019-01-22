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