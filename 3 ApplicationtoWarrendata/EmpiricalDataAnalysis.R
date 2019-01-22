#This code was written by Jolien Cremers
#More information can be found in the Readme file


#Loading and restructuring data
require(haven)
require(circular)
require(bpnreg)
require(dplyr)

Maps <- haven::read_spss(file = "3 ApplicationtoWarrendata/EmpiricalData.sav")

Maps$Error.rad <- (Maps$Error/180)*pi
Maps$L.c <- Maps$Learn-mean(Maps$Learn)
Maps$Maze <- as.factor(Maps$Maze)
Maps$Trial.type <- as.factor(Maps$Trial.type)

euclidean <- subset(Maps, Maze == 0)$Error
non.euclidean <- subset(Maps, Maze == 1)$Error

standard.euc <- subset(Maps, Maze == 0 & Trial.type == 0)$Error
standard.neuc <- subset(Maps, Maze == 1 & Trial.type == 0)$Error
probe.euc <- subset(Maps, Maze == 0 & Trial.type == 1)$Error
probe.neuc <- subset(Maps, Maze == 1 & Trial.type == 1)$Error

#Descriptives (Table 4)
mean_circ(euclidean, units = "degrees")
mean_circ(non.euclidean, units = "degrees")
mean_circ(standard.euc, units = "degrees")
mean_circ(standard.neuc, units = "degrees")
mean_circ(probe.euc, units = "degrees")
mean_circ(probe.neuc, units = "degrees")

rho_circ(euclidean, units = "degrees")
rho_circ(non.euclidean, units = "degrees")
rho_circ(standard.euc, units = "degrees")
rho_circ(standard.neuc, units = "degrees")
rho_circ(probe.euc, units = "degrees")
rho_circ(probe.neuc, units = "degrees")

eucl <- subset(Maps, Maze == 0)$Learn
neucl <- subset(Maps, Maze == 1 )$Learn

mean(eucl)
mean(neucl)
sd(eucl)
sd(neucl)

#Save data files in correct formal for analyses
Maps_p <- subset(Maps, Trial.type == 1)[, c("Error.rad", "Maze", "Learn")]
Maps_s <- subset(Maps, Trial.type == 0)[, c("Error.rad", "Maze", "Learn")]

write.table(Maps_p, file = "3 ApplicationtoWarrendata/MapsP.txt", sep = "\t")
write.table(Maps_s, file = "3 ApplicationtoWarrendata/MapsS.txt", sep = "\t")

MapsP <- read.table("3 ApplicationtoWarrendata/MapsP.txt", sep = "\t", header = TRUE)
write.table(MapsP, row.names=F, col.names=F,file="MapsP_1_80_0.0_0.0_0.0_0.0.dat") 

MapsS <- read.table("3 ApplicationtoWarrendata/MapsS.txt", sep = "\t", header = TRUE)
write.table(MapsS, row.names=F, col.names=F,file="MapsS_1_80_0.0_0.0_0.0_0.0.dat") 


Maps_pm <- cbind(subset(Maps, Trial.type == 1) %>%
                   group_by(Subject) %>%
                   summarise_at(vars(Learn), funs(mean(., na.rm=TRUE))),
                 subset(Maps, Trial.type == 1) %>%
                   group_by(Subject) %>%
                   summarise_at(vars(Maze), funs(unique)),
                 subset(Maps, Trial.type == 1) %>%
                   group_by(Subject) %>%
                   summarise_at(vars(Error.rad), funs(mean_circ)))[,c('Error.rad', 'Maze', 'Learn')]

Maps_sm <- cbind(subset(Maps, Trial.type == 0) %>%
                   group_by(Subject) %>%
                   summarise_at(vars(Learn), funs(mean(., na.rm=TRUE))),
                 subset(Maps, Trial.type == 0) %>%
                   group_by(Subject) %>%
                   summarise_at(vars(Maze), funs(unique)),
                 subset(Maps, Trial.type == 0) %>%
                   group_by(Subject) %>%
                   summarise_at(vars(Error.rad), funs(mean_circ)))[,c('Error.rad', 'Maze', 'Learn')]

write.table(Maps_pm, file = "3 ApplicationtoWarrendata/MapsPM.txt", sep = "\t")
write.table(Maps_sm, file = "3 ApplicationtoWarrendata/MapsSM.txt", sep = "\t")

MapsPM <- read.table("MapsPM.txt", sep = "\t", header = TRUE)
write.table(MapsPM, row.names=F, col.names=F,file="MapsPM_1_20_0.0_0.0_0.0_0.0.dat") 

MapsSM <- read.table("MapsSM.txt", sep = "\t", header = TRUE)
write.table(MapsSM, row.names=F, col.names=F,file="MapsSM_1_20_0.0_0.0_0.0_0.0.dat") 



#Check convergence
WN_P <- read.table("3 ApplicationtoWarrendata/Output WN/Probe/raw_output_7000_1_MapsP_1_80_0.0_0.0_0.0_0.0.dat", header=F)
names(WN_P) <- c("i","b0a","b0b","b1a","b1b","b2a","b2b","ra","rb")

plot(WN_P$b0a, type="l", col="red")
lines(WN_P$b0b,  col="blue")
plot(WN_P$b1a, type="l", col="red")
lines(WN_P$b1b,  col="blue")
plot(WN_P$b2a, type="l", col="red")
lines(WN_P$b2b,  col="blue")
plot(WN_P$ra, type="l", col="red")
lines(WN_P$rb,  col="blue")

WN_S <- read.table("3 ApplicationtoWarrendata/Output WN/Standard/raw_output_7000_1_MapsS_1_80_0.0_0.0_0.0_0.0.dat", header=F)
names(WN_S) <- c("i","b0a","b0b","b1a","b1b","b2a","b2b","ra","rb")

plot(WN_S$b0a, type="l", col="red")
lines(WN_S$b0b,  col="blue")
plot(WN_S$b1a, type="l", col="red")
lines(WN_S$b1b,  col="blue")
plot(WN_S$b2a, type="l", col="red")
lines(WN_S$b2b,  col="blue")
plot(WN_S$ra, type="l", col="red")
lines(WN_S$rb,  col="blue")

WC_P <- read.table("3 ApplicationtoWarrendata/Output WC/Probe/raw_output_7000_1_MapsP_1_80_0.0_0.0_0.0_0.0.dat", header=F)
names(WC_P) <- c("i","b0a","b0b","b1a","b1b","b2a","b2b","ra","rb")

plot(WC_P$b0a, type="l", col="red")
lines(WC_P$b0b,  col="blue")
plot(WC_P$b1a, type="l", col="red")
lines(WC_P$b1b,  col="blue")
plot(WC_P$b2a, type="l", col="red")
lines(WC_P$b2b,  col="blue")
plot(WC_P$ra, type="l", col="red")
lines(WC_P$rb,  col="blue")

WC_S <- read.table("3 ApplicationtoWarrendata/Output WC/Standard/raw_output_7000_1_MapsS_1_80_0.0_0.0_0.0_0.0.dat", header=F)
names(WC_S) <- c("i","b0a","b0b","b1a","b1b","b2a","b2b","ra","rb")

plot(WC_S$b0a, type="l", col="red")
lines(WC_S$b0b,  col="blue")
plot(WC_S$b1a, type="l", col="red")
lines(WC_S$b1b,  col="blue")
plot(WC_S$b2a, type="l", col="red")
lines(WC_S$b2b,  col="blue")
plot(WC_S$ra, type="l", col="red")
lines(WC_S$rb,  col="blue")


#Averaged
WN_PM <- read.table("3 ApplicationtoWarrendata/Output WN/Probe/Averaged/raw_output_7000_1_MapsPM_1_20_0.0_0.0_0.0_0.0.dat", header=F)
names(WN_PM) <- c("i","b0a","b0b","b1a","b1b","b2a","b2b","ra","rb")

plot(WN_PM$b0a, type="l", col="red")
lines(WN_PM$b0b,  col="blue")
plot(WN_PM$b1a, type="l", col="red")
lines(WN_PM$b1b,  col="blue")
plot(WN_PM$b2b, type="l", col="red")
lines(WN_PM$b2a,  col="blue")
plot(WN_PM$ra, type="l", col="red")
lines(WN_PM$rb,  col="blue")

WN_SM <- read.table("3 ApplicationtoWarrendata/Output WN/Standard/Averaged/raw_output_7000_1_MapsSM_1_20_0.0_0.0_0.0_0.0.dat", header=F)
names(WN_SM) <- c("i","b0a","b0b","b1a","b1b","b2a","b2b","ra","rb")

plot(WN_SM$b0a, type="l", col="red")
lines(WN_SM$b0b,  col="blue")
plot(WN_SM$b1a, type="l", col="red")
lines(WN_SM$b1b,  col="blue")
plot(WN_SM$b2b, type="l", col="red")
lines(WN_SM$b2a,  col="blue")
plot(WN_SM$ra, type="l", col="red")
lines(WN_SM$rb,  col="blue")

WC_PM <- read.table("3 ApplicationtoWarrendata/Output WC/Probe/Averaged/raw_output_7000_1_MapsPM_1_20_0.0_0.0_0.0_0.0.dat", header=F)
names(WC_PM) <- c("i","b0a","b0b","b1a","b1b","b2a","b2b","ra","rb")

plot(WC_PM$b0a, type="l", col="red")
lines(WC_PM$b0b,  col="blue")
plot(WC_PM$b1a, type="l", col="red")
lines(WC_PM$b1b,  col="blue")
plot(WC_PM$b2a, type="l", col="red")
lines(WC_PM$b2b,  col="blue")
plot(WC_PM$ra, type="l", col="red")
lines(WC_PM$rb,  col="blue")

WC_SM <- read.table("3 ApplicationtoWarrendata/Output WC/Standard/Averaged/raw_output_7000_1_MapsSM_1_20_0.0_0.0_0.0_0.0.dat", header=F)
names(WC_SM) <- c("i","b0a","b0b","b1a","b1b","b2a","b2b","ra","rb")

plot(WC_SM$b0a, type="l", col="red")
lines(WC_SM$b0b,  col="blue")
plot(WC_SM$b1a, type="l", col="red")
lines(WC_SM$b1b,  col="blue")
plot(WC_SM$b2a, type="l", col="red")
lines(WC_SM$b2b,  col="blue")
plot(WC_SM$ra, type="l", col="red")
lines(WC_SM$rb,  col="blue")





#Check autocorrelation
autocor <- read.table("3 ApplicationtoWarrendata/Output WN/Standard/autocor_4000_1_MapsS_1_80_0.0_0.0_0.0_0.0.dat", header=F)
names(autocor)<- c("lag", "b0", "b1","b2","rho")

plot(autocor$b0[1:100]~autocor$lag[1:100], col="red", type="l", ylim=c(0,20))
lines(autocor$b0[101:200]~autocor$lag[101:200], col="blue")
plot(autocor$b1[1:100]~autocor$lag[1:100], col="red", type="l", ylim=c(0,20))
lines(autocor$b1[101:200]~autocor$lag[101:200], col="blue")
plot(autocor$b2[1:100]~autocor$lag[1:100], col="red", type="l", ylim=c(0,20))
lines(autocor$b2[101:200]~autocor$lag[101:200], col="blue")
plot(autocor$rho[1:100]~autocor$lag[1:100], col="red", type="l", ylim=c(0,20))
lines(autocor$rho[101:200]~autocor$lag[101:200], col="blue")

autocor <- read.table("3 ApplicationtoWarrendata/Output WC/Standard/autocor_4000_1_MapsS_1_80_0.0_0.0_0.0_0.0.dat", header=F)
names(autocor)<- c("lag", "b0", "b1","b2","rho")

plot(autocor$b0[1:100]~autocor$lag[1:100], col="red", type="l", ylim=c(0,20))
lines(autocor$b0[101:200]~autocor$lag[101:200], col="blue")
plot(autocor$b1[1:100]~autocor$lag[1:100], col="red", type="l", ylim=c(0,20))
lines(autocor$b1[101:200]~autocor$lag[101:200], col="blue")
plot(autocor$b2[1:100]~autocor$lag[1:100], col="red", type="l", ylim=c(0,20))
lines(autocor$b2[101:200]~autocor$lag[101:200], col="blue")
plot(autocor$rho[1:100]~autocor$lag[1:100], col="red", type="l", ylim=c(0,20))
lines(autocor$rho[101:200]~autocor$lag[101:200], col="blue")

autocor <- read.table("3 ApplicationtoWarrendata/Output WN/Probe/autocor_4000_1_MapsP_1_80_0.0_0.0_0.0_0.0.dat", header=F)
names(autocor)<- c("lag", "b0", "b1","b2","rho")

plot(autocor$b0[1:100]~autocor$lag[1:100], col="red", type="l", ylim=c(0,20))
lines(autocor$b0[101:200]~autocor$lag[101:200], col="blue")
plot(autocor$b1[1:100]~autocor$lag[1:100], col="red", type="l", ylim=c(0,20))
lines(autocor$b1[101:200]~autocor$lag[101:200], col="blue")
plot(autocor$b2[1:100]~autocor$lag[1:100], col="red", type="l", ylim=c(0,20))
lines(autocor$b2[101:200]~autocor$lag[101:200], col="blue")
plot(autocor$rho[1:100]~autocor$lag[1:100], col="red", type="l", ylim=c(0,20))
lines(autocor$rho[101:200]~autocor$lag[101:200], col="blue")

autocor <- read.table("3 ApplicationtoWarrendata/Output WC/Probe/autocor_4000_1_MapsP_1_80_0.0_0.0_0.0_0.0.dat", header=F)
names(autocor)<- c("lag", "b0", "b1","b2","rho")

plot(autocor$b0[1:100]~autocor$lag[1:100], col="red", type="l", ylim=c(0,20))
lines(autocor$b0[101:200]~autocor$lag[101:200], col="blue")
plot(autocor$b1[1:100]~autocor$lag[1:100], col="red", type="l", ylim=c(0,20))
lines(autocor$b1[101:200]~autocor$lag[101:200], col="blue")
plot(autocor$b2[1:100]~autocor$lag[1:100], col="red", type="l", ylim=c(0,20))
lines(autocor$b2[101:200]~autocor$lag[101:200], col="blue")
plot(autocor$rho[1:100]~autocor$lag[1:100], col="red", type="l", ylim=c(0,20))
lines(autocor$rho[101:200]~autocor$lag[101:200], col="blue")

#Averaged

autocor <- read.table("3 ApplicationtoWarrendata/Output WN/Standard/Averaged/autocor_4000_1_MapsSM_1_20_0.0_0.0_0.0_0.0.dat", header=F)
names(autocor)<- c("lag", "b0", "b1","b2","rho")

plot(autocor$b0[1:100]~autocor$lag[1:100], col="red", type="l", ylim=c(0,20))
lines(autocor$b0[101:200]~autocor$lag[101:200], col="blue")
plot(autocor$b1[1:100]~autocor$lag[1:100], col="red", type="l", ylim=c(0,20))
lines(autocor$b1[101:200]~autocor$lag[101:200], col="blue")
plot(autocor$b2[1:100]~autocor$lag[1:100], col="red", type="l", ylim=c(0,20))
lines(autocor$b2[101:200]~autocor$lag[101:200], col="blue")
plot(autocor$rho[1:100]~autocor$lag[1:100], col="red", type="l", ylim=c(0,20))
lines(autocor$rho[101:200]~autocor$lag[101:200], col="blue")

autocor <- read.table("3 ApplicationtoWarrendata/Output WC/Standard/Averaged/autocor_4000_1_MapsSM_1_20_0.0_0.0_0.0_0.0.dat", header=F)
names(autocor)<- c("lag", "b0", "b1","b2","rho")

plot(autocor$b0[1:100]~autocor$lag[1:100], col="red", type="l", ylim=c(0,20))
lines(autocor$b0[101:200]~autocor$lag[101:200], col="blue")
plot(autocor$b1[1:100]~autocor$lag[1:100], col="red", type="l", ylim=c(0,20))
lines(autocor$b1[101:200]~autocor$lag[101:200], col="blue")
plot(autocor$b2[1:100]~autocor$lag[1:100], col="red", type="l", ylim=c(0,20))
lines(autocor$b2[101:200]~autocor$lag[101:200], col="blue")
plot(autocor$rho[1:100]~autocor$lag[1:100], col="red", type="l", ylim=c(0,20))
lines(autocor$rho[101:200]~autocor$lag[101:200], col="blue")

autocor <- read.table("3 ApplicationtoWarrendata/Output WN/Probe/Averaged/autocor_4000_1_MapsPM_1_20_0.0_0.0_0.0_0.0.dat", header=F)
names(autocor)<- c("lag", "b0", "b1","b2","rho")

plot(autocor$b0[1:100]~autocor$lag[1:100], col="red", type="l", ylim=c(0,20))
lines(autocor$b0[101:200]~autocor$lag[101:200], col="blue")
plot(autocor$b1[1:100]~autocor$lag[1:100], col="red", type="l", ylim=c(0,20))
lines(autocor$b1[101:200]~autocor$lag[101:200], col="blue")
plot(autocor$b2[1:100]~autocor$lag[1:100], col="red", type="l", ylim=c(0,20))
lines(autocor$b2[101:200]~autocor$lag[101:200], col="blue")
plot(autocor$rho[1:100]~autocor$lag[1:100], col="red", type="l", ylim=c(0,20))
lines(autocor$rho[101:200]~autocor$lag[101:200], col="blue")

autocor <- read.table("3 ApplicationtoWarrendata/Output WC/Probe/Averaged/autocor_4000_1_MapsPM_1_20_0.0_0.0_0.0_0.0.dat", header=F)
names(autocor)<- c("lag", "b0", "b1","b2","rho")

plot(autocor$b0[1:100]~autocor$lag[1:100], col="red", type="l", ylim=c(0,20))
lines(autocor$b0[101:200]~autocor$lag[101:200], col="blue")
plot(autocor$b1[1:100]~autocor$lag[1:100], col="red", type="l", ylim=c(0,20))
lines(autocor$b1[101:200]~autocor$lag[101:200], col="blue")
plot(autocor$b2[1:100]~autocor$lag[1:100], col="red", type="l", ylim=c(0,20))
lines(autocor$b2[101:200]~autocor$lag[101:200], col="blue")
plot(autocor$rho[1:100]~autocor$lag[1:100], col="red", type="l", ylim=c(0,20))
lines(autocor$rho[101:200]~autocor$lag[101:200], col="blue")

#Results
#We want to report the output in degrees, rather than radians. Function to do this:
output <- function(results){
  print(mean(results))
  print(sd(results))
  print(quantile(results,c(0.025, 0.975)))
}

output(circular(c(WN_P[,2][3001:7000]*360/(2*pi),WN_P[,3][3001:7000]*360/(2*pi)),unit="degrees"))
output(circular(c(WN_P[,4][3001:7000]*360/(2*pi),WN_P[,5][3001:7000]*360/(2*pi)),unit="degrees"))
output(circular(c(WN_P[,6][3001:7000]*360/(2*pi),WN_P[,7][3001:7000]*360/(2*pi)),unit="degrees"))
mode_est(c(WN_P[,8][3001:7000],WN_P[,9][3001:7000]))
hpd_est(c(WN_P[,8][3001:7000],WN_P[,9][3001:7000]))

output(circular(c(WC_P[,2][3001:7000]*360/(2*pi),WC_P[,3][3001:7000]*360/(2*pi)),unit="degrees"))
output(circular(c(WC_P[,4][3001:7000]*360/(2*pi),WC_P[,5][3001:7000]*360/(2*pi)),unit="degrees"))
output(circular(c(WC_P[,6][3001:7000]*360/(2*pi),WC_P[,7][3001:7000]*360/(2*pi)),unit="degrees"))
mode_est(c(WC_P[,8][3001:7000],WC_P[,9][3001:7000]))
hpd_est(c(WC_P[,8][3001:7000],WC_P[,9][3001:7000]))

output(circular(c(WN_S[,2][3001:7000]*360/(2*pi),WN_S[,3][3001:7000]*360/(2*pi)),unit="degrees"))
output(circular(c(WN_S[,4][3001:7000]*360/(2*pi),WN_S[,5][3001:7000]*360/(2*pi)),unit="degrees"))
output(circular(c(WN_S[,6][3001:7000]*360/(2*pi),WN_S[,7][3001:7000]*360/(2*pi)),unit="degrees"))
mode_est(c(WN_S[,8][3001:7000],WN_S[,9][3001:7000]))
hpd_est(c(WN_S[,8][3001:7000],WN_S[,9][3001:7000]))

output(circular(c(WC_S[,2][3001:7000]*360/(2*pi),WC_S[,3][3001:7000]*360/(2*pi)),unit="degrees"))
output(circular(c(WC_S[,4][3001:7000]*360/(2*pi),WC_S[,5][3001:7000]*360/(2*pi)),unit="degrees"))
output(circular(c(WC_S[,6][3001:7000]*360/(2*pi),WC_S[,7][3001:7000]*360/(2*pi)),unit="degrees"))
mode_est(c(WC_S[,8][3001:7000],WC_S[,9][3001:7000]))
hpd_est(c(WC_S[,8][3001:7000],WC_S[,9][3001:7000]))


#Averaged (Tables 5 and 6)


output(circular(c(WN_PM[,2][3001:7000]*360/(2*pi),WN_PM[,3][3001:7000]*360/(2*pi)),unit="degrees"))
output(circular(c(WN_PM[,4][3001:7000]*360/(2*pi),WN_PM[,5][3001:7000]*360/(2*pi)),unit="degrees"))
output(circular(c(WN_PM[,6][3001:7000]*360/(2*pi),WN_PM[,7][3001:7000]*360/(2*pi)),unit="degrees"))
mode_est(c(WN_PM[,8][3001:7000],WN_PM[,9][3001:7000]))
hpd_est(c(WN_PM[,8][3001:7000],WN_PM[,9][3001:7000]))

output(circular(c(WC_PM[,2][3001:7000]*360/(2*pi),WC_PM[,3][3001:7000]*360/(2*pi)),unit="degrees"))
output(circular(c(WC_PM[,4][3001:7000]*360/(2*pi),WC_PM[,5][3001:7000]*360/(2*pi)),unit="degrees"))
output(circular(c(WC_PM[,6][3001:7000]*360/(2*pi),WC_PM[,7][3001:7000]*360/(2*pi)),unit="degrees"))
mode_est(c(WC_PM[,8][3001:7000],WC_PM[,9][3001:7000]))
hpd_est(c(WC_PM[,8][3001:7000],WC_PM[,9][3001:7000]))

output(circular(c(WN_SM[,2][3001:7000]*360/(2*pi),WN_SM[,3][3001:7000]*360/(2*pi)),unit="degrees"))
output(circular(c(WN_SM[,4][3001:7000]*360/(2*pi),WN_SM[,5][3001:7000]*360/(2*pi)),unit="degrees"))
output(circular(c(WN_SM[,6][3001:7000]*360/(2*pi),WN_SM[,7][3001:7000]*360/(2*pi)),unit="degrees"))
mode_est(c(WN_SM[,8][3001:7000],WN_SM[,9][3001:7000]))
hpd_est(c(WN_SM[,8][3001:7000],WN_SM[,9][3001:7000]))

output(circular(c(WC_SM[,2][3001:7000]*360/(2*pi),WC_SM[,3][3001:7000]*360/(2*pi)),unit="degrees"))
output(circular(c(WC_SM[,4][3001:7000]*360/(2*pi),WC_SM[,5][3001:7000]*360/(2*pi)),unit="degrees"))
output(circular(c(WC_SM[,6][3001:7000]*360/(2*pi),WC_SM[,7][3001:7000]*360/(2*pi)),unit="degrees"))
mode_est(c(WC_SM[,8][3001:7000],WC_SM[,9][3001:7000]))
hpd_est(c(WC_SM[,8][3001:7000],WC_SM[,9][3001:7000]))
