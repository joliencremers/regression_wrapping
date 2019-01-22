#This code was written by Inge Jansen

##########################################################################################
####This R code was used to generate the simulated data used in the simulation studies####
##########################################################################################

#Required packages
require(circular)
require(MASS)



##################################################################################
#######################Data generating function###################################
##################################################################################


#Generic function to calculate N times Samples observations of y, given a simulated
#dataset x1 and x2
#b0, b1, b2 and rho are the true values of the model parameters
#method should either be "WC" for wrapped Cauchy or "WN" for wrapped Normal
calculate_y <- function(x1, x2, b0, b1, b2, rho, method){
  #calculate the predicted values of y and add residuals with mean=0 and rho=rho
  if(method=="WC")  y <- b0 + b1*x1 + b2*x2 + rwrappedcauchy(length(x1), mu=0, rho=rho)
  if(method=="WN")  y <- b0 + b1*x1 + b2*x2 + rnorm(length(x1), mean=0, sd=sqrt(-2*log(rho)))
  
  #return y modulo 2pi
  y <- y %% (2*pi)
  return(y)
}

#in all simulation studies (except the study on sample size) a sample size N of 50 was used
N       <- 50

#The number of samples in all sutdies was 500
samples <- 500

####################################################################
#############Simulation study 3.1: Ancova setting###################
####################################################################
set.seed(23)
if(!dir.exists("Ancova")) dir.create("Ancova")
setwd("Ancova")
#x1 is dichotomous with probability p=0.5, x2 is normally distributed with mean zero and sd 1
#x2 is scaled
x1 <- rep(rbinom(N,1,0.5),samples)
x2 <- rep(scale(rnorm(N,mean=0, sd=1)),samples)

#Generating all files for WN:
for(b0 in c("0.0", "3.0","6.0")){
  for(b1 in c("0.0", "3.0","6.0")){
    for(b2 in c("-5.0","-1.0","0.0","1.0","5.0")){
      for(rho in c("0.5", "0.9")){
        #Generate the y data
        y        <- calculate_y(x1, x2, as.numeric(b0), as.numeric(b1),as.numeric(b2), as.numeric(rho), method="WN")
        #Compose the correct filename
        filename <- paste0(c("AncovaWN", samples, N, b0, b1, b2,rho), collapse="_")
        filename <- paste0(c(filename,".dat"), collapse="")
        #Save the data
        write.table(data.frame(y, x1, x2), row.names=F, col.names=F,file=filename) 
      }
    }
  }
}

#Generating all files for WC:
for(b0 in c("0.0", "3.0","6.0")){
  for(b1 in c("0.0", "3.0","6.0")){
    for(b2 in c("-5.0","-1.0","0.0","1.0","5.0")){
      for(rho in c("0.5", "0.9")){
        #Generate the y data
        y        <- calculate_y(x1, x2, as.numeric(b0), as.numeric(b1),as.numeric(b2), as.numeric(rho), method="WC")
        #Compose the correct filename
        filename <- paste0(c("AncovaWC", samples, N, b0, b1, b2,rho), collapse="_")
        filename <- paste0(c(filename,".dat"), collapse="")
        #Save the data
        write.table(data.frame(y, x1, x2), row.names=F, col.names=F,file=filename) 
      }
    }
  }
}

setwd("..")
################################################################
#############Simulation study 3.2: LP setting###################
################################################################
set.seed(23)
if(!dir.exists("LP")) dir.create("LP")
setwd("LP")
#x1 and x2 are multivariate normally distributed with mean zero, sd 1 and zero correlation
cor <- 0.0
Sigma <- matrix(c(1,cor,cor,1),2,2)
d <- mvrnorm(N, mu=c(0,0), Sigma)
x1 <- rep(scale(d[,1]), samples)
x2 <- rep(scale(d[,2]), samples)

#Generating all files for WN:
for(b0 in c("3.0")){
  for(b1 in c("-5.0","-1.0","0.0","1.0","5.0")){
    for(b2 in c("-5.0","-1.0","0.0","1.0","5.0")){
      for(rho in c("0.5", "0.9")){
        #Generate the y data
        y        <- calculate_y(x1, x2, as.numeric(b0), as.numeric(b1),as.numeric(b2), as.numeric(rho), method="WN")
        #Compose the correct filename
        filename <- paste0(c("LPWN", samples, N, b0, b1, b2,rho), collapse="_")
        filename <- paste0(c(filename,".dat"), collapse="")
        #Save the data
        write.table(data.frame(y, x1, x2), row.names=F, col.names=F,file=filename) 
      }
    }
  }
}

#Generating all files for WC:
for(b0 in c("3.0")){
  for(b1 in c("-5.0","-1.0","0.0","1.0","5.0")){
    for(b2 in c("-5.0","-1.0","0.0","1.0","5.0")){
      for(rho in c("0.5", "0.9")){
        #Generate the y data
        y        <- calculate_y(x1, x2, as.numeric(b0), as.numeric(b1),as.numeric(b2), as.numeric(rho), method="WC")
        #Compose the correct filename
        filename <- paste0(c("LPWC", samples, N, b0, b1, b2,rho), collapse="_")
        filename <- paste0(c(filename,".dat"), collapse="")
        #Save the data
        write.table(data.frame(y, x1, x2), row.names=F, col.names=F,file=filename) 
      }
    }
  }
}

setwd("..")
###################################################################
#############Simulation study 3.3.1: Sample size###################
###################################################################
set.seed(23)
if(!dir.exists("N")) dir.create("N")
setwd("N")

#The Ancova setting:
for(N in c(10,15,20,30,50,75)){
  set.seed(23)
  x1 <- rep(rbinom(N,1,0.5),samples)
  x2 <- rep(scale(rnorm(N,mean=0, sd=1)),samples)
  
  #Generating all files for WN:
  for(b0 in c("3.0")){
    for(b1 in c("3.0")){
      for(b2 in c("1.0")){
        for(rho in c("0.5", "0.9")){
          #Generate the y data
          y        <- calculate_y(x1, x2, as.numeric(b0), as.numeric(b1),as.numeric(b2), as.numeric(rho), method="WN")
          #Compose the correct filename
          filename <- paste0(c("AncovaWN", samples, N, b0, b1, b2,rho), collapse="_")
          filename <- paste0(c(filename,".dat"), collapse="")
          #Save the data
          write.table(data.frame(y, x1, x2), row.names=F, col.names=F,file=filename) 
        }
      }
    }
  }
  
  #Generating all files for WC:
  for(b0 in c("3.0")){
    for(b1 in c("3.0")){
      for(b2 in c("1.0")){
        for(rho in c("0.5", "0.9")){
          #Generate the y data
          y        <- calculate_y(x1, x2, as.numeric(b0), as.numeric(b1),as.numeric(b2), as.numeric(rho), method="WC")
          #Compose the correct filename
          filename <- paste0(c("AncovaWC", samples, N, b0, b1, b2,rho), collapse="_")
          filename <- paste0(c(filename,".dat"), collapse="")
          #Save the data
          write.table(data.frame(y, x1, x2), row.names=F, col.names=F,file=filename) 
        }
      }
    }
  }
  
}



#The Linear predictor setting:
for(N in c(10,15,20,30,50,75)){
  set.seed(23)
  cor <- 0.0
  Sigma <- matrix(c(1,cor,cor,1),2,2)
  d <- mvrnorm(N, mu=c(0,0), Sigma)
  x1 <- rep(scale(d[,1]), samples)
  x2 <- rep(scale(d[,2]), samples)
  
  #Generating all files for WN:
  for(b0 in c("3.0")){
    for(b1 in c("1.0")){
      for(b2 in c("1.0")){
        for(rho in c("0.5", "0.9")){
          #Generate the y data
          y        <- calculate_y(x1, x2, as.numeric(b0), as.numeric(b1),as.numeric(b2), as.numeric(rho), method="WN")
          #Compose the correct filename
          filename <- paste0(c("LPWN", samples, N, b0, b1, b2,rho), collapse="_")
          filename <- paste0(c(filename,".dat"), collapse="")
          #Save the data
          write.table(data.frame(y, x1, x2), row.names=F, col.names=F,file=filename) 
        }
      }
    }
  }

  #Generating all files for WC:
  for(b0 in c("3.0")){
    for(b1 in c("1.0")){
      for(b2 in c("1.0")){
        for(rho in c("0.5", "0.9")){
          #Generate the y data
          y        <- calculate_y(x1, x2, as.numeric(b0), as.numeric(b1),as.numeric(b2), as.numeric(rho), method="WC")
          #Compose the correct filename
          filename <- paste0(c("LPWC", samples, N, b0, b1, b2,rho), collapse="_")
          filename <- paste0(c(filename,".dat"), collapse="")
          #Save the data
          write.table(data.frame(y, x1, x2), row.names=F, col.names=F,file=filename) 
        }
      }
    }
  }

}



N<-50
setwd("..")
##################################################################################
#############Simulation study 3.3.2: violation error assumption###################
##################################################################################
set.seed(23)
if(!dir.exists("Errors")) dir.create("Errors")
setwd("Errors")

file.copy(from="../Ancova/AncovaWN_500_50_3.0_3.0_1.0_0.9.dat",to="AncovaWN_500_50_3.0_3.0_1.0_0.9.dat")
file.copy(from="../Ancova/AncovaWC_500_50_3.0_3.0_1.0_0.9.dat",to="AncovaWC_500_50_3.0_3.0_1.0_0.9.dat")

file.copy(from="../LP/LPWN_500_50_3.0_1.0_1.0_0.9.dat",to="LPWN_500_50_3.0_1.0_1.0_0.9.dat")
file.copy(from="../LP/LPWC_500_50_3.0_1.0_1.0_0.9.dat",to="LPWC_500_50_3.0_1.0_1.0_0.9.dat")

setwd("..")
#########################################################################
#############Simulation study 3.3.3: Multicollinearity###################
#########################################################################
set.seed(23)
if(!dir.exists("Multicollinearity")) dir.create("Multicollinearity")
setwd("Multicollinearity")

for(cor in c(-0.9,-0.5,-0.1,0.1,0.5,0.9)){
  set.seed(23)
  Sigma <- matrix(c(1,cor,cor,1),2,2)
  d <- mvrnorm(N, mu=c(0,0), Sigma)
  x1 <- rep(scale(d[,1]), samples)
  x2 <- rep(scale(d[,2]), samples)

  #Generating all files for WN:
  for(b0 in c("3.0")){
    for(b1 in c("1.0")){
      for(b2 in c("1.0")){
        for(rho in c("0.5", "0.9")){
          #Generate the y data
          y        <- calculate_y(x1, x2, as.numeric(b0), as.numeric(b1),as.numeric(b2), as.numeric(rho), method="WN")
          #Compose the correct filename
          filename <- paste0(c("LP",cor,"WN"), collapse="")
          filename <- paste0(c(filename, samples, N, b0, b1, b2,rho), collapse="_")
          filename <- paste0(c(filename,".dat"), collapse="")
          #Save the data
          write.table(data.frame(y, x1, x2), row.names=F, col.names=F,file=filename) 
        }
      }
    }
  }
  
  #Generating all files for WC:
  for(b0 in c("3.0")){
    for(b1 in c("1.0")){
      for(b2 in c("1.0")){
        for(rho in c("0.5", "0.9")){
          #Generate the y data
          y        <- calculate_y(x1, x2, as.numeric(b0), as.numeric(b1),as.numeric(b2), as.numeric(rho), method="WC")
          #Compose the correct filename
          filename <- paste0(c("LP",cor,"WC"), collapse="")
          filename <- paste0(c(filename, samples, N, b0, b1, b2,rho), collapse="_")
          filename <- paste0(c(filename,".dat"), collapse="")
          #Save the data
          write.table(data.frame(y, x1, x2), row.names=F, col.names=F,file=filename) 
        }
      }
    }
  }
}

#Results for zero correlation were already obtained in the LP setting
setwd("..")

######################################################################
#############Simulation study 3.3.4: Group size#######################
######################################################################
set.seed(23)
if(!dir.exists("Group_size")) dir.create("Group_size")
setwd("Group_size")

for(N_group in  c(5, 15, 25, 35, 45)){

  x1 <- rep(c(rep(1,N_group),rep(0,N-N_group)),samples)
  x2 <- rep(rep(0,N),samples)

  #Generating all files for WN:
  for(b0 in c("0.0")){
    for(b1 in c("1.0")){
      for(b2 in c("0.0")){
        for(rho in c("0.5", "0.9")){
          #Generate the y data
          y        <- calculate_y(x1, x2, as.numeric(b0), as.numeric(b1),as.numeric(b2), as.numeric(rho), method="WN")
          #Compose the correct filename
          filename <- paste0(c("Ancova",N_group,"WN"))
          filename <- paste0(c(filename, samples, N, b0, b1,rho), collapse="_")
          filename <- paste0(c(filename,".dat"), collapse="")
          #Save the data
          write.table(data.frame(y, x1), row.names=F, col.names=F,file=filename) 
        }
      }
    }
  }
  
  #Generating all files for WC:
  for(b0 in c("0.0")){
    for(b1 in c("1.0")){
      for(b2 in c("0.0")){
        for(rho in c("0.5", "0.9")){
          #Generate the y data
          y        <- calculate_y(x1, x2, as.numeric(b0), as.numeric(b1),as.numeric(b2), as.numeric(rho), method="WC")
          #Compose the correct filename
          filename <- paste0(c("Ancova",N_group,"WC"))
          filename <- paste0(c(filename, samples, N, b0, b1,rho), collapse="_")
          filename <- paste0(c(filename,".dat"), collapse="")
          #Save the data
          write.table(data.frame(y, x1), row.names=F, col.names=F,file=filename) 
        }
      }
    }
  }
  
}
setwd("..")
