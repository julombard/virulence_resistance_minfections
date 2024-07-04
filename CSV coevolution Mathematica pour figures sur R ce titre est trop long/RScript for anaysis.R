#### Script for Figures Virulence - Tolerance model ######


library(readr)
library(ggplot2)

dir <- choose.dir()

setwd(dir)

#Set basic working directory
cwd <- setwd(dir)


################################################################################
################################ DISPERSAL #####################################
################################################################################


#I. Get the folder with dispersal files

# Look for the output folder in the directory and list files in it
outputs_dir <- paste(cwd, "Dispersal", sep = "/")
outputs_dir
outdirectory<-setwd(outputs_dir)
listfiles <- list.files(outputs_dir)
listfiles

#II. Getting the Dataframes
data_coinf <- read.csv("DispersalCoinfFinal.csv", header = FALSE)
colnames(data_coinf) <- c("Dispersal","Resistance", "Viru_Obs", "Viru_Theo")

data_supinf <- read.csv("DispersalSupinfFinal.csv", header = FALSE)
colnames(data_supinf)<-c("Dispersal","Resistance", "Viru_Obs", "Viru_Theo")


#III. Do the plots !

##### Host resistance versus diserpsal #####
#Basic graph
plot( data_coinf$Resistance~data_coinf$Dispersal , type="b" , bty="l" , xlab="Dispersal" , ylab="ESS resistance" , col='blue' , lwd=3 , pch=16 , ylim=c(0.3,0.68), xlim=c(0,5.5) )
        lines(data_supinf$Resistance ~data_supinf$Dispersal , col="Red" , lwd=3 , pch=16 , type="b" )

        # Add a legend
legend("bottomright", 
       legend = c("Coinfections", "Superinfections"), 
       col = c("Blue", "Red"), 
       pch = c(16,16), 
       bty = "n", 
       pt.cex = 2, 
       cex = 1.2, 
       text.col = "black", 
       horiz = F , 
       inset = c(0.1, 0.1))

#### Parasite Virulence versus Dispersal ####
#Basic graph
plot( data_coinf$Viru_Obs~data_coinf$Dispersal , type="b" , bty="l" , xlab="Dispersal" , ylab="ESS virulence" , col='blue' , lwd=3 , pch=16 , ylim=c(0.8,1.1), xlim=c(0,5.25) )
lines(data_supinf$Viru_Obs ~data_supinf$Dispersal , col="Red" , lwd=3 , pch=16 , type="b" )

# Add a legend
legend("bottomright", 
       legend = c("Coinfections", "Superinfections"), 
       col = c("Blue", "Red"), 
       pch = c(16,16), 
       bty = "n", 
       pt.cex = 2, 
       cex = 1.2, 
       text.col = "black", 
       horiz = F , 
       inset = c(0.1, 0.1))

#Basic graph
delta_r0_coinf  <- data_coinf$Viru_Obs-data_coinf$Viru_Theo
delta_r0_supinf <- data_supinf$Viru_Obs - data_supinf$Viru_Theo

plot( delta_r0_coinf~data_coinf$Dispersal , type="b" , bty="l" ,ylab = "Deviation from single infections",  xlab="Dispersal" , col='blue' , lwd=3 , pch=16 , ylim=c(0,0.15), xlim=c(0,4.65) )
lines(delta_r0_supinf ~data_supinf$Dispersal , col="Red" , lwd=3 , pch=16 , type="b" )
abline(0,0, lwd =2.5)
# Add a legend
legend("bottomright", 
       legend = c("Coinfections", "Superinfections"), 
       col = c("Blue", "Red"), 
       pch = c(16,16), 
       bty = "n", 
       pt.cex = 2, 
       cex = 1.2, 
       text.col = "black", 
       horiz = F , 
       inset = c(0.1, 0.1))

## The Complete graph with R0opt and ESS values
plot( data_coinf$Viru_Obs~data_coinf$Dispersal , type="b" , bty="l" , xlab="Recovery" , ylab="ESS virulence" , col='blue' , lwd=3 , pch=16 , ylim=c(0,1), xlim=c(0,2) )
lines(data_supinf$Viru_Obs ~data_supinf$Dispersal , col="Red" , lwd=3 , pch=16 , type="b" )
lines(data_coinf$Viru_Theo ~data_coinf$Dispersal , col="Blue" , lwd=2 , pch=1 , type="b" )
lines(data_supinf$Viru_Theo ~data_supinf$Dispersal , col="Red" , lwd=2 , pch=1 , type="b" )
abline(0,0, lwd =2.5)
# Add a legend
legend("bottomright", 
       legend = c("Coinfections", "Superinfections", "CoinfR0", "SupinfR0"), 
       col = c("Blue", "Red", "Blue", "Red"), 
       pch = c(16,16, 1,1), 
       bty = "n", 
       pt.cex = 2, 
       cex = 1.2, 
       text.col = "black", 
       horiz = F , 
       inset = c(0.1, 0.1))


################################################################################
################################ RECOVERY #####################################
################################################################################


#I. Get the folder with dispersal files

# Look for the output folder in the directory and list files in it
outputs_dir <- paste(cwd, "Recovery", sep = "/")
outputs_dir
outdirectory<-setwd(outputs_dir)
listfiles <- list.files(outputs_dir)
listfiles

#II. Getting the Dataframes
data_coinf <- read.csv("RecoveryCoinfFinal.csv", header = FALSE)
colnames(data_coinf) <- c("Recovery","Resistance", "Viru_Obs", "Viru_Theo")

data_supinf <- read.csv("RecoverySupinfFinal.csv", header = FALSE)
colnames(data_supinf)<-c("Recovery","Resistance", "Viru_Obs", "Viru_Theo")


#III. Do the plots !
##### Host resistance versus diserpsal #####
#Basic graph
plot( data_coinf$Resistance~data_coinf$Recovery , type="b" , bty="l" , xlab="Recovery" , ylab="ESS resistance" , col='blue' , lwd=3 , pch=16 , ylim=c(0,0.8) )
lines(data_supinf$Resistance ~data_supinf$Recovery , col="Red" , lwd=3 , pch=16 , type="b" )

# Add a legend
legend("bottomright", 
       legend = c("Coinfections", "Superinfections"), 
       col = c("Blue", "Red"), 
       pch = c(16,16), 
       bty = "n", 
       pt.cex = 2, 
       cex = 1.2, 
       text.col = "black", 
       horiz = F , 
       inset = c(0.1, 0.1))

#### Parasite Virulence versus Dispersal ####
#Basic graph
plot( data_coinf$Viru_Obs~data_coinf$Recovery , type="b" , bty="l" , xlab="Recovery" , ylab="ESS virulence" , col='blue' , lwd=3 , pch=16 , ylim=c(0.5,1.1), xlim=c(0,2.1) )
lines(data_supinf$Viru_Obs ~data_supinf$Recovery , col="Red" , lwd=3 , pch=16 , type="b" )

# Add a legend
legend("bottomright", 
       legend = c("Coinfections", "Superinfections"), 
       col = c("Blue", "Red"), 
       pch = c(16,16), 
       bty = "n", 
       pt.cex = 2, 
       cex = 1.2, 
       text.col = "black", 
       horiz = F , 
       inset = c(0.1, 0.1))

# The R0 Optimization 


# Get the distance between predicted ESS with coevolution and multiple infection versus ESS predicted under R0 optimization.

#Basic graph
delta_r0_coinf  <- data_coinf$Viru_Obs-data_coinf$Viru_Theo
delta_r0_supinf <- data_supinf$Viru_Obs - data_supinf$Viru_Theo

plot( delta_r0_coinf~data_coinf$Recovery , type="b" , bty="l" , xlab="Recovery" , ylab="Deviation from single infections" , col='blue' , lwd=3 , pch=16 , ylim=c(0,0.6), xlim=c(0,2.5) )
lines(delta_r0_supinf ~data_supinf$Recovery , col="Red" , lwd=3 , pch=16 , type="b" )
abline(0,0, lwd =2.5)
# Add a legend
legend("topright", 
       legend = c("Coinfections", "Superinfections"), 
       col = c("Blue", "Red"), 
       pch = c(16,16), 
       bty = "n", 
       pt.cex = 2, 
       cex = 1.2, 
       text.col = "black", 
       horiz = F , 
       inset = c(0.1, 0.1))


## The Complete graph with R0opt and ESS values
plot( data_coinf$Viru_Obs~data_coinf$Recovery , type="b" , bty="l" , xlab="Recovery" , ylab="ESS virulence" , col='blue' , lwd=3 , pch=16 , ylim=c(0,1), xlim=c(0,2) )
lines(data_supinf$Viru_Obs ~data_supinf$Recovery , col="Red" , lwd=3 , pch=16 , type="b" )
lines(data_coinf$Viru_Theo ~data_coinf$Recovery , col="Blue" , lwd=2 , pch=1 , type="b" )
lines(data_supinf$Viru_Theo ~data_supinf$Recovery , col="Red" , lwd=2 , pch=1 , type="b" )
abline(0,0, lwd =2.5)
# Add a legend
legend("bottomright", 
       legend = c("Coinfections", "Superinfections", "CoinfR0", "SupinfR0"), 
       col = c("Blue", "Red", "Blue", "Red"), 
       pch = c(16,16, 1,1), 
       bty = "n", 
       pt.cex = 2, 
       cex = 1.2, 
       text.col = "black", 
       horiz = F , 
       inset = c(0.1, 0.1))


################################################################################
############################# Background Mortality #############################
################################################################################


#I. Get the folder with dispersal files

# Look for the output folder in the directory and list files in it
outputs_dir <- paste(cwd, "Background Mortality", sep = "/")
outputs_dir
outdirectory<-setwd(outputs_dir)
listfiles <- list.files(outputs_dir)
listfiles

#II. Getting the Dataframes
data_coinf <- read.csv("BGmortalityCoinfFinal.csv", header = FALSE)
colnames(data_coinf) <- c("Background Mortality","Resistance", "Viru_Obs", "Viru_Theo")

data_supinf <- read.csv("BGmortalitySupinfFinal.csv", header = FALSE)
colnames(data_supinf)<-c("Background Mortality","Resistance", "Viru_Obs", "Viru_Theo")


#III. Do the plots !
##### Host resistance versus diserpsal #####
#Basic graph
plot( data_coinf$Resistance~data_coinf$`Background Mortality` , type="b" , bty="l" , xlab="Background mortality" , ylab="ESS resistance" , col='blue' , lwd=3 , pch=16 , ylim=c(0,0.8) )
lines(data_supinf$Resistance ~data_supinf$`Background Mortality` , col="Red" , lwd=3 , pch=16 , type="b" )

# Add a legend
legend("bottomright", 
       legend = c("Coinfections", "Superinfections"), 
       col = c("Blue", "Red"), 
       pch = c(16,16), 
       bty = "n", 
       pt.cex = 2, 
       cex = 1.2, 
       text.col = "black", 
       horiz = F , 
       inset = c(0.1, 0.1))

#### Parasite Virulence versus Dispersal ####
#Basic graph
plot( data_coinf$Viru_Obs~data_coinf$`Background Mortality` , type="b" , bty="l" , xlab="Background mortality" , ylab="ESS virulence" , col='blue' , lwd=3 , pch=16 , ylim=c(0.8,1.1), xlim=c(0,0.8) )
lines(data_supinf$Viru_Obs ~data_supinf$`Background Mortality` , col="Red" , lwd=3 , pch=16 , type="b" )

# Add a legend
legend("bottomright", 
       legend = c("Coinfections", "Superinfections"), 
       col = c("Blue", "Red"), 
       pch = c(16,16), 
       bty = "n", 
       pt.cex = 2, 
       cex = 1.2, 
       text.col = "black", 
       horiz = F , 
       inset = c(0.1, 0.1))

#Basic graph
delta_r0_coinf  <- data_coinf$Viru_Obs-data_coinf$Viru_Theo
delta_r0_supinf <- data_supinf$Viru_Obs - data_supinf$Viru_Theo

plot( delta_r0_coinf~data_coinf$`Background Mortality` , type="b" , bty="l" , xlab="Background mortality" , ylab="Deviation from single infections" , col='blue' , lwd=3 , pch=16 , ylim=c(0,0.2), xlim=c(0,0.8) )
lines(delta_r0_supinf ~data_supinf$`Background Mortality` , col="Red" , lwd=3 , pch=16 , type="b" )
abline(0,0, lwd =2.5)
# Add a legend
legend("topright", 
       legend = c("Coinfections", "Superinfections"), 
       col = c("Blue", "Red"), 
       pch = c(16,16), 
       bty = "n", 
       pt.cex = 2, 
       cex = 1.2, 
       text.col = "black", 
       horiz = F , 
       inset = c(0.1, 0.1))

## The Complete graph with R0opt and ESS values
plot( data_coinf$Viru_Obs~data_coinf$`Background Mortality` , type="b" , bty="l" , xlab="Recovery" , ylab="ESS virulence" , col='blue' , lwd=3 , pch=16 , ylim=c(0.5,1), xlim=c(0,0.8) )
lines(data_supinf$Viru_Obs ~data_supinf$`Background Mortality` , col="Red" , lwd=3 , pch=16 , type="b" )
lines(data_coinf$Viru_Theo ~data_coinf$`Background Mortality` , col="Blue" , lwd=2 , pch=1 , type="b" )
lines(data_supinf$Viru_Theo ~data_supinf$`Background Mortality` , col="Red" , lwd=2 , pch=1 , type="b" )
abline(0,0, lwd =2.5)
# Add a legend
legend("bottomright", 
       legend = c("Coinfections", "Superinfections", "CoinfR0", "SupinfR0"), 
       col = c("Blue", "Red", "Blue", "Red"), 
       pch = c(16,16, 1,1), 
       bty = "n", 
       pt.cex = 2, 
       cex = 1.2, 
       text.col = "black", 
       horiz = F , 
       inset = c(0.1, 0.1))

