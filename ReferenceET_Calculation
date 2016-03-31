rm(list = ls())
library(openair)
library(Hmisc)

#-------------------------------------------------------------------------------
## input and output file paths

input.path <- "C:/Users/Mike/Dropbox/PhD/Research/Part I/DataAnalysis/Soyflux_CR1000.csv"

#-------------------------------------------------------------------------------
##Input file location of .csv file for the climate station considered

Station <-
  read.table(input.path, header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)

#-------------------------------------------------------------------------------
##Input altitude, latitude and name of the climate station 

Location <- "Capuaba Farm"
z <- 426.9                            #altitude in m
lat <- -13.2875                       #latitude in decimal degrees
Wheight <- 3.40                       #height of the sensor measuring wind speed, in m

#-------------------------------------------------------------------------------
# Date conversion

attach(Station)

Station$timestamp <- as.POSIXct(Station$timestamp, "%m/%d/%Y %H:%M", tz="GMT")        
Station$date <- as.Date(Station$timestamp, "%Y-%m-%d %H:%M:%S", tz="GMT") 

#Station.hourly <- timeAverage(Station, avg.time = "hour")

#-------------------------------------------------------------------------------
## Propagation of error based on measurements

e.Tair <- 0.3                                         # error from the Vaisala
e.Ws   <- 0.3                                         # error from the Vaisala 
e.RH   <- 4                                           # error from the Vaisala

#-------------------------------------------------------------------------------
## List of variables, constants used in the calculation

uz <- Ws                                              #rename mean wind speed in m/s  
Station$lambda <- 2.501 - (2.361*0.001)*Tair          #Temperature dependent latent heat of vapourization (MJ/kg)
Station$e.lambda <- 0.3*(2.361*0.001)                 #error in the latent heat
Cp <- 1.013*10^-3                                     #Specific heat capacity at constant P (MJ/kg.degC)
sigma <- 4.903*10^-9                                  #Boltzmann constant in MJ/K^4m^2day
Station$u2 <- uz*(4.87/(log(67.8*Wheight-5.42)))      #Wind speed at 2m height calculated from uz, equation (47) 

# error in u2
e.Wheight <- 0.5                                      # error in height measurement in cm
df1   <- 67.8*Wheight
e.df1 <- df1*(0.5/3.4) 
df2   <- log(67.8*Wheight-5.42)
e.df2 <- e.df1/(df1 - 5.52)
df3   <- 4.67/df2
e.df3 <- 4.67*e.df2/df2

Station$e.u2 <- Station$u2*sqrt( (e.Ws/Ws)^2 + (e.df3/df3)^2 ) 

#-------------------------------------------------------------------------------
##Calculation of the psychrometric constant 'gamm' (kPa/degC), equation (8)

Station$gamm <- (Cp*Pair*0.1)/(0.622*Station$lambda)
df4   <- 0.622*Station$lambda
e.df4 <- 0.622*Station$e.lambda 
df5   <- Cp*Pair*0.1
e.df5 <- Cp*0.1*0.5

Station$e.gamm <- Station$gamm*sqrt( (e.df4/df4)^2 + (e.df5/df5)^2 )

#-------------------------------------------------------------------------------
##Calculation of the vapour pressure deficit (es - ea) (kPa)

#Saturation vapour pressure 'es' (kPa), equation (11)
Station$es <- signif(0.6108*exp(17.27*Tair/(Tair+237.3)), digits = 3)     # keep 3 sig. figs
df6   <- 17.27*Tair
e.df6 <- df6*(e.Tair/Tair)
df7   <- df6/(Tair + 237.3)
e.df7 <- df7*sqrt( (e.df6/df6)^2 + (e.Tair/(Tair + 237.3))^2 )

Station$e.es <- Station$es*(e.df7)

#Actual vapour pressure 'ea' (kPa)
Station$ea   <- signif(RH*Station$es*0.01, digits = 3)                    # keep 3 sig. figs
Station$e.ea <- Station$ea*sqrt( (Station$e.es/Station$es)^2 + (e.RH/RH)^2 )

#Vapour pressure deficit (kPa)
Station$VPD   <- signif(Station$es - Station$ea, digits = 3)                # keep 3 sig. figs
Station$e.VPD <- sqrt( (Station$e.es)^2 + (Station$e.ea)^2 ) 

#-------------------------------------------------------------------------------
##Calculation of the slope of the saturated vapour pressure curve, Delta (kPa/degC)

Station$delta   <- signif(4098*(0.6108*exp((17.27*Tair)/(Tair+237.3)))/((Tair+237.3)^2), digits = 3) # keep 3 sig. figs
e.df8 <- 2*(e.Tair/(Tair + 237.3))                                                                   
df9   <- df6/((Tair + 237.3)^2)
e.df9 <- df9*sqrt( (e.df8/((Tair + 237.3)^2))^2 + (e.df6/df6)^2 )

Station$e.delta <- 4098*(Station$delta*e.df9)

#-------------------------------------------------------------------------------
# Conversion of Rn and G from W/m2 into MJ/m230min, with 5% error in Rn (assumed) and 10% in G (manual)

## Correction implemented for wind speed on the NRLite 2
Station$Rn.corr <- Station$Rn*(1+0.01*Ws^(3/4))
df10   <- (1+0.01*Ws^(3/4))
e.df10 <- (3/4)*0.01*(e.Ws/Ws)
Station$e.Rn.corr <- Station$Rn.corr*sqrt( ((0.05*Rn)/Rn)^2 + (e.df10/df10)^2 )

Station$Rn.MJ   <- Station$Rn.corr*3600*0.5*10^-6     #(W/m2)*(J/Ws)*(3600s/hr)*0.5(hr/30min) 
Station$e.Rn.MJ <- Station$e.Rn.corr*3600*0.5*10^-6

# need to apply correction factors to the TEMs and Huksefflux 

G <- data.frame()
for (i in 1:length(Station$timestamp)) {
  g <- c(Station$Hukse_shf_Avg[i], Station$TEM1[i], Station$TEM2[i], Station$TEM3[i], Station$TEM4[i]) 
  g.mean <- mean(g, na.rm = TRUE)*3600*0.5*10^-6    #convert to MJ/m2 30min
  G <- rbind(G, g.mean) 
}
colnames(G) <-("G.MJ")
Station$G.MJ <- G$G.MJ
Station$e.G.MJ <- 0.10*Station$G.MJ

#-------------------------------------------------------------------------------
#Calculation of reference ET (ET0), in mm/day, equation (6)

Station$ET0 <- (0.408*Station$delta*(Station$Rn.MJ-Station$G.MJ) + 
                  Station$gamm*(37/(Tair+273))*Station$u2*Station$VPD)/(Station$delta + Station$gamm*(1+0.34*Station$u2))

Station$ET0 <- signif(Station$ET0, digits = 3)        ##keep 2 significant figures (from ea)

df12   <- 0.408*Station$delta*abs(Station$Rn.MJ-Station$G.MJ)
e.df12 <- df12*sqrt( (Station$e.delta/Station$delta)^2 + ((Station$e.Rn.corr + Station$e.G.MJ)/(Station$Rn.corr + Station$G.MJ))^2 )
df13   <- Station$gamm*37*Station$u2*Station$VPD
e.df13 <- df13*sqrt( (Station$e.gamm/Station$gamm)^2 + (Station$e.u2/Station$u2)^2 + (Station$e.VPD/Station$VPD)^2 )
df14   <- Tair + 273
e.df14 <- e.Tair/df14
df15   <- df13/df14
e.df15 <- df15*sqrt( (e.df13/df13)^2 + (e.df14/df14)^2 )
df16   <- df12 + df15
e.df16 <- sqrt( (e.df12)^2 + (e.df15)^2 )
df17   <- 1+0.34*Station$u2
e.df17 <- 0.34*Station$e.u2
df18   <- Station$gamm*df17
e.df18 <- df18*sqrt( (Station$e.gamm/Station$gamm)^2 + (e.df17/df17)^2 )

Station$e.ET0 <- abs(Station$ET0)*sqrt( (e.df18/df18)^2 + (e.df15/df15)^2 )

# Take daily averages of the time series
Station.daily <- timeAverage(Station, avg.time = "day")
PPT.daily     <- Station.daily$Precip*48
Rn.MJ.daily   <- Station.daily$Rn.MJ*48
G.MJ.daily    <- Station.daily$G.MJ*48

ET0.daily     <- aggregate(Station$ET0, list(day = jday), sum)  
#be careful that this is perhaps not correct to use

#Plot energy balance and ET0

par(mfrow = c(4,1), mar=c(1,4,2,1), oma = c(1,1,1,1))
plot(Station.daily$date, Rn.MJ.daily, type = "l", ylab = "Rn (MJ/m2s)", xlab = "")
plot(Station.daily$date, G.MJ.daily, type = "l", ylab = "G (MJ/m2s)", xlab = "")
plot(Station.daily$date, ET0.daily$x, type = "l", ylab = "ET0 (mm/d)", xlab = "")
plot(Station.daily$date, PPT.daily, type = "h", ylab = "P (mm/d)", xlab = "")
par(mfrow = c(1,1))

##Export graph of time series of Rn, Vap.deficit, ETO
#dev.print(pdf, 
 # file="C:/Users/Mike/Desktop/Poxoreo_Station_ET0_3.pdf",
  # width=7.5, height=10, pointsize=5)

detach(Station)

#-------------------------------------------------------------------------------
#### END #######################################################################
