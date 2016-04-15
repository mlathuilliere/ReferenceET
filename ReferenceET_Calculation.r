rm(list = ls())
library(openair)
library(Hmisc)

#-------------------------------------------------------------------------------
## Input/output file paths

input.path         <- "C:/Users/Mike/Dropbox/PhD/Research/Part I/DataAnalysis/Soyflux_CR1000.csv"
output.path.graph  <- "C:/users/Mike/Desktop/Reference_ET.pdf"
output.path.table  <- "C:/users/Mike/Desktop/Reference_ET.txt"
output.path.table2 <- "C:/users/Mike/Desktop/Station_data.txt" 

#-------------------------------------------------------------------------------
## Input file location of .csv file for the climate station considered

Station <-
  read.table(input.path, header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)

#-------------------------------------------------------------------------------
## Input altitude, latitude and name of the climate station 

Location <- "Capuaba Farm"
z        <- 426.9                      #altitude in m
lat      <- -13.2875                   #latitude in decimal degrees
long     <- 50.0882                    #longitude in decimal degrees
Lz       <- 60                         #Longitude of center of time zone
timestep <- 0.5                        #0.5 for 30 min time step, or 1 for hourly
Wheight  <- 3.40                       #height of the sensor measuring wind speed, in m
a.s      <- 0.41                       #calibration factor to relate clear sky shortwave radiation (Rso)
                                          # to extraterrestrial radiation (Ra) - site specific

#input albedo assumptions or measurements
alpha.ref  <- 0.23

#input days of interest for planting season
start <- "2015-10-02"
end   <- "2016-02-01"

#-------------------------------------------------------------------------------
## Date conversion

Station$timestamp     <- as.POSIXct(Station$timestamp, "%m/%d/%Y %H:%M", tz="GMT")        
Station$timestamp.AMT <- as.POSIXct(Station$timestamp, "%m/%d/%Y %H:%M", tz="GMT")
attributes(Station$timestamp.AMT)$tzone <- "America/Cuiaba"
Station$date <- as.Date(Station$timestamp, "%Y-%m-%d %H:%M:%S", tz="GMT")

# Convert times to decimals and AMT timezone
Station$time <- sapply(strsplit(format.Date(Station$timestamp.AMT, "%H:%M", tz="AMT"),":"),
  function(x){
    x <- as.numeric(x)
    (x[1]+x[2]/60)
  }
)

# Select the time series of interest                    
Station <- selectByDate(Station, start = start, end = end)

#-------------------------------------------------------------------------------
## Propagation of error based on measurements

e.Tair <- 0.3                                         #error from the Vaisala WXT520 (oC)
e.Ws   <- 0.3                                         #error from the Vaisala WXT520 (m/s)
e.RH   <- 4                                           #error from the Vaisala WXT520 (%)
e.Pair <- 0.5                                         #error from the Vaisala WXT520 (hPa)
e.Rs   <- 0.03*Station$Rs                             #error from LI200X pyranometer
e.Wheight <- 0.5                                      #error in positioning of Vaisala WXY520 (cm)

#-------------------------------------------------------------------------------
## List of variables, constants used in the calculation

attach(Station)

uz <- Ws                                              #rename mean wind speed in m/s  
Station$lambda <- 2.501 - (2.361*0.001)*Tair          #Temperature dependent latent heat of vapourization (MJ/kg)
Station$e.lambda <- 0.3*(2.361*0.001)                 #error in the latent heat of vapourization (MJ/kg)
Cp <- 1.013*10^-3                                     #Specific heat capacity at constant P (MJ/kg.degC)
sigma <- 4.903*10^-9                                  #Boltzmann constant in MJ/K^4m^2day
Station$u2 <- uz*(4.87/(log(67.8*Wheight-5.42)))      #Wind speed at 2m height calculated from uz, equation (47) 

# error in u2, wind speed at 2 m height
df1   <- 67.8*Wheight
e.df1 <- df1*(0.5/3.4) 
df2   <- log(67.8*Wheight-5.42)
e.df2 <- e.df1/(df1 - 5.52)
df3   <- 4.67/df2
e.df3 <- 4.67*e.df2/df2

Station$e.u2 <- Station$u2*sqrt( (e.Ws/Ws)^2 + (e.df3/df3)^2 ) 

#-------------------------------------------------------------------------------
## Calculation of the psychrometric constant 'gamm' (kPa/degC), equation (8)

Station$gamm <- (Cp*Pair*0.1)/(0.622*Station$lambda)

# error in the psychrometric constant 'gamm'
df4   <- 0.622*Station$lambda
e.df4 <- 0.622*Station$e.lambda 
df5   <- Cp*Pair*0.1
e.df5 <- Cp*0.1*e.Pair

Station$e.gamm <- Station$gamm*sqrt( (e.df4/df4)^2 + (e.df5/df5)^2 )

#-------------------------------------------------------------------------------
## Calculation of the vapour pressure deficit (es - ea) (kPa)

#Saturation vapour pressure 'es' (kPa), equation (11)
Station$es <- signif(0.6108*exp(17.27*Tair/(Tair+237.3)), digits = 3)     # keep 3 sig. figs

#error in es
df6   <- 17.27*Tair
e.df6 <- df6*(e.Tair/Tair)
df7   <- df6/(Tair + 237.3)
e.df7 <- df7*sqrt( (e.df6/df6)^2 + (e.Tair/(Tair + 237.3))^2 )
Station$e.es <- Station$es*(e.df7)

#Actual vapour pressure 'ea' (kPa)
Station$ea   <- signif(RH*Station$es*0.01, digits = 3)                    # keep 3 sig. figs

# error in ea
Station$e.ea <- Station$ea*sqrt( (Station$e.es/Station$es)^2 + (e.RH/RH)^2 )

#Vapour pressure deficit (kPa)
Station$VPD   <- signif(Station$es - Station$ea, digits = 3)              # keep 3 sig. figs

#error in VPD
Station$e.VPD <- sqrt( (Station$e.es)^2 + (Station$e.ea)^2 ) 

#-------------------------------------------------------------------------------
## Calculation of the slope of the saturated vapour pressure curve, Delta (kPa/degC)

Station$Delta   <- signif(4098*(0.6108*exp((17.27*Tair)/(Tair+237.3)))/((Tair+237.3)^2), digits = 3) # keep 3 sig. figs

#error in Delta
e.df8 <- 2*(e.Tair/(Tair + 237.3))                                                                   
df9   <- df6/((Tair + 237.3)^2)
e.df9 <- df9*sqrt( (e.df8/((Tair + 237.3)^2))^2 + (e.df6/df6)^2 )
Station$e.Delta <- 4098*(Station$Delta*e.df9)

#-------------------------------------------------------------------------------
## Calculation of the extraterrestrial radiation (Ra) equation (28)

##Input longitude (fi) and latitude (lat)
fi <- signif((pi/180)*(lat), digits = 4)         ##in rad, equation (22), keep 4 significant figures
Gsc <- 0.0820                                    ##Solar constant in MJ/m2m-1
fi.2 <- (279.575 + 0.986*jday)*pi/180

#Calculation of inverse relative distance Earth-Sun, equation (23)
dr <- 1+0.033*cos(2*pi*jday/365)                                                   

#Calculation of the solar declination, equation (24)
delta <- 0.409*sin(-1.39+(2*pi*jday/365))                                          

#Seasonal correction for solar time (Sc), equation (33) and (32)
b.coef <- 2*pi*(jday - 81)/364
Sc     <- 0.1645*sin(2*b.coef) - 0.1255*cos(b.coef) - 0.025*sin(b.coef)    # in hour

#Solar time angle at midpoint of the period, equation (31) 
w  <- (pi/12)*(( (Station$time - 0.25) + 0.06667*(Lz - long) + Sc) - 12)    # in rad
w1 <- w - (pi*timestep/24)                                                  # in rad (from equation (29))
w2 <- w + (pi*timestep/24)                                                  # in rad (from equation (30))

#Calculation of extraterrestial radiation, in MJ/m2-30min, equation (28)
Station$Ra.MJ <- signif((24*60/pi)*Gsc*dr*((w2 - w1)*sin(fi)*sin(delta)+cos(fi)*cos(delta)*(sin(w2) - sin(w1))), digits = 4)
Station$Ra.MJ <- ifelse(Station$Ra.MJ <0, 0, Station$Ra.MJ)     # if the solar angle is negative (nighttime) then Ra = 0

#Convert Ra into W/m2.s
Station$Ra.W <- Station$Ra.MJ*10^6/(3600*24)

#-------------------------------------------------------------------------------
## Determination of theoretical Rn for reference grass crop 

#Conversion of Rs into MJ/m2-30min
Station$Rs.MJ   <- Station$Rs*3600*0.5*10^-6
Station$e.Rs.MJ <- e.Rs*3600*0.5*10^-6 

#Calculation of net shortwave radiation (Rns), equation (38)
Station$Rns.MJ <- (1 - alpha.ref)*Station$Rs.MJ            # net shortwave radiation (MJ/m2-30min)
Station$e.Rns.MJ <- (1 - alpha.ref)*Station$e.Rs.MJ        # error in Rns.MJ (MJ/m2-30min)

#Calculation of clear sky net shortwave radiation (Rso), equation (37)
#Station$Rso.MJ <- (0.75 + z*2*10^-5)*Station$Ra.MJ         # clear sky net shortwave radiation (MJ/m2-30min)
Station$Rso.MJ <- a.s*Station$Ra.MJ                       # using Rso=0.42Ra based on measurements

#Calculation of net longwave radiation (Rnl), equation (39)
Rs.Rso <- ifelse(Station$Rso.MJ == 0, 0.5, Station$Rs.MJ/Station$Rso.MJ)
Rs.Rso <- ifelse(Rs.Rso >1, 1, Rs.Rso)                     #used to constrain Rs with values of Rso on sunny days 
#an improvement can be made on the above line to select the last Rs/Rso before sunset for nighttime Rnl
#as of now the value is assumed to be 0.5 as per FAO56

Station$Rnl.MJ <- (0.1021*10^-9)*((Station$Tair + 273.15)^4)*(0.34-0.14*sqrt(Station$ea))*(1.35*(Rs.Rso) - 0.35)

#Calculation of the error in Rnl.MJ
df10   <- (Station$Tair + 273.15)^4
e.df10 <- df10*4*(e.Tair/Station$Tair) 
df11   <- sqrt(Station$ea)
e.df11 <- df11*0.5*(Station$e.ea/Station$ea)
Station$e.Rnl.MJ <- (0.1021*10^-9)*e.df10*0.14*e.df11*(1.35/Station$Rso.MJ)*Station$e.Rs.MJ

# Calculation of theoretical Rn (over grass) considering above equations
Station$Rn.MJ.grass <- Station$Rns.MJ - Station$Rnl.MJ     # in MJ/m2-30min
Station$e.Rn.MJ.grass <- ifelse(Station$Rso.MJ == 0, Station$e.Rns.MJ, Station$e.Rns.MJ + Station$e.Rnl.MJ)    #error in Rn.MJ.grass

#-------------------------------------------------------------------------------
## Ground heat flux measurements

#Create data frame for all G sensor measurements
G <- data.frame()
for (i in 1:length(Station$timestamp)) {
  g <- c(Station$Hukse_shf_Avg[i], Station$TEM1[i], Station$TEM2[i], Station$TEM3[i], Station$TEM4[i]) 
  g.mean <- mean(g, na.rm = TRUE)*3600*0.5*10^-6    #convert to MJ/m2 30min
  G <- rbind(G, g.mean) 
}
colnames(G) <-("G.MJ")
Station$G.MJ <- G$G.MJ

#error in G.MJ
Station$e.G.MJ <- 0.10*Station$G.MJ

#-------------------------------------------------------------------------------
#Calculation of reference ET (ET0), in mm/30min, equation (6)

Station$ET0 <- (0.408*Station$Delta*(Station$Rn.MJ.grass-Station$G.MJ) + 
                  Station$gamm*(37/(Tair+273))*Station$u2*Station$VPD)/(Station$Delta + Station$gamm*(1+0.34*Station$u2))

Station$ET0 <- signif(Station$ET0, digits = 3)        ##keep 2 significant figures (from ea)

#error in ET0 with 10% error in G values
df12   <- 0.408*Station$Delta*abs(Station$Rn.MJ-Station$G.MJ)
e.df12 <- df12*sqrt( (Station$e.Delta/Station$Delta)^2 + ((Station$e.Rn.MR.grass + Station$e.G.MJ)/(Station$Rn.MJ.grass + Station$G.MJ))^2 )
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

detach(Station)

#-------------------------------------------------------------------------------
## Take daily averages of the time series

PPT.daily     <- aggregate(Station$Precip, FUN = sum, by = list(Date = Station$date), na.rm = "TRUE")
Ra.MJ.daily   <- aggregate(Station$Ra.MJ,  FUN = sum, by = list(Date = Station$date), na.rm = "TRUE")
Rs.MJ.daily   <- aggregate(Station$Rs.MJ,  FUN = sum, by = list(Date = Station$date), na.rm = "TRUE")
Rn.MJ.daily   <- aggregate(Station$Rn.MJ.grass, FUN = sum, by =  list(Date = Station$date), na.rm = "TRUE")
G.MJ.daily    <- aggregate(Station$G.MJ, FUN = sum, by = list(Date = Station$date), na.rm = "TRUE")
ET0.daily     <- aggregate(Station$ET0, FUN = sum, by = list(Date = Station$date), na.rm = "TRUE") 
e.ET0.daily   <- aggregate(Station$e.ET0, FUN = sum, by = list(Date = Station$date), na.rm = "TRUE")

#Create data frame for export of all variables for Reference ET daily calculation
Reference.ET <- data.frame(ET0.daily$Date, Ra.MJ.daily$x, Rs.MJ.daily$x, Rn.MJ.daily$x, G.MJ.daily$x, ET0.daily$x, e.ET0.daily$x)
colnames(Reference.ET) <- c("Date", "Ra.MJ", "Rs.MJ", "Rn.MJ", "G.MJ", "ET0", "e.ET0")

#Plot energy balance and ET0
par(mfrow = c(4,1), mar=c(2,4,2,1), oma = c(3,2,1,1))
plot(PPT.daily$Date, PPT.daily$x, type = "h", ylab = "", xaxt="n", xaxs = "i")
mtext(expression(paste("PPT (mm ", d^{-1}, ")", sep = "")), side = 2, line = 3, cex = 0.75)
axis.Date(1, at=seq(PPT.daily$Date[1], max(PPT.daily$Date), by="months"), format = "%b-%y", labels = TRUE)
plot(Rn.MJ.daily$Date, Rn.MJ.daily$x, type = "l", ylab = "", xaxt="n", xaxs = "i")
mtext(expression(paste("Rn (MJ", " m"^{-2}, "d"^{-1}, ")", sep = "")), side = 2, line = 3, cex = 0.75)
axis.Date(1, at=seq(Rn.MJ.daily$Date[1], max(Rn.MJ.daily$Date), by="months"), format = "%b-%y", labels = TRUE)
plot(G.MJ.daily$Date, G.MJ.daily$x, type = "l", ylab = "", xaxt="n", xaxs = "i")
mtext(expression(paste("G (MJ", " m"^{-2}, "d"^{-1}, ")", sep = "")), side = 2, line = 3, cex = 0.75)
axis.Date(1, at=seq(G.MJ.daily$Date[1], max(G.MJ.daily$Date), by="months"), format = "%b-%y", labels = TRUE)
plot(ET0.daily$Date, ET0.daily$x, type = "l", ylab = "", xaxt="n", xaxs = "i")
mtext(expression(paste("ET"[0], " (mm d"^{-1}, ")", sep ="")), side = 2, line = 3, cex = 0.75)
axis.Date(1, at=seq(ET0.daily$Date[1], max(ET0.daily$Date), by="months"), format = "%b-%y", labels = TRUE)
title(main = paste(Location, "from", start, "to", end, sep = " "), line = -1, outer = "TRUE")
par(mfrow = c(1,1))

#Export graph of time series of Rn, Vap.deficit, ETO
dev.print(pdf, file=output.path.graph, width=5, height=6, pointsize=9)

#Export table
write.table(Reference.ET, file=output.path.table, sep = ",", na = "", dec = ".", row.names = FALSE, col.names = TRUE )
write.table(Station, file=output.path.table2, sep = ",", na = "", dec = ".", row.names = FALSE, col.names = TRUE )

#-------------------------------------------------------------------------------
## Calculation of the crop albedo (alpha.crop) using Rn for grass surface and Rn measurements
## above the crop canopy

##Conversion of Net radiation (Rn) from W/m2 into MJ/m2-30min with 5% error in Rn (assumed)

# Correction implemented for wind speed on the NRLite 2
Station$Rn.corr <- Station$Rn*(1+0.01*Station$Ws^(3/4))

#error in Rn.corr
df19   <- (1+0.01*Station$Ws^(3/4))
e.df19 <- (3/4)*0.01*(e.Ws/Station$Ws)
Station$e.Rn.corr <- Station$Rn.corr*sqrt( ((0.05*Station$Rn)/Station$Rn)^2 + (e.df10/df10)^2 )

#Rn.corr conversion to MJ/m2 30min
Station$Rn.MJ.crop   <- Station$Rn.corr*3600*0.5*10^-6     #(W/m2)*(J/Ws)*(3600s/hr)*0.5(hr/30min) 
Station$e.Rn.MJ.crop <- Station$e.Rn.corr*3600*0.5*10^-6

Rn.MJ.daily.crop   <- aggregate(Station$Rn.MJ.crop, FUN = sum, list(Date = Station$date))
Rs.MJ.daily        <- aggregate(Station$Rs.MJ, FUN = sum, list(Date = Station$date))

attach(Station)
alpha.crop <- 1- ((Rns.MJ - Rn.MJ.grass + Rn.MJ.crop)/Rs.MJ)
albedo.crop <- data.frame(Station$date, Station$timestamp, ifelse(alpha.crop <0|alpha.crop>1, NA, alpha.crop))   #albedo between 0 and 1
colnames(albedo.crop) <- c("date", "timestamp", "albedo")

# Calculate the error in albedo calculation

#-------------------------------------------------------------------------------
#### END #######################################################################