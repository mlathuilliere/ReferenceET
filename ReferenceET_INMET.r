
library(openair)

rm(list = ls())

#-------------------------------------------------------------------------------
## Input/output file paths

input.path  <- "D:/Thesis MSC/Aquarius/Canarana/INMET_Canarana_1999_2015.csv"
output.path.graph <- "C:/users/Mike/Desktop/Reference_ET.pdf"
output.path.table <- "C:/users/Mike/Desktop/Reference_ET.txt"

#-------------------------------------------------------------------------------

Station <-  read.table(input.path, header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)

#-------------------------------------------------------------------------------
##Input altitude, latitude and name of the climate station 

Location <- "Canarana Station"
z       <- 430                  #altitude in m
lat     <- -13.47               #latitude in decimal degrees
Wheight <- 10                   #height of the sensor measuring wind speed, in m
start.date <- as.Date("2000/01/01", format = "%Y/%m/%d")
end.date   <- as.Date("2013/12/31", format = "%Y/%m/%d")
  
#-------------------------------------------------------------------------------
# Date conversion

attach(Station)
Date <- as.Date(Station$Date, "%m/%d/%Y")
Station$date <- as.Date(Station$Date, "%m/%d/%Y")
J    <- as.numeric(strftime(Station$date, format = "%j"))     #conversion to julian day of year

#-------------------------------------------------------------------------------                 
# List of variables, constants used in the calculation

uz     <- Wspeed.mean                                                     #rename mean wind speed in m/s  
lambda <- ifelse(is.na(Tair.mean), 2.45, 2.501 - (2.361*0.001)*Tair.mean) #Temperature dependent latent heat of vapourization (MJ/kg)
Cp     <- 1.013*10^-3                                                     #Specific heat capacity at constant P (MJ/kg.degC)
sigma  <- 4.903*10^-9                                                     #Boltzmann constant in MJ/K^4m^2day
u2     <- uz*(4.87/(log(67.8*Wheight-5.42)))                              #Wind speed at 2m height calculated from uz, equation (47) 

#-------------------------------------------------------------------------------
## Calculation of the psychrometric constant 'gamm' (kPa/degC), equation (8)

gamm <- (Cp*P.mean*0.1)/(0.622*lambda)

#-------------------------------------------------------------------------------
## Calculation of the vapour pressure deficit (es - ea) (kPa)

# Calculation of the saturation vapour pressure 'es' (kPa), equation (11)
eoTmax <- signif(0.6108*exp(17.27*Tmax/(Tmax+237.3)), digits = 3)        ##keep 3 significant figures
eoTmin <- signif(0.6108*exp(17.27*Tmin/(Tmin+237.3)), digits = 3)        ##keep 3 significant figures

es <- signif(ifelse(is.na(eoTmax)|is.na(eoTmin),0.6108*exp(17.27*Tmean/(Tmean+237.3)),
   (eoTmax+eoTmin)/2), digits = 3)          ##if Tmax or Tmin are missing, 'es' is calculated using Tmean, eq(12)
                                            ##keep 3 significant figures                                                                          

# Calculation of the actual vapour pressure 'ea' (kPa), equation (13) and equation (48)
ea <- signif(ifelse(is.na(eoTmax)|is.na(RHmax)|is.na(RHmin), eoTmin,((eoTmin*RHmax/100)
   +(eoTmax*RHmin/100))/2), digits = 3)     ##if Tmax, RHmax, RHmin missing, then assume Tmin = dewpoint temperature                               ##RHmean is determined by RHmax and RHmin, so eq(19) cannot be used
                                            ##keep 2 significant figures (due to RH)                                                                             

# Vapour pressure deficit (kPa)
VPD <- signif(ifelse((es - ea)<0,0,(es-ea)), digits = 2)     ##keep 2 significant figure (due to ea)

#-------------------------------------------------------------------------------
##Calculation of the slope of the saturated vapour pressure curve, Delta (kPa/degC)

Delta <- signif(4098*(0.6108*exp((17.27*Tmean)/(Tmean+237.3)))/((Tmean+237.3)^2), digits = 3)
                                            ##keep 3 significant figures

#-------------------------------------------------------------------------------
## Calculation of incoming radiation to determine ET0 using data from meteorological stations

# Input longitude (fi) and latitude (lat)
fi  <- signif((pi/180)*(lat), digits = 4)   ##in rad, equation (22), keep 4 significant figures (from latitude)
Gsc <- 0.0820                               ##solar constant in MJ/m2m-1

# Calculation of inverse relative distance Earth-Sun, equation (23)
dr <- 1+0.033*cos(2*pi*J/365)                                                   

# Calculation of the solar declination, equation (24)
delta <- 0.409*sin(-1.39+(2*pi*J/365))                                          

# Calculation of sunset hour angle (rad), equation (26)
ws <- signif(acos(-tan(fi)*tan(delta)), digits = 4)   ##keep 4 significant figures (from fi)
                                               
# Calculation of extraterrestial radiation, in MJ/m2day, equation (28)
Ra <- signif((24*60/pi)*Gsc*dr*(ws*sin(fi)*sin(delta)+cos(fi)*cos(delta)*sin(ws)), digits = 4)

# Calculation of clear sky solar radiation, in MJ/m2day, equation (37)
Rso <- signif((0.75+z*2*10^-5)*Ra, digits = 4)       ##as and bs of equation (35) are unavailable
                                                     ##keep 4 significant figures (from Ra)                                               
                                                 
# Calculation of incoming solar radiation using Tmax and Tmin, in MJ/m2day, equation (50)
Rs <- signif(0.16*Ra*(Tmax-Tmin)^0.5, digits = 3)    ##Hargreaves radiation formula using 0.16 for interior regions
                                                     ##keep 3 significant figures (from Temp)

# Calculation of net radiation from land surface albedo, in MJ/m2day, equation (38) 
Rns <- signif((1-0.23)*Rs, digits = 3)               ##albedo = 0.23 for reference crop
                                                     ##keep 3 significant figures (from Rs)

# Calculation of net outgoing longwave radiation, in MJ/m2day, equation (39)
Tmax.K <- Tmax + 273.16                              ##conversion deg.C to Kelvin
Tmin.K <- Tmin + 273.16                              ##conversion deg.C to Kelvin
Rnl <- signif(sigma*(((Tmax.K^4)+(Tmin.K^4))/2)*(0.34-0.14*(ea^0.5))*(1.35*(Rs/Rso)-0.35), digits = 2)
                                                     ##keep 2 significant figures (from ea)

# Calculation of net shortwave radiation, in MJ/m2day, equation (40)
Rn <- Rns - Rnl                                      ##for albedo = 0.23, reference crop

#-------------------------------------------------------------------------------
## Calculation of reference ET (ET0), in mm/day, equation (6)

ET0 <- signif((0.408*Delta*(Rn-0)+ gamm*(900/(Tmean+273))*u2*VPD)/(Delta + gamm*(1+0.34*u2)), digits = 2)
                                         ##keep 2 significant figures (from ea)

# Creation of a new data frame with all variables to calculate ET0
attach(Station)

Reference.ET <- data.frame(Date, RHmean, Tmax, Tmin, Tmean, eoTmin, eoTmax, ea, VPD, Precip, ET0)


# Calculate decadal average following FAO56
Reference.ET$date <- as.Date(Reference.ET$Date, "%Y-%m-%d")

# use date selection
Reference.ET  <- selectByDate(Reference.ET, start = start.date, end = end.date)

Reference.ET.10day <- timeAverage(Reference.ET, avg.time = "10 day", statistic = "mean")
test <- vector()
for (i in (1:length(Reference.ET.10day$ET0))) {
  df1 <- rep(Reference.ET.10day$ET0[i], 10)
  test <- append(test, df1)
}

# Combine decadal average data to the Reference.ET data frame
Reference.ET$ET0.10 <- test[1:length(Reference.ET$ET0)]

detach(Station)

#-------------------------------------------------------------------------------
# Export data to table
write.table(Reference.ET, file=output.path.table, sep = ",", na = "", dec = ".", row.names = FALSE, col.names = TRUE )

#-------------------------------------------------------------------------------
## Plot data to check for anomalies in the time series, Precipitation, Rn, VPD, ET0

attach(Reference.ET)
par(mfrow = c(4,1), mar=c(2,4,2,1), oma = c(3,2,1,1))
plot(Reference.ET$Date, Precip, ylab = "", xlab = "", xaxt="n", xaxs = "i", type="h")
mtext(expression(paste("PPT (mm ", d^{-1}, ")", sep = "")), side = 2, line = 3, cex = 0.75)
axis.Date(1, at=seq(Date[1], max(Date), by="years"), format = "%Y", labels = TRUE)
plot(Date, Rn, ylab = "", xlab = "", xaxt="n", xaxs = "i")
mtext(expression(paste("Rn (MJ", " m"^{-2}, "d"^{-1}, ")", sep = "")), side = 2, line = 3, cex = 0.75)
axis.Date(1, at=seq(Date[1], max(Date), by="years"), format = "%Y", labels = TRUE)
plot(Date,VPD, ylab = "", xlab = "", xaxt="n", xaxs = "i", col="blue")
mtext("VPD (kPa)", side = 2, line = 3, cex = 0.75)
axis.Date(1, at=seq(Date[1], max(Date), by="years"), format = "%Y", labels = TRUE)
plot(Date,ET0, ylab = "", xlab = "", xaxt="n", xaxs = "i", col="red")
mtext(expression(paste("ET"[0], " (mm d"^{-1}, ")", sep ="")), side = 2, line = 3, cex = 0.75)
axis.Date(1, at=seq(Date[1], max(Date), by="years"), format = "%Y", labels = TRUE)
title(Location, cex = 1.5, outer = TRUE)
par(mfrow=c(1,1))
detach(Reference.ET)

##Export graph 
dev.print(pdf, file=output.path.graph, width=7.5, height=10, pointsize=5)

#-------------------------------------------------------------------------------
#### END #######################################################################
