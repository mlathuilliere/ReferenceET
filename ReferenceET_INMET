rm(list = ls())

#-------------------------------------------------------------------------------
##Input file location of .csv file for the climate station considered

Station <-
  read.table("C:/Users/Mike/Dropbox/Thesis/Aquarius/Poxoreo/INMET_Poxoreo_Aquarius3.csv", 
   header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)

#-------------------------------------------------------------------------------
##Input altitude, latitude and name of the climate station 

Location <- "Poxoreo Station"
z <- 450                                                                        #altitude in m
lat <- -15.83                                                                   #latitude in decimal degrees
Wheight <- 10                                                                   #height of the sensor measuring wind speed, in m

#-------------------------------------------------------------------------------
attach(Station)
                                                  
##Date conversions from the .csv file
Date <- as.character(Date)
Date <- as.Date(Date, "%m/%d/%Y")
J <- as.numeric(format(Date, "%j"))                                             #conversion to julian day 
                 
## List of variables, constants used in the calculation
uz <- Wspeed.mean                                                               #rename mean wind speed in m/s  
lambda <- ifelse(is.na(Tair.mean), 2.45, 2.501 - (2.361*0.001)*Tair.mean)       #Temperature dependent latent heat of vapourization (MJ/kg)
Cp <- 1.013*10^-3                                                               #Specific heat capacity at constant P (MJ/kg.degC)
sigma <- 4.903*10^-9                                                            #Boltzmann constant in MJ/K^4m^2day
u2 <- uz*(4.87/(log(67.8*Wheight-5.42)))                                        #Wind speed at 2m height calculated from uz, equation (47) 

#-------------------------------------------------------------------------------
##Calculation of the psychrometric constant 'gamm' (kPa/degC), equation (8)

gamm <- (Cp*P.mean*0.1)/(0.622*lambda)

#-------------------------------------------------------------------------------
##Calculation of the vapour pressure deficit (es - ea) (kPa)

##Calculation of the saturation vapour pressure 'es' (kPa), equation (11)

eo.Tmax <- 0.6108*exp(17.27*Tmax/(Tmax+237.3))
eoTmax <- signif(eo.Tmax, digits = 3)                                           ##keep 3 significant figures
eo.Tmin <- 0.6108*exp(17.27*Tmin/(Tmin+237.3))
eoTmin <- signif(eo.Tmin, digits = 3)                                           ##keep 3 significatn figures

es1 <- ifelse(is.na(eoTmax)|is.na(eoTmin),0.6108*exp(17.27*Tmean/(Tmean+237.3)),
   (eoTmax+eoTmin)/2)                                                           ##if Tmax or Tmin are missing, 'es' is calculated using Tmean, eq(12)
es <- signif(es1, digits = 3)                                                   ##keep 3 significant figures                                                                          

##Calculation of the actual vapour pressure 'ea' (kPa), equation (13) and equation (48)

ea1 <- ifelse(is.na(eoTmax)|is.na(RHmax)|is.na(RHmin), eoTmin,((eoTmin*RHmax/100)
   +(eoTmax*RHmin/100))/2)                                                      ##if Tmax, RHmax, RHmin missing, then assume Tmin = dewpoint temperature                               ##RHmean is determined by RHmax and RHmin, so eq(19) cannot be used
ea <- signif(ea1, digits = 2)                                                   ##keep 2 significant figures (due to RH)                                                                             

#Vapour pressure deficit (kPa)
   
Vap.deficit1 <- ifelse((es - ea)<0,0,(es-ea))     
Vap.deficit <- signif(Vap.deficit1, digits = 2)                                 ##keep 2 significant figures (due to ea)

#-------------------------------------------------------------------------------
##Calculation of the slope of the saturated vapour pressure curve, Delta (kPa/degC)

Delta1 <- 4098*(0.6108*exp((17.27*Tmean)/(Tmean+237.3)))/((Tmean+237.3)^2)
Delta <- signif(Delta1, digits = 3)                                             ##keep 3 significatn figures

#-------------------------------------------------------------------------------
##Calculation of incoming radiation to determine ET0 using data from meteorological stations

##Input longitude (fi) and latitude (lat)
fi1 <- (pi/180)*(lat)                                                           ##in rad, equation (22)
fi <- signif(fi1, digits = 4)                                                   ##keep 4 significant figures (from latitude)
Gsc <- 0.0820                                                                   ##Solar constant in MJ/m2m-1

#Calculation of inverse relative distance Earth-Sun, equation (23)
dr <- 1+0.033*cos(2*pi*J/365)                                                   

#Calculation of the solar declination, equation (24)
delta <- 0.409*sin(-1.39+(2*pi*J/365))                                          

#Calculation of sunset hour angle (rad), equation (26)
ws1 <- acos(-tan(fi)*tan(delta))
ws <- signif(ws1, digits = 4)                                                   ##keep 4 significant figures (from fi)

#Calculation of extraterrestial radiation, in MJ/m2day, equation (28)
Ra1 <- (24*60/pi)*Gsc*dr*(ws*sin(fi)*sin(delta)+cos(fi)*cos(delta)*sin(ws))
Ra <- signif(Ra1, digits = 4)                                                   ##keep 4 significant figures (from fi)

#Calculation of clear sky solar radiation, in MJ/m2day, equation (37)
Rso1 <- (0.75+z*2*10^-5)*Ra                                                     ##as and bs of equation (35) are unavailable
Rso <- signif(Rso1, digits = 4)                                                 ##keep 4 significant figures (from Ra)

#Calculation of incoming solar radiation using Tmax and Tmin, in MJ/m2day, equation (50)
Rs1 <- 0.16*Ra*(Tmax-Tmin)^0.5                                                  #Hargreaves radiation formula using 0.16 for interior regions
Rs <- signif(Rs1, digits = 3)                                                   ##keep 3 significant figures (from Temp)

#Calculation of net radiation from land surface albedo, in MJ/m2day, equation (38) 
Rns1 <- (1-0.23)*Rs                                                             ##albedo = 0.23 for reference crop
Rns <- signif(Rns1, digits = 3)                                                 ##keep 3 significant figures (from Rs)

#Calculation of net outgoing longwave radiation, in MJ/m2day, equation (39)
Tmax.K <- Tmax + 273.16                                                         #conversion deg.C to Kelvin
Tmin.K <- Tmin + 273.16                                                         #conversion deg.C to Kelvin
Rnl1 <- sigma*(((Tmax.K^4)+(Tmin.K^4))/2)*(0.34-0.14*(ea^0.5))*(1.35*(Rs/Rso)-0.35)
Rnl <- signif(Rnl1, digits = 2)                                                 ##keep 2 significant figures (from ea)

#Calculation of net shortwave radiation, in MJ/m2day, equation (40)
Rn <- Rns - Rnl                                                                 #for albedo = 0.23, reference crop

#-------------------------------------------------------------------------------
#Calculation of reference ET (ET0), in mm/day, equation (6)

ref.ET1 <- (0.408*Delta*(Rn-0)+ gamm*(900/(Tmean+273))*u2*Vap.deficit)/(Delta + gamm*(1+0.34*u2))
ref.ET <- signif(ref.ET1, digits = 2)                                           ##keep 2 significant figures (from ea)

Reference.Evap <- data.frame(Date, RHmean, Tmax, Tmin, Tmean, eoTmin, eoTmax, ea, ref.ET, Precip)

attach(Reference.Evap)
decades <- 485                                                                  #enter the number of decades based on the availble data
decade <- rep(1:decades, each=10)
df1 <- aggregate(ref.ET, by = list(decade), mean, na.rm = TRUE)
ET0.1a <- rep(df1$x, each = 10)
ET0.1 <- signif(ET0.1a, digits = 2)                                             ##keep 2 significant figures for ET0
df2 <- aggregate(ref.ET, by = list(decade), sd, na.rm = TRUE)
ET0stdev.1a <- rep(df2$x, each = 10)
ET0stdev.1 <- signif(ET0stdev.1a, digits = 2)                                   ##keep 2 significant figures for ET0

detach(Reference.Evap)

#Creation of the new data frame and a graph 
Reference.ET <- data.frame(Date, RHmin, RHmax, RHmean, Tmax, Tmin, Tmean, Rn, 
   u2, es, ea, Vap.deficit, ET0.1, Precip)

write.table(Reference.ET, file="C:/Users/Mike/Desktop/Poxoreo_decadal_ET0_3.txt", sep = ",", na = "", dec = ".", 
   row.names = FALSE, col.names = TRUE )

##Plotting of the data to check for calculation errors
par(mfrow=c(3,1), mar=c(2,2,2,2), oma=c(1.5,2,1.5,1))
plot(Date,ET0.1, ylab = "", main="Incoming Radiation albedo = 0.23", col="red")
plot(Date,Vap.deficit, ylab = "", main="Vapour Pressure Deficit (kPa)", col="blue")
plot(Date,ET0.1, ylab = "", main="ET0 (mm/day), albedo = 0.23")
title(Location, cex = 1.5, outer = TRUE)
par(mfrow=c(1,1))

##Export graph of time series of Rn, Vap.deficit, ETO
dev.print(pdf, 
  file="C:/Users/Mike/Desktop/Poxoreo_Station_ET0_3.pdf",
   width=7.5, height=10, pointsize=5)

detach(Station)

#-------------------------------------------------------------------------------
#### END #######################################################################
