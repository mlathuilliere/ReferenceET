# Reference ET calculation INMET

Calculation of reference ET (ET0) of as per FAO guidelines Irrigation and Drainage Paper 56  (FAO56)
http://www.fao.org/docrep/X0490E/x0490e00.htm#Contents
All equations are numbered according to the FAO56 numbering

Meteorological data is obtained from the Brazilian National Meteorological Institute (INMET) and
is focused on conventional stations only available at 
http://www.inmet.gov.br/portal/index.php?r=bdmep/bdmep

Data should have first been analyzed and manipulated to provie daily values (mean, max, min) into with an exported .csv file containing the following headings (stdev = standard deviation) with NA
for not available data:

Date              month/day/year
Tair mean         mean air temperature, in deg.C
Tair.stdev        standard deviation of mean air temperature
RHmean            mean relative humidity calculated at (RHmax + RHmin)/2, in %
RH.stdev          standard deviation of the mean relative humidity
P.mean            mean atmospheric pressure, in hPa
P.stdev           standard deviation of atmospheric pressure
Wspeed.mean       mean wind speed (record height of sensor), in m/s
Wspeed.stdev      standard deviation of wind speed
Cloudcover.mean   mean cloud cover, as a fraction of 10 (/10), 
Cloudcover.stdev  standard deviation of cloud cover
Tmax              maximum temperature over 24hr period, in deg.C
Tmin              minimum temperature over 24hr period, in deg.C
Precip            total precipitation over a 24hr period, in mm
Tmean             mean temperature calculated as (Tmax+Tmin)/2, in deg.C
RHmax             maximum humidity recorded at 8am
RHmin             minimum humidity recorded at 2pm


