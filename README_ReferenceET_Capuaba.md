# Reference ET Calculation Capuaba

Calculation of reference ET (ET0) as per FAO guidelines Irrigation and Drainage Paper 56 (FAO56) by Allen et al. (1998) http://www.fao.org/docrep/X0490E/x0490e00.htm#Contents, reviewed in Allen et al. (2011):

For hourly measurements (or less)
Short grass reference crop: albedo: 0.23, height: 0.12 m, surface resistance: 50 s/m
Tall grass reference crop: albedo: 0.23, height: 0.50 m, surface resistance: 30 s/m

For daily measurements surface resistance can increase to 70 s/m (short grass) and 45 s/m (tall grass)

All equations are numbered according to the FAO56 equation numbering.

The code calculates ET0 from measurements taken at Capuaba farm (Mato Grosso, Brazil) and delivers a graph of daily radiation balance and ET0 (exported as .pdf), as well as a table of daily data as a text file.

Important notes:

1) ET0.sg (mm/30min or mm/day) refers to evapotranspiration of the short grass reference surface
   ET0.tg (mm/30min or mm/day) refers to evapotranspiration of the tall grass reference surface
   In this calculation, these differences are minor (less than the measurement error)

2) The values of ET0 are calculated with error bars (e.ET0) obtained by propagating sensor measurement error obtained from the sensor specifications

   Sensors used:
    - Net radiation (Rn): Kipp & Zonene NRLite 2 (assume 5% error in measurement)
    - Meteorological data: Vaisala WXT520 meteorological stattion
      -Wind speed (Ws)        +/- 0.3 m/s
      -air temperature (Tair) +/- 0.3 oC
      -relative humidity (RH) +/- 4   %
    - Ground heat flux (G): Huksefflux and thermal plates (assume 10% error in measurement)
    - Solar radiation (Rs): LI-COR 200X with +/- 3% typical accuracy

Reference cited:

Allen, R.G. et al (2011) Evapotranspiration information reporting: I. Factors foverning measurement accuracy, Agricutural Water Management 98(6): 899-920

Allen, R.G. et al. (1998) Crop evapotranspiration: guidelines for computing crop water requirements, Food and Agriculture Organization of the United Nations,http://www.fao.org/docrep/X0490E/X0490E00.htm
   
Campbell, G.S., and Norman, J.M. (1998) An introduction to environmental biophysics, New York: Springer, pp 286.
