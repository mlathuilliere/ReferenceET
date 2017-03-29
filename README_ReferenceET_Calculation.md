# Reference ET Calculation Capuaba

Calculation of reference ET (ET0) as per FAO guidelines Irrigation and Drainage Paper 56 (FAO56) http://www.fao.org/docrep/X0490E/x0490e00.htm#Contents
All equations are numbered according to the FAO56 equation numbering.

The code calculates ET0 from measurements taken at Capuaba farm (Mato Grosso, Brazil) and delivers a graph
of daily radiation balance and ET0 (exported as .pdf), as well as a table of daily data as a text file.

Important notes:

1) ET0 (mm/30min or mm/day) refers to evapotranspiration of a reference surface defined as a well-watered
grass of 0.12 m height, with surface resistance of 70 s/m and an albedo of 0.23. 

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

Allen et al. (1998)
Campbell and Norman (1998)