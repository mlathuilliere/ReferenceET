# ReferenceET_Calculation

Calculation of reference ET (ET0) as per FAO guidelines Irrigation and Drainage Paper 56  (FAO56)
http://www.fao.org/docrep/X0490E/x0490e00.htm#Contents
All equations are numbered according to the FAO56 equation numbering

The code delivers a graph of daily radiation balance and ET0 (exported as .pdf), as well as a table of 30-min data as a text file.

Important notes:

1) ET0 (mm/30min or mm/day) refers to evapotranspiration of a reference surface defined as 0.12 m
   highwell watered grass, with surface resistance of 70 s/m and an albedo of 0.23.
   Based on the position of your net radiometer, you might be able to determine the albedo of your
   surface. E.g. if your net radiometer is installed above a crop canopy then net radiation of grass    Rn(grass) is Rn(grass) = Rs(alpha(crop) + alpha(grass)) + Rn(crop)
   where Rs is the incoming shortwave radiation measured by a pyranometer, alpha are the albedos of
   the crop (needed) and the grass reference (= 0.23 accoding to FAO56)

2) The values of ET0 are calculated with error bars (e.ET0) obtained by propagating sensor
   measurement error obtained from the sensor specifications

   Sensors used:
    - Net radiation (Rn): Kipp & Zonene NRLite 2 (assume 5% error in measurement)
    - Meteorological data: Vaisala WXT520 meteorological stattion
      -Wind speed (Ws)        +/- 0.3 m/s
      -air temperature (Tair) +/- 0.3 oC
      -relative humidity (RH) +/- 4   %
    - Ground heat flux (G): Huksefflux and thermal plates (assume 10% error in measurement)
    - Solar radiation (Rs): LI-COR 200X with +/- 3% typical accuracy