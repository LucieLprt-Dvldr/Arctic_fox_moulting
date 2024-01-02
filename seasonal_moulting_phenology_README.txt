DATASET FOR SEASONAL COAT MOULTING PHENOLOGY OF ARCTIC FOXES

Data for: 
A camera trap based assessment of climate-driven phenotypic plasticity of seasonal moulting in an endangered carnivore.
Lucie Laporte-Devylder, Kristine R. Ulvund, Lars Rød-Eriksen, Ola Olsson, Øystein Flagstad, Arild Landa, Nina E. Eide, Craig R. Jackson.

Contact: lucie.lprt@gmail.com

The dataset "seasonal moulting phenology.csv" is the characterization of the spring moulting phenology of the observed arctic foxes in Snøhetta, between the years 2011 and 2018, and in relation to seasonal values of temperature, two snow parameters and rodent density index.
This dataset was used in a linear mixed model (LMM) to investigate the phenotypic plasticity of arctic foxes in response to environmental conditions, and was built based on pictures extracted from motion-triggered camera traps installed on supplementary feeders nearby den sites and release sites, 
as part of the Norwegian Institute for Nature Reserach's (NINA) Arctic Fox Captive Breeding and Release Programme.

For every arctic fox, each row represents a time point corresponding to a single moulting event. Whether the moult progression of an artic fox was observed from initiation to completion (95 to 0), or only in parts (e.g. 25 to 0) is specified by the ID of each arctic fox. 

Columns explanation:
year = year of observation
rodent = rodent cycle phase reflecting abundance of locally occuring Lemmus, Myodes and Microtus species. Index values range from 1 to 4 (1 – low, 2 – increasing, 3 – peak, 4 – decreasing; see Angerbjörn et al., 2013 for details). 
temperature = average seasonal air temperature for the moulting season (April-July) for all sites, in °C.
snow_depth = Site-specific values of snow depth present on May 1st, in mm.
snow_continuous = Site-specific values of the number of days with continuous snow on the ground from January 1st.
site = location of the camera within the study area.
morph = coat colour morph of the arctic fox (W = white, B = blue)
indiv_ID = unique identifier attributed to the observed arctic foxes based on physical characteristics.
moult_score = progression stage of the recorded moulting event. Numbers correspond to categories of "% winter coat", and can take the value 95 (moult initiating), 75, 50, 25, or 0 (moult completed, full summer coat).
date = date of the observed transition to a given moulting stage, in Julian day (January 1st = 1, December 31st = 366 in 2012 and 2016, 365 all other years)