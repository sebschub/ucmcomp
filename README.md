ucmcomp
=======

[EDIT MD] Just checking whether this works...

This repository includes scripts to compare RCM output with BUBBLE
data. In particular, different UCM will be tested.

The actual data is not included.

Requirements
------------

R ≥ 3.1.0 with the following packages
* ggplot2
* reshape2
* dplyr
* tidyr
* RNetCDF

-------------------------------------------------------------------------------

readmeasdata.R
--------------

Put BUBBLE data into data frame for analysis.


readSebastian.R
---------------

Read Sebastian's simulation data and put it into data frame.


netcdffunc.R
------------

Some tool functions for reading CCLM netCDF output.


sitecoord.R
-----------

Coordinates of measurement sites.


CCLMcalc.R
----------

Calculations applied to raw CCLM data


comparison.R
------------

Main comparison of measurements with simulation.
