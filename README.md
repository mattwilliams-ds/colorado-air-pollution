# colorado-air-pollution
A statistical analysis of air pollution data in Colorado, USA.

This analysis looks at air pollutants measured between 2000 and 2016 in Adams, Denver, and Jackson counties of Colorado.

In the MWprojectCorrelation.R file, I perform a correllation analysis looking for relationships between the four measured pollutants. Namely, nitrogen dioxide, sulfur dioxide, ozone, and carbon monoxide.

In the MWprojectAssociation.R file, I look for association rules. This is done by creating attributes for air quality index values that are either OK or HIGH as well as creating an attribute for flagging which season the measurements were taken in. The apriori algorithm looks for patterns of when pollutants were high and in what season it occured in.

Finally, in the MWprojectKmeans.R file, I perform a k-means clustering analysis of the data to determine how the pollutant levels cluster with respect to the months of the year. This file is still in development and will be updated as it is finished.

The data set is available from here: https://data.world/data-society/us-air-pollution-data
