###r code for ndvi trend analysis###

#load required packages
library(raster)
library(spatialEco)
library(sp)
library(rgdal)

#set working directory
setwd("C:/Users/ToughBook/Documents/Katherine/Landsat/r_analysis/landsat")

#read in raster files
#note that I created null rasters for missing years (1985, 1988, 2011, and 2012) using the set null tool in ArcGIS - these null rasters are labelled with the year and then the characters "0000" instead of mmdd
#note that there are two rasters per year when the date in that year falls after August 11 - the raster with 0000 instead of mmdd is full of NA values so that you can calculate the trends with and without post-August 11 rasters included

ndvi_19840731 <- raster("final_adjusted_ndvi/unrestricted/ndvi_19840731.tif")
ndvi_19850000 <- raster("final_adjusted_ndvi/unrestricted/ndvi_19850000.tif")
ndvi_19860000 <- raster("final_adjusted_ndvi/restricted/ndvi_19860000.tif")
ndvi_19870816 <- raster("final_adjusted_ndvi/unrestricted/ndvi_19870816.tif")
ndvi_19880000 <- raster("final_adjusted_ndvi/unrestricted/ndvi_19880000.tif")
ndvi_19890729 <- raster("final_adjusted_ndvi/unrestricted/ndvi_19890729.tif")
ndvi_19900716 <- raster("final_adjusted_ndvi/unrestricted/ndvi_19900716.tif")
ndvi_19910811 <- raster("final_adjusted_ndvi/unrestricted/ndvi_19910811.tif")
ndvi_19920728 <- raster("final_adjusted_ndvi/unrestricted/ndvi_19920728.tif")
ndvi_19930816 <- raster("final_adjusted_ndvi/unrestricted/ndvi_19930816.tif")
ndvi_19940803 <- raster("final_adjusted_ndvi/unrestricted/ndvi_19940803.tif")
ndvi_19950730 <- raster("final_adjusted_ndvi/unrestricted/ndvi_19950730.tif")
ndvi_19960817 <- raster("final_adjusted_ndvi/unrestricted/ndvi_19960817.tif")
ndvi_19970719 <- raster("final_adjusted_ndvi/unrestricted/ndvi_19970719.tif")
ndvi_19980722 <- raster("final_adjusted_ndvi/unrestricted/ndvi_19980722.tif")
ndvi_19990717 <- raster("final_adjusted_ndvi/unrestricted/ndvi_19990717adj.tif")
ndvi_20000804 <- raster("final_adjusted_ndvi/unrestricted/ndvi_20000804adj.tif")
ndvi_20010729 <- raster("final_adjusted_ndvi/unrestricted/ndvi_20010729adj.tif")
ndvi_20020817 <- raster("final_adjusted_ndvi/unrestricted/ndvi_20020817adj.tif")
ndvi_20030720 <- raster("final_adjusted_ndvi/unrestricted/ndvi_20030720.tif")
ndvi_20040729 <- raster("final_adjusted_ndvi/unrestricted/ndvi_20040729.tif")
ndvi_20050817 <- raster("final_adjusted_ndvi/unrestricted/ndvi_20050817.tif")
ndvi_20060719 <- raster("final_adjusted_ndvi/unrestricted/ndvi_20060719.tif")
ndvi_20070722 <- raster("final_adjusted_ndvi/unrestricted/ndvi_20070722.tif")
ndvi_20080717 <- raster("final_adjusted_ndvi/unrestricted/ndvi_20080717.tif")
ndvi_20090727 <- raster("final_adjusted_ndvi/unrestricted/ndvi_20090727.tif")
ndvi_20100723 <- raster("final_adjusted_ndvi/unrestricted/ndvi_20100723.tif")
ndvi_20110000 <- raster("final_adjusted_ndvi/unrestricted/ndvi_20110000.tif")
ndvi_20120000 <- raster("final_adjusted_ndvi/unrestricted/ndvi_20120000.tif")
ndvi_20130807 <- raster("final_adjusted_ndvi/unrestricted/ndvi_20130807adj.tif")
ndvi_20140803 <- raster("final_adjusted_ndvi/unrestricted/ndvi_20140803adj.tif")
ndvi_20150806 <- raster("final_adjusted_ndvi/unrestricted/ndvi_20150806adj.tif")
ndvi_20160714 <- raster("final_adjusted_ndvi/unrestricted/ndvi_20160714adj.tif")
ndvi_20170811 <- raster("final_adjusted_ndvi/unrestricted/ndvi_20170811adj.tif")
ndvi_20180000 <- raster("final_adjusted_ndvi/restricted/ndvi_20180000adj.tif")
ndvi_20190716 <- raster("final_adjusted_ndvi/unrestricted/ndvi_20190716adj.tif")

#create stack of rasters for all available years
final_stack <- stack(ndvi_19840731, ndvi_19850000, ndvi_19860000, ndvi_19870816, ndvi_19880000, ndvi_19890729, ndvi_19900716, ndvi_19910811, ndvi_19920728, ndvi_19930816, ndvi_19940803, ndvi_19950730, ndvi_19960817, ndvi_19970719, ndvi_19980722, ndvi_19990717, ndvi_20000804, ndvi_20010729, ndvi_20020817, ndvi_20030720, ndvi_20040729, ndvi_20050817, ndvi_20060719, ndvi_20070722, ndvi_20080717, ndvi_20090727, ndvi_20100723, ndvi_20110000, ndvi_20120000, ndvi_20130807, ndvi_20140803, ndvi_20150806, ndvi_20160714, ndvi_20170811, ndvi_20180000, ndvi_20190716)

#create stack for just the last 10 years to compare to Betula spp. growth trends (with Aug. 17 as cutoff)
final_stack_2009to2019 <- stack(ndvi_20090727, ndvi_20100723, ndvi_20110000, ndvi_20120000, ndvi_20130807, ndvi_20140803, ndvi_20150806, ndvi_20160714, ndvi_20170811, ndvi_20180000, ndvi_20190716)

#run theil-sen slope estimator for each stack
final_theilsen_output <- raster.kendall(x = final_stack, intercept = TRUE, p.value = TRUE, confidence = TRUE, tau = TRUE)
final_2009to2019_theilsen_output <- raster.kendall(x = final_stack_2009to2019, intercept = TRUE, p.value = TRUE, confidence = TRUE, tau = TRUE)

#export theilsen slopes as .tif files
writeRaster(final_theilsen_output$layer.1, filename = "C:/Users/ToughBook/Documents/Katherine/Landsat/r_analysis/landsat/r_trend_output/ndvi_final_theilsen_slope.tif", format = "GTiff")
writeRaster(final_2009to2019_theilsen_output$layer.1, filename = "C:/Users/ToughBook/Documents/Katherine/Landsat/r_analysis/landsat/r_trend_output/ndvi_final_2009to2019_theilsen_slope.tif", format = "GTiff")

#export theilsen p-values as .tif files
writeRaster(final_theilsen_output$layer.2, filename = "C:/Users/ToughBook/Documents/Katherine/Landsat/r_analysis/landsat/r_trend_output/ndvi_final_theilsen_pvalue.tif", format = "GTiff")
writeRaster(final_2009to2019_theilsen_output$layer.2, filename = "C:/Users/ToughBook/Documents/Katherine/Landsat/r_analysis/landsat/r_trend_output/ndvi_final_2009to2019_theilsen_pvalue.tif", format = "GTiff")
