#' ModelThreshold - Thresholds Maxent suitability prediction based on user input. Output of the function is binary map of
#' predicted suitability as 1 and non suitable region as 0. 
#' 
#' Noninteractive version. 
#' 
#' Function thresholds the suitability predicted by Maxent or by any other niche modeling algorithm, provided that 
#' predicted file is in .asc format. Function needs 4 parameters, input file, which is prediction file. Occurrence file, 
#' this a occurrence file used in calibrating the model. Format of this file is, speciesname, Longitude, Latitude.
#' PercentThreshold - what percent of omission is agreeable while calibrating the model. This percent is suppose to be decided
#' depending upon how the occurrences are sampled. 
#' OutSuitFile - Final thresholded prediction will be stored in this file and the format of this file is .asc. Suitability
#' will be reclassified into 0 and 1. All the pixels above the thresholded value will be classified as 1 and the pixels below
#' threshold value will be classified as 0.  
#' @import raster
#' @param InSuitFile - Maxent prediction in .asc format
#' @param OccurrenceFile - Occurrences using which Maxent model is trained.
#' @param PercentThreshold - Percent of omission error, ranges between 0 - 1
#' @param OutSuitFile - Output file name. This file will be stored in the current working directory, if no path is given.
#' @examples \dontrun{
#' ModelThreshold()
#' }
#' @export

ModelThreshold <- function(InSuitFile=NA, OccurrenceFile=NA, PercentThreshold=NA, OutSuitFile=NA)
{
  if(is.na(InSuitFile)){
    stop("Please specify InSuitFile (Input raster)")
  }    
  if(is.na(OccurrenceFile)){
    stop("Please specify OccurrenceFile (occurrence table)")
  }
  if(is.na(PercentThreshold)){
    stop("Please specify PercentThreshold (threshold between 0 to 1)")
  } 
  if(is.na(OutSuitFile)){
    stop("Please specify OutSuitFile (output file name)")
  }   
  InRast = raster(InSuitFile)
	plot(InRast)
	Occur = read.table(OccurrenceFile, header=T, sep =",")
	Occur = Occur[,-1]
	ExtRast = extract(InRast, Occur)
	Occur1 = sort(ExtRast)
    RclVal = Occur1[round(length(Occur1) * PercentThreshold) + 1]
	rc = reclassify(InRast, c(0,RclVal,0, RclVal,1,1))
	writeRaster(rc, OutSuitFile)
	plot(rc)
}
