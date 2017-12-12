#' iCropRaster - Crops (masks) the raster files with the shape file. (Interactive)
#' 
#' Function crops (masks) the raster files with the shape file. Interactive 
#' version. For noninteractive veriosn to be used in scripts check \link{CropRaster}
#' 
#' Masked files are stored in the current working directory with original name of 
#' file + the sufix provided.
#' For example, if the input asc files are bio1.asc, bio2.asc, and you give the 
#' suffix as 'n', then the output masked files are bio1n.asc, bio2n.asc
#' If by chance your files do not have any extension, then comment the statement 
#' below the note 'For file with extension' and uncomment the FileName statement 
#' below the note 'For files without extension
#' 
#' @import raster
#' @import maptools
#' @param filelist - list of raster files to crop
#' @param ShapeFile - mask shapefile used to crop rasters 
#' @param sufix - sufix for the output file name
#' @examples \dontrun{
#' iCropRaster()
#' }
#' @export
iCropRaster<-function(filelist=NA,ShapeFile=NA,sufix=NA)
{
  if(is.na(filelist)){
    filelist = choose.files(caption="Select ASCII files to crop: ")
  }
  if(is.na(ShapeFile)){
    ShapeFile = file.choose("Select shape file as crop mask: ")
  }
  if(is.na(sufix)){
    sufix = readline("Enter Sufix to output file name: ")
  }
  ext1 = sufix
  Shp1 = readShapePoly(ShapeFile)
  for (i in 1:length(filelist))
  {
    r1 = raster(filelist[i])
    cr1 = crop(r1,Shp1)
    cr2 = mask(cr1,Shp1)
    ### For file with extension. 
    FileName = paste(substr(filelist[i],1,nchar(filelist[i])-4),ext1,substr(filelist[i],nchar(filelist[i])-3,nchar(filelist[i])),sep="")
    ### For files without extension
    writeRaster(cr2,FileName, "ascii")
    plot(cr2)
    print(i)
  }
}
