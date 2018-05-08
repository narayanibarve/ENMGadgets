#' BatchMask - Crops (masks) the raster files with multiple shape file.
#' 
#' Noninteractive version. For interactive version refer \link{iBatchMask}
#' 
#' Function crops (masks) the raster files with multiple shape file and stores them in separate folders.
#' Masked files are stored in seperate folders with shape file name and names of the masked files are kept same. 
#' For example, if the input asc files are bio1.asc, bio2.asc, and you select multiple shape files like shp1.shp, shp2.shp, shp3.shp, then 
#' 3 folders named shp1, shp2 and shp3 will be created in output folder specified by user. Each folder will contain bio1.asc, bio2.asc
#' If by chance your files does not have any extension, then comment the statement below the note 'For file with extension'. 
#' and uncomment the FileName statement below the note 'For files without extension
#' @import raster
#' @import maptools
#' @param ASCfilelist - list of raster files to crop
#' @param SHPfilelist - mask shapefiles used to crop rasters 
#' @param OPDirName - Output folder, where subfolders of shape files will be created 
#' @examples \dontrun{
#' BatchMask()
#' }
#' @export
BatchMask <- function(ASCfilelist=NA, SHPfilelist=NA, OPDirName= NA)
{ 
  if(is.na(ASCfilelist)) {
    stop("Please specify ASCfilelist (ASCII files to crops) or use 
         iBatchMask for interactive version")
  }
  if(is.na(SHPfilelist)){
    stop("Please specify HPfilelist (shape files as crop mask) or use 
         iBatchMask for interactive version")
  }
  if (is.na(OPDirName)) {
    stop("Please specify OPDirName (output folder) or use 
         iBatchMask for interactive version")
  }
  d1 = length(SHPfilelist)
  for (k in 1:d1)
  {
    ShapeFile1 = SHPfilelist[k]
    print (paste("Current shape file is : ", ShapeFile1, sep = ""))
    CropData(ASCfilelist,as.character(ShapeFile1),as.character(OPDirName))
  }
}

CropData<-function(filelist, ShapeFile, DirName)
{
  Shp1 = readShapePoly(ShapeFile)
  ShapeName = basename(ShapeFile)
  ShapeDir = gsub(".shp","", ShapeName)

  OpDir = paste(dirname(DirName), basename(DirName), ShapeDir, sep = "/")
  if (dir.exists(OpDir) == FALSE)
  {
    dir.create(OpDir, recursive = TRUE)
  }
  for (i in 1:length(filelist))
  {
    r1 = raster(filelist[i])
    cr1 = crop(r1,Shp1)
    cr2 = mask(cr1,Shp1)
	FileName = basename(filelist[i])
    print(paste("Current ASC file is ", FileName, sep=""))
	writeRaster(cr2, paste(OpDir, FileName, sep ="/"), "ascii")
    image(cr2,asp=1)
  }
}
