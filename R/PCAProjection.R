#' PCAProjection - PCA of Raster files
#' 
#' Performs PCSRaster and projects it on another set of rasters
#' 
#' Noninteractive version. 
#' 
#' @param BioStackFiles - ESRI ASCII grid files of predictor variables
#' @param LoadingFile - name of output file which stores loadings for components
#' @param CompImpFile - name of output file of the PCA summaries
#' @param ProjectonStackFiles - Future climate ESRI ASCII grid files or a list of 
#' lists for multiple projections
#' @param OutputFolder - Output folder or list of output folder names for storing 
#' the projection(s)
#' @return a summary of the PCA is returned as a structure
#' @examples \dontrun{
#' pcaop = PCAProjection()
#' }
#' @import raster
#' @importFrom stats na.omit prcomp
#' @export

PCAProjection <- function(BioStackFiles=NA,LoadingFile=NA,
                          CompImpFile=NA,ProjectonStackFiles=NA,
                          OutputFolder=NA)
{
  if(is.na(BioStackFiles[1])){
    stop("Please specify BioStackFiles (bioclimatic ASCII files)")
  }
  print(BioStackFiles)
  BioStack = MakeStack(BioStackFiles)
  if(is.na(LoadingFile)){
    stop("Please specify LoadingFile (PCA loading)")
  }
  if(is.na(CompImpFile)){
    stop("Please specify CompImpFile (PCA summary)")
  }  
  BioPt1 = rasterToPoints(BioStack)
  print("Generating principal component")
  pcaSummary = prcomp(na.omit(BioPt1[,3:dim(BioPt1)[2]]), center=TRUE, scale = TRUE)
  pcaScores = pcaSummary$x
  d1 = dim(pcaScores)
  for (i in 1:d1[2]) 
  {
    print(paste("Writing principal component file ", i ), sep = " ")
    tbl1 = cbind(BioPt1[,1:2], pcaScores[,i])
    ### Faster but has error sometimes. The problem lies with rasterFromXYZ function.
    # r2 = rasterFromXYZ(tbl1,digits=5)
    ### till here
    
    ## Slower but will work irrespective 
    XYTbl = tbl1[,1:2]
    r1 = BioStack[[i]]
    r2 = rasterize(XYTbl,r1,field=tbl1[,3])
    ## Till here
    
    FileName = paste("Comp",i,".asc", sep = "")
    writeRaster(r2,FileName)
  }
  
  write.table(pcaSummary$rotation, LoadingFile, row.names=T, col.names=T, sep = ",")
  
  ### Summary table for PCA 
  StdDev = pcaSummary$sdev
  ## Variance explained by each component
  VarExp = pcaSummary$sdev^2/sum(pcaSummary$sdev^2)
  # cumulative variance explained
  CumVar = cumsum(VarExp)
  ColNames = paste("PC", seq(1,length(StdDev)), sep = "")
  RowNames = c("Standard deviation", "Proportion of Variance", 
               "Cumulative Proportion")
  
  SumPCAMat = rbind(StdDev, VarExp, CumVar)
  
  write.table(SumPCAMat, CompImpFile, row.names=RowNames, col.names=ColNames, sep = ",")
  ### variance explained by each component
  ### pcaSummary$sdev^2 / sum(pcaSummary$sdev^2)
  
  ### find out how many components are required to get more than 95% 
  print(CumVar)
  i = which(CumVar >= 0.95)
  print(paste("First ", i[1], " components explains >= 95% of variance.", sep = ""))
  if(is.na(ProjectonStackFiles[1])){
    FutProject = readline("Do you want to project on different data? (Y/N) :")
    while (FutProject == "Y")
    {
      if (FutProject=="Y")
      {
        F1 = PredictOnNewData(pcaSummary,ProjectonStackFiles=NA,OutputFolder=NA)
        FutProject = readline("Do you want to project on different data? (Y/N) :")
      }
    }
  } else {
    if(class(ProjectonStackFiles)=="character"){
      F1 = PredictOnNewData(pcaSummary,ProjectonStackFiles,OutputFolder)
    } else {
      for(i in 1:length(ProjectonStackFiles)){
        F1 = PredictOnNewData(pcaSummary,ProjectonStackFiles[[i]],OutputFolder[i])
        
      }
    }
  }
  return(pcaSummary)
}

## pcaop = PCARaster()



##Change directory to projection data folder
## Stack data you would like to project to

PredictOnNewData <- function(pcaSummary=NA,ProjectonStackFiles=NA,OutputFolder=NA)
{
  if(is.na(ProjectonStackFiles[1])){
    stop("Please specify ProjectonStackFiles (bioclimatic ASCII files to predict) 
          or use iPCAProjection for interactive version")
  } 
  FutStack <- MakeStack(ProjectonStackFiles)
  
  if(is.na(OutputFolder)){
    stop("Please specify OutputFolder (Output folder for Projection files) or use 
         iPCAProjection for interactive version")
  } 
  if(is.na(pcaSummary[1])){
    print("Please supply summary output of PCARaster function")
    return(NULL)
  } 
  fut = rasterToPoints(FutStack)
  XYpts = fut[,1:2]
  FutData <- fut[,3:dim(fut)[2]]
  FutNames = names(FutData)
  PresNames <- names(pcaSummary[[4]])
  FutData <- data.frame(FutData)
  #names(FutData) = PresNames
  names(FutData) <- names(pcaSummary[[4]])
  #print(names(FutData))
  #readline()
  ##Predict using present data pca object and projection data set as newdata
  pcf2=predict(pcaSummary,newdata=FutData)
  
  ##Bring in a raster layer with the extent you would like to project to (same extent as the newdata)
  for (i in 1:length(FutStack@layers)) 
  {
    print(paste("Writing principal component file of projection", i ), sep = " ")
    tbl1 = cbind(XYpts[,1:2], pcf2[,i])
    
    ### Faster but has error sometimes. The problem lies with rasterFromXYZ function.
    # r2 = rasterFromXYZ(tbl1,digits=5)
    ### till here
    
    ## Slower but will work irrespective 
    XYTbl = tbl1[,1:2]
    r1 = FutStack[[1]]
    r2 = rasterize(XYTbl,r1,field=tbl1[,3])
    ## Till here
    
    FileName = paste(OutputFolder, "\\Comp",i,".asc", sep = "")
    writeRaster(r2,FileName)
  }
}