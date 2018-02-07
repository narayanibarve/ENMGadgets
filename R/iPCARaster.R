
#' iPCARaster - PCA of Raster files (Interactive)
#' 
#' Performs Principle Component Analysis of Raster objects and returns summary 
#' and loadings for the componetns. *Interactive version*. For noninteractive veriosn 
#' to be used in scripts check \link{PCARaster}
#' 
#' Main function to generate PCA for the selected bioclimatic layer and then save 
#' the pca components in ASCII format.
#' Run the function pcaop = iPCARaster(). This function will ask user to choose the 
#' bioclimatic ASCII files. pca components are stored as Comp1.asc, Comp2.asc...... and so on. 
#' This function retuns the pca of the ascii data supplied to do further processing 
#' like checking for eigen values, broken stick etc. 
#' 
#' @param BioStackFiles - ESRI ASCII grid files of predictor variables
#' @param LoadingFile - name of output file which stores loadings for components
#' @param CompImpFile - name of output file of the PCA summaries
#' @param OPfolder - name of output folder to save PCA component, loading and 
#' summary file
#' @return a summary of the PCA is returnd as a strcture
#' @examples \dontrun{
#' pcaop = iPCARaster()
#' }
#' @import raster
#' @importFrom utils choose.dir choose.files write.table
#' @importFrom stats prcomp na.omit
#' @export


iPCARaster <- function(BioStackFiles=NA,LoadingFile=NA,CompImpFile=NA,OPfolder=NA)
{
  if(is.na(BioStackFiles)){
    BioStackFiles = choose.files(caption="Select bioclimatic ASCII files : ")
  }
  BioStack = MakeStack(BioStackFiles)
  if(is.na(LoadingFile)){
    LoadingFile = readline("File name for PCA loading : ")
  }
  if(is.na(CompImpFile)){
    CompImpFile = readline("File name for PCA summary : ")
  }
  if(is.na(OPfolder)){
    OPfolder = choose.dir("Output folder to save PCA components : ")
  }  
  BioPt1 = rasterToPoints(BioStack)
  BioPt1 = BioPt1[complete.cases(BioPt1), ]
  #BioPt1 = rasterToPoints(BioStack)
  print("Generating principal component")
  pcaPt1 = prcomp(na.omit(BioPt1[,3:dim(BioPt1)[2]]), center=TRUE, scale = TRUE)
  pcaScores = pcaPt1$x
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
    
    FileName = paste(OPfolder, "/Comp",i,".asc", sep = "")
    writeRaster(r2,FileName)
  }
  write.table(pcaPt1$rotation, paste(OPfolder, "/", LoadingFile, sep="") , row.names=T, col.names=T, sep = ",")
  ### Summary table for PCA 
  StdDev = pcaPt1$sdev
  ## Variance explained by each component
  VarExp = pcaPt1$sdev^2/sum(pcaPt1$sdev^2)
  # cumulative variance explained
  CumVar = cumsum(VarExp)
  ColNames = paste("PC", seq(1,length(StdDev)), sep = "")
  RowNames = c("Standard deviation", "Proportion of Variance", "Cumulative Proportion")
  SumPCAMat = rbind(StdDev, VarExp, CumVar)
  write.table(SumPCAMat, paste(OPfolder, "/", CompImpFile, sep = ""), row.names=RowNames, col.names=ColNames, sep = ",")
  ### variance explained by each component
  ### pcaPt1$sdev^2 / sum(pcaPt1$sdev^2)
  
  ### find out how many components are required to get more than 95% 
  print(CumVar)
  i = which(CumVar >= 0.95)
  print(paste("First ", i[1], " components explains >= 95% of variance.", sep = ""))
  return(pcaPt1)
}

MakeStack <- function(Mfiles)
{
  #Mfiles = choose.files(caption=Prompt)
  for (i in 1: length(Mfiles))
  {
    fl1 = raster(Mfiles[i])
    if (i == 1)
    {
      stk = stack(fl1)
    }
    else
    {
      stk = stack(stk, fl1)
    }
  }
  return(stk)
}


