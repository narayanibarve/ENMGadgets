#' BatchMaxent - A function to execute Maxent niche modeling algorithm 
#' for multiple projections and multiple species. (Noninteractive)
#' 
#' Function BatchMaxent executes Maxent algorithm using dismo and it gives the flexibility of projecting the model to multiple projection
#' areas. This process is done iteratively if the species list contains more than on species. To run this function successfully specific
#' directory structure should be maintained. 
#'
#' In the projpath parameter, specify the path where the data is residing. MNameSameAsSpecies parameter is also required. This parameter
#' is set to TRUE when M_folderName in the Calibration folder is same as species name. And is set to FALSE when M_folderName is different than
#' species name. This scenerio exists, when a single M_folderName can be used for multiple species for calibration. This in this case, specify
#' M_folderName in species list file. arglist parameter is a list of arguments to be passed to Maxent. See documentation of Maxent for more details.
#'
#' See the description below for projpath directory structure. 
#' Within the project path directory, there should be one species list file in .csv format and 3 subfolders as mentioned below. 
#' Species list file should be passed as parameter in parameter named InpFlName. The structure of this species list file is 
#' SpeciesName, M_folderName. Species name is the name of the file which contains species occurrences and this occurrence
#' file should be stored in Occurrences folder. M_folderName is the name of the directory which contains the background environmental data
#' for model calibration. If M_folderName is same as SpeciesName, then set the parameter MNameSameAsSpecies to TRUE. In that case M_folderName
#' column in species list file can be ignored. The structure of the directory should be as given below.
#' 1) Calibration - This folder should contain subfolders with name as specified in M_folder name in the species list file. These subfolders
#' must contain the background environmental data. 2)Occurrences - This folder should contain occurrence data for each species in seperate file
#' with species name as mentioned in species list file. 3) Projections - This folder should contain the background environmental data for projections
#' and data should be stored in subfolders, so if one has to project it only to one area, still data should go in subfolder. Name of the subfolder
#' can be user choice.
#' Output will be stored in ModelOutput folder within project path. This ModelOutput folder will contain subfolders CaliOutput, which contains
#' output of model calibration, for each species there are 3 files, one is model prediction, standard deviation of the bootstrapped model, and 
#' variable contribution. Other subfolders are projection folders with 2 files per species, model prediction, and standard deviation of bootstrapped model
#' @import dismo
#' @import rgdal
#' @param projpath - Project path where all the required files for model calibrations are stored. e.g. "c:/SpDist/Model"
#' @param InpFlName - Name of the input file containing SpeciesName (OccurrenceFileName), M_folderName
#' @param MNameSameAsSpecies - Logical variable, to designate, if the M_folderName is same as OccurrenceFileName or not.  
#' @param arglist - List of arguments to be passed to maxent for model fitting and prediction.  
#' @examples \dontrun{
#' BatchMaxent()
#' }
#' @importFrom utils read.csv read.table write.table
#' @importFrom stats sd
#' @export


BatchMaxent <- function(projpath=NA, InpFlName=NA, MNameSameAsSpecies=FALSE, arglist=NA)
{
    Valid = TRUE
    if (is.na(projpath)){
      stop("Please specify projpath (the project path) or use iBatchMaxent for interactive version")
    }
	
	if (!file.exists(projpath))
	{
	   print(paste("No folder name ", projpath, sep = ","))
	   Valid = FALSE
	} else {
		OccuFolder = paste(projpath,"/Occurrences", sep = "")
	    ProjFolder = paste(projpath,"/Projections", sep = "")
	    CaliFolder = paste(projpath,"/Calibration", sep = "")
    }
	
	if (!(file.exists(OccuFolder) & file.exists(ProjFolder) & file.exists(CaliFolder)))
	{ 
	   print("Check folder names within project path.")
	   Valid = FALSE
	}
## Get input file name in case species name and M folder name is not same	
	if (is.na(InpFlName) & (MNameSameAsSpecies == FALSE) )
	{ 
	  stop("Please specify InpFlName (file name with path for calibration area and 
	       species connection) or use iBatchMaxent for interactive version")
	}

## if the file name is supplied and Speccies name and M folder name is not same. 	
	if (!file.exists(InpFlName) & (MNameSameAsSpecies == FALSE) )
	{ 
	   print("Check folder names within project path.")
	   Valid = FALSE
    }	
	
	if (Valid == TRUE)
	{
		setwd(projpath)
		### if multiple species have same name then generate a file for mapping species name, m folder name
		if (MNameSameAsSpecies == FALSE)
		{
		  splist=read.csv(InpFlName, header=T, sep = ",")
		} else	{
		  la = list.files(OccuFolder)
		  l1 = strsplit(la,".csv")
		  l2 = unlist(l1)
		  splist = cbind(l2,l2)
		}
		
		StartNo = 1
		EndNo = dim(splist)[1]
		timemat = matrix(0, nrow=EndNo*2, ncol = 3)
		k = 1
		
	    for(i in StartNo:EndNo)
	    {
		   ptm = proc.time()
		   species = as.character(splist[i,1])
		   print(paste("Current species ", species, sep = ""))
		   files <- list.files(path=paste(CaliFolder, "/", splist[i,2], sep=""), pattern='asc', full.names=TRUE )
		   
		   Caldir=paste(projpath, "/ModelOutput/CaliOutput", sep="")

		   ### Create model output directory		  
		   if (!file.exists(Caldir))
		   {
		      dir.create(Caldir, recursive=TRUE)
		   }	   
		   
		   predictors <- stack(files)
		   print("Generated predictors....")	  
		   ###  Get the occurrences
		   file1 <- paste(OccuFolder,"/",species,".csv",sep="")
		  
		   occur <- read.table(file1, header=TRUE, sep=',')
		   occur <- occur[,-1]
		   names(occur)<-c("lon","lat")

		   print("Fitting model...")
		   xm1 <- maxent(predictors, occur, args=arglist)												   
			  
	###### Predict on the calibration data
	       print("Predicting on calibration data...")
		   CaliPred = predict(xm1,predictors)

	############ CALIBRATION
		   ### Get the average of Model calibration and save
		   CaliAvg = mean(CaliPred)
		   OpCaliName = paste(Caldir, "/", species,"_avg.asc", sep="")
		   writeRaster(CaliAvg, OpCaliName)

		   ### Get the Standard Deviation of Model calibration and save
		   CaliStd = calc(CaliPred, fun=sd)
		   OpSDCaliName = paste(Caldir, "/", species,"_stddev_avg.asc", sep="")
		   writeRaster(CaliStd, OpSDCaliName)
		   
	############ VARIABLE CONTRIBUTION
		   ### write the variable contribution 
		   OpFlName = paste(Caldir, "/", species,"_VarContri.txt", sep="")
		   ModelResults = xm1@results
		   RowEnd = 6 + (length(files) * 2)
		   VarContri = ModelResults[7:RowEnd,]
		   write.table(VarContri, OpFlName, row.names=T,col.names=T, sep =",", quote=FALSE)
		   
	#### Project the model result from here	.
		   
		   modlist = list.dirs(ProjFolder, recursive=FALSE, full.names = FALSE)
		   for (j in 1:length(modlist))
		   {
			  print("Predicting on projection data...")
			  print(modlist[j])
		#### Set up the output directory as per the model.
              moddir=paste(projpath, "/ModelOutput/", modlist[j], sep="")
				 
		### Generate projection files list
              pfiles <- list.files(path=paste(ProjFolder, "/", modlist[j], sep=""), pattern='asc', full.names=TRUE )


	### Create model output directory		  
			  if (!file.exists(moddir))
			  {
				dir.create(moddir, recursive=TRUE)
			  }
		### Create project files stack
			  projectors <- stack(pfiles)

		### Project the model to future scenarios
			  ProjPred = predict(xm1,projectors)
			  print("Model fitting and prediction done. Now writing the files")


	############ PROJECTION		  
			  ### Get the average of projection on future data and save
			  ProjAvg = mean(ProjPred)
			  OpProjName = paste(moddir, "/", species,"_", modlist[j],"_avg.asc", sep ="")
			  writeRaster(ProjAvg, OpProjName)		  
			  
			  ### Get the Standard Deviation of projection on future data and save
			  ProjStd = calc(ProjPred, fun=sd)
			  OpSDProjName = paste(moddir, "/", species,"_", modlist[j],"_stddev_avg.asc", sep ="")
			  writeRaster(ProjStd , OpSDProjName)		  

		  } ### For Modlist
		  print(proc.time()-ptm)
		  tm = proc.time()-ptm
		  timemat[i, 1] = species
		  timemat[i, 2] = tm[[3]]
		  #readline()
	    } ### For species
		return(timemat)
	} else {
	 print ("Some problem in input parameters. ")
	}
} ### For BatchMaxent


