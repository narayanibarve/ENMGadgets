#' DistanceFilter - Raryfy the points in the dataset with threshold distance
#' 
#' @param input - input file name containing species name, Latitude and Longitude
#' @param threshold - value of threshold in (decimal) degrees
#' @param output - name of the output file
#' @examples \dontrun{
#' DistanceFilter()
#' }
#' @importFrom stats dist
#' @export

DistanceFilter <- function(input=NA, threshold=1, output=NA)
{
  if(is.na(input)){
    print("Please provide input file containing species name, Latitude and Longitude")
    return(NULL)
  }
  if(is.na(output)){
    print("Please provide output file name.")
    return(NULL)
  }
  
  tbl2 = read.table(input, header=T, sep =",")
  tbl1 = tbl2[,2:3]
  dist1 <- as.matrix(dist(tbl1, method ="euclidian", upper=T))
  SelfDist <- which(dist1 == min(dist1))
  dist1[SelfDist]=NA
  mindist = -1000
  Iteno = 1
  while (mindist < threshold)
  {
    print(paste("Iteration No", Iteno, sep = ":"))
    i = which(dist1==min(dist1,na.rm=T))
    divisor = ncol(dist1)
    coltochk = i[1] %% divisor
    if (coltochk == 0)
    {
      coltochk = i[2] %% divisor
    }
    DelRows = which( dist1[,coltochk] == min(dist1,na.rm=T))
    dist1 = dist1[-DelRows,-DelRows]
    mindist = min(dist1,na.rm=T)
    Iteno = Iteno + 1 
  }
  collist = as.numeric(dimnames(dist1)[[1]])
  NewTbl1 <- tbl1[collist,]
  write.table(NewTbl1,output, row.names=F, col.names=T, sep=",")
  plot(tbl1[,1],tbl1[,2],col="blue",asp=1)
  points(NewTbl1[,1],NewTbl1[,2],col="red")
  return(NewTbl1)
}
