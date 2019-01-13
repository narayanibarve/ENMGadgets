ENMGadgets
==========

## About
Gadgets for Ecological Niche Modelling


## Install

### Install the development version using `install_github` within Hadley's [devtools](https://github.com/hadley/devtools) package.

```R
install.packages("devtools")
require(devtools)

install_github("narayanibarve/ENMGadgets")
require(ENMGadgets)
```


### Packages `ENMGadgets` depends on
+ [raster] (http://cran.r-project.org/web/packages/raster/)
+ [maptools] (http://cran.r-project.org/web/packages/maptools/)
+ [sqldf] (http://cran.r-project.org/web/packages/sqldf/)
+ [fields] (http://cran.r-project.org/web/packages/fields/)
+ [rgdal] (http://cran.r-project.org/web/packages/rgdal/)
+ [dismo] (http://cran.r-project.org/web/packages/dismo/)


### Functions currently available

#### PCARaster

```r
pcaop = PCARaster()
```

#### PCAProjection

```r
pcaop = PCAProjection()
```

#### CropRaster

```r
CropRaster()
```

#### DistanceFilter
```r
DistanceFilter()
```

#### NicheViews
```r
NicheViews()
```

#### BatchMask
```r
BatchMask()
```

#### ModelThreshold
```r
ModelThreshold()
```

#### PartialROC
```r
PartialROC()
```

#### MOP
```r
MOP()
```

#### BatchMaxent
```r
BatchMaxent()
```