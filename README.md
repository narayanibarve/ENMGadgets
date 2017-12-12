ENMGadgets
==========

## About
Gadgets for Ecological Niche Modelling


## Install

### Install the development version using `install_github` within Hadley's [devtools](https://github.com/hadley/devtools) package.

```R
install.packages("devtools")
require(devtools)

install_github("vijaybarve/ENMGadgets")
require(ENMGadgets)
```

Note: 

Windows users have to first install [Rtools](http://cran.r-project.org/bin/windows/Rtools/) and make sure the path is included in system path setting.

### Packages `ENMGadgets` depends on
+ [raster] (http://cran.r-project.org/web/packages/raster/)
+ [maptools] (http://cran.r-project.org/web/packages/raster/)
+ [sqldf] (http://cran.r-project.org/web/packages/raster/)
+ [fields] (http://cran.r-project.org/web/packages/raster/)
+ [rgdal] (http://cran.r-project.org/web/packages/rgdal/)
+ [dismo] (http://cran.r-project.org/web/packages/dismo/)


### Functions currently available

#### PCARaster

```r
pcaop = iPCARaster()
```

#### PCAProjection

```r
pcaop = iPCAProjection()
```

#### CropRaster

```r
iCropRaster()
```

#### DistanceFilter
```r
DistanceFilter()
```

#### NicheViews
```r
iNicheViews()
```

#### BatchMask
```r
iBatchMask()
```

#### ModelThreshold
```r
iModelThreshold()
```

#### PartialROC
```r
iPartialROC()
```

#### MOP
```r
iMOP()
```

#### BatchMaxent
```r
iBatchMaxent()
```