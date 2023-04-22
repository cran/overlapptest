### R code from vignette source 'Readshapefiles.Rnw'

###################################################
### code chunk number 1: Readshapefiles.Rnw:7-8
###################################################
options(SweaveHooks=list(fig=function() par(mar=c(1,1,1,1))))


###################################################
### code chunk number 2: Readshapefiles.Rnw:26-32
###################################################
library(spatstat.geom)
options(useFancyQuotes=FALSE)
sdate <- read.dcf(file = system.file("DESCRIPTION", package = "overlapptest"),
         fields = "Date")
sversion <- read.dcf(file = system.file("DESCRIPTION", package = "overlapptest"),
         fields = "Version")


###################################################
### code chunk number 3: Readshapefiles.Rnw:103-104
###################################################
library(sf)


###################################################
### code chunk number 4: Readshapefiles.Rnw:108-109 (eval = FALSE)
###################################################
## x <- st_read("mydata.shp")


###################################################
### code chunk number 5: Readshapefiles.Rnw:114-115 (eval = FALSE)
###################################################
## class(x)


###################################################
### code chunk number 6: Readshapefiles.Rnw:125-126
###################################################
dire<-getwd()


###################################################
### code chunk number 7: Readshapefiles.Rnw:129-131
###################################################
setwd(system.file("shapes", package="overlapptest"))
Androsace <- st_read("Androsace.shp", quiet =TRUE)


###################################################
### code chunk number 8: Readshapefiles.Rnw:133-134
###################################################
class(Androsace)


###################################################
### code chunk number 9: Readshapefiles.Rnw:136-137
###################################################
setwd(dire)


###################################################
### code chunk number 10: Readshapefiles.Rnw:144-145
###################################################
library(spatstat.geom)


###################################################
### code chunk number 11: Readshapefiles.Rnw:154-155
###################################################
spatstat.geom::spatstat.options(fixpolygons=FALSE)


###################################################
### code chunk number 12: Readshapefiles.Rnw:169-170
###################################################
Androsace <- as.owin(Androsace, check_polygons=FALSE)


###################################################
### code chunk number 13: Readshapefiles.Rnw:178-179 (eval = FALSE)
###################################################
## library(overlapptest)


###################################################
### code chunk number 14: Readshapefiles.Rnw:193-226
###################################################

check.ventana <- 
function (ventana) 
{
    malos1 <- NULL
    malos2 <- NULL
    for (i in 1:length(ventana$bdry)) {
        xy <- list(x = ventana$bdry[[i]]$x, y= ventana$bdry[[i]]$y)
       test <- try(owin(poly=xy), silent=TRUE)
       if(inherits (test, "try-error")){
            malos1 <- c(malos1, i)
            test2 <- try(owin(poly = lapply(xy, rev), silent = TRUE))
             if(!inherits (test2, "try-error")){
                ventana$bdry[[i]] <- owin(poly = lapply(xy, rev))$bdry
            }  else{
 	       malos2 <- c(malos2, i)
		ventana$bdry[[i]]$area <- NULL
	        ventana$bdry[[i]]$hole <- NULL
               }
        }
    }
    attr(ventana, "corrected") <- malos1
    attr(ventana, "not.corrected") <- malos2
    if (!is.null(malos1)) 
        cat(length(malos1), "problematic polygon(s) detected \n \n")
    if (!is.null(malos1) & is.null(malos2)) 
        cat("all problematic polygons have been repared\n \n")
    if (!is.null(malos2)) 
        cat(length(malos2), "problematic polygon(s) could not been repared\n \n")
    return(ventana)
}




###################################################
### code chunk number 15: Readshapefiles.Rnw:229-230
###################################################
Androsace <- check.ventana(Androsace)


###################################################
### code chunk number 16: Readshapefiles.Rnw:236-237
###################################################
options(width=80)


###################################################
### code chunk number 17: Readshapefiles.Rnw:239-240
###################################################
attributes (Androsace)


