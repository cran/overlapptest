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
### code chunk number 3: Readshapefiles.Rnw:104-106
###################################################
library(rgdal)
library(maptools)


###################################################
### code chunk number 4: Readshapefiles.Rnw:110-111 (eval = FALSE)
###################################################
## x <- readOGR("mydata.shp")


###################################################
### code chunk number 5: Readshapefiles.Rnw:116-117 (eval = FALSE)
###################################################
## class(x)


###################################################
### code chunk number 6: Readshapefiles.Rnw:128-129
###################################################
dire<-getwd()


###################################################
### code chunk number 7: Readshapefiles.Rnw:132-134
###################################################
setwd(system.file("shapes", package="overlapptest"))
Androsace <- readOGR("Androsace.shp", verbose=FALSE)


###################################################
### code chunk number 8: Readshapefiles.Rnw:136-137
###################################################
class(Androsace)


###################################################
### code chunk number 9: Readshapefiles.Rnw:139-140
###################################################
setwd(dire)


###################################################
### code chunk number 10: Readshapefiles.Rnw:148-150
###################################################
library(maptools)
library(spatstat.geom)


###################################################
### code chunk number 11: Readshapefiles.Rnw:159-160
###################################################
spatstat.geom::spatstat.options(fixpolygons=FALSE)


###################################################
### code chunk number 12: Readshapefiles.Rnw:172-173
###################################################
Androsace<- as(Androsace, "owin")


###################################################
### code chunk number 13: Readshapefiles.Rnw:181-182 (eval = FALSE)
###################################################
## library(overlapptest)


###################################################
### code chunk number 14: Readshapefiles.Rnw:196-219
###################################################
check.ventana <-
function(ventana){
    malos1 <- NULL
    malos2<- NULL
    for( i in 1:length(ventana$bdry)){
       test <- try(owin(poly=ventana$bdry[[i]]), silent=TRUE)
       if(class(test)=="try-error"){
            malos1<- c(malos1, i) # anota en que subpoligono hay un error
            # intenta darle la vuelta a las coordenadas a ver si eso lo resuleve
            test2 <- try( owin(poly=lapply(ventana$bdry[[i]], rev), silent=TRUE))
              if(class(test2)!="try-error"){
                   # si al dar la vuelta a las cooredenadas se arregla, grabamos las nuevas coordenadas
                   ventana$bdry[[i]] <-lapply(ventana$bdry[[i]], rev)
	        } else malos2<- c(malos2, i)
	}
     }
   attr(ventana, "corrected")  <-malos1
   attr(ventana, "not.corrected") <- malos2
   if(!is.null(malos1)) cat( length(malos1), "problematic polygon(s) detected \n \n")
   if(!is.null(malos1) & is.null(malos2)) cat("all problematic polygons have been repared\n \n")
   if(!is.null(malos2)) cat(length(malos2), "problematic polygon(s) could not been repared\n \n")
   return(ventana)
}


###################################################
### code chunk number 15: Readshapefiles.Rnw:222-223
###################################################
Androsace <- check.ventana(Androsace)


###################################################
### code chunk number 16: Readshapefiles.Rnw:229-230
###################################################
options(width=80)


###################################################
### code chunk number 17: Readshapefiles.Rnw:232-233
###################################################
attributes (Androsace)


