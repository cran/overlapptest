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
