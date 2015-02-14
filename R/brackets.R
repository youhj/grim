#' Add a Curly Brace to a Plot
#'
#' Draw curly braces between a pair of points
#'
#' @import grid
#' @import gridBase
#' @param x1 x coordinate of point *from* which to draw.
#' @param y1 y coordinate of point *from* which to draw.
#' @param x2 x coordinate of point *to* which to draw.
#' @param y2 y coordinate of point *to* which to draw.
#' @param h height
#' @param prop logical; if `TRUE', the height is propotional to the width of
#' the brace; if `FALSE', the height is constant (picas).
#' @param code integer code. determine direction of the curly brace
#' @param aspect determines the position of the break.  produces balanced curly brace with the default aspect (0.5).
#' @param length length of curls [0, 1]
#' @param curvature roundness [0, 1]
#' @param ... other graphical parameters
#' @examples
#' plot(0, type="n", xlim=c(0,20), ylim=c(0,20))
#' for(i in 1:5) {
#'    brace(i, 10-i, i, 10+i, col=i, h=1)
#'    brace(10-i, 20-i, 10+i, 20-i)
#'    brace(6, 14-2*i, 6+2*i, 14)
#' }
#' @export
brace <- function(x1, y1, x2, y2, h=1, prop=FALSE, code=1, aspect=0.5, length=0.125, curvature=0.5,  ...) {
    if(code==2) {
        tmp <- c(x1, y1)
        x1 <- x2; y1 <- y2;
        x2 <- tmp[1]; y2 <- tmp[2];
        aspect <- 1 - aspect
    }
    vps <- baseViewports()
    pushViewport(vps$inner, vps$figure, vps$plot)
    x1 <- as.numeric(convertX(unit(x1, "native"), "picas"))
    x2 <- as.numeric(convertX(unit(x2, "native"), "picas"))    
    y1 <- as.numeric(convertY(unit(y1, "native"), "picas"))
    y2 <- as.numeric(convertY(unit(y2, "native"), "picas"))
    dx <- x2 - x1
    dy <- y2 - y1
    w <- sqrt(dx^2 + dy^2)
    th <- 180 * atan2(dy, dx) / pi

    vp <- viewport(x=x1, y=y1, width=w, height=ifelse(prop, w*h, h),
                   default.units="picas",
                   angle=th, just=c("left","bottom"),
                   gp=gpar(...)) 
    pushViewport(vp)

    r <- length
    z <-  aspect
    grid.curve(x1=0, y1=0, x2=r, y2=0.5, curvature=-curvature)     
    grid.move.to(r, 0.5) 
    grid.line.to(z-r, 0.5) 
    grid.curve(x1=z-r, y1=0.5, x2=z, y2=1, curvature=curvature) 
    grid.curve(x1=1-r, y1=0.5, x2=1, y2=0, curvature=-curvature) 
    grid.move.to(1-r, 0.5) 
    grid.line.to(z+r, 0.5) 
    grid.curve(x1=z, y1=1, x2=z+r, y2=0.5, curvature=curvature) 
    popViewport()
    popViewport()    
} 






