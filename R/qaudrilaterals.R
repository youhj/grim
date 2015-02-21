

##' Add Rectangles to a Base Graphic Plot
##' 
##' Draw rectangles
##' 
##' @param x0 x coordinate of the origin
##' @param y0 y coordinate of the origin
##' @param x1 x coordinate of the unit vector
##' @param y1 y coordinate of the unit vector
##' @param hpos horizontal position of rectangle on the baseline (x0, y0) -- (x1, y1)
##' @param hoffset horizontal offset (in picas if unit is not specified) from the \code{hpos}
##' @param voffset vertical offset (in picas if unit is not specified) from the baseline (x0, y0) -- (x1, y1)
##' @param width width of the rectangle (relative to the length of the unit vector)
##' @param height height of the rectangle (in picas)
##' @param just the justification. See \code{\link[grid]{grid.rect}}
##' @param hjust A numeric vector specifying horizontal justification. See \code{\link[grid]{grid.rect}}
##' @param vjust A numeric vector specifying vertical justification. See \code{\link[grid]{grid.rect}}
##' @param ... other graphical parameters
##' 
grim.rect <- function(x0, y0, x1, y1, hpos = 0, hoffset = 0, voffset = 0,
                      width = 1, height = 1, just = "center", hjust = NULL, vjust = NULL, ...) {
    
    vps <- baseViewports()
    pushViewport(vps$inner, vps$figure, vps$plot)

    x0 <- as.numeric(convertX(unit(x0, "native"), "picas"))
    x1 <- as.numeric(convertX(unit(x1, "native"), "picas"))    
    y0 <- as.numeric(convertY(unit(y0, "native"), "picas"))
    y1 <- as.numeric(convertY(unit(y1, "native"), "picas"))

    if(is.unit(voffset)) {voffset <- as.numeric(convertHeight(voffset, "picas")) }
    if(is.unit(hoffset)) {voffset <- as.numeric(convertHeight(hoffset, "picas")) }
    
    dx <- x1 - x0
    dy <- y1 - y0
    w <- sqrt(dx^2 + dy^2)
    h <- 1
    th <- 180 * atan2(dy, dx) / pi

    vp <- viewport(x = x0, y = y0, width = w, height = h,
                   default.units = "picas",
                   angle = th, just = c("left","bottom"))
    pushViewport(vp)

    hpos <- as.numeric(convertX(unit(hpos, "npc"), "picas"))
    
    g <- rectGrob(x = unit(hpos + hoffset, "picas"), y = unit(voffset, "picas"),
                  width=width, height=height,
                  just = just, hjust = hjust, vjust = vjust, gp = gpar(...))
    grid.draw(g)
    
    popViewport()
    popViewport()            
}
