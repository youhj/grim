#' Add Text to a Base Graphics Plot
#'
#' Draw rotated text
#'
#' @import grid
#' @import gridBase
#' @param x0 x coordinate of the origin
#' @param y0 y coordinate of the origin
#' @param x1 x coordinate of the unit vector
#' @param y1 y coordinate of the unit vector
#' @param label text labels
#' @param hpos horizontal position of text on the baseline (x0, y0) -- (x1, y1)
#' @param hoffset horizontal offset (in picas if unit is not specified) from the
#' \code{hpos}
#' @param voffset vertical offset (in picas if unit is not specified) from the
#' baseline (x0, y0) -- (x1, y1)
#' @param just the justification of the text. If there are two values, the
#' first value specifies horizontal justification and the second value
#' specifies vertical justification. Possible string values are: vertical
#' justification. Possible string values are: \code{left}, \code{right},
#' \code{centre}, \code{center}, \code{bottom}, and \code{top}. See \link[grid]{grid.text}
#' @param hjust A numeric vector specifying horizontal justification. See \link[grid]{grid.text}
#' @param vjust A numeric vector specifying vertical justification. See \link[grid]{grid.text}
#' @param rot the angle to rotatate the text. See \link[grid]{grid.text}
#' @param ... other graphical parameters
#' @examples
#' xs <- 0:10
#' ys <- 10*runif(11)
#' labels <- signif(sqrt(diff(xs)^2 + diff(ys)^2), 3)
#' plot(xs, ys, xlim = c(0,10), ylim = c(0,10))
#' for(i in 1:10) {
#'    grim.bracket(xs[i], ys[i], xs[i+1], ys[i+1],voffset = 0.2, code = i%%2 + 1)
#'    grim.text(xs[i], ys[i], xs[i+1], ys[i+1], label = labels[i],voffset = 2*ifelse(i%%2 == 0, 1, -1))
#'}
#' @export
grim.text <- function(x0, y0, x1, y1, label, hpos = 0.5, hoffset = 0, voffset = 0, 
                      just = "center", hjust = NULL, vjust = NULL, rot = 0, ...) {
    
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
    
    g <- textGrob(label = label, x = unit(hpos + hoffset, "picas"), y = unit(voffset, "picas"),
                  just = just, rot = rot, hjust = hjust, vjust = vjust, gp = gpar(...))
    grid.draw(g)
    
    popViewport()
    popViewport()        
} 

