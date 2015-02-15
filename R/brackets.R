#' Add a Bracket to a Plot (base graphics)
#'
#' Draw brackets, parentheses, curly brackets, angled brackets between a pair of points
#'
#' @import grid
#' @import gridBase
#' @param x1 x coordinate of point *from* which to draw.
#' @param y1 y coordinate of point *from* which to draw.
#' @param x2 x coordinate of point *to* which to draw.
#' @param y2 y coordinate of point *to* which to draw.
#' @param h height
#' @param prop logical; if `TRUE', the height is propotional to the width of
#' the bracket; if `FALSE', the height is constant (picas).
#' @param code integer code. determine direction of the bracket left/above (1), right/under (2)
#' @param offset offset from the baseline (x1, y1) -- (x2, y2)
#' @param padding padding sizes of bottom, left, top, right
#' @param center relative center location, where head is located
#' @param type bracket type: "curly", "round", "square", or "angle".
#' @param head shape of the head. height of the head. widths and curvatures of
#' left and right sides. if NULL, no head.
#' @param tail shape of the left and right tails.
#' @param ... other graphical parameters
#' @examples
#'plot(0, type="n", xlim=c(-1, 16), ylim=c(-0.5, 3.5), xaxp=c(0,15,15), yaxp=c(0,3,3), ylab="")
#' grim.bracket(0, 0, 0, 3)
#' grim.bracket(1, 0, 1, 3, h=2)
#' grim.bracket(2, 0, 2, 3, h=3)
#' grim.bracket(3, 0, 3, 3, code=2)
#' grim.bracket(4, 0, 4, 3, center=0.3)
#' grim.bracket(5, 0, 5, 3, center=0.9)
#' grim.bracket(6, 0, 6, 3, lty=2)
#' grim.bracket(7, 0, 7, 3, lwd=2)
#' grim.bracket(8, 0, 8, 3, col="red")
#' grim.bracket(9, 0, 9, 3, type="round")
#' grim.bracket(10, 0, 10, 3, type="round", h=3)
#' grim.bracket(11, 0, 11, 3, type="square", h=2)
#' grim.bracket(12, 0, 12, 3, type="square", code=2)
#' grim.bracket(13, 0, 13, 3, type="angle")
#' grim.bracket(14, 0, 14, 3, type="angle", code=2, center=1/3, h=2) 
#' grim.bracket(15, 0, 15, 3, type="angle", code=2, center=2/3) 
#' @export
grim.bracket <- function(x1, y1, x2, y2, h=unit(1, "picas"), prop=FALSE, code=1, offset=0, padding=rep(0, 4),
                    center=0.5,
                    type=c("curly", "round", "square", "angle"),
                    head=list(height=c(0.5, 0.5), width=c(1/8, 1/8), curvature=c(0.5, 0.5)),
                    tail=list(height=c(0.5, 0.5), width=c(1/8, 1/8), curvature=c(0.5, 0.5)), ...) {
    if(code==2) {
        tmp <- c(x1, y1)
        x1 <- x2; y1 <- y2;
        x2 <- tmp[1]; y2 <- tmp[2];
    }
    if(length(padding)==1) { padding <- rep(padding, 4); }
    else if(length(padding==2)) { padding <- rep(padding, 2); }
    else { stop("invalid padding length!"); }
    
    vps <- baseViewports()
    pushViewport(vps$inner, vps$figure, vps$plot)
    x1 <- as.numeric(convertX(unit(x1, "native"), "picas"))
    x2 <- as.numeric(convertX(unit(x2, "native"), "picas"))    
    y1 <- as.numeric(convertY(unit(y1, "native"), "picas"))
    y2 <- as.numeric(convertY(unit(y2, "native"), "picas"))
    if(!is.unit(h)) { h <- unit(h, "native") }
    h <- as.numeric(convertHeight(h, "picas"))    
    offset <- as.numeric(convertHeight(unit(offset, "native"), "picas"))
    padding[c(1, 3)] <- as.numeric(convertHeight(unit(padding[c(1, 3)], "native"), "picas"))
    padding[c(2, 4)] <- as.numeric(convertWidth(unit(padding[c(2, 4)], "native"), "picas"))
    
    dx <- x2 - x1
    dy <- y2 - y1
    w <- sqrt(dx^2 + dy^2)
    th <- 180 * atan2(dy, dx) / pi

    vp <- viewport(x=x1, y=y1, width=w, height=ifelse(prop, w*h, h)+offset,
                   default.units="picas",
                   angle=th, just=c("left","bottom"),
                   gp=gpar(...)) 
    pushViewport(vp)
    
    
    vp2 <- viewport(x=padding[2], y=offset+padding[1], width=w-sum(padding[c(2,4)]),
                    height=ifelse(prop, w*h, h)-sum(padding[c(1,3)]),
                    default.units="picas", just=c("left", "bottom"), gp=gpar(...))
    pushViewport(vp2)
    
    ll <- as.list(match.call(expand.dots=FALSE))[-c(1:5)]
    for(arg in c("h", "prop", "code", "offset", "padding", "...")) { if(arg %in% names(ll)) ll[[arg]] <- NULL }
    do.call(grid.bracket, ll)
    
    popViewport()
    popViewport()
    popViewport()        
} 

#' @rdname bracketGrob
#' @examples
#' grid.newpage()
#' grid.bracket()
#' @export
grid.bracket <-function(x1=0, y1=0, x2=1, y2=1, center=0.5,
                        type=c("curly", "round", "square", "angle"),
                        head=list(height=c(0.5, 0.5), width=c(1/8, 1/8), curvature=c(0.5, 0.5)),
                        tail=list(height=c(0.5, 0.5), width=c(1/8, 1/8), curvature=c(0.5, 0.5)),
                        direction=c("above", "under", "left", "right"),
                      default.units="npc", gp=gpar()) {
    grid.draw(do.call(bracketGrob, as.list(match.call())[-1]))
}    

#' Draw a Brackets
#'
#' Draw brackets, parentheses, curly brackets, angled brackets
#'
#' @param x1 x coordinate of bottom left
#' @param y1 y coordinate of bottom left
#' @param x2 x coordinate of top right
#' @param y2 y coordinate of top right
#' @param center relative center location, where head is located
#' @param type bracket type: "curly", "round", "square", or "angle".
#' @param head shape of the head. height of the head. widths and curvatures of
#' left and right sides. if NULL, no head.
#' @param tail shape of the left and right tails.
#' @param direction direction of the bracket
#' @param default.units default units
#' @param gp An object of class `gpar'. graphical parameters.
#' @export
bracketGrob <- function(x1=0, y1=0, x2=1, y2=1, center=0.5,
                        type=c("curly", "round", "square", "angle"),
                        head=list(height=c(0.5, 0.5), width=c(1/8, 1/8), curvature=c(0.5, 0.5)),
                        tail=list(height=c(0.5, 0.5), width=c(1/8, 1/8), curvature=c(0.5, 0.5)),
                      direction=c("above", "under", "left", "right"),
                      default.units="npc", gp=gpar()) {

    if(!missing(type) && (!missing(head) || !missing(tail))) {
        stop("specify 'type' or customize 'head' and 'tail'")
    } else if(!missing(type)) {
        type <- match.arg(type)
        if(type=="curly") {
            width <- min(1/8, min(center, 1-center)/4)
            head <-list(height=0.5, width=width, curvature=0.5)
            tail <- list(height=0.5, width=width,curvature=0.5)        
        } else if(type=="round") {
            head <- NULL;
            tail <- list(height=1.0, width=c(center, 1-center), curvature=0.1)
        } else if(type=="square") {
            head <- NULL;
            tail <- list(height=1.0, width=0, curvature=0)
        } else if(type=="angle") {
            head <- NULL;
            tail <- list(height=1.0, width=c(center, 1-center), curvature=0);
        }
    } else if (missing(type) && missing(head) && missing(tail) && !missing(center)) {
            width <- min(1/8, min(center, 1-center)/4)
            head <-list(height=0.5, width=width, curvature=0.5)
            tail <- list(height=0.5, width=width,curvature=0.5)                
    }

    
    if(is.null(head)) { head <- list(height=0, width=0, curvature=0) }
    if(is.null(tail)) { tail <- list(height=0, width=0, curvature=0) }    
    if(length(head$height)==1) { head$height <- rep(head$height, 2) }    
    if(length(head$width)==1) { head$width <- rep(head$width, 2) }
    if(length(head$curvature)==1) { head$curvature <- rep(head$curvature, 2) }
    if(length(tail$height)==1) { tail$height <- rep(tail$height, 2) }        
    if(length(tail$width)==1) { tail$width <- rep(tail$width, 2) }
    if(length(tail$curvature)==1) { tail$curvature <- rep(tail$curvature, 2) }
    
    xtmp <- range(c(x1, x2)); x1 <- xtmp[1]; x2 <- xtmp[2]
    ytmp <- range(c(y1, y2)); y1 <- ytmp[1]; y2 <- ytmp[2]
    haspect <- c(tail$height[1], head$height[1], head$height[2], tail$height[2])
    waspect <- c(tail$width[1], head$width[1], head$width[2], tail$width[2])
    curv <- c(tail$curvature[1], head$curvature[1], head$curvature[2], tail$curvature[2])

    
    
    direction <- match.arg(direction)
    if(direction=="above") {
        w <- waspect * (x2 - x1)
        h <- haspect * (y2 - y1)
        xc <- x1 + center*(x2-x1)
        xs <- c(x1, x1+w[1], xc-w[2], xc, xc+w[3], x2-w[4], x2)
        ys <- c(y1, y1+h[1], y2-h[2], y2, y2-h[3], y1+h[4], y1)
    } else if(direction=="under") {
        w <- waspect * (x2 - x1)
        h <- haspect * (y2 - y1)        
        xc <- x1 + center*(x2-x1)
        xs <- c(x1, x1+w[1], xc-w[2], xc, xc+w[3], x2-w[4], x2)
        ys <- c(y2, y2-h[1], y1+h[2], y1, y1+h[3], y2-h[4], y2)
        curv <- -curv
    } else if(direction=="left") {
        w <- waspect * (y2 - y1)
        h <- haspect * (x2 - x1)        
        yc <- y1 + center*(y2-y1)
        xs <- c(x2, x2-h[1], x1+h[2], x1, x1+h[3], x2-h[4], x2)
        ys <- c(y1, y1+w[1], yc-w[2], yc, yc+w[3], y2-w[4], y2)
    } else if(direction=="right") {
        w <- waspect * (y2 - y1)
        h <- haspect * (x2 - x1)        
        yc <- y1 + center*(y2-y1)
        xs <- c(x1, x1+h[1], x2-h[2], x2, x2-h[3], x1+h[4], x1)
        ys <- c(y1, y1+w[1], yc-w[2], yc, yc+w[3], y2-w[4], y2)        
        curv <- -curv
    }

    if( isTRUE(all.equal(c(xs[1], ys[1]), c(xs[2], ys[2]))) ) {
        g1 <- NULL
    } else {
        g1 <- curveGrob(x1=xs[1], y1=ys[1], x2=xs[2], y2=ys[2],
                        curvature=-curv[1], default.units=default.units, gp=gp)
    }
    if( isTRUE(all.equal(c(xs[2], ys[2]), c(xs[3], ys[3]))) ) {
        g2 <- NULL
        g3 <- NULL
    } else {
        g2 <- curveGrob(xs[2], ys[2], mean(xs[2:3]), mean(ys[2:3]), curvature=0)
        g3 <- curveGrob(mean(xs[2:3]), mean(ys[2:3]), xs[3], ys[3], curvature=0)
    }
    if( isTRUE(all.equal(c(xs[3], ys[3]), c(xs[4], ys[4]))) ) {
        #g4 <- linesGrob(xs[3:4], ys[3:4], default.units=default.units, gp=gp)
        g4 <- NULL
    } else {
        g4 <- curveGrob(x1=xs[3], y1=ys[3], x2=xs[4], y2=ys[4],
                        curvature=curv[2], default.units=default.units, gp=gp)
    }
    if( isTRUE(all.equal(c(xs[4], ys[4]), c(xs[5], ys[5]))) ) {
        #g5 <- linesGrob(xs[4:5], ys[4:5], default.units=default.units, gp=gp)
        g5 <- NULL
    } else {
        g5 <- curveGrob(x1=xs[4], y1=ys[4], x2=xs[5], y2=ys[5],
                        curvature=curv[3], default.units=default.units, gp=gp)
    }
    if( isTRUE(all.equal(c(xs[5], ys[5]), c(xs[6], ys[6]))) ) {
        g6 <- NULL
        g7 <- NULL
    } else {
        g6 <- curveGrob(xs[5], ys[5], mean(xs[5:6]), mean(ys[5:6]), curvature=0)
        g7 <- curveGrob(mean(xs[5:6]), mean(ys[5:6]), xs[6], ys[6], curvature=0 )
    }
    if( isTRUE(all.equal(c(xs[6], ys[6]), c(xs[7], ys[7]))) ) {
        g8 <- NULL
    } else {
        g8 <- curveGrob(x1=xs[6], y1=ys[6], x2=xs[7], y2=ys[7],
                        curvature=-curv[4], default.units=default.units, gp=gp)
    }
    
    gTree(children=gList(g1, g2, g3, g4, g5, g6, g7, g8))
}
