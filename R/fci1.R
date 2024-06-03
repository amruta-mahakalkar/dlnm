#
fci1 <-
  function(ci, x, high, low, ci.arg, plot.arg, shift, noeff=NULL) {
    #
    ################################################################################
    #
    if(ci=="area") {
      polygon.arg <- modifyList(list(col=grey(0.9),border=NA),ci.arg)
      polygon.arg <- modifyList(polygon.arg,
                                list(x=c(x,rev(x)),y=c(high,rev(low))))
      do.call(polygon,polygon.arg)
    } else if(ci=="bars") {
      # range <- diff(range(x))/300
      range <- 0.02
      segments.arg <- modifyList(ci.arg,list(x0=x+shift,y0=high,x1=x+shift,y1=low))
      do.call(segments,segments.arg)
      segments.arg <- modifyList(segments.arg,list(x0=x-range+shift,y0=high,
                                                   x1=x+range+shift,y1=high))
      do.call(segments,segments.arg)
      segments.arg <- modifyList(segments.arg,list(x0=x-range+shift,y0=low,
                                                   x1=x+range+shift,y1=low))
      do.call(segments,segments.arg)
    } else if(ci=="lines") {
      lines.arg <- list(lty=2)
      if(!is.null(plot.arg$col)) lines.arg$col <- plot.arg$col
      lines.arg <- modifyList(lines.arg,ci.arg)
      lines.arg <- modifyList(lines.arg,list(x=x+shift,y=high))
      do.call(lines,lines.arg)
      lines.arg <- modifyList(lines.arg,list(x=x+shift,y=low))
      do.call(lines,lines.arg)
    }
    if(!is.null(noeff)) abline(h=noeff)
  }

###