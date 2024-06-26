###
### R routines for the R package dlnm (c)
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

### R routines for the R package dlnm (c)
lines.offseted <-
  function(x, ptype, var=NULL, lag=NULL, ci="n", ci.arg, shift,
           ci.level=x$ci.level, cumul=FALSE, exp=NULL, ...) {
    #
    ################################################################################
    #
    if(all(class(x)!="crosspred")) stop("'x' must be of class 'crosspred'")
    ci <- match.arg(ci,c("area","bars","lines","n"))
    #
    # SETTING DEFAULT FOR ptype: OVERALL FOR NO LAG, SLICES FOR VAR/LAG, OTHERWISE 3D
    if(missing(ptype)) {
      if(!is.null(var)||!is.null(lag)) {
        ptype <- "slices"
      }else ptype <- "overall"
    }
    ptype <- match.arg(ptype,c("slices","overall"))
    #
    if(!xor(is.null(var),is.null(lag))&(ptype=="slices")) {
      stop("One (only) of 'var' or 'lag' must be provided when ptype='slices'")
    }
    if(!is.null(var)&&!is.numeric(var)&&length(var)>1&&ptype=="slices") {
      stop("'var' must be a numeric scalar")
    }
    if(!is.null(lag)&&!is.numeric(lag)&&length(lag)>1&&ptype=="slices") {
      stop("'lag' must be a numeric scalar")
    }
    if(!is.null(var)&&any(!var%in%x$predvar)&&(ptype=="slices")) {
      stop("'var' must match values used for prediction")
    }
    if(!is.null(lag)&&any(!lag%in%seqlag(x$lag,x$bylag))&&(ptype=="slices")) {
      stop("'lag' must match values used for prediction")
    }
    if(missing(ci.arg)) {
      ci.arg <- list()
    } else if(!is.list(ci.arg)) stop("'ci.arg' must be a list")
    if(!is.numeric(ci.level)||ci.level>=1||ci.level<=0) {
      stop("'ci.level' must be numeric and between 0 and 1")
    }
    if(cumul==TRUE) {
      # SET THE LAG STEP EQUAL TO 1
      x$bylag <- 1
      if(is.null(x$cumfit)) {
        stop("Cumulative outcomes can be plotted if predicted in the 'crosspred'
  object. Set the argument 'cumul=TRUE' in the function crosspred()")
      }
    }
    if(!is.null(exp)&&!is.logical(exp)) stop("'exp' must be logical")
    #
    ##########################################################################
    # COMPUTE OUTCOMES
    #
    # CUMULATIVE IF CUMUL==T
    if(cumul==TRUE) {
      x$matfit <- x$cumfit
      x$matse <- x$cumse
    }
    #
    # SET THE Z LEVEL EQUAL TO THAT STORED IN OBJECT IF NOT PROVIDED
    z <- qnorm(1-(1-ci.level)/2)
    x$mathigh <- x$matfit+z*x$matse
    x$matlow <- x$matfit-z*x$matse
    x$allhigh <- x$allfit+z*x$allse
    x$alllow <- x$allfit-z*x$allse
    #
    # EXPONENTIAL
    if((is.null(exp)&&!is.null(x$model.link)&&x$model.link%in%c("log","logit"))||
       (!is.null(exp)&&exp==TRUE)) {
      x$matfit <- exp(x$matfit)
      x$mathigh <- exp(x$mathigh)
      x$matlow <- exp(x$matlow)
      x$allfit <- exp(x$allfit)
      x$allhigh <- exp(x$allhigh)
      x$alllow <- exp(x$alllow)
    }
    #
    ##########################################################################
    # GRAPHS
    #
    ##########
    # SLICES
    ##########
    #
    if(ptype=="slices") {
      # LAG
      if(!is.null(lag)) {
        xlag <- paste("lag",lag,sep="")
        # SET DEFAULT VALUES IF NOT INCLUDED BY THE USER
        plot.arg <- list(type="p")
        plot.arg <- modifyList(plot.arg,list(...))		
        # PLOT CONFIDENCE INTERVALS (IF ANY)
        fci1(ci=ci,x=x$predvar,high=x$mathigh[,xlag],
             low=x$matlow[,xlag],ci.arg,plot.arg,shift)
        plot.arg <- modifyList(plot.arg,c(list(x=x$predvar + shift,
                                               y=x$matfit[,xlag])))
        do.call("points",plot.arg)
      }
      # VAR
      if(!is.null(var)) {
        xvar <- as.character(var)
        # SET DEFAULT VALUES IF NOT INCLUDED BY THE USER
        plot.arg <- list(type="p")
        plot.arg <- modifyList(plot.arg,list(...))		
        # PLOT CONFIDENCE INTERVALS (IF ANY)
        fci1(ci=ci,x=seqlag(x$lag,x$bylag),high=x$mathigh[xvar,],
             low=x$matlow[xvar,],ci.arg,plot.arg,shift)
        plot.arg <- modifyList(plot.arg,c(list(x=seqlag(x$lag,x$bylag) + shift,
                                               y=x$matfit[xvar,])))
        do.call("points",plot.arg)
      }
    }
  }
