library(data.table) # for everything
library(ggplot2)  # for plotting
library(reshape2) # for melting


actual_pred_plot <- function(var.by=var.by,
                    var.response=var.response,
                    data=data,
                    var.predict.current=NULL,
                    var.predict.reference=NULL,
                    var.split=NULL,
                    var.weight=NULL,
                    var.by.buckets=NULL,
                    remove.na=FALSE,
                    sort.factor=FALSE,
                    errorbars=FALSE,
                    subset.to=NA,
                    count.line=NULL,
                    barline.ratio=1,
                    title="",
                    make.plot=TRUE
){
  ############################################################################# 
  # FUNCTION: actual_pred_plot
  #
  #   Procedure for making univariate plots. This will produce one graph from a
  #   data frame/data table, containing overlaid exposure histograms, actual 
  #   line(s) and optionally one or many predicted lines. Additionally, two-way 
  #   interactions are possible and well-handled. Returns a ggplot object.
  #
  # ARGUMENTS:
  #   var.by:  The name of the variable whose levels to group by in the plot.
  #       Determines the bins for histogram and line plot. (string) [required]
  #   var.response:  The name of the var containing the actual response. (string)
  #       [required]
  #   data:  The data.frame/.table object to generate the plots from. [required]
  #   var.predict.current:  The name of the var containing the first predicted  
  #       response. (string) [optional]
  #   var.predict.reference:  The name of the var containing the second predicted
  #       response. If you have more than two predicted cols to plot, you can 
  #       pass a vector of predicted colnames here (or to predict.current) and 
  #       actual_pred_plot will sort it out (string) [optional] 
  #   var.split: The name of the var to interact with var.by. (string) [optional]
  #   var.weight:  The name of the var to be used to weight each observation.
  #       If NULL (default), all obs are given equal weight. (string) [optional]        
  #   var.by.buckets: How many buckets to group factor vars into (num) [optional]
  #   remove.na: If response/predictions have NAs, should we remove them?
  #   sort.factor: Do you want to sort x-axis so predicted line always increases?
  #   errorbars:  Boolean, should the actual line include error bars od +-2SE?
  #       Defaults to FALSE. [optional]
  #   subset.to:  Vector of length 2 specifiying where to cap a numeric var.by. 
  #       e.g. c(min.val, max.val). To only specify one boundary use NA; e.g.
  #       c(NA, max.val) (numeric) [optional]
  #   barline.ratio:  What should the ratio of maximum bar height to maximum line
  #       value be? (num) [optional]
  #   title:  Do you want a title? (string) [optional]    
  #   make.plot:  Boolean, if FALSE the grid used to make the plot will be 
  #       returned instead of the plot itself. [optional]
  #
  #############################################################################
  
  # pull out only the cols you need and convert to DT if necessary
  needed.cols <- c(var.by, var.split, var.response, 
                   var.predict.current, var.predict.reference)
  
  if("data.table" %in% class(data)){
    dat.frm <- data[, (needed.cols), with=FALSE]
  } else {
    dat.frm <- data.table(data[, needed.cols])
  }
  
  # cap var.by at specified limits
  if(!is.na(subset.to[1])){
    dat.frm[[var.by]] <- pmax(dat.frm[[var.by]], subset.to[1]) 
  }
  if(!is.na(subset.to[2])){
    dat.frm[[var.by]] <- pmin(dat.frm[[var.by]], subset.to[2]) 
  }
  
  # upgroup poorly exposed buckets if specified
  if(!is.null(var.by.buckets)){
    if(nlevels(dat.frm[[var.by]]) > var.by.buckets)
      dat.frm[[var.by]] <- keep_n_top(dat.frm[[var.by]], var.by.buckets) 
  } 
  
  # make sure there are fewer than 100 levels for numerics
  if(length(unique(dat.frm[[var.by]])) > 100){
    dat.frm[[var.by]] <- round_down_to_100_levels(dat.frm[[var.by]])
  }
  
  # set key (if var.split = NULL this does same as setkeyv(dat.frm, var.by))
  setkeyv(dat.frm, c(var.by, var.split))
  
  # get means using .SD and .SDcols, nice advantage of assigning nice colnames
  meanable.cols <- c(var.response, var.predict.current, var.predict.reference)
  
  # if response/predicted cols contain NAs throw an error by default, or..
  if(remove.na){
    meanme <- function(vector.to.mean){
      mean(vector.to.mean, na.rm=TRUE)
    }
  } else {
    meanme <- function(vector.to.mean){
      mean(vector.to.mean)
    }
  }
  
  meantable <- dat.frm[, lapply(.SD, function(x) meanme(x)),
                       by = key(dat.frm),
                       .SDcols = (meanable.cols)]
  
  if(!is.null(var.weight)){
    counttable <- dat.frm[, list(count=sum(var.weight)), 
                          by=key(dat.frm), with=FALSE]
  } else {
    counttable <- dat.frm[, list(count=.N), by=key(dat.frm)]
  }
  
  # don't necessarily need to merge, but makes the ggplot call simpler
  plotme <- meantable[counttable]  
  
  # scale the count, make it fit on response axis  
  rescale_factor <- max(plotme[["count"]]) / max(plotme[[var.response]]) 
  plotme$count_scaled <- plotme$count / (barline.ratio * rescale_factor)
  
  
  
  # which variables to use as melt id?
  melt.id <- c(var.by, var.split, "count", "count_scaled")
  
  # add in error bars if requested
  if(errorbars){
    
    sterrtable <- dat.frm[, list(sterr = sd(get(var.response))/sqrt(.N)), 
                          by=key(dat.frm)]
    
    plotme <- plotme[sterrtable]                      
    
    plotme$ymax <- plotme[[var.response]] + 2 * plotme$sterr
    plotme$ymin <- plotme[[var.response]] - 2 * plotme$sterr
    plotme$sterr <- NULL
    
    melt.id <- c(melt.id, "ymax", "ymin")
    gg.errors <- geom_errorbar(aes_string(x=var.by,
                                          ymax= "ymax",
                                          ymin= "ymin",
                                          color="variable"),
                               stat="identity",
                               width=0.25,
                               alpha=0.75)
    
  }else{ gg.errors <- NULL}
  
  # melt into a form that ggplot likes
  plotme <- melt(plotme, id=melt.id)
  plotme$grouping <- plotme$variable
  
  # get rid of errors on the non-response rows (to make error bar colours work)
  if(errorbars) plotme[!variable == var.response, ':=' (ymax = NA, ymin = NA)]
  
  
  if(!is.null(var.split)) {
    
    plotme[[var.split]] <- as.factor(plotme[[var.split]])
    plotme$grouping <- paste0(plotme[[var.split]], " - ", plotme$variable)
    var.split2 <- var.split
    delete_linetype_legend <- NULL
    
  }else{
    
    var.split2 <- NULL
    var.split <- "variable"
    delete_linetype_legend <- scale_linetype(guide='none')
  }
  
  if(is.null(var.predict.current) & is.null(var.predict.reference)) {
    delete_linetype_legend <- scale_linetype(guide='none')
  }
  
  # do you want to sort plot so predicted line is always increasing?
  if(sort.factor){
    levels <- plotme[variable == var.predict.current][order(value)][[var.by]]
    plotme[[var.by]] <- factor(plotme[[var.by]], levels=levels)
  } else {
    plotme[[var.by]] <- as.factor(plotme[[var.by]])
  }
  
  # add a dashed line to give some sort of reference for height of count bars
  if(!is.null(count.line)){
    countline_rescaled <- count.line/(rescale_factor*barline.ratio)
    gg.countline <- geom_hline(yintercept=countline_rescaled, 
                               colour="grey",
                               linetype="longdash",
                               size=0.1,
                               alpha=0.05)
    gg.annotate <- annotate("text", 
                            x=plotme[[var.by]][1], 
                            y=countline_rescaled,
                            label = paste0("N = ", count.line),
                            hjust=0,
                            vjust=1)
    
  } else { 
    gg.countline <- NULL
    gg.annotate <- NULL
  }
  
  # do you want the plot or just the grid used to make it?
  if(!make.plot){
    return(plotme)
  }
  
  plt <- ggplot(plotme) +
    geom_line(aes_string(x=var.by,
                         y="value",
                         group="grouping",
                         color=var.split,
                         linetype="variable"),
              stat="identity") +
    gg.errors +
    geom_point(aes_string(x=var.by,
                          y="value",
                          group="grouping",
                          color=var.split,
                          shape=var.split),
               stat="identity", 
               size=2) +
    geom_bar(aes_string(x=var.by,
                        y="count_scaled",
                        group="grouping",
                        fill=var.split2),
             stat="identity",
             alpha=0.25,
             position="dodge") +
    geom_hline(yintercept=count.line/(rescale_factor*barline.ratio), 
               colour="black",
               linetype="longdash",
               size=0.25) +
    gg.countline + gg.annotate +
    theme(legend.position=c(1,0.25),legend.justification=c(1,1)) +
    ylab(var.response) + delete_linetype_legend +
    ggtitle(title) 
  
  return(plt)
  
}


round_down_to_100_levels <- function(vectortoround){
  #############################################################################
  # FUNCTION: round_down_to_100_levels
  #
  #   actual_pred_plot requires a function for binning continuous numerical variables 
  #   into discrete buckets. Because of the way data.table (specifically setkey) 
  #   works, we need to upgroup numeric variables sensibly before summarizing.
  #   Here we simply try rounding less and less finely until the result has less 
  #   than 100 groups. Maintains the distribution/shape of var.by (unlike decile
  #   binning). Pulled this out of actual_pred_plot into its own function for easier 
  #   maintenance. In the future I will modularize more of actual_pred_plot. Plan to add 
  #   check to determine numerical v categorical vars and handle appropriately.
  #
  #############################################################################
  
  # try rounding to these numbers in sequence
  toTheNearest <- c(0.01, 0.02, 0.05, 0.1, 1, 2,
                    5, 10, 20, 50, 100, 200, 500, 1000, 2000, 5000, 10000)
  # initialise i to step through above attempts
  i <- 1
  
  # only round if you need to 
  if(length(unique(vectortoround)) > 100){ 
    
    # initialise new rounded version of variable
    rounded <- vectortoround 
    
    # keep trying til one sticks, if a var won't round to 2000 you're done for
    while(length(unique(rounded)) > 100){
      rounded <- round(vectortoround / toTheNearest[i]) * toTheNearest[i]
      i <- i + 1
    }
    print(paste0("Rounded to nearest ", as.character(toTheNearest[i-1])))
    return(rounded)
  }
  return(vectortoround)
}


keep_n_top <- function(factor, n){
  ############################################################################
  # FUNCTION: keep_n_top 
  #    
  #   Treat a factor so that it retains only the n-1 most exposed levels.
  # 
  # ARGUMENTS:
  #   factor:  factor variable to modify
  #   n:  number of levels to retain
  # RETURNS:
  #   Factor as input with the n - 1 most exposed levels and an 'Other' level
  #
  ############################################################################
  
  top <- table(factor)
  top <- top[order(top, decreasing=TRUE)][1:(n - 1)]
  factor <- as.character(factor)
  factor[!factor %in% names(top)] <- "Other"
  as.factor(factor)
  
}