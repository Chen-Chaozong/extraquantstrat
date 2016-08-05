#'
#'  add a stop loss signal to a strategy
#' 
#' Actually, it's a special kind rules build on normal rule function.  
#' It will apply after  \code{\link{applyStoplossSig}}  
#' and add stop loss rules in the rule space.
#' 
#' All stop rules depends on the trade signals.
#' @param strategy an object of type 'strategy' to add the rule to
#' @param sigcol name of the trade signal, must correspond to the signal lable
#' @param arguments named list of default arguments to be passed to an stop loss rule function when executed
#' @param parameters vector of strings naming parameters to be saved for apply-time definition
#' @param label arbitrary text label for stop loss rule output, NULL default will be converted to '<name>.stop'
#' @param type one of "stopLimit","stopPercent","stopVolatility","stopTargetperiod" ,for now "stopVolatility" is unavailable
#' @param ... any other passthru parameters
#' @param enabled TRUE/FALSE whether the rule is enabled for use in applying the strategy, default TRUE
#' @param indexnum if you are updating a specific rule, the index number in the $rules[type] list to update
#' @param store TRUE/FALSE whether to store the strategy in the .strategy environment, or return it.  default FALSE
#' @return if \code{strategy} was the name of a strategy, the name. It it was a strategy, the updated strategy. 
#' @export
add.stoplosssig <- function(strategy, sigcol, arguments, parameters=NULL, label=NULL, type=c(NULL,"stopLimit","stopPercent","stopVolatility","stopTargetperiod"), ..., enabled=TRUE, indexnum=NULL, store=FALSE) {
  if (!is.strategy(strategy)) {
    strategy<-try(getStrategy(strategy))
    if(inherits(strategy,"try-error"))
      stop ("You must supply an object or the name of an object of type 'strategy'.")
    store=TRUE
  } 
  type=type[1]
  if(is.null(type)) stop("You must specify a type")
  if(is.na(charmatch(type,c("stopLimit","stopPercent","stopVolatility","stopTargetperiod")))) stop(paste("type:",type,' must be one of "stopLimit","stopPercent","stopVolatility","stopTargetperiod"'))
  
  tmp_stoploss<-list()
  tmp_stoploss$sigcol<-sigcol
  tmp_stoploss$type<-type
  # if we have a 'label', that will be the name of the stop loss indicator, if it already exists, 
  #     it will be overwritten.  If label is NULL the indicator name will be "<name>.stop" 
  #     unless that already exists in which case we will append that with a number. 
  if(is.null(label)) {
    label <- paste(sigcol,"stop",sep='.')
    gl <- grep(label, names(strategy$stoploss))
    if (!identical(integer(0), gl)) label <- paste(label, length(gl)+1, sep=".")
  }   
  tmp_stoploss$label<-label
  tmp_stoploss$enabled=enabled
  if (!is.list(arguments)) stop("arguments must be passed as a named list")
  tmp_stoploss$arguments<-arguments
  tmp_stoploss$arguments$sigcol <- sigcol
  if(!is.null(parameters)) tmp_stoploss$parameters = parameters
  if(length(list(...))) tmp_stoploss<-c(tmp_stoploss,list(...))
  
  if(!hasArg(indexnum) || (hasArg(indexnum) && is.null(indexnum))) indexnum = length(strategy$stoploss)+1
  indexnum <- if (!is.null(indexnum)) {indexnum} else label 
  
  tmp_stoploss$call<-match.call()
  class(tmp_stoploss)<-'strat_stoploss'
  
  strategy$stoploss[[indexnum]]<-tmp_stoploss
  
  if (store) assign(strategy$name,strategy,envir=as.environment(.strategy))
  else return(strategy)
  strategy$name
}


#' apply the stop loss signal in the strategy to arbitrary market data
#' 
#' \code{applyStoplossSig} will take the \code{mktdata} object, 
#' and will apply each stop loss signal specified in the strategy definition to it.
#' 
#' @param strategy an object of type 'strategy' to add the indicator to
#' @param mktdata an xts object containing market data.  depending on indicators, may need to be in OHLCV or BBO formats
#' @param parameters named list of parameters to be applied during evaluation of the strategy
#' @param ... any other passthru parameters
#' @param enabled TRUE/FALSE whether the signal is enabled for use in applying the strategy, default TRUE
#' @return \code{mktdata} with stop loss signal colums added.
#' @export
applyStoplossSig <- function(strategy, mktdata, parameters=NULL, ..., enabled=TRUE) {
  # ensure no duplicate index values in mktdata
  if(any(diff(.index(mktdata)) == 0)) {
    warning("'mktdata' index contains duplicates; calling 'make.index.unique'")
    mktdata <- make.index.unique(mktdata)
  }
  
  if (!is.strategy(strategy)) {
    strategy<-try(getStrategy(strategy))
    if(inherits(strategy,"try-error"))
      stop ("You must supply an object of type 'strategy'.")
  } 
  ret <- NULL
  
  for (sl in strategy$stoploss){
    if(is.function(sl$type)) {
      slFun <- sl$type
    } else {
      if(exists(sl$type, mode="function")) {
        slFun <- get(sl$type, mode="function")
      } else {
        sig.type <- paste("sig", sl$type, sep=".")
        if(exists(sig.type, mode="function")) {
          slFun <- get(sig.type, mode="function")
          sl$type <- sig.type
        } else {
          message("Skipping stop loss rule ", sl$type,
                  " because there is no function by that name to call")
          next
        }
      }
    }
    
    if(!isTRUE(sl$enabled)) next()
    
    tmp_args <- sl$arguments
    
    if(is.null(sl$arguments$orderside) & !isTRUE(sl$arguments$orderqty == 0))
    {
      if (sl$arguments$orderqty>0 ){
        #we have a long position
        tmp_args$orderside<-'long'
      } else if (sl$arguments$orderqty<0){
        #we have a short position
        tmp_args$orderside<-'short'
      } 
    } else tmp_args$orderside <- sl$arguments$orderside        
    
    # replace default function arguments with sl$arguments
    # .formals <- formals(sl$name)
    # .formals <- modify.args(.formals, tmp_args, dots=TRUE)
    # # now add arguments from parameters
    # .formals <- modify.args(.formals, parameters, dots=TRUE)
    # # now add dots
    # .formals <- modify.args(.formals, NULL, ..., dots=TRUE)
    # # remove ... to avoid matching multiple args
    # .formals$`...` <- NULL
    
    tmp_val <- do.call(slFun, tmp_args)
    
    #add label
    if(is.null(colnames(tmp_val)))
      colnames(tmp_val) <- seq(ncol(tmp_val))
    if(!identical(colnames(tmp_val),tmp_args$label)) 
      colnames(tmp_val) <- tmp_args$label
    
    if (nrow(mktdata)==nrow(tmp_val) | length(mktdata)==length(tmp_val)) {
      # the stoploss returned a time series, so we'll name it and cbind it
      mktdata<-cbind(mktdata,tmp_val)
    } else {
      # the stoploss returned something else, add it to the ret list
      if(is.null(ret)) ret<-list()
      ret[[sl$name]]<-tmp_val
    }
  } #end stop loss loop
  mktdata<<-mktdata
  if(is.null(ret)) {
    return(mktdata)
  }
  else return(ret)
}

#' generate a limit stop loss signal
#' 
#' @param sigcol the column name of a trade signal
#' @param label text label to apply to the output
#' @param data data to apply
#' @param column named column to apply stop loss signal
#' @param limit limit loss of price
#' @param type one of c("static","dynamic")
#' @param orderside one of c("long","short") the orderside of the trade signal
#' @param offset1 numeric offset to be added to the first column prior to comparison
#' @param offset2 numeric offset to be added to the second column prior to comparison
#' @param ... any other passthru parameters
#' @export
stopLimit <- function(sigcol,label,data=mktdata, column, limit,type=c("static","dynamic"),orderside=c("long","short"), offset1=0, offset2=0, ...) {
  ret_stop = FALSE
  datarow <- NROW(data)
  tmp_ind <- NULL
  datanames <- colnames(data)
  column <- datanames[match.names(column,datanames)]
  if(orderside=="long"){
    if(type=="static"){
      for(i in 1:datarow){
        tmp_p <- tail(data[data[1:i,sigcol]==1,column],1)
        if(!is.numeric(tmp_p)){
          tmp_ind[i] <-  NA    
        } else{
          tmp_ind[i] <- tmp_p - limit    
        }
      }
    } else {
      for(i in 1:datarow){
        lastsig <- tail(which(data[1:i,sigcol]==1),1)
        if(!length(lastsig)){
          tmp_ind[i] <-  NA    
        } else{
          tmp_ind[i] <- max(data[lastsig:i,column]) - limit    
        }
      } 
    }
    tmp_data <- cbind(data[,column],reclass(tmp_ind,data) )
    colnames(tmp_data) <- c(column,label)
    ret_stop = suppressWarnings(ret_stop | diff(sigComparison(label=label,data=tmp_data,columns=names(tmp_data),relationship="lt",offset1=offset1, offset2=offset2))==1)
  } else {
    if(type=="static"){
      for(i in 1:datarow){
        tmp_p <- tail(data[data[1:i,sigcol]==1,column],1)
        if(!is.numeric(tmp_p)){
          tmp_ind[i] <-  NA    
        } else{
          tmp_ind[i] <- tmp_p + limit    
        }
      }
    } else {
      for(i in 1:datarow){
        lastsig <- tail(which(data[1:i,sigcol]==1),1)
        if(!length(lastsig)){
          tmp_ind[i] <-  NA    
        } else{
          tmp_ind[i] <- max(data[lastsig:i,column]) + limit    
        }
      }
    }
    tmp_data <- cbind(data[,column],reclass(tmp_ind,data) )
    colnames(tmp_data) <- c(column,label)
    ret_stop = suppressWarnings(ret_stop | diff(sigComparison(label=label,data=tmp_data,columns=names(tmp_data),relationship="gt",offset1=offset1, offset2=offset2))==1)
  }
  is.na(ret_stop) <- which(!ret_stop)
  colnames(ret_stop)<-label
  return(ret_stop)
}


#' generate a percent stop loss signal
#' 
#' @param sigcol the column name of a trade signal
#' @param label text label to apply to the output
#' @param data data to apply
#' @param column named column to apply stop loss signal
#' @param percent max percent loss
#' @param type one of c("static","dynamic")
#' @param orderside one of c("long","short") the orderside of the trade signal
#' @param offset1 numeric offset to be added to the first column prior to comparison
#' @param offset2 numeric offset to be added to the second column prior to comparison
#' @param ... any other passthru parameters
#' @export
stopPercent <- function(sigcol,label,data=mktdata, column, percent,type=c("static","dynamic"),orderside=c("long","short"), offset1=0, offset2=0, ...) {
  ret_stop = FALSE
  datarow <- NROW(data)
  tmp_ind <- NULL
  datanames <- colnames(data)
  column <- datanames[match.names(column,datanames)]
  if(orderside=="long"){
    if(type=="static"){
      for(i in 1:datarow){
        tmp_p <- tail(data[data[1:i,sigcol]==1,column],1)
        if(!is.numeric(tmp_p)){
          tmp_ind[i] <-  NA    
        } else{
          tmp_ind[i] <- tmp_p*(1-percent)   
        }
      }
    } else {
      for(i in 1:datarow){
        lastsig <- tail(which(data[1:i,sigcol]==1),1)
        if(!length(lastsig)){
          tmp_ind[i] <-  NA    
        } else{
          tmp_ind[i] <- max(data[lastsig:i,column])*(1-percent)    
        }
      }
    }
    tmp_data <- cbind(data[,column],reclass(tmp_ind,data) )
    colnames(tmp_data) <- c(column,label)
    ret_stop = suppressWarnings(ret_stop | diff(sigComparison(label=label,data=tmp_data,columns=names(tmp_data),relationship="lt",offset1=offset1, offset2=offset2))==1)
  } else {
    if(type=="static"){
      for(i in 1:datarow){
        tmp_p <- tail(data[data[1:i,sigcol]==1,column],1)
        if(!is.numeric(tmp_p)){
          tmp_ind[i] <-  NA    
        } else{
          tmp_ind[i] <- tmp_p*(1+percent)    
        }
      }
    } else {
      for(i in 1:datarow){
        lastsig <- tail(which(data[1:i,sigcol]==1),1)
        if(!length(lastsig)){
          tmp_ind[i] <-  NA    
        } else{
          tmp_ind[i] <- max(data[lastsig:i,column])*(1+percent)    
        }
      }
    }
    tmp_data <- cbind(data[,column],reclass(tmp_ind,data) )
    colnames(tmp_data) <- c(column,label)
    ret_stop = suppressWarnings(ret_stop | diff(sigComparison(label=label,data=tmp_data,columns=names(tmp_data),relationship="gt",offset1=offset1, offset2=offset2))==1)
  }
  is.na(ret_stop) <- which(!ret_stop)
  colnames(ret_stop)<-label
  return(ret_stop)
}



#' generate a traget period stop loss signal
#' 
#' @param sigcol the column name of a trade signal
#' @param label text label to apply to the output
#' @param data data to apply
#' @param period target stop loss period
#' @param ... any other passthru parameters
#' @export
stopTargetperiod <- function(sigcol,label,data=mktdata, period, ...) {
  ret_stop = Lag(data[,sigcol],period)
  is.na(ret_stop) <- which(!ret_stop)
  colnames(ret_stop)<-label
  return(ret_stop)
}



#' add a stop loss rule to a strategy
#'
#' @param strategy an object of type 'strategy' to add the rule to
#' @param name name of the stop loss rule, must correspond to an R function
#' @param arguments named list of default arguments to be passed to an rule function when executed
#' @param parameters vector of strings naming parameters to be saved for apply-time definition
#' @param label arbitrary text label for rule output, NULL default will be converted to '<name>.rule'
#' @param parent the label of the parent rule for a chain rule, unavailable
#' @param type should be exit, and it is default
#' @param ... any other passthru parameters
#' @param enabled TRUE/FALSE whether the rule is enabled for use in applying the strategy, default TRUE
#' @param indexnum if you are updating a specific rule, the index number in the $rules[type] list to update
#' @param path.dep TRUE/FALSE whether rule is path dependent, default TRUE, see Details 
#' @param timespan an xts/ISO-8601 style \emph{time} subset, like "T08:00/T15:00", see Details
#' @param store TRUE/FALSE whether to store the strategy in the .strategy environment, or return it.  default FALSE
#' @param storefun TRUE/FALSE whether to store the function in the rule, default TRUE.  setting this option to FALSE may slow the backtest, but makes \code{\link{debug}} usable
#' @return if \code{strategy} was the name of a strategy, the name. It it was a strategy, the updated strategy. 
#' @export 
add.stoplossrule <- function(strategy, name, arguments, parameters=NULL, label=NULL, parent=NULL, type="exit",..., enabled=TRUE, indexnum=NULL, path.dep=TRUE, timespan=NULL, store=FALSE, storefun=TRUE) {
  if (!is.strategy(strategy)) {
    strategy<-try(getStrategy(strategy))
    if(inherits(strategy,"try-error"))
      stop ("You must supply an object or the name of an object of type 'strategy'.")
    store=TRUE
  } 
   type <- "exit"
  # if(is.null(type)) stop("You must specify a type")
  # if(is.na(charmatch(type,c("risk","order","rebalance","exit","enter","chain","pre","post")))) stop(paste("type:",type,' must be one of "risk", "order", "rebalance", "exit", "enter", "chain", "pre", or "post"'))
  tmp_rule<-list()
  if(!is.function(name) && isTRUE(storefun)) {
    if(exists(name, mode="function")) {
      fn <- get(name, mode="function")
    } else {
      rule.name <- paste("rule", name, sep=".")
      if(exists(rule.name, mode="function")) {
        fn <- get(rule.name, mode="function")
        name <- rule.name
      } else {
        message("Skipping rule ", name," because there is no function by that name to call")
        next
      }
    }
  } else {
    fn <- name
  }
  if(is.null(arguments$orderside)){
    message("Skipping rule! orderside", name,' must be one of "long","short"')
    next
  } else if(is.na(charmatch(arguments$orderside,c("long","short")))){
    message("Skipping rule! orderside", name,' must be one of "long","short"')
    next
  } 
  tmp_rule$name<-fn
  tmp_rule$type<- type
  # if(type == 'chain')
  # {
  #   if(is.null(parent)) stop("You must specify the label of the parent rule if ruletype=='chain'")
  #   tmp_rule$parent<-parent
  # }
  tmp_rule$enabled<-enabled
  if (!is.list(arguments)) stop("arguments must be passed as a named list")
  if(is.null(label)) label = paste(name,"stoprule",sep='.')
  tmp_rule$label<-label
  if(!is.null(arguments$threshold)) arguments$threshold <- NULL
  if(is.numeric(arguments$orderqty)) arguments$orderqty <- arguments$orderqty
  tmp_rule$arguments<-arguments
  if(!is.null(parameters)) tmp_rule$parameters = parameters
  if(!is.null(timespan)) tmp_rule$timespan = timespan
  tmp_rule$path.dep<-path.dep
  if(length(list(...))) tmp_rule<-c(tmp_rule,list(...))
  
  tmp_rule$call<-match.call()
  class(tmp_rule)<-'trade_rule'
  if(!hasArg(indexnum) | (hasArg(indexnum) & is.null(indexnum))) indexnum = length(strategy$rules[[type]])+1
  strategy$rules[[type]][[indexnum]]<-tmp_rule
  
  tmp_rule<-tmp_rule
  
  if (store) assign(strategy$name,strategy,envir=as.environment(.strategy))
  else return(strategy)
  strategy$name
}