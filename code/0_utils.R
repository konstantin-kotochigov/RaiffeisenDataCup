require(plyr)
require(randomForest)

# To compile with manual use Roxygen

#' Get variables class information
#'
#' Creates data frame with info about dataset columns including proposed conversions and optionally convert
#'
#' @param data data frame with variables of interest
#' @param y.name target variable
#' @param max.cardinality maximum levels of numeric variable to be marked for conversion to factor
#' @param convert if not set - return information about variables; if set - return converted data frame and print information
#' @examples
#' df.cols <- classify.columns(df)
#' @return data frame with column types and statistics
#' @export
classify.columns <- function(data, y.name="target", max.cardinality=6)
{
  df <- data.frame(var=colnames(data), current.class=rep("",ncol(data)), proposed.class=rep("",ncol(data)), lev=rep(0,ncol(data)), missing=rep(0,ncol(data)), format=rep('',ncol(data)), stringsAsFactors = FALSE)
  df$N <- nrow(data)
  for (i in 1:ncol(data))
  {
    
    cat(paste("\rProcessing column ",i,"/",ncol(data),"\r",sep=""))
    
    df$lev[i] <- length(levels(as.factor(data[,i])))
    df$current.class[i] <- class(data[,i])
    df$missing[i] <- ifelse(df$current.class[i]=='factor', sum(data[,i]==""), sum(is.na(data[,i])))
    
    # Check for Date columns (of the form digits, not-digit, digits, not-digit, digits)
    if (sum(grepl("^\\d{1,4}[^0-9]\\d{1,2}[^0-9]\\d{1,4}$", data[,i])) > 10)
      df$proposed.class[i] <- 'Date'
    
  }
  df$current.class[df$var==y.name] <- df$proposed.class[df$var %in% y.name] <- "target"
  df$proposed.class[df$current.class %in% c("numeric","integer")] <- ifelse(df$lev[df$current.class %in% c("numeric","integer")] < max.cardinality, "factor", "numeric")
  df$proposed.class[grepl("woe.", df$var)] <- "numeric"
  df$proposed.class[df$current.class %in% c("factor","character","logical") & df$proposed.class!="Date"] <- "factor"
  df$proposed.class[df$current.class %in% c("Date")] <- "Date"
  df$proposed.class[df$lev == df$N] <- "id"
  
  return (df)
  
}

#' Get proposed type conversions
#'
#' Selects conversions suggested by classify.columns function
#'
#' @param df output of classify.columns function
#' @examples
#' get.proposed.conversions(classify.columns(df))
#' @return data frame with variables to convert
#' @export
get.proposed.conversions <- function(df)
{
  if (sum(df$current.class != df$proposed.class) == 0)
    print("All variables are appropriate")
  else
    return(df[df$current.class != df$proposed.class,])
}

#' Deduct date format
#' @export
get.date.format <- function(var)
{
  
  require(stringr)
  
  if (sum(grepl('^\\d{1,2}.\\d{1,2}.\\d{4}$', var)) > 10)
  {
    delimiter <- setdiff(unique(substr(var,nchar(as.character(var))-4,nchar(as.character(var))-4)), c(NA, ""))[1]
    m <- max(as.numeric(str_extract(substr(var,1,2), "\\d+")), na.rm=TRUE)
    return (ifelse(m>12,paste("%d","%m","%Y",sep=delimiter),paste("%m","%d","%Y",sep=delimiter)))
  }
  
  if (sum(grepl('^\\d{1,2}.\\d{1,2}.\\d{2}$', var)) > 10)
  {
    delimiter <- setdiff(unique(substr(var,nchar(as.character(var))-2,nchar(as.character(var))-2)), c(NA, ""))[1]
    m <- max(as.numeric(str_extract(substr(var,1,2), "\\d+")), na.rm=TRUE)
    return (ifelse(m>12,paste("%d","%m","%y",sep=delimiter),paste("%m","%d","%y",sep=delimiter)))
  }
  
  if (sum(grepl('^\\d{4}.\\d{1,2}.\\d{1,2}$', var)) > 10)
  {
    delimiter <- setdiff(unique(substr(var,5,5)), c(NA, ""))[1]
    return (paste("%Y","%m","%d",sep=delimiter))
  }
  
  return (NA)
  
}


#' Convert columns
#'
#' Converts dataset columns according to proposed mapping
#'
#' @param df - data frame with information about variables (may be result of classify.columns function)
#' @param data - data frame to convert
#'
#' @examples
#' df <- convert.columns(classify.columns(df), df)
#'
#' @return data frame with mapping (variable value) -> (variable group)
#
# ----------------------------------------------------------------
#' @export
convert.columns <- function(df, data)
{
  convert.to.factor <- df$var[df$current.class != df$proposed.class & df$proposed.class=="factor"]
  data[,convert.to.factor] <- as.data.frame(apply(data[,convert.to.factor], 2, function(x) as.factor(x)))
  print(paste("Converted ", length(convert.to.factor)," columns to factor"))
  
  convert.to.numeric <- df$var[df$current.class != df$proposed.class & df$proposed.class=="numeric"]
  data[,convert.to.numeric] <- as.data.frame(apply(data[,convert.to.numeric], 2, function(x) as.numeric(x)))
  print(paste("Converted ", length(convert.to.numeric)," columns to numeric"))
  
  convert.to.date <- df$var[df$current.class != df$proposed.class & df$proposed.class=="Date"]
  for (var in convert.to.date)
    data[,var] <- as.Date(data[,var], get.date.format(data[,var]))
  print(paste("Converted ", length(convert.to.date)," columns to Date"))
  
  # Generate
  # if (is.null(get.date.columns(data))==FALSE) generate.date.features(data, get.date.columns(data))
  return(data)
}

#' Get names of all numeric columns
#'
#' @param data data frame of interest
#' @return vector of numeric variable names
#' @examples
#' df.numerics <- get.numeric.columns(df)
#' @export
get.numeric.columns <- function(data)
{
  result <- c()
  for (i in 1:length(colnames(data)))
  {
    if (class(data[,i]) %in% c("numeric","integer")) result <- c(result, colnames(data)[i])
  }
  return(result)
}

#' Get names of all factor columns
#'
#' @param data data frame of interest
#' @return vector of factor variable names
#' @examples
#' df.numerics <- get.factor.columns(df)
#' @export
get.factor.columns <- function(data)
{
  result <- c()
  for (i in 1:length(colnames(data)))
  {
    if (class(data[,i]) == "factor") result <- c(result, colnames(data)[i])
  }
  return(result)
}

#' Get names of all date columns
#'
#' @param data data.frame of interest
#' @return vector of date variable names
#' @examples
#' df.numerics <- get.date.columns(df)
#' @export
get.date.columns <- function(data)
{
  result <- c()
  for (i in 1:length(colnames(data)))
  {
    if (class(data[,i]) == "Date") result <- c(result, colnames(data)[i])
  }
  return(result)
}

#' Generate date features
#'
#' Generates features based on columns of date type. Creates year, month and day features both numeric and factor.
#'
#' @param data data frame of interest
#' @param date.variables vector of date variable names
#' @param overwrite let function overwrite already existing variables with the same names
#' @return data frame with new generated variables
#' @examples
#' df <- generate.date.features(df, c('date1','date2'))
#' @export
generate.date.features <- function(data, date.variables, overwrite=FALSE)
{
  for (i in date.variables)
    if (class(data[,i]) != "Date")
      stop("All variables must of class Date")
  
  for (i in date.variables)
    for (j in c(".month.num",".day.num",".year.num",".month",".day",".year"))
      if (paste(i,j,sep="") %in% colnames(data))
        if (overwrite)
          warning(paste("Variable '", paste(i,j,sep=""),"' is already defined in input data frame", sep=""))
  else
    stop(paste("Variable '", paste(i,j,sep=""),"' is already defined in input data frame. To overwrite specify parameter 'overwrite=TRUE'.", sep=""))
  
  result <- data
  
  for (i in date.variables)
  {
    result[,paste(i,".month.num",sep="")] <- as.numeric(format(data[,i], "%m"))
    result[,paste(i,".day.num",sep="")] <- as.numeric(format(data[,i], "%d"))
    result[,paste(i,".year.num",sep="")] <- as.numeric(format(data[,i], "%Y"))
    result[,paste(i,".month",sep="")] <- factor(format(data[,i], "%m"))
    result[,paste(i,".day",sep="")] <- factor(format(data[,i], "%d"))
    result[,paste(i,".year",sep="")] <- factor(format(data[,i], "%Y"))
  }
  return(result)
}

#' Calculate Chi-squared importance of each attribute
#'
#' Creates a data frame with calculated chi-squared difference between good class and bad class for each factor
#'
#' @param x data frame with variables of interest (all must be factors)
#' @param y target variable
#'
#' @return data frame with variable names sorted by chi-squared statistics
#' @export
get.chisq.importance <- function(x, y)
{
  variables <- colnames(x)
  df <- data.frame(var=rep('',length(variables)), chisq=rep(0,length(variables)), stringsAsFactors = FALSE)
  for (i in 1:length(variables))
  {
    df$var[i] <- variables[i]
    df$chisq[i] <- round(as.numeric(chisq.test(table(x[,variables[i]], y))$statistic),2)
  }
  df <- df[order(df$chisq, decreasing=TRUE),]
  rownames(df) <- NULL
  return(df)
}

#' Calculate AUC importance of each attribute
#'
#' Creates a data frame with calculated AUC difference between good class and bad class for each variable
#'
#' @param x data frame with variables of interest (all must be numerics)
#' @param y target variable
#'
#' @return data frame with variable names sorted by AUC statistics
#' @export
get.auc.importance <- function(x,y)
{
  
  require(AUC)
  
  variables <- colnames(x)
  n.variables <- length(variables)
  df <- data.frame(var=rep('',n.variables), auc=rep(NA,n.variables), fill.rate=rep(NA,n.variables), stringsAsFactors = FALSE)
  for (i in 1:n.variables)
  {
    cat(paste("\rProcessing variable [",i,"/",n.variables,"]\r",sep=""))
    df$var[i] <- variables[i]
    if ((class(x[,i]) %in% c("numeric","integer"))==FALSE) next
    df$auc[i] <- round(AUC::auc(AUC::roc(x[,i],y)),5)
    df$fill.rate[i] <- 1 - round((sum(is.na(x[,i])) / nrow(x)), 5)
  }
  cat("\n")
  df <- df[order(abs(df$auc-0.5), decreasing=TRUE),]
  rownames(df) <- NULL
  return(df)
}

#' Calculate RandomForest importance of each attribute
#'
#' Creates a data frame with calculated RandomForest importance for each variable
#'
#' @param x data frame with variables of interest
#' @param y target variable
#'
#' @return data frame with variable names sorted by RandomForest statistics
#' @export
get.rf.importance <- function(x,y){
  
  require(randomForest)
  
  print("Fitting Random Forest on all data. This might take some time.")
  rf.fit <- randomForest(x, y, ntree=100, importance=TRUE)
  result <- as.data.frame(rf.fit$importance[,c('MeanDecreaseAccuracy','MeanDecreaseGini')])
  result$var <- rownames(result)
  rownames(result) <- NULL
  result <- result[order(result$MeanDecreaseAccuracy, decreasing=TRUE), c('var','MeanDecreaseAccuracy','MeanDecreaseGini')]
  
  return(result)
}

#' Calculate combined importance measure of each attribute
#'
#' Creates a data frame with calculated chi-squared, AUC and RandomForest measures combined into one score and sorted by it
#'
#' @param x data frame with variables of interest
#' @param y target variable
#'
#' @return data frame with variable names sorted by combined importance statistics
#' @export
get.var.importance <- function(x,y)
{
  
  print("Computing AUC based importance")
  auc.imp <- get.auc.importance(x, y)
  rownames(auc.imp) <- NULL
  auc.imp$num <- as.numeric(rownames(auc.imp))
  
  print("Computing Random Forest based importance")
  rf.imp <- get.rf.importance(x, y)
  rownames(rf.imp) <- NULL
  rf.imp$num <- as.numeric(rownames(rf.imp))
  
  print("Computing Chi-squared based importance")
  chisq.imp <- get.chisq.importance(x, y)
  rownames(chisq.imp) <- NULL
  chisq.imp$num <- as.numeric(rownames(chisq.imp))
  
  print("Preparing result table")
  x <- merge(auc.imp[,c('var','num')], rf.imp[,c('var','num')], by='var', all.x=TRUE, all.y=TRUE)
  x <- merge(x, chisq.imp[,c('var','num')], by='var', all.x=TRUE, all.y=TRUE)
  colnames(x) <- c("VAR","AUC.RANK","RF.RANK","CHISQ.RANK")
  x$TOTAL.RANK <- x$AUC.RANK + x$RF.RANK + x$CHISQ.RANK
  x<-x[order(x$TOTAL.RANK),]
  rownames(x) <- NULL
  
  return(x)
  
}

# compute.woe <- function(factor, target)
# {
#
#   if (class(target) != "factor")
#     stop("target variable must be factor")
#
#   if (class(factor) != "factor")
#     stop("factor variable must be factor")
#
#   x <- table(factor, target)
#   n.categories <- nrow(x)
#   x <- cbind(x,rep(0,n.categories))
#   x <- cbind(x,rep(0,n.categories))
#   x <- cbind(x,rep(0,n.categories))
#   x <- cbind(x,rep(0,n.categories))
#   colnames(x) <- c('0','1','rate','avg','lift','woe')
#   x <- rbind(x,c(0,0,0,0,0,0))
#   rownames(x)[n.categories+1] <- 'Total'
#   x[n.categories+1,1] <- sum(x[1:n.categories,1])
#   x[n.categories+1,2] <- sum(x[1:n.categories,2])
#   x[,3] <- round(x[,2] / ( x[,1] + x[,2] ), 2)
#   x[,4] <- x[n.categories+1,3]
#   x[,5] <- round(x[,3] / x[,4], 2)
#   x[,6] <- log((x[,2] / x[,1]) / (x[n.categories+1,2] / x[n.categories+1,1]))
#
#   x <- x[c(order(x[1:n.categories,5], decreasing=TRUE),n.categories+1),]
#
#   return (x)
#
# }

# ----------------------------------------------------------------
#
# Calculate performance for each top-n subset of variables using RandomForest Fitting
#
# Function "get.chisq.importance"
#
#   input:
#     data - covariates (numerics and factors with <53 levels)
#     sorted.variables - vector of variables sorted by their importance
#     y.name - name of target variable
#     analyze.top.n - limit calculations to top "analyze.top.n" variables
#     RF.trees - tune confidence by setting number of trees for RandomForest fitting
#     times - number of repetitions to get more stable results
#     trainSplit - how to partition dataset: rate of training examples
#     logging - "sparse" - only control information / "dense" - all information
#
#   output:
#     data frame with performance measures (accuracy and for binary models - AUC) for each subset
#
# ----------------------------------------------------------------
#' Beta
#' @export
get.rf.subset.quality <- function(data, sorted.variables, y.name, analyze.top.n=50, RF.trees=100, times=1, trainSplit=0.7, logging='sparse'){
  
  variable.subsets <- data.frame(var=sorted.variables, accuracy=rep(0, length(sorted.variables)), auc=rep(0, length(sorted.variables)))
  
  for (current.repeat in 1:times)
  {
    
    print(paste("Iteration ", current.repeat))
    
    trainIndex <- createDataPartition(data[,y.name], p=trainSplit, list=FALSE)
    data.train <- data[trainIndex,]
    data.test <- data[-trainIndex,]
    
    if (length(levels(as.factor(data[,y.name]))) == 2)
      compute.auc=TRUE
    
    for (i in 2:analyze.top.n)
    {
      variable.subsets$var[i] <- sorted.variables[i]
      if (logging != 'sparse') print(paste("[Cycle ", current.repeat, "] Evalutaing for top ", i," subset: ", sorted.variables[i], sep=""))
      rf.fit <- randomForest(y=data.train[,y.name], x=data.train[,sorted.variables[1:i]], n.tree=RF.trees, importance=FALSE)
      
      # For all kinds of classification compute accuracy
      scores.class <- predict(rf.fit, data.test[,sorted.variables[1:i]], type="response")
      variable.subsets$accuracy[i] <- variable.subsets$accuracy[i] + confusionMatrix(data.test[,y.name], scores.class)$overall[1]
      
      # For binary classification compute AUCs
      if (compute.auc)
      {
        scores.prob <- predict(rf.fit, data.test[,sorted.variables[1:i]], type="prob")
        variable.subsets$auc[i] <- variable.subsets$auc[i] + roc(as.factor(data.test[,y.name]), scores.prob[,2])$auc
      }
      
      if (logging != 'sparse') print(paste("Accuracy=", round(variable.subsets$accuracy[i],5), sep=""))
    }
    
    #variable.subsets[,paste(accuracy,".", current.repeat, sep="")] <- variable.subsets$accuracy
  }
  
  variable.subsets$accuracy <- round(variable.subsets$accuracy / times, 5)
  variable.subsets$auc <- round(variable.subsets$auc / times, 5)
  
  plot(variable.subsets$auc[2:analyze.top.n], type='l')
  #predict(loess(~as.numeric(rownames(voip.money))))[1:2000]))
  return(variable.subsets)
}


#' Put rare factor values into separate group
#'
#' Sets rare values of a factor to new value "other". Thus limits the number of factor levels.
#'
#' @param x data vector
#' @param top.values number of bins to create (plus "other" group)
#' @param min.frequency if factor value is less requent than this value, then put it to "other" group
#' @examples
#' df$var <- group.factor(df$var, top.values=10)
#' df$var <- group.factor(df$var, min.frequency=100)
#' @return data frame with mapping (variable value) -> (variable group)
#' @export
group.factor <- function(x, top.values=10, min.frequency=NA)
{
  
  require(plyr)
  
  if (class(x)!="factor")
    stop("Variable must be factor")
  
  # If there are less levels in variable than "top.values" parameter
  top.values <- min(length(levels(x)), top.values)
  
  x <- data.frame(x)
  colnames(x) <- c("x")
  
  counts <- count(x)
  counts <- counts[order(counts$freq, decreasing=TRUE),]
  counts$group <- rep("", nrow(counts))
  rownames(counts) <- NULL
  
  # If we choose to select by minimum frequency, set top.values accordingly [1,n]
  if (!is.na(min.frequency))
  {
    if (sum(counts$freq >= min.frequency)==0)
    {
      top.values <- 0
    } else {
      top.values <- max(which(counts$freq >= min.frequency))
    }
  }
  
  # Set group values for top categories
  counts$group[1:top.values] <- as.character(counts$x[1:top.values])
  
  # Set "other" for all other categories
  if(nrow(counts) > top.values) counts$group[(top.values+1):nrow(counts)] <- rep("other", nrow(counts)-top.values)
  
  return(factor(join(x,counts[,c("x","group")], by="x", type="left")[[2]]))
  
}


#' Apply groupings to new data
#'
#' Groups factor values according to mapping generated as an output of a function group.factor. Thus the same grouping can be applied to new datasets.
#' Now it uses dataset itself as a mapping, which is not very efficient. In future releases a proper mapping will be generated and used.
#'
#' @param data data with already grouped data
#' @param variables factors to analyze
#' @param new.data new data frame with factor variables to convert
#' @param other.factor name of the "other" group
#' @examples
#' new.df <- apply.factor.grouping(df, c('var1','var2','var3'), new.df)
#' @return new data frame with added columns named group.X (where X is variable name)
#
# ----------------------------------------------------------------
#' @export
apply.factor.grouping <- function(data, variables, new.data, other.factor="other")
{
  for (i in 1:length(variables))
  {
    
    # if (length(intersect(paste("group.", variables, sep=""), new.data)) > 0)
    #  stop(paste("New data already contains variable",intersect(paste("group.", variables, sep=""),sep="")))
    
    # Use initial dataset as mapping (value -> group)
    group.map <- unique(data.frame(var=data[,variables[i]], group=as.character(data[,variables[i]]), stringsAsFactors=FALSE))
    
    # Set name for the new variable
    colnames(group.map) <- c("var", paste("group.",variables[i],sep=""))
    # colnames(group.map) <- c("var", variables[i])
    
    # Merge created group variable into the new dataset
    new.data <- merge(new.data, group.map, by.x=variables[i], by.y="var", all.x=TRUE)
    
    # Select just merged variable
    current.vector <- new.data[,paste("group.",variables[i], sep="")]
    # current.vector <- new.data[,variables[i]]
    
    # If the value in new dataset didn't match with the mapping, set group to "OTHER" (name of the group can be changed in parameters)
    current.vector[is.na(current.vector)] <- "other"
    
    # Set new group variable to factor
    # new.data[,paste("group.",variables[i], sep="")] <- as.factor(current.vector)
    new.data[,variables[i]] <- as.factor(current.vector)
    
    new.data[,paste("group.",variables[i], sep="")] <- NULL
    
  }
  return(new.data)
}

# TODO: change algorithm from RF to some classification model
#' Optimal factor cutoff (Beta)
#'
#' @export
optimal.factor.grouping <- function(x,y, fit.type="glm", max.levels=30)
{
  
  require(randomForest)
  require(AUC)
  require(caret)
  require(klaR)
  
  if (!("0" %in% levels(y) & "1" %in% levels(y)))
    stop("Please set target values to 0 and 1. Current version works only with numeric values")
  
  if (fit.type=="glm")
    print("Fit model = GLM (logistic regression)")
  if (fit.type=="rf")
    print("Fit model = Random Forest")
  
  
  trainIndex <- createDataPartition(y, p=0.7, list=FALSE)
  y.train <- y[trainIndex]
  y.test <- y[-trainIndex]
  result <- c()
  woes <- c()
  for (i in 1:max.levels)
  {
    cat(paste("\rProcessing group #",i,sep=""))
    x.grouped <- group.factor(x, top.values=i)
    x.train <- x.grouped[trainIndex]
    x.test <- x.grouped[-trainIndex]
    if (fit.type=="glm")
    {
      model.fit <- glm(formula("y~x"), data=data.frame(x=x.grouped, y=y), family=binomial(logit))
      model.pred <- predict(model.fit, data.frame(x=x.grouped), type="response")
      auc <- as.numeric(AUC:auc(AUC:roc(model.pred, y)))
      woes <- c(woes, woe(formula("y~x"), data=data.frame(x=x.grouped, y=y))$IV)
    }
    if (fit.type=="rf")
    {
      model.fit <- randomForest(data.frame(x=x.train), y.train, ntree=100)
      model.pred <- predict(model.fit, data.frame(x=x.test), type="prob")[,1]
      auc <- as.numeric(AUC:auc(AUC:roc(model.pred,y.test)))
    }
    
    result <- c(result, auc)
    
  }
  cat(paste(" => Optimal number of groups=",which.max(result)))
  cat("\n")
  return(as.vector(result))
}

# optimal.factor.grouping <- function(x,y, max.levels=30)
# {
#   result <- c()
#   for (i in 1:max.levels)
#   {
#     cat(paste("\rProcessing group #",i,sep=""))
#     x.grouped <- group.factor(x, top.values=i)
#     woe(y~x, data=data.frame(x,y), appont=TRUE)
#     result[length(result) + 1] <- abs(as.numeric(roc(x.grouped)$auc)-0.5)
#   }
#   print("\n")
#   abs(result-0.5)
#   return(result))
# }


# ----------------------------------------------------------------
#
# Calculate AUC for each individual variable (for binomial cases only)
#
# Function "get.auc"
#
#   input:
#     x - data frame with covariates
#     y - binary target variable
#
#   output:
#     data frame with covariates from x sorted by AUC values
#
# ----------------------------------------------------------------
#' @export
get.auc <- function(y,x)
{
  
  require(AUC)
  
  auc.df <- as.data.frame(sapply(x,function(a) AUC::auc(AUC::roc(a,y))))
  colnames(auc.df) <- "auc"
  auc.df$var <- rownames(auc.df)
  rownames(auc.df) <- NULL
  auc.df <- auc.df[,c('var','auc')]
  auc.df$gini <- 2*abs(auc.df$auc-0.5)
  auc.df <- auc.df[order(abs(auc.df$auc-0.5), decreasing=TRUE),]
  return(auc.df)
}

# ----------------------------------------------------------------
#
# Calculate AUC for model based on each top-n subset of sorted variables
#
# Function "get.top.subset"
#
#   input:
#     variables - sorted data frame of variable names
#     train.data - training data frame
#     test.data - test data frame
#
#   output:
#     data frame with covariates from x sorted by AUC values
#
# ----------------------------------------------------------------
#'
#' @export
get.top.subset <- function(variables, data, target)
{
  
  require(caret)
  
  subsets.df <- data.frame(
    subset.num<-rep("",length(variables)),
    auc<-rep(0, length(variables)),stringsAsFactors = FALSE)
  colnames(subsets.df) <- c("subset.num","auc")
  
  trainIndex <- createDataPartition(data[,target], p=0.7, list=FALSE)
  train.data <- data[trainIndex,]
  test.data <- data[-trainIndex,]
  
  for (i in 1:length(variables))
  {
    cat(paste("\rProcessing variable: ", i," / ",length(variables),sep=""))
    current.vars <- variables[1:i]
    subsets.df$subset.num[i] <- paste("Top-",i,sep="")
    glm.fit <- glm(formula=as.formula(paste(target,"~.")), data=train.data[,c(current.vars,target)], family=binomial(probit))
    score <- predict(glm.fit, test.data, type='response')
    subsets.df$auc[i] <- roc(test.data[,target], score)$auc
  }
  return(subsets.df)
}





# Impute missing values in variable by its non-missing sample
random.imp <- function (a){
  missing <- is.na(a)
  n.missing <- sum(missing)
  a.obs <- a[!missing]
  imputed <- a
  imputed[missing] <- sample(a.obs, n.missing, replace=TRUE)
  return (imputed)
}

mean.imp <- function (a){
  a[is.na(a)] <-  mean(a, na.rm=TRUE)
  return(a)
}

#' Set missing to random
#'
#' Sets missing values to values from a random sample, which repeats distribution of non-missing values
#'
#' @param data data frame of interest
#' @param variables variables with possibly missing values
#' @return data frame with missing values replaced by values from random sample
#' @export
set.missing.to.random <- function(data, variables)
{
  data[variables] <- as.data.frame(apply(data[variables], 2, function(x) {random.imp(x)}))
  return(data)
}

impute.missing.mean <- function(data, variables)
{
  data[variables] <- as.data.frame(apply(data[variables], 2, function(x) {mean.imp(x)}))
  return(data)
}

#' Standardize columns
#'
#' Calculates z-score for one or several variables
#'
#' @param data data.frame with variables of interest
#' @param variables vector of variables to process
#'
#' @return The same data frame with variables replaced with their z-scores
#'
#' @examples
#' standardize.columns(df, c('var1','var2','var3'))
#'
#' @export
standardize.columns <- function(data, variables)
{
  data[variables] <- as.data.frame(apply(data[variables], 2, function(x) {scale(x)}))
  return(data)
}

# ----------------------------------------------------------------
#
# Replace outliers
#
# Function "replace.outliers.mean"
#
#   input:
#     data - data frame with variables of interest
#     variables - vector of variables to replace outliers
#     sigma.cutoff - minimum deviation (in sigmas) to be considered as an outlier
#
#   output:
#     data frame with replaced outliers variables only
#
# ----------------------------------------------------------------
replace.outliers.mean.f <- function (a, sigma.cutoff, var.name)
{
  print(paste("replaced outliers: ", sum(abs(a)>sigma.cutoff), sep=""))
  a[abs(a)>sigma.cutoff] <-  mean(a, na.rm=TRUE)
  return(a)
}

replace.outliers.mean <- function(data, variables, sigma.cutoff=3)
{
  data[variables] <- as.data.frame(apply(data[variables], 2, function(x) {replace.outliers.mean.f(x,sigma.cutoff)}))
}

#' Replace numeric missing values with mean
#'
#' Replaces missing (NA) values with column mean. Only applicable for numerics.
#'
#' @param data - data frame with variables of interest
#' @param variables - vector of numeric variables to replace missing values
#' @return data frame with replaced missing values for selected variables
#'
#' @export
set.missing.to.mean <- function(data, variables)
{
  data.means <- colMeans(data[variables], na.rm=TRUE)
  for (i in variables)
    data[is.na(data[,i]),i] <- data.means[i]
  return(data)
}

#' Replace numeric missing values with zero
#'
#' Replaces missing (NA) values with zero. Only applicable for numerics.
#'
#' @param data data frame with variables of interest
#' @param variables vector of numeric variables to replace missing values
#'
#' @return data frame with replaced missing values for selected variables
#'
#' @export
set.missing.to.zero <- function(data, variables)
{
  for (i in variables)
    data[is.na(data[,i]),i] <- 0
  return(data)
}

#' Replace empty factor values with "NA"
#'
#' Replaces blank factor values with "NA". Recreates factor variable with new level.
#'
#' @param data data frame with variables of interest
#' @param variables vector of factors
#' @param logging flag to write detailed stats in standard output (default=FALSE)
#'
#' @return data frame with replaced missing values for selected variables
#'
#' @examples
#' set.missing.factors.to.NA(df, c('var1','var2','var3'), logging=TRUE)
#' @export
set.missing.factors.to.NA <- function(data, variables, logging=FALSE)
{
  for (i in variables)
  {
    if (class(data[,i]) != "factor") stop("All variables must be factors!")
    
    if (logging) print(paste("Replaced ",sum(is.na(data[,i]))," missing values for factor ", i))
    data[,i] <- as.character(data[,i])
    data[is.na(data[,i]), i] <- "NA"
    
    if (logging) print(paste("Replaced ",sum(data[,i]=="")," blank values for factor ", i))
    data[data[,i]=="", i] <- "NA"
    data[,i] <- factor(data[,i])
  }
  return(data)
}



#' Predict missing numeric values (beta)
#'
#' Uses regression to predict most probable value
#' Was not tested yet
#'
#' @export
set.missing.to.prediction <- function(x)
{
  for (current.var in colnames(x))
  {
    print(paste("Processing variable ", current.var, sep=""))
    missingIndex <- is.na(x[,current.var])
    non.missingIndex <- !is.na(x[,current.var])
    
    lm.fit <- lm(as.formula(paste(current.var,"~.",sep="")), data=x[non.missingIndex,])
    lm.pred <- as.vector(predict(lm.fit, x[missingIndex,]))
    
    print(paste("Could not impute for ", sum(is.na(lm.pred)), " cases out of ", length(lm.pred), sep=""))
    huy <-  x[, current.var]
    huy[missingIndex] <- lm.pred
    x[, current.var] <- huy
  }
  return(x)
}

#' Compute average AUC with cross-validation
#'
#' Repeats random split, fits GLM model on train dataset and computes AUC on test dataset and calculates average AUC
#' In further releases will be replaced with more efficient cross-validation techniques
#'
#' @param formula formula to fit regression on
#' @param data input data.frame
#' @param target.var target variable name
#' @param n number of repetitions
#' @param fit.type Type of model to use (GLM or RandomForest)
#' @param train.rate fraction of data to set as training (default=0.7)
#' @export
cv.glm <- function(formula, data, target.var, n, fit.type="glm", train.rate=0.7)
{
  
  require(caret)
  require(randomForest)
  require(AUC)
  
  if (fit.type=="glm")
    cat("Fitting method = GLM (logit)\n")
  if (fit.type=="rf")
    cat("Fitting method = Random Forest\n")
  
  auc <- c()
  # cases <- c()
  for (i in 1:n)
  {
    cat(paste("\rIteration [",i,"/",n,"]",sep=""))
    trainIndex <- createDataPartition(data[,target.var], p=train.rate, list=FALSE)
    trainData <- data[trainIndex,]
    testData <- data[-trainIndex,]
    if (fit.type=="glm")
    {
      cv.fit <- glm(formula, data=trainData, family = binomial(logit))
      testData$score <- predict(cv.fit, testData)
    }
    if (fit.type=="rf")
    {
      cv.fit <- randomForest(formula=formula, data=trainData, n.tree=100, proximity=FALSE)
      testData$score <- predict(cv.fit, testData, type="prob")[,1]
    }
    auc[i] <- as.numeric(AUC:auc(testData$score, testData[,target.var]))
    #cases[i] <- complete.cases(trainData[,as.character(attr(terms(formula(formula)), 'variables'))[-c(1,2)]])
  }
  cat("\nIteration Results:")
  print(auc)
  #cat("\n Results:")
  #print(auc)
  print(paste("Average AUC = ", as.character(round(mean(auc),4)), "; Standard Deviation = ", as.character(round(sd(auc), 4)), sep=""))
  return(mean(auc))
}


#' Beta
#'
#' @export
find.optimal.subset <- function(x, y)
{
  n.vars <- length(x)
  n.iters <- 100
  subset.sizes <- rexp(n.iters, r=0.25)
  for (i in subset.sizes)
  {
    current.subset <- sample(1:n.vars, i, replace=FALSE)
    rf.fit <- randomForest(y=y, x=x, ntree=100, importance=FALSE)
  }
  return(1)
}

#'  Create WOE mapping
#'
#'  Computes WOE
#'  If one group is empty, woe := 2 * max(WOE) (or 2 * min(WOE))
#'
#' @param x data.frame with varaibles
#' @param vars variables of interest (must be factors!)
#' @param y target variable
#' @examples
#' create.woe(df, c('var1','var2','var3'), df$target)
#' @return data.frame with WOE mapping (value->woe) that can be further applied to new datasets
#' @export
create.woe <- function(x, vars=colnames(x), y)
{
  
  for (i in vars)
    if (class(x[,i])!='factor')
      stop(paste(i," is not a factor! To compute WOEs all variables must be factors",sep=""))
  
  if (length(levels(y)) != 2)
    stop(paste("Target variable should be binary! Now it has ", length(levels(y)), " levels", sep=""))
  
  result <- data.frame(var=character(0), category=character(0), woe=numeric(0))
  for (i in 1:length(vars))
  {
    cat("\rCalculating WOE for variable ",i," / ",length(vars),"\r",sep="")
    
    d <- merge(data.frame(table(x[y==0,vars[i]])), data.frame(table(x[y==1,vars[i]])), by.x='Var1', by.y='Var1', all.x=TRUE, all.y=TRUE)
    d$var <- vars[i]
    d<-d[,c(4,1,2,3)]
    colnames(d) <- c('var','category','cnt.0','cnt.1')
    
    total0 <- sum(d[,'cnt.0'])
    total1 <- sum(d[,'cnt.1'])
    d$woe <- log((d[,'cnt.0'] / total0) / (d[,'cnt.1'] / total1))
    
    d$woe[d$woe==Inf] <- 2 * max(d$woe[d$woe!=Inf])
    d$woe[d$woe==-Inf] <- 2 * min(d$woe[d$woe!=-Inf])
    d$woe[d$woe==Inf] <- 1
    d$woe[d$woe==-Inf] <- -1
    
    d <- d[,c('var','category','woe')]
    result <- rbind(result, d)
  }
  cat("\n")
  return(result)
}

#' Apply WOE mapping to new data
#'
#' Replaces factor values with corresponding WOE scores
#'
#' @param x data.frame with factor values to replace
#' @param woe.map generated with create.woe (or manually) mapping
#' @examples
#' woe.apply(df, woe.map)
#' @export
woe.apply <- function(x, woe.map)
{
  
  if (nrow(woe.map[,c('var','category')]) != nrow(unique(woe.map[,c('var','category')])))
    stop("Non-unique categories. Check WOE mapping.")
  
  vars <- unique(woe.map$var)
  
  # Set row numbers as IDs (to merge intermediate results into initial table)
  rownames(x) <- NULL
  x$row <- rownames(x)
  
  for (i in 1:length(vars))
  {
    cat("\rApplying WOE to variable ",i,"/",length(vars),"\r",sep="")
    woe.var <- merge(data.frame(row=x$row, category=x[,vars[i]]), woe.map[woe.map$var==vars[i],c('category','woe')], by.x='category', by.y='category', all.x=TRUE)
    woe.var <- woe.var[,c('row','woe')]
    woe.var$woe <- ifelse(is.na(woe.var$woe)==TRUE, 0, woe.var$woe)
    colnames(woe.var) <- c('row',paste("woe.",vars[i],sep=""))
    x <- merge(x, woe.var, by.x='row', by.y='row', all.x=TRUE)
  }
  cat("\n")
  
  # Reset to initial sorting
  x <- x[order(x$row),]
  
  # Delete temporary attribute
  x$row <- NULL
  
  return (x)
}

#' Create cross-tabulation between factor and binary target
#'
#' Creates a table with stats about factor values. Also computes Lift and WOEs. Print totals. Sorts result by lift.
#'
#' @param factor factor variable
#' @param target binary target variable
#' @return Data frame with the number of cases of each class for every factor value, lift and woe.
#' @examples
#' compute.woe(df$var, df$target)
#' @export
compute.woe <- function(factor, target)
{
  
  if (class(target) != "factor")
    stop("target variable must be factor")
  
  if (class(factor) != "factor")
    stop("factor variable must be factor")
  
  x <- table(factor, target)
  n.categories <- nrow(x)
  x <- cbind(x,rep(0,n.categories))
  x <- cbind(x,rep(0,n.categories))
  x <- cbind(x,rep(0,n.categories))
  x <- cbind(x,rep(0,n.categories))
  colnames(x) <- c('0','1','rate','avg','lift','woe')
  x <- rbind(x,c(0,0,0,0,0,0))
  rownames(x)[n.categories+1] <- 'Total'
  x[n.categories+1,1] <- sum(x[1:n.categories,1])
  x[n.categories+1,2] <- sum(x[1:n.categories,2])
  x[,3] <- round(x[,2] / ( x[,1] + x[,2] ), 2)
  x[,4] <- x[n.categories+1,3]
  x[,5] <- round(x[,3] / x[,4], 2)
  x[,6] <- log((x[,2] / x[,1]) / (x[n.categories+1,2] / x[n.categories+1,1]))
  
  x <- x[c(order(x[1:n.categories,5], decreasing=TRUE),n.categories+1),]
  
  return (x)
  
}


