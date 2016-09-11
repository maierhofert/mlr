#' @title Generate lags and differences for feature variables
#'
#' @description
#' Replace all variables with their generated lagged and differenced variables.
#' Uses the \code{xts} framework for developing lags and is only available for \code{TimeTasks}.
#'
#' @template arg_taskdf
#' @template arg_taskdf_target
#' @param lag [\code{integer}]\cr
#'   An integer vector of lag lengths.
#' @param difference [\code{integer}]\cr
#'   An integer of the order of differencing
#' @param cols [\code{character}]\cr
#'   A character vector of columns to create lag features for.
#'   Default is to use all columns. NOTE: For forecast regression tasks, it is not
#'   a good idea to make lags of your target variable. So if cols are not specied by
#'   the user, createLagDiffFeatures will return a regr task.
#' @param seasonal.lag [\code{integer}]\cr
#'   An integer vector of seasonal lag lengths, made as \code{seasonal.lag * frequency}
#' @param seasonal.difference [\code{integer}]\cr
#'   An integer of the seasonal order of difference, made as \code{seasonal.difference * frequency}
#' @param frequency [\code{integer}]\cr
#'   An integer representing the periodicity in the time series. If frequency is declared in the task,
#'   the task frequency will be used.
#' @param na.pad [\code{logical}]\cr
#'   A logical to denote whether the data should be padded to the original size with NAs
#' @param difference.lag [\code{integer}]\cr
#'   An integer denoting the period to difference over
#' @param seasonal.difference.lag [\code{integer}]\cr
#'   An integer denoting the period to seasonaly difference over
#' @param return.nonlag [\code{logical}]\cr
#'   A logical to denote whether the original unlagged features should be returned
#' @export
#' @family eda_and_preprocess
#' @examples
#' set.seed(1234)
#' dat = arima.sim(model = list(ar = c(.5,.2), ma = c(.4), order = c(2,0,1)), n = 200)
#' times = (as.POSIXlt("1992-01-14")) + lubridate::days(1:200)
#' dat = xts::xts(dat,order.by = times)
#' colnames(dat) = c("arma_test")
#' regr.task = makeRegrTask(id = "Lagged ML model", data = as.data.frame(dat), target = "arma_test")
#' regr.task.lag = createLagDiffFeatures(regr.task, lag = 1L:10L, difference = 0L)
createLagDiffFeatures = function(obj, lag = 0L, difference = 0L, difference.lag = 1L,
                                 cols = NULL, target = character(0L),
                                 seasonal.lag = 0L, seasonal.difference = 0L,
                                 seasonal.difference.lag = 1L, frequency = 1L,
                                 na.pad = TRUE, return.nonlag = TRUE) {
  assertInteger(lag,lower = 0L, upper = 1000L)
  assertInteger(difference,lower = 0L, upper = 1000L, len = 1L)
  assertInteger(difference.lag,lower = 0L, upper = 1000L, len = 1L)
  assertInteger(seasonal.lag,lower = 0L, upper = 1000L)
  assertInteger(seasonal.difference,lower = 0L, upper = 1000L, len = 1L)
  assertInteger(seasonal.difference.lag,lower = 0L, upper = 1000L, len = 1L)
  assertLogical(na.pad)
  assert(checkClass(obj, "xts"), checkClass(obj, "Task"))
  assertCharacter(target, any.missing = FALSE)
  if (!is.null(cols))
    assertCharacter(cols, any.missing = FALSE)

  UseMethod("createLagDiffFeatures")
}

#' @export
#' @importFrom zoo cbind.zoo
createLagDiffFeatures.xts = function(obj, lag = 0L, difference = 0L, difference.lag = 1L,
                                     cols = NULL, target = character(0L),
                                     seasonal.lag = 0L, seasonal.difference = 0L,
                                     seasonal.difference.lag = 1L, frequency = 1L,
                                     na.pad = TRUE, return.nonlag = TRUE) {
  nums = vlapply(obj, is.numeric)
  work.cols = colnames(obj)
  if (!is.null(cols)) {
    assertSubset(cols, work.cols)
    x = obj[, cols]
  } else {
    cols = work.cols
  }

  seasonal.lag = seasonal.lag * frequency
  seasonal.difference = seasonal.difference * frequency

  if (any(lag > 0L) && (difference > 0L)){
    # FIXME: xts does not support lagging characters more than once
    ## we use an lapply to get around this
    xLagDiff = lapply(lag, function(i) lag.xts(obj, k = i))
    xLagDiff = do.call(cbind.zoo, xLagDiff)
    xLagDiff = diff.xts(xLagDiff, lag = difference.lag, differences = difference)
  } else if (any(lag > 0L)){
    xLagDiff = lapply(lag,function(i) lag.xts(obj, k = i))
    xLagDiff = do.call(cbind.zoo, xLagDiff)
  } else if (difference > 0L){
    xLagDiff = diff.xts(obj, lag = difference.lag, differences = difference)
  } else {
    xLagDiff = NULL
  }


  if (frequency > 1L){
    if (any(seasonal.lag > 0L) && (seasonal.difference > 0L)){
      xSeasonLagDiff = lapply(seasonal.lag, function(i) lag.xts(obj, k = i))
      xSeasonLagDiff = do.call(cbind.zoo, xSeasonLagDiff)
      xSeasonLagDiff = diff.xts(xSeasonLagDiff, lag = seasonal.difference.lag,
                                differences = seasonal.difference)
    } else if (any(seasonal.lag > 0L)){
      xSeasonLagDiff = lapply(seasonal.lag, function(i) lag.xts(obj, k = i))
      xSeasonLagDiff = do.call(cbind.zoo, xSeasonLagDiff)
    } else if (seasonal.difference > 0L){
      xSeasonLagDiff = diff.xts(obj, lag = seasonal.difference.lag,
                                differences = seasonal.difference)
    } else {
      xSeasonLagDiff = NULL
    }
  } else {
    xSeasonLagDiff = NULL
  }
  if (!is.null(xLagDiff))
    colnames(xLagDiff) = paste0(work.cols,"_lag", rep(lag, each = length(work.cols)),
                                "_diff",
                                rep(difference, each = length(work.cols)))
  if (!is.null(xSeasonLagDiff))
    colnames(xSeasonLagDiff) = paste0(work.cols,"_lag",
                                      rep(seasonal.lag, each = length(work.cols)),
                                      "_diff",
                                      rep(seasonal.difference, each = length(work.cols)))

  if (!is.null(xLagDiff) && !is.null(xSeasonLagDiff)){
    repeatedCols = !(colnames(xLagDiff) %in% colnames(xSeasonLagDiff))
    x = cbind(xLagDiff[, repeatedCols],xSeasonLagDiff)
  } else if(!is.null(xLagDiff)){
    x = xLagDiff
  } else if (!is.null(xSeasonLagDiff)){
    x = xSeasonLagDiff
  } else {
    stop("No lags or differences were chosen")
  }

  if (return.nonlag){
    obj = obj[, c(intersect(work.cols, cols)), drop = FALSE]
    obj = cbind(obj, x)
  } else {
    obj = cbind(obj[, target], x)
  }

  if (na.pad == FALSE){
    removeNaPad = 1:(max(lag, seasonal.lag) +
                       (max(difference * difference.lag,
                            seasonal.difference * seasonal.difference.lag)))
    obj = obj[-removeNaPad,]
  }
  return(obj)
}

#' @export
createLagDiffFeatures.Task = function(obj, lag = 0L, difference = 0L,
                                      difference.lag = 1L, cols = NULL,
                                      target = character(0L), seasonal.lag = 0L,
                                      seasonal.difference = 0L, seasonal.difference.lag = 1L,
                                      frequency = 1L, na.pad = TRUE,
                                      return.nonlag = TRUE) {
  target = getTaskTargetNames(obj)
  data = getTaskData(obj)

  nums = vlapply(data, is.numeric)
  td = getTaskDescription(obj)
  if (!is.null(td$frequency) && frequency == 1L)
    frequency = td$frequency
  if (!is.null(cols)){
    if (!all(cols %in% colnames(data)))
      stop("Chosen cols not in data")
    work.cols = intersect(colnames(data), cols)
  } else {
    work.cols = colnames(data)
  }
  # We store the original columns as we need them for forecasting
  data.original = data


  data = data[,work.cols, drop = FALSE]
  row.dates = try(as.POSIXct(rownames(data)), silent = TRUE)
  if (is.error(row.dates))
    stop("The data's rownames must be convertible to POSIXct")
  data.total = list()[1:2]
  work.cols.nums = vlapply(data, is.numeric)
  # We first go over the numeric columns
  if (any(work.cols.nums)){

    data.nums  = xts::xts(data[, nums, drop = FALSE], order.by = row.dates)
    colnames(data.nums) = work.cols[nums]

    data.nums = createLagDiffFeatures( obj = data.nums, lag = lag, difference = difference,
                                       difference.lag = difference.lag,
                                       cols = cols, target = target,
                                       seasonal.lag = seasonal.lag,
                                       seasonal.difference = seasonal.difference,
                                       seasonal.difference.lag = seasonal.difference.lag,
                                       frequency = frequency, na.pad = na.pad,
                                       return.nonlag = return.nonlag)

    data.total[[1]] = as.data.frame(data.nums)
  } else {
    if (return.nonlag)
      data.total[[1]] = data.original[, nums, drop = FALSE]
    else
      data.total[[1]] = NULL
  }

  # Then we lag everything else
  if (any(!work.cols.nums)){

    data.other = data[, !work.cols.nums, drop = FALSE]
    # This is kind of gross, but we need to preserve our factor levels
    ## and xts objects convert everything to character
    facs = vlapply(data.other, is.factor)
    column.fac = colnames(data.other)[facs]
    fac.levels = lapply(data.other[, facs, drop = FALSE], levels)

    # Now turn the data.frame into an xts object
    data.other = xts::xts(data.other, order.by = row.dates)
    colnames(data.other) = work.cols[!work.cols.nums]

    # We can't difference non-numeric data so it's all set to zero
    data.other = createLagDiffFeatures( obj = data.other, lag = lag, difference = 0L,
                                        difference.lag = 0L,
                                        cols = cols, target = target,
                                        seasonal.lag = seasonal.lag,
                                        seasonal.difference = 0L,
                                        seasonal.difference.lag = 0L,
                                        frequency = frequency, na.pad = na.pad,
                                        return.nonlag = return.nonlag)
    data.other = as.data.frame(data.other)
    # We have to go through and reapply the appropriate levels
    facs.lag = lapply(paste0(column.fac,"_"), function(x) stringi::stri_detect_fixed(colnames(data.other), x))

    lapply(1:length(facs.lag), function(i) invisible(levels(data.other[,facs.lag[[i]]]) <<- fac.levels[i]))
    data.total[[2]] = data.other
  } else {
    if (return.nonlag)
      data.total[[2]] = data.original[,!nums,drop=FALSE]
    else
      data.total[[2]] = NULL
  }

  data.total = filterNull(data.total)
  # make sure removing na padding still gives us the same sizes of data
  min.length = unlist(lapply(data.total, nrow))
  min.length = min(min.length)
  data.total = lapply(data.total, function(x) x[I(nrow(x)-min.length + 1):nrow(x),,drop = FALSE])
  data.total = as.data.frame(data.total)
  #data.total = cbind(data.original[I(nrow(data.original)-min.length + 1):nrow(data.original),target],
  #                   data.total)
  #colnames(data.total)[1:length(target)] = target

  obj = changeData(obj,data = data.total)
  data.original = data.original[I(nrow(data) - (max(lag) * frequency + difference + difference.lag +
                                                  max(seasonal.lag) * frequency + seasonal.difference +
                                                  seasonal.difference.lag - frequency)):I(nrow(data)), , drop=FALSE]
  obj$task.desc$pre.proc$data.original = data.original
  obj$task.desc$pre.proc$par.vals = list(lag = lag, difference = difference,
                                         difference.lag = difference.lag,
                                         cols = cols, target = target,
                                         seasonal.lag = seasonal.lag,
                                         seasonal.difference = seasonal.difference,
                                         seasonal.difference.lag = seasonal.difference.lag,
                                         frequency = frequency, na.pad = na.pad,
                                         return.nonlag = return.nonlag)
  obj
}


#' @title Update Lagged and Differenced Data
#'
#' @description
#' Update a task modified by lags and differences with new data
#'
#' @param task [\code{\link{Task}}]\cr
#'   The task.
#' @param newdata [\code{data.frame}]\cr
#'   New observations to update the model
#' @param weights [\code{numeric}]\cr
#'   Optional, non-negative case weight vector to be used during fitting.
#'   If given, must be of same length as \code{subset} and in corresponding order.
#'   By default \code{NULL} which means no weights are used unless specified in the task (\code{\link{Task}}).
#'   Weights from the task will be overwritten.
#' @param ... [any]\cr
#'   Currently ignored.
#' @return [\code{\link{WrappedModel}}].
#' @export
#' @importFrom xts rbind.xts
updateLagDiff = function(task, newdata, weights,...) {
  assertClass(task, "Task")
  assertDataFrame(newdata)
  if (missing(weights))
    weights = task$weights



  data = rbind(task$task.desc$pre.proc$data.original, newdata)
  row.dates = try(as.POSIXct(rownames(data)), silent = TRUE)
  if (is.error(row.dates))
    stop("The data's rownames must be convertible to POSIXct")

  data = xts::xts(data, order.by = row.dates)
  lagdiff.func = function(...){
    createLagDiffFeatures(obj = data, ...)
  }
  data = do.call(lagdiff.func, task$task.desc$pre.proc$par.vals)
  data = data.frame(data, row.names = index(data))

  obj = changeData(task, data, weights)
  obj
}

#' @export
as.data.frame.xts = function(x, ...) {
  data.frame(row.names = index(x), zoo::coredata(x))
}
