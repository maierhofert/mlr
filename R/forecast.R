#' @title  Forecasting generic
#'
#' @description Forecasts using a fitted model
#'
#' @param object
#'  A model to be used for forecasting
#'
#' @param ...
#'  Used to pass package specific parameters
#' @export
forecast = function(object, ...) {
  UseMethod("forecast")
}

#' @title Forecast a trained learner.
#'
#' @description
#' Forecast the target variable of new data using a fitted model.
#' What is stored exactly in the [\code{\link{Prediction}}] object depends
#' on the \code{predict.type} setting of the \code{\link{Learner}}.
#' If \code{predict.type} was set to \dQuote{prob} probability thresholding
#' can be done calling the \code{\link{setThreshold}} function on the
#' prediction object.
#'
#' The row names of the input \code{task} or \code{newdata} are preserved in the output.
#'
#' @param object [\code{\link{WrappedModel}}]\cr
#'   Wrapped model, result of \code{\link{train}}.
#' @param task [\code{\link{Task}}]\cr
#'   The task. This is only used for fcregr and mfcregr, in which it's used to gather date information
#' @param newdata [\code{data.frame}]\cr
#'   Optional: A data frame of external regressors which must have the same number of rows as your forecast length h
#' @param h [\code{integer(1)}]
#'   An integer specifying the forecast horizon
#' @param ... [any]\cr
#'   Currently ignored.
#' @return [\code{\link{Prediction}}].
#' @family predict
#' @export
#' @examples
#' # train and forecast
#' dat  = arima.sim(model = list(ar = c(.8,.1), ma = c(.4), order = c(2,0,1)), n = 300)
#' jump = data.frame(jump = ifelse(diff(dat) > .5, "up","down"))
#' times = (as.POSIXct("1992-01-14")) + lubridate::days(1:299)
#' rownames(jump) = times
#'
#' classif.task = makeClassifTask(data = jump,target = "jump")
#' classif.task = createLagDiffFeatures(classif.task, lag = 1L:15L,
#'                                      na.pad = FALSE)
#' classif.learn = makeLearner("classif.ada")
#' classif.train = train(classif.learn,classif.task)
#' forecast(classif.train, h = 10)
#'
#' # predict now probabiliies instead of class labels
#' lrn = makeLearner("classif.lda", predict.type = "prob")
#' model = train(lrn, classif.task)
#' f = forecast(model, h = 10)
#' print(f)
#' getPredictionProbabilities(f)
forecast.WrappedModel = function(object, newdata = NULL, task, h = 10, ...) {
  model = object
  learner = model$learner
  td = model$task.desc

  assertClass(model, classes = "WrappedModel")
  if (any(c("fcregr","mfcregr") %in% model$learner$type ))
    if (!missing(newdata))
      return(predict(object, newdata = newdata))
  else
    return(predict(object, task = task))

  if (!missing(task))
    stop("Tasks are only accepted for fcregr and mfcregr tasks.
         forecast is only used after you have already trained your learner so only newdata is accepted")
  assertIntegerish(h,lower = 1)
  if (is.null(td$pre.proc))
    stop("Forecasting requires lags")

  if (!is.null(newdata)){
    ##FIXME: Better way to make into a data frame? This doesn't protect against a lot of things
    if (xts::is.xts(newdata)){
      warningf("Provided data for prediction is not a pure data.frame but from class %s, hence it will be converted.",  class(newdata)[1])
      newdata = as.data.frame(newdata, row.names = as.character(index(newdata)))
      assertDataFrame(newdata, min.rows = 1L)
    } else if (class(newdata)[1] != "data.frame") {
      warningf("Provided data for prediction is not a pure data.frame but from class %s, hence it will be converted.",  class(newdata)[1])
      newdata = as.data.frame(newdata)
    }
    assertDataFrame(newdata)
    t.col = match(td$target, colnames(newdata))
    if (nrow(newdata) != h)
      stop("The new data supplied must be the length of your forecast")
  } else {
    t.col = NA
  }
  # get truth and drop target col, if target in newdata
  if (!all(is.na(t.col))) {
    if (length(t.col) > 1L && anyMissing(t.col))
      stop("Some but not all target columns found in data")
    truth = newdata[, t.col, drop = TRUE]
    if (is.list(truth))
      truth = data.frame(truth)
    newdata = newdata[, -t.col, drop = FALSE]
  } else {
    truth = NULL
  }


  proc.vals = td$pre.proc$par.vals
  max.lag = max(c(proc.vals$lag,proc.vals$difference.lag,
                  proc.vals$seasonal.lag * proc.vals$frequency,
                  proc.vals$seasonal.difference.lag * proc.vals$frequency))

  # This just cuts the amount of data we need to use
  data = td$pre.proc$data.original[,td$target, drop = FALSE]
  if (is.null(truth)){
    row.names = rownames(data)
    sec = lubridate::dseconds(lubridate::int_diff(as.POSIXct(row.names)))
    # We take the median seconds between intervals as this won't get
    # a wrong day until about 200 months in the future.
    med_sec = mean(sec)
    start = as.POSIXct(row.names[length(row.names)])
    row.names = start + rep(med_sec,h) * 1:h
  } else {
    row.names = row.names(truth)
  }
  #data = data[max.lag:length(data),1,drop = FALSE]

  error = NA_character_
  # was there an error in building the model? --> return NAs
  if (isFailureModel(model)) {
    p = predictFailureModel(model, newdata)
    time.predict = NA_real_
  } else {
    pars = list(
      .data = data,
      .newdata = newdata,
      .proc.vals = proc.vals,
      .h = h,
      .td = td,
      .model = model
    )
    debug.seed = getMlrOption("debug.seed", NULL)
    if (!is.null(debug.seed))
      set.seed(debug.seed)
    opts = getLearnerOptions(learner, c("show.learner.output", "on.learner.error", "on.learner.warning"))
    fun1 = if (opts$show.learner.output) identity else capture.output
    fun2 = if (opts$on.learner.error == "stop") identity else function(x) try(x, silent = TRUE)
    if (opts$on.learner.warning == "quiet") {
      old.warn.opt = getOption("warn")
      on.exit(options(warn = old.warn.opt))
      options(warn = -1L)
    }
    time.predict = measureTime(fun1(p <- fun2(do.call(makeForecast, pars))))
  }

  ids = NULL
  makePrediction(task.desc = td, row.names = row.names, id = ids, truth = truth,
                 predict.type = learner$predict.type,
                 predict.threshold = learner$predict.threshold,
                 y = p, time = time.predict, error = error)
}

makeForecast = function(.data, .newdata, .proc.vals, .h, .td, .model, ...) {
  forecasts = list()[1:I(.h)]
  for (i in 1:(.h)){

    .data = rbind(.data,NA)
    # The dates here will be thrown away later
    times = as.POSIXct("1992-01-14") + lubridate::days(1:I(nrow(.data)))
    dat.xts = xts::xts(.data, order.by = times)
    colnames(dat.xts) = .td$target

    # get lag structure
    lagdiff.func = function(...){
      createLagDiffFeatures(obj = dat.xts,...)
    }
    data.lag = do.call(lagdiff.func, .proc.vals)
    data.lag = as.data.frame(data.lag)
    data.step = data.lag[nrow(data.lag),,drop = FALSE]
    if (!is.null(.newdata))
      data.step = cbind(data.step,.newdata[i,])

    # predict
    pred = predict(.model, newdata = data.step)

    if (pred$predict.type == "response"){
      forecasts[[i]] = pred$data
      .data[nrow(.data),] = pred$data$response
    } else if (pred$predict.type == "prob"){
      #FIXME: I don't know regex well enough to do this in one sweep
      colnames(pred$data) = stringr::str_replace(colnames(pred$data),"prob","")
      colnames(pred$data) = stringr::str_replace(colnames(pred$data),"[.]","")
      .data[nrow(.data),] =pred$data$response
      pred$data$response = NULL
      forecasts[[i]] = pred$data
    } else if (pred$predict.type == "se"){
      forecasts[[i]] = pred$data
      .data[nrow(.data),] = pred$data$response
    }
  }

  p = do.call(rbind,forecasts)
  p$truth = NULL
  p
}
