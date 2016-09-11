#'@export
makeRLearner.fcregr.nnetar = function() {
  makeRLearnerForecastRegr(
    cl = "fcregr.nnetar",
    package = "forecast",
    par.set = makeParamSet(
      # no default for p
      makeIntegerLearnerParam(id = "p", lower = 0L),
      makeIntegerLearnerParam(id = "P", lower = 0L, default = 1L),
      # no default for size
      makeIntegerLearnerParam(id = "size", lower = 0L),
      makeIntegerLearnerParam(id = "repeats", lower = 1L, default = 20L),
      makeNumericLearnerParam(id = "lambda"),
      makeUntypedLearnerParam(id = "model", default = NULL),
      makeLogicalLearnerParam(id = "scale.inputs", default = TRUE),
      # nnet params
      makeIntegerLearnerParam(id = "maxit", default = 100L, lower = 1L),
      makeLogicalLearnerParam(id = "linout", default = FALSE, requires = quote(entropy==FALSE && softmax==FALSE && censored==FALSE)),
      makeLogicalLearnerParam(id = "entropy", default = FALSE, requires = quote(linout==FALSE && softmax==FALSE && censored==FALSE)),
      makeLogicalLearnerParam(id = "softmax", default = FALSE, requires = quote(entropy==FALSE && linout==FALSE && censored==FALSE)),
      makeLogicalLearnerParam(id = "censored", default = FALSE, requires = quote(linout==FALSE && softmax==FALSE && entropy==FALSE)),
      makeLogicalLearnerParam(id = "skip", default = FALSE),
      makeNumericLearnerParam(id = "rang", default = 0.7),
      makeNumericLearnerParam(id = "decay", default = 0, lower = 0),
      makeLogicalLearnerParam(id = "Hess", default = FALSE),
      makeLogicalLearnerParam(id = "trace", default = TRUE, tunable = FALSE),
      makeIntegerLearnerParam(id = "MaxNWts", default = 1000L, lower = 1L),
      # FIXME_PK: Why are abstoll and reltoll written with 2 "l"?
      makeNumericLearnerParam(id = "abstol", default = 1.0e-4),
      makeNumericLearnerParam(id = "reltol", default = 1.0e-8),
      # predict params
      makeIntegerLearnerParam(id = "h", lower = 0L, default = expression(ifelse(object$m > 1, 2 * object$m, 10)),
                              when = "predict", tunable = FALSE),
      makeLogicalLearnerParam(id = "PI", default = FALSE, tunable = FALSE),
      makeNumericVectorLearnerParam(id = "level", len = NA, default = c(80,95), when = "predict", tunable = FALSE),
      makeLogicalLearnerParam(id = "bootstrap", default = FALSE, when = "predict", tunable = FALSE),
      makeIntegerLearnerParam(id = "npaths", default = 1000, when = "predict"),
      makeUntypedLearnerParam(id = "innov", default = NULL, when = "predict"),
      # simulate params
      makeIntegerLearnerParam(id = "nsim", lower = 0L, default = expression(length(object$x))),
      makeIntegerLearnerParam(id = "seed", default = NULL),
      makeLogicalLearnerParam(id = "future", default = TRUE),
      key = c("object", "m", "x")
      ),
    properties = c("numerics", "quantile"),
    name = "Neural Network Time Series Forecasts",
    short.name = "nnetar",
    note = "All variables besides the target will be passed to the xreg argument."
  )
}
#'@export
trainLearner.fcregr.nnetar = function(.learner, .task, .subset, .weights = NULL, ...) {
  data = getTaskData(.task,.subset,target.extra = TRUE)
  data$target = ts(data$target, start = 1, frequency = .task$task.desc$frequency)
  if (is.null(.weights)){
    if (ncol(data$data) != 0){
      data$data = ts(data$data, start = 1, frequency = .task$task.desc$frequency)
      forecast::nnetar(y = data$target,xreg = data$data, ...)
    } else {
      forecast::nnetar(y = data$target, ...)
    }
  } else {
    if (ncol(data$data) != 0){
      data$data = ts(data$data, start = 1, frequency = .task$task.desc$frequency)
      forecast::nnetar(y = data$target,xreg = data$data, ...)
    } else {
      forecast::nnetar(y = data$target, ...)
    }
  }
}



#' @export
updateLearner.fcregr.nnetar = function(.learner, .model, .newdata, .task, .truth, .weights = NULL, ...) {
  target = getTaskTargetNames(.task)
  data = ts(.truth, start = 1, frequency = .task$task.desc$frequency)
  if (is.null(weights)){
    if (ncol(data$data) != 0){
      data$data = ts(data$data, start = 1, frequency = .task$task.desc$frequency)
      forecast::nnetar(y = data$target,xreg = data$data, model = .model$learner.model, ...)
    } else {
      forecast::nnetar(y = data$target, model = .model$learner.model, ...)
    }
  } else {
    if (ncol(data$data) != 0){
      data$data = ts(data$data, start = 1, frequency = .task$task.desc$frequency)
      forecast::nnetar(y = data$target,xreg = data$data, model = .model$learner.model, weights = .weights, ...)
    } else {
      forecast::nnetar(y = data$target, model = .model$learner.model, weights = .weights, ...)
    }
  }
}

#'@export
predictLearner.fcregr.nnetar = function(.learner, .model, .newdata, ...) {
  se.fit = .learner$predict.type == "quantile"
  model.td = getTaskDescription(.model)

  if (all(model.td$n.feat == 0)){
    p = forecast::forecast(.model$learner.model, ...)
  } else {
    .newdata = ts(.newdata, start = 1, frequency = .model$task.desc$frequency)
    p = forecast::forecast(.model$learner.model, xreg = .newdata, ...)
  }
  if (!se.fit){
    p = as.numeric(p$mean)
  } else {
    pMean  = as.matrix(p$mean)
    pLower = p$lower
    pUpper = p$upper
    colnames(pMean)  = "point_forecast"
    colnames(pLower) = stri_paste("lower_",p$level)
    colnames(pUpper) = stri_paste("upper_",p$level)
    p = cbind(pMean,pLower,pUpper)
  }
  return(p)
}

