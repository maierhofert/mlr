#'@export
makeRLearner.fcregr.Arima = function() {
  makeRLearnerForecastRegr(
    cl = "fcregr.Arima",
    package = "forecast",
    par.set = makeParamSet(
      makeIntegerVectorLearnerParam(id = "order", len = 3L,
                                    lower = 0L, upper = Inf,
                                    default = c(0L,0L,0L)),
      makeIntegerVectorLearnerParam(id = "seasonal", len = 3L,
                                    lower = 0L, upper = Inf,
                                    default = c(0L,0L,0L)),
      makeLogicalLearnerParam(id = "include.mean", default = TRUE),
      makeLogicalLearnerParam(id = "include.drift", default = FALSE),
      makeNumericLearnerParam(id = "lambda", default = NULL, special.vals = list(NULL), when = "both"),
      makeLogicalLearnerParam(id = "biasadj", default = FALSE, when = "both"),
      makeDiscreteLearnerParam(id = "method", values = c("CSS-ML", "ML", "CSS"), default = "CSS-ML"),
      makeUntypedLearnerParam(id = "model", default = NULL),
      # arima params
      makeLogicalLearnerParam(id = "transform.pars", default = TRUE),
      makeNumericVectorLearnerParam(id = "fixed", len = NA, default = NULL, special.vals = list(NULL)),
      makeNumericVectorLearnerParam(id = "init", len = NA, default = NULL, special.vals = list(NULL)),
      # No default
      makeIntegerLearnerParam("n.cond", lower = 0),
      makeDiscreteLearnerParam("SSinit", values = c("Gardner1980", "Rossignol2011"), default = "Gardner1980", tunable = FALSE),
      makeDiscreteLearnerParam("optim.method", default = "BFGS", values = c("Nelder-Mead", "BFGS",
                                                                            "CG", "L-BFGS-B",
                                                                            "SANN", "Brent"),
                               tunable = FALSE),
      makeUntypedLearnerParam("optim.controls", default = list(), tunable = FALSE),
      makeNumericLearnerParam("kappa", lower = 1e6, upper = Inf, tunable = FALSE),
      # prediction params
      makeIntegerLearnerParam(id = "h", lower = 0, upper = Inf,
                              default = expression(ifelse(object$arma[5]>1,2*object$arma[5],10)),
                              tunable = FALSE,
                              when = "predict"),
      makeLogicalLearnerParam(id = "bootstrap", default = FALSE, when = "predict", tunable = FALSE),
      makeNumericVectorLearnerParam(id = "level", len = NA, default = c(80, 95), when = "predict", tunable = FALSE),
      makeLogicalLearnerParam(id = "fan", default = FALSE, when = "predict", tunable = FALSE),
      makeIntegerLearnerParam(id = "npaths", default = 5000, when = "predict"),
      # simulate params
      makeIntegerLearnerParam(id = "nsim", lower = 0L, default = expression(length(object$x))),
      makeIntegerLearnerParam(id = "seed", default = NULL, special.vals = list(NULL)),
      makeLogicalLearnerParam(id = "future", default = TRUE),
      keys = c("x", "object", "arma")
    ),
    properties = c("numerics","quantile"),
    name = "AutoRegressive Integrated Moving Average",
    short.name = "Arima",
    note = "All variables besides the target will be passed to the xreg argument."
    )
}

#'@export
trainLearner.fcregr.Arima = function(.learner, .task, .subset, .weights = NULL, ...) {

  data = getTaskData(.task,.subset, target.extra = TRUE)
  data$target = ts(data$target, start = 1, frequency = .task$task.desc$frequency)
  if (ncol(data$data) != 0){
    data$data = ts(data$data, start = 1, frequency = .task$task.desc$frequency)
    forecast::Arima(y = data$target,xreg = data$data, ...)
  } else {
    forecast::Arima(y = data$target, ...)
  }
}

#'@export
predictLearner.fcregr.Arima = function(.learner, .model, .newdata, ...) {
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


#' @export
updateLearner.fcregr.Arima = function(.learner, .model, .newdata, .task, .truth, .weights = NULL, ...) {
  target = ts(.truth, start = 1, frequency = .task$task.desc$frequency)
  if (is.null(.newdata) == 0){
    updated = forecast::Arima(y = target, model = .model$learner.model)
  } else {
    xdata = ts(.newdata, start = 1, frequency = .task$task.desc$frequency)
    updated = forecast::Arima(y = target, model = .model$learner.model, xreg = xdata )
  }
  return(updated)
}


