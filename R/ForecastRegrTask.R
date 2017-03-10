#' Create a task for univariate forecasting
#'
#' @description Creates a task for univariate forecasting learners
#' @export
#' @rdname Task
#' @importFrom zoo index coredata
makeForecastRegrTask = function(id = deparse(substitute(data)), data, target,
                                weights = NULL, blocking = NULL, frequency = 1L, fixup.data = "warn",
                                check.data = TRUE) {
  assertString(id)
  assertClass(data,"xts")
  assertString(target)
  assertInteger(frequency, lower = 0L, max.len = 1L)
  assertChoice(fixup.data, choices = c("no", "quiet", "warn"))
  assertFlag(check.data)

  data = data.frame(row.names = index(data), coredata(data))

  if (fixup.data != "no") {
    if (is.integer(data[[target]]))
      data[[target]] = as.double(data[[target]])
  }

  task = makeSupervisedTask("fcregr", data, target, weights, blocking, fixup.data = fixup.data, check.data = check.data)

  if (check.data) {
    assertNumeric(data[[target]], any.missing = FALSE, finite = TRUE, .var.name = target)
  }

  task$task.desc = makeForecastRegrTaskDesc(id, data, target, weights, blocking, frequency)
  addClasses(task, c("ForecastRegrTask", "TimeTask"))
}

makeForecastRegrTaskDesc = function(id, data, target, weights, blocking, frequency) {
  td = makeTaskDescInternal("fcregr", id, data, target, weights, blocking)
  td$dates = c(rownames(data)[1],rownames(data)[nrow(data)])
  td$frequency = frequency
  addClasses(td, c("ForecastRegrTaskDesc", "SupervisedTaskDesc"))
}


#' @export
print.ForecastRegrTask = function(x, print.weights = TRUE, ...) {
  td = getTaskDescription(x)
  catf("Task: %s", td$id)
  catf("Type: %s", td$type)
  catf("Target: %s", td$target)
  catf("Observations: %i", td$size)
  catf("Dates:\n Start: %s \n End:   %s", td$dates[1], td$dates[2])
  catf("Frequency: %i", td$frequency)
  catf("Features:")
  catf(printToChar(td$n.feat, collapse = "\n"))
  catf("Missings: %s", td$has.missings)
  if (print.weights)
    catf("Has weights: %s", td$has.weights)
  catf("Has blocking: %s", td$has.blocking)
}

