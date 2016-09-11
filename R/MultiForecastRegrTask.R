#' Create an task object for multivariate forecasting.
#'
#' @description Creates a task that is used for multivariate forecasting. Target the name of a single series
#' or 'All'. Targeting a single series will produce a single output similar to univariate tasks while
#' treating all other variables endogeneously. targeting 'All' will forecast all variables forward.
#' @export
#' @rdname Task
#' @importFrom zoo index coredata
makeMultiForecastRegrTask = function(id = deparse(substitute(data)), data, target,
                                weights = NULL, blocking = NULL, fixup.data = "warn",
                                check.data = TRUE, frequency = 1L) {
  assertString(id)
  assertClass(data,"xts")
  assertString(target)
  assertChoice(target, c("all", colnames(data)))
  assertInteger(frequency, lower = 0L, max.len = 1L)
  assertChoice(fixup.data, choices = c("no", "quiet", "warn"))
  assertFlag(check.data)


  is.target.all = target == "all"
  row.names = index(data)
  col.names = colnames(data)
  assert(
    checkClass(row.names, "POSIXlt"),
    checkClass(row.names, "POSIXt"),
    checkClass(row.names, "POSIXct")
    )
  data = data.frame(row.names = row.names, coredata(data))

  if (fixup.data != "no" && all(!is.target.all)) {
    if (is.integer(data[[target]]))
      data[[target]] = as.double(data[[target]])
  } else if (fixup.data != "no"){
    is.int = vlapply(data, is.integer)
    if (all(is.int)){
      data = lapply(data, as.double)
      data = as.data.frame(row.names = row.names, data)
    }
  }

  if (is.target.all)
    target = colnames(data)

  task = makeSupervisedTask("mfcregr", data, target, weights, blocking, fixup.data = fixup.data, check.data = check.data)

  if (check.data && !is.target.all) {
    assertNumeric(data[[target]], any.missing = FALSE, finite = TRUE, .var.name = target)
  } else if (check.data && is.target.all){
    lapply(data,assertNumeric, any.missing = FALSE, finite = TRUE)
  }

  task$task.desc = makeTaskDesc.MultiForecastRegrTask(id, data, target, weights, frequency, blocking)
  addClasses(task, c("MultiForecastRegrTask","TimeTask"))
}

makeTaskDesc.MultiForecastRegrTask = function(id, data, target, weights, frequency, blocking) {
  td = makeTaskDescInternal("mfcregr", id, data, target, weights, blocking)
  td$dates = c(rownames(data)[1], rownames(data)[nrow(data)])
  td$frequency = frequency
  td$col.names = colnames(data)
  addClasses(td, c("TaskDescMultiForecastRegr", "TaskDescSupervised"))
}


#' @export
print.MultiForecastRegrTask = function(x, print.weights = TRUE, ...) {
  td = getTaskDescription(x)
  catf("Task: %s", td$id)
  catf("Type: %s", td$type)
  catf("Target: %s", stri_paste(td$target, collapse = " "))
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

