
#' Default Boocoin using random forest with 500 trees
#'
#' @param data A data frame
#' @param index
#' @param p Share of data used for testing (between 0 and 1)
#' @param formula formula for testing the condition Y _||_ X | Z is written as Y ~ X + Z
#' @param extra for the Y ~ X + Z this would be X
#' @param performance performance measure rmse, r2 or accuracy
#'
#' @return
#' @export
#'
#' @examples
boocoin_default <- function(data, index, p, formula, extra, performance = 'rmse'){
  resample <- data[index, ]
  formula <- formula(formula)
  # Data Partition
  inTraining <- sample(nrow(resample), round(nrow(resample)*p))
  training <- resample[inTraining,]
  testing  <- resample[-inTraining,]
  testingResponse <- model.frame(update(formula, .~0),testing)
  testingInputs   <- model.frame(formula(delete.response(terms(formula))), testing)

  # Model1
  model1 <- bagging(
    formula,
    data = training,
    coob = TRUE,
  )
  r21 <- cor(testingResponse, predict(model1, newdata = testingInputs))^2

  # Model2
  training[[extra]] = sample(training[[extra]])
  testing[[extra]] = sample(testing[[extra]])
  testingResponse <- model.frame(update(formula, .~0),testing)
  testingInputs   <- model.frame(formula(delete.response(terms(formula))), testing)
  model2 <- bagging(
    formula,
    data = training,
    coob = TRUE
  )
  r22 <- cor(testingResponse, predict(model2, newdata = testingInputs))^2

  diff_r2 <- r21-r22
}
