library(tidyverse)
library(dagitty)
library(caret)
library(ipred)
library(dplyr)
library(rsample)
library(pastecs)
library(party)
library(reshape2)
library(boot)
library(parallel)
library(pbapply)


#' Title
#'
#' @param data
#' @param index
#' @param p
#' @param formula
#' @param extra
#'
#' @return
#' @export
#'
#' @examples
NBPtestingGeneral <- function(data, index, p, formula, extra){
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

booty <- function(cl, data, statistic,
                  B=10, N=100, R=100, p=0.6, probs=c(0.05,0.95),
                  formula="X ~ Y + Z", extra="Y"){
  res <- pblapply(cl=cl,1:B, function(i){
    set.seed(i)
    data <- data.frame(data)
    boot_test <- boot(data=data, statistic = statistic, R=R, p=p,
                      formula=formula, extra=extra)

    # Bootstrap Confidence interval
    c(mean(boot_test$t),
      2*pnorm(abs(mean(boot_test$t)/sd(boot_test$t) ), lower.tail = FALSE),
      quantile(boot_test$t, probs = probs[1]),
      quantile(boot_test$t, probs = probs[2]))
  })
  results <- do.call(rbind, res)
  colnames(results) <- c('mean','pvalue','CIlower', 'CIupper')
  results
}


cl <- makeCluster(detectCores()- 1, type = "PSOCK")
clusterExport(cl, c("NBPtestingGeneral"))
libs <- clusterEvalQ(cl, {library(boot);library(ipred)})

#Examples

# 100, three_dag #
##################
tic <- proc.time()
results1 <- booty(cl, dataFunc=three_dag, statistic=NBPtestingGeneral,
                  B=10, N=100, R=100, p=0.6, probs=c(0.05,0.95),
                  formula="X ~ Y + Z", extra="Y")
#write.csv(as.data.frame(results),"./NBPtesting_100.csv", row.names = TRUE)
toc <- proc.time()

tic + toc

#####################
# 200, four_var_cat #
#####################
results2 <- booty(cl, dataFunc=four_var_cat, statistic=NBPtestingGeneral,
                  B=10, N=200, R=100, p=0.7, probs=c(0.05,0.95),
                  formula="X2 ~ X3 + X4", extra="X3")
#write.csv(as.data.frame(results),"./NBPtestingFVC_200.csv", row.names = TRUE)


stopCluster(cl)


