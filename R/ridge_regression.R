#' Implements ridge regression with many predictors
#'
#' This function computes coefficients for ridge regression
#' All columns of the provided data frame are used as predictors, except the
#' one specified as a response.
#'
#' No interaction terms are included.
#'
#'
#' @param dat A data frame
#' @param response The name of a response variable in the data frame (unquoted)
#' @param lambda A vector of penalty terms to try
#'
#' @return A data frame of coefficients
#'
#' @import dplyr
#' @import purrr
#'
#' @export
ridge_regression <- function(dat, response, lambda) {

  x <- dat %>% select(-{{response}})
  y <- as.matrix(dat %>% pull({{response}}))

  x <- scale(x)
  x <- cbind(1, x) %>%
   as.matrix(x)

  get_betas <- function(x, y, lambda){
    my_results <- as.data.frame(t(solve(t(x) %*% x + lambda*diag(ncol(x))) %*% (t(x) %*% y)))
    return(my_results)
    }

  my_results <- purrr::map_dfr(lambda, ~get_betas(x,y,.x))

  colnames(my_results)[1] <- "Intercept"

  my_results <- cbind(my_results,lambda)


  ### This should be a data frame, with columns named
  ### "Intercept" and the same variable names as dat, and also a column
  ### called "lambda".

  return(my_results)
}

  #' Determines the best penalty term from a set of options
  #'
  #' This function uses a randomly chosen test and training set
  #'
  #' No interaction terms are included.
  #'
  #'
  #' @param train_dat A data frame to construct the model from
  #' @param test_dat A data frame to test the model on
  #' @param response The name of a response variable in the data frame (unquoted)
  #' @param lambda A vector of penalty terms to try
  #'
  #' @return A data frame of penalty terms and resulting errors
  #'
  #' @import dplyr
  #'
  #' @export
  find_best_lambda <- function(train_dat, test_dat, response, lambda) {

    x <- train_dat %>% select(-{{response}})
    y <- as.matrix(train_dat %>% pull({{response}}))
    test <- as.matrix(test_dat %>% pull({{response}}))

    x <- scale(x)
    x <- cbind(1, x) %>%
      as.matrix(x)

  get_sse <- function(x,y,y_test,lambda) {
    betas <- (t(solve(t(x) %*% x + lambda*diag(ncol(x))) %*% (t(x) %*% y)))
    preds <- x %*% t(betas)
    results <- as.data.frame(cbind(test, preds))
    sse <- results %>%
        mutate(se = ((test - preds)^2)) %>%
        summarize(error = sum(se))

      sse <- as.data.frame(cbind(lambda, sse))
      return(sse)
    }

    lambda_errors <- purrr::map_dfr(lambda, ~get_sse(x,y,test,.x))

    ### lambda_errors should be a data frame with two columns: "lambda" and "error"
    ### For each lambda, you should record the resulting Sum of Squared error
    ### (i.e., the predicted value minus the real value squared) from prediction
    ### on the test dataset. ie: sum((y-hat - yi)^2)

    return(lambda_errors)
  }

