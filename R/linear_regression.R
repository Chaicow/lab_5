#' Implements simple linear regression by hand
#'
#' @param dat A data frame
#' @param response The name of a response variable in the data frame (unquoted)
#' @param explanatory The name of the explanatory variable in the data frame (unquoted)
#' @param method The method used to compute the coefficients (NULL, "qr", "gradientdescent")
#'
#' @return A data frame of coefficients
#'
#' @import dplyr
#'
#' @export
simple_linear_regression <- function(dat, response, explanatory, method = NULL){

  x <- dat %>% pull({{explanatory}})
  y <- dat %>% pull({{response}})

  explan_name <- dat %>%
    select({{explanatory}}) %>%
    names()

  ### Edit code after here

  x_mat <- cbind(1, x) %>%
    as.matrix()

  y_mat <- as.matrix(y)

  x_inverse <- solve(crossprod(x_mat))

  xy_mat <- t(x_mat) %*% y_mat

  betas <- x_inverse %*% xy_mat

  beta_0 <- betas[1]
  beta_1 <- betas[2]

  ### Stop editing

  results <- tibble::tibble(
    Intercept = beta_0,
    Slope = beta_1
  )

  names(results)[2] <- explan_name

  return(results)

}

#' Implements linear regression with many predictors by hand
#'
#' This function computes coefficients for multiple regression by hand.
#' All columns of the provided data frame are used as predictors, except the
#' one specified as a response.
#'
#' No interaction terms are included.
#'
#'
#' @param dat A data frame
#' @param response The name of a response variable in the data frame (unquoted)
#' @param method The method used to compute the coefficients (NULL, "qr", "gradientdescent")
#'
#' @return A data frame of coefficients
#'
#' @import dplyr
#'
#'@export
#'
multiple_linear_regression <- function(dat, response, method = NULL) {
  y <- as.matrix(dat %>%
                   pull({{response}}))
  x <- dat %>%
    select(-{{response}})

  x <- cbind(1, x) %>%
    as.matrix()

  x_inverse <- solve(crossprod(x))
  xy_mat <- t(x) %*% y
  coefs <- x_inverse %*% xy_mat

  df_coefs <- as.data.frame(t(coefs))

  colnames(df_coefs)[1] <- "Intercept"

  return(df_coefs)

}
