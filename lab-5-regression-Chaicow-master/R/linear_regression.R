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

  x_bar <- mean(x)
  y_bar <- mean(y)

### Edit code after here

  sd_x <- sd(x)
  sd_y <- sd(y)

  beta_1 <- calc_slope(x, x_bar, y, y_bar, sd_x, sd_y)
  beta_0 <- y_bar - beta_1 * x_bar

### Stop editing

  results <- tibble::tibble(
    Intercept = beta_0,
    Slope = beta_1
  )

  names(results)[2] <- explan_name

  return(results)
}

calc_slope <- function(x, x_bar, y, y_bar, sd_x, sd_y) {

  x_com <- c()
  y_com <- c()

for(i in 1:length(x)) {
    x_com <- c(x_com, (x[i] - x_bar) / sd_x)
    y_com <- c(y_com, (y[i] - y_bar) / sd_y)
}

  xy_com <- x_com * y_com

  r <- sum(xy_com) / (length(x) - 1)

  slope <- (r * sd_y) / sd_x

  return(slope)
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
  y <- as.matrix(dat %>% pull({{response}}))
  x <- dat %>% select(-{{response}})

  x <- cbind(1, x) %>%
    as.matrix()

  first <- solve(crossprod(x))
  second <- t(x)%*%y
  a <- first%*%second

 df_a <- as.data.frame(t(a))

 colnames(df_a)[1] <- "Intercept"

 return(df_a)

}
