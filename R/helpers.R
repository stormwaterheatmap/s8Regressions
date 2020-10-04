# helper functions
#-------------------------------



#' Plot Distrbutions
#'
#'
#' @param coc Character vector naming the constituent of concern
#' @param df dataframe of observations
#'
#' @return plots of distributions
#' @export
#'
#' @examples
plot_distributions <- function(coc, df) {
  par(mfrow = c(2, 2))
  lab <- paste0(coc, " (", units, ")")
  x <- (df$concentration)
  car::qqPlot((x), "lnorm", lwd = 1, main = lab)
  car::qqPlot(x, "exp", lwd = 1, main = lab)

  fit.weibull <- fitdist(x, "weibull")
  car::qqPlot(x, "weibull",
    shape = fit.weibull$estimate[1],
    scale = fit.weibull$estimate[2], lwd = 1, main = lab
  )
  fit.gamma <- fitdist((x), "gamma")

  car::qqPlot((x), "gamma",
    shape = (fit.gamma$estimate[1]),
    scale = (fit.gamma$estimate[2]), lwd = 1, main = lab
  )
}


# Use the preProcess function from the caret package to generate preprocess elements
#' Title
#'
#' @param df data frame of observations
#' @param variables character vector of variables to center and scale
#'
#' @return list: (scaled: list of preprocess results,  df.transformed: dataframe of transformed data.)
#' @export
#'
#' @examples
scale_and_center <- function(df, variables) {
  df.scaled <- df.coc %>%
    dplyr::select(variables) %>%
    preProcess(method = c("center", "scale"))

  # produce a dataframe with the centered and scaled data
  df.transformed <- predict(df.scaled, df.coc)
  df.transformed$logConcentration <- log(df.transformed$concentration)

  return(list(scaled = df.scaled, transformed = df.transformed))
}




#' Select Lasso Function
#'
#' @param formula.obj - a formula representing the regression to test
#' @param df - dataframe containing observations
#'
#' @return df.out - df - dataframe containing lasso results
#' @export
#'
#' @examples
selectLasso <- function(formula.obj, df) {
  lambda <- 10^seq(-1, 2, length = 100)
  AIC_vec <- rep(Inf, length(lambda))
  coeff <- list()

  for (j in seq_len(length(lambda))) {
    ls1 <- (
      glmmLasso(
        formula.obj,
        # family = (),
        rnd = NULL,
        switch.NR = FALSE,
        data = df,
        lambda = lambda[j],
        final.re = F
      )
    )
    AIC_vec[j] <- ls1$aic
    coeff[[j]] <- data.frame(t(ls1$coefficients), lambda = lambda[j])
  }
  names(coeff) <- lambda
  df.out <-
    data.frame(matrix(unlist(coeff), nrow = length(lambda), byrow = T),
      stringsAsFactors =
        FALSE
    )
  colnames(df.out) <- c(names(coef(ls1)), "lambda")
  df.out <-
    pivot_longer(df.out,
      -c(lambda, `(Intercept)`),
      names_to = "predictor",
      values_to = "val"
    )
  return(df.out)
}



#' CV Function
#' Function for cross-validation of gamlss models
#'
#' @param models - list of models to test
#'
#' @return cv.df - dataframe of cross-validation results
#' @export
#'
#' @examples
cv.function <- function(models) {
  modCV <- list() # empty list of models.
  for (i in seq_len(length(models))) {
    model <- models[[i]]

    modCV[[i]] <- gamlssCV(
      formula = formula(model, what = "mu"),
      sigma.formula = formula(model, what = "sigma"),
      data = df.coc, family = LOGNO, rand = folds, control = gamlss.control(
        c.crit = 0.002,
        trace = FALSE
      )
    )
  }
  cv.df <-
    data.frame(
      "model" = names(models),
      "GAIC" = unlist(lapply(modCV, CV))
    )
  return(cv.df) # return results
}

#
#' Add Fit
#'
#' function to add fitted column to observatons
#' @param model - model to add fit to
#' @param data - dataframe of observations
#'
#' @return data frame with new column of fitted data
#' @export
#'
#' @examples
add.fit <- function(model, data) {
  return(data %>%
    add_column(.fit = fitted(model, data = data)) %>%
    mutate(logConcentration = log(concentration)))
}
