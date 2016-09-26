#' @title Plot the path of the variables of iregnet fit
#'
#' @description
#' Produces a coefficient profile plot of the coefficient paths for a fitted
#' "iregnet" object.
#'
#' @param fit The S3 object of type \code{iregnet} returned by the \code{iregnet}
#' method.
#'
#' @param xvar Variable on the X-axis against which the coefficients are
#' plotted. \code{norm} plots against the L1 norm of the coefficients, i.e.,
#' arclength. \code{lambda} plots against log-lambda sequence.
#'
#' @param label If \code{TRUE}, coefficient names / variable sequence numbers
#' are plotted along with the curves.
#'
#' @param ... Other parameters to be passed to the \code{matplot} function.
#'
#' @details
#' \code{Intercept} (if present) is \strong{not} included in the \code{arclength}
#' since it is never regularized. It is also not plotted.
#' This function uses \link{tidymatrix} function to obtain a matrix from the
#' \link{iregnet} object. It can be directly used to produce other plots.
#'
#' @method
plot.iregnet <- function(x, xvar=c("norm", "lambda"), label=T, ...) {
  stopifnot_error("Invalid / no x object provided", !missing(x),
                  class(x) == "iregnet")
  xvar <- match.arg(xvar)

  tidym <- tidymatrix(x)
  start_index <- as.integer(x$intercept) + 1
  n <- nrow(x$beta)
  varnames <- rownames(x$beta)[start_index:n]
  coefficients <- tidym[, varnames]
  switch(xvar,
    "lambda" = {
      xdata <- log(tidym[, xvar])
      xlab <- "Log Lambda"
    },
    "norm" = {
      xdata <- tidym[, xvar]
      xlab <- "L1 Norm of Coefficients"
    }
  )

  ylab <- "Coefficients"
  type = list(...)$type
  if (is.null(type))
    matplot(xdata, coefficients, lty=1, xlab=xlab, ylab=ylab, type="l", ...)
  else
    matplot(xdata, coefficients, lty=1, xlab=xlab, ylab=ylab, ...)

  if (label) {
     legend("topright", varnames, col=seq_len(n -1),cex=0.8,fill=seq_len(n))
  }
}
