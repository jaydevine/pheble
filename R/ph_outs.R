#' Compute interquartile range.
#'
#' The \code{ph_iqr} function computes the interquartile range.
#'
#' @param x A \code{numeric} vector.
#' @param na.rm A \code{logical} value: FALSE (default). If true, any NA is removed before quantiles are computed.
#' @param type An \code{integer} value (1:9) selecting one of 9 quantile algorithms.
#' @returns The interquartile range.
#' @export
ph_iqr <- function(x, na.rm = FALSE, type = 7)
{
    diff(stats::quantile(as.numeric(x), c(0.25, 0.75),
                         na.rm = na.rm, names = FALSE,
                         type = type))
}

#' Detect outliers.
#'
#' The \code{ph_outs} function detects outliers.
#'
#' @param x A \code{numeric} vector.
#' @returns The outlier indices.
#' @export
ph_outs <- function(x)
{
    q <- stats::quantile(x, probs=c(.25, .75), na.rm = FALSE)
    iqr <- ph_iqr(x)
    # Upper range.
    upper <-  q[2] + (1.5*iqr)
    # Lower range.
    lower <- q[1] - (1.5*iqr)
    # Return outlier indices.
    inds <- which(x > upper, arr.ind = TRUE)
    return(inds)
}
