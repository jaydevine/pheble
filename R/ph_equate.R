#' Equate factors levels across columns.
#'
#' The \code{ph_equate} function ensures that the factor levels in all columns are equal. When classification are heavily biased or inaccurate, they can return new class predictions that do not contain every level in the original data. This can interfere with model evaluation functions e.g. via a confusion matrix.
#'
#' @param df A \code{data.frame} of column-wise class predictions from each ensemble model.
#' @param class_col A \code{character} value for the column name of the actual classes.
#' @returns A \code{data.frame} of column-wise class predictions with equivalent class levels.
#' @export
ph_equate <- function(df, class_col)
{
    if (!is.character(class_col)) { class_col <- as.character(class_col) }
    # Ensure that the actual class column is the first column.
    df <- df[, c(which(colnames(df) == class_col), which(colnames(df) != class_col))]
    i <- 1
    # Move through remainder of data frame and ensure that each column has the same levels vector as the actual class column.
    while (i < length(names(df))) {
        i <- i + 1
        df[, i] <- factor(df[, i], levels = rev(unique(df[, 1])), ordered = TRUE)
    }
    return(df)
}
