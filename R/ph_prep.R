#' Preprocessing for phenotype classification via ensemble learning.
#'
#' The \code{ph_prep} function splits a data frame into training, validation, and test sets, all while ensuring that every class is represented in each dataset. By default, it performs a Principal Component Analysis on the training set data and projects the validation and test data into that space.
#'
#' @param df A \code{data.frame} containing a column of unique ids, a column of classes, and an arbitrary number of \code{numeric} columns.
#' @param ids_col A \code{character} value for the name of the ids column.
#' @param class_col A \code{character} value for the name of the class column.
#' @param vali_pct A \code{numeric} value for the percentage of training data to use as validation data: 0.15 (default).
#' @param test_pct A \code{numeric} value for the percentage of total data to use as test data: 0.15 (default).
#' @param pca A \code{logical} value for completing principal component analysis on the dataset to reduce dimensionality: \code{TRUE} (default).
#' @param pca_pct A \code{numeric} value for the proportion of variance to subset the PCA with: 0.95 (default).
#' @return A list containing the following components:\tabular{ll}{
#'    \code{train_df} \tab The training set data frame. \cr
#'    \tab \cr
#'    \code{vali_df} \tab The validation set data frame. \cr
#'    \tab \cr
#'    \code{test_df} \tab The test set data frame. \cr
#'    \tab \cr
#'    \code{train_split} \tab The training set indices from the original data frame. \cr
#'    \tab \cr
#'    \code{vali_split} \tab The validation set indices from the original data frame. \cr
#'    \tab \cr
#'    \code{test_split} \tab The test set indices from the original data frame. \cr
#'    \tab \cr
#'    \code{vali_pct} \tab The percentage of training data used as validation data. \cr
#'    \tab \cr
#'    \code{test_pct} \tab The percentage of total data used as test data. \cr
#'    \tab \cr
#'    \code{pca} \tab The training set \code{prcomp} object. \cr
#'    \tab \cr
#'    \code{pca_pct} \tab The proportion of variance used to subset the PCA. \cr
#' }
#' @export
ph_prep <- function(df, ids_col, class_col, vali_pct = 0.15, test_pct = 0.15,
                    pca = TRUE, pca_pct = 0.95)
{
    output <- NULL
    if (!is.data.frame(df)) { df <- as.data.frame(df) }
    if (any(is.na(df)) != FALSE) {
        df <- stats::na.omit(df)
        warning("The data frame contains NAs. Rows have been removed.")
    }
    if (!is.character(ids_col)) { ids_col <- as.character(ids_col) }
    if (!is.character(class_col)) { class_col <- as.character(class_col) }
    if (!(ids_col %in% colnames(df)))
        stop("The ids column is either not in the data frame or it is differently named.")
    if (!(class_col %in% colnames(df)))
        stop("The class column is either not in the data frame or it is differently named.")
    # Convert ids and class column names.
    colnames(df)[which(colnames(df) == ids_col)] <- "ids"
    colnames(df)[which(colnames(df) == class_col)] <- "class"
    if (any(duplicated(df$ids)) != FALSE)
        stop("This data frame contains duplicate ids. Ensure they are unique.")
    if (data.table::between(test_pct, 0, 1) != TRUE)
        stop("The percentage of data for testing must be expressed as a decimal between 0 and 1.")
    if (data.table::between(vali_pct, 0, 1) != TRUE)
      stop("The percentage of data for validation must be expressed as a decimal between 0 and 1.")
    # Split entire data frame with testing indices.
    test_split <- c(caret::createDataPartition(as.factor(df$class), times = 1, p = test_pct, list = F))
    test_df <- df[test_split, ]
    train_df <- df[-test_split, ]
    # Split training set with validation indices.
    vali_samp <- caret::createDataPartition(as.factor(train_df$class), p = vali_pct, list = F)
    vali_df <- train_df[vali_samp, ]
    train_seq <- seq(1, length(train_df$class), 1)
    train_samp <- setdiff(train_seq, vali_samp)
    train_split <- stats::na.omit(match(train_df$ids[train_samp], df$ids))
    vali_split <- stats::na.omit(match(train_df$ids[vali_samp], df$ids))
    # Make class column first column.
    df <- df[, c(which(colnames(df) == class_col), which(colnames(df) != class_col))]
    rownames(df) <- df$ids
    # Get rid of ids column.
    df <- df[, -which(names(df) %in% c("ids"))]
    # Subset original data frame with splitting indices.
    train_df <- df[c(train_split), ]
    vali_df <- df[c(vali_split), ]
    test_df <- df[c(test_split), ]
    if (pca != FALSE) {
        pca_pct <- pca_pct
        if (data.table::between(pca_pct, 0, 1) != TRUE)
            stop("The proportion of variance to subset the PCA with must be expressed as a decimal between 0 and 1.")
        pca_obj <- stats::prcomp(train_df[,-1])
        mu <- colMeans(train_df[,-1])
        pov <- pca_obj$sdev^2/sum(pca_obj$sdev^2)
        scores <- as.data.frame(pca_obj$x)
        count <- 0
        var_sum <- 0
        pc_vec <- c()
        # Loop through proportion of variance and add pc # to vector.
        while (var_sum < pca_pct) {
            count <- count + 1
            var_sum <- var_sum + pov[count]
            pc_vec[count] <- count
        }
        pc_vec <- sort(pc_vec)
        scores <- pca_obj$x[, c(pc_vec)]
        output <- list(scores = scores, pc_vec = pc_vec, pca_obj = pca_obj, mu = mu)
        # Project data.
        train_pca <- scale(train_df[,-1], output$pca_obj$center, output$pca_obj$scale) %*% output$pca_obj$rotation
        vali_pca <- scale(vali_df[,-1], output$pca_obj$center, output$pca_obj$scale) %*% output$pca_obj$rotation
        test_pca <- scale(test_df[,-1], output$pca_obj$center, output$pca_obj$scale) %*% output$pca_obj$rotation
        # Subset data by pc vector.
        train_pcs <- train_pca[, c(output$pc_vec)]
        vali_pcs <- vali_pca[, c(output$pc_vec)]
        test_pcs <- test_pca[, c(output$pc_vec)]
        # Bind class with pcs.
        train_df <- cbind.data.frame(train_df$class, train_pcs)
        vali_df <- cbind.data.frame(vali_df$class, vali_pcs)
        test_df <- cbind.data.frame(test_df$class, test_pcs)
        colnames(train_df)[1] <- "class"
        colnames(vali_df)[1] <- "class"
        colnames(test_df)[1] <- "class"
    }
    list(train_df = train_df, vali_df = vali_df, test_df = test_df,
         train_split = train_split, vali_split = vali_split, test_split = test_split,
         vali_pct = vali_pct, test_pct = test_pct, pca = pca, pca_pct = pca_pct)
}
