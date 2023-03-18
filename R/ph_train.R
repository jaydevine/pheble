#' Generate predictions for phenotype ensemble.
#'
#' The \code{ph_train} function automatically trains a set of binary or multi-class classification models to ultimately build a new dataset of predictions. The data preprocessing and hyperparameter tuning are handled internally to minimize user input and simplify the training.

#' @param df A \code{data.frame} containing a column of unique ids, a column of classes, and an arbitrary number of \code{numeric} columns.
#' @param ids_col A \code{character} value for the name of the ids column.
#' @param class_col A \code{character} value for the name of the class column.
#' @param vali_pct A \code{numeric} value for the percentage (decimal) of training data to use as validation data: 0.15 (default).
#' @param test_pct A \code{numeric} value for the percentage (decimal) of total data to use as test data: 0.15 (default).
#' @param pca A \code{logical} value for completing principal component analysis on the dataset to reduce dimensionality: \code{TRUE} (default).
#' @param pca_pct A \code{numeric} value for the proportion of variance decimal) to subset the PCA with: 0.95 (default).
#' @param resample_method A \code{character} value for the resampling training method: "boot" (default), "cv", LOOCV", "repeatedcv".
#' @param number An \code{integer} value for the number of resampling iterations (25 default for boot) or folds (10 default for cross-validation).
#' @param repeats An \code{integer} value for the number of sets of folds for repeated cross-validation.
#' @param search A \code{character} value for the hyperparameter search strategy: "random" (default), "grid".
#' @param sampling A \code{character} value for the sampling strategy, sometimes used to fix class imbalances: \code{NULL} (default), "up", "down", "smote".
#' @param n_cores An \code{integer} value for the number of cores to include in the cluster: detectCores() - 1 (default).
#' @param task A \code{character} value for the type of classification \code{task}: "multi" (default), "binary".
#' @param methods A \code{character} value enumerating the names (at least two, unless "all") of the classification methods to ensemble: "all" (default).
#' \itemize{
#'   \item If the \code{task} is "binary", there are 33 methods to choose from: "AdaBag", "AdaBoost.M1", "C5.0", "evtree", "glmnet", "hda", "kernelpls", "kknn", "lda", "loclda", "mda", "nb", "nnet", "pda", "pls", "qda", "rda", "rf", "sparseLDA", "stepLDA", "stepQDA", "treebag", "svmLinear", "svmPoly","svmRadial", "gaussprLinear" (slow), "gaussprPoly" (slow), "gaussprRadial" (slow), "bagEarthGCV", "cforest", "earth", "fda", "hdda".
#'   \item If the \code{task} is "multi", 30:  "AdaBag", "AdaBoost.M1",  "C5.0", "evtree", "glmnet", "hda", "kernelpls", "kknn", "lda", "loclda", "mda", "nb", "nnet", "pda", "pls", "qda", "rda", "rf", "sparseLDA", "stepLDA", "stepQDA", "treebag", "svmLinear", "svmPoly", "svmRadial", "bagEarthGCV", "cforest", "earth", "fda", "hdda".
#' }
#' @param metric A \code{character} value for which summary metric should be used to select the optimal model: "ROC" (default for "binary") and "Kappa" (default for "multi")
#' @param tune_length If \code{search = "random"} (default), this is an \code{integer} value for the maximum number of hyperparameter combinations to test for each training model in the ensemble; if \code{search = "grid"}, this is an \code{integer} value for the number of levels of each hyperparameter to test for each model.
#' @param quiet A \code{logical} value for whether progress should be printed: TRUE (default), FALSE.
#' @return A list containing the following components:\tabular{ll}{
#'    \code{train_models} \tab The \code{train} models for the ensemble. \cr
#'    \tab \cr
#'    \code{train_df} \tab The training data frame. \cr
#'    \tab \cr
#'    \code{vali_df} \tab The validation data frame. \cr
#'    \tab \cr
#'    \code{test_df} \tab The test data frame. \cr
#'    \tab \cr
#'    \code{splits} \tab The training, validation, and test set indices. \cr
#'    \tab \cr
#'    \code{task} \tab The type of classification task. \cr
#'    \tab \cr
#'    \code{ctrl} \tab A \code{trainControl} object. \cr
#'    \tab \cr
#'    \code{methods} \tab The names of the classification methods to ensemble. \cr
#'    \tab \cr
#'    \code{search} \tab The hyperparameter search strategy. \cr
#'    \tab \cr
#'    \code{n_cores} \tab The number of cores for parallel processing. \cr
#'    \tab \cr
#'    \code{metric} \tab The summary metric used to select the optimal model. \cr
#'    \tab \cr
#'    \code{tune_length} \tab The maximum number of hyperparameter combinations ("random") or individual hyperparameter depth ("grid").  \cr
#' }
#' @export
ph_train <- function(df, ids_col, class_col, vali_pct = 0.15, test_pct = 0.15, pca = TRUE,
                     pca_pct = 0.95, resample_method = "boot", number = ifelse(grepl("cv", resample_method, ignore.case = TRUE), 10, 25),
                     repeats = ifelse(grepl("dcv$", resample_method, ignore.case = TRUE), 3, NA),
                     search = "random", sampling = NULL, n_cores = parallel::detectCores() - 1, task = "multi", methods = "all",
                     metric = ifelse(task == "multi", "Kappa", "ROC"), tune_length = 10, quiet = FALSE)
{
    if (!is.numeric(n_cores))
        stop("Number of cores must be numeric (an integer).")
    if (!(task %in% c("multi", "binary")))
        stop("Classification task does not exist.")
    if (all(methods %in% c("AdaBag", "AdaBoost.M1", "bagEarthGCV", "C5.0", "cforest", "earth",
                           "evtree", "fda", "gaussprLinear", "gaussprPoly", "gaussprRadial",
                           "glmnet", "hda", "hdda", "kernelpls", "kknn", "lda", "loclda", "mda",
                           "nb", "nnet", "pda", "pls", "qda", "rda", "rf", "sparseLDA",
                           "stepLDA", "stepQDA", "svmLinear", "svmPoly", "svmRadial",
                           "treebag", "all")) != TRUE)
        stop("At least one classification method has been entered incorrectly or is not available.")
    if (length(methods) > 1) {
        if ("all" %in% methods != FALSE)
           stop("The method \"all\" cannot be added to other methods.")
    }
    if (!(metric %in% c("logLoss", "Accuracy", "Mean_Balanced_Accuracy", "Mean_F1", "Kappa", "ROC")))
        stop("Metric does not exist.")
    if (tune_length < 1)
        stop("Tune length must be 1 or higher.")
    # Start cluster.
    cl <- parallel::makeCluster(n_cores)
    doParallel::registerDoParallel(cl)
    # Prepare data.
    pre_df <- ph_prep(df = df, ids_col = ids_col, class_col = class_col,
                      vali_pct = vali_pct, test_pct = test_pct, pca = pca,
                      pca_pct = pca_pct)
    if (quiet != TRUE) { message("Preprocessing complete.") }
    # Explicitly define training, validation, and testing variables.
    splits <- list(pre_df$train_split, pre_df$vali_split, pre_df$test_split)
    names(splits)[1:3] <- c("train_split", "vali_split", "test_split")
    train_df <- pre_df$train_df
    vali_df <- pre_df$vali_df
    test_df <- pre_df$test_df
    train_x <- pre_df$train_df[, -which(names(pre_df$train_df) %in% c("class"))]
    vali_x <- pre_df$vali_df[, -which(names(pre_df$vali_df) %in% c("class"))]
    test_x <- pre_df$test_df[, -which(names(pre_df$test_df) %in% c("class"))]
    train_df$class <- as.factor(train_df$class)
    vali_df$class <- as.factor(vali_df$class)
    test_df$class <- as.factor(test_df$class)
    if (length(levels(train_df$class)) > 2 & task == "binary")
        stop("The task and number of class levels do not match.")
    # Verify that factor levels in test and validation datasets have > 2 observations.
    vali_levels <- as.data.frame(table(vali_df$class))
    test_levels <- as.data.frame(table(test_df$class))
    vali_check <- which(vali_levels$Freq < 2)
    test_check <- which(test_levels$Freq < 2)
    if (length(vali_check) > 0)
      stop(cat(paste0(vali_levels$Var1[vali_check], " needs at least two observations in the validation set.", "\n"), sep = ""))
    if (length(test_check) > 0)
      stop(cat(paste0(test_levels$Var1[test_check], " needs at least two observations in the test set.", "\n"), sep = ""))
    # Define resampling strategy.
    ctrl <- ph_ctrl(class = train_df$class, resample_method = resample_method, number = number,
                    repeats = repeats, search = search, sampling = sampling)
    # Initialize classification loop.
    train_models <- list()
    if (task == "binary") {
        par_methods <- c("AdaBag", "AdaBoost.M1", "C5.0", "evtree", "glmnet",
                         "hda", "kernelpls", "kknn", "lda", "loclda", "mda",
                         "nb", "nnet", "pda", "pls", "qda", "rda", "rf", "sparseLDA",
                         "stepLDA", "stepQDA", "treebag")
        formula_methods <- c("svmLinear", "svmPoly","svmRadial", "gaussprLinear",
                             "gaussprPoly", "gaussprRadial")
        nopar_methods <- c("bagEarthGCV", "cforest", "earth", "fda", "hdda")
        if ("all" %in% methods) {
            par_methods <- par_methods
            formula_methods <- formula_methods
            nopar_methods <- nopar_methods
        } else {
            if (all(methods %in% c(par_methods, formula_methods, nopar_methods)) != TRUE)
                stop("At least one classification method has been entered incorrectly or is not available for binary classification.")
            if (length(methods) < 2)
                stop("At least two methods are required for the ensemble.")
            par_methods <- intersect(methods, par_methods)
            formula_methods <- intersect(methods, formula_methods)
            nopar_methods <- intersect(methods, nopar_methods)
        }
        iter_a <- 0
        if (length(par_methods) > 0) {
            for (i in par_methods) {
                iter_a <- iter_a + 1
                if (quiet != TRUE) { message(paste0("Working on ", i, " model.")) }
                train_models[[iter_a]] <- try(caret::train(x = train_x, y = train_df$class, metric = metric,
                                                           method = i, allowParallel = TRUE,
                                                           trControl = ctrl, tuneLength = tune_length),
                                              silent = TRUE)
                if (inherits(train_models[[iter_a]], "try-error")) { next }
                names(train_models)[iter_a] <- i
            }
        }
        iter_b <- length(par_methods)
        if (length(formula_methods) > 0) {
            for (i in formula_methods) {
                iter_b <- iter_b + 1
                if (quiet != TRUE) { message(paste0("Working on ", i, " model.")) }
                train_models[[iter_b]] <- try(caret::train(class~., data = train_df, metric = metric, method = i,
                                                           allowParallel = TRUE, trControl = ctrl,
                                                           tuneLength = tune_length),
                                              silent = TRUE)
                if (inherits(train_models[[iter_b]], "try-error")) { next }
                names(train_models)[iter_b] <- i
            }
        }
        iter_c <- length(par_methods) + length(formula_methods)
        if (length(nopar_methods) > 0) {
            for (i in nopar_methods) {
                iter_c <- iter_c + 1
                if (quiet != TRUE) { message(paste0("Working on ", i, " model.")) }
                train_models[[iter_c]] <- try(caret::train(x = train_x, y = train_df$class, metric = metric, method = i,
                                                           trControl = ctrl, tuneLength = tune_length),
                                              silent = TRUE)
                if (inherits(train_models[[iter_c]], "try-error")) { next }
                names(train_models)[iter_c] <- i
            }
        }
        if (quiet != TRUE) { message("Training complete.") }
    } else {
        par_methods <- c("AdaBag", "AdaBoost.M1",  "C5.0", "evtree", "glmnet", "hda",
                         "kernelpls", "kknn", "lda", "loclda", "mda", "nb", "nnet",
                         "pda", "pls", "qda", "rda", "rf", "sparseLDA","stepLDA","stepQDA",
                         "treebag")
        formula_methods <- c("svmLinear", "svmPoly", "svmRadial")
        nopar_methods <- c("bagEarthGCV", "cforest", "earth", "fda", "hdda")
        if ("all" %in% methods) {
            par_methods <- par_methods
            formula_methods <- formula_methods
            nopar_methods <- nopar_methods
        } else {
            if (all(methods %in% c(par_methods, formula_methods, nopar_methods)) != TRUE)
                stop("At least one classification method has been entered incorrectly or is not available for multi-class classification.")
        if (length(methods) < 2)
            stop("At least two methods are required for the ensemble.")
        par_methods <- intersect(methods, par_methods)
        formula_methods <- intersect(methods, formula_methods)
        nopar_methods <- intersect(methods, nopar_methods)
        }
        iter_a <- 0
        if (length(par_methods) > 0) {
            for (i in par_methods) {
                iter_a <- iter_a + 1
                if (quiet != TRUE) { message(paste0("Working on ", i, " model.")) }
                train_models[[iter_a]] <- try(caret::train(x = train_x, y = train_df$class, metric = metric,
                                                           method = i, allowParallel = TRUE,
                                                           trControl = ctrl, tuneLength = tune_length),
                                              silent = TRUE)
                if (inherits(train_models[[iter_a]], "try-error")) { next }
                names(train_models)[iter_a] <- i
            }
        }
        iter_b <- length(par_methods)
        if (length(formula_methods) > 0) {
            for (i in formula_methods) {
                iter_b <- iter_b + 1
                if (quiet != TRUE) { message(paste0("Working on ", i, " model.")) }
                train_models[[iter_b]] <- try(caret::train(class~., data = train_df, metric = metric, method = i,
                                                           allowParallel = TRUE, trControl = ctrl,
                                                           tuneLength = tune_length),
                                              silent = TRUE)
                if (inherits(train_models[[iter_b]], "try-error")) { next }
                names(train_models)[iter_b] <- i
            }
        }
        iter_c <- length(par_methods) + length(formula_methods)
        if (length(nopar_methods) > 0) {
            for (i in nopar_methods) {
                iter_c <- iter_c + 1
                if (quiet != TRUE) { message(paste0("Working on ", i, " model.")) }
                train_models[[iter_c]] <- try(caret::train(x = train_x, y = train_df$class, metric = metric, method = i,
                                                           trControl = ctrl, tuneLength = tune_length),
                                              silent = TRUE)
                if (inherits(train_models[[iter_c]], "try-error")) { next }
                names(train_models)[iter_c] <- i
            }
        }
        if (quiet != TRUE) { message("Training complete.") }
    }
    train_fail <- which(lapply(train_models, length) == 1)
    if (length(train_fail) > 0) {
        train_models <- train_models[-c(train_fail)]
    }
    # Turn off cluster.
    on.exit(parallel::stopCluster(cl))
    # Output vars.
    list(train_models = train_models, train_df = train_df, vali_df = vali_df, test_df = test_df,
         splits = splits, task = task, ctrl = ctrl, methods = methods,
         search = search, n_cores = n_cores, metric = metric, tune_length = tune_length)
}
