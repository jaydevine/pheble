#' Classify phenotypes via ensemble learning.
#'
#' The \code{ph_ensemble} function recruits classification predictions from a list of algorithms to train an ensemble model. This can be a list of manually trained algorithms from \code{train} or, more conveniently, the output from \code{ph_train}. The hyperparameter tuning and model evaluations are handled internally to simplify the ensembling process.
#'
#' @param train_models A \code{list} of at least two \code{train} models.
#' @param train_df A \code{data.frame} containing a class column and the training data.
#' @param vali_df A \code{data.frame} containing a class column and the validation data.
#' @param test_df A \code{data.frame} containing a class column and the test data.
#' @param class_col A \code{character} value for the name of the class column. This should be consistent across data frames.
#' @param resample_method A \code{character} value for the resampling training method: "boot" (default), "cv", LOOCV", "repeatedcv".
#' @param number An \code{integer} value for the number of resampling iterations (25 default for boot) or folds (10 default for cross-validation).
#' @param repeats An \code{integer} value for the number of sets of folds for repeated cross-validation.
#' @param search A \code{character} value for the hyperparameter search strategy: "random" (default), "grid".
#' @param sampling A \code{character} value for the sampling strategy, sometimes used to fix class imbalances: \code{NULL} (default), "up", "down", "smote".
#' @param n_cores An \code{integer} value for the number of cores to include in the cluster: detectCores() - 1 (default).
#' @param task A \code{character} value for the type of classification \code{task}: "multi" (default), "binary".
#' @param metric A \code{character} value for which summary metric should be used to select the optimal model: "ROC" (default for "binary") and "Kappa" (default for "multi")
#' @param top_models A \code{numeric} value for the top n training models to ensemble: 3 (default). Every training model is ordered according to their final metric value (e.g., "ROC" or "Kappa") and the top n models are selected.
#' @param metalearner A \code{character} value for the algorithm used to train the ensemble: "glmnet" (default), "rf". Other methods, such as those listed in ph_train methods, may also be used.
#' @param tune_length If \code{search = "random"} (default), this is an \code{integer} value for the maximum number of hyperparameter combinations to test for each training model in the ensemble; if \code{search = "grid"}, this is an \code{integer} value for the number of levels of each hyperparameter to test for each model.
#' @param quiet A \code{logical} value for whether progress should be printed: TRUE (default), FALSE.
#' @return A list containing the following components:\tabular{ll}{
#'    \code{ensemble_test_preds} \tab The ensemble predictions for the test set. \cr
#'    \tab \cr
#'    \code{vali_preds} \tab The validation predictions for the top models. \cr
#'    \tab \cr
#'    \code{test_preds} \tab The test predictions for the top models. \cr
#'    \tab \cr
#'    \code{all_test_preds} \tab The test predictions for every successfully trained model. \cr
#'    \tab \cr
#'    \code{all_test_results} \tab The confusion matrix results obtained from comparing the model test predictions (i.e., original models and ensemble) against the actual test classes.  \cr
#'    \tab \cr
#'    \code{ensemble_model} \tab The ensemble \code{train} object. \cr
#'    \tab \cr
#'    \code{var_imps} \tab The ensemble variable importances obtained via weighted averaging. The original train importances are multiplied by the model's importance in the ensemble, then averaged across models and normalized.  \cr
#'    \tab \cr
#'    \code{train_df} \tab The training data frame. \cr
#'    \tab \cr
#'    \code{vali_df} \tab The validation data frame. \cr
#'    \tab \cr
#'    \code{test_df} \tab The test data frame. \cr
#'    \tab \cr
#'    \code{train_models} \tab The \code{train} models for the ensemble. \cr
#'    \tab \cr
#'    \code{ctrl} \tab A \code{trainControl} object. \cr
#'    \tab \cr
#'    \code{metric} \tab The summary metric used to select the optimal model. \cr
#'    \tab \cr
#'    \code{task} \tab The type of classification task. \cr
#'    \tab \cr
#'    \code{tuneLength} \tab The maximum number of hyperparameter combinations ("random") or individual hyperparameter depth ("grid").  \cr
#'    \tab \cr
#'    \code{top_models} \tab The number of top methods selected for the ensemble.  \cr
#'    \tab \cr
#'    \code{metalearner} \tab The algorithm used to train the ensemble. \cr
#' }
#' @export
ph_ensemble <- function(train_models, train_df, vali_df, test_df, class_col = "class",
                        resample_method = "boot", number = ifelse(grepl("cv", resample_method, ignore.case = TRUE), 10, 25),
                        repeats = ifelse(grepl("dcv$", resample_method, ignore.case = TRUE), 3, NA),
                        search = "random", sampling = NULL, n_cores = parallel::detectCores() - 1, task = "multi",
                        metric = ifelse(task == "multi", "Kappa", "ROC"), top_models = 3,
                        metalearner = ifelse(task == "multi", "glmnet", "rf"),
                        tune_length = 10, quiet = FALSE)
{
    # Ensure that class is the first column of each data frame.
    if (!is.character(class_col)) { class_col <- as.character(class_col) }
    if (class_col %in% colnames(train_df) != TRUE |
        class_col %in% colnames(vali_df) != TRUE |
        class_col %in% colnames(test_df) != TRUE)
        stop("Either the class column name is not in one or more of the data frames or it is inconsistent between data frames.")
    train_df <- train_df[, c(which(colnames(train_df) == class_col), which(colnames(train_df) != class_col))]
    vali_df <- vali_df[, c(which(colnames(vali_df) == class_col), which(colnames(vali_df) != class_col))]
    test_df <- test_df[, c(which(colnames(test_df) == class_col), which(colnames(test_df) != class_col))]
    colnames(train_df)[which(colnames(train_df) == class_col)] <- "class"
    colnames(vali_df)[which(colnames(vali_df) == class_col)] <- "class"
    colnames(test_df)[which(colnames(test_df) == class_col)] <- "class"
    if (all.equal(levels(train_df$class), levels(vali_df$class)) != TRUE |
        all.equal(levels(train_df$class), levels(test_df$class)) != TRUE |
        all.equal(levels(vali_df$class), levels(test_df$class)) != TRUE)
        stop("The class levels are inconsistent between data frames.")
    if (ncol(train_df) != ncol(vali_df) |
        ncol(train_df) != ncol(test_df) |
        ncol(vali_df) != ncol(test_df))
        stop("The number of predictors (columns) are inconsistent between data frames.")
    if (!is.numeric(n_cores))
        stop("The number of cores must be numeric (an integer).")
    if (!(task %in% c("multi", "binary")))
        stop("The classification task does not exist.")
    if (length(train_models) < 2)
        stop("At least two train models are needed for the ensemble.")
    if (!(metric %in% c("logLoss", "Accuracy", "Mean_Balanced_Accuracy", "Mean_F1", "Kappa", "ROC")))
        stop("The metric does not exist.")
    if (tune_length < 1)
        stop("Tune length must be 1 or higher.")
    if (!is.numeric(top_models))
        stop("The number of models for the ensemble must be numeric (an integer).")
    # Start cluster.
    cl <- parallel::makeCluster(n_cores)
    doParallel::registerDoParallel(cl)
    train_x <- train_df[, -which(names(train_df) %in% c("class"))]
    vali_x <- vali_df[, -which(names(vali_df) %in% c("class"))]
    test_x <- test_df[, -which(names(test_df) %in% c("class"))]
    train_df$class <- as.factor(train_df$class)
    vali_df$class <- as.factor(vali_df$class)
    test_df$class <- as.factor(test_df$class)
    # Check levels.
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
    # Evaluate every initial model.
    all_test_preds = data.frame(matrix(nrow = nrow(test_df), ncol = length(train_models)))
    test_preds_cm <- list()
    iter_a <- 0
    if (quiet != TRUE) { message("Generating test predictions for all models.") }
    for (i in train_models) {
        iter_a <- iter_a + 1
        test_eval <- ph_eval(df = test_df, model = i, class = test_df$class)
        all_test_preds[, iter_a] <- test_eval$preds
        test_preds_cm[[iter_a]] <- test_eval$cm
        colnames(all_test_preds)[iter_a] <- names(train_models)[iter_a]
        names(test_preds_cm)[iter_a] <- names(train_models)[iter_a]
    }
    test_results <- do.call("rbind", test_preds_cm)
    rownames(test_results) <- names(test_preds_cm)
    # Rank the models.
    rValues <- caret::resamples(train_models)
    rValues_sum <- summary(rValues)
    acc_sum <- as.data.frame(rValues_sum[[3]][metric])
    # Order methods by mean (4th col).
    if (quiet != TRUE) { message("Ranking train models.") }
    acc_sum <- acc_sum[order(-acc_sum[,4]), ]
    if (top_models > nrow(acc_sum)) {
        top_models <- nrow(acc_sum)
        warning(paste0("The number of top models is greater than the number of potential models. Setting top_models to ", top_models, "."))
    }
    top_methods <- rownames(acc_sum)[1:top_models]
    # Match acc_methods with method_list.
    top_acc_match <- match(top_methods, names(train_models))
    train_models <- train_models[c(top_acc_match)]
    # Get variable importances from these models.
    if (quiet != TRUE) { message("Prepare variable importance data frame.") }
    var_imps <- matrix(nrow = ncol(train_df[, -1]), ncol = length(top_acc_match))
    colnames(var_imps) <- names(train_models)
    rownames(var_imps) <- colnames(train_df[, -1])
    # Evaluate models on validation and test sets.
    vali_preds = data.frame(matrix(nrow = nrow(vali_df), ncol = length(top_acc_match)))
    test_preds = data.frame(matrix(nrow = nrow(test_df), ncol = length(top_acc_match)))
    iter_b <- 0
    if (quiet != TRUE) { message("Generating validation and test predictions for ensemble model.") }
    for (i in train_models) {
        iter_b <- iter_b + 1
        var_imp <- caret::varImp(i)
        var_imp_match <- match(rownames(var_imp$importance), rownames(var_imps))
        var_imps[var_imp_match, iter_b] <- var_imp$importance[, 1]
        vali_preds[, iter_b] <- stats::predict(i, vali_df)
        test_preds[, iter_b] <- stats::predict(i, test_df)
        colnames(vali_preds)[iter_b] <- names(train_models)[iter_b]
        colnames(test_preds)[iter_b] <- names(train_models)[iter_b]
    }
    # Convert NAs to 0s. Model unfortunately doesn't support variable importance.
    var_imps[is.na(var_imps)] <- 0
    # Concatenate original validation and test class with preds.
    vali_preds <- data.frame(class = vali_df$class, vali_preds, stringsAsFactors = F)
    test_preds <- data.frame(class = test_df$class, test_preds, stringsAsFactors = F)
    # Make sure factor levels are the same across data frame.
    vali_preds <- ph_equate(vali_preds, "class")
    test_preds <- ph_equate(test_preds, "class")
    # Ensemble model.
    if (quiet != TRUE) { message("Working on ensemble model.") }
    ensemble_model <- caret::train(class~., data = vali_preds, metric = metric, method = metalearner,
                                   allowParallel = TRUE, trControl = ctrl, tuneLength = tune_length)
    ensemble_imp <- caret::varImp(ensemble_model)
    if (quiet != TRUE) { message("Training complete.") }
    ensemble_eval <- ph_eval(df = test_preds, model = ensemble_model, class = test_df$class)
    ensemble_test_preds <- ensemble_eval$preds
    ensemble_test_results <- ensemble_eval$cm
    rownames(ensemble_test_results) <- paste0("top_", top_models, "_ensemble")
    # Variable importances from ensemble.
    var_imps <- as.data.frame(rowMeans(t(t(var_imps) * ensemble_imp$importance[,1])))
    colnames(var_imps)[1] <- "Importance"
    var_imps <- var_imps %>% dplyr::arrange(dplyr::desc(Importance))
    var_imps$Importance <- (var_imps$Importance - min(var_imps$Importance)) /
                           (max(var_imps$Importance) - min(var_imps$Importance))
    # Combine individual model test results and ensemble test results, then sort by balanced accuracy.
    all_test_results <- rbind.data.frame(test_results, ensemble_test_results)
    all_test_results <- data.table::setDT(all_test_results, keep.rownames = TRUE)[order(`Balanced Accuracy`, decreasing = TRUE)]
    colnames(all_test_results)[1] <- "Method"
    # Turn off cluster.
    on.exit(parallel::stopCluster(cl))
    # Output vars.
    list(ensemble_test_preds = ensemble_test_preds, vali_preds = vali_preds, test_preds = test_preds,
         all_test_preds = all_test_preds, all_test_results = all_test_results, ensemble_model = ensemble_model,
         var_imps = var_imps, train_df = train_df, vali_df = vali_df, test_df = test_df,
         train_models = train_models, ctrl = ctrl, metric = metric, task = task,
         tune_length = tune_length, top_models = top_models, metalearner = metalearner)
}
