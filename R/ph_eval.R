#' Evaluate a phenotype classification model.
#'
#' The \code{ph_eval} function generates a confusion matrix for binary or multi-class classification; for the multi-class case, the results are averaged across all class levels.
#'
#' @param df A \code{data.frame} of unseen data (e.g., validation or test data) that relates to the training data with which \code{model} was trained.
#' @param model A \code{train} model.
#' @param class A (\code{factor}) value of the actual classes of unseen data.
#' @return A list containing the following components:\tabular{ll}{
#'    \code{preds} \tab The class predictions. \cr
#'    \tab \cr
#'    \code{cm} \tab Evaluation results from the confusion matrix; for the multi-class case, the results are averaged across all class levels. \cr
#' }
#' @export
ph_eval <- function(df, model, class)
{
    if (!is.factor(class)) { class <- as.factor(class) }
    task <- ifelse(length(levels(class)) > 2, "multi", "binary")
    # Generate predictions with model.
    preds <- stats::predict(model, df)
    if (task == "multi") {
        # Confusion matrix; take the mean for a given class if the model returns NA.
        cm <- caret::confusionMatrix(preds, class)
        for (i in 1:ncol(cm$byClass)) {
            cm$byClass[is.na(cm$byClass[,i]), i] <- mean(cm$byClass[,i], na.rm = TRUE)
        }
        # Means and 95% ci; just going to retrieve means.
        cm_means <- as.data.frame(apply(as.matrix(cm$byClass), 2, function(x) gmodels::ci(x)))
        cm_means <- cm_means[1, ]
        # Accuracy from overall.
        acc <- unname(cm$overall[1])
        # Kappa from overall.
        kappa <- unname(cm$overall[2])
        cm <- cbind.data.frame(cm_means, acc, kappa)
        colnames(cm)[12:13] <- c("Accuracy", "Kappa")
    } else {
        # Confusion matrix.
        cm <- caret::confusionMatrix(preds, class)
        cm_means <- as.data.frame(t(cm$byClass))
        cm_overall <- as.data.frame(t(cm$overall))
        cm <- cbind.data.frame(cm_means, cm_overall)
    }
    list(preds = preds, cm = cm)
}
