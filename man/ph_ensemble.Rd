% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ph_ensemble.R
\name{ph_ensemble}
\alias{ph_ensemble}
\title{Classify phenotypes via ensemble learning.}
\usage{
ph_ensemble(
  train_models,
  train_df,
  vali_df,
  test_df,
  class_col,
  ctrl,
  train_seed = 123,
  n_cores = 2,
  task = "multi",
  metric = ifelse(task == "multi", "Kappa", "ROC"),
  top_models = 3,
  metalearner = ifelse(task == "multi", "glmnet", "rf"),
  tune_length = 10,
  quiet = FALSE
)
}
\arguments{
\item{train_models}{A \code{list} of at least two \code{train} models.}

\item{train_df}{A \code{data.frame} containing a class column and the training data.}

\item{vali_df}{A \code{data.frame} containing a class column and the validation data.}

\item{test_df}{A \code{data.frame} containing a class column and the test data.}

\item{class_col}{A \code{character} value for the name of the class column. This should be consistent across data frames.}

\item{ctrl}{A \code{list} containing the resampling strategy (e.g., "boot") and other parameters for \code{trainControl}. Automatically create one via \code{ph_ctrl} or manually create it with \code{trainControl}.}

\item{train_seed}{A \code{numeric} value to set the training seed and control the randomness of creating resamples: 123 (default).}

\item{n_cores}{An \code{integer} value for the number of cores to include in the cluster: 2 (default). We highly recommend increasing this value to, e.g., parallel::detectCores() - 1.}

\item{task}{A \code{character} value for the type of classification \code{task}: "multi" (default), "binary".}

\item{metric}{A \code{character} value for which summary metric should be used to select the optimal model: "ROC" (default for "binary") and "Kappa" (default for "multi"). Other options include "logLoss", "Accuracy", "Mean_Balanced_Accuracy", and "Mean_F1".}

\item{top_models}{A \code{numeric} value for the top n training models to ensemble: 3 (default). Every training model is ordered according to their final metric value (e.g., "ROC" or "Kappa") and the top n models are selected.}

\item{metalearner}{A \code{character} value for the algorithm used to train the ensemble: "glmnet" (default), "rf". Other methods, such as those listed in ph_train methods, may also be used.}

\item{tune_length}{If \code{search = "random"} (default), this is an \code{integer} value for the maximum number of hyperparameter combinations to test for each training model in the ensemble; if \code{search = "grid"}, this is an \code{integer} value for the number of levels of each hyperparameter to test for each model.}

\item{quiet}{A \code{logical} value for whether progress should be printed: TRUE (default), FALSE.}
}
\value{
A list containing the following components:\tabular{ll}{
\code{ensemble_test_preds} \tab The ensemble predictions for the test set. \cr
\tab \cr
\code{vali_preds} \tab The validation predictions for the top models. \cr
\tab \cr
\code{test_preds} \tab The test predictions for the top models. \cr
\tab \cr
\code{all_test_preds} \tab The test predictions for every successfully trained model. \cr
\tab \cr
\code{all_test_results} \tab The confusion matrix results obtained from comparing the model test predictions (i.e., original models and ensemble) against the actual test classes.  \cr
\tab \cr
\code{ensemble_model} \tab The ensemble \code{train} object. \cr
\tab \cr
\code{var_imps} \tab The ensemble variable importances obtained via weighted averaging. The original train importances are multiplied by the model's importance in the ensemble, then averaged across models and normalized.  \cr
\tab \cr
\code{train_df} \tab The training data frame. \cr
\tab \cr
\code{vali_df} \tab The validation data frame. \cr
\tab \cr
\code{test_df} \tab The test data frame. \cr
\tab \cr
\code{train_models} \tab The \code{train} models for the ensemble. \cr
\tab \cr
\code{ctrl} \tab A \code{trainControl} object. \cr
\tab \cr
\code{metric} \tab The summary metric used to select the optimal model. \cr
\tab \cr
\code{task} \tab The type of classification task. \cr
\tab \cr
\code{tune_length} \tab The maximum number of hyperparameter combinations ("random") or individual hyperparameter depth ("grid").  \cr
\tab \cr
\code{top_models} \tab The number of top methods selected for the ensemble.  \cr
\tab \cr
\code{metalearner} \tab The algorithm used to train the ensemble. \cr
}
}
\description{
The \code{ph_ensemble} function uses classification predictions from a list of algorithms to train an ensemble model.
This can be a list of manually trained algorithms from \code{train} or, more conveniently, the output from \code{ph_train}.
The hyperparameter tuning and model evaluations are handled internally to simplify the ensembling process. This function
assumes some preprocessing has been performed, hence the training, validation, and test set requirements.
}
\examples{
## Import data.
data(ph_crocs)
\donttest{
## Remove anomalies with autoencoder.
rm_outs <- ph_anomaly(df = ph_crocs, ids_col = "Biosample",
                      class_col = "Species", method = "ae")
## Preprocess anomaly-free data frame into train, validation, and test sets
## with PCs as predictors.
pc_dfs <- ph_prep(df = rm_outs$df, ids_col = "Biosample",
                  class_col = "Species", vali_pct = 0.15,
                  test_pct = 0.15, method = "pca")
## Echo control object for train function.
ctrl <- ph_ctrl(ph_crocs$Species, resample_method = "boot")
## Train all models for ensemble.
## Note: Increasing n_cores will dramatically reduce train time.
train_models <- ph_train(train_df = pc_dfs$train_df,
                         vali_df = pc_dfs$vali_df,
                         test_df = pc_dfs$test_df,
                         class_col = "Species",
                         ctrl = ctrl,
                         task = "multi",
                         methods = "all",
                         tune_length = 5,
                         quiet = FALSE)
## You can also train just a few, although more is preferable.
## Note: Increasing n_cores will dramatically reduce train time.
train_models <- ph_train(train_df = pc_dfs$train_df,
                         vali_df = pc_dfs$vali_df,
                         test_df = pc_dfs$test_df,
                         class_col = "Species",
                         ctrl = ctrl,
                         task = "multi",
                         methods = c("lda", "mda",
                         "nnet", "pda", "sparseLDA"),
                         tune_length = 5,
                         quiet = FALSE)
## Train the ensemble.
## Note: Increasing n_cores will dramatically reduce train time.
ensemble_model <- ph_ensemble(train_models = train_models$train_models,
                              train_df = pc_dfs$train_df,
                              vali_df = pc_dfs$vali_df,
                              test_df = pc_dfs$test_df,
                              class_col = "Species",
                              ctrl = ctrl,
                              task = "multi",
                              top_models = 3,
                              metalearner = "glmnet",
                              tune_length = 25,
                              quiet = FALSE)
}
}
