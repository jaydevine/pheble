## R CMD check results

0 errors | 0 warnings | 4 notes

❯ checking CRAN incoming feasibility ... NOTE
  Maintainer: ‘Jay Devine <jay.devine1@ucalgary.ca>’
  
  New submission
  
  Version contains large components (0.0.0.9000)
  
  Possibly misspelled words in DESCRIPTION:
    Phenotypes (2:37)
  
  Found the following (possibly) invalid DOIs:
    DOI: TBD
      From: inst/CITATION
      Message: Invalid DOI
      
* "Phenotypes" is not misspelled. In addition, a DOI doesn't exist (i.e., "TBD") because the paper hasn't been published yet.

❯ checking package dependencies ... NOTE
  Imports includes 31 non-default packages.
  Importing from so many packages makes the package vulnerable to any of
  them becoming unavailable.  Move as many as possible to Suggests and
  use conditionally.

* We need all of these packages to call the methods in the ensemble, otherwise method = "all" can't be run.

❯ checking dependencies in R code ... NOTE
  Namespaces in Imports field not imported from:
    ‘C50’ ‘HDclassif’ ‘MASS’ ‘MLmetrics’ ‘Matrix’ ‘adabag’ ‘base’
    ‘caTools’ ‘e1071’ ‘earth’ ‘evtree’ ‘frbs’ ‘glmnet’ ‘hda’ ‘ipred’
    ‘kernlab’ ‘kknn’ ‘klaR’ ‘mda’ ‘nnet’ ‘party’ ‘pls’ ‘randomForest’
    ‘rpartScore’ ‘sparseLDA’ ‘themis’
    All declared Imports should be used.
    
* The methods from these packages are in caret's namespace, so we don't need to call them explicitly.

❯ checking examples ... NOTE
  Examples with CPU (user + system) or elapsed time > 5s
               user system elapsed
  ph_ensemble 4.069  0.712  99.884
  ph_train    3.437  0.646  88.118
  ph_eval     3.388  0.692  89.367
  ph_prep     3.026  0.714  87.920
  ph_anomaly  1.229  0.115  56.130
  
* These methods can take a few minutes to run, especially if the dataset is large.

* This is a new release.
