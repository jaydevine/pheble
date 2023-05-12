## R CMD check results

0 errors | 0 warnings | 2 notes

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

* This is a new release.
