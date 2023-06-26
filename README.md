# pheble
pheble is a software package for classifying high-dimensional phenotypes with ensemble learning.

### To install the current pheble R package from CRAN:

<i> Within R:</i>

<code> install.packages("pheble", dependencies=TRUE) </code>

### To install the current pheble R package from Github using devtools:

<i> Within R:</i>

<code> install.packages("devtools") </code>

<code> devtools::install_github("jaydevine/pheble") </code>

# Notes:
This package contains a number of dependencies that contain different models for the ensemble. If <code> dependencies=TRUE </code> is not specified in <code> install.packages() </code>, you may run into issues. We ran into problems with outdated vctrs and cli packages. To fix the issue, we uninstalled and reinstalled them.

For any questions, you can contact the author via email: jay.devine1@ucalgary.ca
