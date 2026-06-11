# discnorm 0.2.2

* Fixed a bug in the internal numerical approximation of marginal standard
  deviations (used by `catLSadj()` when a margin is supplied without `sd`):
  the approximation was wrong for distributions whose support does not
  include zero.
* `catLSadj()` now coerces its input to a data frame (matrix input was
  previously treated as continuous data, silently skipping the polychoric
  step) and validates that `marginslist` matches the number of columns.
* `bootTest()` now gives informative errors for missing values,
  non-integer data, and datasets with fewer than two columns.
* Updated `inst/CITATION` to `bibentry()` style as requested by CRAN,
  corrected its header (it referred to the covsim package), and added the
  Grønneberg & Foldnes (2022) reference for `catLSadj()`.
* Added a testthat suite.

# discnorm 0.2.1

Resolved dependency issue with package CDM

# discnorm 0.2.0

Added function catLSadj() for adjusted polychoric correlations.

# discnorm 0.1.1

Updated release, concerning polychoric estimator options.
