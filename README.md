<img src="https://cran.r-project.org/web/packages/ctv/readme/man/figures/logo_alpha.png" align="right" alt="ctv logo" width="100" />

# CRAN Task Views

## Overview

CRAN task views aim to provide some guidance which packages on the [Comprehensive
R Archive Network (CRAN)](https://CRAN.R-project.org/) are relevant for tasks
related to a certain topic. They give a brief overview of the included packages
which can also be automatically installed using the
[ctv](https://CRAN.R-project.org/package=ctv) package. The views are intended to
have a sharp focus so that it is sufficiently clear which packages should be
included (or excluded) - and they are not meant to endorse the "best" packages
for a given task.

- [List of available CRAN Task Views](https://CRAN.R-project.org/web/views/)
- [GitHub repository of the CRAN Task View Initiative](https://github.com/cran-task-views/ctv/)
- Introduction: Zeileis A, Bivand R, Eddelbuettel D, Hornik K, Piaskowski J, Vialaneix N (2025).
  "The CRAN Task View Initiative." _The R Journal_, **17**(2), 4-14.
  [doi:10.32614/RJ-2025-011](https://doi.org/10.32614/RJ-2025-011).


## Installation

To automatically install the views, the [ctv](https://CRAN.R-project.org/package=ctv)
package needs to be installed. The stable version of is available on CRAN:

``` r
install.packages("ctv")
```

The latest development version can be installed from
[R-universe](https://zeileis.R-universe.dev/ctv):

``` r
install.packages("ctv", repos = "https://zeileis.R-universe.dev")
```

When `ctv` is available, the task views can be installed via `install.views()` or `update.views()`
(where the latter only installs those packages which are not installed and up-to-date),
e.g.,

``` r
ctv::install.views("Econometrics")
ctv::update.views("Econometrics")
```

To query information about a particular task view on CRAN from within R or to
obtain the list of all task views available, respectively, the following
commands are provided:

``` r
ctv::ctv("Econometrics")
ctv::available.views()
```

## Contributions

The task views as a whole are overseen and coordinated by the _CRAN Task View
Editors_ and each individual task view is maintained by a group of volunteers.
See the repository of the
[CRAN Task View Initiative](https://github.com/cran-task-views/ctv/)
for details on how to contribute to an existing task view or propose a new one.
