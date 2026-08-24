## Release status

This is the initial submission of readAOFM 0.1.0. The package has not yet
been submitted to or accepted by CRAN.

Maintainer: Joel F <joel.findlay@gmail.com>

## Test environment

* Local: macOS Sequoia 15.6 (Apple Silicon), R 4.5.1, UTF-8; Apple clang
  17.0.0 and GNU Fortran 15.1.0.

The source tarball was built with `R CMD build`, installed into a clean
isolated library, and checked with `R CMD check --as-cran`.

## R CMD check results

* 0 errors | 0 warnings | 1 note
* 169 test expectations passed with no failures, warnings, or skips.
* All examples, three vignettes and their rebuild, and the PDF and HTML
  manuals completed successfully.

The note is:

* `New submission`

The `New submission` note is expected because readAOFM has not previously
been published on CRAN.

Win-builder and R-hub were not run for this preparation pass. Multi-platform
GitHub Actions checks run on source-branch and default-branch pushes; results
are recorded in the repository's Actions history.
