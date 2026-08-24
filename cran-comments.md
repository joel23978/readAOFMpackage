## Release status

This is the initial submission of readAOFM 0.1.0. The package has not yet
been submitted to or accepted by CRAN.

## Test environment

* Local: macOS Sequoia 15.6 (Apple Silicon), R 4.5.1, UTF-8; Apple clang
  17.0.0 and GNU Fortran 15.1.0.

The source tarball was built with `R CMD build`, installed into a clean
isolated library, and checked with `R CMD check --as-cran`.

## R CMD check results

* 0 errors | 0 warnings | 2 notes
* 169 test expectations passed with no failures, warnings, or skips.
* All examples, three vignettes and their rebuild, and the PDF and HTML
  manuals completed successfully.

The notes are:

* `New submission`
* `unable to verify current time`

The `New submission` note is expected because readAOFM has not previously
been published on CRAN. The clock-verification note is specific to the local
check host's external time-service probe; no future-dated package file was
reported. A separate complete check in the same environment verified the
clock and reported only `New submission`.

Win-builder and R-hub were not run for this preparation pass. Multi-platform
GitHub Actions checks are configured separately; no result is claimed here
until those remote jobs have completed.
