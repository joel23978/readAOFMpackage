# readAOFM 0.1.0

This release prepares readAOFM for an initial CRAN submission. CRAN acceptance
remains pending.

* Documented the package's ten exported functions, including their return
  shapes, errors, network requirements, and opt-in file-writing behaviour.
* Added a task-oriented path from offline catalogue search to a selected live
  AOFM workbook and clarified the supported source families and raw-only
  catalogue entries.
* Added a deterministic end-to-end example that retrieves Treasury Bond
  issuance through the public API and charts bids and allotments from a
  provenance-documented official snapshot.
* Standardized the package author, maintainer, and copyright-holder display as
  Joel F across package and repository metadata.
* Added deterministic, fixture-backed documentation and examples for the
  workbook readers, together with troubleshooting guidance for changed source
  URLs and layouts.
* Added fixed connection, transfer, low-speed, and workbook-size bounds to live
  downloads so unavailable or oversized sources fail within defined limits;
  successful parser outputs and public function signatures are unchanged.
* Updated package metadata, licensing attribution, community guidance, and
  release checks for portable R-package builds, including multi-platform
  R CMD check, coverage, and pkgdown workflows.
