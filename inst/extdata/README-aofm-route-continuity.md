# AOFM route continuity audit

`aofm-route-continuity-2026-07-27.csv` records a live comparison of the 23
supported readAOFM routes against the workbooks downloaded by package commit
`8d9cdffa0cf5fe94aa363afe15e15ff1f9dad5b9`.
`aofm-live-source-urls-2026-07-27.csv` records the exact active and historical
AOFM Data Hub URLs used in the audit.

The audit parses each workbook through the package parser and compares its
primary observation dates and natural identities. Natural identities use all
published hierarchy fields for position and ownership data; stable transaction
identifiers plus measure names for transactional data; `period`, `group`, and
`name` for turnover; and `date`, `type`, and `name` for term-premium data.
Whitespace in stable identifiers and hierarchy labels is normalized before
comparison.

The active turnover workbooks contain the redesigned 2026 observations only.
The AOFM Data Hub separately publishes historical workbooks covering July 2016
through December 2025. Those are the only routes that require source stitching.
The combined turnover series has no missing month at the December 2025 /
January 2026 boundary.

The current full-history workbooks intentionally withdraw a small number of
superseded observations: the executive summary's provisional 30 April 2025
snapshot and invalid derived duration/tenor cells in several dealt-basis
position files. These are current-source revisions, not truncated history, and
are not reintroduced from retired workbooks.

Exact repeated rows in TIB issuance, retail buyback, and Securities Lending
Facility data are present in the AOFM source itself. They are retained because
the public schema does not expose a transaction identifier that would prove
the records are accidental duplicates.
