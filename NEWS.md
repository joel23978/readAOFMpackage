# readAOFM 0.1.1

- Replace retired `/media/{id}` downloads with the current workbook URLs
  published by the AOFM Data Hub for all 23 supported tables.
- Support Notes-first workbooks and POSIX-second date headers used by current
  AOFM files.
- Parse the new security, region, and counterparty turnover layouts and join
  them to the 2016–2025 historical turnover workbooks in `read_aofm()`.
- Stitch turnover sources by `period`, `group`, and `name`, with deterministic
  current-source precedence for revised overlaps, duplicate-key rejection, and
  SHA-256 provenance for both source workbooks.
