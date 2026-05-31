# Restoring Fish Passage in the Fraser Region — 2023

> Reproducible, web-first fish-passage restoration-planning report for the Fraser Region, prepared on behalf of the Skeena Environmental Restoration Network (SERN BC).

**Read the report:** <https://www.newgraphenvironment.com/fish_passage_fraser_2023_reporting/>
&middot; **Source:** [`NewGraphEnvironment/fish_passage_fraser_2023_reporting`](https://github.com/NewGraphEnvironment/fish_passage_fraser_2023_reporting)
&middot; **Version history:** [`NEWS.md`](NEWS.md)

## What this is

The 2023 iteration of the Fraser Region fish-passage restoration-planning report. Assembles crossing-by-crossing assessments, fish-presence and habitat-confirmation data, and prioritization analyses across the region. Each prioritized barrier has a dedicated appendix with photos, field measurements, and recommended remediation; the executive summary rolls them up for partners and decision-makers. Source data and methods are open — anyone can rebuild the report from `scripts/run.R`.

The 2025 iteration carries this work forward — see [`fish_passage_fraser_2025_reporting`](https://github.com/NewGraphEnvironment/fish_passage_fraser_2025_reporting).

## Sections

- **Executive Summary** ([`0050-executive-summary.Rmd`](0050-executive-summary.Rmd))
- **Introduction** ([`0100-intro.Rmd`](0100-intro.Rmd))
- **Background** ([`0200-background.Rmd`](0200-background.Rmd))
- **Methods** ([`0300-methods.Rmd`](0300-methods.Rmd))
- **Results** ([`0400-results.Rmd`](0400-results.Rmd))
- **Recommendations** ([`0500-recommendations.Rmd`](0500-recommendations.Rmd))
- **Appendices** ([`0600-appendix.Rmd`](0600-appendix.Rmd) + per-site `0800-appendix-*.Rmd` files) — site-specific data for each prioritized crossing.

## Build

```r
source("scripts/run.R")
```

## License

MIT (see [`LICENSE`](LICENSE)). Indigenous traditional knowledge shared with this work remains the cultural and intellectual property of the contributing Nations; sharing or republishing those contributions requires explicit consent.
