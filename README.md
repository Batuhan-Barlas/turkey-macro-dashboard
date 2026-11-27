# Türkiye Macro Monitor — experimental starter

**Review before uploading.** This starter adapts the supplied multicountry `template.html` and the selected logic from `macro_db_data.R`, `macro_db_plots.R` and `shiny_app_v2.R`. It is not a production deployment. Confirm permission to reuse the original template and redistribute all source data on GitHub Pages. No original historical snapshot, private paths, keys, logos, reserves/swap/global scripts or sample economic observations are included. The original template retains its unused Spain-specific code and styles; only the Turkey sections are exposed. English defaults to on.

## Local trial (RStudio)

1. Extract this ZIP into a new local project folder. Set RStudio's working directory to this folder (`getwd()`).
2. Install `install.packages(c("CBRT", "jsonlite", "zoo", "R6", "httr", "readxl", "openxlsx"))`. CBRT >= 0.2.0 is required for EVDS3.
3. Set a valid, rotated key in your local `.Renviron` (ignored by Git): `EVDS_API_KEY=YOUR_KEY`. Restart RStudio. Do not share the key or put it in the source.
4. Run `source("R/update_data.R", encoding="UTF-8")`. This produces `site/drive_ingestion/snapshots/turkey.json`. Compare against Shiny output before publishing. If the CBRT library or an endpoint fails, the pipeline stops and does not replace the previous snapshot. A missing optional input produces a message and no corresponding lines.
5. Serve the static folder over HTTP: from the project directory run `python -m http.server 8000 --directory site` in a terminal (only a local server, no Python economic logic). Browse to `http://localhost:8000/`. Alternatively use any local static HTTP server. Opening the HTML via file:// will not load the snapshot.

## First GitHub deployment (only after approval)

- Upload the *contents* of the starter folder (including hidden `.github` and `.gitignore`) to your new repository, not the ZIP or the enclosing folder. GitHub Desktop is recommended for committing hidden folders correctly. Do not copy the original Shiny ZIP, original snapshot or secrets. Review the diff before committing. If any source/data cannot be published publicly, keep development local or use an approved private host.
- GitHub repository Settings > Secrets and variables > Actions > New repository secret: name `EVDS_API_KEY`. Use a rotated valid key.
- Settings > Pages > Build and deployment > Source: GitHub Actions.
- Actions > Update Turkey data and deploy > Run workflow on main. The first successful run will publish the `site` folder only, at `https://batuhan-barlas.github.io/turkey-macro-dashboard/` (assuming the specified owner and repo).
- Scheduled runs at 06:15 and 14:15 UTC (09:15 and 17:15 Türkiye time), plus manual workflow dispatch. No automatic run on push, intentional to allow review. Scheduled Actions can be delayed or disabled by GitHub policies. Failure before deployment leaves the previous published site in place. The website Refresh button reloads the published snapshot; it does not trigger R.
- This workflow uses artifact deployment, not commits of historical snapshots. Data is regenerated on each run; the last successful deployment is retained by Pages. Private repositories need an eligible Pages plan; public Pages outputs are public, even if source visibility is restricted. Do not deploy until authorized.

## Migration map

- Loan/deposit rates: six EVDS flow rates, two relative spreads, two daily funding rates. Weekly data is kept weekly rather than relying on mixed frequency defaults; two corridor lines await approved inputs.
- CPI: five annual inflation series based on the new 2025-base EVDS codes from Shiny. Sort dates and complete the monthly calendar before computing 12-month lags. Trend inflation is trailing three-month arithmetic average of seasonally adjusted monthly changes (not annualized). The supplied extractor is included, with platform-specific download selection. The CPI adapter follows the original Shiny spreadsheet layout; verify logged selected headers and compare values before deployment.
- Balance of payments: four monthly components and their 12-month sums. Current account, financial account and reserve transaction signs inverted exactly as in Shiny. Reserve **transactions** are included because they are a BOP component; no standalone CBRT reserves section.
- External trade: exports, imports, balance and trailing 12-month sums. Verify source units before release: original plot divided trade by 1e6 and BOP by 1e3 to display billions. Missing monthly periods are not silently replaced with zero.
- The original SVG chart engine, palette, typography, date range selector, responsive cards, refresh and Blackboard are retained. The output uses the original Turkey snapshot contract (`origen`, `series`, `observaciones`, Spanish metadata fields). Four new blocks and weekly (`Semanal`/`W`) support need to be ported when unifying dashboards. Original 55-series workbook snapshot is not shipped. To merge later, agree on permission, ownership and ID namespace; avoid overwriting existing IDs.

## Optional inputs (local only in v0.2)

Create `inputs/rate_bands.csv` with header `date,LOWER_BAND,UPPER_BAND`, effective dates ISO YYYY-MM-DD, rates in percent. Values are carried forward until the next effective date. Verify the source and current coverage. The original CBRT HTML table scraper has not been validated and is not used yet.

Create `inputs/cpi_sa.csv` with header `date,CPI,Core_B,Core_C,Goods,Services`, dates ISO, official seasonally adjusted **index levels** (not growth rates). This optional local override bypasses live TurkStat retrieval; otherwise the pipeline downloads the workbook automatically. Inputs are ignored by Git. Live TurkStat failure stops deployment. No fake data fills these gaps.

## Validation status

Frontend syntax and snapshot schema can be checked with Node and Python. Actual EVDS download, live source unit/frequency verification, and R execution require R plus a valid key; they are not certified by this starter. Compare overlapping observations and dates to Shiny; confirm the weekly flow frequency, daily funding dates and CPI series coverage. Verify BOP accounting signs and trade/BOP units against metadata. Additional package lockfile and source checks should be added after the initial working run.

## v0.2 changes

Added R/tuik_extractor.R and R/fetch_tuik_cpi.R. Windows retains wininet; Linux uses libcurl. The original positional series mapping is preserved, with date and numeric checks and selected headers logged. Actual workbook selection, headings and values must be verified locally and on GitHub; R is not available in the preparation environment. The corridor remains optional. The validator now requires all five trend series.

### National accounts (Activity)

`R/national_accounts.R` adds quarterly real GDP YoY, SCA QoQ and two
estimated contribution decompositions. It uses the existing EVDS key and
schedule. The full history is re-fetched from 2009Q1 (YoY begins no earlier
than 2010Q1). Contributions use same-quarter prior-year nominal GDP shares
and real YoY rates; imports are subtracted and services sector contributions
are summed. Residuals include omitted components (inventories/NPISH on the
demand side, net product taxes on supply) and chain-linking differences.
These are estimates, not official published contributions.

**First run requires review:** catalogue matching uses CBRT's bundled metadata
and refuses missing or ambiguous matches. It is not live-API verified.
Run `Rscript R/list_gdp_catalogue.R` with CBRT installed to export the catalogue.
If matching fails, create `inputs/gdp_series.csv` with columns `group,item,code`
using verified EVDS codes. Required groups/items:

- `demand_real`, `demand_nominal`: GDP,C,I,G,X,M
- `gdp_sca`: GDP
- `supply_real`, `supply_nominal`: A,IND,F,GI,J,K,L,MN,OQ,RU

Use quarterly data, consistent nominal units and current non-archived groups.
Review the resolved mappings printed in the update log even if matching succeeds.
The new validator checks quarter dates, completeness and reconciliation. A failed
build does not deploy. The checked-in snapshot retains existing observations only
(normalized from date-keyed objects to arrays); GDP appears after a successful update.
To validate this older snapshot only, pass `--legacy` to the validator.
