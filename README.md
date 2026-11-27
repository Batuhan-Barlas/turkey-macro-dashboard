# Türkiye Macro Monitor

A dashboard for monitoring the Turkish economy through activity, inflation, external trade, balance of payments, and banking and financial market indicators. The aim is to present consistent, interpretable time series from official sources in a single interface.

## Sources and methodology

Data are retrieved from [CBRT EVDS](https://evds3.tcmb.gov.tr/), [TurkStat](https://data.tuik.gov.tr/) and the [CBRT official interest rate decisions table](https://www.tcmb.gov.tr/wps/wcm/connect/TR/TCMB+TR/Main+Menu/Temel+Faaliyetler/Para+Politikasi/Merkez+Bankasi+Faiz+Oranlari/faiz-oranlari).

Series retain their native frequency where appropriate. Annual inflation uses 12-month index changes; trend inflation is a trailing three-month average of seasonally adjusted monthly changes, not annualised. GDP contributions are estimates using prior-year same-quarter nominal shares and real growth rates, with residuals reconciling to headline growth. External trade and balance of payments include trailing 12-month totals. Overnight borrowing and lending rates are carried forward daily from their effective dates. Further details are provided in the dashboard's Sources & Method section.

## Data updates

R retrieves and transforms source data. GitHub Actions rebuilds and validates the snapshot twice daily (approximately 09:15 and 17:15 Türkiye time) and publishes successful builds through GitHub Pages. Failed builds leave the last successful deployment available. The dashboard refresh control reloads the published snapshot rather than triggering source downloads.

[Open dashboard](https://batuhan-barlas.github.io/turkey-macro-dashboard/)
