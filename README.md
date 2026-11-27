# Türkiye Macroeconomic Dashboard

[**Open the dashboard**](https://batuhan-barlas.github.io/turkey-macro-dashboard/)

## Overview

The Türkiye Macroeconomic Dashboard provides a centralized, interactive platform for monitoring the Turkish economy. Its objective is to facilitate economic analysis by bringing together official macroeconomic, banking and financial indicators in a consistent, regularly updated interface.

The dashboard is designed to minimize manual data collection, support comparisons across indicators and provide a transparent, scalable framework for incorporating new economic series.

## Dashboard structure

| Section | Coverage |
| --- | --- |
| Activity | Real GDP growth and expenditure- and production-side growth contributions. |
| Inflation | Headline and core inflation, goods and services inflation, and underlying inflation trends. |
| External Trade | Exports, imports and trade balance. |
| Balance of Payments | Current account, financial account, reserve transactions and net errors and omissions. |
| Banking & Financial Markets | Loan and deposit rates, interest rate spreads, central bank funding and overnight repo rates. |
| Blackboard | Custom visualization and comparison of available economic indicators. |
| Sources & Method | Data sources, calculation methodology and a dynamic series catalogue. |

## Sources and methodology

Data are obtained from the [Central Bank of the Republic of Türkiye (CBRT), Electronic Data Delivery System (EVDS)](https://evds3.tcmb.gov.tr/) and the [Turkish Statistical Institute (TurkStat)](https://data.tuik.gov.tr/). Data retrieval from EVDS uses a securely stored API key; dashboard users do not need one.

- **Activity:** Real GDP growth is calculated from chain-linked volume indices. Quarterly growth uses seasonally and calendar-adjusted data. GDP growth contributions are estimated using same-quarter prior-year nominal GDP weights and real year-on-year growth rates, with residuals reconciling the contributions to aggregate growth.
- **Inflation:** Annual inflation is calculated from 2025-base CPI indices. Trend inflation is the trailing three-month arithmetic average of seasonally adjusted monthly inflation rates, without annualization.
- **External Trade and Balance of Payments:** Monthly flows and trailing 12-month sums are expressed in USD billions using the pipeline's source-unit conversions. Balance of payments indicators follow the dashboard's stated accounting sign conventions.
- **Banking & Financial Markets:** Weekly loan and deposit flow rates and daily funding and repo rates retain their original frequencies. Loan-to-deposit rate spreads are calculated as relative differences rather than simple percentage-point gaps.

Official data may be revised, and missing observations are not replaced with zero. GDP growth contributions are estimates rather than officially published contributions. Detailed definitions, source codes, treatments, unit conversion assumptions and latest observations are available in the **Sources & Method** tab.

## Data updates and architecture

The dashboard uses an R-based data pipeline and a lightweight static web interface hosted on GitHub Pages. GitHub Actions retrieves official data, applies the required transformations, validates the resulting dataset and deploys the updated dashboard.

Updates are scheduled every day at approximately **09:15 and 17:15 Türkiye time (UTC+3)**. The workflow can also be triggered manually. The **Refresh data** button reloads the latest published dataset, and the interface checks for updates every 15 minutes; neither operation initiates a new download from the original sources.

The separation of data retrieval, statistical transformations and visualization allows new indicators to be added without redesigning the dashboard. A failed update does not replace the last successfully deployed version.
