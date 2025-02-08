# OSHA Severe Injury Data Exploration

**Purpose:** Use [data.table](https://cran.r-project.org/web/packages/data.table/index.html) to explore how select industries are impacted by severe injuries over time within the United States. Here, I produced basic rates of injury, by indutry from 2015 - 2022. 

## Industries Explored

Here's your data formatted as a markdown table with a title:

# Table 1 - Codes Used in Analysis

| NAICS Code | Industry Name |
|------------|---------------|
| 21 | Mining, Quarrying, & Oil and Gas Extraction |
| 22 | Utilities |
| 23 | Construction |
| 48-49 | Transportation & Warehousing |
| 72 | Accommodation & Food Services |

# Data Sources

1. [OSHA Severe Injury Data](https://www.osha.gov/severeinjury)
2. [BLS Quarterly Census of Economic Wages](https://www.bls.gov/cew/)

Small enough that I just stuck it into the repo

# Script Flow 

1. **clean_bls_data.R**: Script to filter and clean QCEW data for our selected industries ---> Send off to 'clean_data' folder.
2. **get_osha_injury_counts.R**: Produces counts of injury with OSHA data and combines with QCEW industry estimates to produce rates. Sends off to 'clean_data' folder.
3. **reports/osha_injury_report.qmd**: Code use to produce visuals and quarto doc.

# Packages Used

1.  Wickham H (2022). _rvest: Easily Harvest (Scrape) Web Pages_. R package version 1.0.3, <https://CRAN.R-project.org/package=rvest>.
2. Firke S (2023). _janitor: Simple Tools for Examining and Cleaning Dirty Data_. R package version 2.2.0,
  <https://CRAN.R-project.org/package=janitor>.
3. Wickham H, Averick M, Bryan J, Chang W, McGowan LD, François R, Grolemund G, Hayes A, Henry L, Hester J, Kuhn M, Pedersen TL, Miller
  E, Bache SM, Müller K, Ooms J, Robinson D, Seidel DP, Spinu V, Takahashi K, Vaughan D, Wilke C, Woo K, Yutani H (2019). “Welcome to
  the tidyverse.” _Journal of Open Source Software_, *4*(43), 1686. doi:10.21105/joss.01686 <https://doi.org/10.21105/joss.01686>.
4. Barrett T, Dowle M, Srinivasan A, Gorecki J, Chirico M, Hocking T (2024). _data.table: Extension of `data.frame`_. R package version
  1.15.4, <https://CRAN.R-project.org/package=data.table>.
5.  Müller K (2020). _here: A Simpler Way to Find Your Files_. R package version 1.0.1, <https://CRAN.R-project.org/package=here>.
6.   Wickham H, Bryan J (2023). _readxl: Read Excel Files_. R package version 1.4.3, <https://CRAN.R-project.org/package=readxl>.
7.   Allaire J, Dervieux C (2024). _quarto: R Interface to 'Quarto' Markdown Publishing System_. R package version 1.4.4,
  <https://CRAN.R-project.org/package=quarto>.
