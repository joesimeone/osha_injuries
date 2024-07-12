library(rvest)
library(janitor)


## ----------------------------------------------------------------------------
# 1. Import html w/ state codes for queries ----
## ---------------------------------------------------------------------------
# bls_st_codes <- read_html("https://data.bls.gov/cew/doc/titles/area/area_titles.htm")
# 
# ## Target html table
# bls_st_tbl <- bls_st_codes %>% 
#               html_node("table") %>% 
#               html_table(head = TRUE)
# 
# ## This is just here so I didn't ping the website twice. Next time code out.
# bls_st_tbl <- test

# write_csv(bls_st_tbl, here("data", "bls_st_codes.csv"))

bls_st_tbl <- here("data", "bls_st_codes.csv")


bls_st_tbl <- clean_names(bls_st_tbl) %>% 
              filter(bls_st_tbl, str_detect(u_s_total, " Statewide") & !
                     str_detect(u_s_total, " Not Statewide ")) %>% 
              mutate(state_name = str_remove(u_s_total, " -- Statewide"))


## Check if we have all states (staes from osha_injury script)
states$state_fix <- snakecase::to_mixed_case(states$state_fix)
hmm <- setdiff(bls_st_tbl$state_name, states$state_fix) 

## Diff is due to snake case upper casing two word states. Everything is there.

## ----------------------------------------------------------------------------
# 2. Use BLS provided query ----
## ----------------------------------------------------------------------------

years <- c("2015", "2016", "2017", "2018", 
           "2019", "2020", "2021", "2022")

qtr <- c("1", "2", "3", "4")

tsting <- c("29000", "42000", "48000")

qcewGetAreaData <- function(year, qtr, area) {
  url <- "http://data.bls.gov/cew/data/api/YEAR/QTR/area/AREA.csv"
  url <- sub("YEAR", year, url, ignore.case=FALSE)
  url <- sub("QTR", tolower(qtr), url, ignore.case=FALSE)
  url <- sub("AREA", toupper(area), url, ignore.case=FALSE)
  read.csv(url, header = TRUE, sep = ",", quote="\"", dec=".", na.strings=" ", skip=0)
}

## All possible combinations of quarter, state, year, that we can use in pmap with qcew function
query_combos <- expand.grid(year = years,
                            qtr = qtr,
                            area = tsting)


## Create a list of state, year, quarter combos. Each list element represents: individual state, year, quarter 
bls_st_list <- pmap(query_combos, qcewGetAreaData)


# ----------------------------------------------------------------------------
## Harmonize with OSHA data -----
# ----------------------------------------------------------------------------
bls_st_list <- pls
bls_industry_filter <- c("11", "23", "21", "22", "48-49",
                         "31-33", "72")


bls_st_industry <- list_rbind(bls_st_list) %>% 
                   filter(industry_code %in% bls_industry_filter)


bls_st_industry <- bls_st_industry %>%  mutate(industry_name = 
                           case_when(
                               industry_code == "11" ~ "Agriculture, Forestry, Fishing & Hunting",
                               industry_code == "23" ~ "Construction",
                               industry_code == "21" ~ "Mining, Quarrying, & Oil and Gas Extraction",
                               industry_code == "22" ~ "Utilities",
                               industry_code == "72" ~ "Accomodation & Food Services",
                               industry_code == "48-49" ~ "Transportation & Warehousing",
                               industry_code == "31-33" ~ "Manufacturing",
                               TRUE ~ "YOU MESSED UP"),
                           total_employment = (month1_emplvl + month2_emplvl + month3_emplvl)
                    )
                    
  
a <- select(bls_st_industry, area_fips, industry_name, industry_code, year, qtr, month1_emplvl, month2_emplvl, month3_emplvl, total_employment)
 
