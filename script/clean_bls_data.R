library(rvest)
library(janitor)
library(here)
library(tidyverse)
library(data.table)

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

bls_st_tbl <- read_csv(here("data", "bls_st_codes.csv"))


bls_st_tbl <- clean_names(bls_st_tbl) %>% 
              filter(str_detect(u_s_total, " Statewide") & !
                     str_detect(u_s_total, " Not Statewide ")) %>% 
              mutate(state_name = str_remove(u_s_total, " -- Statewide"))

states <- as.data.frame(cbind(state_fix = toupper(state.name), 
                              state_abb_fix = state.abb))


## This guys here if something messed up. Download from query - It's quicker... 
bls_st_industry <- read.csv(here("data", "qecw_data_pull_7.19.24"), 
                            colClasses = c(area_fips = "character")) %>% 
    select(-state, -u_s_total.x, -u_s_total.y, -state_name)


## Check if we have all states (staes from osha_injury script)
states$state_fix <- snakecase::to_mixed_case(states$state_fix)
st_diff <- setdiff(bls_st_tbl$state_name, states$state_fix) 

## Diff is due to snake case upper casing two word states. Everything is there.
print(st_diff)

## ----------------------------------------------------------------------------
# 2. Use BLS provided query ----
## ----------------------------------------------------------------------------

## Get codes for states of interest
# filter(bls_st_tbl, state_name  %in% c("New Jersey", "Texas", 
#                                       "Florida", "Pennsylvania"))
# 
# ## Load vectors to help iterate data query
# years <- c("2015", "2016", "2017", "2018", 
#            "2019", "2020", "2021", "2022")
# 
# qtr <- c("1", "2", "3", "4")
# 
# st_codes <- unique(bls_st_tbl$us000)
# 
# qcewGetAreaData <- function(year, qtr, area) {
#   url <- "http://data.bls.gov/cew/data/api/YEAR/QTR/area/AREA.csv"
#   url <- sub("YEAR", year, url, ignore.case=FALSE)
#   url <- sub("QTR", tolower(qtr), url, ignore.case=FALSE)
#   url <- sub("AREA", toupper(area), url, ignore.case=FALSE)
#   read.csv(url, header = TRUE, sep = ",", quote="\"", dec=".", na.strings=" ", skip=0)
# }
# 
# ## All possible combinations of quarter, state, year, that we can use in pmap with qcew function
# query_combos <- expand.grid(year = years,
#                             qtr = qtr,
#                             area = st_codes)
# 
# 
# ## Create a list of state, year, quarter combos. Each list element represents: individual state, year, quarter 
# bls_st_list <- pmap(query_combos, qcewGetAreaData)



# ----------------------------------------------------------------------------
## 3. Harmonize with OSHA data -----
# ----------------------------------------------------------------------------
bls_industry_filter <- c("11", "23", "21", "22", "48-49",
                         "31-33", "72")


bls_st_industry <- bls_st_industry %>% 
                   filter(industry_code %in% bls_industry_filter) %>% 
                   mutate(industry_name = 
                           case_when(
                                     industry_code == "11" ~ "Agriculture, Forestry, Fishing & Hunting",
                                     industry_code == "23" ~ "Construction",
                                     industry_code == "21" ~ "Mining, Quarrying, & Oil and Gas Extraction",
                                     industry_code == "22" ~ "Utilities",
                                     industry_code == "72" ~ "Accomodation & Food Services",
                                     industry_code == "48-49" ~ "Transportation & Warehousing",
                                     industry_code == "31-33" ~ "Manufacturing",
                                     TRUE ~ "YOU MESSED UP"),
                           total_employment = (month1_emplvl + month2_emplvl + month3_emplvl),
                           area_fips = as.character(area_fips)
                    )

## Correcting bls_codes - They're off from bls_st_tbl on import

bls_st_industry <- bls_st_industry %>% 
                   mutate(string_fixer = nchar(area_fips),
                          corrected_code = if_else(string_fixer == 4,
                                                   paste("0", area_fips, sep = ""),
                                                   area_fips)) %>%
             
                  select(-string_fixer, -area_fips) %>% 
                  rename(area_fips = corrected_code)


bls_st_industry <- bls_st_industry %>% 
                   left_join(bls_st_tbl, by = c("area_fips" = "us000"))


bls_st_industry <- rename(bls_st_industry, state = state_name)
bls_st_industry <- as.data.table(bls_st_industry)

#write_csv(bls_st_industry, here("data", "qecw_data_pull_7.19.24.csv"))
# ------------------------------------------------------------------------------
## 4. Derive Industry employment numbers ----
# ------------------------------------------------------------------------------
get_dt_sums <- function(grp, denom){
  
  dat <- bls_st_industry[, .(total_emp = sum(total_employment)),
                  by =  c(grp)][
                    , `:=`(all_emp_tot = sum(total_emp), 
                           pct_emp = (total_emp / sum(total_emp)) * 100), by = c(denom)]
  
  return(dat)
}

employ_tbls <- list(all_emp = bls_st_industry[, .(total_emp = sum(total_employment)),
                                              by = industry_name][, `:=`(all_emp_tot = sum(total_emp), 
                                                                         pct_emp = ( total_emp/ sum(total_emp)) * 100)],
                 
                 ann_emp = get_dt_sums(c("year", "industry_name"), c("year")),
                 st_emp = get_dt_sums(c("state", "industry_name"), c("state")),
                 state_yr_emp = get_dt_sums(c("state", "year", "industry_name"),
                                              c("state", "year"))
                 
)

# ----------------------------------------------------------------------------
## 5. Write total employment tables ----
# ----------------------------------------------------------------------------

csv_names <- c("industry_emp", "annual_industry_emp", 
               "st_emp", "state_yr_emp")

map2(employ_tbls, csv_names, 
     ~write_csv(.x, here("summary_data",
                          paste(.y, ".csv", sep = "")
                         )
               )
     )


