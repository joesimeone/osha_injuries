library(tidyverse)
library(here)
library(data.table)
library(janitor)
library(readxl)



# 11 Agriculture, Forestry, Fishing and Hunting
# 23 construction
# 21 Mining, Quarrying, and Oil and Gas Extraction
# 22 Utilities
# 48, 49 Transportation and Warehousing
# 31 - 33 Manufacturing
# 72 Accommodation and Food Services
## Load states in df to minimize recoding


# Import data -------------------------------------------------------------

## Injury data
osha_inj <- fread(here("data", "OSHA_severeinjurydata.csv")) %>% clean_names()

## employment data
files <- list.files(here("summary_data"), pattern = "_emp", full.names = TRUE)



emp_tbl_names <- c("ann_emp", "all_emp", "st_emp", "st_yr_emp")



bls_employ <- map(files, ~read_csv(.x) %>% clean_names())
bls_employ <- map(bls_employ, as.data.table)

names(bls_employ) <- emp_tbl_names

## Reorder list to match up w/ injury_tbls

bls_employ <- list(all_emp = bls_employ$all_emp, 
                   ann_emp = bls_employ$ann_emp,
                   st_emp = bls_employ$st_emp, 
                   st_yr_emp = bls_employ$st_yr_emp)

names(bls_employ) <- emp_tbl_names


# Vectors for cleaning ----------------------------------------------------

## To format states
states <- as.data.frame(cbind(state_fix = toupper(state.name), 
                              state_abb_fix = state.abb))

## To filter industries 
industry_filter <- c("11", "23", "21", "22", "48", "49",
                     "31", "32", "33", "72")

## To select bls vars
bls_vars <- c("st", "st_name", "year", 
              "naics", "industry", "annual_average_employment")

# Clean osha data ---------------------------------------------------------
states <- as.data.frame(cbind(state_fix = toupper(state.name), 
                              state_abb_fix = state.abb))



## Join states w/ og data to create third variable. Recode leftover abbreviations
osha_inj <- osha_inj %>% 
            left_join(states, by = c("state" = "state_abb_fix")) %>% 
            mutate(state_fix = ifelse(is.na(state_fix),
                                      state, state_fix),
            state_fix = case_when(state_fix == "DC" ~ "DISTRICT OF COLUMBIA",
                                  state_fix == "AS" ~ "AMERICAN SAMOA",
                                  state_fix == "GU" ~ "GUAM",
                                  state_fix == "MP" ~ "NORTHERN MARIANA ISLANDS",
                                  state_fix == "PR" ~ "PUERTO RICO",
                                  state_fix == "VI" ~ "VIRGIN ISLANDS",
                                  TRUE ~ state_fix)
      )

## Join data by cleaned state variable. Fix leftover missings
osha_inj <- osha_inj %>% 
            left_join(states, by = c("state_fix")) %>% 
            mutate(state_abb_fix = case_when(
                          state == "DC" | state == "DISTRICT OF COLUMBIA" ~ "DC",
                          state == "AS" | state == "AMERICAN SAMOA" ~ "AS",
                          state == "GU" | state == "GUAM" ~ "GU",
                          state == "MP" | state =="NORHTERN MARIANA ISLANDS" ~                                                                                          "MP",
                          state == "PR" | state == "PUERTO RICO" ~ "PR",
                          state == "VI" | state == "VIRGIN ISLANDS" ~ "VI",
                          TRUE ~ state_abb_fix)
                        ) %>% 
            select(!c("state")) %>% 
            rename(state = state_fix,
                   state_abb = state_abb_fix)

## Last bits of cleaning
osha_inj <- osha_inj %>% 
            mutate(date = as.Date(event_date, format = "%m/%d/%Y"),
                   month = month(date),
                   year = year(date)) %>% 
            mutate(industry = str_extract(primary_naics, "^.{2}")) %>% 
            mutate(industry_name = 
                     case_when(
                       industry == "11" ~ "Agriculture, Forestry, Fishing & Hunting",
                       industry == "23" ~ "Construction",
                       industry == "21" ~ "Mining, Quarrying, & Oil and Gas Extraction",
                       industry == "22" ~ "Utilities",
                       industry == "72" ~ "Accomodation & Food Services",
                       industry %in% c("48", "49") ~ "Transportation & Warehousing",
                       industry %in% c("31", "32", "33") ~ "Manufacturing",
                       TRUE ~ "GETS FILTERED OUT"),
                   industry_name = factor(industry_name, levels = unique(industry_name))) 

## Ok, we're going to do most of the calculations in data.table for practice, 
## but going to do some dplyr stuff for what I don't feel like thinking hard about right now. 




# Get counts, percentages -------------------------------------------------
industry_filter <- c("11", "23", "21", "22", "48", "49",
                     "31", "32", "33", "72")

inj_counts <- osha_inj[industry %chin% industry_filter, .N, by = industry]



get_dt_counts <- function(grp, denom){
  
  dat <- osha_inj[industry %chin% industry_filter, .N,
                  keyby =  c(grp)][
                    , `:=`(total = sum(N), 
                           pct = (N / sum(N)) * 100), by = c(denom)]
  
  return(dat)
}
          
inj_tbls <- list(all_inj = osha_inj[industry %chin% industry_filter, .N,
                                    by = industry_name][ , `:=`(total = sum(N), 
                                                                pct = (N / sum(N)) * 100)],
                 
                 ann_inj = get_dt_counts(c("year", "industry_name"), c("year")),
                 st_inj = get_dt_counts(c("state", "industry_name"), c("state")),
                 state_yr_inj = get_dt_counts(c("state", "year", "industry_name"),
                                              c("state", "year"))
                 
)





# Join injury - employment tables -----------------------------------------
## Prep employment table for join ------------------------------------------
bls_employ <- map(bls_employ, 
                  ~.x %>% mutate(industry_name = factor(industry_name, 
                                                        levels = unique(industry_name))
                  ))

bls_employ$st_emp$state <-toupper(bls_employ$st_emp$state)
bls_employ$st_yr_emp$state <-toupper(bls_employ$st_yr_emp$state)



## How would you iterate this?? 

inj_emp_tbls <- list( 
                    all_industry = inj_tbls$all_inj[bls_employ$all_emp,
                                                    on = "industry_name"],
                    ann_industry = inj_tbls$ann_inj[bls_employ$ann_emp,
                                                    on = c("industry_name", "year")],
                    st_industry = inj_tbls$st_inj[bls_employ$st_emp,
                                                  on = c("industry_name", "state")],
                    st_yr_industry = inj_tbls$state_yr_inj[bls_employ$st_yr_emp,
                                                           on = c("industry_name", "state",
                                                                  "year")]
                    )



# Write tables  -----------------------------------------------------------
tbl_names <- list(
                  "osha_all_industry_injuries.csv",
                  "osha_annual_industry_injuries.csv",
                  "osha_state_industry_injuries.csv",
                  "osha_state_annual_injuries.csv")


for (i in 1:length(inj_tbls)){
  
  fwrite(inj_tbls[[i]], here("summary_data", tbl_names[[i]]))}
                  
