# See College Scorecard source here: https://collegescorecard.ed.gov/data/
# R library to download data: https://github.com/btskinner/rscorecard
# Sign up for API key: https://api.data.gov/signup/


library(tidyverse)
library(rscorecard)


# -----------------------
# Search data dictionary
# -----------------------

# Search keyword
sc_dict('debt')

# Search by source
sc_dict('NSLDS', search_col = 'source')

# Save dictionary as dataframe
sc_dict('30,000')
lo_inc_vars <- sc_dict('30,000', limit = Inf, return_df = T)


# ----------------
# Fetch variables
# ----------------

# Store API key
sc_key('SkePia32HkA5G7rxnh721Awf0BwP562763QVr2tS')

sc_dict("debt")

sc <- sc_init() %>% 
  sc_select(
    unitid,
    md_earn_wne_inc1_p10, mn_earn_wne_p10, mn_earn_wne_inc1_p6, mn_earn_wne_p6, md_earn_wne_inc1_p8, md_earn_wne_p8, lo_inc_debt_mdn, debt_mdn, lo_inc_debt_n, debt_n,
    hbcu, pbi, annhi, tribal, aanapii, hsi, nanti
  ) %>%
  sc_year('latest') %>%
  sc_get()

# Add MSI variable
sc <- sc %>% 
  mutate(across(hbcu:nanti, ~if_else(.x == 1, cur_column(), NA_character_), .names = '{.col}_text')) %>%
  unite(msi, hbcu_text:nanti_text, sep = ';', remove = T, na.rm = T) %>% 
  relocate(year, .after = last_col())

# ------------------
# Edit column names
# ------------------

# Get column names from data dictionary
#names(sc)

#Map Variable names to the Data Dictionary Descriptions (they are LONG)

#sc_desc <- plyr::mapvalues(names(sc), lo_inc_vars$varname, lo_inc_vars$description, warn_missing = F)
#sc_source <- plyr::mapvalues(names(sc), lo_inc_vars$varname, lo_inc_vars$source, warn_missing = F)

#sc_names <- str_c(sc_desc, ' (', sc_source, ')')
#names(sc) <- c('UnitID', sc_names[-1])

sc <- sc %>%
  rename(
    "10 Year Median Earnings (0-30,000)" = "md_earn_wne_inc1_p10",
    "10 Year Median Earnings" = "mn_earn_wne_p10",
    "6 Year Mean Earnings (0-30,000)" = "mn_earn_wne_inc1_p6",
    "6 Year Mean Earnings" = "mn_earn_wne_p6",
    "8 Year Median Earnings (0-30,000)" = "md_earn_wne_inc1_p8",
    "8 Year Median Earnings" = "md_earn_wne_p8",
    "Median Student Loan Debt (0-30,000)" = "lo_inc_debt_mdn",
    "Median Student Loan Debt" = "debt_mdn",	 
    "Number of Students in the Median Debt, Low-Income (0-30,000) Student Cohort" = "lo_inc_debt_n",
    "Number of Students in the Median Debt, Student Cohort" = "debt_n",
    "Historically Black College or University" = "hbcu", 
    "Predominately Black Institution" = "pbi", 
    "Asian American and Native American Pacific Islander-Serving Institution" = "annhi", 
    "Tribal College or University" = "tribal", 
    "Asian American and Native American Pacific Islander-Serving Institutions" = "aanapii", 
    "Hispanic Serving Institution" = "hsi", 
    "Native American Non-Tribal Institution" = "nanti"
  )


# ----------
# Save data
# ----------

write_csv(sc, file = 'scorecard.csv')
