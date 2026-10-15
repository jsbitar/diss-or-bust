library(tidyverse)

data_dir <- file.path('.', 'data')


# -------------
# Read in data
# -------------

# College results data
college_results <- read_delim(file.path(data_dir, 'ANALYST_VIEW_2023.CSV'), na = c('', 'NA', '\\N'), col_types = cols('UNITID' = 'c'))%>%
  select(-`Institution Name`, -`Institution Size Category`, -`Degree of Urbanization`, -`Sector of Institution`, -`City`, -`Latitude`, -`Longitude`, -`Level of Institution`, -`Control of Institution`)                                                                                   

# DROPPING MULTIPLE VARIABLES
# select(-`Institution Name`, -Address)

# IPEDS data
ipeds <- read_csv(file.path(data_dir, 'ipeds.csv'), col_types = cols('UnitID' = 'c'))

# College scorecard data
scorecard <- read_csv(file.path(data_dir, 'scorecard.csv'), col_types = cols('unitid' = 'c'))

# --------------------
# Merge and save data AND RENAMING VARIABLES
# --------------------

results <- college_results %>% 
  left_join(ipeds, by = c('UNITID' = 'UnitID')) %>% 
  left_join(scorecard, by = c('UNITID' = 'unitid')) %>% 
  mutate(
    `In-State Cost of Attendance` = rowMeans(select(., `In-State Cost of Attendance (On Campus)`, `In-State Cost of Attendance (Off Campus)`), na.rm = T),
    `In-State Cost of Attendance` = if_else(is.nan(`In-State Cost of Attendance`), NA, round(`In-State Cost of Attendance`, 0)),
    `Debt-to-Earnings Ratio` = `Median Student Loan Debt` / `10 Year Median Earnings`,
    `Debt-to-Earnings Ratio (0-30,000)` = `Median Student Loan Debt (0-30,000)` / `10 Year Median Earnings (0-30,000)`
  )

write_csv(results, file = file.path(data_dir, 'regression_results.csv'))


# -----------------
# Summarizing data
# -----------------

# # View dataframe
# View(results)
# 
# # Sort by column
# View(results %>% arrange(`Number of Bachelor Degrees Grand Total`))
# View(results %>% arrange(desc(`Number of Bachelor Degrees Grand Total`)))
# 
# # Summarize dataframe
# summary(results)
# 
# # Plot histogram for numeric variables
# hist(results$`The median debt for students with family income between $0-$30,000 (NSLDS)`)
# 
# # View counts for categorical variables
# table(results$`Institution Type`)


# --------------
# Pivoting data
# --------------

# Select number of bachelor degrees variables
# results %>% select(contains('Number of Bachelor Degrees')) %>% names() %>% writeLines()
# 
# degrees_df <- results %>% 
#   select(UNIQUE_IDENTIFICATION_NUMBER_OF_THE_INSTITUTION, `Institution Name`, matches('Number of Bachelor Degrees [^G].+ Total'))
# 
# # Regex tool: https://regex101.com/
# 
# # Remove NA rows
# degrees_df <- results %>%
#   select(UNIQUE_IDENTIFICATION_NUMBER_OF_THE_INSTITUTION, `Institution Name`, matches('Number of Bachelor Degrees [^G].+ Total')) %>% 
#   filter(!is.na(`Number of Bachelor Degrees White Total`))
# 
# # Pivot wide to long
# degrees_df <- degrees_df %>% 
#   pivot_longer(
#     cols = -c(UNIQUE_IDENTIFICATION_NUMBER_OF_THE_INSTITUTION, `Institution Name`),
#     names_to = 'race',
#     names_pattern = 'Number of Bachelor Degrees (.+) Total',
#     values_to = 'total'
#   )
# 
# # Pivot long to wide
# degrees_df %>% 
#   pivot_wider(
#     names_from = 'race',
#     names_glue = 'Number of Bachelor Degrees {race} Total',
#     values_from = 'total'
#   ) %>% 
#   View()
# 
# # More on tidy data: https://anyone-can-cook.github.io/rclass1/lectures/tidy_data/tidy_data.html
# 
# # Create bar plot
# degrees_df %>% 
#   filter(`Institution Name` == 'University of California-Los Angeles') %>% 
#   ggplot(aes(x = total, y = race)) +
#   geom_col() +
#   theme_minimal()
# 
# 
# # ------------------
# # Linear regression
# # ------------------
# 
# # Select net price and debt variables for low income group
# price_debt_df <- results %>% 
#   select(UNIQUE_IDENTIFICATION_NUMBER_OF_THE_INSTITUTION, `Average net price (income 0-30,000)-students awarded Title IV federal financial aid, 2020-21 (SFA2021)`, `The median debt for students with family income between $0-$30,000 (NSLDS)`)
# 
# # Create scatterplot
# price_debt_df %>% 
#   ggplot(aes(x = `Average net price (income 0-30,000)-students awarded Title IV federal financial aid, 2020-21 (SFA2021)`, y = `The median debt for students with family income between $0-$30,000 (NSLDS)`)) +
#   geom_point() +
#   stat_smooth(method = 'lm') +
#   xlab('Average net price') + ylab('Median debt')
# 
# # Run linear regression
# model <- lm(
#   data = price_debt_df,
#   formula = `The median debt for students with family income between $0-$30,000 (NSLDS)` ~ `Average net price (income 0-30,000)-students awarded Title IV federal financial aid, 2020-21 (SFA2021)`
# )
# 
# # View model info
# model
# summary(model)
