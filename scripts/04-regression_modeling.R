library(tidyverse)

options(scipen = 999)

data_dir <- file.path('.', 'data')


# Read in data
results <- read_csv(file.path(data_dir, 'regression_results.csv'))

# Convert categorical variables to factor
#results$`Degree of Urbanization` <- factor(
#  results$`Degree of Urbanization`,
#  levels = c(
#    'City: Large', 'City: Midsize', 'City: Small',
#    'Suburb: Large', 'Suburb: Midsize', 'Suburb: Small',
#    'Town: Fringe', 'Town: Distant', 'Town: Remote',
#    'Rural: Fringe', 'Rural: Distant', 'Rural: Remote'
#  ),
#  ordered = T
#)

# Glimpse
results %>% glimpse()

# Filter rows to keep
results <- results %>% 
  mutate(
    keep = `Carnegie Classification 2015: Basic` %in% c(
      'Baccalaureate Colleges: Arts & Sciences Focus', 'Baccalaureate Colleges: Diverse Fields', "Baccalaureate/Associate's Colleges: Mixed Baccalaureate/Associate's",
      "Master's Colleges & Universities: Larger Programs", "Master's Colleges & Universities: Medium Programs", "Master's Colleges & Universities: Small Programs",
      'Doctoral Universities: Moderate Research Activity', 'Special Focus Four-Year: Arts, Music & Design Schools', 'Special Focus Four-Year: Business & Management Schools',
      'Special Focus Four-Year: Engineering Schools', 'Special Focus Four-Year: Faith-Related Institutions', 'Special Focus Four-Year: Law Schools', 'Special Focus Four-Year: Medical Schools & Centers', 
      'Special Focus Four-Year: Other Health Professions Schools', 'Special Focus Four-Year: Other Special Focus Institutions', 'Special Focus Four-Year: Other Technology-Related Schools'
    ) & (`Percent admitted total` > 0.5 | is.na(`Percent admitted total`))
  )

filtered_results <- results %>% filter(keep)  # 1892 filtered schools

# Check variables
table(is.na(filtered_results$`Percent admitted total`))  # 686 missing `Percent admitted total`

table(filtered_results$`Sector of Institution`)

# Variables in model
variables <- c(
  'In-State Cost of Attendance', 'Median Student Loan Debt', 'Median Student Loan Debt (0-30,000)', '10 Year Median Earnings', '10 Year Median Earnings (0-30,000)', 'Published In-State Tuition',
  'Average net price-students awarded grant or scholarship aid 2022-23', 'Average net price (income $0-30k)-students awarded Title IV aid 2022-23', 'Average Net Price for Students Awarded Title IV Federal Financial Aid, Income Level (0-30,000)',
  'Average Amount of Grant and Scholarship Aid Awarded, Income Level (0-30,000)', 'Percent of Students Awarded Pell Grants', 'Degree of Urbanization',
  'Total Amount of Grant and Scholarship Aid Awarded, Income Level (0-30,000)', 'Instructional expenditures per full-time equivalent student',
  'Graduation rate - 4 years (total)', 'Graduation rate - 6 years total', 'Student-to-faculty ratio', 'Proportion of faculty that is full-time', 'Three-year cohort default rate'
)

for (v in variables) {
  print(v)
  print(table(is.na(filtered_results[[v]])))  # TRUE = # of rows that are missing values
  print(summary(filtered_results[[v]]))
  
  #if (class(filtered_results[[v]]) == 'character') {
    #print(table(filtered_results[[v]]))
 #}
    if (is.character(filtered_results[[v]]) || is.factor(filtered_results[[v]])) {
      print(table(filtered_results[[v]]))
    }
}

#Regression Equations

#no independent variables
lm(
  `10 Year Median Earnings (0-30,000)` ~ 1,
  data = filtered_results
)

#mod zero + summary function
mod0 <- lm(
  `10 Year Median Earnings (0-30,000)` ~ 1,
  data = filtered_results
)

summary(mod0)

#net price inquiry, mod1
# add variables one at a time
filtered_results <- filtered_results %>%
  mutate(
    net_price_0_30k_1000 = `Average net price (income $0-30k)-students awarded Title IV aid 2022-23` / 1000, 
    in_state_coa_1000 = `In-State Cost of Attendance` / 1000,
    in_state_tuition_1000 = `Published In-State Tuition` / 1000,
    avg_grant_aid_0_30k_1000 = `Average grant/scholarship aid awarded-income $0-30k 2022-23` / 1000,
#    pct_pell_1000 = `Percent of Students Awarded Pell Grants` / 1000,
    total_grant_aid_0_30k_1000 = `Total grant/scholarship aid awarded-income $0-30k 2022-23` / 1000,
    instruction_expenses_fte_1000 = `Instructional expenditures per full-time equivalent student` / 1000,
#    grad_rate_4yr_1000 = `Graduation rate - 4 years (total)` / 1000,
#    stu_fac_ratio_1000 = `Student-to-faculty ratio` / 1000,
#    faculty_fte_1000 = `Proportion of faculty that is full-time` / 1000,
#    cdr_1000 = `Three-year cohort default rate` / 1000,
#    pct_stem_degrees_1000 = `Percent Degrees Awarded in Science, Technology, Engineering, and Math` / 1000,
#    pct_arts_degrees_1000 = `Percent Degrees Awarded in Arts & Humanities` / 1000,
  )


#option 1
mod1 <- lm(
  `10 Year Median Earnings (0-30,000)` ~ `Average net price (income $0-30k)-students awarded Title IV aid 2022-23`,
  data = filtered_results
)

summary(mod1)

#option 2 (x in 1000's)
# Add variables one at a time

mod1 <- lm(
  
  `10 Year Median Earnings (0-30,000)` ~ net_price_0_30k_1000,
  
  data = filtered_results
  
)

summary(mod1)

#Urbanization inquiry
filtered_results %>% count(`Degree of Urbanization`)

mod2 <- lm(
  `Debt-to-Earnings Ratio (0-30,000)` ~ 
    net_price_0_30k_1000 +
    factor(`Degree of Urbanization`, ordered = FALSE),
  data = filtered_results
)

summary(mod2)

# Collapse urbanization categories
filtered_results <- filtered_results %>%
  mutate(
    urbanization = case_when(
      `Degree of Urbanization` %in% c("Town: Fringe", "Town: Distant", "Town: Remote") ~ "Town",
      `Degree of Urbanization` %in% c("Rural: Fringe", "Rural: Distant", "Rural: Remote") ~ "Rural",
      `Degree of Urbanization` %in% c("Suburb: Midsize", "Suburb: Small") ~ "Suburb: Midsize/Small",
      TRUE ~ as.character(`Degree of Urbanization`)
    ),
    urbanization = factor(
      urbanization,
      levels = c(
        "City: Large",
        "City: Midsize",
        "City: Small",
        "Suburb: Large",
        "Suburb: Midsize/Small",
        "Town",
        "Rural"
      )
    )
  )

# Check new categories
filtered_results %>% count(urbanization)

mod2 <- lm(
  `10 Year Median Earnings (0-30,000)` ~ 
    net_price_0_30k_1000 +
    factor(`Degree of Urbanization`, ordered = FALSE),
  data = filtered_results
)

summary(mod2)

#Running the numerical X Vars / Earnings ONLY
#Net Price
mod1 <- lm(
  
  `10 Year Median Earnings (0-30,000)` ~ net_price_0_30k_1000,
  
  data = filtered_results
  
)

summary(mod1)


#COA
mod1 <- lm(
  `10 Year Median Earnings (0-30,000)` ~ `In-State Cost of Attendance`,
  data = filtered_results
)

summary(mod1)

#COA / 1000s
mod1 <- lm(
  
  `10 Year Median Earnings (0-30,000)` ~ in_state_coa_1000,
  
  data = filtered_results
  
)

summary(mod1)


# Med Debt / 1000s
mod1 <- lm(
  
  `10 Year Median Earnings (0-30,000)` ~ in_state_coa_1000,
  
  data = filtered_results
  
)

summary(mod1)

x_vars <- c(
  "net_price_0_30k_1000",
  "in_state_coa_1000",
  "in_state_tuition_1000",
  "avg_grant_aid_0_30k_1000",
  "pct_pell_1000",
  "total_grant_aid_0_30k_1000",
  "instruction_expenses_fte_1000",
  "grad_rate_4yr_1000",
  "stu_fac_ratio_1000",
  "faculty_fte_1000",
  "cdr_1000",
  "pct_stem_degrees_1000",
  "pct_arts_degrees_1000"
)

### TEST Earnings ONLY (0-30k) / looping X vars

# DOLLARS VARS / 1000s
# COA
# Cost of Tuition
# Net Price
# Average Grant Aid for Students With Family Incomes of $0–$30,000 
# Total Grant Award Amount for Students With Family Incomes of $0–$30,000 
# Instructional Expenses per FTE 

x_vars <- c(
  "net_price_0_30k_1000",
  "in_state_coa_1000",
  "in_state_tuition_1000",
  "avg_grant_aid_0_30k_1000",
  "total_grant_aid_0_30k_1000",
  "instruction_expenses_fte_1000"
)
# Save all model summaries in one file
for (i in seq_along(x_vars)) {
  model <- lm(
    as.formula(paste0("`10 Year Median Earnings (0-30,000)` ~ `", x_vars[i], "`")),
    data = filtered_results
  )
  capture.output(
    {
      cat("\n========================\n")
      cat("Model: `10 Year Median Earnings (0-30,000)` ~", x_vars[i], "\n")
      cat("========================\n\n")
      print(summary(model))
    },
    file = "dollar_earnings_regression_summary.txt",
    append = (i > 1)
  )
}

# OTHER VARS
# Percent of Students Receiving Pell Grants
# Graduation rate 
# Student-faculty ratio 
# Proportion of Faculty That Are Full-Time 
# CDR 
# ADD: % degrees awarded... from CR: STEM, Arts & Humanities

x_vars <- c("Percent of Students Awarded Pell Grants", "Graduation rate - 4 years (total)", "Student-to-faculty ratio", "Proportion of faculty that is full-time", "Three-year cohort default rate", "Percent Degrees Awarded in Science, Technology, Engineering, and Math", "Percent Degrees Awarded in Arts & Humanities")
# Save all model summaries in one file
for (i in seq_along(x_vars)) {
  model <- lm(
    as.formula(paste0("`10 Year Median Earnings (0-30,000)` ~ `", x_vars[i], "`")),
    data = filtered_results
  )
  capture.output(
    {
      cat("\n========================\n")
      cat("Model: `10 Year Median Earnings (0-30,000)` ~", x_vars[i], "\n")
      cat("========================\n\n")
      print(summary(model))
    },
    file = "other_earnings_regression_summary.txt",
    append = (i > 1)
  )
}


### TEST RATIO: debt-to-earnings (0-30k) / looping X vars

# DOLLARS VARS / 1000s
# COA
# Cost of Tuition
# Net Price
# Average Grant Aid for Students With Family Incomes of $0–$30,000 
# Total Grant Award Amount for Students With Family Incomes of $0–$30,000 
# Instructional Expenses per FTE 

x_vars <- c(
  "net_price_0_30k_1000",
  "in_state_coa_1000",
  "in_state_tuition_1000",
  "avg_grant_aid_0_30k_1000",
  "total_grant_aid_0_30k_1000",
  "instruction_expenses_fte_1000"
)
# Save all model summaries in one file
for (i in seq_along(x_vars)) {
  model <- lm(
    as.formula(paste0("`Debt-to-Earnings Ratio (0-30,000)` ~ `", x_vars[i], "`")),
    data = filtered_results
  )
  capture.output(
    {
      cat("\n========================\n")
      cat("Model: `Debt-to-Earnings Ratio (0-30,000)` ~", x_vars[i], "\n")
      cat("========================\n\n")
      print(summary(model))
    },
    file = "dollar_ratio_regression_summary.txt",
    append = (i > 1)
  )
}

# OTHER VARS
# Percent of Students Receiving Pell Grants
# Graduation rate 
# Student-faculty ratio 
# Proportion of Faculty That Are Full-Time 
# CDR 
# ADD: % degrees awarded... from CR: STEM, Arts & Humanities

x_vars <- c("Percent of Students Awarded Pell Grants", "Graduation rate - 4 years (total)", "Student-to-faculty ratio", "Proportion of faculty that is full-time", "Three-year cohort default rate", "Percent Degrees Awarded in Science, Technology, Engineering, and Math", "Percent Degrees Awarded in Arts & Humanities")
# Save all model summaries in one file
for (i in seq_along(x_vars)) {
  model <- lm(
    as.formula(paste0("`Debt-to-Earnings Ratio (0-30,000)` ~ `", x_vars[i], "`")),
    data = filtered_results
  )
  capture.output(
    {
      cat("\n========================\n")
      cat("Model: `Debt-to-Earnings Ratio (0-30,000)` ~", x_vars[i], "\n")
      cat("========================\n\n")
      print(summary(model))
    },
    file = "other_ratio_regression_summary.txt",
    append = (i > 1)
  )
}

#X vars for consideration
  
#Locale/Urbanicity

# COA
# Cost of Tuition
# Net Price
# Average Grant Aid for Students With Family Incomes of $0–$30,000 
# Percent of Students Receiving Pell Grants
# Total Grant Award Amount for Students With Family Incomes of $0–$30,000 
# Instructional Expenses per FTE 
# Graduation rate 
# Student-faculty ratio 
# Proportion of Faculty That Are Full-Time 
# CDR 
# ADD: % degrees awarded... from CR: STEM, Arts & Humanities
