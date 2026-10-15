library(tidyverse)

data_dir <- file.path('.', 'data')


results <- read_csv(file.path(data_dir, 'regression_results.csv'))

results %>% glimpse()

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

table(is.na(filtered_results$`Percent admitted total`))  # 686 missing `Percent admitted total`

table(filtered_results$`Sector of Institution`)


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
}
