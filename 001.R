# Install the necessary package
install.packages("tidyverse")
install.packages("naniar")
library(tidyverse)
library(gtsummary)
library(easystats)
library(gt)
install.packages("glue")
library(glue)

# Load necessary libraries
library(tidyverse)
library(ggplot2)
library(tseries)
library(naniar)
# Install the necessary package
install.packages("tidyverse")

# Load necessary libraries
library(tidyverse)
library(ggplot2)
library(tseries)
# Install required packages
install.packages("readxl")
install.packages("writexl")
install.packages("tseries")
install.packages("ggplot2")
install.packages("forecast")

# Load necessary libraries
library(readxl)
library(tseries)
library(ggplot2)
library(forecast)
library(dplyr)



# Read the Excel file

AMR_KAP_No_Code <- read_excel("Raw Data/AMR_KAP_No_Code.xlsx")
View(AMR_KAP_No_Code)
# check missing data 
sum(is.na(AMR_KAP_No_Code))
miss_var_summary(AMR_KAP_No_Code)
gg_miss_var(AMR_KAP_No_Code)

# check duplicated rows 
sum(duplicated(AMR_KAP_No_Code))

# View the data (optional, only works in RStudio)

print(AMR_KAP_No_Code)
summary(AMR_KAP_No_Code)
# Table 1. Demographic characteristics of study participants 
AMR_KAP_No_Code |> 
  select(1:8) |> 
  tbl_summary(
    statistic = list(
      all_continuous() ~ "{mean}±{sd}"
    )
  ) |> 
  as_gt() |> 
  gtsave("tables/Table1.docx")

tab_1 <-
  gtcars |>
  dplyr::select(model, year, hp, trq) |>
  dplyr::slice(1:5) |>
  gt(rowname_col = "model") |>
  tab_stubhead(label = "car")
library(gt)

# Define the start and end dates for the data range
start_date <- "2010-06-07"
end_date <- "2010-06-14"

# Create a gt table based on preprocessed
# `sp500` table data
sp500 |>
  dplyr::filter(date >= start_date & date <= end_date) |>
  dplyr::select(-adj_close) |>
  gt() |>
  tab_header(
    title = "S&P 500",
    subtitle = glue::glue("{start_date} to {end_date}")
  ) |>
  fmt_currency() |>
  fmt_date(columns = date, date_style = "wd_m_day_year") |>
  fmt_number(columns = volume, suffixing = TRUE)

# Install necessary packages (run only if not already installed)
install.packages(c("tidyverse", "naniar", "gtsummary", "gt", "glue", "readxl", "writexl", "tseries", "forecast"))

# Load necessary libraries
library(tidyverse)
library(ggplot2)
library(tseries)
library(naniar)
library(gtsummary)
library(gt)
library(glue)
library(readxl)
library(forecast)

# Read the Excel file (replace with your file path)
AMR_KAP_No_Code <- read_excel("Raw Data/AMR_KAP_No_Code.xlsx")

# View the data (optional, only works in RStudio)
View(AMR_KAP_No_Code)

# Check for missing data and visualize
cat("Number of missing values:", sum(is.na(AMR_KAP_No_Code)), "\n")
miss_var_summary(AMR_KAP_No_Code)
gg_miss_var(AMR_KAP_No_Code) + ggtitle("Missing Data per Variable")

# Check for duplicated rows
cat("Number of duplicated rows:", sum(duplicated(AMR_KAP_No_Code)), "\n")

# Summary statistics of the data
summary(AMR_KAP_No_Code)

# Table 1: Demographic characteristics of study participants
AMR_KAP_No_Code %>%
  select(1:8) %>%
  tbl_summary(statistic = list(all_continuous() ~ "{mean}±{sd}")) %>%
  as_gt() %>%
  gtsave("tables/Table1.docx")

# read data sheet2 
data2 <- readxl::read_excel("Raw Data/AMR_KAP_No_Code", sheet = 2)

# Impact of education on level of knowledge of antibiotics 

y = mx + c

model <- lm(TotalKnowledgeScore ~ `Parent’s education level`, data = data2)

summary(model)

report(model)

# Example gt table using gtcars dataset
tab_1 <- gtcars %>%
  select(model, year, hp, trq) %>%
  slice(1:5) %>%
  gt(rowname_col = "model") %>%
  tab_stubhead(label = "Car Models")

# Define the start and end dates for the S&P 500 data range
start_date <- "2010-06-07"
end_date <- "2010-06-14"

# Create a gt table for S&P 500 data
sp500 %>%
  filter(date >= start_date & date <= end_date) %>%
  select(-adj_close) %>%
  gt() %>%
  tab_header(
    title = "S&P 500",
    subtitle = glue("{start_date} to {end_date}")
  ) %>%
  fmt_currency() %>%
  fmt_date(columns = date, date_style = "wd_m_day_year") %>%
  fmt_number(columns = volume, suffixing = TRUE)


