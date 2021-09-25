library(rmarkdown)
library(tidyverse)
library(pdftools)

# https://community.rstudio.com/t/strange-rmd-knitting-behavior/71027
# https://stackoverflow.com/questions/58520640/how-to-cache-intermediate-results-when-rendering-with-parameters


district <- federal_boundaries$district

reports <- tibble(
  filename = str_c("summary-", district, ".pdf"),
  params = map(district, ~list(district = .,   
                               election_or_redist_object = "federal_election",
                               result_name = "Two Candidate Preferred",
                               single_group_code = NA,
                               breaks = seq(0.5,0.95,0.05),
                               from_electorate = "Division",
                               to_electorate_string = "the Division of {{Electorate}}"))
)

reports %>%
  select(output_file = filename, params) %>%
  pwalk(rmarkdown::render, input = "District Summary.Rmd", output_format = "pdf_document", output_dir = "output/")

pdf_combine(list.files("output/", pattern = "\\.pdf", full.names = TRUE), output = "joined.pdf")
