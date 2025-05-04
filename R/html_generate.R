# generate HTML form for each groups
library(tidyverse)
library(fs)

teams_data <- read_csv("html/student_teams.csv", col_types = "cccc")
html_template <- readLines("html/WebPA_input_template.html")
output_path <- "html/forms_for_students"

generate_html <- function(team_df){

  teamId <- as.character(team_df$team_id[1])
  students <- str_c(team_df$firstname, " ", team_df$lastname)
  
  # HTML
  html_content <- html_template
  html_content <- str_replace(html_content, coll('let teamId = 1;'), paste0('let teamId = ', teamId, ';'))
  html_content <- str_replace(html_content, coll('let students = ["John Doe", "Jane Smith", "Sam Johnson", "Michael Brown"];'), paste0('let students = ', jsonlite::toJSON(students), ';'))
  writeLines(html_content, path(output_path, paste0("team_", teamId, ".html")))
}

# process each team
teams_data %>%
  group_by(team_id) %>%
  group_split() %>%
  walk(~generate_html(.x))

