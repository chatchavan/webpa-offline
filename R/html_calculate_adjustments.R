library(tidyverse)
library(jsonlite)
library(fs)

json_root <- "html/Test rating files/"
output_root <- "output"
students_team_path <- "html/student_teams.csv"
min_adj_sd <- 0.2   # if the SD of the adjustment factor is below this threshold, everyone gets the same grade

#-------------------------------------------------------------------------------
# process JSON files
rating_input_df <- (function() {
    table_df <- data.frame(team = character(), rater = character(), 
               ratee = character(), rating = numeric(), comment = character(), 
               overall_comment = character())
  
  json_files <- list.files(json_root, pattern = "\\.json$", full.names = TRUE)
  for (file in json_files) {
    data <- fromJSON(file, flatten = TRUE)
    
    team <- as.character(data$team)
    rater <- data$rater
    
    if ("overallComment" %in% names(data)) {
      overall_comment <- data$overallComment
    } else {
      overall_comment <- NA
    }
    
    for (name in names(data$ratings)) {
      rating <- data$ratings[[name]]$rating
      if ("comment" %in% names(data$ratings[[name]])) {
        comment <- data$ratings[[name]]$comment
      } else {
        comment <- NA
      }
      
      new_row <- 
        tibble(team = team, rater = rater, 
               ratee = name, rating = as.numeric(rating), comment = comment,
               overall_comment = overall_comment)
        
      table_df <- rbind(table_df, new_row)
    }
  }
  
  # return
  table_df
})()

rm(json_root)


#-------------------------------------------------------------------------------
# load student team file

students_team_df <- 
  read_csv(students_team_path,
           col_types = cols(
             team = col_character(),
             student_id = col_character(),
             firstname = col_character(),
             lastname = col_character(),
             email = col_character()
           )) |> 
  mutate(fullname = str_c(firstname, " ", lastname))

rm(students_team_path)

n_group_df <- 
  students_team_df |> 
  group_by(team) |> 
  summarize(team_size = n()) |> 
  ungroup() |> 
  select(team = team, team_size)

#-------------------------------------------------------------------------------
# calculate fudge factor based on the number of received ratings

fudge_df <- (function() {
  n_submitted_df <- 
    rating_input_df |> 
    group_by(team) |> 
    summarize(n_raters = n_distinct(rater))
  
  fudge_df_out <- 
    n_group_df |> 
    left_join(n_submitted_df, by = "team") |> 
    mutate(n_raters = replace_na(n_raters, 0)) |> 
    mutate(fudge_factor = team_size / n_raters) |> 
    select(team, fudge_factor)
  
  # return
  fudge_df_out
})()

#-------------------------------------------------------------------------------
# account for students who did not submit

rating_complete_df <- (function(){
  # create the complete crossing of (rater x ratee) for each team
  complete_list_df <-
    students_team_df |>
    mutate(rater = fullname) |> 
    select(rater, team = team) |> 
    mutate(ratee = rater) |> 
    group_by(team) |> 
    expand(rater, ratee) |> 
    ungroup()
  
  
  # merge the complete list with the actual submitted ratings
  out_df <- 
    complete_list_df |> 
    left_join(rating_input_df, by = c("team", "rater", "ratee")) 
  
  out_df
})()

rm(rating_input_df)



#-------------------------------------------------------------------------------
# calculate adjustments
adjustment_df <- 
  
  rating_complete_df |> 
  
  # normalize by total given ratings from each rater
  mutate(rater_total_given = sum(rating, na.rm = TRUE), .by = c(team, rater)) |>
  mutate(norm_received = rating / rater_total_given) |> 
  
  # accumulate normalized rating for each ratee
  summarize(sum_norm_received = sum(norm_received, na.rm = TRUE), .by = c(team, ratee)) |> 
  
  # adjust with the fudge factor
  left_join(fudge_df, by = "team") |> 
  mutate(adj_factor = sum_norm_received * fudge_factor) |> 
  
  # handle the case when nobody from the group submitted
  mutate(adj_factor = if_else(is.nan(adj_factor), 1, adj_factor)) |> 
  
  # add student ID of ratee
  left_join(students_team_df, by = c("team" = "team", "ratee" = "fullname")) |> 
  
  # keep only relevant columns
  select(team, 
         ratee_student_id  = student_id, 
         ratee, 
         ratee_email  = email, 
         adj_factor)


#-------------------------------------------------------------------------------
# verification: adding up all WebPA score for every group should equal to the number of team members in each group

webpa_check_df <- 
  adjustment_df |> 
  group_by(team) |> 
  summarize(sum_adj = sum(adj_factor)) |> 
  ungroup() |> 
  left_join(n_group_df, by = "team") |> 
  mutate(is_equal = round(sum_adj) == round(team_size))

stopifnot(all(webpa_check_df$is_equal))

#-------------------------------------------------------------------------------
# skip adjustment if SD doesn't reach the threshold

webpa_sd_df <- 
  
  # calculate SD of the adjustment factor for each group
  adjustment_df |> 
  summarize(adj_sd = sd(adj_factor), .by = team) |> 
  inner_join(adjustment_df, by = "team", multiple = "all") |> 
  
  # adjust only if the SD of the adjustment factor exceeds the threshold
  mutate(thresholded_adj_factor = if_else(adj_sd > min_adj_sd, adj_factor, 1))



#-------------------------------------------------------------------------------
# write output

# adjustment
webpa_sd_df |> 
  write_csv(path(output_root, "adjustment_score.csv"))

# detailed rating together with comments
rating_complete_df |> 
  write_csv(path(output_root, "all_ratings.csv")) 