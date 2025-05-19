library(jsonlite)

json_root <- "html/Test rating files/"

#-------------------------------------------------------------------------------
result_df <- 
  data.frame(team = character(), rater = character(), 
             rating_for = character(), rating = numeric(), comment = character(), 
             overall_comment = character())

json_files <- list.files(json_root, pattern = "\\.json$", full.names = TRUE)
for (file in json_files) {
  data <- fromJSON(file, flatten = TRUE)
  
  team <- data$team
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
             rating_for = name, rating = as.numeric(rating), comment = comment,
             overall_comment = overall_comment)
      
    result_df <- rbind(result_df, new_row)
  }
}

result_df

#-------------------------------------------------------------------------------