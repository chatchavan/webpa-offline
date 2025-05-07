library(jsonlite)

directory <- "html/Test rating files/"

json_files <- list.files(directory, pattern = "\\.json$", full.names = TRUE)

result_df <- data.frame(team = character(), rater = character(), rating_for = character(),
                        rating = numeric(), comment = character())

for (file in json_files) {
  data <- fromJSON(file, flatten = TRUE)
  
  team <- data$team
  rater <- data$rater
  
  for (name in names(data$ratings)) {
    rating <- data$ratings[[name]]$rating
    if ("comment" %in% names(data$ratings[[name]])) {
      comment <- data$ratings[[name]]$comment
    } else {
      comment <- NA
    }
    
    new_row <- data.frame(team = team, rater = rater, rating_for = name,
                          rating = as.numeric(rating), comment = comment)
    result_df <- rbind(result_df, new_row)
  }
}

result_df
