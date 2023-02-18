library(tidyverse)
library(openxlsx)

path_input_groups <- "input/students_groups.xlsx"
path_output <- "output/webpa_score.csv"
paths_submitted <- 
  "input/submitted_ratings" %>% 
  list.files(pattern = '*.xlsx', 
             full.names = TRUE, 
             recursive = TRUE)

# read groups
students_group_df <- read.xlsx(path_input_groups, sheet = 1)


#============= File reading ============= 

# function for parsing the rating file
extract_rating <- function(path_rating){
  message("Processing:", path_rating)
  
  a_rating_df <- 
    read.xlsx(path_rating, sheet = 1, namedRegion = "webparegion") %>% 
    select(
      `x` = "My.name.(x)",
      `name` = "Person",
      `rating` = "Rating",
      `comment` = "Comments",
      `group` = "Team")
  
  rating_long_df <- 
    a_rating_df %>% 
    mutate(rater = a_rating_df$name[which(a_rating_df$x == "x")]) %>% 
    select(ratee = name, rater, group_number = group, rating)
  
  rating_long_df
}

# read rating
rating_input_df <-
  paths_submitted %>%
  map_dfr(extract_rating)


#============= WebPA calculation  ============= 

# calculate fudge factor based on the number of received ratings
n_submitted_df <- 
  rating_input_df %>% 
  group_by(group_number) %>% 
  summarize(n_raters = n_distinct(rater))

n_group_df <- 
  students_group_df %>% 
  group_by(Team) %>% 
  summarize(group_size = n()) %>% 
  ungroup() %>% 
  select(group_number = Team, group_size)

fudge_df <-
  n_group_df %>% 
  left_join(n_submitted_df, by = "group_number") %>% 
  mutate(n_raters = replace_na(n_raters, 0)) %>% 
  mutate(fudge_factor = group_size / n_raters) %>% 
  select(group_number, fudge_factor)


# calculate adjustments
webpa_df <- 
  
  # generate list of all possible raters, cross with ratees
  students_group_df %>%
  select(rater = Person, group_number = Team) %>% 
  mutate(ratee = rater) %>% 
  group_by(group_number) %>% 
  expand(rater, ratee) %>% 
  
  # enter the submitted ratings
  left_join(rating_input_df, 
            by = c("group_number", "rater", "ratee")) %>% 
  
  # normalize by total given ratings from each rater
  group_by(group_number, rater) %>%
  mutate(rater_total_given = sum(rating, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(norm_received = rating / rater_total_given) %>% 
  
  # accumulate normalized rating for each ratee
  group_by(group_number, ratee) %>% 
  summarize(sum_norm_received = sum(norm_received, na.rm = TRUE)) %>% 
  ungroup() %>% 
  
  # adjust with the fudge factor
  left_join(fudge_df, by = "group_number") %>% 
  mutate(webpa_score = sum_norm_received * fudge_factor) %>% 
  
  # handle the case when nobody from the group submitted
  mutate(webpa_score = if_else(is.nan(webpa_score), 1, webpa_score)) %>% 
  
  # keep only relevant columns
  select(group_number, ratee, webpa_score)



# ============= Verification  ============= 
# Adding up all WebPA score for every group should equal to the number of team members in each group

webpa_check_df <- 
  webpa_df %>% 
  group_by(group_number) %>% 
  summarize(sum_webpa = sum(webpa_score)) %>% 
  ungroup() %>% 
  left_join(n_group_df, by = "group_number") %>% 
  mutate(is_equal = sum_webpa == group_size)

stopifnot(all(webpa_check_df$is_equal))


# ============= Write output  =============
webpa_df %>% 
  write_csv(path_output) 