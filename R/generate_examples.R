library(tidyverse)
library(fs)
library(openxlsx)


# example data (http://webpaproject.lboro.ac.uk/academic-guidance/a-worked-example-of-the-scoring-algorithm/)
example_df <- tribble(
  ~rater, ~rated, ~rating,
  "Alice",  "Alice",    4,
  "Alice",  "Bob",      4,
  "Alice",  "Claire",   3,
  "Alice",  "David",    2,
  "Alice",  "Elaine",   1,
  "Bob",    "Alice",    3,
  "Bob",    "Bob",      5,
  "Bob",    "Claire",   3,
  "Bob",    "David",    2,
  "Bob",    "Elaine",   0,
  "Claire", "Alice",    4,
  "Claire", "Bob",      4,
  "Claire", "Claire",   4,
  "Claire", "David",    4,
  "Claire", "Elaine",   4,
  "David",  "Alice",    3,
  "David",  "Bob",      5,
  "David",  "Claire",   4,
  "David",  "David",    3,
  "David",  "Elaine",   1)

# copy template files
file.copy(
  "output/template_for_students/Peer assessment Team 01.xlsx", 
  file.path("input/submitted_ratings/group1", str_c(unique(example_df$rater), ".xlsx")),
  overwrite = TRUE)

# =============== process each file ===============
for (a_rater in unique(example_df$rater)) {
  ratings <- 
    example_df %>% 
    filter(rater == a_rater) %>% 
    select(rating)
  
  path_excel <- file.path("input/submitted_ratings/group1", path_ext_set(a_rater, "xlsx"))
  wb <- loadWorkbook(path_excel)
  
  # determine columns
  df <- read.xlsx(wb, sheet = 1, namedRegion = "webparegion")
  col_x <- which(str_detect(names(df), "x"))
  col_rating <- which(str_detect(names(df), "Rating"))
  
  # determine the first data row
  regions <- getNamedRegions(wb)
  webpa_region_i <- which(regions == "webparegion")
  region_xl_coord <- attr(regions, "position")[webpa_region_i]
  row_region_first <- as.numeric(str_match(region_xl_coord, "\\d+"))
  
  
  # write the person selection (x)
  row_x <- which(df$Person == a_rater)
  writeData(wb, sheet = 1, 
            startCol = col_x, 
            startRow = row_region_first + row_x,
            colNames = FALSE,
            x= "x")
  
  
  # write the rating
  writeData(wb, sheet = 1, 
            startCol = col_rating, 
            startRow = row_region_first + 1, # first row is the header row
            colNames = FALSE,
            x= ratings)
  
  # save
  saveWorkbook(wb, path_excel, overwrite = TRUE)
  openXL(path_excel)
}