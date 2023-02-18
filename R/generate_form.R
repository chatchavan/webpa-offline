library(tidyverse)
library(openxlsx)


path_template   <- "input/template.xlsx"
path_group_list <- "input/students_groups.xlsx"
path_output     <- "output/template_for_students"


# cell styles
style_head <- createStyle(
  numFmt = "TEXT",
  textDecoration = "Bold"
)

style_myname <- createStyle(
  numFmt = "TEXT",
  halign = "center",
  valign = "top",
  fgFill = "#E2EFDA",
  locked = FALSE
)

style_person <- createStyle(
  numFmt = "TEXT",
  halign = "left",
  valign = "top",
  locked = TRUE
)

style_rating <- createStyle(
  numFmt = "0",
  halign = "center",
  valign = "top",
  fgFill = "#E2EFDA",
  locked = FALSE
)

style_comment <- createStyle(
  numFmt = "TEXT",
  valign = "top",
  fgFill = "#FFE69A",
  locked = FALSE
)

style_team <- createStyle(
  numFmt = "TEXT",
  halign = "center",
  valign = "top",
  locked = TRUE
)

style_check_formula <- createStyle(
  numFmt = "TEXT",
  fontColour = "#FF4040",
  locked = TRUE
)

# formula for checking
formula_name   <- "=IF(COUNTA(webpa[My name (x)]) = 1, \"\",\"• Please add 'x' on the 'My name (x)' column in front of your name.\")"
formula_rating <- "=IF(COUNT(webpa[Rating]) = ROWS(webpa[Rating]), \"\",\"• Please rate all team members\")"
formula_df <- tribble(
  ~formula,
  formula_name,
  formula_rating
)
class(formula_df$formula) <- c(class(formula_df$formula), "formula")

#================ begin work ================ 

# read the template Excel file
template_wb <- loadWorkbook(path_template)
template_df <- readWorkbook(template_wb, skipEmptyRows = FALSE, skipEmptyCols = FALSE, colNames = FALSE)


# figure out relevant indices
row_header <- which(template_df[1] == "My name (x)")
row_header_df <- template_df[row_header,]

col_myname  <- which(row_header_df == "My name (x)")
col_person  <- which(row_header_df == "Person")
col_rating  <- which(row_header_df == "Rating")
col_team    <- which(row_header_df == "Team")
col_comment <- which(row_header_df == "Comments")
cols_header <- seq(1, max(col_myname, col_person, col_rating, col_team, col_comment))

col_myname  <- int2col(col_myname)
col_person  <- int2col(col_person)
col_rating  <- int2col(col_rating)
col_team    <- int2col(col_team)
col_comment <- int2col(col_comment)
cols_header <- int2col(cols_header)


# read the group data and add columns for the forms
groups_data <- 
  read.xlsx(path_group_list, 1) %>% 
  mutate(
    `My name (x)` = NA,
    `Rating` = NA,
    `Comments` = NA) %>% 
  select(              # order columns must match the template
    `My name (x)`,
    `Person`,
    `Rating`,
    `Comments`,
    `Team`)

data_list <- split(groups_data, f = groups_data$Team) 


#----------- loop for each group ----------- 

for(individual_df in data_list) {
  
  wb_temp <- copyWorkbook(template_wb)
  
  writeDataTable(wb_temp, sheet = 1,
    startRow = row_header,
    as.data.frame(individual_df),
    colNames = TRUE,
    tableName = "webpa",
    withFilter = FALSE)
  
  row_data_first <- row_header + 1
  rows_data      <- seq(row_data_first, length.out = nrow(individual_df))
  row_data_last  <- max(rows_data)
  rows_formula   <- seq(row_data_last + 2, length.out = nrow(formula_df))
  col_formula    <- 1
  
  writeData(wb_temp, 1, 
            startCol = col_formula, 
            startRow = min(rows_formula), 
            colNames = FALSE,
            x = formula_df)
  
  
  # styling
  addStyle(wb_temp, 1, rows = row_header, cols = cols_header, style = style_head)
  addStyle(wb_temp, 1, rows = rows_data,  cols = col_myname,  style = style_myname)
  addStyle(wb_temp, 1, rows = rows_data,  cols = col_person,  style = style_person)
  addStyle(wb_temp, 1, rows = rows_data,  cols = col_rating,  style = style_rating)
  addStyle(wb_temp, 1, rows = rows_data,  cols = col_comment, style = style_comment)
  addStyle(wb_temp, 1, rows = rows_data,  cols = col_team,    style = style_team)
  addStyle(wb_temp, 1, rows = rows_formula, cols = col_formula, style = style_check_formula)
  
  
  # restrict editing
  protectWorksheet(wb_temp, 1, protect = TRUE,
                   lockInsertingRows = TRUE,
                   lockInsertingColumns = TRUE,
                   lockDeletingRows = TRUE,
                   lockDeletingColumns = TRUE,
                   lockSelectingUnlockedCells = FALSE,
                   lockSelectingLockedCells = FALSE,
                   lockSorting = TRUE)
  
  # write output file  
  team_number <- individual_df$Team[1]
  file_name <- file.path(path_output, 
                         sprintf("Peer assessment Team %02d.xlsx", team_number))
  saveWorkbook(wb_temp, file_name, overwrite = TRUE)

} # end for