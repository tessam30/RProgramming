# Creating a list of helper functions that I find myself always writing or looking up parts of


# Create a simple function to replace the column names with the data fron the nth row
rowHeader <- function(df, x = 1) {
  
  # Unlist the names in row x and convert to characters which are passed to the names
  names(df) <- as.character(unlist(df[x,]))
  
  # Remove the row used for the names from the data frame
  df_new = df[-x,]
  
  return(df)
}

tmp = rowHeader(mtcars, x = 1)


# Function from stackoverflow to read all tabs in spreadsheet and return in a list of tibbles
# Inputs required: filename of the excel file

read_excel_allsheets <- function(filename) {
  sheets <- readxl::excel_sheets(filename)
  x = lapply(sheets, function(X) readxl::read_excel(filename, sheet = X, col_names = FALSE))
  names(x) <- sheets
  return(x)
}
