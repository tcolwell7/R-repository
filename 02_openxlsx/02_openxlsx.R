### /// --- Automated spreadsheet creation using openxlsx --- /// ###


# Set up ----

rm(list = ls())

# load libraries:

library(tidyverse)
library(janitor)
library(openxlsx)
library(stringr)
library(readxl) # package for read_excel function

# set up path location

path<-setwd(stringr::str_extract(rstudioapi::getActiveDocumentContext()$path,".+[/]")) 

# upload trade data:
# trade data for 20 + countries for imports and exports with the UK in 2020. 

df <- read_excel("..\\data\\trade_data.xlsx") %>% clean_names()

# 01. Create simple spreadsheet ----

# filter for United states (US). and select top 50 rows

df2 <- 
  df %>%
  filter(country_code == "US") %>%
  head(50)

# export dataframe to excel:

# you can export simply using csv or using openxlsx a xlsx format spreadsheet:
write.csv(df2, "..\\outputs\\01_data_output.csv")
write.xlsx(df2, "..\\outputs\\01_data_output.xlsx")

# ` i. Create simple workbook -------
# you can create your spreadsheet from scratch using openxlsx:
# Step 1. create workbook:

wb <- createWorkbook()

# Step 2. create worksheets:

addWorksheet(wb, sheetName = "Trade data")
addWorksheet(wb, sheetName = "Data filtered")

# Step 3. Select data to insert into spreadsheet 

writeData(wb, sheet = "Trade data", df)
writeData(wb, sheet = "Data filtered", df2, startCol = 2, startRow = 3)


# ` ii Simple formatting ----
# Step 4. (Optional) formatting

# 1. Bold headers. (createStyle)
# 2. Cell borders  (createStyle)
# 3. increase column widths (setColWidths)

# create header style for columns using createStyle
headerStyle <- createStyle(fontSize = 12, wrapText = TRUE, textDecoration = "bold")
# cell border simple formatting
borderStyle <- createStyle(border = "TopBottomLeftRight")
# increase column widths
setColWidths(wb, sheet = "Data filtered", cols = c(2:8), width = 15)

## apply styles:
addStyle(wb, sheet = "Data filtered", style = headerStyle, rows = 3, cols = c(2:8))
addStyle(wb, sheet = "Data filtered", style = borderStyle, rows = 4:53, cols = c(2:8), gridExpand = T)

# Final steps save and export workbook:

saveWorkbook(wb, file = "..\\outputs\\01_created_xl_output.xlsx", overwrite = TRUE)


# 02. Expand formatting ---- 

# create more expansive and detailed formatting for a spreadsheet:

# 0. Expand column widths:
# 1. Add colored borders. 
# 2. Expand and bold headers. 
# 3. Add merged cell headers.
# 4. Format numbers
# 5. conditional formatting
# 6. Freezepane


# set up spreadsheet:

wb <- createWorkbook() 
addWorksheet(wb, sheetName = "Data")
writeData(wb, sheet = "Data", df2, startRow = 2, startCol = 1) # df is 51 rows. 7 columns. 

# 0. set column widths:
setColWidths(wb, sheet = "Data", cols = 1:8, width = 15)

# ` i. Border and header styles --------

# 1. create border style:
borderStyle <- createStyle(border = "TopBottom", borderColour = "#4F81BD")

# 2. create headerStyle:

headerStyle <- createStyle(fontSize = 12, 
                           fontColour = "#FFFFFF", 
                           halign = "center",
                           fgFill = "#4F81BD", 
                           border="TopBottom", 
                           borderColour = "#4F81BD", 
                           wrapText = TRUE, 
                           textDecoration = "bold")

# ` ii. merged cells ---------
# 3. Add merged cell header: (one row merged across all columns 1:7). 

# first write header title
writeData(wb, "Data", "Total trade with United Kingdom", 
          startCol = 1, 
          startRow = 1, 
          borders="surrounding", 
          borderColour = "black")

mergeCells(wb, "Data", cols = 1:7, rows = 1)

firstRow <- createStyle(fontSize = 14, halign = "center", border = "TopBottomLeftRight", textDecoration = "bold", borderStyle = "thick")

# 4. Create number formatting:

# the same border styles are required, otherwise the cells this style applies to resets to default. 
numStyle <- createStyle(numFmt = "#,##0", 
                        border = "TopBottom", 
                        borderColour = "#4F81BD")


# ` iii. Conditional formatting ----- 

# 5. Conditional formatting for low trade values:

# set conditional format style if value is <= 2,000 highlight cell red to indicate low trade. 

negStyle  <- createStyle(fontColour = "#9C0006", bgFill = "#FFC7CE")
conditionalFormatting(wb, "Data", cols=6, rows=3:52, rule="<=2000", style = negStyle)

# ` iv. Freezepanes --------- 

# 6. freezepane:

freezePane(wb, "Data", firstActiveRow = 3, firstActiveCol = 1)

# Add styles:

addStyle(wb, sheet = "Data", borderStyle, rows = 3:52, cols = 1:7, gridExpand = T)
addStyle(wb, sheet = "Data", headerStyle, rows = 2, cols = 1:7)
addStyle(wb, sheet = "Data", numStyle, rows = 3:52, cols = 6, gridExpand = T)
addStyle(wb, sheet = "Data", firstRow, rows = 1, cols = 1:7, gridExpand = T)

# save workbook:

saveWorkbook(wb, file = "..\\outputs\\02_created_xl_output.xlsx", overwrite = TRUE)


# 03 Create function -----------

# create automated function where column names are specified and functions refer to dynamic number of rows and columns. 
# input is the same trade data but is set up to react and create spreadsheets for different size dataframes. 

createFormatted_xl <- function(x,name,folder = NULL){
  
  # x input is the data frame
  # y input is the country name
  # folder is manual_input to change folder/file name. 
  
  rowNo <- nrow(x)
  colNo <- ncol(x)
  
  wb <- createWorkbook() 
  addWorksheet(wb, sheetName = name)
  writeData(wb, sheet = name, x, startRow = 2, startCol = 1) # set row to 2 to insert merged cell in row 1 for header title. 
  
  # 0. set column widths:
  setColWidths(wb, sheet = name, cols = 1:colNo, width = 15)
  
  # 1. create border style:
  borderStyle <- createStyle(border = "TopBottom", borderColour = "#4F81BD")
  
  # 2. create headerStyle:
  headerStyle <- createStyle(fontSize = 12, 
                             fontColour = "#FFFFFF", 
                             halign = "center",
                             fgFill = "#4F81BD", 
                             border="TopBottom", 
                             borderColour = "#4F81BD", 
                             wrapText = TRUE, 
                             textDecoration = "bold")
  
  # 3. Add merged cell header: (one row merged across all columns 1:7). 
  # first write header title
  
  headerTitle <- paste0("Total trade between the UK and ", name)
  
  writeData(wb, name, headerTitle, 
            startCol = 1, 
            startRow = 1, 
            borders="surrounding", 
            borderColour = "black")
  
  mergeCells(wb, name, cols = 1:colNo, rows = 1)
  
  firstRow <- createStyle(fontSize = 14, 
                          halign = "center", 
                          border = "TopBottomLeftRight", 
                          textDecoration = "bold", 
                          borderStyle = "thick")
  
  # 4. Create number formatting:
  
  # the same border styles are required, otherwise the cells this style applies to resets to default. 
  numStyle <- createStyle(numFmt = "#,##0", 
                          border = "TopBottom", 
                          borderColour = "#4F81BD")
  
  # 5. Conditional formatting for low trade values:
  
  # set conditional format style if value is <= 2,000 highlight cell red to indicate low trade. 
  
  negStyle  <- createStyle(fontColour = "#9C0006", bgFill = "#FFC7CE")
  col_index <- grep("value_gbp", colnames(x))
  
  conditionalFormatting(wb, 
                        name, 
                        cols= col_index,
                        rows=3:rowNo, 
                        rule="<=2000", 
                        style = negStyle)
  
  # 6. freezepane:
  
  freezePane(wb, name, firstActiveRow = 3, firstActiveCol = 1)
  
  # Add styles:
  addStyle(wb, sheet = name, borderStyle, rows = 3:rowNo, cols = 1:colNo, gridExpand = T)
  addStyle(wb, sheet = name, headerStyle, rows = 2, cols = 1:colNo)
  addStyle(wb, sheet = name, numStyle, rows = 3:rowNo, cols = 6, gridExpand = T)
  addStyle(wb, sheet = name, firstRow, rows = 1, cols = 1:colNo, gridExpand = T)
  
  # save workbook:
  
  if(is.null(folder)){
  workbookName <- paste0("..\\outputs\\03_",name,"_output.xlsx")
  } else {
  workbookName <- paste0("..\\outputs\\",folder,"_",name,"_output.xlsx")
  }
  
  saveWorkbook(wb, workbookName, overwrite = TRUE)
  
}


# 04 Apply function -------

df3 <- df %>% filter(country_name == "Thailand") %>% arrange(commodity_code) %>% head(50)
df4 <- df %>% filter(country_name == "Taiwan") %>% arrange(commodity_code) %>% head(75)

createFormatted_xl(df3, "Thailand")
createFormatted_xl(df4, "Taiwan")

# if you had to export 20 countries...
# instead of manually typing all of them out a list of countries can be run through a loop. 

# 05 Run multiple countries ----

## // Example scenario : 
##          Filter trade data for 5 countries and save formatted spreadsheets
##          1. For each country - filter data
##          2. Shape dataframe
##          3. Apply create xl function and produce output

country_list <- c("Taiwan", "Thailand", "Vietnam", "Ukraine", "Yemen")

for(c in country_list){
  
  df5 <- 
    df %>% 
    filter(country_name == c) %>%
    arrange(commodity_code) %>%
    head(100)
  
  createFormatted_xl(df5, c, "05")
  
  print(paste0("Spreadsheet with ", nrow(df5), " rows created for ",c))
  
}



# 06 Map function to multiple ----------

# What if you want to save each data set in the same spreadsheet:
# Using map, combined with function can iterate through each country
# and save each country as a separate sheet within the spreadsheet
# NOTE: This can also be achieved using a loop but this is to highlight a different method. 

# insert data to create spreadsheet into list. The data stored in list is then extracted and saved in spreadsheet. 

list <- list()

country_list <- c("Taiwan", "Thailand", "Vietnam", "Ukraine", "United States"," Yemen")

for(c in country_list){
  
  df6 <- 
    df %>% 
    filter(country_name == c) %>%
    arrange(commodity_code) %>%
    head(100)
  
  list[[c]] <- df6
  
}


wb2 <- createWorkbook()

Map(function(data, name){
  
  addWorksheet(wb2, name)
  
  rowNo <- nrow(data)
  colNo <- ncol(data)
  
  writeData(wb2, 
            sheet = name, 
            data, 
            withFilter = TRUE,
            startRow = 2, 
            startCol = 1) # set row to 2 to insert merged cell in row 1 for header title. 
  
  # 0. set column widths:
  setColWidths(wb2, sheet = name, cols = 1:colNo, width = 15)
  
  # 1. create border style:
  borderStyle <- createStyle(border = "TopBottom", borderColour = "#4F81BD")
  
  # 2. create headerStyle:
  headerStyle <- createStyle(fontSize = 12, 
                             fontColour = "#FFFFFF", 
                             halign = "center",
                             fgFill = "#4F81BD", 
                             border="TopBottom", 
                             borderColour = "#4F81BD", 
                             wrapText = TRUE, 
                             textDecoration = "bold")
  
  # 3. Add merged cell header: (one row merged across all columns 1:7). 
  # first write header title
  
  headerTitle <- paste0("Total trade between the UK and ", name)
  
  writeData(wb2, name, headerTitle, 
            startCol = 1, 
            startRow = 1, 
            borders="surrounding", 
            borderColour = "black")
  
  mergeCells(wb2, name, cols = 1:colNo, rows = 1)
  
  firstRow <- createStyle(fontSize = 14, 
                          halign = "center", 
                          border = "TopBottomLeftRight", 
                          textDecoration = "bold", 
                          borderStyle = "thick")
  
  # 4. Create number formatting:
  
  # the same border styles are required, otherwise the cells this style applies to resets to default. 
  numStyle <- createStyle(numFmt = "#,##0", 
                          border = "TopBottom", 
                          borderColour = "#4F81BD")
  
  # 5. Conditional formatting for low trade values:
  
  # set conditional format style if value is <= 2,000 highlight cell red to indicate low trade. 
  
  negStyle  <- createStyle(fontColour = "#9C0006", bgFill = "#FFC7CE")
  col_index <- grep("value_gbp", colnames(data))
  
  conditionalFormatting(wb2, 
                        name, 
                        cols= col_index,
                        rows=3:rowNo, 
                        rule="<=2000", 
                        style = negStyle)
  
  # 6. freezepane:
  
  freezePane(wb2, name, firstActiveRow = 3, firstActiveCol = 1)
  
  # Add styles:
  addStyle(wb2, sheet = name, borderStyle, rows = 3:rowNo, cols = 1:colNo, gridExpand = T)
  addStyle(wb2, sheet = name, headerStyle, rows = 2, cols = 1:colNo)
  addStyle(wb2, sheet = name, numStyle, rows = 3:rowNo, cols = 6, gridExpand = T)
  addStyle(wb2, sheet = name, firstRow, rows = 1, cols = 1:colNo, gridExpand = T)
  
  
}, list, names(list))


saveWorkbook(wb2, file = "..\\outputs\\06_mapped_country_xl.xlsx", overwrite = TRUE)
