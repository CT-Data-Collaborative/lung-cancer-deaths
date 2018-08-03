library(dplyr)
library(datapkg)
library(stringr)
library(tidyr)

##################################################################
#
# Processing Script for Lung Cancer Deaths
# Created by Jenna Daly
# On 09/11/2017
#
##################################################################

#Setup environment
sub_folders <- list.files()
raw_location <- grep("raw", sub_folders, value=T)
path_to_raw <- (paste0(getwd(), "/", raw_location))
raw_data <- dir(path_to_raw, recursive=T, pattern = "xlsx") 

#read in entire xls file (all sheets)
read_excel_allsheets <- function(filename) {
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  names(x) <- sheets
  x
}

for (i in 1:length(raw_data)) {
  mysheets <- read_excel_allsheets(paste0(path_to_raw, "/", raw_data[i]))
  for (j in 1:length(mysheets)) {
    current_sheet_name <- colnames(mysheets[[j]])[1]
    current_sheet_name <- gsub(" Residents", "", current_sheet_name)
    current_sheet_file <- mysheets[[j]] 
    get_year <- substr((unlist(gsub("[^0-9]", "", unlist(raw_data[i])), "")), 2, 9)
    get_year <- sub( '(?<=.{4})', '-', get_year, perl=TRUE )
    assign(paste0(current_sheet_name, "_", get_year), current_sheet_file)
  }
}

#Concatenate all DFs
dfs <- ls()[sapply(mget(ls(), .GlobalEnv), is.data.frame)]

#Combine all geog DFs into one data frame
all_geogs <- data.frame(stringsAsFactors = F)
#Process each geog file
for (i in 1:length(dfs)) {
  #grab first geog file
  current_geog <- get(dfs[i])
  #setup columns and colnames
  current_geog <- current_geog[,1:4]
  colnames(current_geog) <- current_geog[2,]
  #Only grab "lung cancer" rows
  codes <- c("Lung cancer")
  current_geog <- current_geog[grepl(codes, current_geog$`Cause of Death`),]
  #Assign geog name and year based on DF name
  geog_name <- sapply(strsplit(dfs[i], "_"), "[", 1)
  year <- sapply(strsplit(dfs[i], "_"), "[", 2)  
  current_geog$Town <- geog_name
  current_geog$Year <- year
  #bind all geogs together
  all_geogs <- rbind(all_geogs, current_geog)  
}

#Clean up columns/names
all_geogs$`Cause of Death` <- NULL
colnames(all_geogs) <- gsub("\r\n", " ", colnames(all_geogs))
all_geogs <- all_geogs[all_geogs$Town != "current",]

#Convert to long format
all_geogs <- gather(all_geogs, `Measure Type`, Value, `No. of Deaths`:`AAMR`, factor_key=TRUE)
all_geogs$`Measure Type` <- as.character(all_geogs$`Measure Type`)

#Assign Measure Type column
all_geogs$`Measure Type`[all_geogs$`Measure Type` == "No. of Deaths"] <- "Number"
all_geogs$`Measure Type`[all_geogs$`Measure Type` == "Crude Rate"] <- "Crude Rate (per 100,000)"
all_geogs$`Measure Type`[all_geogs$`Measure Type` == "AAMR"] <- "AAMR (per 100,000)"

#Assign Variable column
all_geogs$Variable <- "Lung Cancer Deaths"

#Merge in FIPS
town_fips_dp_URL <- 'https://raw.githubusercontent.com/CT-Data-Collaborative/ct-town-list/master/datapackage.json'
town_fips_dp <- datapkg_read(path = town_fips_dp_URL)
fips <- (town_fips_dp$data[[1]])

all_geogs_fips <- merge(all_geogs, fips, all=T)

#Round Value column
all_geogs_fips$Value <- round(as.numeric(all_geogs_fips$Value), 2)

#Order and sort columns
all_geogs_fips <- all_geogs_fips %>% 
  select(Town, FIPS, Year, `Measure Type`, Variable, Value) %>% 
  arrange(Town, Year, `Measure Type`)

# Write to File
write.table(
  all_geogs_fips,
  file.path(getwd(), "data", "lung-cancer-deaths_2010-2014.csv"),
  sep = ",",
  na = "-6666",
  row.names = F
)
