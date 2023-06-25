# A module that executes npstoret macroinvertebrates sql queries
options(warn=-1)

npstoret_macroinvertebrates_results <- function(){
    tryCatch(
        expr = {
            #----- load external libraries
            suppressWarnings(suppressMessages(library(RODBC)))
            suppressWarnings(suppressMessages(library(tidyverse)))
            
            #----- load project functions
            #-----  Query npstoret db
            channel <- RODBC::odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=C:\\Users\\cwainright\\OneDrive - DOI\\Documents\\data_projects\\2023\\iss156_bss_edd\\data\\npstoret\\NCRN_NPSTORET_BE_20230213.mdb")
            statement <- readr::read_file('modules/npstoret/macroinvertebrates/qry_npstoret_macroinvert_results.sql')
            data <- RODBC::sqlQuery(channel , statement)
            RODBC::odbcCloseAll()
            data <- rename(data, "#Org_Code" = "Org_Code")
            message("`npstoret_macroinvertebrates_results() executed successfully")
            return(data)
        }
    )
}

npstoret_macroinvertebrates_activities <- function(){
    tryCatch(
        expr = {
            #----- load external libraries
            suppressWarnings(suppressMessages(library(RODBC)))
            suppressWarnings(suppressMessages(library(tidyverse)))
            
            #----- load project functions
            #-----  Query npstoret db
            channel <- RODBC::odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=C:\\Users\\cwainright\\OneDrive - DOI\\Documents\\data_projects\\2023\\iss156_bss_edd\\data\\npstoret\\NCRN_NPSTORET_BE_20230213.mdb")
            statement <- readr::read_file('modules/npstoret/macroinvertebrates/qry_npstoret_macroinvert_activities.sql')
            data <- RODBC::sqlQuery(channel , statement)
            RODBC::odbcCloseAll()
            data <- data %>% distinct(Activity_ID, .keep_all = TRUE)
            data <- rename(data, "#Org_Code" = "Org_Code")
            message("`npstoret_macroinvertebrates_activities() executed successfully")
            return(data)
        }
    )
}

npstoret_macroinvertebrates_locations <- function(unique_locs){
    tryCatch(
        expr = {
            #----- load external libraries
            suppressWarnings(suppressMessages(library(RODBC)))
            suppressWarnings(suppressMessages(library(tidyverse)))
            
            #-----  Query npstoret db
            channel <- RODBC::odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=C:\\Users\\cwainright\\OneDrive - DOI\\Documents\\data_projects\\2023\\iss156_bss_edd\\data\\npstoret\\NCRN_NPSTORET_BE_20230213.mdb")
            statement <- readr::read_file('modules/npstoret/macroinvertebrates/qry_npstoret_macroinvert_locations.sql')
            data <- RODBC::sqlQuery(channel , statement)
            RODBC::odbcCloseAll()
            data <- data %>% distinct(Location_ID, .keep_all = TRUE)
            data <- data %>% filter(
                Location_ID %in% unique_locs
            )
            data <- rename(data, "#Org_Code" = "Org_Code")
            message("`npstoret_macroinvertebrates_locations() executed successfully")
            return(data)
        }
    )
}