# a module for `edd_activities()`
# ETL summer_index_marc2022 into edd.activities
options(warn = -1)
marc_2022_summer_exotic_locations <- function(summer_exotic_marc2022, example, results_list){
    tryCatch(
        expr = {
            #----- load external libraries
            suppressWarnings(suppressMessages(library(data.table)))
            suppressWarnings(suppressMessages(library(tidyverse)))
            suppressWarnings(suppressMessages(library(dplyr)))
            suppressWarnings(suppressMessages(library(readxl)))
            
            #----- wrangle
            df <- summer_exotic_marc2022
            df$Site <- paste0(df$Site, "-N") # add the '-N' suffix so we can xref against `results_list$tbl_Locations`
            df <- dplyr::left_join(df, results_list$tbl_Locations %>% select(Site_ID, NCRN_Site_ID, Unit_Code, Loc_Name), by=c("Site" = "Site_ID")) %>%
                mutate(Event_Site_ID = paste0(Site, "-2022")) # create a column `Event_Site_ID`
            
            #----- use NCRN lookup tables to add values that are missing from Marc's data
            df <- dplyr::left_join(df, results_list$tbl_Locations %>% select(-c(Unit_Code, Loc_Name, Site_ID)), by=c("NCRN_Site_ID")) # add lookup values
            
            #----- build to match example
            real <- tibble::tibble(data.frame(matrix(ncol = ncol(example), nrow = nrow(df)))) # empty dataframe
            colnames(real) <- colnames(example) # name columns to match example
            
            real[1] <- "NCRN" # "#Org_Code"
            for(i in 1:nrow(real)){
                real[i,2] <- substr(df$NCRN_Site_ID[i], 6, 9) # "Park_Code" 
            }
            real[3] <- df$NCRN_Site_ID # "Location_ID" shared field with `real_activities.Location_ID`
            real[4] <- df$Loc_Name # "Location_Name"
            real[5] <- "Creek" # "Location_Type"
            real[6] <- df$Dec_Degrees_North # "Latitude"
            real[7] <- df$Dex_Degrees_East # "Longitude"
            real[8] <- "GPS-Unspecified" # "Lat_Lon_Method"
            real[9] <- df$Datum # "Lat_Lon_Datum"
            real[10] <- NA # "Source_Map_Scale_Numeric" 
            real[11] <- NA # "Lat_Lon_Accuracy"
            real[12] <- NA # "Lat_Lon_Accuracy_Unit"
            real[13] <- NA # "Location_Description"
            real[14] <- NA # "Travel_Directions"
            real[15] <- NA # "Location_Purpose"
            real[16] <- NA # "Establishment_Date" 
            real[17] <- df$HUC # "HUC8_Code"
            real[18] <- NA # "HUC12_Code"; remove sci notation
            real[19] <- NA # "Alternate_Location_ID"
            real[20] <- NA # "Alternate_Location_ID_Context"
            real[21] <- NA # "Elevation" 
            real[21] <- NA # "Elevation_Unit" 
            real[23] <- NA # "Elevation_Method" 
            real[24] <- NA # "Elevation_Datum"
            real[25] <- NA # "Elevation_Accuracy"
            real[26] <- NA # "Elevation_Accuracy_Unit"
            real[27] <- "US" # "Country_Code"
            real[28] <- df$State # "State_Code"
            real[29] <- df$County # "County_Name"
            real[30] <- sprintf("%.3f", df$Catchment_Area) # "Drainage_Area"
            # "Drainage_Area_Unit"; design view db.tbl_Locations
            for(i in 1:nrow(real)){
                # catch NA catchment areas
                if(!is.na(real[i,30])){
                    real[i,31] <- "acre"
                }
            }
            real[32] <- NA # "Contributing_Area"
            real[33] <- NA # "Contributing_Area_Unit"
            real[34] <- NA # "Tribal_Land_Indicator"
            real[35] <- NA # "Tribal_Land_Name"
            real[36] <- NA # "Well_ID"
            real[37] <- NA # "Well_Type"
            real[38] <- NA # "Aquifer_Name"
            real[39] <- "Stream habitat survey" # "Formation_Type"
            real[40] <- NA # "Well_Hole_Depth"
            real[41] <- NA # "Well_Hole_Depth_Unit"
            real[42] <- NA # "Well_Status"
            
            real <- as.data.frame(lapply(real, function(y) gsub("\\<NA\\>", NA, y))) # remove "NA" chr strings
            colnames(real)[1] <- "#Org_Code"
            
            # error-checking:
            check_df <- tibble::tibble(data.frame(matrix(ncol=3, nrow=ncol(real))))
            colnames(check_df) <- c("real", "example", "result")
            check_df$real <- colnames(real)
            check_df$example <- colnames(example)
            for(i in 1:nrow(check_df)){
                if(check_df$real[i] == check_df$example[i]){
                    check_df$result[i] <- "MATCH"
                } else {
                    check_df$result[i] <- "MISMATCH"
                }
            }
            
            message(
                if(length(check_df$result == "MATCH") == nrow(check_df)){
                    "`marc_2022_summer_exotic_locations()` executed successfully..."
                } else {
                    for(i in 1:length(check_df$result != "MATCH")){
                        cat(paste(paste0("`real", check_df$real[i], "`"), paste0(" DID NOT MATCH `example.", check_df$example[i][i], "`"), "\n", sep = ""))
                    }
                }
            )
            # assign("real", real, envir = globalenv())
            return(real)
        }
    )
}