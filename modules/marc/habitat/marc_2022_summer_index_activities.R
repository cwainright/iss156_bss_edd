# a module for `edd_activities()`
# ETL summer_index_marc2022 into edd.activities
options(warn = -1)
marc_2022_summer_index_activities <- function(summer_index_marc2022, example, results_list){
    tryCatch(
        expr = {
            #----- load external libraries
            suppressWarnings(suppressMessages(library(data.table)))
            suppressWarnings(suppressMessages(library(tidyverse)))
            suppressWarnings(suppressMessages(library(dplyr)))
            suppressWarnings(suppressMessages(library(readxl)))
            
            #----- wrangle
            df <- summer_index_marc2022
            id_vars <- colnames(df)[1] # the column(s) we want to keep as columns
            measure_vars <- colnames(df %>% select(c(2:ncol(df)))) # the columns we want to melt into `value` and `variable` columns
            df <- melt(data.table::setDT(df), id.vars = id_vars,
                           measure.vars = measure_vars)
            data.table::setnames(df, "variable", "Site_ID") # rename columns to match df2021
            data.table::setnames(df, "Site", "variable") # rename columns to match df2021
            df$Site <- paste0(df$Site, "-N") # add the '-N' suffix so we can xref against `results_list$tbl_Locations`
            df <- dplyr::left_join(df, results_list$tbl_Locations %>% select(Site_ID, NCRN_Site_ID, Unit_Code, Loc_Name), by=c("Site" = "Site_ID")) %>%
                mutate(Event_Site_ID = paste0(Site, "-2022"), # create a column `Event_Site_ID`
                       Year = "2022", # create a column `Year`
                       Start_Date = NA) # create a column `Start_Date`

            # extract date
            tlu_date <- df %>% select(variable, value, Event_Site_ID) %>% subset(variable == "Date" & value != "8") # make a lookup table to grab sample dates
            tlu_date$value <- as.Date(as.numeric(trimws(tlu_date$value)), origin = "1899-12-30")
            df <- subset(df, variable != "Date") # get rid of variable == "Date because Date is its own column
            df <- dplyr::left_join(df, tlu_date %>% select(value, Event_Site_ID), by="Event_Site_ID")
            df$Start_Date <- df$value.y
            df$value.y <- NULL
            data.table::setnames(df, "value.x", "value")
            df$Start_Date <- as.character(df$Start_Date)
            
            # extract time
            tlu_time <- df %>% select(variable, value, Event_Site_ID) %>% subset(variable == "Time") # make a lookup table to grab sample times
            for(i in 1:nrow(tlu_time)){
                tlu_time$min[i] <- str_sub(tlu_time$value[i], nchar(tlu_time$value[i])-1, nchar(tlu_time$value[i]))
                tlu_time$hr[i] <- str_sub(tlu_time$value[i], 0, nchar(tlu_time$value[i])-2)
            }
            tlu_time$time <- paste0(tlu_time$hr, ":", tlu_time$min)
            df <- subset(df, variable != "Time")
            df <- dplyr::left_join(df, tlu_time %>% select(time, Event_Site_ID), by="Event_Site_ID")
            
            #----- build to match example
            real <- tibble::tibble(data.frame(matrix(ncol = ncol(example), nrow = nrow(df)))) # empty dataframe
            colnames(real) <- colnames(example) # name columns to match example
            
            real[1] <- "NCRN" # "#Org_Code" 
            real[2] <- "Biological Stream Survey - physical habitat inventory" # "Stream habitat inventory" # "Project_ID"
            real[3] <- df$NCRN_Site_ID # "Location_ID" shared field with `real_locations.Location_ID`
            # real[4] <- df$Activity_ID # "Activity_ID" shared field with `real_locations.Activity_ID` and `real_results.Activity_ID`
            real[4] <- paste0(df$NCRN_Site_ID, ".m.", format(as.Date(df$Start_Date), "%Y%m%d"))
            real[5] <- "Field Msr/Obs" # "Activity_Type"; choices are: 1) 'Field Msr/Obs' and 2) 'Sample-Routine'
            real[6] <- "Water" # "Medium"  choices are "Water", "Air", and "Other" in `example`
            real[7] <- NA # "Medium_Subdivision"
            real[8] <- "Stream habitat inventory" # "Assemblage_Sampled_Name"
            real[9] <- format(as.Date(df$Start_Date), "%Y-%m-%d") # "Activity_Start_Date"
            real[10] <- df$time # "Activity_Start_Time" 
            real[11] <- "Eastern Time - Washington, DC" # "Activity_Start_Time_Zone" 
            real[12] <- NA # "Activity_End_Date" 
            real[13] <- NA # "Activity_End_Time"
            real[14] <- NA # "Activity_End_Time_Zone"  
            real[15] <- NA # "Activity_Relative_Depth" 
            real[16] <- NA # "Activity_Depth"
            real[17] <- NA # "Activity_Depth_Unit"
            real[18] <- NA # "Activity_Upper_Depth"
            real[19] <- NA # "Activity_Lower_Depth"
            real[20] <- NA # "Activity_Depth_Reference"
            real[21] <- df$Loc_Name # "Additional_Location_Info"
            real[22] <- NA # "Activity_Sampler"; the person who did the sampling?
            real[23] <- NA # "Activity_Recorder"
            real[24] <- NA # "Custody_ID" 
            real[25] <- "NCRN" # "Activity_Conducting_Organization" 
            real[26] <- NA # "Station_Visit_Comment" 
            real[27] <- NA # "Activity_Comment
            real[28] <- NA # "Collection_Method_ID" 
            real[29] <- NA # Possibly a Smith Root LR-24 but not known; "Collection_Equipment_Name" 
            real[30] <- NA # Possibly a Smith Root LR-24 but not known; # "Collection_Equipment_Description" 
            real[31] <- NA # subset(results_list$tlu_Collection_Procedures_Gear_Config, `Field Gear Category` == "Smith Root LR-24")$`Field Procedure ID` # "Gear_Deployment"
            real[32] <- NA # "Container_Type"
            real[33] <- NA # "Container_Color"
            real[34] <- NA # "Container_Size"
            real[35] <- NA # "Container_Size_Unit"
            real[36] <- NA # "Preparation_Method_ID"
            real[37] <- NA # "Chemical_Preservative" # pdf pg 124 (135) https://doimspp.sharepoint.com/:b:/r/sites/NCRNBiologicalStreamSampling/Shared%20Documents/General/Operational%20Reviews/NCRN_Biological_Stream_Survey_Protocol_Ver_2.0_NRR.pdf?csf=1&web=1&e=u0kGN9
            real[38] <- NA # "Thermal_Preservative". Fish are preserved via chemicals, not wet ice
            real[39] <- NA # "Transport_Storage_Description" 
            real[40] <- NA # "Activity_Group_ID"
            real[41] <- df$Event_Site_ID # "Activity_Group_Name" 
            real[42] <- ifelse(is.na(real[41])==TRUE,
                               paste0("Activities for: ", df$NCRN_Site_ID),
                               NA)# "Activity_Group_Type"  
            real[43] <- NA # we don't record stop time for fish events e-fishing, so duration is unknown
            real[44] <- NA # we don't record stop time for fish events e-fishing, so duration unit is unknown
            real[45] <- "Abiotic stream habitat monitoring" # Sampling_Component_Name
            real[46] <- NA # Sampling_Component_Place_In_Series
            real[47] <- NA # "Reach_Length"
            real[48] <- NA# Reach_Length_Unit
            real[49] <- NA # "Reach_Width"
            real[50] <- NA # "Reach_Width_Unit" 
            real[51] <- NA#"Pass_Count"
            real[52] <- NA # "Net_Type"
            real[53] <- NA # "Net_Surface_Area"
            real[54] <- NA # "Net_Surface_Area_Unit"
            real[55] <- NA # "Net_Mesh_Size"
            real[56] <- NA # "Net_Mesh_Size_Unit"  
            real[57] <- NA # "Boat_Speed"
            real[58] <- NA # "Boat_Speed_Unit"
            real[59] <- NA # "Current_Speed"
            real[60] <- NA # "Current_Speed_Unit"
            real[61] <- NA # "Toxicity_Test_Type"
            real[62] <- NA # "Effort"
            real[63] <- NA # "Effort_Unit"
            real <- as.data.frame(lapply(real, function(y) gsub("\\<NA\\>", NA, y))) # remove "NA" chr strings
            colnames(real)[1] <- "#Org_Code"
            
            # error-checking:
            check_df <- tibble::tibble(data.frame(matrix(ncol=3, nrow=ncol(real))))
            colnames(check_df) <- c("acts", "example", "result")
            check_df$acts <- colnames(real)
            check_df$example <- colnames(example)
            for(i in 1:nrow(check_df)){
                if(check_df$acts[i] == check_df$example[i]){
                    check_df$result[i] <- "MATCH"
                } else {
                    check_df$result[i] <- "MISMATCH"
                }
            }
            
            message(
                if(length(check_df$result == "MATCH") == nrow(check_df)){
                    "`marc_2022_summer_index_activities()` executed successfully..."
                } else {
                    for(i in 1:length(check_df$result != "MATCH")){
                        cat(paste(paste0("`acts", check_df$acts[i], "`"), paste0(" DID NOT MATCH `example.", check_df$example[i][i], "`"), "\n", sep = ""))
                    }
                }
            )
            
            # assign("marc_habitat_activities", real, envir = globalenv())
            return(real)
        }
    )
}