# a module for `edd_results()`
# ETL results_list into edd.results
options(warn=-1)
ncrn_benthic_habitat_results <- function(results_list, example){
    tryCatch(
        expr = {
            #----- load external libraries
            suppressWarnings(suppressMessages(library(data.table)))
            suppressWarnings(suppressMessages(library(tidyverse)))
            suppressWarnings(suppressMessages(library(dplyr)))
            suppressWarnings(suppressMessages(library(readxl)))
            
            # make a flat dataframe from `results_list`
            df <- results_list$tbl_Benthic_Habitat
            othben <- df %>% select(Event_ID, OTHTYPE)
            df$Total_Area <- NULL # this is a calculated value, so it belongs in a summary, not a data table.
            df$OTHTYPE <- NULL
            df$SMP_BEN <- NULL
            id_vars <- c("Event_ID") # the column(s) we want to keep as columns
            measure_vars <- colnames(df %>% select(-c(Event_ID))) # the columns we want to melt into `value` and `variable` columns
            df <- melt(data.table::setDT(df), id.vars = id_vars,
                       measure.vars = measure_vars)
            df <- dplyr::left_join(df, othben, by="Event_ID")
            unit_lookup <- data.frame(field=unique(df$variable))
            unit_lookup$unit <-c(
                "Numeric. Square feet of riffle sampled for benthos",
                "Numeric. Square feet of root wad sampled for benthos",
                "Numeric. Square feet of leaf pack smapled for benthos",
                "Numeric. Square feet of macrophytes sampled for benthos",
                "Numeric. Square feet of undercut bank sampled for benthos",
                "Numeric. Square feet of 'other habitats' sampled for benthos. Description of 'other habitat' in $Analytical_Remark"
            )
            unit_lookup$Characteristic_Name <- c("riffle sampled for benthos",
                                               "root wad sampled for benthos",
                                               "leaf pack smapled for benthos",
                                               "macrophytes sampled for benthos",
                                               "undercut bank sampled for benthos",
                                               "'other habitats' sampled for benthos")
            
            df <- dplyr::left_join(df, unit_lookup, by=c("variable" = "field"))
            # join taxonomic values
            df <- dplyr::left_join(df, results_list$tbl_Events %>% select(Event_ID, Start_Date, Start_Time, Location_ID, Comments), by = "Event_ID")
            df <- dplyr::left_join(df, results_list$tbl_Locations %>% select(Location_ID, Loc_Name), by = "Location_ID")
            # df$Characteristic_Name <- "Stream habitat inventory"
            
            #----- re-build `example` from `results_list`
            real <- tibble::tibble(data.frame(matrix(ncol = ncol(example), nrow = nrow(df)))) # empty dataframe
            colnames(real) <- colnames(example) # name columns to match example
            
            real[1] <- "NCRN" # "#Org_Code" 
            real[2] <- df$Event_ID # "Activity_ID" shared field with `real_activities.Activity_ID`
            real[3] <- df$Characteristic_Name # "Characteristic_Name"  
            real[4] <- NA # df$Method_Speciation # "Method_Speciation"
            real[5] <- NA # "Filtered_Fraction"
            real[6] <- NA # "Result_Detection_Condition"
            real[7] <- df$value # "Result_Text"
            real[8] <- df$unit# "Result_Unit"
            real[9] <- NA # "Result_Qualifier"
            real[10] <- "Final" # "Result_Status" 
            real[11] <- "Actual" # "Result_Type" 
            real[12] <- df$Comments # "Result_Comment" 
            real[13] <- NA # "Method_Detection_Limit"
            real[14] <- NA # "Lower_Quantification_Limit"
            real[15] <- NA # "Upper_Quantification_Limit" 
            real[16] <- NA # "Limit_Comment"
            real[17] <- NA # "Temperature_Basis"
            real[18] <- NA # "Statistical_Basis"
            real[19] <- NA # "Time_Basis" 
            real[20] <- NA # "Weight_Basis"
            real[21] <- NA # "Particle_Size_Basis"
            real[22] <- NA # "Precision"
            real[23] <- NA # "Bias"
            real[24] <- NA # "Confidence_Interval" 
            real[25] <- NA # "Upper_Confidence_Limit" 
            real[26] <- NA # "Lower_Confidence_Limit" 
            real[27] <- df$Loc_Name# "Result_Sampling_Point_Name"
            real[28] <- NA # "Result_Depth_Height_Measure"
            real[29] <- NA # "Result_Depth_Height_Measure_Unit" 
            real[30] <- NA # "Result_Depth_Altitude_Reference_Point"
            real[31] <- df$variable # "Analytical_Method_ID"
            real[32] <- df$OTHTYPE # "Analytical_Remark"
            real[33] <- NA # "Lab_ID"
            real[34] <- NA # "Lab_Remark_Code"
            real[35] <- format(df$Start_Date, "%Y-%m-%d") # "Analysis_Start_Date"
            real[36] <- format(df$Start_Time, "%H:%M") # "Analysis_Start_Time" 
            real[37] <- "Eastern Time - Washington, DC" # "Analysis_Start_Time_Zone"
            real[38] <- NA # "Lab_Accreditation_Indicator"
            real[39] <- NA # "Lab_Accreditation_Authority_Name" 
            real[40] <- NA # "Lab_Batch_ID"
            real[41] <- NA # "Lab_Sample_Preparation_ID" 
            real[42] <- NA # "Lab_Sample_Preparation_Start_Date"  
            real[43] <- NA # "Lab_Sample_Preparation_Start_Time"
            real[44] <- NA # "Lab_Sample_Preparation_Start_Time_Zone" 
            real[45] <- NA # "Dilution_Factor"
            real[46] <- NA # "Num_of_Replicates"
            real[47] <- NA # "Data_Logger_Line_Name"
            real[48] <- NA # "Biological_Intent"
            real[49] <- NA # "Biological_Individual_ID"
            real[50] <- NA # "Subject_Taxon"
            real[51] <- NA # "Unidentified_Species_ID"
            real[52] <- NA # "Tissue_Anatomy"
            real[53] <- NA # "Group_Summary_Count_or_Weight"
            real[54] <- NA # "Group_Summary_Count_or_Weight_Unit"
            real[55] <- NA # "Cell_Form"
            real[56] <- NA # "Cell_Shape"  
            real[57] <- NA # "Habit_Name_1"
            real[58] <- NA # "Habit_Name_2"
            real[59] <- NA # "Habit_Name_3"
            real[60] <- NA # "Voltinism"
            real[61] <- NA # "Pollution_Tolerance"
            real[62] <- NA # "Pollution_Tolerance_Scale"
            real[63] <- NA # "Trophic_Level"
            real[64] <- NA # "Functional_Feeding_Group_1"
            real[65] <- NA # "Functional_Feeding_Group_2"
            real[66] <- NA # "Functional_Feeding_Group_3"
            real[67] <- NA # "Resource_ID"
            real[68] <- NA # "Resource_Date"
            real[69] <- NA # "Resource_Title_Name"
            real[70] <- NA # "Resource_Creator_Name"
            real[71] <- NA # "Resource_Publisher_Name"
            real[72] <- NA # "Resource_Publication_Year"
            real[73] <- NA # "Resource_Volume_Pages"
            real[74] <- NA # "Resource_Subject_Text"
            real[75] <- NA # "Frequency_Class_Descriptor_1"
            real[76] <- NA # "Frequency_Class_Bounds_Unit_1"
            real[77] <- NA # "Frequency_Class_Lower_Bound_1"
            real[78] <- NA # "Frequency_Class_Upper_Bound_1"
            real[79] <- NA # "Frequency_Class_Descriptor_2"
            real[80] <- NA # "Frequency_Class_Bounds_Unit_2"
            real[81] <- NA # "Frequency_Class_Lower_Bound_2"
            real[82] <- NA # "Frequency_Class_Upper_Bound_2"
            real[83] <- NA # "Frequency_Class_Descriptor_3"
            real[84] <- NA # "Frequency_Class_Bounds_Unit_3"
            real[85] <- NA # "Frequency_Class_Lower_Bound_3"
            real[86] <- NA # "Frequency_Class_Upper_Bound_3"
            real[87] <- NA # "Taxonomist_Accreditation_Indicator"
            real[88] <- NA # "Taxonomist_Accreditation_Authority_Name"
            real[89] <- "tbl_Benthic_Habitat" # "Result_File_Name"
            
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
                    "`ncrn_benthic_habitat_results()` executed successfully..."
                } else {
                    for(i in 1:length(check_df$result != "MATCH")){
                        cat(paste(paste0("`real.", check_df$real[i], "`"), paste0(" DID NOT MATCH `example.", check_df$example[i][i], "`"), "\n", sep = ""))
                    }
                }
            )
            return(real)
        }
    )
}