# a module for `edd_results()`
# ETL results_list into edd.results
options(warn=-1)
ncrn_chemistry_results <- function(results_list, example){
    tryCatch(
        expr = {
            #----- load external libraries
            suppressWarnings(suppressMessages(library(data.table)))
            suppressWarnings(suppressMessages(library(tidyverse)))
            suppressWarnings(suppressMessages(library(dplyr)))
            suppressWarnings(suppressMessages(library(readxl)))
            
            # make a flat dataframe from `results_list`
            df <- results_list$tbl_Chemistry_Data
            df <- df %>% select(-c("Sampleability_WQ"))
            id_vars <- c("Chem_Data_ID", "Event_ID", "Notes") # the column(s) we want to keep as columns
            measure_vars <- colnames(df %>% select(c(3:ncol(df)))) # the columns we want to melt into `value` and `variable` columns
            df <- melt(data.table::setDT(df), id.vars = id_vars,
                       measure.vars = measure_vars)
            df <- df %>% subset(variable!="Notes") # notes is already a column
            data.table::setnames(df, "value", "Result_Text")
            # df$Characteristic_Name <- "Stream water chemistry"
            # make a lookup table for units and human-readable explanations of $variable
            # from design view of `tbl_Chemistry_Data`
            unit_lookup <- data.frame(variable = unique(df$variable),
                                      Characteristic_Name = c("closed pH value",
                                                            "acid neutralizing capacity",
                                                            "chloride concentration",
                                                            "nitrate nitrogen concentration",
                                                            "sulfate concentration",
                                                            "ortho-phosphate concentration",
                                                            "nitrite nitrogen concentration",
                                                            "ammonia concentration",
                                                            "dissolved organic carbon concentration",
                                                            "source of any acidity",
                                                            "total nitrogen concentration",
                                                            "total phosphorus concentration",
                                                            "specific conductance"),
                                      Result_Unit = c("pH",
                                                      "ueq/L",
                                                      "mg/L",
                                                      "mg/L",
                                                      "mg/L",
                                                      "mg/L",
                                                      "mg/L",
                                                      "mg/L",
                                                      "mg/L",
                                                      NA,
                                                      "mg/L",
                                                      "mg/L",
                                                      "uS/cm"))
            df <- dplyr::left_join(df, unit_lookup, by=c("variable"))
            # df$Result_Text <- as.numeric(df$Result_Text)
            # fix order of magnitude in df$variable=="cond_lab"
            for(i in 1:nrow(df)){
                df$Result_Text[i] <- ifelse(df$variable[i]=="cond_lab",
                                            ifelse(as.numeric(df$Result_Text[i])<1,
                                                   as.numeric(df$Result_Text[i])*1000,
                                                   df$Result_Text[i]),
                                            df$Result_Text[i])
            }
            df <- dplyr::left_join(df, results_list$tbl_Events %>% select(Event_ID, Start_Date, Start_Time, Location_ID), by = "Event_ID")
            df <- dplyr::left_join(df, results_list$tbl_Locations %>% select(Location_ID, Loc_Name), by = "Location_ID")
            
            #----- re-build `example` from `results_list`
            real <- tibble::tibble(data.frame(matrix(ncol = ncol(example), nrow = nrow(df)))) # empty dataframe
            colnames(real) <- colnames(example) # name columns to match example
            
            real[1] <- "NCRN" # "#Org_Code" 
            real[2] <- df$Event_ID # "Activity_ID" shared field with `real_activities.Activity_ID`
            real[3] <- df$Characteristic_Name# "Characteristic_Name"  
            real[4] <- NA # df$Method_Speciation # "Method_Speciation"
            real[5] <- NA # "Filtered_Fraction"
            real[6] <- NA # "Result_Detection_Condition"
            real[7] <- df$Result_Text # "Result_Text"
            for(i in 1:nrow(real)){
                if(!is.na(real$Result_Text[i])){
                    real$Result_Unit[i] <- df$Result_Unit[i] # "Result_Unit"
                }
            }
            real[9] <- NA # "Result_Qualifier"
            real[10] <- "Final" # "Result_Status" 
            real[11] <- "Actual" # "Result_Type" 
            real[12] <- df$Notes # "Result_Comment" 
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
            real[31] <- NA # "Analytical_Method_ID"
            real[32] <- NA # "Analytical_Remark"
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
            real[89] <- "tbl_Chemistry_Data" # "Result_File_Name"
            
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
                    "`ncrn_chemistry_results()` executed successfully..."
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
