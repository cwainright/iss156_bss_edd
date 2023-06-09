# a module for `edd_results()`
# ETL results_list into edd.results
options(warn=-1)
ncrn_fish_results <- function(results_list, example){
    tryCatch(
        expr = {
            #----- load external libraries
            suppressWarnings(suppressMessages(library(data.table)))
            suppressWarnings(suppressMessages(library(tidyverse)))
            suppressWarnings(suppressMessages(library(dplyr)))
            suppressWarnings(suppressMessages(library(readxl)))
            
            # make a flat dataframe from `results_list`
            df <- results_list$tbl_Fish_Data
            df$Characteristic_Name <- "Fish count by species"
            df$Result_Text <- df$Total_Pass_1
            df$Result_Unit <- "count of individuals"
            df <- dplyr::left_join(df, results_list$tlu_Fish %>% select(Latin_Name, Common_Name), by = c("Fish_Species" = "Latin_Name"))
            df <- rename(df, species = Fish_Species)
            df$Result_Qualifier <- "Electrofishing pass 1"
            # df$Fish_Event_ID2 <- paste0(df$Fish_Event_ID, ",", "pass_1")
            
            df2 <- results_list$tbl_Fish_Data
            df2$Characteristic_Name <- "Fish count by species"
            df2$Result_Text <- df$Total_Pass_2
            df2$Result_Unit <- "count of individuals"
            df2 <- dplyr::left_join(df2, results_list$tlu_Fish %>% select(Latin_Name, Common_Name), by = c("Fish_Species" = "Latin_Name"))
            df2 <- rename(df2, species = Fish_Species)
            df2$Result_Qualifier <- "Electrofishing pass 2"
            # df2$Fish_Event_ID2 <- paste0(df2$Fish_Event_ID, ",", "pass_2")
            
            df <- rbind(df, df2)
            
            df2 <- results_list$tbl_GameFish
            df2$Characteristic_Name <- "Individual fish total length in mm"
            df2$Result_Text <- df2$LENGTH
            df2$Result_Unit <- "mm"
            results_list$tlu_Fish$Common_Name <- trimws(results_list$tlu_Fish$Common_Name, which=c("both"))
            df2$SPECIES <- toupper(df2$SPECIES)
            df2 <- dplyr::left_join(df2, results_list$tlu_Fish %>% select(Common_Name, Latin_Name), by = c("SPECIES" = "Common_Name"))
            df2 <- rename(df2, species = Latin_Name)
            df2 <- rename(df2, Common_Name = SPECIES)
            df2$Comments <- NA
            df2$Result_Qualifier <- NA
            
            combined <- rbind(
                df %>% select(Fish_Event_ID, Characteristic_Name, species, Common_Name, Result_Text, Result_Unit, Comments, Result_Qualifier),
                df2 %>% select(Fish_Event_ID, Characteristic_Name, species, Common_Name, Result_Text, Result_Unit, Comments, Result_Qualifier)
            )
            combined$Common_Name <- tolower(combined$Common_Name)
            combined <- dplyr::left_join(combined, results_list$tbl_Fish_Events %>% select(Fish_Event_ID, Event_ID), by = "Fish_Event_ID")
            combined <- dplyr::left_join(combined, results_list$tbl_Events %>% select(Event_ID, Start_Date, Start_Time, Location_ID), by = "Event_ID")
            combined <- dplyr::left_join(combined, results_list$tbl_Locations %>% select(Location_ID, Loc_Name), by = "Location_ID")
            combined$Result_Qualifier <- paste0(combined$Start_Date, ";", combined$Result_Qualifier)
            
            #----- re-build `example` from `results_list`
            real <- tibble::tibble(data.frame(matrix(ncol = ncol(example), nrow = nrow(combined)))) # empty dataframe
            colnames(real) <- colnames(example) # name columns to match example
            
            real[1] <- "NCRN" # "#Org_Code" 
            real[2] <- combined$Event_ID # "Activity_ID" shared field with `real_activities.Activity_ID`
            real[3] <- combined$Characteristic_Name# "Characteristic_Name"  
            real[4] <- combined$Common_Name # "Method_Speciation"
            real[5] <- NA # "Filtered_Fraction"
            real[6] <- NA # "Result_Detection_Condition"
            real[7] <- combined$Result_Text # "Result_Text"
            real[8] <- combined$Result_Unit# "Result_Unit"
            real[9] <- combined$Result_Qualifier # "Result_Qualifier"
            real[10] <- "Final" # "Result_Status" 
            real[11] <- "Actual" # "Result_Type" 
            real[12] <- combined$Comments # "Result_Comment" 
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
            real[27] <- combined$Loc_Name# "Result_Sampling_Point_Name"
            real[28] <- NA # "Result_Depth_Height_Measure"
            real[29] <- NA # "Result_Depth_Height_Measure_Unit" 
            real[30] <- NA # "Result_Depth_Altitude_Reference_Point"
            real[31] <- NA # "Analytical_Method_ID"
            real[32] <- NA # "Analytical_Remark"
            real[33] <- NA # "Lab_ID"
            real[34] <- NA # "Lab_Remark_Code"
            real[35] <- format(combined$Start_Date, "%Y-%m-%d") # "Analysis_Start_Date"
            real[36] <- format(combined$Start_Time, "%H:%M") # "Analysis_Start_Time" 
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
            real[50] <- combined$species# "Subject_Taxon"
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
            real[89] <- "tbl_Fish_Data; tbl_GameFish" # "Result_File_Name"
            
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
                    "`ncrn_fish_results()` executed successfully..."
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