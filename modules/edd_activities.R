# A module called from make_edd.R
# Transpose source data into edd.Activities records
options(warn=-1)
edd_activities <- function(
        results_list
        ,marc2022
        ,marc2021
        ,habitat_marc2021
        ,habitat_marc2022
        ,bob_2021_macroinvert
        ,bob_2021_water_chem
        ,bob_2022_wq
        ,bob_2022_macroinvert
       ){
    tryCatch(
        expr = {
            #----- load external libraries
            suppressWarnings(suppressMessages(library(data.table)))
            suppressWarnings(suppressMessages(library(tidyverse)))
            suppressWarnings(suppressMessages(library(dplyr)))
            suppressWarnings(suppressMessages(library(readxl)))
            
            #----- load project functions
            # ncrn
            source("modules/ncrn/benthic_habitat/ncrn_benthic_habitat_activities.R")
            source("modules/ncrn/chemistry/ncrn_chemistry_activities.R")
            source("modules/ncrn/fish/ncrn_fish_activities.R")
            source("modules/ncrn/habitat/ncrn_habitat_activities.R")
            source("modules/ncrn/macroinvertebrates/ncrn_macroinvertebrates_activities.R")
            # bob
            source("modules/bob/macroinvertebrates/bob_2021_macroinvertebrates_activities.R")
            source("modules/bob/macroinvertebrates/bob_2022_macroinvertebrates_activities.R")
            source("modules/bob/chemistry/bob_2021_chemistry_activities.R")
            source("modules/bob/chemistry/bob_2022_chemistry_activities.R")
            source("modules/bob/habitat/bob_2022_habitat_activities.R")
            #marc
            source("modules/marc/fish/marc_2022_fish_activities.R") # Marc's 2021 fish data are already in ncrn db, so only add 2022
            source("modules/marc/habitat/marc_habitat_activities.R")
            
            #----- load template
            example <- readxl::read_excel("template/NCRN_BSS_EDD_20230105_1300.xlsx", sheet = "Activities") # https://doimspp.sharepoint.com/:x:/r/sites/NCRNDataManagement/Shared%20Documents/General/Standards/Data-Standards/EQuIS-WQX-EDD/NCRN_BSS_EDD_20230105_1300.xlsx?d=w8c283fde9cbd4af480945c8c8bd94ff6&csf=1&web=1&e=7Y9W1M
            
            #----- ncrn.edd.activities
            ncrn_benthic_habitat_activities <- ncrn_benthic_habitat_activities(results_list, example)
            ncrn_chemistry_activities <- ncrn_chemistry_activities(results_list, example)
            ncrn_fish_activities <- ncrn_fish_activities(results_list, example)
            ncrn_habitat_activities <- ncrn_habitat_activities(results_list, example)
            ncrn_macroinvert_activities <- ncrn_macroinvert_activities(results_list, example)
            
            #----- bob.edd.activities
            bob_2021_macroinvert_activities <- bob_2021_macroinvert_activities(results_list, bob_2021_water_chem, example)# bob's water chem and macroinvert samples are the same locations
            bob_2022_macroinvert_activities <- bob_2022_macroinvert_activities(results_list, bob_2022_macroinvert, example)# bob's water chem and macroinvert samples are the same locations
            bob_2021_chemistry_activities <- bob_2021_chemistry_activities(results_list, bob_2021_water_chem, example)
            bob_2022_chemistry_activities <- bob_2022_chemistry_activities(results_list, bob_2022_wq, example)
            bob_2022_habitat_activities <- bob_2022_habitat_activities(results_list, bob_2022_hab, example)
            
            #----- marc.edd.activities
            marc_2022_fish_activities <- marc_2022_fish_activities(marc2022, example) # marc's 2021 fish data are in db.tbl_Fish_Data
            marc_habitat_activities <- marc_habitat_activities(habitat_marc2022, habitat_marc2021, example, results_list) # marc's 2021 fish activities are not in db.tbl_Fish_Data
            
            #----- combine edd.activities
            real <- rbind(
                ncrn_benthic_habitat_activities,
                ncrn_chemistry_activities,
                ncrn_fish_activities,
                ncrn_habitat_activities,
                ncrn_macroinvert_activities,
                bob_2021_macroinvert_activities,
                bob_2022_macroinvert_activities,
                bob_2021_chemistry_activities,
                bob_2022_habitat_activities,
                marc_2022_fish_activities,
                marc_habitat_activities
                )
            
            #----- keep only unique activities
            real <- real %>% distinct(Activity_ID, .keep_all = TRUE)
            
            #----- error-checking
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
                    "`edd_activities()` executed successfully..."
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