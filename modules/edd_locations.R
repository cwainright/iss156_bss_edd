# A module called from make_edd.R
# Transpose source data into edd.Locations records
options(warn=-1)
edd_locations <- function(
        results_list
        ,marc2022
        ,marc2021
        ,habitat_marc2021
        ,habitat_marc2022
        ,summer_index_marc2022
        ,summer_exotic_marc2022
        ,summer_flow_marc2022
        ,bob_2021_macroinvert
        ,bob_2021_water_chem
        ,bob_2022_wq
        ,bob_2022_macroinvert
        ,bob_2022_hab
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
            source("modules/ncrn/benthic_habitat/ncrn_benthic_habitat_locations.R")
            source("modules/ncrn/chemistry/ncrn_chemistry_locations.R")
            source("modules/ncrn/fish/ncrn_fish_locations.R")
            source("modules/ncrn/habitat/ncrn_habitat_locations.R")
            source("modules/ncrn/macroinvertebrates/ncrn_macroinvertebrates_locations.R")
            # bob
            source("modules/bob/macroinvertebrates/bob_2021_macroinvertebrates_locations.R")
            source("modules/bob/macroinvertebrates/bob_2022_macroinvertebrates_locations.R")
            source("modules/bob/chemistry/bob_2021_chemistry_locations.R")
            source("modules/bob/chemistry/bob_2022_chemistry_locations.R")
            source("modules/bob/habitat/bob_2022_habitat_locations.R")
            #marc
            source("modules/marc/fish/marc_2022_fish_locations.R") # Marc's 2021 fish data are already in ncrn db, so only add 2022
            source("modules/marc/habitat/marc_habitat_locations.R")
            source("modules/marc/habitat/marc_2022_summer_index_locations.R")
            source("modules/marc/habitat/marc_2022_summer_exotic_locations.R")
            source("modules/marc/habitat/marc_2022_flow_locations.R")
            #wrangle
            source("modules/wrangle/locations_wrangle.R")
            
            #----- load template
            example <- readxl::read_excel("template/NCRN_BSS_EDD_20230105_1300.xlsx", sheet = "Locations") # https://doimspp.sharepoint.com/:x:/r/sites/NCRNDataManagement/Shared%20Documents/General/Standards/Data-Standards/EQuIS-WQX-EDD/NCRN_BSS_EDD_20230105_1300.xlsx?d=w8c283fde9cbd4af480945c8c8bd94ff6&csf=1&web=1&e=7Y9W1M
            
            #----- ncrn.edd.locations
            ncrn_benthic_habitat_locations <- ncrn_benthic_habitat_locations(results_list, example)
            ncrn_chemistry_locations <- ncrn_chemistry_locations(results_list, example)
            ncrn_fish_locations <- ncrn_fish_locations(results_list, example)
            ncrn_habitat_locations <- ncrn_habitat_locations(results_list, example)
            ncrn_macroinvert_locations <- ncrn_macroinvert_locations(results_list, example)
            
            #----- bob.edd.locations
            bob_2021_macroinvert_locations <- bob_2021_macroinvert_locations(results_list, bob_2021_water_chem, example)# bob's water chem and macroinvert samples are the same locations
            bob_2022_macroinvert_locations <- bob_2022_macroinvert_locations(results_list, bob_2022_macroinvert, example)# bob's water chem and macroinvert samples are the same locations
            bob_2021_chemistry_locations <- bob_2021_chemistry_locations(results_list, bob_2021_water_chem, example)
            bob_2022_chemistry_locations <- bob_2022_chemistry_locations(results_list, bob_2022_wq, example)
            bob_2022_habitat_locations <- bob_2022_habitat_locations(results_list, bob_2022_hab, example)
            
            #----- marc.edd.locations
            marc_2022_fish_locations <- marc_2022_fish_locations(marc2022, example) # marc's 2021 fish data are in db.tbl_Fish_Data
            marc_habitat_locations <- marc_habitat_locations(habitat_marc2022, habitat_marc2021, example, results_list) # marc's 2021 fish locations are not in db.tbl_Fish_Data
            marc_2022_summer_index_locations <- marc_2022_summer_index_locations(summer_index_marc2022, example, results_list)
            marc_2022_summer_exotic_locations <- marc_2022_summer_exotic_locations(summer_exotic_marc2022, example, results_list)
            marc_2022_flow_locations <- marc_2022_flow_locations(summer_flow_marc2022, example, results_list)
            
            #----- combine edd.locations
            real <- rbind(
                ncrn_benthic_habitat_locations
                ,ncrn_chemistry_locations
                ,ncrn_fish_locations
                ,ncrn_habitat_locations
                ,ncrn_macroinvert_locations
                ,bob_2021_macroinvert_locations
                ,bob_2022_macroinvert_locations
                ,bob_2021_chemistry_locations
                ,bob_2022_habitat_locations
                ,marc_2022_fish_locations
                ,marc_habitat_locations
                ,marc_2022_summer_index_locations
                ,marc_2022_summer_exotic_locations
                ,marc_2022_flow_locations
            )
            
            #----- keep only unique locations
            real <- real %>% distinct(Location_ID, .keep_all = TRUE)
            
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
                    "`edd_locations()` executed successfully..."
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