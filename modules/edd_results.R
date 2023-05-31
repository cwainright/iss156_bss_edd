# a module for `make_edd.R`
# ETL
options(warn=-1)
edd_results <- function(
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
            source("modules/ncrn/benthic_habitat/ncrn_benthic_habitat_results.R")
            source("modules/ncrn/chemistry/ncrn_chemistry_results.R")
            source("modules/ncrn/fish/ncrn_fish_results.R")
            source("modules/ncrn/habitat/ncrn_habitat_results.R")
            source("modules/ncrn/macroinvertebrates/ncrn_macroinvertebrates_results.R")
            # bob
            source("modules/bob/macroinvertebrates/bob_2021_macroinvertebrates_results.R")
            source("modules/bob/macroinvertebrates/bob_2022_macroinvertebrates_results.R")
            source("modules/bob/chemistry/bob_2021_chemistry_results.R")
            source("modules/bob/chemistry/bob_2022_chemistry_results.R")
            source("modules/bob/habitat/bob_2022_habitat_results.R")
            #marc
            source("modules/marc/fish/marc_2022_fish_results.R") # Marc's 2021 fish data are already in ncrn db, so only add 2022
            source("modules/marc/habitat/marc_habitat_results.R")
            source("modules/marc/habitat/marc_2022_summer_index_results.R")
            source("modules/marc/habitat/marc_2022_summer_exotic_results.R")
            source("modules/marc/habitat/marc_2022_flow_results.R")
            
            #----- load template
            example <- readxl::read_excel("template/NCRN_BSS_EDD_20230105_1300.xlsx", sheet = "Results") # https://doimspp.sharepoint.com/:x:/r/sites/NCRNDataManagement/Shared%20Documents/General/Standards/Data-Standards/EQuIS-WQX-EDD/NCRN_BSS_EDD_20230105_1300.xlsx?d=w8c283fde9cbd4af480945c8c8bd94ff6&csf=1&web=1&e=7Y9W1M
            
            #----- ncrn.edd.results
            ncrn_benthic_habitat_results <- ncrn_benthic_habitat_results(results_list, example)
            ncrn_chemistry_results <- ncrn_chemistry_results(results_list, example)
            ncrn_fish_results <- ncrn_fish_results(results_list, example)
            ncrn_habitat_results <- ncrn_habitat_results(results_list, example)
            ncrn_macroinvert_results <- ncrn_macroinvert_results(results_list, example)
            
            #----- bob.edd.results
            bob_2021_macroinvert_results <- bob_2021_macroinvert_results(results_list, bob_2021_macroinvert, bob_2021_water_chem, example)# bob's water chem and macroinvert samples are the same locations
            bob_2022_macroinvert_results <- bob_2022_macroinvert_results(results_list, bob_2022_macroinvert, example)# bob's water chem and macroinvert samples are the same locations
            bob_2021_chemistry_results <- bob_2021_chemistry_results(results_list, bob_2021_water_chem, example)
            bob_2022_chemistry_results <- bob_2022_chemistry_results(results_list, bob_2022_wq, example)
            bob_2022_habitat_results <- bob_2022_habitat_results(results_list, bob_2022_hab, example)
            
            #----- marc.edd.results
            marc_2022_fish_results <- marc_2022_fish_results(marc2022, results_list, example) # marc's 2021 fish data are in db.tbl_Fish_Data
            marc_habitat_results <- marc_habitat_results(habitat_marc2022, habitat_marc2021, example, results_list) # marc's 2021 fish results are not in db.tbl_Fish_Data
            marc_2022_summer_index_results <- marc_2022_summer_index_results(summer_index_marc2022, example, results_list)
            marc_2022_summer_exotic_results <- marc_2022_summer_exotic_results(summer_exotic_marc2022, example, results_list)
            marc_2022_flow_results <- marc_2022_flow_results(summer_flow_marc2022, example, results_list)
            
            #----- combine edd.results
            real <- rbind(
                ncrn_benthic_habitat_results
                ,ncrn_chemistry_results
                ,ncrn_fish_results
                ,ncrn_habitat_results
                ,ncrn_macroinvert_results
                ,bob_2021_macroinvert_results
                ,bob_2022_macroinvert_results
                ,bob_2021_chemistry_results
                ,bob_2022_habitat_results
                ,marc_2022_fish_results
                ,marc_habitat_results
                ,marc_2022_summer_index_results
                ,marc_2022_summer_exotic_results
                ,marc_2022_flow_results
            )
            
            #----- unify
            real$Characteristic_Name <- tolower(real$Characteristic_Name)
            real$Characteristic_Name <- gsub("comments2", "comments", real$Characteristic_Name)
            real$Characteristic_Name <- gsub("comments1", "comments", real$Characteristic_Name)
            real$Characteristic_Name <- gsub("_", " ", real$Characteristic_Name)
            real$Characteristic_Name <- gsub("wetted width of stream at bottom of 75m sampling reach", "wetted channel width (m) at bottom (0m) of site", real$Characteristic_Name)
            real$Characteristic_Name <- gsub("wetted width of stream at top of 75m sampling reach", "wetted channel width (m) at top (75m) of site", real$Characteristic_Name)
            real$Characteristic_Name <- gsub("riparian vegetation width on left bank", "left bank riparian width (m)", real$Characteristic_Name)
            real$Characteristic_Name <- gsub("riparian vegetation width on right bank", "right bank riparian width (m)", real$Characteristic_Name)
            real$Characteristic_Name <- gsub("right bank concrete", "presence of concrete on right bank", real$Characteristic_Name)
            real$Characteristic_Name <- gsub("left bank concrete", "presence of concrete on left bank", real$Characteristic_Name)
            real <- subset(real, real$Characteristic_Name != "8 digit watershed code for sampled site") # this is not a result; it's a datum associated with the location and is provided in edd.locations
            
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
                    "`edd_results()` executed successfully..."
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