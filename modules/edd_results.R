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
            #wrangle
            source("modules/wrangle/results_wrangle.R")
            
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
            
            #----- wrangle
            real <- results_wrangle(example, real)
            
            message("`edd.results` built successfully...")
            
            return(real)
        }
    )
}