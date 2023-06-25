# A module called from make_edd.R
# Transpose source data into edd.Activities records
options(warn=-1)
edd_activities <- function(
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
            source("modules/marc/habitat/marc_2022_summer_index_activities.R")
            source("modules/marc/habitat/marc_2022_summer_exotic_activities.R")
            source("modules/marc/habitat/marc_2022_flow_activities.R")
            #npstoret
            source("modules/npstoret/macroinvertebrates/npstoret_macroinvertebrates.R")
            #wrangle
            source("modules/wrangle/activities_wrangle.R")
            
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
            bob_2022_macroinvert_activities <- bob_2022_macroinvert_activities(results_list, bob_2022_macroinvert, bob_2022_wq, example)# bob's water chem and macroinvert samples are the same locations
            bob_2021_chemistry_activities <- bob_2021_chemistry_activities(results_list, bob_2021_water_chem, example)
            bob_2022_chemistry_activities <- bob_2022_chemistry_activities(results_list, bob_2022_wq, example)
            bob_2022_habitat_activities <- bob_2022_habitat_activities(results_list, bob_2022_hab, example)
            
            #----- marc.edd.activities
            marc_2022_fish_activities <- marc_2022_fish_activities(marc2022, example) # marc's 2021 fish data are in db.tbl_Fish_Data
            marc_habitat_activities <- marc_habitat_activities(habitat_marc2022, habitat_marc2021, example, results_list) # marc's 2021 fish activities are not in db.tbl_Fish_Data
            marc_2022_summer_index_activities <- marc_2022_summer_index_activities(summer_index_marc2022, example, results_list)
            marc_2022_summer_exotic_activities <- marc_2022_summer_exotic_activities(summer_exotic_marc2022, example, results_list)
            marc_2022_flow_activities <- marc_2022_flow_activities(summer_flow_marc2022, example, results_list)
            
            #----- npstoret.edd.activities
            npstoret_activities <- npstoret_macroinvertebrates_activities()
            
            #----- combine edd.activities
            real <- rbind(
                ncrn_benthic_habitat_activities
                ,ncrn_chemistry_activities
                ,ncrn_fish_activities
                ,ncrn_habitat_activities
                ,ncrn_macroinvert_activities
                ,bob_2021_macroinvert_activities
                ,bob_2022_macroinvert_activities
                ,bob_2021_chemistry_activities
                ,bob_2022_habitat_activities
                ,marc_2022_fish_activities
                ,marc_habitat_activities
                ,marc_2022_summer_index_activities
                ,marc_2022_summer_exotic_activities
                ,marc_2022_flow_activities
                ,npstoret_activities
                )
            
            
            #----- wrangle
            real <- activities_wrangle(example, real)
            
            message("\n`edd.activities` built successfully...\n")
            
            return(real)
        }
    )
}