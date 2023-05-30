# A module called from main.R
# Load and process data source files
options(warn=-1)
make_edd <- function(write, db){
    tryCatch(
        expr = {
            #----- load external libraries
            suppressWarnings(suppressMessages(library(openxlsx)))
            suppressWarnings(suppressMessages(library(data.table)))
            suppressWarnings(suppressMessages(library(tidyverse)))
            
            #----- load project functions
            source("modules/edd_locations.R")
            source("modules/edd_activities.R")
            source("modules/edd_results.R")
            source("modules/ncrn/query_db.R") # source() == python "import"
            
            #----- read and pre-process static assets
            # marc
            # https://doimspp.sharepoint.com/:f:/r/sites/NCRNBiologicalStreamSampling/Shared%20Documents/General/Annual-Data-Packages/2022/Marc_and_Bob/Fish_Template?csf=1&web=1&e=beb0mc
            marc2021 <- readxl::read_excel("data/marc/ncrn_bss_fish_monitoring_data_2021_marc.xlsx", sheet = "Fish Data (Individuals)") # https://doimspp.sharepoint.com/:x:/r/sites/NCRNBiologicalStreamSampling/Shared%20Documents/General/Annual-Data-Packages/2022/Marc_and_Bob/Fish_Template/NCRN_BSS_Fish_Monitoring_Data_2021_Marc.xlsx?d=w0ccc6d38b831430185bbbf15488bb366&csf=1&web=1&e=FJUBeO
            marc2022 <- readxl::read_excel("data/marc/ncrn_bss_fish_monitoring_data_2022_marc.xlsx", sheet = "ElectrofishingData") # https://doimspp.sharepoint.com/:x:/r/sites/NCRNBiologicalStreamSampling/Shared%20Documents/General/Annual-Data-Packages/2022/Marc_and_Bob/Fish_Template/NCRN_BSS_Fish_Monitoring_Data_2022_Marc.xlsx?d=w306357b1a43a48f4b9f598169043cc6a&csf=1&web=1&e=hzGvLn
            data.table::setnames(marc2022, "Species_ID...12", "common_name")
            data.table::setnames(marc2022, "Species_ID...13", "species_id")
            habitat_marc2021 <- readxl::read_excel("data/marc/ncrn_bss_fish_monitoring_data_stream_habitat_2021_marc.xlsx", sheet = "Summer_Habitat_Data_2021") # https://doimspp.sharepoint.com/:x:/r/sites/NCRNBiologicalStreamSampling/Shared%20Documents/General/Annual-Data-Packages/2022/Marc_and_Bob/Fish_Template/NCRN_BSS_Fish_Monitoring_Data_Stream_Habitat_2021_Marc.xlsx?d=w621453e1ce9f48a6a36d48850938f9cf&csf=1&web=1&e=B3sOOD
            habitat_marc2022 <- readxl::read_excel("data/marc/ncrn_bss_fish_monitoring_data_stream_habitat_2022_marc.xlsx", sheet = "Summer Habitat Data Sheet") # https://doimspp.sharepoint.com/:x:/r/sites/NCRNBiologicalStreamSampling/Shared%20Documents/General/Annual-Data-Packages/2022/Marc_and_Bob/Fish_Template/NCRN_BSS_Fish_Monitoring_Data_Stream_Habitat_2022_Marc.xlsx?d=web62dc84b1204861bb8fff2754a34c88&csf=1&web=1&e=5Um3sm
            for(i in 1:nrow(marc2022)){
                if(stringr::str_detect(marc2022$common_name[i], "\\(")==TRUE){
                    marc2022$common_name[i] <- tolower(stringr::str_extract(marc2022$common_name[i], "(?<=\\().+?(?=\\))")) # https://stackoverflow.com/questions/8613237/extract-info-inside-all-parenthesis-in-r
                }
            }
            marc2022$common_name <- tolower(marc2022$common_name)
            for(i in 1:nrow(marc2021)){
                if(stringr::str_detect(marc2021$Species_ID[i], "\\(")==TRUE){
                    marc2021$Species_ID[i] <- tolower(stringr::str_extract(marc2021$Species_ID[i], "(?<=\\().+?(?=\\))")) # https://stackoverflow.com/questions/8613237/extract-info-inside-all-parenthesis-in-r
                }
            }
            marc2021$Species_ID <- tolower(marc2021$Species_ID)
            summer_index_marc2022 <- readxl::read_excel("data/marc/ncrn_bss_fish_monitoring_data_stream_habitat_2022_marc.xlsx", sheet = "Summer Index Data Sheet")
            summer_exotic_marc2022 <- readxl::read_excel("data/marc/ncrn_bss_fish_monitoring_data_stream_habitat_2022_marc.xlsx", sheet = "Summer Index Exotic Plants")
            summer_flow_marc2022 <- readxl::read_excel("data/marc/ncrn_bss_fish_monitoring_data_stream_habitat_2022_marc.xlsx", sheet = "Summer Habitat Flow")

            # bob
            bob_2021_macroinvert <- readxl::read_excel("data/bob/2021_prwi_sites_bob.xlsx", sheet = "raw benthic data")
            bob_2021_water_chem <- readxl::read_excel("data/bob/2021_prwi_sites_bob.xlsx", sheet = "water chem")
            bob_2022_wq <- readxl::read_excel("data/bob/nps_wq_2022v2_bob.xlsx")
            bob_2022_hab <- readxl::read_excel("data/bob/nps_spring_habitat_2022_bob.xlsx", skip = 1)
            bob_2022_macroinvert <- readxl::read_excel("data/bob/nps_spring_2022_benthics_bob.xlsx")
            
            #-----  Query ncrn db
            RODBC::odbcCloseAll() # close any open db connection
            con <- RODBC::odbcConnectAccess2007(db) # open db connection
            db_objs <- RODBC::sqlTables(con) # test db connection
            tbl_names <- db_objs %>% # choose which tables you want to query
                subset(TABLE_NAME %in% c(
                    "tbl_Events"
                    ,"tbl_Protocol"
                    ,"tbl_Fish_Events"
                    ,"tbl_Locations"
                    ,"tbl_Meta_Events"
                    ,"tlu_Collection_Procedures_Gear_Config"
                    ,"tbl_Electro_Fish_Details"
                    ,"tbl_Fish_Data"
                    ,"tbl_GameFish"
                    ,"tlu_Fish"
                    ,"tlu_Basin_Code"
                    ,"tbl_Summer_PHI"
                    ,"tbl_Spring_PHI"
                    ,"tbl_Chemistry_Data"
                    ,"tlu_Macroinverts"
                    ,"tbl_Benthic_Data"
                    ,"tbl_Benthic_Habitat"
                )
                ) %>%
                select(TABLE_NAME)
            
            # make list of queries so we can extract a few rows from each table
            qry_list <- vector(mode="list", length=nrow(tbl_names))
            names(qry_list) <- tbl_names$TABLE_NAME
            for (i in 1:length(qry_list)){
                qry_list[[i]] <- paste("SELECT * FROM", names(qry_list)[i])
            }
            
            results_list <- query_db(qry_list = qry_list, connection = con)
            RODBC::odbcCloseAll() # close db connection
            
            # tidy up
            rm(db_objs)
            rm(tbl_names)
            rm(qry_list)
            
            #----- call functions that build data for EDD tabs
            activities <- edd_activities(
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
                )
            # activities <- edd_activities(results_list, bob_2021_water_chem)
            locations <- edd_locations(results_list, marc2022, marc2021, habitat_marc2021, habitat_marc2022)
            results <- edd_results(
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
            )
            
            #----- compile data for EDD tabs into a list
            list_of_datasets <- list("Locations" = locations, "Activities" = activities, "Results" = results)
            if(length(list_of_datasets)==3){
                if(nrow(list_of_datasets[[1]]>0) & nrow(list_of_datasets[[2]]>0) & nrow(list_of_datasets[[3]]>0)){
                    assign("EDD", list_of_datasets, envir = globalenv()) # save final product to global environment
                    message("\n\n`make_edd()` executed successfully.\nOutput saved as `EDD` in global environment.\n\n")
                }
            } else {
                message("An error occurred when compiling results.")
                break
            }
            
            #----- write list to xlsx if `write` flag is TRUE
            if(write == TRUE){
                openxlsx::write.xlsx(list_of_datasets, file = file.choose())
            }
        }
    )
}
