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
            
            #----- clean
            real <- real %>%
                mutate(
                    Characteristic_Name = tolower(Characteristic_Name)
                    ,Characteristic_Name = gsub("_", " ", Characteristic_Name)
                ) %>%
                mutate(
                    Characteristic_Name = case_when(
                        Characteristic_Name == "comments2" ~ "comments"
                        ,Characteristic_Name == "comments1" ~ "comments"
                        ,Characteristic_Name == "note" ~ "comments"
                        ,Characteristic_Name == "field comments about site" ~ "comments"
                        ,Characteristic_Name == "wetted width of stream at bottom of 75m sampling reach" ~ "wetted channel width - bottom of site (0m)"
                        ,Characteristic_Name == "wetted width of stream at top of 75m sampling reach" ~ "wetted channel width - top of site (75m)"
                        ,Characteristic_Name == "riparian vegetation width on left bank" ~ "stream physical characteristics - riparian width - left bank"
                        ,Characteristic_Name == "riparian vegetation width on right bank" ~ "stream physical characteristics - riparian width - right bank "
                        ,Characteristic_Name == "right bank concrete" ~ "stream physical characteristics - right bank - length of stream obstructed by pipe (m)"
                        ,Characteristic_Name == "left bank concrete" ~ "stream physical characteristics - left bank - length of stream obstructed by pipe (m)"
                        ,Characteristic_Name == "field comments" ~ "comments"
                        ,Characteristic_Name == "macrophytes sampled for benthos" ~ "square feet of habitat sampled for benthic macroinvertebrates - macrophytes"
                        ,Characteristic_Name == "macrophytes" ~ "square feet of habitat sampled for benthic macroinvertebrates - macrophytes"
                        ,Characteristic_Name == "riffle sampled for benthos" ~ "square feet of habitat sampled for benthic macroinvertebrates - riffle"
                        ,Characteristic_Name == "root wad sampled for benthos" ~ "square feet of habitat sampled for benthic macroinvertebrates - root wad"
                        ,Characteristic_Name == "leaf pack smapled for benthos" ~ "square feet of habitat sampled for benthic macroinvertebrates - leaf pack"
                        ,Characteristic_Name == "leaf pack" ~ "square feet of habitat sampled for benthic macroinvertebrates - leaf pack"
                        ,Characteristic_Name == "undercut bank sampled for benthos" ~ "square feet of habitat sampled for benthic macroinvertebrates - undercut bank"
                        ,Characteristic_Name == "'other habitats' sampled for benthos" ~ "square feet of habitat sampled for benthic macroinvertebrates - other habitat type"
                        ,Characteristic_Name == "number of square feet of undercut bank sampled for benthos" ~ "square feet of habitat sampled for benthic macroinvertebrates - undercut bank"
                        ,Characteristic_Name == "number of square feet of roots or leaves sampled for benthos" ~ "square feet of habiat sampled for benthic macroinvertebrates - roots or leaves"
                        ,Characteristic_Name == "mile-a-minute present/absent" ~ "exotic terrestrial plant relative abundance - persicaria perfoliata (mile-a-minute)"
                        ,Characteristic_Name == "japanese honeysuckle present/absent" ~ "exotic terrestrial plant relative abundance - lonicera japonica (japanese honeysuckle)"
                        ,Characteristic_Name == "japanese honeysuckle" ~ "exotic terrestrial plant relative abundance - lonicera japonica (japanese honeysuckle)"
                        ,Characteristic_Name == "japanese knotweed" ~ "exotic terrestrial plant relative abundance - reynoutria japonica (japanese knotweed)"
                        ,Characteristic_Name == "japanese stiltgrass" ~ "exotic terrestrial plant relative abundance - microstegium vimineum (japanese stiltgrass)"
                        ,Characteristic_Name == "japanese hops" ~ "exotic terrestrial plant relative abundance - humulus japonicus (japanese hops)"
                        ,Characteristic_Name == "bush honeysuckle" ~ "exotic terrestrial plant relative abundance - lonicera tatarica (bush honeysuckle)"
                        ,Characteristic_Name == "wisteria" ~ "exotic terrestrial plant relative abundance - lonicera tatarica (wisteria)"
                        ,Characteristic_Name == "princess tree" ~ "exotic terrestrial plant relative abundance - paulownia tomentosa (princess tree)"
                        ,Characteristic_Name == "ornamental bittersweet" ~ "exotic terrestrial plant relative abundance - celastrus orbiculatus (ornamental bittersweet)"
                        ,Characteristic_Name == "garlic mustard" ~ "exotic terrestrial plant relative abundance - alliaria petiolata (garlic mustard)"
                        ,Characteristic_Name == "green briar" ~ "exotic terrestrial plant relative abundance - smilax rotundifolia (green briar)"
                        ,Characteristic_Name == "english ivy" ~ "exotic terrestrial plant relative abundance - hedera helixa (english ivy)"
                        ,Characteristic_Name == "purple loosestrife" ~ "exotic terrestrial plant relative abundance - lythrum salicaria (purple loosestrife)"
                        ,Characteristic_Name == "fragmites" ~ "exotic terrestrial plant relative abundance - phragmites spp. (reed grasses)"
                        ,Characteristic_Name == "mile-a-minute" ~ "exotic terrestrial plant relative abundance - persicaria perfoliata (mile-a-minute)"
                        ,Characteristic_Name == "privot" ~ "exotic terrestrial plant relative abundance - ligustrum spp. (privet)"
                        ,Characteristic_Name == "wineberry" ~ "exotic terrestrial plant relative abundance - rubus phoenicolasius (wineberry)"
                        ,Characteristic_Name == "multiflora rose" ~ "exotic terrestrial plant relative abundance - rosa multiflora (multiflora rose)"
                        ,Characteristic_Name == "deep pool (> 50 cm depth)  present, absent, extensive" ~ "stream physical characteristics -  relative abundance- deep pool (> 50 cm depth)"
                        ,Characteristic_Name == "undercut banks present, absent, extensive" ~ "stream physical characteristics - relative abundance - undercut banks"
                        ,Characteristic_Name == "undercut banks" ~ "stream physical characteristics - relative abundance - undercut banks"
                        ,Characteristic_Name == "rootwad/woody debris" ~ "stream physical characteristics - relative abundance - rootwad/woody debris"
                        ,Characteristic_Name == "submerged aquatic vegetation" ~ "aquatic vegetation relative abundance - submerged aquatic plants"
                        ,Characteristic_Name == "emergent aquatic vegetation" ~ "aquatic vegetation relative abundance - emergent aquatic plants"
                        ,Characteristic_Name == "floating aquatic vegetation" ~ "aquatic vegetation relative abundance - floating aquatic plants"
                        ,Characteristic_Name == "percentage of rocks (gravel, cobble, and boulders) that are surrounded by, covered, or sunken into the silt, sand, or mud of the stream" ~ "stream physical characteristics - embeddedness - percentage of rocks (gravel, cobble, boulders) covered by sediment (sand, silt)"
                        ,Characteristic_Name == "erosion severity right  bank (0=none; 1=minor; 2=moderate; 3=severe)" ~ "stream physical characteristics - right bank - stream erosion severity"
                        ,Characteristic_Name == "geomorphology sampleability" ~ "sampleability - geomorphology"
                        ,Characteristic_Name == "electrofishing sampleability" ~ "sampleability - electrofishing"
                        ,Characteristic_Name == "habtitat sampleability" ~ "sampleability - habtitat"
                        ,Characteristic_Name == "water quality sampleability" ~ "sampleability - water quality"
                        ,Characteristic_Name == "water quality" ~ "sampleability - water quality"
                        ,Characteristic_Name == "herpetofauna sampleability" ~ "sampleability - herpetofauna"
                        ,Characteristic_Name == "salamanders sampleability" ~ "sampleability - salamanders"
                        ,Characteristic_Name == "crayfishes sampleability" ~ "sampleability - crayfishes"
                        ,Characteristic_Name == "mussels sampleability" ~ "sampleability - mussels"
                        ,Characteristic_Name == "aquatic plants sampleability" ~ "sampleability - macrophytes"
                        ,Characteristic_Name == "exotic plants sampleability" ~ "sampleability - exotic terrestrial plants"
                        ,Characteristic_Name == "time float takes to make it to end point for trial 1" ~ "alternate discharge measurement - float time - trial 1"
                        ,Characteristic_Name == "time float takes to make it to end point for trial 2" ~ "alternate discharge measurement - float time - trial 2"
                        ,Characteristic_Name == "time float takes to make it to end point for trial 3" ~ "alternate discharge measurement - float time - trial 3"
                        ,Characteristic_Name == "number of large wood pieces in wetted stream" ~ "coarse woody debris in wetted stream"
                        ,Characteristic_Name == "number of large wood pieces in active channel but currently dewatered" ~ "coarse woody debris in stream channel but currently dewatered"
                        ,Characteristic_Name == "left dominant substrate at 50m-75m" ~ "stream physical characteristics - 50m-75m - river-left dominant substrate"
                        ,Characteristic_Name == "left subdominant substrate at 50m-75m" ~ "stream physical characteristics - 50m-75m -river-left subdominant substrate"
                        ,Characteristic_Name == "left depth at 50m-75m" ~ "stream physical characteristics - 50m-75m - river-left water depth"
                        ,Characteristic_Name == "left velocity at 50m-75m" ~ "stream physical characteristics - 50m-75m - river-left water velocity"
                        ,Characteristic_Name == "right dominant substrate at 50m-75m" ~ "stream physical characteristics - 50m-75m - river-right dominant substrate"
                        ,Characteristic_Name == "right subdominant substrate at 50m-75m" ~ "stream physical characteristics - 50m-75m - river-right subdominant substrate"
                        ,Characteristic_Name == "right depth at 50m-75m" ~ "stream physical characteristics - 50m-75m - river-right water depth"
                        ,Characteristic_Name == "right velocity at 50m-75m" ~ "stream physical characteristics - 50m-75m - river-right water velocity"
                        ,Characteristic_Name == "left dominant substrate at 25m-50m" ~ "stream physical characteristics - 25m-50m - river-left dominant substrate"
                        ,Characteristic_Name == "left subdominant substrate at 25m-50m" ~ "stream physical characteristics - 25m-50m -river-left subdominant substrate"
                        ,Characteristic_Name == "left depth at 25m-50m" ~ "stream physical characteristics - 25m-50m - river-left water depth"
                        ,Characteristic_Name == "left velocity at 25m-50m" ~ "stream physical characteristics - 25m-50m - river-left water velocity"
                        ,Characteristic_Name == "right dominant substrate at 25m-50m" ~ "stream physical characteristics - 25m-50m - river-right dominant substrate"
                        ,Characteristic_Name == "right subdominant substrate at 25m-50m" ~ "stream physical characteristics - 25m-50m - river-right subdominant substrate"
                        ,Characteristic_Name == "right depth at 25m-50m" ~ "stream physical characteristics - 25m-50m - river-right water depth"
                        ,Characteristic_Name == "right velocity at 25m-50m" ~ "stream physical characteristics - 25m-50m - river-right water velocity"
                        ,Characteristic_Name == "left dominant substrate at 0m-25m" ~ "stream physical characteristics - 0m-25m - river-left dominant substrate"
                        ,Characteristic_Name == "left subdominant substrate at 0m-25m" ~ "stream physical characteristics - 0m-25m -river-left subdominant substrate"
                        ,Characteristic_Name == "left depth at 0m-25m" ~ "stream physical characteristics - 0m-25m - river-left water depth"
                        ,Characteristic_Name == "left velocity at 0m-25m" ~ "stream physical characteristics - 0m-25m - river-left water velocity"
                        ,Characteristic_Name == "right dominant substrate at 0m-25m" ~ "stream physical characteristics - 0m-25m - river-right dominant substrate"
                        ,Characteristic_Name == "right subdominant substrate at 0m-25m" ~ "stream physical characteristics - 0m-25m - river-right subdominant substrate"
                        ,Characteristic_Name == "right depth at 0m-25m" ~ "stream physical characteristics - 0m-25m - river-right water depth"
                        ,Characteristic_Name == "right velocity at 0m-25m" ~ "stream physical characteristics - 0m-25m - river-right water velocity"
                        ,Characteristic_Name == "right velocity at 0m-25m" ~ "stream physical characteristics - 0m-25m - river-right water velocity"
                        ,Characteristic_Name == "presence of gabions on left bank" ~ "stream physical characteristics - left bank - length of stream obstructed by gabion (m)"
                        ,Characteristic_Name == "presence of gabions on right bank" ~ "stream physical characteristics - right bank - length of stream obstructed by gabion (m)"
                        ,Characteristic_Name == "presence of gabions on stream bottom" ~ "stream physical characteristics - stream bottom - length of stream obstructed by gabion (m)"
                        ,Characteristic_Name == "presence of pipe on left bank" ~ "stream physical characteristics - left bank - length of stream obstructed by pipe (m)"
                        ,Characteristic_Name == "presence of pipe on right bank" ~ "stream physical characteristics - right bank - length of stream obstructed by gabion (m)"
                        ,Characteristic_Name == "presence of pipe on stream bottom" ~ "stream physical characteristics - stream bottom - length of stream obstructed by gabion (m)"
                        ,Characteristic_Name == "right bank gabion" ~ "stream physical characteristics - right bank - length of stream obstructed by gabion (m)"
                        ,Characteristic_Name == "left bank gabion" ~ "stream physical characteristics - left bank - length of stream obstructed by gabion (m)"
                        ,Characteristic_Name == "wetted channel width - bottom of site (0m)" ~ "stream physical characteristics - 0m  - wetted channel width (m)"
                        ,Characteristic_Name == "wetted channel width (m) at 25 m from bottom of site" ~ "stream physical characteristics - 25m - wetted channel width (m)"
                        ,Characteristic_Name == "wetted channel width (m) at 50 m from bottom of site" ~ "stream physical characteristics - 50m - wetted channel width (m)"
                        ,Characteristic_Name == "wetted channel width (m) at top (75m) of site" ~ "stream physical characteristics - 75m - wetted channel width (m)"
                        ,Characteristic_Name == "thalweg depth (cm) at bottom of site" ~ "stream physical characteristics - 0m - thalweg depth (cm)"
                        ,Characteristic_Name == "thalweg depth (cm) at 25 m from bottom of site" ~ "stream physical characteristics - 25m - thalweg depth (cm)"
                        ,Characteristic_Name == "thalweg depth (cm) at 50 m from bottom of site" ~ "stream physical characteristics - 50m - thalweg depth (cm)"
                        ,Characteristic_Name == "thalweg depth (cm) at top (75 m) of site" ~ "stream physical characteristics - 75m - thalweg depth (cm)"
                        ,Characteristic_Name == "thalweg velocity (m/s) at bottom of site" ~ "stream physical characteristics - 0m - thalweg velocity (m/s)"
                        ,Characteristic_Name == "thalweg velocity (m/s) at 25 m from bottom of site" ~ "stream physical characteristics - 25m - thalweg velocity (m/s)"
                        ,Characteristic_Name == "thalweg velocity (m/s) at 50 m from bottom of site" ~ "stream physical characteristics - 50m - thalweg velocity (m/s)"
                        ,Characteristic_Name == "thalweg velocity (m/s) at top (75 m) of site" ~ "stream physical characteristics - 75m - thalweg velocity (m/s)"
                        ,Characteristic_Name == "wetted channel width (m) at bottom (0m) of site" ~ "stream physical characteristics - 0m - wetted channel width (m)"
                        ,Characteristic_Name == "wetted channel width - bottom of site (0m)" ~ "stream physical characteristics - 0m - wetted channel width (m)"
                        ,Characteristic_Name == "wetted channel width - top of site (75m)" ~ "stream physical characteristics - 75m - wetted channel width (m)"
                        ,Characteristic_Name == "diversity and quality of water velocity and depths habitat score (0-20)" ~ "stream physical characteristics - diversity and quality of water velocity and depths habitat score (0-20)"
                        ,Characteristic_Name == "velocity" ~ "stream physical characteristics - water velocity associated with a stream lateral location (m/s)"
                        ,Characteristic_Name == "depth" ~ "stream physical characteristics - water depth associated with a stream lateral location (cm)"
                        ,Characteristic_Name == "lat loc" ~ "stream physical characteristics - stream lateral location (m)"
                        ,Characteristic_Name == "stream gradient measurement location (distance from bottom)" ~ "stream physical characteristics - stream gradient measurement location (meters above site-bottom)"
                        ,Characteristic_Name == "stream gradient height measurement (meters)" ~ "stream physical characteristics - stream gradient height measurement (meters above stream gradient measurement location)"
                        ,Characteristic_Name == "stream gradient" ~ "stream physical characteristics - stream gradient (percent slope)"
                        # ,Characteristic_Name %like% "gabion" ~ paste0("stream physical characteristics - ",Characteristic_Name)
                        ,TRUE ~ Characteristic_Name
                    )
                ) %>%
                mutate(
                    Result_Unit = case_when(
                        # Characteristic_Name %like% "exotic terrestrial plant relative abundance" ~ "A = absent, P = present, E = extensive"
                        Characteristic_Name %like% "relative abundance" ~ "A = absent, P = present, E = extensive"
                        ,Characteristic_Name %like% "benthic macroinvertebrates" ~ "square feet"
                        ,Characteristic_Name %like% "sampleability" ~ "S = Sampleable"
                        ,Characteristic_Name %like% "float time" ~ "seconds"
                        ,Characteristic_Name %like% "stream erosion severity" ~ "0 = None; 1 = Minor; 2 = Moderate; 3 = Severe"
                        ,Characteristic_Name %like% "coarse woody debris" ~ "count"
                        ,Characteristic_Name %like% "length of stream obstructed by" ~ "meters"
                        ,Characteristic_Name %like% "wetted channel width" ~ "meters"
                        ,Characteristic_Name %like% "stream gradient measurement location" ~ "meters"
                        ,Characteristic_Name %like% "stream gradient height measurement" ~ "meters"
                        ,Characteristic_Name %like% "percent slope" ~ "percent slope"
                        ,Activity_ID %like% "macroinvertebrates" ~ "count of individuals"
                        ,TRUE ~ Characteristic_Name
                    )
                ) %>%
                mutate(Result_Qualifier = case_when(
                    Characteristic_Name %like% "float time" ~ "for sites with extremely low flow, the speed of a floating object is substituted to allow calculation of discharge"
                    ,Characteristic_Name %like% "alternate flow measurement" ~ "for sites with extremely low flow, the speed of a floating object is substituted to allow calculation of discharge"
                    )
                ) %>%
                mutate(
                    Characteristic_Name = gsub("right bank", "top right bank", Characteristic_Name)
                    ,Characteristic_Name = gsub("left bank", "top left bank", Characteristic_Name)
                    ,Characteristic_Name = gsub("alternate flow measurement", "alternate discharge measurement -", Characteristic_Name)
                ) %>%
                filter(
                    Characteristic_Name != "8 digit watershed code for sampled site" # this is not a result; it's a datum associated with the sampling location, which belongs in (and is reported in) edd.locations
                    ,Characteristic_Name != "benthos" # this is a boolean that simply says whether bob sampled for macroinverts at this Activity_ID; it's not a datum
                    ,Characteristic_Name != "sampleable?" # this is a boolean that simply says whether bob could sample the site; it's not a datum
                ) %>%
                mutate(Result_Text = case_when(
                    Characteristic_Name == "sampleability - electrofishing" & Result_Text == "10" ~ "S" # this is a typo in the source file
                    ,Characteristic_Name == "sampleability - water quality" & Result_Text == "Y" ~ "S" # this is a typo in the source file
                    ,Characteristic_Name %like% "gabion relative abundance" & Result_Text == "0" ~ "A" # this is a typo in the source file
                    ,Characteristic_Name %like% "gabion relative abundance" & Result_Text == "N" ~ "A" # this is a typo in the source file
                    ,TRUE ~ Result_Text
                    )
                ) %>%
                mutate(
                    Activity_ID = trimws(Activity_ID, which="both")
                    ,Characteristic_Name = trimws(Characteristic_Name, which="both")
                    ,Result_Text = trimws(Result_Text, which="both")
                    ,Result_Unit = trimws(Result_Unit, which="both")
                    ,Result_Comment = trimws(Result_Comment, which="both")
                )
            
            myvec <- unique(real$Characteristic_Name)
            subset(myvec, myvec %like% "fragmites")
            unique(real$Characteristic_Name) %like% "stream gradient"
            mysub <- real %>% subset(Characteristic_Name %like% "stream gradient")
            mysub <- real %>% subset(Characteristic_Name == "stream gradient")
            View(mysub)
            unique(mysub$Result_Unit)
            unique(mysub$Result_Text)
            unique(mysub$Characteristic_Name)
            
            
            mysub <- mysub %>% mutate(
                Result_Unit = case_when(
                    Characteristic_Name %like% "percent slope" ~ "percent slope"
                    ,TRUE ~ Result_Unit
                )
            )
            
            sort(unique(real$Characteristic_Name))
            rm(mychars)
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