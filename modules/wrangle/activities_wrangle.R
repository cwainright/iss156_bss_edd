# a module for `edd_results.R`
# ETL, wrangle results
options(warn=-1)
activities_wrangle <- function(example, real){
    tryCatch(
        expr = {
            # import the location lookup table and replace values from source files with vetted values
            loc_lookup <- readxl::read_excel("data/location_lookup.xlsx")
            real$Additional_Location_Info <- ''
            real <- dplyr::left_join(real, loc_lookup, by=c('Location_ID' = 'bad'))
            real$Location_ID <- NULL
            real$lat <- NULL
            real$lon <- NULL
            real$name <- NULL
            real$Project_ID <- 'USNPS NCRN Biological Stream Survey'

            real <- real %>%
                rename(Location_ID = good) %>%
                select(colnames(example)) %>%
                # mutate(
                #     Location_ID = case_when(
                #         Location_ID == "NCRN_ROCR_FEBR_DUP" ~ "NCRN_ROCR_FEBR"
                #         ,Location_ID == "NCRN_PRWI_MBBR_DUP" ~ "NCRN_PRWI_MBBR"
                #         ,TRUE ~ Location_ID
                #     )
                # ) %>%
                # mutate(
                #     Additional_Location_Info = case_when(
                #         Additional_Location_Info == "Hazen Creek" ~"Reservation 630 Creek"
                #         ,Additional_Location_Info == "NCRN Monocacy Park at Bush Creek" ~"Bush Creek"
                #         ,Additional_Location_Info == "Henson Creek @ Suitland Road" ~"Henson Creek"
                #         ,Additional_Location_Info == "Still Creek, Greenbelt Park" ~"Still Creek"
                #         ,Additional_Location_Info == "Whiskey Still Creek" ~"Blue Blazes Creek"
                #         ,Additional_Location_Info == "Visitor's Center Creek" ~"Visitor Center Creek"
                #         ,TRUE ~ Additional_Location_Info
                #     )
                # ) %>%
                mutate(
                    Assemblage_Sampled_Name = case_when(
                        Assemblage_Sampled_Name == "physical habitat inventory" ~ "stream physical habitat"
                        ,TRUE ~ Assemblage_Sampled_Name
                    )
                ) %>%
                mutate(
                    Activity_Recorder = NA
                ) %>%
                mutate(
                    Custody_ID = NA
                ) %>%
                # mutate(
                #     Activity_Group_ID = NA
                # ) %>%
                mutate(
                    Collection_Method_ID = case_when(
                        Collection_Method_ID == "Stream benthic habitat inventory; version 2; protocol date 2009-06-30" ~ "Biological Stream Survey Protocol; version 2; protocol date 2009-06-30"
                        ,Collection_Method_ID == "Stream benthic habitat inventory; version 1; protocol date 2007-01-01" ~ "Biological Stream Survey Protocol; version 1; protocol date 2007-01-01"
                        ,Collection_Method_ID == "Stream habitat inventory; version 2; protocol date 2009-06-30" ~ "Biological Stream Survey Protocol; version 2; protocol date 2009-06-30"
                        ,is.na(Collection_Method_ID) ~ "Biological Stream Survey Protocol; version 2; protocol date 2009-06-30"
                        ,Collection_Method_ID == "Biological Stream Survey Protocol" ~ "Biological Stream Survey Protocol; version 2; protocol date 2009-06-30"
                        ,TRUE ~ Collection_Method_ID
                    )
                ) %>%
                mutate(
                    Preparation_Method_ID = case_when(
                        Preparation_Method_ID == "Stream benthic habitat inventory; version 2; protocol date 2009-06-30" ~ "Biological Stream Survey Protocol; version 2; protocol date 2009-06-30"
                        ,Preparation_Method_ID == "Stream benthic habitat inventory; version 1; protocol date 2007-01-01" ~ "Biological Stream Survey Protocol; version 1; protocol date 2007-01-01"
                        ,Preparation_Method_ID == "Stream habitat inventory; version 2; protocol date 2009-06-30" ~ "Biological Stream Survey Protocol; version 2; protocol date 2009-06-30"
                        ,is.na(Preparation_Method_ID) ~ "Biological Stream Survey Protocol; version 2; protocol date 2009-06-30"
                        ,Preparation_Method_ID == "Biological Stream Survey Protocol" ~ "Biological Stream Survey Protocol; version 2; protocol date 2009-06-30"
                        ,Preparation_Method_ID == "Stream water chemistry; version 2.0; protocol date 2009/116" ~ "Biological Stream Survey Protocol; version 2; protocol date 2009-06-30"
                        ,Preparation_Method_ID == "Stream benthic macroinvertebrates; version 2.0; protocol date 2009/116" ~ "Biological Stream Survey Protocol; version 2; protocol date 2009-06-30"
                        ,TRUE ~ Preparation_Method_ID
                    )
                ) %>%
                mutate(
                    Assemblage_Sampled_Name = case_when(
                        Assemblage_Sampled_Name == "Stream benthic habitat inventory" ~ "stream physical habitat"
                        ,Assemblage_Sampled_Name == "stream physical habitat inventory" ~ "stream physical habitat"
                        ,Assemblage_Sampled_Name == "Stream benthic macroinvertebrates" ~ "stream benthic macroinvertebrates"
                        ,Assemblage_Sampled_Name == "Stream Fish" ~ "stream fish"
                        ,TRUE ~ Assemblage_Sampled_Name
                    )
                )
            
            
            
            # check that data wrangling produced valid units
            # unit_check <- data.frame(
            #     char_name = sort(unique(real$Characteristic_Name))
            #     ,units = NA
            #     ,unit_count = NA
            # )
            # 
            # for(i in 1:nrow(unit_check)){
            #     mytarget <- unit_check$char_name[i]
            #     mysub <- real %>% select(
            #         Characteristic_Name, Result_Unit
            #     ) %>%
            #         filter(
            #             Characteristic_Name == mytarget
            #         )
            #     unit_vec <- sort(unique(mysub$Result_Unit))
            #     unit_check$units[i] <- ifelse(
            #         length(unit_vec) >1,
            #         paste(shQuote(sort(unique(mysub$Result_Unit)), type="cmd"), collapse=", "),
            #         unit_vec
            #     )
            #     unit_check$unit_count[i] <- length(unit_check$units[i])
            # }
            
            
            #----- keep only unique activities
            real <- real %>% distinct(Activity_ID, .keep_all = TRUE)
            
            #----- error-checking:
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
                    message("`activities` wrangled successfully...")
                    return(real)
                } else {
                    for(i in 1:length(check_df$result != "MATCH")){
                        cat(paste(paste0("`real.", check_df$real[i], "`"), paste0(" DID NOT MATCH `example.", check_df$example[i][i], "`"), "\n", sep = ""))
                    }
                }
            )
        }
    )
}