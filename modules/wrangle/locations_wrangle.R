# a module for `edd_results.R`
# ETL, wrangle results
options(warn=-1)
locations_wrangle <- function(example, real){
    tryCatch(
        expr = {
            real <- real %>%
                mutate(
                    Location_ID = case_when(
                        Location_ID == "NCRN_ROCR_FEBR_DUP" ~ "NCRN_ROCR_FEBR"
                        ,Location_ID == "NCRN_PRWI_MBBR_DUP" ~ "NCRN_PRWI_MBBR"
                        ,TRUE ~ Location_ID
                    )
                ) %>%
                mutate(
                    Location_Name = case_when(
                        Location_Name == "Hazen Creek" ~"Reservation 630 Creek"
                        ,Location_Name == "NCRN Monocacy Park at Bush Creek" ~"Bush Creek"
                        ,Location_Name == "Henson Creek @ Suitland Road" ~"Henson Creek"
                        ,Location_Name == "Still Creek, Greenbelt Park" ~"Still Creek"
                        ,Location_Name == "Whiskey Still Creek" ~"Blue Blazes Creek"
                        ,Location_Name == "Visitor's Center Creek" ~"Visitor Center Creek"
                        ,TRUE ~ Location_Name
                    )
                ) %>%
                mutate(
                    Latitude = case_when(
                        Location_ID == "NCRN_ROCR_PACR" & Latitude == "0" ~ "38.9204346469"
                    )
                ) %>%
                mutate(
                    Longitude = case_when(
                        Location_ID == "NCRN_ROCR_PACR" & Latitude == "0" ~ "-77.0994775797"
                    )
                ) %>%
                mutate(
                    Drainage_Area = case_when(
                        Location_ID == "NCRN_ROCR_PACR" & is.na(Drainage_Area) ~ "172.54"
                    )
                ) %>%
                mutate(
                    Drainage_Area_Unit = case_when(
                        Location_ID == "NCRN_ROCR_PACR" & is.na(Drainage_Area_Unit) ~ "acre"
                    )
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
                        "`activities` wrangled successfully..."
                    } else {
                        for(i in 1:length(check_df$result != "MATCH")){
                            cat(paste(paste0("`real.", check_df$real[i], "`"), paste0(" DID NOT MATCH `example.", check_df$example[i][i], "`"), "\n", sep = ""))
                        }
                    }
                )
        }
    )
}