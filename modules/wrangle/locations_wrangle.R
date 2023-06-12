# a module for `edd_results.R`
# ETL, wrangle results
options(warn=-1)
activities_wrangle <- function(example, real){
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
                    "`activities` wrangled successfully..."
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