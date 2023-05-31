# a module for `edd_results()`
# ETL habitat_marc2022 and habitat_marc2021 into edd.results
options(warn = -1)
marc_to_ncrn_lookup <- function(df2022, lookup){
    tryCatch(
        expr = {
            #----- load external libraries
            suppressWarnings(suppressMessages(library(data.table)))
            suppressWarnings(suppressMessages(library(tidyverse)))
            suppressWarnings(suppressMessages(library(dplyr)))
            suppressWarnings(suppressMessages(library(readxl)))

            marc_2022_long <- data.frame(df2022=unique(df2022$dummy))
            marc_2022_long$long <- NA
            marc_2022_long$long[1] <- "erosion extent left bank"
            marc_2022_long$long[2] <- "erosion severity left bank (0=none; 1=minor; 2=moderate; 3=severe)"
            marc_2022_long$long[3] <- "erosion height left bank (meters)"
            marc_2022_long$long[4] <- "erosion extent right bank"
            marc_2022_long$long[5] <- "erosion severity right  bank (0=none; 1=minor; 2=moderate; 3=severe)"
            marc_2022_long$long[6] <- "erosion height right bank (meters)"
            marc_2022_long$long[7] <- "instream habitat score (0-20)"
            marc_2022_long$long[8] <- "epifaunal substrate habitat score (0-20)"
            marc_2022_long$long[9] <- "diversity and quality of water velocity and depths habitat score (0-20)"
            marc_2022_long$long[10] <- "diversity and quality of pool, glide, and eddy habitats score (0-20)"
            marc_2022_long$long[11] <- "diversity and quality of pool, glide, and eddy habitats score (0-20)"
            marc_2022_long$long[12] <- "quality of riffle and run habitats score (0-20)"
            marc_2022_long$long[13] <- "linear extent of riffle habitat (meters; 75 m maximum)"
            marc_2022_long$long[14] <- "percentage of rocks (gravel, cobble, and boulders) that are surrounded by, covered, or sunken into the silt, sand, or mud of the stream"
            marc_2022_long$long[15] <- "percentage of segment that is shaded (including duration of shading; 0-100)"
            marc_2022_long$long[16] <- "braided channel present, absent, extensive"
            marc_2022_long$long[17] <- "riffle present, absent, extensive"
            marc_2022_long$long[18] <- "run/glide present, absent, extensive"
            marc_2022_long$long[19] <- "deep pool (> 50 cm depth)  present, absent, extensive"
            marc_2022_long$long[20] <- "shallow pool (< 50 cm deep) present, absent, extensive"
            marc_2022_long$long[21] <- "gravel present, absent, extensive"
            marc_2022_long$long[22] <- "sand present, absent, extensive"
            marc_2022_long$long[23] <- "silt/clay present, absent, extensive"
            marc_2022_long$long[24] <- "cobbles present, absent, extensive"
            marc_2022_long$long[25] <- "bedrock present, absent, extensive"
            marc_2022_long$long[26] <- "large boulders (> 2m) present, absent, extensive"
            marc_2022_long$long[27] <- "small boulders present (<2m, but larger than cobble), absent, extensive"
            marc_2022_long$long[28] <- "beaver ponds present, absent, extensive"
            marc_2022_long$long[29] <- "overhead cover present, absent, extensive"
            marc_2022_long$long[30] <- "undercut banks present, absent, extensive"
            marc_2022_long$long[31] <- "orange floc iron bacteria surface film present, absent, extensive" # "orange floc" ?!?!?
            marc_2022_long$long[32] <- "number of large wood pieces in wetted stream"
            marc_2022_long$long[33] <- "number of large wood pieces in active channel but currently dewatered"
            marc_2022_long$long[34] <- "number of root wads in wetted stream"
            marc_2022_long$long[35] <- "number of root wads in active channel but currently dewatered"
            marc_2022_long$long[36] <- "maximum depth (cm)"
            marc_2022_long$long[37] <- "wetted channel width (m) at bottom (0m) of site"
            marc_2022_long$long[38] <- "thalweg depth (cm) at bottom of site"
            marc_2022_long$long[39] <- "thalweg velocity (m/s) at bottom of site"
            marc_2022_long$long[40] <- "wetted channel width (m) at 25 m from bottom of site"
            marc_2022_long$long[41] <- "thalweg depth (cm) at 25 m from bottom of site"
            marc_2022_long$long[42] <- "thalweg velocity (m/s) at 25 m from bottom of site"
            marc_2022_long$long[43] <- "wetted channel width (m) at 50 m from bottom of site"
            marc_2022_long$long[44] <- "thalweg depth (cm) at 50 m from bottom of site"
            marc_2022_long$long[45] <- "thalweg velocity (m/s) at 50 m from bottom of site"
            marc_2022_long$long[46] <- "wetted channel width (m) at top (75m) of site"
            marc_2022_long$long[47] <- "thalweg depth (cm) at top (75 m) of site"
            marc_2022_long$long[48] <- "thalweg velocity (m/s) at top (75 m) of site"
            marc_2022_long$long[49] <- "field comments"
            
            marc_2022_long <- dplyr::left_join(marc_2022_long, lookup %>% select(long, short, source, unit), by=c("long"))
            
            marc_2022_long$unit[5] <- "0=none; 1=minor; 2=moderate; 3=severe"
            marc_2022_long$unit[14] <- "percent"
            marc_2022_long$unit[19] <- "A = absent, P = present, E = extensive"
            marc_2022_long$unit[31] <- "A = absent, P = present, E = extensive"

            return(marc_2022_long)
        }
    )
}