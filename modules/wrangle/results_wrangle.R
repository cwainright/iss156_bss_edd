options(warn=-1)
results_wrangle <- function(
        example
        ,results
){
    tryCatch(
        expr = {

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
                    ,Characteristic_Name == "wetted width of stream at bottom of 75m sampling reach" ~ "stream abiotic - 75m - wetted channel width"
                    ,Characteristic_Name == "wetted width of stream at top of 75m sampling reach" ~ "stream abiotic - 0m - wetted channel width"
                    ,Characteristic_Name == "riparian vegetation width on left bank" ~ "stream abiotic - riparian width - left bank"
                    ,Characteristic_Name == "riparian vegetation width on right bank" ~ "stream abiotic - riparian width - right bank "
                    ,Characteristic_Name == "right bank concrete" ~ "stream abiotic - right bank - extent - length of stream channelized by pipe"
                    ,Characteristic_Name == "left bank concrete" ~ "stream abiotic - left bank - extent - length of stream channelized by pipe"
                    ,Characteristic_Name == "field comments" ~ "comments"
                    ,Characteristic_Name == "vegetation types on left bank in riparian buffer most to least common (g=grasses/forbes; r=regenerating deciduous/shrubs; y=young deciduous; m=mature deciduous; o=old deciduous; a=regenerating coniferous; b=yound coniferous; c=mature coniferous; d=lawn)" ~ "stream biotic - left bank - dominant riparian buffer terrestrial vegetation"
                    ,Characteristic_Name == "landuse" ~ "stream biotic - either bank - dominant riparian buffer terrestrial vegetation"
                    ,Characteristic_Name == "left bank veg type" ~ "stream biotic - dominant riparian buffer terrestrial vegetation - left bank"
                    ,Characteristic_Name == "vegetation types on right bank in riparian buffer most to least common (g=grasses/forbes; r=regenerating deciduous/shrubs; y=young deciduous; m=mature deciduous; o=old deciduous; a=regenerating coniferous; b=yound coniferous; c=mature coniferous; d=lawn)" ~ "stream biotic - right bank - dominant riparian buffer terrestrial vegetation"
                    ,Characteristic_Name == "right bank veg type" ~ "stream biotic - dominant riparian buffer terrestrial vegetation - right bank"
                    ,Characteristic_Name == "macrophytes sampled for benthos" ~ "stream biotic - square feet of habitat sampled for benthic macroinvertebrates - macrophytes"
                    ,Characteristic_Name == "macrophytes" ~ "stream biotic - square feet of habitat sampled for benthic macroinvertebrates - macrophytes"
                    ,Characteristic_Name == "riffle sampled for benthos" ~ "stream biotic - square feet of habitat sampled for benthic macroinvertebrates - riffle"
                    ,Characteristic_Name == "root wad sampled for benthos" ~ "stream biotic - square feet of habitat sampled for benthic macroinvertebrates - root wad"
                    ,Characteristic_Name == "leaf pack smapled for benthos" ~ "stream biotic - square feet of habitat sampled for benthic macroinvertebrates - leaf pack"
                    ,Characteristic_Name == "leaf pack" ~ "stream biotic - square feet of habitat sampled for benthic macroinvertebrates - leaf pack"
                    ,Characteristic_Name == "undercut bank sampled for benthos" ~ "stream biotic - square feet of habitat sampled for benthic macroinvertebrates - undercut bank"
                    ,Characteristic_Name == "'other habitats' sampled for benthos" ~ "stream biotic - square feet of habitat sampled for benthic macroinvertebrates - other habitat type"
                    ,Characteristic_Name == "number of square feet of undercut bank sampled for benthos" ~ "stream biotic - square feet of habitat sampled for benthic macroinvertebrates - undercut bank"
                    ,Characteristic_Name == "number of square feet of roots or leaves sampled for benthos" ~ "stream biotic - square feet of habiat sampled for benthic macroinvertebrates - roots or leaves"
                    ,Characteristic_Name == "mile-a-minute present/absent" ~ "stream biotic - exotic terrestrial plant relative abundance - persicaria perfoliata (mile-a-minute)"
                    ,Characteristic_Name == "japanese honeysuckle present/absent" ~ "stream biotic - exotic terrestrial plant relative abundance - lonicera japonica (japanese honeysuckle)"
                    ,Characteristic_Name == "nonnative thistles present/absent" ~ "stream biotic - exotic terrestrial plant relative abundance - nonnative thistles"
                    ,Characteristic_Name == "japanese honeysuckle" ~ "stream biotic - exotic terrestrial plant relative abundance - lonicera japonica (japanese honeysuckle)"
                    ,Characteristic_Name == "japanese knotweed" ~ "stream biotic - exotic terrestrial plant relative abundance - reynoutria japonica (japanese knotweed)"
                    ,Characteristic_Name == "japanese stiltgrass" ~ "stream biotic - exotic terrestrial plant relative abundance - microstegium vimineum (japanese stiltgrass)"
                    ,Characteristic_Name == "microstegium present/absent" ~ "stream biotic - exotic terrestrial plant relative abundance - microstegium vimineum (japanese stiltgrass)"
                    ,Characteristic_Name == "japanese hops" ~ "stream biotic - exotic terrestrial plant relative abundance - humulus japonicus (japanese hops)"
                    ,Characteristic_Name == "bush honeysuckle" ~ "stream biotic - exotic terrestrial plant relative abundance - lonicera tatarica (bush honeysuckle, tatarian honeysuckle)"
                    ,Characteristic_Name == "wisteria" ~ "stream biotic - exotic terrestrial plant relative abundance - wisteria spp. (wisteria)"
                    ,Characteristic_Name == "princess tree" ~ "stream biotic - exotic terrestrial plant relative abundance - paulownia tomentosa (princess tree)"
                    ,Characteristic_Name == "ornamental bittersweet" ~ "stream biotic - exotic terrestrial plant relative abundance - celastrus orbiculatus (ornamental bittersweet)"
                    ,Characteristic_Name == "garlic mustard" ~ "stream biotic - exotic terrestrial plant relative abundance - alliaria petiolata (garlic mustard)"
                    ,Characteristic_Name == "green briar" ~ "stream biotic - exotic terrestrial plant relative abundance - smilax rotundifolia (green briar)"
                    ,Characteristic_Name == "english ivy" ~ "stream biotic - exotic terrestrial plant relative abundance - hedera helixa (english ivy)"
                    ,Characteristic_Name == "purple loosestrife" ~ "stream biotic - exotic terrestrial plant relative abundance - lythrum salicaria (purple loosestrife)"
                    ,Characteristic_Name == "fragmites" ~ "stream biotic - exotic terrestrial plant relative abundance - phragmites spp. (reed grasses)"
                    ,Characteristic_Name == "phragmites present/absent" ~ "stream biotic - exotic terrestrial plant relative abundance - phragmites spp. (reed grasses)"
                    ,Characteristic_Name == "mile-a-minute" ~ "stream biotic - exotic terrestrial plant relative abundance - persicaria perfoliata (mile-a-minute)"
                    ,Characteristic_Name == "privot" ~ "stream biotic - exotic terrestrial plant relative abundance - ligustrum spp. (privet)"
                    ,Characteristic_Name == "wineberry" ~ "stream biotic - exotic terrestrial plant relative abundance - rubus phoenicolasius (wineberry)"
                    ,Characteristic_Name == "multiflora rose" ~ "stream biotic - exotic terrestrial plant relative abundance - rosa multiflora (multiflora rose)"
                    ,Characteristic_Name == "multiflora rose present/absent" ~ "stream biotic - exotic terrestrial plant relative abundance - rosa multiflora (multiflora rose)"
                    ,Characteristic_Name == "reed canary present/absent" ~ "stream biotic - exotic terrestrial plant relative abundance - phalaris arundinacea (reed canary)"
                    ,Characteristic_Name == "reed canary grass present/absent" ~ "stream biotic - exotic terrestrial plant relative abundance - phalaris arundinacea (reed canary)"
                    ,Characteristic_Name == "species of other exotics that may be present" ~ "stream biotic - exotic terrestrial plant relative abundance - other species"
                    ,Characteristic_Name == "other exotic plant species present/absent" ~ "stream biotic - exotic terrestrial plant relative abundance - other species"
                    ,Characteristic_Name == "deep pool (> 50 cm depth)  present, absent, extensive" ~ "stream abiotic - relative abundance - deep pool (> 50 cm depth)"
                    ,Characteristic_Name == "deep pool (> 50 cm depth) present, absent, extensive" ~ "stream abiotic - relative abundance - deep pool (> 50 cm depth)"
                    ,Characteristic_Name == "gravel present, absent, extensive" ~ "stream abiotic - relative abundance - gravel"
                    ,Characteristic_Name == "shallow pool (< 50 cm deep) present, absent, extensive" ~ "stream abiotic - relative abundance - shallow pool (<= 50 cm depth)"
                    ,Characteristic_Name == "cobbles present, absent, extensive" ~ "stream abiotic - relative abundance - cobbles"
                    ,Characteristic_Name == "braided channel present, absent, extensive" ~ "stream abiotic - relative abundance - braided channel"
                    ,Characteristic_Name == "overhead cover present, absent, extensive" ~ "stream abiotic - relative abundance - overhead cover"
                    ,Characteristic_Name == "percentage of segment that is shaded (including duration of shading; 0-100)" ~ "stream abiotic - overhead cover"
                    ,Characteristic_Name == "bedrock present, absent, extensive" ~ "stream abiotic - relative abundance - bedrock"
                    ,Characteristic_Name == "beaver ponds present, absent, extensive" ~ "stream abiotic - relative abundance - beaver ponds"
                    ,Characteristic_Name == "maximum depth (cm)" ~ "stream abiotic - reach maximum depth"
                    ,Characteristic_Name == "linear extent of pool habitat (meters; 75 m maximum)" ~ "stream abiotic - extent - linear extent pool habitat"
                    ,Characteristic_Name == "linear extent of riffle habitat (meters; 75 m maximum)" ~ "stream abiotic - extent - linear extent riffle habitat"
                    ,Characteristic_Name == "large boulders (> 2m) present, absent, extensive" ~ "stream abiotic - relative abundance - large boulders (> 2 m)"
                    ,Characteristic_Name == "riffle present, absent, extensive" ~ "stream abiotic - relative abundance - riffle habitat"
                    ,Characteristic_Name == "quality of riffle and run habitats score (0-20)"  ~ "stream abiotic - habitat quality - riffle"
                    ,Characteristic_Name == "riffle"  ~ "stream abiotic - habitat quality - riffle"
                    ,Characteristic_Name == "run/glide present, absent, extensive" ~ "stream abiotic - relative abundance - run or glide habitat"
                    ,Characteristic_Name == "small boulders present (<2m, but larger than cobble), absent, extensive" ~ "stream abiotic - relative abundance - small boulders (<= 2 m)"
                    ,Characteristic_Name == "sand present, absent, extensive" ~ "stream abiotic - relative abundance - sand"
                    ,Characteristic_Name == "silt/clay present, absent, extensive" ~ "stream abiotic - relative abundance - silt/clay"
                    ,Characteristic_Name == "undercut banks present, absent, extensive" ~ "stream abiotic - relative abundance - undercut banks"
                    ,Characteristic_Name == "undercut banks" ~ "stream abiotic - relative abundance - undercut banks"
                    ,Characteristic_Name == "rootwad/woody debris" ~ "stream abiotic - relative abundance - rootwad/woody debris"
                    ,Characteristic_Name == "submerged aquatic vegetation" ~ "aquatic vegetation relative abundance - submerged aquatic plants"
                    ,Characteristic_Name == "emergent aquatic vegetation" ~ "aquatic vegetation relative abundance - emergent aquatic plants"
                    ,Characteristic_Name == "floating aquatic vegetation" ~ "aquatic vegetation relative abundance - floating aquatic plants"
                    ,Characteristic_Name == "percentage of rocks (gravel, cobble, and boulders) that are surrounded by, covered, or sunken into the silt, sand, or mud of the stream" ~ "stream abiotic - embeddedness - percentage of rocks (gravel, cobble, boulders) covered by sediment (sand, silt)"
                    ,Characteristic_Name == "percentage of hard substrates that are surrounded by fine sediments (0-100%)" ~ "stream abiotic - embeddedness - percentage of rocks (gravel, cobble, boulders) covered by sediment (sand, silt)"
                    ,Characteristic_Name == "erosion severity right bank (0=none; 1=minor; 2=moderate; 3=severe)" ~ "stream abiotic - right bank - stream erosion severity"
                    ,Characteristic_Name == "bar severity top right bank (0=none; 1=minor; 2=moderate; 3=severe)" ~ "stream abiotic - right bank - stream bar severity"
                    ,Characteristic_Name == "geomorphology sampleability" ~ "sampleability - geomorphology"
                    ,Characteristic_Name == "electrofishing sampleability" ~ "sampleability - electrofishing"
                    ,Characteristic_Name == "indicates if electrofishing could be conducted in culvert during summer visit" ~ "sampleability - electrofishing"
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
                    ,Characteristic_Name == "number of large wood pieces in wetted stream" ~ "stream abiotic - coarse woody debris in wetted stream"
                    ,Characteristic_Name == "number of large wood pieces in active channel but currently dewatered" ~ "stream abiotic - coarse woody debris in stream channel but currently dewatered"
                    ,Characteristic_Name == "number of root wads in wetted stream" ~ "stream abiotic - root wads in wetted stream"
                    ,Characteristic_Name == "number of root wads in active channel but currently dewatered" ~ "stream abiotic - root wads in stream channel but currently dewatered"
                    ,Characteristic_Name == "left bank riparian width (m)" ~ "stream abiotic - left bank - riparian width"
                    ,Characteristic_Name == "riparian vegetation buffer broken on left bank (yes/no)" ~ "stream abiotic - left bank - riparian buffer broken"
                    ,Characteristic_Name == "left bank adjacent land cover" ~ "stream abiotic - left bank - adjacent land cover"
                    ,Characteristic_Name == "right bank adjacent land cover" ~ "stream abiotic - right bank - adjacent land cover"
                    ,Characteristic_Name == "adjacent land cover left bank" ~ "stream abiotic - left bank - adjacent land cover"
                    ,Characteristic_Name == "adjacent land cover righ bank" ~ "stream abiotic - right top bank - adjacent land cover"
                    ,Characteristic_Name == "left bank dredge spoil" ~ "stream abiotic - relative abundance - left bank - dredge spoil"
                    ,Characteristic_Name == "right bank dredge spoil" ~ "stream abiotic - relative abundance - right bank - dredge spoil"
                    ,Characteristic_Name == "left bank buffer breaks" ~ "stream abiotic - left bank - buffer breaks"
                    ,Characteristic_Name == "right bank buffer breaks" ~ "stream abiotic - right bank - buffer breaks"
                    ,Characteristic_Name == "right bank riparian width (m)" ~ "stream abiotic - right bank - riparian width"
                    ,Characteristic_Name == "riparian vegetation buffer broken on right bank (yes/no)" ~ "stream abiotic - right bank - riparian buffer broken"
                    ,Characteristic_Name == "left dominant substrate at 50m-75m" ~ "stream abiotic - 50m-75m - river-right dominant substrate"
                    ,Characteristic_Name == "left subdominant substrate at 50m-75m" ~ "stream abiotic - 50m-75m - river-right subdominant substrate"
                    ,Characteristic_Name == "left depth at 50m-75m" ~ "stream abiotic - 50m-75m - river-right water depth"
                    ,Characteristic_Name == "left velocity at 50m-75m" ~ "stream abiotic - 50m-75m - river-right water velocity"
                    ,Characteristic_Name == "right dominant substrate at 50m-75m" ~ "stream abiotic - 50m-75m - river-left dominant substrate"
                    ,Characteristic_Name == "right subdominant substrate at 50m-75m" ~ "stream abiotic - 50m-75m - river-left subdominant substrate"
                    ,Characteristic_Name == "right depth at 50m-75m" ~ "stream abiotic - 50m-75m - river-left water depth"
                    ,Characteristic_Name == "right velocity at 50m-75m" ~ "stream abiotic - 50m-75m - river-left water velocity"
                    ,Characteristic_Name == "left dominant substrate at 25m-50m" ~ "stream abiotic - 25m-50m - river-right dominant substrate"
                    ,Characteristic_Name == "left subdominant substrate at 25m-50m" ~ "stream abiotic - 25m-50m -river-right subdominant substrate"
                    ,Characteristic_Name == "left depth at 25m-50m" ~ "stream abiotic - 25m-50m - river-right water depth"
                    ,Characteristic_Name == "left velocity at 25m-50m" ~ "stream abiotic - 25m-50m - river-right water velocity"
                    ,Characteristic_Name == "right dominant substrate at 25m-50m" ~ "stream abiotic - 25m-50m - river-left dominant substrate"
                    ,Characteristic_Name == "right subdominant substrate at 25m-50m" ~ "stream abiotic - 25m-50m - river-left subdominant substrate"
                    ,Characteristic_Name == "right depth at 25m-50m" ~ "stream abiotic - 25m-50m - river-left water depth"
                    ,Characteristic_Name == "right velocity at 25m-50m" ~ "stream abiotic - 25m-50m - river-left water velocity"
                    ,Characteristic_Name == "left dominant substrate at 0m-25m" ~ "stream abiotic - 0m-25m - river-right dominant substrate"
                    ,Characteristic_Name == "left subdominant substrate at 0m-25m" ~ "stream abiotic - 0m-25m -river-right subdominant substrate"
                    ,Characteristic_Name == "left depth at 0m-25m" ~ "stream abiotic - 0m-25m - river-right water depth"
                    ,Characteristic_Name == "left velocity at 0m-25m" ~ "stream abiotic - 0m-25m - river-right water velocity"
                    ,Characteristic_Name == "right dominant substrate at 0m-25m" ~ "stream abiotic - 0m-25m - river-left dominant substrate"
                    ,Characteristic_Name == "right subdominant substrate at 0m-25m" ~ "stream abiotic - 0m-25m - river-left subdominant substrate"
                    ,Characteristic_Name == "right depth at 0m-25m" ~ "stream abiotic - 0m-25m - river-left water depth"
                    ,Characteristic_Name == "right velocity at 0m-25m" ~ "stream abiotic - 0m-25m - river-left water velocity"
                    ,Characteristic_Name == "dominant substrate of bars present is cobble (yes/no)" ~ "stream abiotic - dominant substrate of bars - cobble"
                    ,Characteristic_Name == "dominant substrate of bars present is gravel (yes/no)" ~ "stream abiotic - dominant substrate of bars - gravel"
                    ,Characteristic_Name == "dominant substrate of bars present is sand (yes/no)" ~ "stream abiotic - dominant substrate of bars - sand"
                    ,Characteristic_Name == "dominant substrate of bars present is silt/clay (yes/no)" ~ "stream abiotic - dominant substrate of bars - silt or clay"
                    ,Characteristic_Name == "presence of gabions on left bank" ~ "stream abiotic - left bank - extent - length of stream channelized by gabion"
                    ,Characteristic_Name == "presence of gabions on right bank" ~ "stream abiotic - right bank - extent - length of stream channelized by gabion"
                    ,Characteristic_Name == "presence of gabions on stream bottom" ~ "stream abiotic - stream bottom - extent  - length of stream channelized by gabion"
                    ,Characteristic_Name == "presence of concrete on left bank" ~ "stream abiotic - left bank - extent - length of stream channelized by concrete"
                    ,Characteristic_Name == "presence of concrete on right bank" ~ "stream abiotic - right bank - extent - length of stream channelized by concrete"
                    ,Characteristic_Name == "presence of concrete on stream bottom" ~ "stream abiotic - stream bottom - extent - length of stream channelized by concrete"
                    ,Characteristic_Name == "presence of dredge spoil on left bank" ~ "stream abiotic - left bank - extent - length of stream channelized by dredge spoil"
                    ,Characteristic_Name == "presence of dredge spoil on right bank" ~ "stream abiotic - right bank - extent - length of stream channelized by dredge spoil"
                    ,Characteristic_Name == "presence of dredge spoil on stream bottom" ~ "stream abiotic - stream bottom - extent - length of stream channelized by dredge spoil"
                    ,Characteristic_Name == "presence of earthern berm on left bank" ~ "stream abiotic - left bank - extent - length of stream channelized by earthern berm"
                    ,Characteristic_Name == "presence of earthern berm on right bank" ~ "stream abiotic - right bank - extent - length of stream channelized by earthern berm"
                    ,Characteristic_Name == "presence of earthern berm on stream bottom" ~ "stream abiotic - stream bottom - extent - length of stream channelized by earthern berm"
                    ,Characteristic_Name == "presence of riprap on left bank" ~ "stream abiotic - left bank - extent - length of stream channelized by earthern berm"
                    ,Characteristic_Name == "presence of riprap on right bank" ~ "stream abiotic - right bank - extent - length of stream channelized by earthern berm"
                    ,Characteristic_Name == "presence of riprap on stream bottom" ~ "stream abiotic - stream bottom - extent - length of stream channelized by earthern berm"
                    ,Characteristic_Name == "presence of pipe on left bank" ~ "stream abiotic - left bank - extent - length of stream channelized by pipe"
                    ,Characteristic_Name == "presence of pipe on right bank" ~ "stream abiotic - right bank - extent - length of stream channelized by gabion"
                    ,Characteristic_Name == "presence of pipe on stream bottom" ~ "stream abiotic - stream bottom - extent - length of stream channelized by gabion"
                    ,Characteristic_Name == "right bank gabion" ~ "stream abiotic - right bank - extent - length of stream channelized by gabion"
                    ,Characteristic_Name == "left bank gabion" ~ "stream abiotic - left bank - extent - length of stream channelized by gabion"
                    ,Characteristic_Name == "right bank earthen berm" ~ "stream abiotic - right bank - extent - stream channelized by earthen berm"
                    ,Characteristic_Name == "left bank earthen berm" ~ "stream abiotic - left bank - extent - stream channelized earthen berm"
                    ,Characteristic_Name == "right bank pipe culvert" ~ "stream abiotic - right bank - extent - stream channelized by pipe culvert"
                    ,Characteristic_Name == "left bank pipe culvert" ~ "stream abiotic - left bank - extent - stream channelized pipe culvert"
                    ,Characteristic_Name == "length of culvert if present" ~ "stream abiotic - either bank - extent - stream channelized pipe culvert"
                    ,Characteristic_Name == "diameter of culvert" ~ "stream abiotic - either bank - diameter of culvert"
                    ,Characteristic_Name == "channelization present" ~ "stream abiotic - either bank - relative abundance - channelization"
                    ,Characteristic_Name == "presence/absence of culvert in sample reach" ~ "stream abiotic - either bank - relative abundance - culvert"
                    ,Characteristic_Name == "culvert in segment" ~ "stream abiotic - either bank - relative abundance - stream channelized pipe culvert"
                    ,Characteristic_Name == "right bank rip-rap" ~ "stream abiotic - right bank - extent - stream channelized by rip-rap"
                    ,Characteristic_Name == "left bank rip-rap" ~ "stream abiotic - left bank - extent - stream channelized rip-rap"
                    ,Characteristic_Name == "wetted channel width - bottom of site (0m)" ~ "stream abiotic - 0m  - wetted channel width"
                    ,Characteristic_Name == "wetted channel width (m) at 25 m from bottom of site" ~ "stream abiotic - 25m - wetted channel width"
                    ,Characteristic_Name == "wetted channel width (m) at 50 m from bottom of site" ~ "stream abiotic - 50m - wetted channel width"
                    ,Characteristic_Name == "wetted channel width (m) at top (75m) of site" ~ "stream abiotic - 75m - wetted channel width"
                    ,Characteristic_Name == "thalweg depth (cm) at bottom of site" ~ "stream abiotic - 0m - thalweg depth"
                    ,Characteristic_Name == "thalweg depth (cm) at 25 m from bottom of site" ~ "stream abiotic - 25m - thalweg depth"
                    ,Characteristic_Name == "thalweg depth (cm) at 50 m from bottom of site" ~ "stream abiotic - 50m - thalweg depth"
                    ,Characteristic_Name == "thalweg depth (cm) at top (75 m) of site" ~ "stream abiotic - 75m - thalweg depth"
                    ,Characteristic_Name == "thalweg velocity (m/s) at bottom of site" ~ "stream abiotic - 0m - thalweg velocity"
                    ,Characteristic_Name == "thalweg velocity (m/s) at 25 m from bottom of site" ~ "stream abiotic - 25m - thalweg velocity"
                    ,Characteristic_Name == "thalweg velocity (m/s) at 50 m from bottom of site" ~ "stream abiotic - 50m - thalweg velocity"
                    ,Characteristic_Name == "thalweg velocity (m/s) at top (75 m) of site" ~ "stream abiotic - 75m - thalweg velocity"
                    ,Characteristic_Name == "wetted channel width (m) at bottom (0m) of site" ~ "stream abiotic - 0m - wetted channel width"
                    ,Characteristic_Name == "wetted channel width - bottom of site (0m)" ~ "stream abiotic - 0m - wetted channel width"
                    ,Characteristic_Name == "wetted channel width - top of site (75m)" ~ "stream abiotic - 75m - wetted channel width"
                    ,Characteristic_Name == "diversity and quality of water velocity and depths habitat score (0-20)" ~ "stream abiotic - habitat quality - water velocity and depth"
                    ,Characteristic_Name == "instream habitat score (0-20)" ~ "stream abiotic - habitat quality - instream habitat"
                    ,Characteristic_Name == "diversity and quality of pool, glide, and eddy habitats score (0-20)" ~ "stream abiotic - habitat quality - pool, glide, eddy"
                    ,Characteristic_Name == "epifaunal substrate habitat score (0-20)" ~ "stream abiotic - habitat quality - epifaunal substrate"
                    ,Characteristic_Name == "velocity" ~ "stream abiotic - water velocity associated with a stream lateral location"
                    ,Characteristic_Name == "depth" ~ "stream abiotic - water depth associated with a stream lateral location"
                    ,Characteristic_Name == "lat loc" ~ "stream abiotic - stream lateral location"
                    ,Characteristic_Name == "latitude of stream blockage" ~ "stream abiotic - stream obstruction - latitude"
                    ,Characteristic_Name == "longitude of stream blockage" ~ "stream abiotic - stream obstruction - longitude"
                    ,Characteristic_Name == "stream gradient measurement location (distance from bottom)" ~ "stream abiotic - stream gradient measurement location (meters above site-bottom)"
                    ,Characteristic_Name == "stream gradient height measurement (meters)" ~ "stream abiotic - stream gradient height measurement (meters above stream gradient measurement location)"
                    ,Characteristic_Name == "stream gradient" ~ "stream abiotic - stream gradient (percent slope)"
                    ,Characteristic_Name == "channel slope (%) using a surveyor's level and rod" ~ "stream abiotic - stream gradient (percent slope)"
                    ,Characteristic_Name == "vernal pool present (y/n)" ~ "stream abiotic - relative abundance - vernal pool"
                    ,Characteristic_Name == "vernal pool" ~ "stream abiotic - relative abundance - vernal pool"
                    ,Characteristic_Name == "presence of commercial/industrial in close proximity of sampling site" ~ "stream abiotic - relative abundance - commercial or industrial development"
                    ,Characteristic_Name == "acid neutralizing capacity" ~ "stream abiotic - chemistry - acid neutralizing capacity"
                    ,Characteristic_Name == "sulfate (mg/l)" ~ "stream abiotic - chemistry - sulfate concentration"
                    ,Characteristic_Name == "sulfate concentration" ~ "stream abiotic - chemistry - sulfate concentration"
                    ,Characteristic_Name == "altitude (feet) from degital elevation model" ~ "stream abiotic - elevation above sea level at sampling location"
                    ,Characteristic_Name == "ammonia concentration" ~ "stream abiotic - chemistry - ammonia concentration"
                    ,Characteristic_Name == "chloride (mg/l)" ~ "stream abiotic - chemistry - chloride concentration"
                    ,Characteristic_Name == "chloride concentration" ~ "stream abiotic - chemistry - chloride concentration"
                    ,Characteristic_Name == "dissolved organic carbon (mg/l)" ~ "stream abiotic - chemistry - dissolved organic carbon concentration"
                    ,Characteristic_Name == "dissolved organic carbon concentration" ~ "stream abiotic - chemistry - dissolved organic carbon concentration"
                    ,Characteristic_Name == "nitrate-n (mg/l)" ~ "stream abiotic - chemistry - nitrate concentration"
                    ,Characteristic_Name == "nitrite-n (mg/l)" ~ "stream abiotic - chemistry - nitrite concentration"
                    ,Characteristic_Name == "source of any acidity" ~ "stream abiotic - chemistry - source of any acidity"
                    ,Characteristic_Name == "nitrate nitrogen concentration" ~ "stream abiotic - chemistry - nitrate concentration"
                    ,Characteristic_Name == "nitrite nitrogen concentration" ~ "stream abiotic - chemistry - nitrite concentration"
                    ,Characteristic_Name == "ortho-phosphate concentration" ~ "stream abiotic - chemistry - orthophosphate concentration"
                    ,Characteristic_Name == "orthophosphate (mg/l)" ~ "stream abiotic - chemistry - orthophosphate concentration"
                    ,Characteristic_Name == "specific conductance" ~ "stream abiotic - chemistry - specific conductance"
                    ,Characteristic_Name == "total ammonia nitrogen (mg/l)" ~ "stream abiotic - chemistry - ammonia concentration"
                    ,Characteristic_Name == "total nitrogen concentration" ~ "stream abiotic - chemistry - total nitrogen concentration"
                    ,Characteristic_Name == "total phosphorus concentration" ~ "stream abiotic - chemistry - total phosphorus concentration"
                    ,Characteristic_Name == "closed ph value" ~ "stream abiotic - chemistry - closed pH value"
                    ,Characteristic_Name == "straight line length of segment" ~ "stream abiotic - straight-line length of 75m reach"
                    ,Characteristic_Name == "stream blockage type (p52)" ~ "stream abiotic - stream obstruction - type"
                    ,Characteristic_Name == "height of stream blockage (meters) if present" ~ "stream abiotic - stream obstruction - height"
                    ,Characteristic_Name == "aesthetic rating ranging from 0 (lowest) to 20 (highest) rating visual appeal of stream and absence of trash" ~ "stream abiotic - aesthetic appeal of sampling site (related to the lack of trash)"
                    ,Characteristic_Name == "trash rating" ~ "stream abiotic - aesthetic appeal of sampling site (related to the lack of trash)"
                    ,Characteristic_Name == "water logger dewatered" ~ "stream abiotic - water logger dewatered status"
                    ,Characteristic_Name == "water quality qc sample collected." ~ "stream abiotic - water quality-control sample collected"
                    ,Characteristic_Name == "water quality duplicate or blank sample collected for quality control" ~ "stream abiotic - water quality-control sample collected"
                    ,Characteristic_Name == "sample is a blank or a duplicate" ~ "stream abiotic - water quality-control sample collected"
                    ,Characteristic_Name == "photodocumentation" ~ "instrumentation or event data - photos taken at site"
                    ,Characteristic_Name == "year of quality control sample" ~ "instrumentation or event data - year of quality control sample"
                    ,Characteristic_Name == "stream segment id number for quality control sample" ~ "instrumentation or event data - stream segment id number for quality control sample"
                    ,Characteristic_Name == "site type for qc sample (r=random; s=sentinel; n=national park service; t=targeted)" ~ "instrumentation or event data - site type for quality control sample"
                    ,Characteristic_Name == "serial code number of temperature logger (if deployed)" ~ "instrumentation or event data - temperature logger serial number"
                    ,Characteristic_Name == "air logger present" ~ "instrumentation or event data - air logger deployed"
                    ,Characteristic_Name == "indicator if temperature logger was deployed (y=yes; n=no)" ~ "instrumentation or event data - temperature logger deployed"
                    ,Characteristic_Name == "orange floc iron bacteria surface film present, absent, extensive" ~ "stream biotic - relative abundance - orange floc iron bacteria"
                    ,Characteristic_Name == "didymo presence" ~ "stream biotic - relative abundance - didymo"
                    ,Characteristic_Name == "didymo voucher" ~ "stream biotic - voucher - didymo"
                    ,Characteristic_Name == "distance from road (m)" ~ "stream abiotic - straight-line distance to nearest road"
                    ,Characteristic_Name == "distance to nearest road" ~ "stream abiotic - straight-line distance to nearest road"
                    ,Characteristic_Name == "individual fish mass (weight) in g" ~ "stream biota - fish - individual fish mass"
                    ,Characteristic_Name == "individual fish total length in mm" ~ "stream biota - fish - individual fish total length"
                    ,Characteristic_Name == "gps location of latitude (only present if coordinates do not fall within 30m of stream or on samplable reach)" ~ "instrumentation or event data - sampling location latitude"
                    ,Characteristic_Name == "gps location of longitude (only present if coordinates do not fall within 30m of stream or on samplable reach)" ~ "instrumentation or event data - sampling location longitude"
                    ,TRUE ~ Characteristic_Name
                )
            ) %>%
            mutate(
                Result_Unit = case_when(
                    Characteristic_Name == "stream biotic - exotic terrestrial plant relative abundance - other species" ~ "species name"
                    ,Characteristic_Name %like% "relative abundance" ~ "A = absent, P = present, E = extensive"
                    ,Characteristic_Name %like% "benthic macroinvertebrates" ~ "square feet"
                    ,Characteristic_Name %like% "sampleability" ~ "S = Sampleable"
                    ,Characteristic_Name %like% "float time" ~ "seconds"
                    ,Characteristic_Name %like% "stream erosion severity" ~ "0 = None; 1 = Minor; 2 = Moderate; 3 = Severe"
                    ,Characteristic_Name == "stream abiotic - coarse woody debris in stream channel but currently dewatered" ~ "count"
                    ,Characteristic_Name == "stream abiotic - coarse woody debris in wetted stream" ~ "count"
                    ,Characteristic_Name == "stream abiotic - root wads in stream channel but currently dewatered" ~ "count"
                    ,Characteristic_Name == "stream abiotic - root wads in wetted stream" ~ "count"
                    ,Characteristic_Name == "stream abiotic - stream obstruction - height" ~ "meters"
                    ,Characteristic_Name %like% "linear extent" ~ "meters"
                    ,Characteristic_Name %like% "straight line length" ~ "meters"
                    ,Characteristic_Name %like% "length of stream obstructed by" ~ "meters"
                    ,Characteristic_Name %like% "wetted channel width" ~ "meters"
                    ,Characteristic_Name %like% "stream gradient measurement location" ~ "meters"
                    ,Characteristic_Name %like% "stream gradient height measurement" ~ "meters"
                    ,Characteristic_Name %like% "lateral stream location" ~ "meters"
                    ,Characteristic_Name %like% "elevation above sea level" ~ "meters"
                    ,Characteristic_Name %like% "riparian width" ~ "meters"
                    ,Characteristic_Name %like% "length of stream channelized" ~ "meters"
                    ,Characteristic_Name %like% "percent slope" ~ "percent slope"
                    ,Characteristic_Name == "stream abiotic - overhead cover" ~ "percent"
                    ,Characteristic_Name %like% "thalweg depth" ~ "centimeters"
                    ,Characteristic_Name %like% "reach maximum depth" ~ "centimeters"
                    ,Characteristic_Name %like% "thalweg velocity" ~ "meters per second"
                    ,Characteristic_Name %like% "sulfate" ~ "mg/L"
                    ,Characteristic_Name %like% "ammonia" ~ "mg/L"
                    ,Characteristic_Name %like% "chloride" ~ "mg/L"
                    ,Characteristic_Name %like% "nitrate" ~ "mg/L"
                    ,Characteristic_Name %like% "orthophosphate" ~ "mg/L"
                    ,Characteristic_Name %like% "dissolved organic carbon" ~ "mg/L"
                    ,Characteristic_Name %like% "acid neutralizing capacity" ~ "ueq/L"
                    ,Characteristic_Name %like% "specific conductance" ~ "uS/cm"
                    ,Characteristic_Name %like% "latitude" ~ "decimal degrees"
                    ,Characteristic_Name %like% "longitude" ~ "decimal degrees"
                    ,Characteristic_Name == "stream abiotic - stream obstruction - type" ~ "DM = dam; PC = pipe culvert; F = fishway; FW = gaging station weir; G = gabion; PX = pipeline crossing; AC = arch culvert; BC = box culvert; TG = tide gate"
                    ,Characteristic_Name == "stream abiotic - water logger dewatered status" ~ "N = not dewatered; Y = dewatered"
                    ,Characteristic_Name == "stream abiotic - water quality-control sample collected" ~ "N = not collected; Y = collected"
                    ,Characteristic_Name %like% "dominant riparian buffer terrestrial vegetation" ~ "(G = grasses/forbes; R = regenerating deciduous/shrubs; Y = young deciduous; M = mature deciduous; O = old deciduous; A = regenerating coniferous; B = young coniferous; C = mature coniferous; D = lawn; W = wetland)"
                    ,Characteristic_Name %like% "aesthetic appeal of sampling site" ~ "0 (lowest) to 20 (highest)"
                    ,Characteristic_Name == "stream abiotic - habitat quality - riffle" ~ "0 (lowest) to 20 (highest)"
                    ,Characteristic_Name == "stream abiotic - habitat quality - water velocity and depth" ~ "0 (lowest) to 20 (highest)"
                    ,Characteristic_Name == "stream abiotic - habitat quality - instream habitat" ~ "0 (lowest) to 20 (highest)"
                    ,Characteristic_Name == "stream abiotic - habitat quality - pool, glide, eddy" ~ "0 (lowest) to 20 (highest)"
                    ,Characteristic_Name == "stream abiotic - habitat quality - epifaunal substrate" ~ "0 (lowest) to 20 (highest)"
                    ,Characteristic_Name == "instrumentation or event data - photos taken at site" ~ "N = not collected; Y = collected"
                    ,Characteristic_Name == "instrumentation or event data - temperature logger deployed" ~ "N = not deployed; Y = deployed"
                    ,Characteristic_Name == "instrumentation or event data - year of quality control sample" ~ "year"
                    ,Characteristic_Name == "instrumentation or event data - stream segment id number for quality control sample" ~ "id number"
                    ,Characteristic_Name == "instrumentation or event data - temperature logger serial number" ~ "id number"
                    ,Characteristic_Name == "instrumentation or event data - site type for quality control sample" ~ "R = random; S = sentinel; N = national park service; T = targeted"
                    ,Characteristic_Name == "stream abiotic - straight-line distance to nearest road" ~ "meters"
                    ,Characteristic_Name == "stream biotic - voucher - didymo" ~ "N = not vouchered; Y = vouchered"
                    ,Characteristic_Name == "stream abiotic - left bank - riparian buffer broken" ~ "N = not broken; Y = broken"
                    ,Characteristic_Name == "stream abiotic - right bank - riparian buffer broken" ~ "N = not broken; Y = broken"
                    ,Characteristic_Name == "stream abiotic - embeddedness - percentage of rocks (gravel, cobble, boulders) covered by sediment (sand, silt)" ~ "percent"
                    ,Characteristic_Name %like% "closed pH value" ~ "pH"
                    ,Characteristic_Name %like% "dominant substrate of bars" ~ "TRUE or FALSE"
                    ,Characteristic_Name == "stream biota - fish - individual fish mass" ~ "grams"
                    ,Characteristic_Name == "stream biota - fish - individual fish total length" ~ "millimeters"
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
                ,Characteristic_Name = gsub("alternate flow measurement", "stream abiotic - alternate discharge measurement -", Characteristic_Name)
            ) %>%
            filter(
                Characteristic_Name != "8 digit watershed code for sampled site" # this is not a result; it's a datum associated with the sampling location, which belongs in (and is reported in) edd.locations
                ,Characteristic_Name != "unique identifier of sampling site" # this is not a result; it's a datum associated with the sampling location, which belongs in (and is reported in) edd.locations
                ,Characteristic_Name != "benthos" # this is a boolean that simply says whether bob sampled for macroinverts at this Activity_ID; it's not a datum
                ,Characteristic_Name != "sampleable?" # this is a boolean that simply says whether bob could sample the site; it's not a datum
                ,Characteristic_Name != "habitat assessment" # this is a boolean that simply says whether bob could do a habitat assessment at the site; it's not a datum
                ,Characteristic_Name != "crew" # filter out field crew names (PII)
                ,Characteristic_Name != "limph1" # this is a nonsense id
            ) %>%
            mutate(Result_Qualifier = case_when(
                Characteristic_Name == "stream abiotic - chemistry - orthophosphate concentration" & Result_Text == "< 0.0032" ~ "reported as undetectable; < 0.0032"
                ,Characteristic_Name == "stream abiotic - chemistry - ammonia concentration" & Result_Text == "< 0.0045" ~ "reported as undetectable; < 0.0045"
                ,Characteristic_Name == "stream abiotic - chemistry - nitrate concentration" & Result_Text == "< 0.0043" ~ "reported as undetectable; < 0.0043"
                ,Characteristic_Name == "instrumentation or event data - sampling location longitude" ~ "only present if coordinates do not fall within 30m of stream or on samplable reach"
                ,Characteristic_Name == "instrumentation or event data - sampling location latitude" ~ "only present if coordinates do not fall within 30m of stream or on samplable reach"
            )) %>%
            mutate(
                Subject_Taxon = tolower(Subject_Taxon)
                ,Subject_Taxon = trimws(Subject_Taxon, which="both")
                ,Subject_Taxon = case_when(
                    Subject_Taxon == "thienemannimyia group" ~ "thienemannimyia"
                    ,Subject_Taxon == "sf chironominae" ~ "chironominae"
                    ,Subject_Taxon == "sf tanypodinae" ~ "tanypodinae"
                    ,Subject_Taxon == "bezzia sp group" ~ "bezzia"
                    ,Subject_Taxon == "sf tanypodinae" ~ "tanypodinae"
                    ,Subject_Taxon == "cottus sp. n." ~ "cottus spp."
                    ,Subject_Taxon == "cottus n.sp." ~ "cottus spp."
                    ,TRUE ~ Subject_Taxon
                )) %>%
            mutate(Result_Text = case_when(
                Characteristic_Name == "sampleability - electrofishing" & Result_Text == "10" ~ "S" # this is a typo in the source file
                ,Characteristic_Name == "sampleability - water quality" & Result_Text == "Y" ~ "S" # this is a typo in the source file
                ,Characteristic_Name %like% "gabion relative abundance" & Result_Text == "0" ~ "A" # this is a typo in the source file
                ,Characteristic_Name %like% "gabion relative abundance" & Result_Text == "N" ~ "A" # this is a typo in the source file
                ,Characteristic_Name %like% "vernal pool relative abundance" & Result_Text == "N" ~ "A" # this is a typo in the source file
                ,Characteristic_Name %like% "vernal pool relative abundance" & Result_Text == "Y" ~ "P" # this is a typo in the source file
                ,Characteristic_Name == "stream abiotic - chemistry - nitrate concentration" & Result_Text == "< 0.0043" ~ "0.0043" # undetectable
                ,Characteristic_Name == "stream abiotic - chemistry - nitrate concentration" & Result_Text == "2.5100000000000001E-2" ~ "0.0251" # not a number
                ,Characteristic_Name == "stream abiotic - chemistry - nitrate concentration" & Result_Text == "2.12E-2" ~ "0.0212" # not a number
                ,Characteristic_Name == "stream abiotic - chemistry - nitrate concentration" & Result_Text == "0.26790000000000003" ~ "0.2679" # sig figs
                ,Characteristic_Name == "stream abiotic - chemistry - nitrate concentration" & Result_Text == "1.8100000000000002E-2" ~ "0.0181" # sig figs
                ,Characteristic_Name == "stream abiotic - chemistry - ammonia concentration" & Result_Text == "1.11E-2" ~ "0.011" # number
                ,Characteristic_Name == "stream abiotic - chemistry - ammonia concentration" & Result_Text == "6.3E-3" ~ "0.0063" # number
                ,Characteristic_Name == "stream abiotic - chemistry - ammonia concentration" & Result_Text == "2.5700000000000001E-2" ~ "0.0257" # sig figs
                ,Characteristic_Name == "stream abiotic - chemistry - ammonia concentration" & Result_Text == "4.7999999999999996E-3" ~ "0.0048" # sig figs
                ,Characteristic_Name == "stream abiotic - chemistry - ammonia concentration" & Result_Text == "5.1999999999999998E-3" ~ "0.0052" # sig figs
                ,Characteristic_Name == "stream abiotic - chemistry - ammonia concentration" & Result_Text == "< 0.0045" ~ NA # undetectable
                ,Characteristic_Name == "stream abiotic - chemistry - orthophosphate concentration" & Result_Text == "5.3E-3" ~ "0.0053" # sig figs
                ,Characteristic_Name == "stream abiotic - chemistry - orthophosphate concentration" & Result_Text == "6.0000000000000001E-3" ~ "0.0060" # sig figs
                ,Characteristic_Name == "stream abiotic - chemistry - orthophosphate concentration" & Result_Text == "1.4E-3" ~ "0.0014" # sig figs
                ,Characteristic_Name == "stream abiotic - chemistry - orthophosphate concentration" & Result_Text == "3.8999999999999998E-3" ~ "0.0039" # sig figs
                ,Characteristic_Name == "stream abiotic - chemistry - orthophosphate concentration" & Result_Text == "< 0.0032" ~ NA # undetectable
                ,Characteristic_Name == "stream abiotic - chemistry - orthophosphate concentration" & Result_Text == "3.3E-3" ~ "0.0033" # sig figs
                ,Characteristic_Name == "stream abiotic - chemistry - source of any acidity" & Result_Text == "ORG" ~ "unspecified organic sources"
                ,Characteristic_Name == "stream abiotic - chemistry - source of any acidity" & Result_Text == "NONE" ~ NA
                ,Characteristic_Name == "stream abiotic - chemistry - source of any acidity" & Result_Text == "none" ~ NA
                ,Characteristic_Name == "stream abiotic - water quality-control sample collected" & Result_Text == "0" ~ "N"
                ,Characteristic_Name == "stream abiotic - either bank - relative abundance - stream channelized pipe culvert" & Result_Text == "N" ~ "A"
                ,Characteristic_Name == "stream abiotic - either bank - relative abundance - channelization" & Result_Text == "1" ~ "A"
                ,Characteristic_Name == "stream abiotic - either bank - relative abundance - channelization" & Result_Text == "0" ~ "P"
                ,Characteristic_Name == "stream biotic - relative abundance - didymo" & Result_Text == "N" ~ "A"
                ,Characteristic_Name == "stream abiotic - either bank - relative abundance - culvert" & Result_Text == "0" ~ "A"
                ,Characteristic_Name == "stream abiotic - either bank - relative abundance - culvert" & Result_Text == "1" ~ "P"
                ,Characteristic_Name == "stream abiotic - either bank - relative abundance - culvert" & Result_Text == "N" ~ "A"
                ,Characteristic_Name == "sampleability - electrofishing" & Result_Text == "0" ~ "N"
                ,Characteristic_Name == "stream abiotic - top left bank - riparian buffer broken" & Result_Text == "0" ~ "N"
                ,Characteristic_Name == "stream abiotic - top left bank - riparian buffer broken" & Result_Text == "1" ~ "N"
                ,Characteristic_Name == "stream abiotic - top right bank - riparian buffer broken" & Result_Text == "0" ~ "N"
                ,Characteristic_Name == "stream abiotic - top right bank - riparian buffer broken" & Result_Text == "1" ~ "N"
                ,Characteristic_Name == "stream biotic - either bank - dominant riparian buffer terrestrial vegetation" & Result_Text == "Deciduous Forest" ~ "M"
                ,Characteristic_Name == "stream biotic - either bank - dominant riparian buffer terrestrial vegetation" & Result_Text == "Decidious Forest" ~ "M"
                ,Characteristic_Name == "stream biotic - either bank - dominant riparian buffer terrestrial vegetation" & Result_Text == "Old Field" ~ "R"
                ,Characteristic_Name == "stream biotic - either bank - dominant riparian buffer terrestrial vegetation" & Result_Text == "Wetland" ~ "W"
                ,Characteristic_Name == "instrumentation or event data - sampling location latitude" & Result_Text == "38 56 24" ~ "38.94" # degree min sec to decimal degrees
                ,Characteristic_Name == "instrumentation or event data - sampling location latitude" & Result_Text == "385550" ~ "38.5550" # orders of magnitude
                ,Characteristic_Name == "instrumentation or event data - sampling location latitude" & Result_Text == "392833" ~ "39.2833"
                ,Characteristic_Name == "instrumentation or event data - sampling location latitude" & Result_Text == "392741" ~ "39.2741"
                ,Characteristic_Name == "instrumentation or event data - sampling location latitude" & Result_Text == "392148" ~ "39.2148"
                ,Characteristic_Name == "instrumentation or event data - sampling location latitude" & Result_Text == "392128" ~ "39.2128"
                ,Characteristic_Name == "instrumentation or event data - sampling location latitude" & Result_Text == "392207" ~ "39.2207"
                ,Characteristic_Name == "instrumentation or event data - sampling location longitude" & Result_Text == "-77 15 41" ~ "-77.261389" # degree min sec to decimal degrees
                ,Characteristic_Name == "instrumentation or event data - sampling location longitude" & Result_Text == "770726" ~ "-77.0726" # orders of magnitude
                ,Characteristic_Name == "instrumentation or event data - sampling location longitude" & Result_Text == "774417" ~ "-77.4417"
                ,Characteristic_Name == "instrumentation or event data - sampling location longitude" & Result_Text == "774351" ~ "-77.4351"
                ,Characteristic_Name == "instrumentation or event data - sampling location longitude" & Result_Text == "772324" ~ "-77.2324"
                ,Characteristic_Name == "instrumentation or event data - sampling location longitude" & Result_Text == "772349" ~ "-77.2349"
                ,Characteristic_Name == "instrumentation or event data - sampling location longitude" & Result_Text == "772318" ~ "-77.2318"
                ,Characteristic_Name == "instrumentation or event data - sampling location longitude" & Result_Text == "77.041638" ~ "-77.041638"
                ,Characteristic_Name == "instrumentation or event data - sampling location longitude" & Result_Text == "77.043842" ~ "-77.043842"
                ,Characteristic_Name == "instrumentation or event data - sampling location longitude" & Result_Text == "77.046552" ~ "-77.046552"
                ,Characteristic_Name == "instrumentation or event data - sampling location longitude" & Result_Text == "77.056286" ~ "-77.056286"
                ,Characteristic_Name == "instrumentation or event data - sampling location longitude" & Result_Text == "77.052022" ~ "-77.052022"
                ,Characteristic_Name == "instrumentation or event data - sampling location longitude" & Result_Text == "77.053476" ~ "-77.053476"
                ,Characteristic_Name == "instrumentation or event data - sampling location longitude" & Result_Text == "77.050748" ~ "-77.050748"
                ,Characteristic_Name == "instrumentation or event data - sampling location longitude" & Result_Text == "77.059312" ~ "-77.059312"
                ,Characteristic_Name %like% "exotic terrestrial plant relative abundance - other species" & Result_Text == "NONE" ~ NA
                ,Characteristic_Name %like% "dominant substrate of bars" & Result_Text == "0" ~ "FALSE"
                ,Characteristic_Name %like% "dominant substrate of bars" & Result_Text == "1" ~ "TRUE"
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
        
        # myvec <- unique(real$Characteristic_Name)
        # subset(myvec, myvec %like% "fragmites")
        # unique(real$Characteristic_Name) %like% "stream gradient"
        # mysub <- real %>% subset(Subject_Taxon == "riffle sampled for benthos")
        # mysub <- real %>% subset(Activity_ID %like% "macroinvertebrates")
        # mysub <- real %>% subset(Characteristic_Name %like% "dominant substrate of bars")
        # View(mysub)
        # unique(mysub$Result_Unit)
        # unique(mysub$Result_Text)
        # unique(mysub$Characteristic_Name)
        # unique(mysub$Result_Qualifier)
        # unique(mysub$Result_File_Name)
        # 
        # paste(shQuote(myvec), collapse=", ")
        # cat(paste(shQuote(myvec, type="cmd"), collapse=", "))
        # 
        # 
        # 
        # # check that data wrangling produced valid units
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
                "`results` wrangled successfully..."
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