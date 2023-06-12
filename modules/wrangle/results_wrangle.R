# a module for `edd_results.R`
# ETL, wrangle results
options(warn=-1)
results_wrangle <- function(example, real){
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
                        ,Characteristic_Name == "left bank veg type" ~ "stream biotic - left bank - dominant riparian buffer terrestrial vegetation"
                        ,Characteristic_Name == "vegetation types on right bank in riparian buffer most to least common (g=grasses/forbes; r=regenerating deciduous/shrubs; y=young deciduous; m=mature deciduous; o=old deciduous; a=regenerating coniferous; b=yound coniferous; c=mature coniferous; d=lawn)" ~ "stream biotic - right bank - dominant riparian buffer terrestrial vegetation"
                        ,Characteristic_Name == "right bank veg type" ~ "stream biotic - right bank - dominant riparian buffer terrestrial vegetation"
                        ,Characteristic_Name == "macrophytes sampled for benthos" ~ "instrumentation or event data - square feet of habiat sampled for benthic macroinvertebrates - macrophytes"
                        ,Characteristic_Name == "macrophytes" ~ "instrumentation or event data - square feet of habiat sampled for benthic macroinvertebrates - macrophytes"
                        ,Characteristic_Name == "riffle sampled for benthos" ~ "instrumentation or event data - square feet of habiat sampled for benthic macroinvertebrates - riffle"
                        ,Characteristic_Name == "root wad sampled for benthos" ~ "instrumentation or event data - square feet of habiat sampled for benthic macroinvertebrates - root wad"
                        ,Characteristic_Name == "leaf pack smapled for benthos" ~ "instrumentation or event data - square feet of habiat sampled for benthic macroinvertebrates - leaf pack"
                        ,Characteristic_Name == "leaf pack" ~ "instrumentation or event data - square feet of habiat sampled for benthic macroinvertebrates - leaf pack"
                        ,Characteristic_Name == "undercut bank sampled for benthos" ~ "instrumentation or event data - square feet of habiat sampled for benthic macroinvertebrates - undercut bank"
                        ,Characteristic_Name == "'other habitats' sampled for benthos" ~ "instrumentation or event data - square feet of habiat sampled for benthic macroinvertebrates - other habitat type"
                        ,Characteristic_Name == "number of square feet of undercut bank sampled for benthos" ~ "instrumentation or event data - square feet of habiat sampled for benthic macroinvertebrates - undercut bank"
                        ,Characteristic_Name == "number of square feet of roots or leaves sampled for benthos" ~ "instrumentation or event data - square feet of habiat sampled for benthic macroinvertebrates - roots or leaves"
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
                        ,Characteristic_Name == "submerged aquatic vegetation" ~ "stream biota - aquatic vegetation - relative abundance - submerged aquatic plants"
                        ,Characteristic_Name == "emergent aquatic vegetation" ~ "stream biota - aquatic vegetation - relative abundance - emergent aquatic plants"
                        ,Characteristic_Name == "floating aquatic vegetation" ~ "stream biota - aquatic vegetation - relative abundance - floating aquatic plants"
                        ,Characteristic_Name == "percentage of rocks (gravel, cobble, and boulders) that are surrounded by, covered, or sunken into the silt, sand, or mud of the stream" ~ "stream abiotic - embeddedness - percentage of rocks (gravel, cobble, boulders) covered by sediment (sand, silt)"
                        ,Characteristic_Name == "percentage of hard substrates that are surrounded by fine sediments (0-100%)" ~ "stream abiotic - embeddedness - percentage of rocks (gravel, cobble, boulders) covered by sediment (sand, silt)"
                        ,Characteristic_Name == "erosion severity right bank (0=none; 1=minor; 2=moderate; 3=severe)" ~ "stream abiotic - right bank - severity - bank erosion"
                        ,Characteristic_Name == "erosion severity right  bank (0=none; 1=minor; 2=moderate; 3=severe)" ~ "stream abiotic - right bank - severity - bank erosion"
                        ,Characteristic_Name == "bar severity right bank (0=none; 1=minor; 2=moderate; 3=severe)" ~ "stream abiotic - right bank - severity - stream bar"
                        ,Characteristic_Name == "erosion severity left bank (0=none; 1=minor; 2=moderate; 3=severe)" ~ "stream abiotic - left bank - severity - bank erosion"
                        ,Characteristic_Name == "bar severity left bank (0=none; 1=minor; 2=moderate; 3=severe)" ~ "stream abiotic - left bank - severity - stream bar"
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
                        ,Characteristic_Name == "time float takes to make it to end point for trial 1" ~ "stream abiotic - alternate discharge measurement - float time - trial 1"
                        ,Characteristic_Name == "time float takes to make it to end point for trial 2" ~ "stream abiotic - alternate discharge measurement - float time - trial 2"
                        ,Characteristic_Name == "time float takes to make it to end point for trial 3" ~ "stream abiotic - alternate discharge measurement - float time - trial 3"
                        ,Characteristic_Name == "alternate flow measurement float distance" ~ "stream abiotic - alternate discharge measurement - float distance"
                        ,Characteristic_Name == "alternate flow measurement flow depth" ~ "stream abiotic - alternate discharge measurement - water depth"
                        ,Characteristic_Name == "alternate flow measurement flow width" ~ "stream abiotic - alternate discharge measurement - wetted width"
                        ,Characteristic_Name == "number of large wood pieces in wetted stream" ~ "stream abiotic - coarse woody debris in wetted stream"
                        ,Characteristic_Name == "number of large wood pieces in active channel but currently dewatered" ~ "stream abiotic - coarse woody debris in stream channel but currently dewatered"
                        ,Characteristic_Name == "number of root wads in wetted stream" ~ "stream abiotic - root wads in wetted stream"
                        ,Characteristic_Name == "number of root wads in active channel but currently dewatered" ~ "stream abiotic - root wads in stream channel but currently dewatered"
                        ,Characteristic_Name == "left bank riparian width (m)" ~ "stream abiotic - left bank - riparian width"
                        ,Characteristic_Name == "riparian vegetation buffer broken on left bank (yes/no)" ~ "stream abiotic - left bank - presence absence - riparian buffer breaks"
                        ,Characteristic_Name == "left bank adjacent land cover" ~ "stream biotic - left bank - adjacent land cover"
                        ,Characteristic_Name == "right bank adjacent land cover" ~ "stream biotic - right bank - adjacent land cover"
                        ,Characteristic_Name == "adjacent land cover left bank" ~ "stream biotic - left bank - adjacent land cover"
                        ,Characteristic_Name == "adjacent land cover righ bank" ~ "stream biotic - right bank - adjacent land cover"
                        ,Characteristic_Name == "left bank dredge spoil" ~ "stream abiotic - relative abundance - left bank - dredge spoil"
                        ,Characteristic_Name == "right bank dredge spoil" ~ "stream abiotic - relative abundance - right bank - dredge spoil"
                        ,Characteristic_Name == "left bank buffer breaks" ~ "stream abiotic - left bank - presence absence - riparian buffer breaks"
                        ,Characteristic_Name == "right bank buffer breaks" ~ "stream abiotic - right bank - presence absence - riparian buffer breaks"
                        ,Characteristic_Name == "right bank riparian width (m)" ~ "stream abiotic - right bank - riparian width"
                        ,Characteristic_Name == "riparian vegetation buffer broken on right bank (yes/no)" ~ "stream abiotic - right bank - presence absence - riparian buffer breaks"
                        ,Characteristic_Name == "left dominant substrate at 50m-75m" ~ "stream abiotic - 50m-75m - river-right dominant substrate"
                        ,Characteristic_Name == "left subdominant substrate at 50m-75m" ~ "stream abiotic - 50m-75m - river-right subdominant substrate"
                        ,Characteristic_Name == "left depth at 50m-75m" ~ "stream abiotic - 50m-75m - river-right water depth"
                        ,Characteristic_Name == "left velocity at 50m-75m" ~ "stream abiotic - 50m-75m - river-right water velocity"
                        ,Characteristic_Name == "right dominant substrate at 50m-75m" ~ "stream abiotic - 50m-75m - river-left dominant substrate"
                        ,Characteristic_Name == "right subdominant substrate at 50m-75m" ~ "stream abiotic - 50m-75m - river-left subdominant substrate"
                        ,Characteristic_Name == "right depth at 50m-75m" ~ "stream abiotic - 50m-75m - river-left water depth"
                        ,Characteristic_Name == "right velocity at 50m-75m" ~ "stream abiotic - 50m-75m - river-left water velocity"
                        ,Characteristic_Name == "left dominant substrate at 25m-50m" ~ "stream abiotic - 25m-50m - river-right dominant substrate"
                        ,Characteristic_Name == "left subdominant substrate at 25m-50m" ~ "stream abiotic - 25m-50m - river-right subdominant substrate"
                        ,Characteristic_Name == "left depth at 25m-50m" ~ "stream abiotic - 25m-50m - river-right water depth"
                        ,Characteristic_Name == "left velocity at 25m-50m" ~ "stream abiotic - 25m-50m - river-right water velocity"
                        ,Characteristic_Name == "right dominant substrate at 25m-50m" ~ "stream abiotic - 25m-50m - river-left dominant substrate"
                        ,Characteristic_Name == "right subdominant substrate at 25m-50m" ~ "stream abiotic - 25m-50m - river-left subdominant substrate"
                        ,Characteristic_Name == "right depth at 25m-50m" ~ "stream abiotic - 25m-50m - river-left water depth"
                        ,Characteristic_Name == "right velocity at 25m-50m" ~ "stream abiotic - 25m-50m - river-left water velocity"
                        ,Characteristic_Name == "left dominant substrate at 0m-25m" ~ "stream abiotic - 0m-25m - river-right dominant substrate"
                        ,Characteristic_Name == "left subdominant substrate at 0m-25m" ~ "stream abiotic - 0m-25m - river-right subdominant substrate"
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
                        ,Characteristic_Name == "left bank earthen berm" ~ "stream abiotic - left bank - extent - stream channelized by earthen berm"
                        ,Characteristic_Name == "right bank pipe culvert" ~ "stream abiotic - right bank - extent - stream channelized by pipe culvert"
                        ,Characteristic_Name == "left bank pipe culvert" ~ "stream abiotic - left bank - extent - stream channelized pipe culvert"
                        ,Characteristic_Name == "length of culvert if present" ~ "stream abiotic - either bank - extent - stream channelized pipe culvert"
                        ,Characteristic_Name == "diameter of culvert" ~ "stream abiotic - either bank - diameter of culvert"
                        ,Characteristic_Name == "channelization present" ~ "stream abiotic - either bank - relative abundance - channelization"
                        ,Characteristic_Name == "presence/absence of culvert in sample reach" ~ "stream abiotic - either bank - relative abundance - culvert"
                        ,Characteristic_Name == "culvert in segment" ~ "stream abiotic - either bank - relative abundance - stream channelized pipe culvert"
                        ,Characteristic_Name == "right bank rip-rap" ~ "stream abiotic - right bank - extent - stream channelized by rip-rap"
                        ,Characteristic_Name == "left bank rip-rap" ~ "stream abiotic - left bank - extent - stream channelized by rip-rap"
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
                        ,Characteristic_Name == "stream gradient" ~ "stream abiotic - stream gradient"
                        ,Characteristic_Name == "channel slope (%) using a surveyor's level and rod" ~ "stream abiotic - stream gradient"
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
                        ,Characteristic_Name == "orange floc iron bacteria surface film present, absent, extensive" ~ "stream biota - relative abundance - orange floc iron bacteria"
                        ,Characteristic_Name == "didymo presence" ~ "stream biota - didymo - relative abundance"
                        ,Characteristic_Name == "didymo voucher" ~ "stream biota - didymo - voucher"
                        ,Characteristic_Name == "distance from road (m)" ~ "stream abiotic - straight-line distance to nearest road"
                        ,Characteristic_Name == "distance to nearest road" ~ "stream abiotic - straight-line distance to nearest road"
                        ,Characteristic_Name == "individual fish mass (weight) in g" ~ "stream biota - fish - individual fish mass"
                        ,Characteristic_Name == "individual fish total length in mm" ~ "stream biota - fish - individual fish total length"
                        ,Characteristic_Name == "fish count by species" ~ "stream biota - fish - count of individuals captured by species"
                        ,Characteristic_Name == "gps location of latitude (only present if coordinates do not fall within 30m of stream or on samplable reach)" ~ "instrumentation or event data - sampling location latitude"
                        ,Characteristic_Name == "gps location of longitude (only present if coordinates do not fall within 30m of stream or on samplable reach)" ~ "instrumentation or event data - sampling location longitude"
                        ,Characteristic_Name == "stream macroinvertebrate sampling" ~ "stream biota - benthic macroinvertebrates - count of individuals captured by taxon"
                        ,Characteristic_Name == "erosion extent left bank" ~ "stream abiotic - left bank - extent - bank erosion"
                        ,Characteristic_Name == "erosion extent right bank" ~ "stream abiotic - right bank - extent - bank erosion"
                        ,Characteristic_Name == "erosion height left bank (meters)" ~ "stream abiotic - left bank - height - bank erosion"
                        ,Characteristic_Name == "erosion height right bank (meters)" ~ "stream abiotic - right bank - height - bank erosion"
                        ,Characteristic_Name == "eroded area left bank (square meters/10)" ~ "stream abiotic - left bank - area - bank erosion"
                        ,Characteristic_Name == "eroded area right bank (square meters/10)" ~ "stream abiotic - right bank - area - bank erosion"
                        ,Characteristic_Name == "facies mapping" ~ "instrumentation or event data - facies mapping completed"
                        ,Characteristic_Name == "presence of coniferous forest in close proximity of sampling site" ~ "stream abiotic - either bank - adjacent land cover - presence absence - coniferous forest"
                        ,Characteristic_Name == "presence of cropland in close proximity of sampling site" ~ "stream abiotic - either bank - adjacent land cover - presence absence - cropland"
                        ,Characteristic_Name == "presence of deciduous forest in close proximity of sampling site" ~ "stream abiotic - either bank - adjacent land cover - presence absence - deciduous forest"
                        ,Characteristic_Name == "presence of golf course in close proximity of sampling site" ~ "stream abiotic - either bank - adjacent land cover - presence absence - golf course"
                        ,Characteristic_Name == "presence of landfills in close proximity of sampling site" ~ "stream abiotic - either bank - adjacent land cover - presence absence - landfill"
                        ,Characteristic_Name == "presence of old field in close proximity of sampling site" ~ "stream abiotic - either bank - adjacent land cover - presence absence - old field"
                        ,Characteristic_Name == "presence of orchards/vinyards in close proximity of sampling site" ~ "stream abiotic - either bank - adjacent land cover - presence absence - orchard or vineyard"
                        ,Characteristic_Name == "presence of pasture in close proximity of sampling site" ~ "stream abiotic - either bank - adjacent land cover - presence absence - pasture"
                        ,Characteristic_Name == "presence of residential development in close proximity of sampling site" ~ "stream abiotic - either bank - adjacent land cover - presence absence - residential development"
                        ,Characteristic_Name == "presence of surface mines in close proximity of sampling site" ~ "stream abiotic - either bank - adjacent land cover - presence absence - surface mines"
                        ,Characteristic_Name == "presence of wetlands in close proximity of sampling site" ~ "stream abiotic - either bank - adjacent land cover - presence absence - wetlands"
                        ,Characteristic_Name == "bar formation = extensive" ~ "stream abiotic - bar formation - presence absence - extensive"
                        ,Characteristic_Name == "bar formation = minor" ~ "stream abiotic - bar formation - presence absence - minor"
                        ,Characteristic_Name == "bar formation = moderate" ~ "stream abiotic - bar formation - presence absence - moderate"
                        ,Characteristic_Name == "bar formation = none" ~ "stream abiotic - bar formation - presence absence - none"
                        ,TRUE ~ Characteristic_Name
                    )
                ) %>%
                mutate(
                    Result_Unit = case_when(
                        Characteristic_Name == "stream biotic - exotic terrestrial plant relative abundance - other species" ~ "A = absent, P = present, E = extensiv"
                        ,Characteristic_Name %like% "relative abundance" ~ "A = absent, P = present, E = extensive"
                        ,Characteristic_Name == "stream biota - benthic macroinvertebrates - count of individuals captured by taxon" ~ "count"
                        ,Characteristic_Name %like% "benthic macroinvertebrates" ~ "square feet"
                        ,Characteristic_Name %like% "area - bank erosion" ~ "square meters"
                        ,Characteristic_Name %like% "sampleability" ~ "S = Sampleable"
                        ,Characteristic_Name %like% "float time" ~ "seconds"
                        ,Characteristic_Name %like% "severity - bank erosion" ~ "0 = None; 1 = Minor; 2 = Moderate; 3 = Severe"
                        ,Characteristic_Name %like% "severity - stream bar" ~ "0 = None; 1 = Minor; 2 = Moderate; 3 = Severe"
                        ,Characteristic_Name == "stream abiotic - coarse woody debris in stream channel but currently dewatered" ~ "count"
                        ,Characteristic_Name == "stream abiotic - coarse woody debris in wetted stream" ~ "count"
                        ,Characteristic_Name == "stream abiotic - root wads in stream channel but currently dewatered" ~ "count"
                        ,Characteristic_Name == "stream abiotic - root wads in wetted stream" ~ "count"
                        ,Characteristic_Name == "stream abiotic - stream obstruction - height" ~ "meters"
                        ,Characteristic_Name == "stream abiotic - straight-line length of 75m reach" ~ "meters"
                        ,Characteristic_Name %like% "linear extent" ~ "meters"
                        ,Characteristic_Name %like% "straight line length" ~ "meters"
                        ,Characteristic_Name %like% "length of stream obstructed by" ~ "meters"
                        ,Characteristic_Name %like% "wetted channel width" ~ "meters"
                        ,Characteristic_Name %like% "stream gradient measurement location" ~ "meters"
                        ,Characteristic_Name %like% "stream gradient height measurement" ~ "meters"
                        ,Characteristic_Name %like% "stream lateral location" ~ "meters"
                        ,Characteristic_Name %like% "elevation above sea level" ~ "meters"
                        ,Characteristic_Name %like% "riparian width" ~ "meters"
                        ,Characteristic_Name %like% "length of stream channelized" ~ "meters"
                        ,Characteristic_Name %like% "extent - bank erosion" ~ "meters"
                        ,Characteristic_Name %like% "height - bank erosion" ~ "meters"
                        ,Characteristic_Name %like% "extent - stream channelized by earthen berm" ~ "meters"
                        ,Characteristic_Name %like% "extent - stream channelized by pipe culvert" ~ "meters"
                        ,Characteristic_Name %like% "extent - stream channelized by rip-rap" ~ "meters"
                        ,Characteristic_Name %like% "percent slope" ~ "percent slope"
                        ,Characteristic_Name %like% "stream gradient" ~ "percent slope"
                        ,Characteristic_Name == "stream abiotic - overhead cover" ~ "percent"
                        ,Characteristic_Name %like% "thalweg depth" ~ "centimeters"
                        ,Characteristic_Name %like% "reach maximum depth" ~ "centimeters"
                        ,Characteristic_Name %like% "river-left water depth" ~ "centimeters"
                        ,Characteristic_Name %like% "river-right water depth" ~ "centimeters"
                        ,Characteristic_Name %like% "thalweg velocity" ~ "meters per second"
                        ,Characteristic_Name %like% "water velocity" ~ "meters per second"
                        ,Characteristic_Name %like% "sulfate" ~ "mg/L"
                        ,Characteristic_Name %like% "ammonia" ~ "mg/L"
                        ,Characteristic_Name %like% "chloride" ~ "mg/L"
                        ,Characteristic_Name %like% "nitrate" ~ "mg/L"
                        ,Characteristic_Name %like% "nitrite" ~ "mg/L"
                        ,Characteristic_Name %like% "total nitrogen" ~ "mg/L"
                        ,Characteristic_Name %like% "total phosphorus" ~ "mg/L"
                        ,Characteristic_Name %like% "orthophosphate" ~ "mg/L"
                        ,Characteristic_Name %like% "dissolved organic carbon" ~ "mg/L"
                        ,Characteristic_Name %like% "acid neutralizing capacity" ~ "ueq/L"
                        ,Characteristic_Name %like% "specific conductance" ~ "uS/cm"
                        ,Characteristic_Name %like% "latitude" ~ "decimal degrees"
                        ,Characteristic_Name %like% "longitude" ~ "decimal degrees"
                        ,Characteristic_Name == "stream abiotic - stream obstruction - type" ~ "DM = dam; PC = pipe culvert; F = fishway; FW = gaging station weir; G = gabion; PX = pipeline crossing; AC = arch culvert; BC = box culvert; TG = tide gate"
                        ,Characteristic_Name == "stream abiotic - water logger dewatered status" ~ "N = not dewatered; Y = dewatered"
                        ,Characteristic_Name == "stream abiotic - water quality-control sample collected" ~ "N = not collected; Y = collected"
                        ,Characteristic_Name %like% "dominant riparian buffer terrestrial vegetation" ~ "G = grasses/forbes; R = regenerating deciduous/shrubs; Y = young deciduous; M = mature deciduous; O = old deciduous; A = regenerating coniferous; B = young coniferous; C = mature coniferous; D = lawn; W = wetland"
                        ,Characteristic_Name %like% "aesthetic appeal of sampling site" ~ "0 (lowest) to 20 (highest)"
                        ,Characteristic_Name == "stream abiotic - habitat quality - riffle" ~ "0 (lowest) to 20 (highest)"
                        ,Characteristic_Name == "stream abiotic - habitat quality - water velocity and depth" ~ "0 (lowest) to 20 (highest)"
                        ,Characteristic_Name == "stream abiotic - habitat quality - instream habitat" ~ "0 (lowest) to 20 (highest)"
                        ,Characteristic_Name == "stream abiotic - habitat quality - pool, glide, eddy" ~ "0 (lowest) to 20 (highest)"
                        ,Characteristic_Name == "stream abiotic - habitat quality - epifaunal substrate" ~ "0 (lowest) to 20 (highest)"
                        ,Characteristic_Name == "instrumentation or event data - photos taken at site" ~ "N = not collected; Y = collected"
                        ,Characteristic_Name == "instrumentation or event data - temperature logger deployed" ~ "N = not deployed; Y = deployed"
                        ,Characteristic_Name == "instrumentation or event data - air logger deployed" ~ "N = not deployed; Y = deployed"
                        ,Characteristic_Name == "instrumentation or event data - facies mapping completed" ~ "N = no; Y = yes"
                        ,Characteristic_Name == "instrumentation or event data - year of quality control sample" ~ "year"
                        ,Characteristic_Name == "instrumentation or event data - stream segment id number for quality control sample" ~ "id number"
                        ,Characteristic_Name == "instrumentation or event data - temperature logger serial number" ~ "id number"
                        ,Characteristic_Name == "instrumentation or event data - site type for quality control sample" ~ "R = random; S = sentinel; N = national park service; T = targeted"
                        ,Characteristic_Name == "stream abiotic - straight-line distance to nearest road" ~ "meters"
                        ,Characteristic_Name == "stream biota - didymo - voucher" ~ "N = not vouchered; Y = vouchered"
                        ,Characteristic_Name == "stream abiotic - right bank - presence absence - riparian buffer breaks" ~ "P = present; A = absent"
                        ,Characteristic_Name == "stream abiotic - left bank - presence absence - riparian buffer breaks" ~ "P = present; A = absent"
                        ,Characteristic_Name == "stream abiotic - embeddedness - percentage of rocks (gravel, cobble, boulders) covered by sediment (sand, silt)" ~ "percent"
                        ,Characteristic_Name %like% "closed pH value" ~ "pH"
                        ,Characteristic_Name %like% "dominant substrate of bars" ~ "TRUE or FALSE"
                        ,Characteristic_Name %like% "bar formation" ~ "TRUE or FALSE"
                        ,Characteristic_Name %like% "stream abiotic - either bank - adjacent land cover" ~ "A = absent; P = present"
                        ,Characteristic_Name %like% "river-left dominant substrate" ~ "G = gravel; B = boulder; C = cobble; S = sand or silt"
                        ,Characteristic_Name %like% "river-right dominant substrate" ~ "G = gravel; B = boulder; C = cobble; S = sand or silt"
                        ,Characteristic_Name %like% "river-left subdominant substrate" ~ "G = gravel; B = boulder; C = cobble; S = sand or silt"
                        ,Characteristic_Name %like% "river-right subdominant substrate" ~ "G = gravel; B = boulder; C = cobble; S = sand or silt"
                        ,Characteristic_Name %like% "alternate flow measurement float distance" ~ "centimeters"
                        ,Characteristic_Name == "stream abiotic - alternate discharge measurement - float distance" ~ "centimeters"
                        ,Characteristic_Name == "stream abiotic - alternate discharge measurement - water depth" ~ "centimeters"
                        ,Characteristic_Name == "stream abiotic - alternate discharge measurement - wetted width" ~ "centimeters"
                        ,Characteristic_Name == "stream abiotic - water depth associated with a stream lateral location" ~ "centimeters"
                        ,Characteristic_Name == "stream abiotic - water depth associated with a stream lateral location" ~ "centimeters"
                        ,Characteristic_Name == "stream biota - fish - individual fish mass" ~ "grams"
                        ,Characteristic_Name == "stream biota - fish - individual fish total length" ~ "millimeters"
                        ,Characteristic_Name == "stream biota - fish - count of individuals captured by species" ~ "count"
                        ,Characteristic_Name == "stream biota - benthic macroinvertebrates" ~ "count"
                        ,Characteristic_Name == "stream abiotic - either bank - diameter of culvert" ~ "meters"
                        ,Characteristic_Name == "stream abiotic - bar formation - presence absence" ~ "P = present; A = absent"
                        ,Characteristic_Name == "stream abiotic - chemistry - source of any acidity" ~ "text"
                        ,Characteristic_Name %like% " extent - stream channelized pipe culvert" ~ "meters"
                        ,Characteristic_Name %like% "adjacent land cover" ~ "FR = forest; OF = old field; EM = emergent vegetation; LN = mowed lawn; TG = tall grass; LO = logged area; SL = bare soil; RR = railroad; PV = paved road; PK = parking lot, industrial, or commercial; GR = gravel road; DI = dirt road; PA = pasture; OR = orchard; CP = cropland; HO = housing"
                        # ,Activity_ID %like% "macroinvertebrates" ~ "count of individuals"
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
                ) %>%
                mutate(
                    Result_Sampling_Point_Name = case_when(
                        Result_Sampling_Point_Name == "Hazen Creek" ~"Reservation 630 Creek"
                        ,Result_Sampling_Point_Name == "NCRN Monocacy Park at Bush Creek" ~"Bush Creek"
                        ,Result_Sampling_Point_Name == "Henson Creek @ Suitland Road" ~"Henson Creek"
                        ,Result_Sampling_Point_Name == "Still Creek, Greenbelt Park" ~"Still Creek"
                        ,Result_Sampling_Point_Name == "Whiskey Still Creek" ~"Blue Blazes Creek"
                        ,Result_Sampling_Point_Name == "Visitor's Center Creek" ~"Visitor Center Creek"
                        ,TRUE ~ Result_Sampling_Point_Name
                    )
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
                    Characteristic_Name == "stream abiotic - chemistry - orthophosphate concentration" & Result_Text == "< 0.0032" ~ "reported '< 0.0032'; undetectable"
                    ,Characteristic_Name == "stream abiotic - chemistry - ammonia concentration" & Result_Text == "< 0.0045" ~ "reported as '< 0.0045'; undetectable"
                    ,Characteristic_Name == "stream abiotic - chemistry - nitrate concentration" & Result_Text == "< 0.0043" ~ "reported as '< 0.0043'; undetectable"
                    ,Characteristic_Name == "stream abiotic - chemistry - nitrite concentration" & Result_Text == "< 0.0028" ~ "reported as '< 0.0028'; undetectable"
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
                    ,Characteristic_Name == "stream abiotic - chemistry - nitrate concentration" & Result_Text == "< 0.0043" ~ NA # undetectable
                    ,Characteristic_Name == "stream abiotic - chemistry - nitrate concentration" & Result_Text == "2.5100000000000001E-2" ~ "0.0251" # sig figs
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
                    ,Characteristic_Name == "stream biota - didymo - relative abundance" & Result_Text == "N" ~ "A"
                    ,Characteristic_Name == "stream abiotic - either bank - relative abundance - culvert" & Result_Text == "0" ~ "A"
                    ,Characteristic_Name == "stream abiotic - either bank - relative abundance - culvert" & Result_Text == "1" ~ "P"
                    ,Characteristic_Name == "stream abiotic - either bank - relative abundance - culvert" & Result_Text == "N" ~ "A"
                    ,Characteristic_Name == "sampleability - electrofishing" & Result_Text == "0" ~ "N"
                    ,Characteristic_Name == "stream abiotic - top right bank - presence absence - riparian buffer breaks" & Result_Text == "0" ~ "A"
                    ,Characteristic_Name == "stream abiotic - top right bank - presence absence - riparian buffer breaks" & Result_Text == "1" ~ "P"
                    ,Characteristic_Name == "stream abiotic - top left bank - presence absence - riparian buffer breaks" & Result_Text == "0" ~ "A"
                    ,Characteristic_Name == "stream abiotic - top left bank - presence absence - riparian buffer breaks" & Result_Text == "1" ~ "P"
                    ,Characteristic_Name == "stream biotic - either bank - dominant riparian buffer terrestrial vegetation" & Result_Text == "Deciduous Forest" ~ "M"
                    ,Characteristic_Name == "stream biotic - either bank - dominant riparian buffer terrestrial vegetation" & Result_Text == "Decidious Forest" ~ "M"
                    ,Characteristic_Name == "stream biotic - either bank - dominant riparian buffer terrestrial vegetation" & Result_Text == "Old Field" ~ "R"
                    ,Characteristic_Name == "stream biotic - either bank - dominant riparian buffer terrestrial vegetation" & Result_Text == "Wetland" ~ "W"
                    ,Characteristic_Name == "instrumentation or event data - sampling location latitude" & Result_Text == "38 56 24" ~ "38.94" # degree min sec to decimal degrees
                    ,Characteristic_Name == "instrumentation or event data - sampling location latitude" & Result_Text == "385550" ~ "38.930556" # "385550" == "38d 55m 50s" degree min sec to decimal degrees
                    ,Characteristic_Name == "instrumentation or event data - sampling location latitude" & Result_Text == "392833" ~ "39.475833"
                    ,Characteristic_Name == "instrumentation or event data - sampling location latitude" & Result_Text == "392741" ~ "39.461389"
                    ,Characteristic_Name == "instrumentation or event data - sampling location latitude" & Result_Text == "392148" ~ "39.363333"
                    ,Characteristic_Name == "instrumentation or event data - sampling location latitude" & Result_Text == "392128" ~ "39.357778"
                    ,Characteristic_Name == "instrumentation or event data - sampling location latitude" & Result_Text == "392207" ~ "39.368611"
                    ,Characteristic_Name == "instrumentation or event data - sampling location longitude" & Result_Text == "-77 15 41" ~ "-77.261389" # degree min sec to decimal degrees
                    ,Characteristic_Name == "instrumentation or event data - sampling location longitude" & Result_Text == "770726" ~ "-77.123889"
                    ,Characteristic_Name == "instrumentation or event data - sampling location longitude" & Result_Text == "774417" ~ "-77.738056"
                    ,Characteristic_Name == "instrumentation or event data - sampling location longitude" & Result_Text == "774351" ~ "-77.730833"
                    ,Characteristic_Name == "instrumentation or event data - sampling location longitude" & Result_Text == "772324" ~ "-77.390000"
                    ,Characteristic_Name == "instrumentation or event data - sampling location longitude" & Result_Text == "772349" ~ "--77.396944"
                    ,Characteristic_Name == "instrumentation or event data - sampling location longitude" & Result_Text == "772318" ~ "-77.388333"
                    ,Characteristic_Name == "instrumentation or event data - sampling location longitude" & Result_Text == "77.041638" ~ "-77.041638"
                    ,Characteristic_Name == "instrumentation or event data - sampling location longitude" & Result_Text == "77.043842" ~ "-77.043842"
                    ,Characteristic_Name == "instrumentation or event data - sampling location longitude" & Result_Text == "77.046552" ~ "-77.046552"
                    ,Characteristic_Name == "instrumentation or event data - sampling location longitude" & Result_Text == "77.056286" ~ "-77.056286"
                    ,Characteristic_Name == "instrumentation or event data - sampling location longitude" & Result_Text == "77.052022" ~ "-77.052022"
                    ,Characteristic_Name == "instrumentation or event data - sampling location longitude" & Result_Text == "77.053476" ~ "-77.053476"
                    ,Characteristic_Name == "instrumentation or event data - sampling location longitude" & Result_Text == "77.050748" ~ "-77.050748"
                    ,Characteristic_Name == "instrumentation or event data - sampling location longitude" & Result_Text == "77.059312" ~ "-77.059312"
                    ,Characteristic_Name == "stream abiotic - water velocity associated with a stream lateral location" & Result_Text == "7.0000000000000007E-2" ~ "0.07" # sig figs
                    ,Characteristic_Name %like% "exotic terrestrial plant relative abundance - other species" & Result_Text == "NONE" ~ NA
                    ,Characteristic_Name %like% "dominant substrate of bars" & Result_Text == "0" ~ "FALSE"
                    ,Characteristic_Name %like% "dominant substrate of bars" & Result_Text == "1" ~ "TRUE"
                    ,Characteristic_Name %like% "stream abiotic - either bank - adjacent land cover" & Result_Text == "1" ~ "Present"
                    ,Characteristic_Name %like% "stream abiotic - either bank - adjacent land cover" & Result_Text == "0" ~ "Absent"
                    ,Characteristic_Name == "stream abiotic - bar formation - presence absence" & Result_Text == "0" ~ "Absent"
                    ,Characteristic_Name == "stream abiotic - bar formation - presence absence" & Result_Text == "Yes" ~ "Present"
                    ,Characteristic_Name == "stream abiotic - bar formation - presence absence" & Result_Text == "No" ~ "Absent"
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