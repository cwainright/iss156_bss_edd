# Generate one EDD-formatted xlsx from many NCRN and collaborator data files

Biological Stream Survey (BSS) data include monitoring and/or inventory data of fish, benthic macroinvertebrates, and abiotic habitat/water chemistry metrics.

This repo combines all NPS NCRN BSS data into one data object and writes that object out as an excel file.

# How to use this repo:
1. Clone the repo from GitHub to a new RStudio project (file -> new project -> version control -> git -> paste the github url)
2. Copy "NCRN_MBSS_be_2022.mdb" from DOI sharepoint to your project's `data/ncrn` directory
    - [SharePoint link](https://doimspp.sharepoint.com/:u:/r/sites/NCRNBiologicalStreamSampling/Shared%20Documents/General/Annual-Data-Packages/2022/NCRN_MBSS/NCRN_MBSS_2022.mdb?csf=1&web=1&e=hJHp4F)
    - `~OneDrive - DOI\Documents - NPS-NCRN-Biological Stream Sampling\General\Annual-Data-Packages\2022\NCRN_MBSS`
    - Note: all other data source files are included in the repo
3. Run `renv::autoload()` in your R session to pull project dependencies
4. Run main.R in your R session
