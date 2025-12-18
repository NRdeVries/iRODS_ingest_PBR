# R code to run interactively

# what this script does:
# - create configuration files: for each Assay, and optionally for the Project and Study/Studies
# - create list of files to transfer, these will be included in the tar archive(one tar file per Assay)
# - create SGE job scripts for creating tar archives
# - create an ro-crate-metadata.json file per Study
# - create metadata tables for iRODS ingest: an Excel file listing the the files to trasnfer (tar archive(s), list of files to transfer,
# ro-crate-metadata.json) and their metadata. Furthermore ;-separated CSV files for Investigation/Project and Study metadata



#############################################################################################################################################
######################################## variables to be defined before running #############################################################
#############################################################################################################################################
# path to user-created metadata Excel file
excel="newmetadata.PBR.20250915_projectX.xlsx"

newProject=TRUE # create metadata table for currect Project? (TRUE for new project, FALSE if already added before)
newStudies=c("E000006","E000007") # for which Studies' metadata should be added? (which of the Studies in the Excel file are new?)

iRODS_ingest_repo_path = "~/iRODS_ingest_PBR"
queue = "terri.q" # on which queue should the tar job be run (should match destination scratchpad)

#############################################################################################################################################
#############################################################################################################################################

source(file.path(iRODS_ingest_repo_path,"PBR_scripts/functions_dataupload.R"))
source(file.path(iRODS_ingest_repo_path,"PBR_scripts/PBR_settings.R"))

#############################################################################################################################################
#############################################################################################################################################


# write files for data upload in to-be-created folder ./upload/

# write config file
makeConfig_perExp(excel)

# write text files (1 per Assay) containing list of files to add to tar archive
# optional: copy the excel file also to the source folder
makeTarList(excel)

# create tar archive(s)
tar_tarlists(excel, queue)

# write ro-crate-metadata.json file per experiment
crate <- create_ro_crate()
crate <- add_investigation_from_excel(crate, excel)
crates <- add_studies_from_excel_make_cratelist(crate, excel)

newcratelist = list()
assaydf <- readxl::read_excel(excel, sheet = "Assay")
newcratelist <- lapply(unique(assaydf$EXPID), function(x) append(newcratelist, add_assays_from_excel_perstudycrate(crates[[x]], excel, x)))

write_ro_crate_perExp(newcratelist, "./")

# create iRODS metadata Excel file for data (attach to tar file) and csv file for Investigation and Study folders
makeiRODSExcelFile(excel, outfilename = "metadata_file.xlsx") # also transfer ro-crate-metadata.json, tarname.tarlist.txt


# prepare configuration file + iRODS metadata csv file for Project/Investigation metadata, to be attached to folder (collection)
# only run for new Projects (or updated metadata)
if (newProject){
    makeiRODSProjectMetadata(excel, outfilename = folder <- file.path("upload","metadata_project.csv"))
    makeConfigFile_Project(file.path("upload"))
}

# prepare configuration file + iRODS metadata csv file for Study metadata, to be attached to folder (collection)
# run for each new Study

for (EXPID in newStudies){
    makeiRODSStudyMetadata(excel, EXPID)
    makeConfigFile_Study(file.path("upload", EXPID))
}
