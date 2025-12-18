# NB if TO_TAPE = FALSE, data will not be sent to Tape archive!

makeConfigFile <- function(location = "./", expfolder){
  cat("write ", file.path(location, expfolder, "config.json\n"))
  jsonlite::write_json(list(
    SMB_MOUNT= FALSE,
    ZIP_FOLDERS= FALSE,
    ZIP_SPLIT_ABOVE_5TB= TRUE,
    TO_TAPE= FALSE,
    NUM_ZIPPERS= 1,
    NUM_IWORKERS= 1,
    SMB= list(
      SMB_USER= "<user>",
      SMB_PATH="",
      SMB_LETTER= ""
    ),
    IRODS_ENV_FILE= "irods_environment.json",
    LOCAL_SOURCE_PATH= file.path(location, expfolder),
    LOCAL_ZIP_TEMP= "./",
    LOCAL_ZIP_SPACE= "50GB",
    IRODS_TARGET_PATH= "/ArchvPROD/PSG/PBR/",
    METADATA_EXCEL= "metadata_file.xlsx", #file.path(location, expfolder, "metadata_file.xlsx"),
    PROGRESS_FILE= file.path(location, expfolder, "in_progress.csv")
  ), path = file.path(location, expfolder, "config.json"), pretty = TRUE, auto_unbox = TRUE)
}


makeConfigFile_Study <- function(expfolder){
  cat("write ", file.path(expfolder, "metadata_study_config.json\n"))
  jsonlite::write_json(list(
    SMB_MOUNT= FALSE,
    ZIP_FOLDERS= FALSE,
    ZIP_SPLIT_ABOVE_5TB= TRUE,
    TO_TAPE= FALSE,
    NUM_ZIPPERS= 1,
    NUM_IWORKERS= 1,
    SMB= list(
      SMB_USER= "<user>",
      SMB_PATH="",
      SMB_LETTER= ""
    ),
    IRODS_ENV_FILE= "irods_environment.json",
    LOCAL_SOURCE_PATH= file.path(expfolder),
    LOCAL_ZIP_TEMP= "./",
    LOCAL_ZIP_SPACE= "50GB",
    IRODS_TARGET_PATH= "/ArchvPROD/PSG/PBR/",
    METADATA_STUDY_CSV= "metadata_study.csv"
  ), path = file.path(expfolder, "metadata_study_config.json"), pretty = TRUE, auto_unbox = TRUE)
}

makeConfigFile_Project <- function(expfolder){
  cat("write ", file.path(expfolder, "metadata_project_config.json\n"))
  jsonlite::write_json(list(
    SMB_MOUNT= FALSE,
    ZIP_FOLDERS= FALSE,
    ZIP_SPLIT_ABOVE_5TB= TRUE,
    TO_TAPE= FALSE,
    NUM_ZIPPERS= 1,
    NUM_IWORKERS= 1,
    SMB= list(
      SMB_USER= "<user>",
      SMB_PATH="",
      SMB_LETTER= ""
    ),
    IRODS_ENV_FILE= "irods_environment.json",
    LOCAL_SOURCE_PATH= file.path(expfolder),
    LOCAL_ZIP_TEMP= "./",
    LOCAL_ZIP_SPACE= "50GB",
    IRODS_TARGET_PATH= "/ArchvPROD/PSG/PBR/",
    METADATA_PROJECT_CSV= "metadata_project.csv"
  ), path = file.path(expfolder, "metadata_project_config.json"), pretty = TRUE, auto_unbox = TRUE)
}
