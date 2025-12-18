library(readxl)
library(jsonlite)
library(dplyr)
library(openxlsx)

# ===========================================================================================================
# This file contains functions 
# A to prepare data transfer to Tape
# B to create RO-crate metadata json file
# ===========================================================================================================



# ===========================================================================================================
# A. functions to prepare data transfer to Tape
# Functions: 
# makeConfig_perExp() - writing configuration file file
# makeiRODSExcelFile() - create iRODS metadata table to add metadate to the tar archive(s) 
# makeiRODSProjectMetadata() - create iRODS metadata table to add metadata to the Investigation folder
# makeiRODSStudyMetadata() - create iRODS metadata table to add metadata to the Study folder
# makeTarList() - create a text file listing all files to be added to the Assay tar archive 
# tar_tarlists() - submit jobs to create the tar archive based on the tarlist

# output will be written to (new) folder ./upload/EXPID versus the current working directory. 
# ===========================================================================================================


# make iRODS configuration file "config.json" for each Study/Experiment
# excel filename defined in config file should match the output filename of makeiRODSExcelFile() (currently "metadata_file.xlsx")
makeConfig_perExp <- function(excelfile){
  studydf <- readxl::read_excel(excelfile, sheet = "Study")
  for (experiment in studydf$EXPID){
    exp_folder <- file.path("upload", experiment)
    if (!dir.exists(exp_folder)){ cat("create dir", exp_folder,"\n")
      dir.create(exp_folder, recursive = TRUE)}
    location = getwd()
    makeConfigFile(location, exp_folder)
  }
}

# Write list of files to be added to tar archive to a text file "[tarname].tarlist.txt", one list per Assay
makeTarList <- function(excelfile){
  studydf <- readxl::read_excel(excelfile, sheet = "Study")
  assaydf <- readxl::read_excel(excelfile, sheet = "Assay")
  for (row in seq(nrow(assaydf))){
    if (assaydf[[row,"transfer_data"]] %in% c("true", "True", "yes", "Yes")){
      folder <- file.path("upload", assaydf[[row,"EXPID"]])
      if (!dir.exists(folder)){ dir.create(folder, recursive = TRUE)}
      files <- list.files(as.character(assaydf[[row,"pathcluster"]]), full.names = FALSE)

      # exclude metadata and config files from the tar archive
      totarfiles <- setdiff(files, c('metadata_file.xlsx','config.json'))
      # write tarlist to folder that will be copied to Tape. When list of files is appended also to RO-crate metadata json it may be
      # better to not also include tarlist there

      cat(paste0("-C",as.character(assaydf[[row,"pathcluster"]])), "\n", file = file.path(folder, paste0(assaydf[[row,"tarname"]],".tarlist.txt")), append = FALSE)
      cat(totarfiles, sep = "\n", file = file.path(folder, paste0(assaydf[[row,"tarname"]],".tarlist.txt")), append = TRUE)
    }
    }
}

# submit PBR cluster jobs to create the tar archives per experiment per assay (queue currently hard-coded in single_tar.sh)
# there can be multiple assays per study, so name of file-list text file should be specific for assay -> use "tarname" field from excel for that
# check if tar file already exists so the same Excel file can be expanded instead of having to create a new Excel file every time an assay is added
tar_tarlists <- function(excelfile, iRODS_ingest_repo_path, queue) {
  #studydf <- readxl::read_excel(excelfile, sheet = "Study")
  assaydf <- readxl::read_excel(excelfile, sheet = "Assay")
  for (row in seq(nrow(assaydf))){
    if (assaydf[[row,"transfer_data"]] %in% c("true", "True", "yes", "Yes")){
    folder <- file.path("upload", assaydf[[row,"EXPID"]])
    if (!dir.exists(folder)){ dir.create(folder)}
      # only create tar file if it has not been created before
      tarfile <- file.path(folder,assaydf[[row,"tarname"]])
      tarlist <- file.path(folder, paste0(assaydf[[row,"tarname"]],".tarlist.txt"))
      if (!file.exists(tarfile)){
      # single_tar.sh command: tar -cf "@$"
      tararguments = c("-q", queue, file.path(iRODS_ingest_repo_path, "PBR_scripts/single_tar.sh"), tarfile, " -T ", tarlist)
      arguments = c(tararguments)
      cat("submit job",arguments)
      system2("qsub", arguments)
      }
    }
  }
}


# make Excel file for transfer of per assay: 1. a tar archive, 2. text file listing files in tar archive and 3. per Study: one ro-crate-metadata.json
# NB Excel file is not transferred to Tape by default, however you can copy it to the source folder you are about to send to Tape to have it included in the tar archive
# to tar files on Tape add metadata (based on Excel file Assay sheet) (add Investigation and Study metadata to their folders via different function)
makeiRODSExcelFile <- function(user_excel, outfilename){
  # could include fields from both Investigation, Study and Assay sheet
  projectdf <- readxl::read_excel(user_excel, sheet = "Investigation")
  if (nrow(projectdf)==1){print(paste0("Start to make metadata Excel file for project ", projectdf$PROJID))
  } else {
    print("Excel file is not filled in correctly: it is only possible to provide a single Project/Investigation per submission (with one or more Studies/Experiments in the Study tab)")}
  assaydf_full <- readxl::read_excel(user_excel, sheet = "Assay")
  #studydf <- readxl::read_excel(excelfile, sheet = "Study")
  #for (row in seq(nrow(studydf))){ # need to transfer a ro-crate-metadata.json per Study
    #folder <- file.path("upload", assaydf[[row,"EXPID"]])
  # to metadata Excel file add metadata per assay: for the tar files and also add lines for the tarlist file for transfer
  for (EXPID in unique(assaydf_full$EXPID)){
    #folder <- file.path("upload", studydf[[row,"EXPID"]])
    folder <- file.path("upload", EXPID)
    df <- data.frame()
    assaydf <- assaydf_full[assaydf_full$EXPID==EXPID,]
    for (i in seq(nrow(assaydf))){ # need to add a line to transfer the tar file, and a line to transfer the tarlist (if user set transfer_data "True")
      if (assaydf[[i,"transfer_data"]] %in% c("true", "True", "yes", "Yes")){
        #EXPID = assaydf[[i,"EXPID"]]
        # keys (column names) that can be selected: PROJID	EXPID	description	dateCreated	license	pathcluster	tarname	taxID	organism	measurementMethod	measurementTechnique	comments	transfer_data
        l = list(
          PROJID = assaydf[[i,"PROJID"]],
          EXPID = assaydf[[i,"EXPID"]],
          description = assaydf[[i,"description"]],
          license = assaydf[[i,"license"]],
          taxID = assaydf[[i,"taxID"]],
          organism = assaydf[[i,"organism"]],
          comments = assaydf[[i,"comments"]],
          measurementMethod = assaydf[[i,"measurementMethod"]],
          measurementTechnique = assaydf[[i,"measurementTechnique"]],
          Foldername = assaydf[[i,"tarname"]] # should be a file name rather then a folder name, and only these files are transferred (if df["_to_upload"] == "v")
        )

        l["_to_upload"] <- "v"
        #df_line <- as.data.frame(do.call(cbind, l))
        df <- rbind(df, as.data.frame(do.call(cbind, l)))
        #df <- rbind(df, as.data.frame(l))

        # possibly also tarlist.txt?
        #df <- rbind(df, df) # this will add the same metadata for the tar file also to the other files.
        #df$Foldername <- c(df$Foldername[1], file.path(folder, paste0(assaydf[[i,"tarname"]],".tarlist.txt")))
        #df[nrow(df)+1,] <- rep("-",ncol(df)) # no need to add metadata to the tarlist file; this last line is to transfer also the tarlist file
        df[nrow(df)+1,] <- df[nrow(df),] # I need "PROJID" and "EXPID" to transfer files to the right target path
        df[nrow(df),"Foldername"] <- paste0(assaydf[[i,"tarname"]],".tarlist.txt") # tarlist filename is based on what user provided in tarname field
        df[nrow(df),"_to_upload"] <- "v"
      }
    }
    # to Excel file add an ro-crate-metadata.json per Study to transfer to Tape
    print(df)
    #df[nrow(df)+1,] <- rep("-",ncol(df)) # the json itself does not need to have metadata
    df[nrow(df)+1,] <- df[nrow(df),] # I need "PROJID" and "EXPID" to transfer files to the right target path
    df[nrow(df),"Foldername"] <- "ro-crate-metadata.json"
    df[nrow(df),"_to_upload"] <- "v"
    print(df)
    openxlsx::write.xlsx(df, file = file.path(folder, outfilename))
  }
}

# add metadata to Investigation folder; create csv file as an Excel file is less convenient for quickly printing in Linux
makeiRODSProjectMetadata <- function(user_excel, outfilename){
  projectdf <- readxl::read_excel(user_excel, sheet = "Investigation")
  if (nrow(projectdf)==1){print(paste0("Start to make metadata Excel file for project ", projectdf$PROJID))
  } else {
    print("Excel file is not filled in correctly: it is only possible to provide a single Project/Investigation per submission (with one or more Studies/Experiments in the Study tab)")}

  file.create(outfilename)
  folder <- "upload"
  # as it was already checked that table has a single row, no need to loop over lines like for the Assay and Study dataframes
  # keys (column names) that can be selected: "PROJID" "title" "description" "people" "email" "dateCreated" "grantIdentifier" "wurProjectnr"
  l = list(
    PROJID = projectdf[[1,"PROJID"]],
    title = projectdf[[1,"title"]],
    description = projectdf[[1,"description"]],
    dateCreated = as.character(projectdf[[1,"dateCreated"]]),
    people = projectdf[[1,"people"]],
    email = projectdf[[1,"email"]],
    grantIdentifier  = projectdf[[1,"grantIdentifier "]],
    wurProjectnr = projectdf[[1,"wurProjectnr"]]
  )
  df <- as.data.frame(do.call(cbind, l))
  # write.csv2 uses a comma for the decimal point and a semicolon for the separator (because some fields contain comma-separated lists)
  write.csv2(df, file=outfilename, row.names = FALSE, quote = FALSE)
}

# add metadata to Study folder; create csv file as an Excel file is less convenient for quickly printing in Linux
makeiRODSStudyMetadata <- function(user_excel, EXPID, outfilename){
  studydf_full <- readxl::read_excel(user_excel, sheet = "Study")
  outfilename = file.path("upload", EXPID, "metadata_study.csv")
  studydf <- studydf_full[studydf_full$EXPID==EXPID,]

  if (nrow(studydf)==1){print(paste0("Start to make metadata Excel file for Study ", studydf$EXPID))
  } else {
    cat("Excel file is not filled in correctly: there should be one row with EXPID ", EXPID, "\n" )}

  file.create(outfilename)
  # keys (column names) that can be selected: "PROJID" "EXPID" "title" "description" "people" "email"
  df = data.frame(
    PROJID = studydf$PROJID,
    EXPID = studydf$EXPID,
    title = studydf$title,
    description = studydf$description,
    people = studydf$people,
    email = studydf$email
  )
    # write.csv2 uses a comma for the decimal point and a semicolon for the separator; as some fields contain comma-separated lists that
    # will be split
    write.csv2(df, file=outfilename, row.names = FALSE, quote = FALSE)
}




# ===========================================================================================================
# B. functions to write RO-crate metadata json file "ro-crate-metadata.json"
# Functions: 
# create_ro_crate() - create an RO crate
# add_investigation_from_excel() - add Project/Investigation information from user Excel file to RO crate
# add_studies_from_excel() - add Experiment/Study information from all Studies from user Excel file to RO crate
# add_studies_from_excel_make_cratelist() - add Experiment/Study information from user Excel file to one RO crate per Study
# add_assays_from_excel_perstudycrate() - add Assay information from user Excel file to RO crate per Study
# write_ro_crate_perExp() - write one ro-crate-metadata.json per Study in ./upload/EXPID
# ===========================================================================================================


# compatible with multi-line Study sheets #
# Like Dataplant
# eg says Investigation ID should be: "@id": "./"

create_ro_crate <- function() {
  list(
    "@context" = list("https://w3id.org/ro/crate/1.2/context"), # optionally add additional contexts, like #"https://raw.githubusercontent.com/TheJacksonLaboratory/ro-crate-isa-context/main/isa/isa_context_1_0.json"
    "@graph" = list(
      list(
        "@id" = "ro-crate-metadata.json",
        "@type" = "CreativeWork",
        "about" = list("@id" = "./")
      )
    )
  )
}


# Helper function to extract givenName and familyName; returns list with property-value pairs appropriate for a schema.org Person Type
extract_person_info <- function(full_name, email) {
  name_parts <- strsplit(full_name, " ")[[1]]
  given_name <- name_parts[1] # assume it is the part until the first space (keeps names with - intact but fails when last name contains a space)
  family_name <- paste(name_parts[-1], collapse = " ") # without the part before the first space (in case first name contains one or more spaces)

  # check for different order names and email addresses: is last part of family name part of email address? If not assume a mistake has been made
  if (!grepl( tolower(unlist(strsplit(family_name, " "))[length(unlist(strsplit(family_name, " ")))]), tolower(email))) {
    stop(paste0("Family name ", family_name," not found in email ", email))
  }
  list(
    "@id" = email, # using email address as id. Online examples sometimes use ORCID
    "@type" = "Person",
    givenName = given_name,
    familyName = family_name,
    name = full_name, # also keep, as processing first and last names may not always be successful
    email = email
    # could add more properties such as affiliation, in form of a ror, eg for Wageningen University & Research: https://ror.org/04qw24q55
  )
  }


# to RO crate add: all Investigation key-value pairs as they are written in the Excel file, but also 'translate' some to schema convention, 
# eg type, additionalType, process the people and email fields) Also add obligatory Property 'name': set to value of 'title'
add_investigation_from_excel <- function(crate, excelpath) {
  sheets <- readxl::excel_sheets(excelpath)
  df <- readxl::read_excel(excelpath, sheet = "Investigation")

  # process people and email keys, which can be a single person or a comma-separated list of persons in the Excel input file
  # assume same order for both fields (check if last name occurs in email address; if not found exits function)
  peoplenames_string = df$people
  emails_string = df$email
  names_vec <- trimws(unlist(strsplit(peoplenames_string, ",")))
  emails_vec <- trimws(unlist(strsplit(emails_string, ",")))
  people <- lapply(seq_along(names_vec), function(i) extract_person_info(names_vec[i], emails_vec[i]))

  creators_list <- lapply(people, function(person) list(`@id` = person$`@id`))

  metadata1 = list(
    "@id" = "./",
    "@type" = "schema.org/Dataset",
    "additionalType" = "Investigation",
    "identifier" = df$PROJID,
    "name" = df$title,
    creator = creators_list)

  metadata2 = as.list(df)

  crate[["@graph"]] <- append(crate[["@graph"]], list(c(metadata1,metadata2)))
  crate[["@graph"]] <- append(crate[["@graph"]], people)
  #crate[["@graph"]][[2]][["hasPart"]] <- append(crate[["@graph"]][[2]][["hasPart"]], list(list("@id" = id)))

  crate
}


# create a list of crates, each containing the Investigation metadata. Length of the list is the number of Studies in the Excel file
# Add Study metadata to each crate in the list
add_studies_from_excel_make_cratelist <- function(crate, excelpath) {
  sheets <- readxl::excel_sheets(excelpath)
  df <- readxl::read_excel(excelpath, sheet = "Study")

  cratelist = list()

  for (row in seq(nrow(df))){
    id = df[[row,"EXPID"]]
    # process people key, which can be a single person or a comma-separated list of persons
    peoplenames_string = df[[row,"people"]]
    emails_string = df[[row,"email"]]
    names_vec <- trimws(unlist(strsplit(peoplenames_string, ",")))
    emails_vec <- trimws(unlist(strsplit(emails_string, ",")))

    people <- lapply(seq_along(names_vec), function(i) extract_person_info(names_vec[i], emails_vec[i]))
    # Build creator list
    creators_list <- lapply(people, function(person) list(`@id` = person$`@id`))

    metadata1 = list(
      "@id" = df[[row,"EXPID"]], # or: file.path("ArchvPROD/PSG/PBR/",df[[row,"PROJID"]],df[[row,"EXPID"]])
      "@type" = "schema.org/Dataset",
      "additionalType" = "Study",
      "identifier" = df[[row,"EXPID"]],
      "name" = df[[row,"title"]],
       creator = creators_list
      )

    metadata2 = as.list(df[row,])
    # initially filled cratelist per row/Study (not named list) but if I want to add Assay to Study perhaps convenient to use Study name/EXPID?
    # changed all cratelist[[row]] to cratelist[[id]]
    cratelist[[id]] <- crate
    #cratelist[[id]][["@graph"]] <- append(cratelist[[id]][["@graph"]], list(c(metadata1,metadata2)))
    cratelist[[id]][["@graph"]] <- append(cratelist[[id]][["@graph"]], list(c(metadata1,metadata2)))
    cratelist[[id]][["@graph"]] <- append(cratelist[[id]][["@graph"]], people)
    cratelist[[id]][["@graph"]][[2]][["hasPart"]] <- append(cratelist[[id]][["@graph"]][[2]][["hasPart"]], list(list("@id" = id)))
    #jsonlite::write_json(cratelist[[id]], file.path("upload", df[[row,"EXPID"]], "ro-crate-metadata.json"), auto_unbox = TRUE, pretty = TRUE)
  }
  cratelist
}

#people <- lapply(seq_along(names_vec), function(i) extract_person_info(names_vec[i], emails_vec[i]))



# Add assays metadata to it's Studies' crate
add_assays_from_excel_perstudycrate <- function(crate, excelpath, this_EXPID) {
  assaydf <- readxl::read_excel(excelpath, sheet = "Assay")
  df <- filter(assaydf, EXPID==this_EXPID)
  for (row in seq(nrow(df))){
    id = df[[row,"tarname"]]

    metadata1 = list(
      "@id" = df[[row,"tarname"]],
      "@type" = "schema.org/Dataset",
      "additionalType" = "Assay",
      "identifier" = df[[row,"tarname"]],
      "name" = df[[row,"description"]])

    metadata2 = as.list(df[row,])

    crate[["@graph"]] <- append(crate[["@graph"]], list(c(metadata1,metadata2)))
    #crate[["@graph"]] <- append(crate[["@graph"]], people) # how can I avoid double persons in the end, if eg the same person is added to both Investigation and Study?
    # find Study to which Assay needs to be added to "hasPart" Property
    #study_graph_item <- which(sapply(seq_along(crate[["@graph"]]), function(x) crate[["@graph"]][[x]]$"@id"==df[[row,"EXPID"]]))
    #study_graph_item <- which(sapply(crate[["@graph"]], function(x) x$"@id"==df[[row,"EXPID"]]))
    study_graph_item <- which(sapply(crate[["@graph"]], function(x) x$"@id"==this_EXPID))
    crate[["@graph"]][[study_graph_item]][["hasPart"]] <- append(crate[["@graph"]][[study_graph_item]][["hasPart"]], list(list("@id" = id)))
    #crate[["@graph"]][[6]][["hasPart"]] <- append(crate[["@graph"]][[6]][["hasPart"]], list(list("@id" = id)))
  }
  crate
}

# write ro-crate-metadata.json files to the Study folders
write_ro_crate_perExp <- function(crates, folder = "./", output_file = "ro-crate-metadata.json") {
  # crates should have a single Investigation/Project, with a single Study (graph[[2]]$hasPart[[1]] contains this single EXPID)
  for (crate in crates){
    experiment = crate$`@graph`[[2]]$hasPart[[1]]$`@id`
    #EXPfolder <- paste0("data-", experiment)
    exp_folder <- file.path("upload", experiment)
    jsonlite::write_json(crate, file.path(folder, exp_folder, output_file), auto_unbox = TRUE, pretty = TRUE)
  }
}



####################################################################################################################
###################################### functions not used for current workflow #####################################
####################################################################################################################

#
#
# makeiRODSFolderMetadata <- function(user_excel, outfilename){
#
#   projectdf <- readxl::read_excel(user_excel, sheet = "Investigation")
#   if (nrow(projectdf)==1){print(paste0("Start to make metadata Excel file for project ", projectdf$PROJID))
#   } else {
#     print("Excel file is not filled in correctly: it is only possible to provide a single Project/Investigation per submission (with one or more Studies/Experiments in the Study tab)")}
#
#   studydf <- readxl::read_excel(user_excel, sheet = "Study")
#   file.create(outfilename)
#   for (i in seq(nrow(studydf))){
#     EXPID = studydf[[i,"EXPID"]]
#     folder <- paste0("data-", EXPID)
#     df = data.frame(
#       PROJID = studydf[[i,"PROJID"]],
#       EXPID = studydf[[i,"EXPID"]],
#       title = studydf[[i,"title"]],
#       description = studydf[[i,"description"]],
#       dateCreated = as.character(studydf[[i,"dateCreated"]]),
#       license = studydf[[i,"license"]],
#       people = studydf[[i,"people"]],
#       email = studydf[[i,"email"]],
#       orgID = studydf[[i,"orgID"]],
#       orgname = studydf[[i,"orgname"]],
#       filetype = studydf[[i,"filetype"]],
#       platform = studydf[[i,"platform"]],
#       strategy = studydf[[i,"strategy"]]
#     )
#     write.csv2(df, file=outfilename, row.names = FALSE, quote = FALSE, append = TRUE)
#
#   }
# }






makeiRODSStudyMetadata_old <- function(user_excel, outfilename){
  studydf <- readxl::read_excel(user_excel, sheet = "Study")

  file.create(outfilename)
  for (i in seq(nrow(studydf))){
    EXPID = studydf[[i,"EXPID"]]
    #folder <- file.path("upload", assaydf[[row,"EXPID"]])
    folder <- file.path("upload",EXPID)
    # keys (column names) that can be selected: "PROJID" "EXPID" "title" "description" "people" "email"
    df = data.frame(
      PROJID = studydf[[i,"PROJID"]],
      EXPID = studydf[[i,"EXPID"]],
      title = studydf[[i,"title"]],
      description = studydf[[i,"description"]],
      people = studydf[[i,"people"]],
      email = studydf[[i,"email"]]
    )
    # write.csv2 uses a comma for the decimal point and a semicolon for the separator; as some fields contain comma-separated lists that
    # will be split
    write.csv2(df, file=outfilename, row.names = FALSE, quote = FALSE, append = TRUE)
  }
}


############# Functions for writing eventually a single ro-crate-metadata.json per Project
# Add all studies to the crate
add_studies_from_excel <- function(crate, excelpath) {
  sheets <- readxl::excel_sheets(excelpath)
  df <- readxl::read_excel(excelpath, sheet = "Study")

  for (row in seq(nrow(df))){
    id = df[[row,"EXPID"]]

    # process people key, which can be a single person or a comma-separated list of persons
    peoplenames_string = df[[row,"people"]]
    emails_string = df[[row,"email"]]
    names_vec <- trimws(unlist(strsplit(peoplenames_string, ",")))
    emails_vec <- trimws(unlist(strsplit(emails_string, ",")))

    people <- lapply(seq_along(names_vec), function(i) extract_person_info(names_vec[i], emails_vec[i]))
    # Build creator list
    creators_list <- lapply(people, function(person) list(`@id` = person$`@id`))

    metadata1 = list(
      "@id" = df[[row,"EXPID"]],
      "@type" = "schema.org/Dataset",
      "additionalType" = "Study",
      "identifier" = df[[row,"EXPID"]],
      "name" = df[[row,"title"]],
      creator = creators_list)

    metadata2 = as.list(df[row,])

    crate[["@graph"]] <- append(crate[["@graph"]], list(c(metadata1,metadata2)))
    crate[["@graph"]] <- append(crate[["@graph"]], people) # how can I avoid double persons in the end, if eg the same person is added to both Investigation and Study?
    crate[["@graph"]][[2]][["hasPart"]] <- append(crate[["@graph"]][[2]][["hasPart"]], list(list("@id" = id)))
  }
  crate
}

# Add assays to the crate (one crate per Investigation/project)
add_assays_from_excel <- function(crate, excelpath) {
  df <- readxl::read_excel(excelpath, sheet = "Assay")

  for (row in seq(nrow(df))){
    id = df[[row,"tarname"]]

    # for now no people info added to Assay level
    # # process people key, which can be a single person or a comma-separated list of persons
    # peoplenames_string = df[[row,"people"]]
    # emails_string = df[[row,"email"]]
    # names_vec <- trimws(unlist(strsplit(peoplenames_string, ",")))
    # emails_vec <- trimws(unlist(strsplit(emails_string, ",")))
    #
    # people <- lapply(seq_along(names_vec), function(i) extract_person_info(names_vec[i], emails_vec[i]))
    # # Build creator list
    # creators_list <- lapply(people, function(person) list(`@id` = person$`@id`))

    metadata1 = list(
      "@id" = df[[row,"tarname"]],
      "@type" = "schema.org/Dataset",
      "additionalType" = "Assay",
      "identifier" = df[[row,"tarname"]],
      "name" = df[[row,"description"]])

    metadata2 = as.list(df[row,])

    crate[["@graph"]] <- append(crate[["@graph"]], list(c(metadata1,metadata2)))
    #crate[["@graph"]] <- append(crate[["@graph"]], people) # how can I avoid double persons in the end, if eg the same person is added to both Investigation and Study?
    # find Study to which Assay needs to be added to "hasPart" Property
    study_graph_item <- which(sapply(seq_along(crate[["@graph"]]), function(x) crate[["@graph"]][[x]]$"@id"==df[[row,"EXPID"]]))
    crate[["@graph"]][[study_graph_item]][["hasPart"]] <- append(crate[["@graph"]][[study_graph_item]][["hasPart"]], list(list("@id" = id)))
  }
  crate
}

# write ro-crate metadata json for entire project
write_ro_crate <- function(crate, folder = "./", output_file = "ro-crate-metadata.json") {
  jsonlite::write_json(crate, file.path(folder, output_file), auto_unbox = TRUE, pretty = TRUE)
}
