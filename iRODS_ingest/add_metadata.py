from getpass import getpass
from pathlib import Path
import argparse
import logging
import multiprocessing
import pandas as pd
import queue

import utils as utils
from __init__ import FIVE_TB_FILE_LIMIT
# iBridges instantiates a logger which causes the basic config setting to be ignored
utils.setup_logger()
import ioperations as ioperations
from smb import SMB
from helpers import create_task_df, check_paths
from zipper import ZipperProcess
from ibridges import Session


if __name__ == "__main__":
    # Parse arguments
    parser = argparse.ArgumentParser(description="Script to process and upload files.")
    parser.add_argument('--config', type=str, required=False, help='Path to the config file')
    parser.add_argument(
        "--level",
        choices=["study", "investigation"],
        required=True,
        help="Add metadata to folder level: 'study' or 'investigation'"
    )
    args = parser.parse_args()

    # Check and load the config
    if args.config:
        config_file = Path(args.config)
    else:
        config_file = Path(__file__).parent.joinpath("config_add_foldermetadata.json")
    if not utils.check_file_exists(config_file):
        logging.error('Missing config file, exiting')
        exit(1)
    config = utils.load_json(config_file)

    # # Prep progress CSV path
    # if 'PROGRESS_FILE' in config.keys() and config['PROGRESS_FILE'] and Path(config['PROGRESS_FILE']).parent.is_dir():
    #     progress_file_path = Path(config['PROGRESS_FILE'])
    # else:
    #     progress_file_path = Path(__file__).parent.joinpath('in_progress_metadata.csv')

    # Retreive users password, used to mount the W if desired and login to iRODS
    password = getpass('Your iRODS password')
    # utils.check_file_exists(config['IRODS_TARGET_PATH'])
    # #utils.check_file_exists(config['LOCAL_SOURCE_PATH'])
    # utils.check_file_exists(config['METADATA_PROJ_CSV'])
    # utils.check_file_exists(config['METADATA_STUDY_CSV'])

    source_path, zip_path, target_ipath, ienv = check_paths(config, password)

    env_file = Path("~").expanduser().joinpath(".irods", config['IRODS_ENV_FILE'])
    ienv = utils.load_json(env_file)
    isession = Session(irods_env=ienv, password=password)

    source_path = Path(config['LOCAL_SOURCE_PATH'])
    # Check if there is an 'in_progress_metadata.csv', if not create it
    #metada_df_proj = pd.read_excel(Path(source_path).joinpath(config['METADATA_PROJ_EXCEL']),
    #                            skiprows=0, engine="openpyxl")


    if args.level == "investigation":
        # Code to run for study
        print("Addding metadata to Investigation")
        # put your study-specific logic here

        # add metadata to Project folder
        metada_df_proj = pd.read_csv(Path(source_path).joinpath(config['METADATA_PROJECT_CSV']),
        skiprows=0, sep=";")
        to_upload_df = metada_df_proj
        if '_status' not in to_upload_df.columns:
            to_upload_df['_status'] = ""


        # Add metadata
        for ind, row in to_upload_df.iterrows():
            row['_iPath'] = Path(config['IRODS_TARGET_PATH']).joinpath(row['PROJID']) # /ArchvPROD/PSG/PBR/PROJIDxxxxx
            ioperations.add_metadata_folder(isession, row)
            to_upload_df.at[ind, '_status'] = 'Metadata added'


    elif args.level == "study":
        # Code to run for investigation
        print("Addding metadata to Study")



        # add metadata to Study folder
        metada_df_study = pd.read_csv(Path(source_path).joinpath(config['METADATA_STUDY_CSV']),
            skiprows=0, sep=";")
        to_upload_df = metada_df_study
        if '_status' not in to_upload_df.columns:
            to_upload_df['_status'] = ""

        #to_upload_df.to_csv(progress_file_path, index=False)

        # Add metadata
        for ind, row in to_upload_df.iterrows():
            row['_iPath'] = Path(config['IRODS_TARGET_PATH']).joinpath(row['PROJID']).joinpath(row['EXPID']) # /ArchvPROD/PSG/PBR/PROJIDxxxxx/Exxxxx
            ioperations.add_metadata_folder(isession, row)
            to_upload_df.at[ind, '_status'] = 'Metadata added'
        #to_upload_df.to_csv(progress_file_path, index=False)

        # Print the summary of the statuses
        status_counts = to_upload_df['_status'].value_counts()
        logging.info(status_counts)
