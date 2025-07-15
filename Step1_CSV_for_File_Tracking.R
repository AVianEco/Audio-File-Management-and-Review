# Script Name: Create_CSV_for_File_Tracking.R
#
# Purpose: This script is intended to take a list of files in a directory 
# and create a dataframe. It will allow the user to parse the filepaths and date-times 
# to make the files more easily sortable and analyzed for deficiencies. See
# https://github.com/avianeco for other scripts to analyze recording deficiencies using the 
# CSV created here.
#
# Input: A file directory with acoustic files. This directory can include subfolders. 
# Note however, to create a CSV that can be used with A/Vian Eco's recording deficiency 
# analysis scripts all audio files MUST be on the same level of your data structure.
#
# Two examples are given below for clarity on what is meant by "same level of data structure"
#
# E.g. GOOD: some files in Project_name/Site_1/2023/March/data/audiofile.filetype and 
# some files in Project_name/Site_2/2024/May/data/audiofile.filetype would work.
# 
# E.g. BAD: some files in Project_name/Site_1/2022/audiofile.filetype 
# and some files in Project_name/Site_2/2022/June/data/audiofile.filetype would not work
#
# Output: A modified dataframe saved as a CSV file.
#
# Dependencies: This script requires the libraries listed under "load libraries" to run. 
#
# AI Disclosure: OpenAI's ChatGPT 3.5 was used for coding assistance while 
# developing this R Script.
#
#**User Modifcations Required: The user must: *
#**define location of files, Output CSV name, new Column headers, and device manufacturer. *#
#**Review checking step for data structure consistency**
#     You will see stars "*" on headers where user edits are required.
#
# Version: 11
# Date: March 12, 2024
# Author: Tyne M. Baker
# Change Log: 
#   24JAN2023- V1- Initial drafting of functional script.
#   31JAN2023- V2 Clean-up and formatting to match other AVian scripts.
#   1FEB2023- V3 Add "remove non .wav files" functionality.
#   18APR2023- V4 Add device specifications to improve time-date parsing.
#   19APR2023- V5 Add device-specific time and date parsing steps.
#   20APR2023- V6 Add steps to check for misplaced audio files and remove non-audio files.
#   26APR2023- V7 Add file size column to original dataframe.
#   26JUN2023- V8 Add sampling frequency and length to original dataframe.
#   12MAR2024- V9 Add AI disclosure, tidy script.
#   06MAY2024- V10 Revise the Time and Date parsing to account for converted W4V 
#                   files from Wildlife Acoustics.
#   25NOV2024- V11 Revise time and date to pull info in reference to file suffix.

# ------Clear the Workspace-------
rm(list = ls())

# ------Load Libraries------
library(tidyverse) 
library(tcltk)
library(tuneR)

# ------Define File Locations and Output Name*------

# Find the file path to the files you want to index.
#   This will pop up a window for selection, you may have to minimize RStudio to see this window.
source_filepath <- tk_choose.dir()

# Find the directory where you'd like to save the modified output csv. 
destination_filepath <- tk_choose.dir()

# Edit the file name within the "" below to name the output csv. Leave the quotes and
# ".csv" in the output name. 
output_CSV_name <-"Audio_file_tracking.csv"

#----Device specifications*---

# Below are File naming conventions for specific device manufacturers. If your file
#   naming convention matches one of these, you can input the associated manufacturer 
#   to parse date-time from your files accurately in subsequent steps. 

# Copy and paste your manufacturer name within the "" into the following code 
# to replace the capitalized words where indicated.

    #"Titley Scientific": Prefix_YYYY-MM-DD_HH-MM-SS.filetype
    #"Wildlife Acoustics": Prefix_YYYYMMDD_HHMMSS.filetype
    #"Open Acoustic Devices": YYYYMMDD_HHMMSS.wav
    #"Cornell": Prefix_YYYYMMDD_HHMMSS.wav
    #"Frontier Labs": YYYYMMDDTHHMMSS+RecordingName.wav
    #"Other" (including Peersonic, Avisoft, etc.): Unknown or user-defined. Date and 
    #   time cannot be parsed by this script for these manufacturers.

device_manuf<-c("INSERT NAME OF MANUFACTURER FROM ABOVE OPTIONS")

#-----Create a file list dataframe-----

#Note portions of this script referencing your file directory may take a while 
# to run depending on your hard drive speed and the number of files you are indexing. 

#list all the files
File_List<-list.files(path = source_filepath,full.names=TRUE,recursive=TRUE)

#count the number of files
num_file<-length(File_List)

#create a dataframe from the list
file_df <- data.frame(ID = 1:num_file, file_loc = File_List)

# ------Define New Column Headers*-----

# Find the first filepath with the most subfolders, and print it in the console.
max_index <- str_count(file_df$file_loc, "/")%>%
  which.max()
print(file_df[max_index,"file_loc"])

#Use the printed path to create headers for new columns containing information in the sub-folders.
#   ensure your column with audio file names (e.g. 20210914_120436.wav) is called "file_name",
#   and the folder indicating your site name is "site_ID" to use this script with 
#   subsequent data-checking scripts.
# Ensure all column names are unique.
header_file_df<-c("insert","heading","names","for","each","item","before","slashes","file_name")

# ------Split out the new columns------

#Split the columns by "/" create a new dataframe with the edited column headers.
file_df<-separate(data = file_df, col = file_loc, into = c(header_file_df), sep = "/", remove = FALSE)

#view split columns:
view(file_df)

#------Check that your data structure is consistent*----

# Define audio file extensions
audio_exten <- c(".wav", ".mp3", ".zc", ".w4v")

# find rows that don't match the audio-file pattern
files_unmatched <- file_df[!grepl(paste(audio_exten, collapse = "|"), file_df$file_name), ]

# View the results
view(files_unmatched)

# Are there audio file names out of the "file_name" column? 

misplaced_audio<-files_unmatched %>%
  select(-file_name) %>%
  filter_all(any_vars(str_detect(., paste(audio_exten, collapse = "|"))))

view(misplaced_audio)

# This could mean you have inconsistency in your file structure. 
#   e.g. Do some sites have more subfolders than others?  You may have to do some file 
#   management to have all audio files on the same level of the directory, 
#   or you may need to fix your column headers.

#------Remove NAs and other non-audio files from your file list----

# If there are no audio files misplaced but you have NAs or non_audio extensions (e.g. .txt)
#   in "file_name" column you will need to remove "unmatched" rows to parse date-time.

# If you prefer to export the whole data frame with all files but without parsed date-time, 
#   you can skip to the export section and use "file_df" instead of "file_track" 
#   in the final CSV export step. 

# Remove rows where file name isn't an audio file
file_track <<- file_df %>%
  filter_all(any_vars(str_detect(file_name, paste(audio_exten, collapse = "|"))))

view(file_track)

#-----Read in info about the file (size in mb, duration, sampling frequency)----

#Pull sampling frequency and length in seconds for each file
 #this may take time to process, see the console for progress details.
for (i in 1:nrow(file_track)) {
  file_path <- file_track$file_loc[i]
  
  # Wrap the readWave() function in a tryCatch block
  tryCatch({
    audio <- readWave(file_path)
    file_info <- file.info(file_path)
    
    # Sampling frequency
    sampling_frequency <- audio@samp.rate
    
    # Duration in seconds
    duration <- length(audio@left) / audio@samp.rate
    
    #file size in bytes
    file_size_bytes <- file_info$size
    
    # Assign values to new columns
    file_track$sampling_frequency[i] <- sampling_frequency
    file_track$duration_s[i] <- duration
    file_track$file_size_mb[i] <- file_size_bytes/(1024*1024)
    
    #describe progress in the console
    cat("Processed file", i, "of", nrow(file_track), "files\n")
  }, error = function(e) {
    # Handle errors
    cat("Error processing file", i, ":", conditionMessage(e), "\n")
    # Optionally, you can set default values for the columns to indicate the file couldn't be processed.
    # For example, file_track$sampling_frequency[i] <- NA
    #                          file_track$duration_s[i] <- NA
    #                          file_track$file_size_mb[i] <- NA
  }, warning = function(w) {
    # Handle warnings
    cat("Warning in processing file", i, ":", conditionMessage(w), "\n")
  })
}
#view the updated file_track database
view(file_track)

#-----Pull the time out of the file name into a new column-----

# Create a function to find the "." before the file extension type
getdot <- function(x) {
    location <- tail(unlist(gregexpr("\\.", x)), n=1)
    return(location)
    }

# Pull the values that represent time in the file_name based off your specified manufacturer.

for (i in seq_along(file_track$file_name)) {
  if (device_manuf %in% c("Wildlife Acoustics", "Open Acoustic Devices", "Cornell")) {
    location <- getdot(file_track$file_name[i])
    file_track$time[i] <- str_sub(file_track$file_name[i], location-6, location-1) 
  } else if (device_manuf == "Titley Scientific") {
    location <- getdot(file_track$file_name[i])
    file_track$time[i] <- str_sub(file_track$file_name[i], location-8, location-1)
  } else if (device_manuf == "Frontier Labs") {
    file_track$time[i]<-str_sub(file_track$file_name[i],10,15)
  } else {
  }}

# Check if "time" column exists and format time (HH:MM:SS).
if ("time" %in% colnames(file_track)) {
  message("Time column created and formatted")
  if (device_manuf == "Titley Scientific") {
    file_track$time<-parse_time(file_track$time, "%H-%M-%S")
  } else {
    file_track$time<-parse_time(file_track$time, "%H%M%S")
  }
} else {
  message("Time column not created")
  }


#-----Pull the date out of the file name into a new column-----

# Pull the values that represent date in the file_name based off your specified manufacturer.
for (i in seq_along(file_track$file_name)) {
    if (device_manuf %in% c("Wildlife Acoustics", "Open Acoustic Devices", "Cornell")) {
    location <- getdot(file_track$file_name[i])
    file_track$date[i] <- str_sub(file_track$file_name[i], location-15, location-8)
  } else if (device_manuf == "Titley Scientific") {
    location <- getdot(file_track$file_name[i])
    file_track$date[i] <- str_sub(file_track$file_name[i], location-20, location-10)
  } else if (device_manuf == "Frontier Labs") {
    location <- getdot(file_track$file_name[i])
    file_track$date<-str_sub(file_track$file_name,1,8)
  } else {
  }}

# Check if "Date" column exists and format date (YYYY-MM-DD)
if ("date" %in% colnames(file_track)) {
  message("Date column created and formatted")
  if (device_manuf == "Titley Scientific") {
    file_track$date<-parse_date(file_track$date, "%Y-%m-%d")
  } else {
    file_track$date<-parse_date(file_track$date, "%Y%m%d")
  }
} else {
  message("Date column not created")
}

#review the final file_track database*
view(file_track)

#Export file_track to CSV for reference or further manipulation.
write.csv(file_track, file.path(destination_filepath,output_CSV_name), row.names=FALSE)
