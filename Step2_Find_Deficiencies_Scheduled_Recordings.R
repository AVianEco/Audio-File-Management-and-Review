# Script Name: Find Recording Deficiencies - for scheduled recordings
#
# Purpose: This script is intended to examine your audio recordings for drop outs by site, and schedule.

#
# Input: A CSV file listing all your audio files. See the Step1_CSV_for_File_Tracking.R script
# available on AVianEco Github for a script to make this CSV from your file directory.
#
# Output: Data visualizations showing recording drop out by site, date, and time.
#
# Dependencies: This script requires the R-packages detailed in "load libraries" section.
#         This script focuses on scheduled recordings, it might be used for triggered recordings
#         but the results may not be as directly helpful in understanding recording errors.
#         A script to examine triggered recordings is in the works.
#
# AI Disclosure: Several versions of OpenAI's ChatGPT and Google Gemini were used 
# for coding assistance while developing this R Script.
#
#**User Modifcations Required: The user must: define the File Location and output directory**
#**Translate your relavent headers to required headers**
#**Define the deployment period and give schedule details**
#     You will see stars "*" on headers where user edits are required.
#
#
# Version: 9
# Date: March 12, 2024
# Author: Tyne M. Baker
# Change Log: #   11MAY2023- V0- Initial drafting of script structure.
              #   26MAY2023- V1- Add a Graph for Plotting File Sizes
              #   29MAY2023- V3- Add an Optional file length graph.
              #   26JUN2023- V4- Add the "problems list" as a summary.
              #   25JUL2023- V5- Add sampling frequency checking step.
              #   10AUG2023- V6- Add recording duration checking step.
              #   21AUG2023- V7- General restructuring, aesthetics, and revising problems list.
              #   12MAR2024- V8- Added AI disclosure, updates to indicate this is for scheduled
                              #   recordings.
              #   10JUL2025- V9 - Work on summary problem table. 

# ------Clear the Workspace-------
rm(list = ls())

# ------Load Libraries------
library(anytime)
library(lubridate)
library(tidyverse)
library(ggplot2)
library(tcltk)

# ------INPUT AND OUTPUTS: Define File Locations and Output Name*------
# Find the file name and path to the saved file tracking CSV on your computer.
source_CSV <- tk_choose.files()

# Find the directory where you'd like to save graph and CSV outputs.
destination_filepath <- tk_choose.dir()

# Load the file
file_track <- read.csv(source_CSV)

#-----DATA CHECKING: Check your headers*-----
# Create the list of required headers
headers_req<-c("file_name","time","date","site_ID")

# Check against your file list.
has_req<-sum(headers_req %in% colnames(file_track))

# Describe in console what columns need replacing.
if(has_req==4){
  message("All required headers exist skip to DATA CHECKING: Format date and time")
} else{
  missing_headers <- headers_req[!(headers_req %in% colnames(file_track))]
  message(paste("You are missing some required headers, this includes: ", paste(missing_headers, collapse = ", "), 
                ". Go to the assign required headers section and replace headers as required."))
}

#-----DATA CHECKING: Assign required headers to your dataframe*-----

#view your dataframe to understand your headers
view(file_track)

#review the code below and replace headers from your dataset with the relevant missing headers

#replace column with audio file names in it with "file_name".
colnames(file_track)[which(colnames(file_track) == "INSERTYOUROLDTIMECOLUMNNAME")]<- "file_name"

#replace column with time in it with "time".
colnames(file_track)[which(colnames(file_track) == "INSERTYOUROLDTIMECOLUMNNAME")]<- "time"

#replace column with dates in it with "date".
colnames(file_track)[which(colnames(file_track) == "INSERTYOUROLDTIMECOLUMNNAME")]<- "date"

#replace column with site numbers or identifiers in it with "site_ID".
colnames(file_track)[which(colnames(file_track) == "INSERTYOUROLDTIMECOLUMNNAME")]<- "site_ID"

#-----DATA CHECKING: Format date and time-----

# apply anydate() function to convert to standard YYYY-MM-DD format
file_track$date <- anydate(file_track$date)

#format time using parse-time:
for (i in seq_along(file_track$time)) {
  if (grepl(":", file_track$time[i])) {
    # Do nothing if the time is already in HH:MM:SS format)
  } else {
    # Remove dashes and parse time
    file_track$time[i] <- gsub("-", "", file_track$time[i])
    file_track$time[i] <- parse_time(file_track$time[i], "%H%M%S")
  }
}

#-----SCHEDULE INFORMATION: Input information about your recording schedule-----

#Define number of files per day in your device recording schedule.

fileperday<-24 #edit this number with your own files/day estimate based off your recording schedule.

#Define the date deployed.
deployment_date<-ymd(YYYYMMDD) #edit this with your specific date of deployment

#Define the date retrieved.
retrieval_date<- ymd(YYYYMMDD) #edit this with your specific date of data or device retrieval.

#replace x,y,z with the sampling frequency or frequencies from your programmed
# schedule and settings in Hz (e.g. c(24000); or c(24000,256000,96000))
programmed_SF<-c(x,y,z)

##replace x,y,z with the file lengths from your programmed schedule and settings
# in minutes (e.g. c(5); or c(5,10,60)). If you programmed for "continuous recording" many devices have
# a max recording length setting and/or file size maximum. 
# Check your settings or user manual for this info.
programmed_recduration<-c(x,y,z)

#Calculate number of days between dates
deployment_length <- as.numeric(difftime(retrieval_date, deployment_date, units = "days")+1)
message(paste("Your devices were deployed up to:", deployment_length, "days."))

#Calculate total possible audio recordings
est_total_files<-fileperday * deployment_length
message(paste("Based on your recording schedule and deployment length, 
  the total recordings possible per site are:", est_total_files, "files."))

#Check the number of unique site_IDs
unique_siteIDs <-unique(file_track$site_ID)
message(paste("You have ", length(unique_siteIDs), "unique site IDs in your dataset"))

#-----Plot Number of Recordings Captured per Site-----

#Count number of recordings per site
site_recs <- file_track %>%
  group_by(site_ID) %>%
  summarise(n_files = n())

#Create a column indicating the a percentage of total possible recordings captured per site
site_recs <- site_recs%>%
  mutate(percent_estimated_files = n_files/est_total_files*100)

#Sort the table lowest to highest by percent of estimated files captured.
site_recs <- site_recs %>%
  arrange(percent_estimated_files)

#View the table
view(site_recs)

# Set up the plot
G1 <- ggplot(site_recs, aes(x = site_ID, y = n_files)) +
  # Add the bars
  geom_bar(stat = "identity", fill="royalblue3") +
  # Format the plot and add labels
  labs(title = "Number Recordings by Site",
       x = "Site ID",
       y = "Count of Total Recordings") +
  theme_bw(base_size = 14) +
  # Add the horizontal line and format it
  geom_hline(aes(yintercept = est_total_files, linetype = "Number of Scheduled Files"),
             color = "goldenrod1", linewidth = 1.5, show.legend = TRUE) +
  scale_linetype_manual(name = "", values = "dashed") +
  # Set the fill colors and labels
  scale_fill_manual(values = "goldenrod1",
                    labels = "") +
  ylim(0,(max(max(site_recs$n_files)*1.1, est_total_files))) +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 90))

# Show the plot
G1

#-----Plot Dates Active by Site-----

#create a dataframe with all unique dates by site. 
date_check <- file_track %>%
  group_by(site_ID, date) %>%
  summarise(file_count = n(), .groups = 'drop')

# Order by site_ID and date (ascending oldest to newest)
date_check <- date_check[order(date_check$site_ID, date_check$date),]

view(date_check)

# Plot with ggplot2
G2<- ggplot(date_check, aes(x = date, y = site_ID)) +
        theme_bw() +
        geom_point(aes(size = file_count, color = file_count)) +
        scale_radius(range = c(2, 6)) +      
        scale_color_gradient2(low = "red2", mid = "goldenrod1", high = "yellow",
                              midpoint= mean(date_check$file_count)) +
        labs(x = "Dates Active", y = "Site ID", title = "Files Captured by Date") +
        guides(colour = guide_legend("Number of Files"), 
               size = guide_legend("Number of Files")) +
        theme(legend.position = "bottom",
              axis.text.x = element_text(angle = 90)) +
  #you may wish to change the number of days to make the X-axis more legible*
        scale_x_date(date_breaks = "2 day", date_labels = "%Y-%m-%d")
# Show the plot
G2

#now you can examine this plot to see dates with low numbers of files (small red dots).
#are any dates unexpectedly low?

#-----Plot Times active by site-----

#create a dataframe with all unique dates by site. 
time_check<-file_track %>%
  group_by(site_ID, time) %>%
  summarise(file_count=n(), .groups = "drop")

# Order by site_ID and time
time_check <- time_check[order(time_check$site_ID, time_check$time),]

view(time_check)

# Create a vector of time values from "00:00:00" to "23:00:00"
time_values <- seq(as.POSIXct("00:00:00", format = "%H:%M:%S"), 
                   as.POSIXct("23:00:00", format = "%H:%M:%S"), by = "hour")

# Convert the time column in your data frame to POSIXct format
time_check$time <- as.POSIXct(time_check$time, format = "%H:%M:%S")

# Plot with ggplot2
G3<- ggplot(time_check, aes(x = time, y = site_ID)) +
  theme_bw() +
  geom_point(aes(size = file_count, color = file_count)) +
  scale_size(range = c(2, 6)) +
  scale_color_gradient2(low = "yellow", mid = "seagreen2", high = "royalblue3",
                        midpoint= mean(time_check$file_count)) +
  labs(x = "Times Active", y = "Site ID", title = "Files Captured by Time") +
  guides(colour = guide_legend("Number of Files"), 
         size = guide_legend("Number of Files")) +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 90)) +
  scale_x_time(breaks = time_values, labels = format(time_values, "%H:%M:%S"))

# Show the plot
G3

#now you can examine this plot to see times with low numbers of files (small yellow dots).
#are any times unexpectedly low on files? Is there any time missing?
#are there any small yellow overlapping blue or greenish dots? These are probably times that have a
#   strange slightly shifted time (e.g. 10:00:07 instead of programmed recording time of 10:00:00).

#-----Plot file duration-----

#Create a dataset that describes the number of files matching or not matching the programmed recording duration.
duration_check <- file_track %>%
  mutate(duration_min_rounded = round(duration_s / 60)) %>%
  group_by(site_ID, duration_min_rounded) %>%
  summarise(file_count = n(), .groups = "drop") %>%
  mutate(duration_match = duration_min_rounded %in% programmed_recduration) %>%
  group_by(site_ID, duration_match) %>%
  summarise(n_files = sum(file_count), .groups = "drop") %>%
  group_by(site_ID) %>%
  mutate(
    percent = n_files / sum(n_files) * 100,
    #if your number of files per site exceeds 1,000,000 increase the digits in this round.
    label = paste0(round(percent,digits = 3), "%")
  )

#create a pie charts that show the percent matching vs. not matching durations by site.
G4<- ggplot(duration_check, aes(x = "", y = percent, fill = duration_match)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  facet_wrap(~ site_ID) +
  theme_void() +
  labs(title = "% Files Matching Programmed Duration")+
  scale_fill_manual(values = c("TRUE" = "seagreen2", "FALSE" = "red2")) +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5)) +
  labs(fill = "Match?")

#Show the Plots
G4

#-----Plot file sampling frequency-----

#Create a dataset that describes the number of files matching or not matching the programmed sampling frequency.
SF_check <- file_track %>%
  group_by(site_ID, sampling_frequency) %>%
  summarise(file_count = n(), .groups = "drop") %>%
  mutate(SF_match = sampling_frequency %in% programmed_SF) %>%
  group_by(site_ID, SF_match) %>%
  summarise(n_files = sum(file_count), .groups = "drop") %>%
  group_by(site_ID) %>%
  mutate(
    percent = n_files / sum(n_files) * 100,
    #if your number of files per site exceeds 1,000,000 increase the digits in this round.
    label = paste0(round(percent, digits = 3), "%")
  )

#create a pie charts that show the percent matching vs. not matching durations by site.
G5<- ggplot(SF_check, aes(x = "", y = percent, fill = SF_match)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  facet_wrap(~ site_ID) +
  theme_void() +
  labs(title = "% Files Matching Programmed Sampling Frequency")+
  scale_fill_manual(values = c("TRUE" = "royalblue", "FALSE" = "yellow")) +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5)) +
  labs(fill = "Match?")

#Show the Plots
G5

#OVERVIEW LIST: Create a summary list of errors-----

#list all recording errors we will review in this script
recording_errors <- c("Captured <75% of estimated Files",
                      "Captured <75% of days of mean days recorded at all sites",
                      "Captured unique times not captured at other sites",
                      "Sampling frequency different from programmed",
                      "Recording lengths are differ by 1 min or more from programmed")

#create a dataframe with the list of sites for the recording errors
problems_list <- data.frame(recording_errors = recording_errors)

#create a list of unique sites
site_list <- unique(file_track$site_ID)

# Add empty columns for each site
for (site in site_list) {
  problems_list[[site]] <- NA
}

#-- Check for number of files "Captured <75% of estimated Files"--

#review number of files and check against condition files.
for (site in site_list) {
  # Retrieve the relevant information from site_recs dataframe
  n_files <- site_recs$site_ID[site_recs$site_ID == site]
  percent_estimated <- site_recs$percent_estimated_files[site_recs$site_ID == site]
  
  # Check condition at sites and populate problems_list with the binary value 
  # (1=yes has this problem, 0=no problem)
  if (percent_estimated < 75) {
    problems_list[1, site] <- 1
  } else {
    problems_list[1, site] <- 0
  }
}

# View the updated dataframe
view(problems_list)

#-- Check of dates "Captured <75% of days of mean days recorded at all sites"--

#count number of dates recorded by site
count_dates<- date_check %>%
  group_by(site_ID) %>%
  summarise(days_recording = n(), .groups = 'drop')


#update the problems_list for sites with a low number of unique dates or "low number of days recorded".
for (site in site_list) {
  # Retrieve the relevant information from site_recs dataframe
  n_files <- count_dates$site_ID[count_dates$site_ID == site]
  days_recording <- count_dates$days_recording[count_dates$site_ID == site]
  
  # Check the condition and populate the value in problems_list
  if (days_recording < mean(count_dates$days_recording)*3/4) {
    problems_list[2, site] <- 1
  } else {
    problems_list[2, site] <- 0
  }
}

# View the updated dataframe
view(problems_list)

#-- check for times "Captured unique times not captured at other sites"--

# First, count how many sites each time appears at
time_counts <- time_check %>%
  distinct(site_ID, time) %>%
  group_by(time) %>%
  summarise(n_sites = n(), .groups = 'drop')

# Now, join this back to the original distinct times to identify unique times for each site
site_unique_times <- time_check %>%
  distinct(site_ID, time) %>%
  left_join(time_counts, by = "time")

# Update the problems_list for sites with unique times
for (site in site_list) {
  # Check if there are any times for this site that appear at only 1 site
  has_unique_time <- any(site_unique_times$n_sites[site_unique_times$site_ID == site] == 1)
  
  # Populate the problems_list
  if (has_unique_time) {
    problems_list[3, site] <- 1
  } else {
    problems_list[3, site] <- 0
  }
}

# View the updated dataframe
view(problems_list)

#-- check sampling frequencies "Sampling frequency different from programmed"--

#list sites with unmatched_SF
unmatched_SF_sites <-SF_check %>%
  filter(SF_match == "FALSE") %>%
  pull(site_ID) %>%
  as.list()

#update the problems_list for sites with unexpected sampling frequencies. 
if(length(unmatched_SF_sites) == 0) {
  message("There are no sites with unmatched sampling frequencies")
  problems_list[4,-1]<-0
}else{
  for (i in 1:nrow(site_list)) {
    if (site_list[i, "site"] %in% unmatched_SF_sites) {
      problems_list[4, "site"] <- 1
    } else {
      problems_list[4, "site"] <- 0
    }
  }}

# View the updated dataframe
view(problems_list)

#-- check recording lengths "Recording lengths are differ by 30s or more from programmed"---

#make a df of site with unexpected recording durations.
unmatched_recduration <- duration_check %>%
  filter(duration_match == "FALSE") %>%
  pull(site_ID) %>%
  as.list()
  
#update the problems_list to identify sites with "sites unexpected file lengths"
if(length(unmatched_SF_sites) == 0) {
  message("There are no sites with unmatched times")
  problems_list[5,-1]<-0
}else{
  for (i in 1:nrow(site_list)) {
    if (site_list[i, "site"] %in% unmatched_SF_sites) {
      problems_list[5, "site"] <- 1
    } else {
      problems_list[5, "site"] <- 0
    }
  }}

# View the updated dataframe
view(problems_list)

#-----Export the summary outputs----

# View the plot for Recordings Captured per Site
G1

#Save the graph
ggsave(file.path(destination_filepath, "Num_Files_by_Site.png"), 
       plot = G1, width = 8, height = 6, dpi = 300)

# View the plot for Number of Files by Date
G2

#Save the graph
ggsave(file.path(destination_filepath, "Number_Files_by_Date.png"), 
       plot = G2, width = 8, height = 6, dpi = 300)

#view the plot for Number of Files by Time of Day
G3

#Save the graph
ggsave(file.path(destination_filepath, "Number_Files_by_Time.png"), 
       plot = G3, width = 8, height = 6, dpi = 300)

#view the plot for Percent files matching programmed duration by site
G4

#Save the graph
ggsave(file.path(destination_filepath, "Matching_Programmed_Duration.png"), 
       plot = G4, width = 8, height = 6, dpi = 300)

#view the plot for Percent files matching programmed sampling frequency by site
G5

#Save the graph
ggsave(file.path(destination_filepath, "Matching_Programmed_SF.png"), 
       plot = G5, width = 8, height = 6, dpi = 300)

#view problems list
view(problems_list)

#Export problems list to CSV
write.csv(problems_list, file.path(destination_filepath,"problems_list.csv"), row.names=FALSE)

