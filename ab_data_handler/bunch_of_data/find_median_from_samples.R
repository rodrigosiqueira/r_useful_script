# Copyright (C) 2016 Rodrigo Siqueira
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License version 3 as
# published by the Free Software Foundation.

# Capture samples
# @param path Path to a directory with all csvs files
list_of_samples <- function(path)
{
  if (!dir.exists(path))
  {
    stop("Please, inform a valid directory")
  }

  # Handling files
  file_samples <- list.files(path, pattern='*.csv', full.names = FALSE)
  file_numbers <- as.numeric(sub('\\.csv$', '', file_samples))

  # Read all files
  all_files <- lapply(file.path(path, file_samples), read.csv)

  return(all_files)
}

# Based on a folder with samples (csv files), read all of them and calculate
# median
# @param save_to calculated median to
find_median_from_samples <- function(save_to)
{
  all_files <- list_of_samples(save_to)

  # Set up variables
  total_lines <- nrow(all_files[[1]]) # Expected 101
  total_files <- length(all_files) # Total of samples. It can vary
  final_table <- data.frame(Percentage=numeric(), Time=numeric())

  # Go through each line
  for (lines in 1:total_lines)
  {
    tmp_row <- data.frame(Percentage=numeric(), Time=numeric())
    # Cross all files, and get each column value
    for (target in 1:total_files)
    {
      sample_time <- all_files[[target]][lines, 2]
      value_sample <- data.frame(Percentage=lines, Time=sample_time)
      tmp_row <- rbind(tmp_row, value_sample)
    }
    # Find median
    calculed_median <- median(tmp_row$Time)
    newrow <- data.frame(Percentage=lines, Time=calculed_median)
    final_table <- rbind(final_table, newrow)
  }
  return (final_table)
}

process_data_median <- function(read_files_from, save_to)
{
  table_of_median <- find_median_from_samples(read_files_from)
  write.csv(table_of_median, file=save_to)
}

# Read arguments
paths_arguments <- commandArgs(trailingOnly=TRUE)

# Test if there three argument. If not, return an error.
if (length(paths_arguments) < 2)
{
  stop('You have to supply the target path and destination')
} else if (length(paths_arguments) == 2)
{
  # default output file
  source_files <- paths_arguments[1]
  dest_files <- paths_arguments[2]
}

process_data_median(source_files, dest_files)
