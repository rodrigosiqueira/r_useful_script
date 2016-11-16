#!/usr/bin/env Rscript

# Copyright (C) 2016 Rodrigo Siqueira
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License version 3 as
# published by the Free Software Foundation.

# Load CSV file, and return data.frame structure
load_csv_file <- function(path_csv)
{
  data <- read.csv(file=path_csv, skip=1,
                   col.names=c('Percentage','Time'))
  return(data)
}

# Create boxplot of data visualization
# @param increase_rate Increase rate to use in x axis
# @param base_path base path to read data
# @param dest_path path to save data
boxplot_visualization <- function(increase_rate, base_path, dest_path)
{
  label_x <- seq(from=increase_rate, by=increase_rate, length.out=10)
  column <- c('NULL', 'NULL', 'NULL', NA, 'NULL')
  options(scipen=100) # FIXME: Change the expansion. Not a good idea

  for (mpm in c('event', 'worker', 'prefork'))
  {
    output_name <- paste(mpm, 'boxplot.png', sep='_')
    current_dest_path <- paste(dest_path, '/', output_name, sep='')
    png(current_dest_path, width=1024, height=768)

    boxes <- list(1:10)

    for (position in 1:10)
    {
      current_file <- paste(label_x[position], mpm, sep='_')
      current_file <- paste(base_path,'/', current_file, '.csv', sep='')
      boxes[[1]][position] <- read.csv(file=current_file, colClasses=column)
    }

    boxplot(boxes[[1]][[1]], boxes[[1]][[2]],
            boxes[[1]][[3]], boxes[[1]][[4]],
            boxes[[1]][[5]], boxes[[1]][[6]],
            boxes[[1]][[7]], boxes[[1]][[8]],
            boxes[[1]][[9]], boxes[[1]][[10]],
            las=2, names=label_x)
    mtext('Average time (ms)', side=2, line=3)
    mtext('Requests', side=1, line=4)
    title(main=mpm)
    dev.off()
  }
  return (0)
}

# Read arguments
pathsArguments <- commandArgs(trailingOnly=TRUE)

# Test if there three argument. If not, return an error.
if (length(pathsArguments) < 4)
{
  stop('You have to supply the path for each mpm strategy')
} else if (length(pathsArguments) == 4)
{
  # default output file
  graph_name <- pathsArguments[1]
  event_file <- pathsArguments[2]
  worker_file <- pathsArguments[3]
  prefork_file <- pathsArguments[4]
}

event <- load_csv_file(event_file)
worker <- load_csv_file(worker_file)
prefork <- load_csv_file(prefork_file)

boxplot_visualization(graph_name, event, worker, prefork)
