#!/usr/bin/env Rscript

# Copyright (C) 2016 Rodrigo Siqueira
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License version 3 as
# published by the Free Software Foundation.
source(file="util/flexible_parameters.R")

load_tsv_file <- function(path_tsv)
{
  labels <- c('starttime', 'seconds', 'ctime', 'dtime', 'ttime', 'wait')
  
  if(!file.exists(path_tsv))
  {
    stop(paste('File does not exist: ', path_csv))
  }
  loaded_data <- read.table(file=path_tsv, col.names=labels, sep='\t')

  return(loaded_data)
}

# Create boxplot of data visualization
# @param increase_rate Increase rate to use in x axis
# @param base_path base path to read data
# @param dest_path path to save data
boxplot_visualization <- function(graph_name, values_to_plot)
{
  names_line <- names(values_to_plot)
  total_elements <- length(values_to_plot)
  set_of_data <- c()
  colors <- c('blue', 'green', 'red', 'orange', 'black', 'grey', 'navy')

  # Load data, find max value for X and create label variables
  invisible_box <- data.frame()
  for (line in names_line)
  {
    loaded_file <- load_tsv_file(values_to_plot[[line]])
    set_of_data[[line]] <- loaded_file
    l <- data.frame(line = c(loaded_file$ttime))
    invisible_box <- rbind(invisible_box, l)
  }
  png(graph_name, width=1024, height=768)

  boxplot(invisible_box, xlim=c(0.5, total_elements - 0.5),
          boxfill=rgb(1, 1, 1, alpha=1), border=rgb(1, 1, 1, alpha=1),
          xaxt="n", las=2)

  position <- 1
  for (key in names_line)
  {
    current_data <- set_of_data[[key]]
    boxplot(as.numeric(current_data$ttime), add=TRUE, boxwex=0.45, at=position,
            las=2, xaxt="n")
    position <- position + 0.5
  }
  mtext('Average time (ms)', side=2, line=3)
  dev.off()
  return (0)
}

# Read arguments
boxplot_visualization(save_to, parameters)
