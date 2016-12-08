#------------------------------------------------------------------------------
# Handling script execution
#------------------------------------------------------------------------------
# Read arguments
path_arguments <- commandArgs(trailingOnly=TRUE)

# Verify arguments, before start
arguments_length <- length(path_arguments)
if (((arguments_length %% 2) == 0) | (arguments_length < 3))
{
  message <- 'missing file operand'
  usage <- 'Rscript binned_data.R SAVE_IMAGE LINE_NAME FILE_TO_PLOT ...'
  message <- paste(message, usage, sep='\n')
  stop(message)
} else
{
  index <- 1
  parameters <- c()
  for (name in path_arguments)
  {
     # Image path to save
     if (index == 1)
     {
        save_to <- path_arguments[index]
     }
     # Names are even number
     else if (index %% 2 == 0)
     {
       nameIndex <- path_arguments[index]
     }
     # Values are odd number
     else
     {
      parameters[[nameIndex]] <- path_arguments[index]
     }
     index <- index + 1
  }
}
