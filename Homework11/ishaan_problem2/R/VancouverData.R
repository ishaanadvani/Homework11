#VancouverData.R

# load the dataset, but filter for breaking/entry
load_filtered_dataset <- function()
{
  d <- read.csv("raw data/crime_in_vancouver.csv", 
                header=T, stringsAsFactors = F)
  d <- dplyr::filter(d, grepl("Break", d$TYPE))
  return (d)
}