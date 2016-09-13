#' Load datasets from inst/extdata folder into the memory
#'
#' @param dataset the name of the dataset to load
#' @return dataset
#' @import data.table
#' @export
load_dataset <- function(dataset)
{
  fn <- system.file("extdata", paste0(dataset,".tab"), package = "causalA16")
  DATA <- read.delim(fn)
}
