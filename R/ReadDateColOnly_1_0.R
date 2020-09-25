#' Old Testing function to only import the DateCol of a WUFIPlus file.
#'
#' @param v.Path Character. Path to WUFIPlus file.
#'
#' @return Character. DateCol of WUFIPlus
#' @export v.WufiData[,1]
#'
#' @examples
#' x <- 1
ReadDateCol.Only <- function(v.Path){
  table <- utils::read.table(v.Path, skip = 0, dec = ",", sep = "\t",
                      header = FALSE, nrow = 7)
  count.columns.line <- 0
  repeat {
    count.columns.line <- count.columns.line + 1
    x <- strsplit(as.character(table[, 1][count.columns.line]),
                  " ")[[1]][1]
    if (x == "1")
      break
  }
  count.columns.line <- count.columns.line - 1
  count.columns.column <- length(utils::read.table(v.Path, nrows = 1,
                                            skip = (count.columns.line - 1)))
  Count.Col <- as.numeric(strsplit(as.character(utils::read.table(v.Path,
                                                           nrows = 1, skip = (count.columns.line - 1))[[count.columns.column]]),
                                   ":"))
  Col.Names <- as.character((utils::read.table(v.Path, nrows = Count.Col,
                                        skip = count.columns.line, sep = "\t",comment="",quote=NULL))[, 1])


  Dist.Col <- c(24, rep(14, Count.Col - 1))                                 # hier comment und quote
  DateNum <- "cc"
  Col.Count <- paste(rep("c", Count.Col - 2), collapse = "")
  v.WufiData <- vroom::vroom(v.Path, col_positions = vroom::fwf_widths(Dist.Col,
                                                            col_names = NULL), skip = count.columns.line + 1 +
                           Count.Col, col_types = paste(DateNum, Col.Count,
                                                        sep = ""))
  return(v.WufiData[, 1])
}
