#' Read WUFIPlus Data into R very very quickly!
#'
#' @param v.Path Character. Path to file.
#' @param select.columns Vector. Which columns should be chosen?
#'
#'
#' @return Data. Tibble of WUFIPlus Data.
#' @export v.WufiData Data in WUFIPlusfile
#'
#' @examples
#' #asdf
f.GetWufiPlusData_vroom <- function (v.Path, select.columns = c())
{
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

  if (length(select.columns) == 0) {
    Dist.Col <- c(24, rep(14, Count.Col - 1))                                 # hier comment und quote
    DateNum <- "cc"
    Col.Count <- paste(rep("c", Count.Col - 2), collapse = "")
    v.WufiData <- as.data.frame(vroom::vroom_fwf(v.Path, col_positions = vroom::fwf_widths(Dist.Col,
                                                                                          col_names = NULL), skip = count.columns.line + 1 +
                                                  Count.Col, col_types = paste(DateNum, Col.Count,
                                                                               sep = "")))

    v.WufiData[, 1]<- f.ConvertPercentageDate(v.WufiData[, 1])
    v.DateFormat   <- f.GuessDate_for_GetWUFIplus(v.WufiData[, 1])
    v.WufiData[, 1] <- as.POSIXct(strptime(v.WufiData[, 1],
                                           format = v.DateFormat))

    # Seperator-Abfrage
    o <- 0
    repeat{
      o <- o +1
      Find.Poi <- grepl("\\.", v.WufiData[o,2:length(v.WufiData[1,])])
      Find.Com <- grepl(",", v.WufiData[o,2:length(v.WufiData[1,])])
      if(any(Find.Com)){
        DEC <- ","
        break
      }
      if(any(Find.Poi)){
        DEC <- "."
        break
      }
    }
    for (i in 2:length(v.WufiData[1, ])) {
      tryCatch(v.WufiData[, i] <- utils::type.convert(v.WufiData[,
                                                                 i], dec = DEC), error = function(e) {
                                                                 })
    }

    colnames(v.WufiData) <- Col.Names
    return(v.WufiData)
  }
  if (length(select.columns) > 0) {
    if(1%in%select.columns){select.columns <- select.columns[-which(select.columns==1)]}
    select.columns <- select.columns -1
    endcol <- 24+ (select.columns) * 14                                    # hier comment und quote
    startcol <- endcol - 13
    start <- c(1, startcol)
    end <- c(24, endcol)
    Col.Count <- paste(rep("c", length(select.columns) +
                             1), collapse = "")
    v.WufiData <- as.data.frame(vroom::vroom_fwf(v.Path, col_positions = vroom::fwf_positions(start,
                                                                                             end, col_names = NULL), skip = count.columns.line +
                                                  1 + Count.Col, col_types = Col.Count))
    v.WufiData[, 1]<- f.ConvertPercentageDate(v.WufiData[, 1])
    v.DateFormat <- f.GuessDate_for_GetWUFIplus(v.WufiData[, 1])
    v.WufiData[, 1] <- as.POSIXct(strptime(v.WufiData[, 1],
                                           format = v.DateFormat))
    # Seperator-Abfrage
    o <- 0
    repeat{
      o <- o +1
      Find.Poi <- grepl("\\.", v.WufiData[o,2:length(v.WufiData[1,])])
      Find.Com <- grepl(",", v.WufiData[o,2:length(v.WufiData[1,])])
      if(any(Find.Com)){
        DEC <- ","
        break
      }
      if(any(Find.Poi)){
        DEC <- "."
        break
      }
    }
    for (i in 2:length(v.WufiData[1, ])) {
      tryCatch(v.WufiData[, i] <- utils::type.convert(v.WufiData[,
                                                                 i], dec = DEC), error = function(e) {
                                                                 })
    }
    colnames(v.WufiData) <- Col.Names[c(1, select.columns+1)]

    return(v.WufiData)
  }
}
