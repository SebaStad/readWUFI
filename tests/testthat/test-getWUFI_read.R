test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})


# # Filepath ----------------------------------------------------------------
# v_path <- "D:\\25 Desktop Stuff 30102019\\WUFI Functions beta\\Test TextDateien"
# setwd(v_path)
# file <- list.files(pattern="why")
#
# dat <- readWUFI::f.GetWufiPlusData_vroom(file)
# dat <- readWUFI::f.GetWufiPlusData_readr_b(file)
#
# microbenchmark::microbenchmark(readWUFI::f.GetWufiPlusData_vroom(file),
#                                readWUFI::f.GetWufiPlusData_readr_b(file), times = 10)
#
#
# test_path <- "D:/25 Desktop Stuff 30102019/WUFI Functions beta/Test TextDateien/"
# test_csv  <- "D:/25 Desktop Stuff 30102019/WUFI Functions beta/Test TextDateien/test_csv/"
# test_files <- c("01_percent_date_and_long",
#                 "02_percent_date_and_short",
#                 "03_normal_format",
#                 "04_super_strange_format")
#
# for(i in test_files){
#
#   test_data <- readWUFI::f.GetWufiPlusData_readr_b(paste0(test_path,i,".txt"))
#   readr::write_csv2(test_data, paste0(test_csv,i,".csv"))
#
# }
#
# qq <- readr::read_csv2(paste0(test_csv,i,".csv"))
#
