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



test_data <- readWUFI::f.GetWufiPlusData_readr_b("D:\\25 Desktop Stuff 30102019\\WUFI Functions beta\\Test TextDateien\\whyjanwhywhyyyy.txt")
