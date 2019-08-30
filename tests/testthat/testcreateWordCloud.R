context("Function:createWordCloud")

test_that("the function works as expected",{
  
  text <- c(rep("test", 10), rep(stringi::stri_rand_strings(10, sample(7:12, 10, replace = TRUE), pattern = "[A-Za-z0-9]"), 3)) 
    
  expect_is(createTermDocumentMatrix(text, type = "text"), "list")
  
  expect_length(createTermDocumentMatrix(text, type = "text"), 2)
  
  expect_identical(as.character(createTermDocumentMatrix(text, type = "text")$freqTable[[1]][1]),
                   "test")
  
  
  }
  )
  
