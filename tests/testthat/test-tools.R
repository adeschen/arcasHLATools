### Unit tests for functions in tools.R file

library(arcasHLATools)


directory <- system.file("extdata/arcasHLAFiles", package = "arcasHLATools")

### Tests readOneArcasHLAGenotypeFile() function

context("Test for readOneArcasHLAGenotypeFile() function")

test_that("readOneArcasHLAGenotypeFile() should return an error when extension is a numeric", {

    file1 <- "Sample001.genotype.json"

    message <- "extension must be a character string"
    expect_error(readOneArcasHLAGenotypeFile(file_path = paste0(directory, "/", file1),
                                             extension=44), message)
})

test_that("readOneArcasHLAGenotypeFile() must return good result", {

    file1 <- "Sample001.genotype.json"

    result <- readOneArcasHLAGenotypeFile(file_path = paste0(directory, "/", file1),
                                          extension=".genotype.json")

    expected <- data.frame( A_1=c("A*11:303"), A_2=c("A*03:01"),
                            B_1=c("B*27:05:02"), B_2=c("B*27:05:02"),
                            C_1=c("C*01:02:01"), C_2=c("C*01:02:01"),
                            DMA_1=c("DMA*01:01:01"), DMA_2=c("DMA*01:01:01"),
                            DMB_1=c("DMB*01:01:01"), DMB_2=c("DMB*01:01:01"),
                            DOA_1=c("DOA*01:01:02"), DOA_2=c("DOA*01:01:02"),
                            DOB_1=c("DOB*01:01:01"), DOB_2=c("DOB*01:01:01"),
                            DPA1_1=c("DPA1*01:03:01"), DPA1_2=c("DPA1*01:03:01"),
                            DPB1_1=c("DPB1*04:02:01"), DPB1_2=c("DPB1*04:02:01"),
                            DQA1_1=c("DQA1*01:01:01"), DQA1_2=c("DQA1*01:01:01"),
                            DQB1_1=c("DQB1*05:01:01"), DQB1_2=c("DQB1*05:45"),
                            DRA_1=c("DRA*01:01:01"), DRA_2=c("DRA*01:01:01"),
                            DRB1_1=c("DRB1*01:01:01"), DRB1_2=c("DRB1*01:01:01"),
                            DRB5_1=c("DRB5*01:01:01"), DRB5_2=c("DRB5*01:01:01"),
                            E_1 = c("E*01:01:01"), E_2=c("E*01:03:02"),
                            stringsAsFactors =  FALSE)
    rownames(expected) <- "Sample001"


    expect_equal(result, expected)
})
