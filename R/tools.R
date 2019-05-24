#' @title Extract all HLA typing from all genotypes json files present
#' in a directory
#'
#' @description TODO
#'
#' @param directory_path a \code{character} string corresponding to the
#' name of the directory containing the arcasHLA
#' genotype files.
#'
#' @param prefix a \code{character} string specifying the prefix of the files
#' to load. It is usefull when only a subsection of the files need
#' to be loaded. Default = \code{""}.
#'
#' @param suffix  a \code{character} string specifying the suffix of the files
#' to load. It is usefull when only a subsection of the files need
#' to be loaded. Default = \code{".genotype.json"}.
#'
#' @param recursive a \code{logical} indicating if the listing should
#' recurse into subdirectories. Default = \code{FALSE}.
#'
#' @return a TODO
#'
#' @examples
#'
#' ## Directory containing arcasHLA genotype files
#' directory <- system.file("extdata/arcasHLAFiles", package = "arcasHLATools")
#'
#' ## Reading all genotype files in the directory
#' readAllArcasHLAGenotypefromDir(directory)
#'
#' @author Astrid Deschenes
#' @importFrom dplyr bind_rows
#' @export
readAllArcasHLAGenotypefromDir <- function(directory_path, prefix = "",
                              suffix = ".genotype.json", recursive = FALSE) {
    # Parameters validation
    if (!file.exists(directory_path)) {
        e <- file.info(directory_path)
        if (!e$isdir)
        stop(paste0("No such directory ", directory_path))
    }

    if (!is.character(prefix)) {
        stop("prefix must be a character string")
    }

    if (!is.character(suffix)) {
        stop("suffix must be a character string")
    }

    if (!is.logical(recursive)) {
        stop("recursive must be a logical")
    }

    pattern <- NULL
    if (prefix != "") {
        pattern <- paste0(prefix, "*")
        if (suffix != "") {
            pattern <- paste0(pattern, suffix)
        }
    } else if (suffix != "") {
        pattern <- paste0("*", suffix)
    }

    filesList <- list.files(path = directory_path, pattern = pattern,
                                all.files = FALSE, full.names = TRUE,
                                recursive = recursive,
                                ignore.case = FALSE, include.dirs = FALSE,
                                no.. = FALSE)
   results <- list()
   for(fileInfo in filesList) {
       typing <- readOneArcasHLAGenotypeFile(fileInfo)
       results[[rownames(typing)]] <- typing
   }

   return(bind_rows(results, .id = "Name"))
}

#' @title Extract all HLA typing from one genotypes json file
#'
#' @description TODO
#'
#' @param file_path a \code{character} string corresponding to the
#' name of the arcasHLA genotype file.
#'
#' @param extension a \code{character} string correpondint to the extension
#' of the genotype file. The extension will be removed from the file name which
#' is going to be assigned to the dataset. Default=".genotype.json".
#'
#' @return TODO
#'
#' @examples
#'
#'
#' ## Path to an arcasHLA genotype file
#' jsonFile <- system.file("extdata/arcasHLAFiles", "Sample002.genotype.json",
#'     package = "arcasHLATools")
#'
#' ## Extracting file information
#' readOneArcasHLAGenotypeFile(jsonFile)
#'
#' @author Astrid Deschenes
#' @importFrom jsonlite fromJSON
#' @export
readOneArcasHLAGenotypeFile <- function(file_path,
                                            extension=".genotype.json") {

    # Parameters validation
    if (!file.exists(file_path)) {
        e <- file.info(file_path)
        if (e$isdir)
            stop(paste0("No such file ", file_path))
    }

    if (!is.character(extension)) {
            stop("extension must be a character string")
    }

    typing <- fromJSON(file_path, simplifyVector = FALSE)

    all <- list()
    for (gene in names(typing)) {
        entries <- typing[[gene]]
        all[paste0(gene, "_1")] <- entries[1]
        if (length(entries) > 1) {
            all[paste0(gene, "_2")] <- entries[2]
        } else {
            all[paste0(gene, "_2")] <- entries[1]
        }
    }

    result <- do.call(cbind, all)

    fileName <- basename(file_path)
    fileName <- gsub(extension, replacement = "", x = fileName)

    final <- as.data.frame(result, row.names= c(fileName),
                            stringsAsFactors=FALSE)
    return(final)
}


