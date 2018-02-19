#' @title Import Sequences
#'
#' @description Imports a file containing ID and sequences to a dataframe.
#'
#' @param file The name of the input file containing the sequence alignment.
#'   Works with .nex, .fasta and .phy files.
#'
#' @return A dataframe containing the sequence identifiers as the first column
#'   and the sequence data in the second column.
#'
#'
#' @export
import_sequences <- function(file) {
    
    ## Import .nex file ##
    import_nex <- function(file) {
        
        # Read file and comvert to dataframe
        dataframe <- as.data.frame(scan(file, character(), quiet = TRUE))
        
        # Find where the sequences start
        start.line <- grep("matrix", dataframe[, 1], ignore.case = TRUE) + 1
        
        # Find where the sequences end
        end.line <- grep("end", dataframe[start.line:length(dataframe[, 1]), 1], ignore.case = TRUE)[1] - 3 + start.line
        
        # Length of the sequence data
        l <- end.line - start.line
        
        # Find the number of sequences
        n.tax <- utils::type.convert(gsub(";", "", gsub("ntax=", "", dataframe[grep("ntax=", dataframe[, 1], ignore.case = TRUE), 
            ], ignore.case = TRUE)))
        
        # ID of the sequences
        seq.name <- dataframe[(1:(n.tax)) * 2 + start.line - 2, ]
        
        # Create a list of all the sequences
        seq.text <- list()
        for (n in 1:n.tax) {
            sequence <- paste(dataframe[(1:((l + 1)/(2 * n.tax))) * 2 * n.tax - (2 * n.tax - start.line + 1 - 2 * 
                n), ], collapse = "")
            seq.text <- rbind(seq.text, sequence)
        }
        
        # Combine ID and sequences into a data frame
        nex_data <- suppressWarnings(cbind.data.frame(seq.name, seq.text))
        
        # Return data frame
        nex_data
    }
    
    # import fasta files
    import_fasta <- function(file) {
        phylotools::read.fasta(file, clean_name = FALSE)
    }
    # import phylic files
    import_phy <- function(file) {
        phylotools::read.phylip(file, clean_name = FALSE)
    }
    
    # Find the apporpraite function for the filetype
    if (grepl("*.nex$", file) == TRUE) {
        data <- import_nex(file)
    } else {
        if (grepl("*.fasta$", file) == TRUE) {
            data <- import_fasta(file)
        } else {
            if (grepl("*.phy$", file) == TRUE) {
                data <- import_phy(file)
            } else {
                stop("error")
            }
        }
    }
    
    if (length(unique(nchar(as.vector(data[, 2])))) != 1) 
        stop("Inconsistent sequence lengths")
    
    # remove apostrophes from names
    data[, 1] <- gsub("'", "", data[, 1])
    
    data
}
