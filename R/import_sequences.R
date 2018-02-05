#' @title Import Sequences
#'
#' @description Imports a file containing ID and sequences to a dataframe
#'
#' @param file Input file containing sequences. Accepts .nex, .fasta and .phy files
#'
#' @export
import_sequences <- function(file) {
    
    ## Iport .fasta file ##
    import_fasta <- function(file) {
        
        # Read File
        fasta_file <- readLines(file)
        
        # Find read ID using > tag
        names_fasta <- fasta_file[grep(pattern = ">", x = fasta_file)]
        
        # Remove tag from names
        names_fasta <- gsub(">", "", names_fasta)
        
        # Remove '' from names
        names_fasta <- names_fasta <- gsub("'", "", names_fasta)
        
        
        # Create empty list to add sequences to
        fasta_sequence <- list()
        
        # Add sequences to list
        for (n in 1:(length(names_fasta) - 1)) {
            sequence <- fasta_file[(grep(pattern = ">", x = fasta_file)[n] + 1):(grep(pattern = ">", x = fasta_file)[n + 
                1] - 1)]
            sequence <- paste(sequence, collapse = "")
            
            fasta_sequence <- rbind(fasta_sequence, sequence)
        }
        
        sequence <- fasta_file[(grep(pattern = ">", x = fasta_file)[length(names_fasta)] + 1):length(fasta_file)]
        sequence <- paste(sequence, collapse = "")
        fasta_sequence <- rbind(fasta_sequence, sequence)
        
        # Combine seqences and ID to a data frame
        fasta_data <- suppressWarnings(cbind.data.frame(names_fasta, fasta_sequence))
        
        # Return data frame
        fasta_data
    }
    
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
        name <- dataframe[(1:(n.tax)) * 2 + start.line - 2, ]
        
        # Create a list of all the sequences
        sequence <- list()
        for (n in 1:n.tax) {
            sequence_ <- paste(dataframe[(1:((l + 1)/(2 * n.tax))) * 2 * n.tax - (2 * n.tax - start.line + 1 - 2 * 
                n), ], collapse = "")
            sequence <- rbind(sequence, sequence_)
        }
        
        # Combine ID and sequences into a data frame
        nex_data <- suppressWarnings(cbind.data.frame(name, sequence))
        
        # Return data frame
        nex_data
    }
    
    ## Import .phy file ##
    import_phy <- function(file) {
        
        # Read file
        phy_data <- as.data.frame(scan(file, character()))
        
        # Find the number of sequences
        n.tax <- phy_data[1, 1]
        
        # Find how many chracters each sequence is
        n.char <- phy_data[2, 1]
        
        # Create list of ID
        phy_names <- phy_data[((1:n.tax) * 2 + 1), 1]
        
        # Create list of sequences
        phy_sequence <- phy_data[((1:n.tax) * 2 + 2), 1]
        
        # Combine ID and sequences into a dataframe
        phy_data <- suppressWarnings(cbind.data.frame(phy_names, phy_sequence))
        
        # Return dataframe
        phy_data
        
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
    
    data
}
