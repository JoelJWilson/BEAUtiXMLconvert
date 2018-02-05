#' @title Create BEAST XML file
#'
#' @description Import sequences and place them in an XML file with the required settings
#'
#' @param file Input file with ID and sequences. Accepts .nex, .fasta and .phy files
#' @param template XML file to be used as a template.
#' @param name File name that the resulting XML file will be saved as.
#' @param loc File containing location data.
#' @param chainLength Integer determining the chainlength of the MCMC run. Defaults to template file specifications.
#' @param storeEvery Integer determining how often to store the results of the MCMC run. Defaults to template file specifications.
#'
#' @export
convert_XML <- function(file, template, name, loc = FALSE, chainLength = FALSE, storeEvery = FALSE) {

    # Import sequence data from specified file
    data <- import_sequences(file)

    # Create a template XML file from specified file
    XML_template <- XML::xmlInternalTreeParse(template)  #imports xml into internal tree
    XML_template <- XML::xmlRoot(XML_template)  #converts imported tree into a workable node

    data_attrs <- XML::xmlAttrs(XML_template[["data"]])  #attributes of the node 'data'
    data <- XML::newXMLNode("data")  #creates new node
    XML::xmlAttrs(data) <- data_attrs  #writes attributes to new node
    XML::removeChildren(XML_template, "data")  #removes node 'data' and all sequences
    XML::addChildren(XML_template, data, at = 0)  #adds new node

    # Function to add sequences to an XML node
    edit_Sequence <- function(sequence, ...) {

        args <- list(...)
        id <- args[1]
        taxon <- args[2]
        totalcount <- args[3]
        value <- args[4]

        XML::xmlAttrs(sequence)["id"] <- id
        XML::xmlAttrs(sequence)["taxon"] <- taxon
        XML::xmlAttrs(sequence)["totalcount"] <- totalcount
        XML::xmlAttrs(sequence)["value"] <- value
    }

    # add new sequences to template
    for (n in 1:(length(data[, 2]))) {
        sequence <- XML::newXMLNode("sequence")
        edit_Sequence(sequence, paste("seq_", data[n, 1], sep = ""), paste(data[n, 1]), "4", paste(data[n, 2]))
        XML::addChildren(XML_template[["data"]], sequence)
    }

    # Paste dates in template XML file
    dates_text <- paste(data[, 1], "=", "0", ",\n", sep = "", collapse = "")
    dates_text[length(dates_text)] <- gsub(",", "", dates_text[length(dates_text)])
    XML_template[["run"]][["stae"]][["tree"]][["trait"]][["text"]] <- dates_text

    # Import location data
    if (loc != FALSE) {

        # Read location data file
        loc_data <- utils::read.delim(loc)

        # Paste location data in template XML file
        loc_text <- paste(loc_data[, 1], "=", loc[, 2], ",\n", sep = "", collapse = "")
        loc_text[length(loc_text)] <- gsub(",", "", loc_text[length(loc_text)])
        XML_template[["run"]][["distribution"]][[2]][[2]][["data"]][["traitSet"]][["text"]] <- loc_text

        # Edit XML attributes
        loc_names <- unique(sort(loc_data[, 2]))
        loc_count <- length(loc_names)
        codeMap_attr <- paste(paste(loc_names, "=", 1:loc_count - 1, ",", collapse = "", sep = ""), "? =", paste(1:loc_count -
            1, collapse = " "))
        XML::xmlAttrs(XML_template[["run"]][["distribution"]][[2]][[2]][["data"]][["userDataType"]])["codeMap"] <- codeMap_attr
        XML::xmlAttrs(XML_template[["run"]][["distribution"]][[2]][[2]][["data"]][["userDataType"]])["states"] <- loc_count
    }
    # change MCMC chainlength
    if (chainLength != FALSE) {
        XML::xmlAttrs(XML_template[["run"]])["chanLength"] <- chainLength
    }
    # Change store every
    if (storeEvery != FALSE) {
        XML::xmlAttrs(XML_template[["run"]][["state"]])["storeEvery"] <- storeEvery
    }


    ## Replace Names-----------------------------------------------------------------------------------------------

    # Extract the file name from the template
    template_name <- tools::file_path_sans_ext(gsub(".*/", "", template))

    # Extract the alignment name from the template
    alignment_name <- XML::xmlAttrs(XML_template[["data"]])["id"]

    # Extract file name from saved filepath
    file_name <- tools::file_path_sans_ext(gsub(".*/", "", name))

    # Extract the file name from the sequence import
    import_name <- tools::file_path_sans_ext(gsub(".*/", "", file))

    # Convert to character string
    XML_doc <- XML::toString.XMLNode(XML_template)

    # Replace template name with new
    XML_doc <- gsub(template_name, file_name, XML_doc)

    # Replace old aligment name with new
    XML_doc <- gsub(alignment_name, import_name, XML_doc)

    ## Save file-----------------------------------------------------------------------------------------------------
    ## Save the new XML file with a name
    write(XML_doc, file = name)

}
