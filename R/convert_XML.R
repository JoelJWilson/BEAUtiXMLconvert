#' @title Create BEAST XML file
#'
#' @description Import sequences and place them in an XML file with the required settings
#'
#' @param file Input file with ID and sequences. Accepts .nex, .fasta and .phy files
#' @param template XML file to be used as a template.
#' @param name File name that the resulting XML file will be saved as.
#' @param loc File containing location data.
#' @param chainLength Integer determining the chainlength of the MCMC run. Defaults to length of template file
#' @param storeEvery Integer determining how often to store the results of the MCMC run. Defaults to template number
#'
#' @export
convert_XML <- function(file, template, name, loc = FALSE, chainLength = FALSE, storeEvery = FALSE) {

    # Import sequence data from specified file
    data <- import_sequences(file)

    # Create a template XML file from specified file
    template <- template_XML(template)

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
        XML::addChildren(template[["data"]], sequence)
    }

    # Removing node with dates
    XML::removeChildren(template[["run"]][["state"]][["tree"]], "trait")
    # Paste in sequence IDs with dat 0
    text <- (paste(data[, 1], "=0,\n", sep = ""))
    # remove comma from final ID
    text[length(text)] <- gsub(",", "", text[length(text)])
    # adding back in nodes and attributes that were removed
    XML::newXMLNode("trait", text, attrs = c(id = "dateTrait.t:test", spec = "beast.evolution.tree.TraitSet", traitname = "date"),
        parent = template[["run"]][["state"]][["tree"]], at = 0)
    XML::newXMLNode("taxa", attrs = c(id = "TaxonSet.test", spec = "TaxonSet"), parent = template[["run"]][["state"]][["tree"]][["trait"]])
    XML::newXMLNode("alignment", attrs = c(idref = "test"), parent = template[["run"]][["state"]][["tree"]][["trait"]][["taxa"]])

    # Import location data
    if (loc != FALSE) {

        # Read location data file
        loc_data <- utils::read.delim(loc)

        # Paste location data in template XML file
        loc_text <- paste(loc_data[, 1], "=", loc[, 2], ",\n", sep = "", collapse = "")
        loc_text[length(loc_text)] <- gsub(",", "", loc_text[length(loc_text)])
        template[["run"]][["distribution"]][[2]][[2]][["data"]][["traitSet"]][["text"]] <- loc_text

        # Edit XML attributes
        loc_names <- unique(sort(loc_data[, 2]))
        loc_count <- length(loc_names)
        codeMap_attr <- paste(paste(loc_names, "=", 1:loc_count - 1, ",", collapse = "", sep = ""), "? =", paste(1:loc_count -
            1, collapse = " "))
        XML::xmlAttrs(template[["run"]][["distribution"]][[2]][[2]][["data"]][["userDataType"]])["codeMap"] <- codeMap_attr
        XML::xmlAttrs(template[["run"]][["distribution"]][[2]][[2]][["data"]][["userDataType"]])["states"] <- loc_count
    }
    # change MCMC chainlength
    if (chainLength != FALSE){
      XML::xmlAttrs(template[["run"]])["chanLength"] <- chainLength
    }
    # Change store every
    if (storeEvery != FALSE){
      XML::xmlAttrs(template[["run"]][["state"]])["storeEvery"] <- storeEvery
    }


    ## Replace Names-----------------------------------------------------------------------------------------------

    # Extract the file name from the template
    template_name <- tools::file_path_sans_ext(gsub(".*/", "", template))

    # Extract the alignment name from the template
    alignment_name <- XML::xmlAttrs(template[["data"]])["id"]

    # Extract file name from saved filepath
    file_name <- tools::file_path_sans_ext(gsub(".*/", "", name))

    # Extract the file name from the sequence import
    import_name <- tools::file_path_sans_ext(gsub(".*/", "", file))

    # Convert to character string
    XML_doc <- XML::toString.XMLNode(template)

    # Replace template name with new
    XML_doc <- gsub(template_name, file_name, XML_doc)

    # Replace old aligment name with new
    XML_doc <- gsub(alignment_name, import_name, XML_doc)

    ## Save file-----------------------------------------------------------------------------------------------------
    ## Save the new XML file with a name
    write(XML_doc, file = name)

}
