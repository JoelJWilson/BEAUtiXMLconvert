#' @title Create BEAST XML file
#'
#' @description Import sequences and place them in an XML file with the required settings
#'
#' @param file Input file with ID and sequences. Accepts .nex, .fasta and .phy files
#' @param template XML file to be used as a template.
#' @param name File name that the resulting XML file will be saved as.
#' @param loc File containing location data.
#'
#' @export
convert_XML <- function(file, template, name, loc = FALSE) {

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
        loc <- utils::read.delim(loc)
        loc_text <- paste(loc[, 1], "=", loc[, 2], ",\n", sep = "", collapse = "")
        loc_text[length(loc_text)] <- gsub(",", "", loc_text[length(loc_text)])
        template[["run"]][["distribution"]][[2]][[2]][["data"]][["traitSet"]][["text"]] <- loc_text
    }

    # Save the new XML file with a name
    XML::saveXML(template, name)

    # Print new XML file
    template

}
