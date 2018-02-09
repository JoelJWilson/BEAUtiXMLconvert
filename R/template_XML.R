#' @title Create template XML file
#'
#' @description Take a template XML file and remove alignment data
#'
#' @param template Name of template XML file
#' @param file Name of file to write output template XML file to
#'
#' @export
template_XML <- function(template, file) {
    # Create a template XML file from specified file
    if (grepl("*.xml$", template, ignore.case = TRUE) == TRUE) {
        XML_template <- XML::xmlInternalTreeParse(file)  #imports xml into internal tree
        XML_template <- XML::xmlRoot(XML_template)  #converts imported tree into a workable node
    } else {
        stop("Template is not an XML file")
    }
    
    
    
    data_attrs <- XML::xmlAttrs(XML_template[["data"]])  #attributes of the node 'data'
    data_node <- XML::newXMLNode("data")  #creates new node
    XML::xmlAttrs(data_node) <- data_attrs  #writes attributes to new node
    XML::removeChildren(XML_template, "data")  #removes node 'data' and all sequences
    XML::addChildren(XML_template, data_node, at = 0)  #adds new node
    
    # Paste dates in template XML file
    
    if (mode(XML_template[["run"]][["state"]][["tree"]][["trait"]][["text"]]) != "NULL") {
        XML_template[["run"]][["state"]][["tree"]][["trait"]][["text"]] <- ""
    }
    
    # Import location data
    if (mode(XML_template[["run"]][["distribution"]][[2]][[2]][["data"]][["traitSet"]][["text"]]) != "NULL") {
        XML_template[["run"]][["distribution"]][[2]][[2]][["data"]][["traitSet"]][["text"]] <- ""
    }
    
    
    
    
    ## Replace Names-----------------------------------------------------------------------------------------------
    ## Extract file name from saved filepath
    file_name <- tools::file_path_sans_ext(gsub(".*/", "", file))
    
    # Convert to character string
    XML_doc <- XML::toString.XMLNode(XML_template)
    
    # Change .trees output name
    XML_doc <- gsub("fileName=(.{,30})\\.trees", paste("fileName=\\\"", file_name, ".trees", sep = ""), XML_doc)
    
    # Change .log output name
    XML_doc <- gsub("fileName=(.{,30})\\.log", paste("fileName=\\\"", file_name, ".log", sep = ""), XML_doc)
    
    ## Save
    ## file-----------------------------------------------------------------------------------------------------
    write(XML_doc, file = file)
}
