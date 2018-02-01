#' @title Import template XML file
#' 
#' @description Take a template XML file and replace sequences and dates
#' 
#' @param file template XML file
#' 
#' @export
template_XML <- function(file) {
    template <- XML::xmlInternalTreeParse(file)  #imports xml into internal tree
    template <- XML::xmlRoot(template)  #converts imported tree into a workable node
    
    data_attrs <- XML::xmlAttrs(template[["data"]])  #attributes of the node 'data'
    data <- XML::newXMLNode("data")  #creates new node
    XML::xmlAttrs(data) <- data_attrs  #writes attributes to new node
    XML::removeChildren(template, "data")  #removes node 'data' and all sequences
    XML::addChildren(template, data, at = 0)  #adds new node
    
    XML::removeChildren(template[["run"]][["state"]][["tree"]][["trait"]], "text")  #removes taxon and dates
    
    template
}
