#' @title Import template XML file
#' 
#' @description Take a template XML file and replace sequences and dates
#' 
#' @param file template XML file
#' 
#' @export
template_XML <- function(file) {
    template <- xmlInternalTreeParse(file)  #imports xml into internal tree
    template <- xmlRoot(template)  #converts imported tree into a workable node
    
    data_attrs <- xmlAttrs(template[["data"]])  #attributes of the node 'data'
    data <- newXMLNode("data")  #creates new node
    xmlAttrs(data) <- data_attrs  #writes attributes to new node
    removeChildren(template, "data")  #removes node 'data' and all sequences
    addChildren(template, data, at = 0)  #adds new node
    
    removeChildren(template[["run"]][["state"]][["tree"]][["trait"]], "text")  #removes taxon and dates
    
    template
}
