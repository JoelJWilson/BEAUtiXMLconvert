#' @title Create BEAST XML file
#' 
#' @description Import sequences and place them in an XML file with the required settings
#' 
#' @param file Input file with ID and sequences. Accepts .nex, .fasta and .phy files
#' @param template XML file to be used as a template
#' @param name File name that the resulting XML file will be saved as
#' 
#' @export
convert_XML <- function(file,template,name){
  
  # Import sequence data from specified file
  data <- import_file(file)
  
  # Create a template XML file from specified file
  template <- template_XML(template)
  
  #add new sequences to template
  for (n in 1:(length(data[,2]))) {
    sequence <- newXMLNode("sequence")
    edit_Sequence(sequence,paste("seq_",data[n,1],sep = ""),paste(data[n,1]),"4",paste(data[n,2]))
    addChildren(template[["data"]],sequence)
  }
  
  #
  removeChildren(template[["run"]][["state"]][["tree"]],"trait")
  text <- (paste(data[,1],"=0,\n",sep=""))
  text[length(text)] <- gsub(",","",text[length(text)])
  newXMLNode("trait", text, attrs=c(id="dateTrait.t:test",spec="beast.evolution.tree.TraitSet",traitname="date"),parent=template[["run"]][["state"]][["tree"]],at=0)
  newXMLNode("taxa",attrs=c(id='TaxonSet.test',spec='TaxonSet'),parent=template[["run"]][["state"]][["tree"]][["trait"]])
  newXMLNode("alignment",attrs=c(idref="test"),parent=template[["run"]][["state"]][["tree"]][["trait"]][["taxa"]])
  
  
  saveXML(template, name)
  template
  
}