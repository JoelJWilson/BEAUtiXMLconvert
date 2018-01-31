#' @title Import Sequences
#' 
#' @description Imports a file containing ID and sequences
#' 
#' @param file Input file containing sequences. Accepts .nex, .fasta and .phy files
#' 
#' @export
import_sequences <- function(file){
  
  import_fasta <- function(file){
    fasta_file <- readLines(file)
    fasta_file[grep(pattern = ">",x =fasta_file)]->names_fasta
    gsub(">","",names_fasta)->names_fasta
    gsub("'","",names_fasta)->names_fasta
    
    fasta_sequence <- list()
    for (n in 1:(length(names_fasta)-1))
    {fasta_file[(grep(pattern = ">",x =fasta_file)[n]+1):(grep(pattern = ">",x =fasta_file)[n+1]-1)]-> sequence
      paste(sequence,collapse="") -> sequence
      rbind(fasta_sequence,sequence) -> fasta_sequence
    }
    fasta_file[(grep(pattern = ">",x =fasta_file)[length(names_fasta)]+1):length(fasta_file)]-> sequence
    paste(sequence,collapse="") -> sequence
    rbind(fasta_sequence,sequence) -> fasta_sequence
    
    cbind.data.frame(names_fasta,fasta_sequence) -> fasta_data
    
    fasta_data
  }
  
  import_nex <- function(file){
    
    dataframe <- as.data.frame(scan(file,character()))
    
    start.line <- grep("matrix",dataframe[,1],ignore.case = TRUE)+1
    end.line <- grep("end",dataframe[start.line:length(dataframe[,1]),1],ignore.case = TRUE)[1]-3+start.line
    l <- end.line - start.line
    n.tax <- type.convert(gsub(";","",gsub("ntax=","",dataframe[grep("ntax=",dataframe[,1],ignore.case = TRUE),],ignore.case = TRUE)))
    
    name <- dataframe[(1:(n.tax))*2+start.line-2,]
    
    sequence <- list()
    for (n in 1:n.tax){
      sequence_ <- paste(dataframe[(1:((l+1)/(2*n.tax)))*2*n.tax-(2*n.tax-start.line+1-2*n),],collapse="")
      sequence <- rbind(sequence,sequence_)
    }
    
    nex_data <- cbind.data.frame(name,sequence)
    
    nex_data
  }
  
  import_phy <- function(file){
    phy_data <- as.data.frame(scan(file,character()))
    n.tax <- phy_data[1,1]
    n.char <- phy_data[2,1]
    
    phy_names <- phy_data[((1:n.tax)*2+1),1]
    
    phy_sequence <- phy_data[((1:n.tax)*2+2),1]
    
    phy_data <- cbind.data.frame(phy_names,phy_sequence)
    
    phy_data
    
  }
  
  if (grepl("*.nex$",file)==TRUE) {
    import_nex(file) -> data
    } else {
    if (grepl("*.fasta$",file)==TRUE) {
      import_fasta(file) -> data
      } else {
      if (grepl("*.phy$",file)==TRUE) {
      import_phy(file) -> data
        } else {stop("error")}}
    }
    
  data
  }