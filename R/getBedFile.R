#' CloudKicker
#' 
#' Design to store possible usefull function
#' 
#' This function retrive a dataframe with position of genes transcripts along the genome version you choosed.

#' @param geneList A vector of gene names in caracter string
#' @param version A caracter string describing the genome version to choose
#' @export
 

GetBedFile <- function(genesList, version = "hg19"){
  
  channel <- RMySQL::dbConnect(RMySQL::MySQL(), user="genome", host="genome-mysql.cse.ucsc.edu" )
  
  query <- function(...) DBI::dbGetQuery(channel, ...)
  
  query(paste("USE", version))
  
  line <- paste("select distinct X.geneSymbol,G.chrom,G.txStart,G.txEnd, G.strand from knownGene as G, kgXref as X where X.geneSymbol in (", paste("'",paste(genesList, collapse = "','", sep=""),"'", sep = ""),") and X.kgId=G.name")
  print(paste("select distinct X.geneSymbol,G.chrom,G.txStart,G.txEnd, G.strand from knownGene as G, kgXref as X where X.geneSymbol in (", paste("'",paste(genesList, collapse = "','", sep=""),"'", sep = ""),") and X.kgId=G.name"))

  bedfile <- query(line)
  
  if(length(unique(bedfile$chrom)) == 1) 
  { print("Those genes come from the same chromosome") } else 
  { paste("Les g?nes proviennent de", length(unique(bedfile)), "chomsome") }
  
  allcons <- RMySQL::dbListConnections(RMySQL::MySQL())
  for(con in allcons) RMySQL::dbDisconnect(con)
  
  return(bedfile) 
  
}