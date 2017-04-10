#' CloudKicker
#' 
#' Design to store possible usefull function
#' 
#' This function plot ggplot2 boxplot according an order vector. It will also work 

#' @param geneList A vector of gene names in caracter string
#' @param version A caracter string describing the genome version to choose
#' @export
 

myBoxPlots <-function(ListeofListe, newOrder) {

  listeNames <- names(ListeofListe)
  
  foo = lapply(seq_along(ListeofListe), function(i) {
    OneBoxplot <- ListeofListe[[i]]
  
    ##get p-value from wilcox.test
    if(all(is.na(unlist(OneBoxplot)))){
     return(NA)
    }
    else{
		
      p <- wilcox.test(x = as.numeric(OneBoxplot[[1]]), y =as.numeric(OneBoxplot[[2]]))
      
      dfgr1 <- data.frame(value = unlist(OneBoxplot[[1]]), stringsAsFactors = FALSE, type = "1", group = probeNames[i])    
      dfgr2 <- data.frame(value = unlist(OneBoxplot[[2]]), stringsAsFactors = FALSE, type = "2", group = probeNames[i])    
      dfTot <-rbind(dfgr1, dfgr2)
      
      return(dfTot)
     
    }
  })
  bigDF <- do.call(rbind, foo)

  ##################################get the dataFrame in order
  oldOrder <-unique(bigDF$group)
  tmp <- factor(bigDF$group, levels = newOrder, ordered = TRUE)
  bigDF <- bigDF[order(tmp),]
	
  ##################################delete all NA rows
  ind <- apply(bigDF, 1, function(x) all(is.na(x)))
  bigDF <- bigDF[!ind, ]

  #################################create a new group column with the index to plot 
  vec <- (1:length(unique(bigDF$group)))
  vecRep <- rep(LETTERS[vec], each=368)
  bigDF$newGroup <- paste(vecRep, bigDF$group, sep=".")

  
  ################################# display the plot
  ProbesBoxplot <- ggplot2::ggplot(data= bigDF, aes(x = as.character(newGroup), y = value)) + geom_boxplot((aes(fill=type))) + theme(axis.text.x = element_text(angle = 60, hjust = 1))
  ProbesBoxplot
}