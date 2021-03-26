clean.dada2.taxa.table = function(tax.tbl){
  ## This function scans each row of the taxa table, and when it encounters an
  ## NA, replaces the rest of the row with "Unassigned" followed by the lowest
  ## assigned rank. For example, if there was a cat (Felidae) of unknown genus,
  ## the genus and species would both say "Unassigned Family Felidae". For the
  ## top-level taxonomic rank, an extra step is added, so that those features
  ## are just called "Unassigned"
  
  ## In case input is a matrix, convert to a data.frame
  if(!is.data.frame(tax.tbl)){ tax.tbl = as.data.frame(tax.tbl) }
  tax.levels = ncol(tax.tbl)
  
  ## Since you can't easily assign elements to new levels in factors, we convert
  ## all columns to characters. But sapply removes row names and returns a
  ## matrix, so we need to capture those, turn the whole thing back into a
  ## data.frame, and also re-add the row names. Ugh.
  
  tax.tbl = as.data.frame(sapply(tax.tbl, FUN = as.character), row.names = row.names(tax.tbl))
  
  for(i in 1:nrow(tax.tbl)){
    if(!is.na(tax.tbl[i,tax.levels])){
      tax.tbl[i,j] = paste0(tax.tbl[i,(j-1)], " ", tax.tbl[i,j])
    }else{
      for(j in 2:tax.levels){
        if(is.na(tax.tbl[i,j])){
          tax.tbl[i,j:tax.levels] = rep(
            paste0("Unassigned ", 
                   names(tax.tbl)[j-1],
                   " ",
                   tax.tbl[i,j-1]),
            length(j:tax.levels))
        }
      }
    }
  }
  ## Special step to allow for features that are unassigned even at the top
  ## level. Although they aren't useful directly, they can be important when
  ## estimating relative abundances, since they represent actual sequences
  ## picked up by primers and could simply be uncharacterized in the database.
  tax.tbl[,1][which(is.na(tax.tbl[,1]))] = "Unassigned"
  
  return(tax.tbl)
}
