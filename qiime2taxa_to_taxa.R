fix_qiime2_taxa = function(taxonomy){
  row.names(taxonomy) = taxonomy$Feature.ID
  taxonomy$Feature.ID = taxonomy$Confidence = NULL
  taxonomy$Species = taxonomy$Genus = taxonomy$Family = taxonomy$Order = 
    taxonomy$Class = taxonomy$Phylum = taxonomy$Kingdom = NA
  taxonomy = as.data.frame(t(taxonomy))
  for(i in 1:dim(taxonomy)[2]){
    taxonomy[2:(length(strsplit(taxonomy[1,i], "; ")[[1]]) + 1),i] = strsplit(taxonomy[1,i], "; ")[[1]]
  }
  taxonomy = as.data.frame(t(taxonomy))
  taxonomy$Taxon = NULL
  for(i in 1:dim(taxonomy)[2]){
    taxonomy[,i] = gsub("^.__", "", taxonomy[,i])
    taxonomy[,i][which(taxonomy[,i] == "")] = NA
  }
  return(clean.dada2.taxa.table(taxonomy))
}