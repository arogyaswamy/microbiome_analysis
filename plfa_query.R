plfa.query = function(selection = "Fungi",
                      ref = plfa,
                      plfa.guide = plfa.guide) {
  
  # Make sure all the selected groupings in selection match a grouping in guide.
  if(!all(selection %in% names(guide))){
      stop("Selection not in guide. Check capitalization and update query or guide.")
  }
  print(length(selection))
  # If selection has length < 2, only do a single lookup, and let result be a vector
  if(length(selection) <= 1){
    print(ls())
    result = rowSums(ref[which(names(ref) %in% get(paste0("guide$", selection), envir = parent.env(environment())))])
  }else{
  # If selection has length >= 2, do multiple lookups, and let result be a data.frame  
    result = data.frame()
    for(i in 1:length(selection)){
      result[,i] = rowSums(ref[,which(names(ref) %in% get(paste0("guide$", selection[i])))])
    }
    # Name the colums of the data.frame with the groupings used in guide
    names(result) = selection
  }
  
  return(result)
  
}
