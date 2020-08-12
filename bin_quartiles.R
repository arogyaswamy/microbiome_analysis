bin.quartiles = prfunction(df, skip = "SkipNone", label.names = FALSE){ 
  # Function returns the binned values according to quartiles. Pass only the
  # subset of columns you want binned, or use skip to pass a character vector of
  # names of columns to leave as is.
  
  # Get the breaks for each of the columns. The fourth entry is skipped because
  # it is the mean, rather than the median
  if(label.names == FALSE){
    label.names = c("Low", "Moderately Low", "Moderately High", "High")
  }
  if(is.vector(df)){
    return(cut(df, breaks = summary(df)[-4], labels = label.names, include.lowest = TRUE))
  }else if(is.data.frame(df)){
    df.names = names(df)
    df.names = df.names[which(!df.names %in% skip)]
    
    temp = NULL
    
    for(i in 1:length(df.names)){
      temp <<- cbind(temp,
                   cut(
                     get(paste0("df$", df.names[i])),
                     breaks = summary(get(paste0("df$", df.names[i])))[-4],
                     labels = label.names,
                     include.lowest = TRUE
                     )
                   )
    }
    return(temp)
  }
  
}