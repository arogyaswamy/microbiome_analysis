## Implement the whole DESeq2 pipeline in a single function. Includes options to
## return results (useful for a binary comparison of a single term), generate an
## MA plot based on the results, or return the plot as an object (useful if you
## want to make a multi-panel plot with ggarrange, for example). If you want to
## return results or a plot, you need to specify either a contrast argument or a
## name argument. If you want to make and/or return a plot, remember to include
## all the arguments required by ggmaplot, except the first (data), which is
## provided by this function. If you want the default plot style, make sure you
## still remember to include an argument for the plot header, since that isn't
## automatically generated (e.g.: main = expression("Uniform" %->% "Gradient").
## This function also allows you to apply a variance stabilizing transformation.
## Since DESeq2 should not use normalized counts as input, if you request a
## normalization, this function will just return that normalized object and
## ignore everything else. You probably don't want to run this just to make
## plots since it will take several minutes, but it could be useful if you have
## multiple models to run, and you know what comparisons you need for each one.
KA.DESEq.pipeline = function(test.formula, feature.table, metadata.obj, 
                             limiting.column = "SampleName", vst.transform = FALSE,
                             threshold = 10, boost = 1,
                             return.DESeq = TRUE, generate.plot = FALSE, default.plot = TRUE,
                             return.plot = FALSE, return.results = FALSE, ...){
  require(DESeq2); require(vegan); require(ggpubr)
  
  if((return.DESeq + return.plot + return.results) > 1){
    stop("Error: Function can only return one object/value.")
  }else{}
  
  ## Remove extremely low-abundance features, since we probably don't care about them anyway
  feature.table = feature.table[,which(colSums(feature.table) >= threshold)]
  
  ## DESeq doesn't like it when every feature has zero (i.e., no gene/taxon is
  ## ubiquitous), so we'll add 1 to every count. This shouldn't make a
  ## difference to our results, since we already removed any feature observed
  ## fewer than 10 (or threshold) times
  feature.table = feature.table + 1
  
  ## Import DESeq dataset from the feature table. Ensures that the analysis only
  ## includes samples that are in the metadata object.
  temp.deseq.obj = DESeqDataSetFromMatrix(
    countData = as.data.frame(t(
      feature.table[which(row.names(feature.table) %in% getElement(metadata.obj, limiting.column)), ])
      ),
    colData = full_data,
    design = test.formula
  )
  
  if(vst.transform){
    temp.deseq.obj = estimateSizeFactors(temp.deseq.obj, type = "poscounts")
    temp.deseq.obj = varianceStabilizingTransformation(temp.deseq.obj)
    if(any(return.plot, generate.plot, return.results)){
      warning("DESeq2 should not be used on transformed reads. Enjoy your VST-normalized data, though.")
    }
    return(temp.deseq.obj)
  }else{}
  

  temp.deseq.obj = DESeq(temp.deseq.obj)
  if(return.DESeq & !generate.plot){
    return(temp.deseq.obj)
  }else if(generate.plot | return.plot | return.results){
    temp.results = results(temp.deseq.obj, ...)
    ## Make a plot if needed, printing and/or returning it as requested
    if((return.plot | generate.plot) & default.plot){
      plt = ggmaplot(data = temp.results, size = 2, genenames = temp.results@rownames,
                     label.rectangle = TRUE, font.label = c("bold", 11), shape = 21,
                     fdr = 0.01, fc = 2, top = 10, ggtheme = theme_pubr(), ...)
      if(generate.plot){
        print(plt)
      }else{
        ## The only way to get here is if return.plot was TRUE
        return(plt)
      }
    }else if(return.plot | generate.plot){
      plt = ggmaplot(data = temp.results, ...)
      if(generate.plot){
        print(plt)
      }else{
        return(plt)
      }
    }
    if(return.plot){
      return(plt)
    }else{
      return(temp.results)
    }
  }
  if(return.DESeq){
    return(temp.deseq.obj)
  }
  ## Congratulations, you've reached the end of the maze, and you never asked
  ## for anything to be returned. Your reward is getting the DESeq object
  ## returned invisibly, in case you have a setup where you want to print a
  ## plot, but maybe save your analysis. Or if you forgot, and you want to
  ## retrieve it with x <- .Last.value
  invisible(temp.deseq.obj)
}

my.ma.plotter = function(deseq.obj, tax.table, tx.rank, variable, numer.name, denom.name, ...){
  require(DESeq2); require(ggpubr)
  tmp.results = results(deseq.obj, contrast = c(variable, numer.name, denom.name))
  combined.df = merge(as.data.frame(tmp.results), tax.table, by = 0)
  plt = ggmaplot(data = combined.df, genenames = getElement(combined.df, tx.rank), 
                 main = paste0(numer.name, " vs. ", denom.name, " (", tx.rank, ")"), 
                 label.rectangle = TRUE, font.label = c("bold", 12), ...)
  print(plt)
  invisible(plt)
}