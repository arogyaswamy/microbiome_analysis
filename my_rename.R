rename = function(original, new_name = "temp00000000", remove_original = TRUE){
  if(new_name == "temp00000000"){
    new_name = paste0("temp", paste0(floor(rnorm(2, 50000, 5000)), collapse = ""))
  }
  if(exists(new_name, where = 1)){
    return(paste0("Error: An object with the name \"", new_name, "\" already exists."))
  }
  
  assign(new_name, original, 1)
  if(exists(new_name, where = 1)){
    print("Original object was successfully copied to the global environment with the name:")
    print("\"", new_name, ".\"")
  }
  if(remove_original){
    rm(original, pos = 1)
    print("And the original object should be removed.")
  }else{
    print("But the original object still exists.")
  }
}