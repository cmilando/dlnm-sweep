expand_arglist <- function(arglist) {
  
  # Empty list to hold the output.
  out_l <- list()
  
  # --- Helper functions ---
  # Define a function to create a grid of all combinations of two inputs.
  expand_grid2 <- function(a, b, b_name) {
    if (nrow(a) == 0) {
      tmp <- NA
      o <- data.frame(tmp)
      o[b_name] <- list(b)
    } else {
      o <- expand_grid(a, b)
      colnames(o)[ncol(o)] <- b_name
    }
    return(o)
  }
  
  # Define a function to bind two inputs into columns of a data frame.
  cbind2 <- function(a, b, b_name) {
    if (nrow(a) == 0) {
      tmp <- NA
      o <- data.frame(tmp)
    } 
    o[b_name] <- list(b)
    return(o)
  }
  
  # Define a function to unlist the 'knots' elements of a list.
  unlist_knots <- function(lst) {
    for (i in 1:length(lst)) {
      for (name_i in names(lst[[i]])) {
        if (name_i == "knots") {
          stopifnot(length(lst[[i]]["knots"]) == 1)
          lst[[i]]["knots"] <- lst[[i]]["knots"][[1]]
        }
      }
    }
    return(lst)
  }
  
  # --- Main loop ---
  # Loop over each set of arguments in 'arglist'.
  for (arg_set_i in arglist) {
    
    # Get the names of the arguments in the current set.
    arg_i_names <- names(arg_set_i)
    
    # Create an empty data frame to hold the output for the current set.
    out_i <- data.frame()
    
    # Loop over each argument in the current set.
    for (arg_i in arg_i_names) {
      
      # Get the value of the current argument.
      y <- arg_set_i[[arg_i]]
      
      if (arg_i == "knots") {
        
        # If the argument is a list, expand it into a grid.
        # If it is not a list, bind it into a column of a data frame.
        if (typeof(y) == "list") {
          out_i <- expand_grid2(out_i, y, arg_i)
        } else {
          out_i <- cbind2(out_i, y, arg_i)
        }
        
      } else {
        
        # If the argument is not known, issue a warning.
        known_vars <- c("df", "intercept", "degree", "fun")
        if (!(arg_i %in% known_vars)) {
          warning(sprintf(
            'Argument "%s" is not in [%s], behavior unknown.',
            arg_i, paste(known_vars, collapse = ", ")
          ))
        }
        
        # Assume the argument is a vector, and expand it into a grid.
        y <- arg_set_i[[arg_i]]
        stopifnot(typeof(y) %in% c("double", "character"))
        out_i <- expand_grid2(out_i, y, arg_i)
        
      }
    }
    
    # Convert each row of the output data frame into a named list.
    out_l_i <- lapply(1:nrow(out_i), function(i) sapply(out_i[i, ], list))
    
    # Unlist the 'knots' elements of the output list.
    out_l_i_clean <- unlist_knots(out_l_i)
    
    # Append the output list for the current set to the overall output list.
    out_l <- append(out_l, out_l_i_clean)
  }
  
  # return and remove any named $tmp
  lapply(out_l, function(ll) ll[! (names(ll) %in% "tmp")])
}
