# Define a ggplot2 object as "sourceable", which means that it knows
# how to keep track of its source code (parameter must be a ggplot2 object)
sourceable <- function(x) {
  stopifnot(methods::is(x, "ggplot"))
  attr(x, "sourcecode") <- deparse(substitute(x))
  attr(x, "deps") <- list()
  class(x) <- c("sourceable", class(x))
  x
}


# Overwrite the plus operator so that if a "sourceable" object is used,
# the source code is kept
`+` <- function(e1, e2) {
  if (methods::is(e1, "sourceable")) {
    res <- base::`+`(e1, e2)
    attr(res, "sourcecode") <- paste(attr(res, "sourcecode"),
                                     deparse(substitute(+e2)))
    res
  }
  # If we're not dealing with a "sourceable" object, carry on with regular +
  else {
    base::`+`(e1, e2)
  }
}




bb <- 40
p <- sourceable(ggplot(mtcars, aes(mpg,wt)))
p <- p + geom_point()
p <- p + geom_line()
p <- p + theme_bw(bb)
p <- add_source_dep(p, c("bb"))


add_source_dep <- function(x, deps) {
  stopifnot(methods::is(x, "sourceable"))
  if (length(deps) == 0) {
    return(x)
  }
  
  parentFrame <- parent.frame(1)
  
  
  attr(x, "deps") <- append(attr(x, "deps"), setNames(
    lapply(deps, function(dep) {
      eval(parse(text = dep), envir = parentFrame)
    }), make.names(deps)))
  x
}


get_sourcecode <- function(x) {
  stopifnot(methods::is(x, "sourceable"))
  
  result <- attr(x, "sourcecode")
  result <- gsub("\\+[[:space:]]*", "\\+\n  ", result)
  
  source_vars <- attr(x, "deps")
  for (source_var in names(source_vars)) {
    result <- paste0(source_var, " <- ", capture.output(dput(source_vars[[source_var]])), "\n", result)
  }
  
  if (exists("input", envir = parent.frame())) {
    input_vars <- stringr::str_extract_all(result, "input\\$[[:alnum:]]*")[[1]]
    input_vars <- sub("input\\$", "", input_vars)
    input_list <- reactiveValuesToList(get("input", envir = parent.frame()))
    if (length(input_vars) > 0) {
      input_vars_source <- paste0("input <- list()\n")
      for (input_var in input_vars) {
        input_vars_source <- paste0(input_vars_source, "input$", input_var, " <- ",
                                    capture.output(dput(get(input_var, envir = as.environment(input_list)))), "\n") 
      }
      result <- paste0(input_vars_source, "\n", result)
    }
  }
  
  result
}


