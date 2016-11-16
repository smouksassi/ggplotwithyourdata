sourceable <- function() {
  structure(
    c(),
    class = "sourceable",
    deps = list()
  )
}
`+` <- function(e1, e2) {
  if (methods::is(e1, "sourceable")) {
    class(e1) <- NULL
    if (length(e1) == 0) {
      structure(
        append(e1, substitute(e2)),
        class = "sourceable",
        deps = attr(e1, "deps")
      )
    } else {
      structure(
        append(e1, substitute(+ e2)),
        class = "sourceable",
        deps = attr(e1, "deps")
      )
    }
  } else {
    base::`+`(e1, e2)
  }
}
add_source_dep <- function(x, deps) {
  stopifnot(methods::is(x, "sourceable"))
  if (length(deps) == 0) {
    return(x)
  }
  
  
  attr(x, "deps") <- append(attr(x, "deps"), setNames(lapply(deps, get, envir = parent.frame()), deps))
  x
}




get_source <- function(x) {
  stopifnot(methods::is(x, "sourceable"))
  paste(unlist(lapply(x, deparse)), collapse = " ")
}
decorate_source <- function(x) {
  stopifnot(methods::is(x, "sourceable"))
  
  result <- get_source(x)
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
print.sourceable <- function(x, ...) {
  cat(decorate_source(x))
}
as.character.sourceable <- function(x, ...) {
  get_source(x)
}
run_source <- function(x) {
  stopifnot(methods::is(x, "sourceable"))
  
  env_list <- attr(x, "deps")
  if (exists("input", envir = parent.frame())) {
    env_list$input <- reactiveValuesToList(get("input", envir = parent.frame()))
  } 
  
  
  eval(parse(text = get_source(x)), envir = list2env(env_list))
}
