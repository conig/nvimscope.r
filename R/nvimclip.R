#' nvimclip
#'
#' @param objname Name of the object to be processed. Default is "mtcars".
#' @param name_fun Function to extract names from the object. Default is names.
#' @return JSON representation of the processed object, copied to the clipboard.
#' @importFrom future.apply future_lapply
#' @example nvimscope.r::nvimclip(mtcars)
#' @export
nvimclip <- function(obj) {
  if (!dir.exists("/tmp/nvim-rmdclip")) {
    dir.create("/tmp/nvim-rmdclip", recursive = TRUE)
  }
  s4 <- isS4(obj)
  # Attempt to convert target object to a list
  contents <- tryCatch(
    {
      if (s4) {
        s4_to_list(obj)
      } else {
        as.list(obj)
      }
    },
    # if not possible, record error in json.
    error = function(e) {
      writeLines("", "/tmp/nvim-rmdclip/error.json")
      stop("Could not find object.")
    }
  )
  s4 <- ifelse(s4, "true", "false")
  # is object is null, record error in json.
  if (is.null(obj)) {
    writeLines("", "/tmp/nvim-rmdclip/error.json")
    stop("Could not find object.")
  }

  use_cores <- FALSE
  # use multicore for large objects
  if (is(obj, "data.frame")) {
    if (nrow(obj) * ncol(obj) > 1000000) {
      use_cores <- max(c(future::availableCores() - 10, 1))
      original_plan <- future::plan()
      future::plan("multicore", workers = use_cores)
      use_cores <- TRUE
    } else {
      future::plan("sequential")
      use_cores <- FALSE
    }
  }
  # For each item in the list, process the contents
  contents_names <- names(contents)
  processed_data <- future.apply::future_lapply(seq_along(contents), function(i) {
    name <- contents_names[i]
    # message(name)
    if (FALSE) {
      debug <- TRUE
    } else {
      debug <- FALSE
    }
    if (is.null(name)) name <- i
    x <- contents[[i]]
    data.frame(name = name, contents = process_contents(x, name, debug = debug))
  }) |>
    data.table::rbindlist(ignore.attr = TRUE)
  # Write processed data to json file
  list(s4 = s4, contents = processed_data) |>
    jsonlite::toJSON(pretty = TRUE, escape_unicode = FALSE) |>
    writeLines("/tmp/nvim-rmdclip/menu.json")
  # Reset plan to original
  if (use_cores) {
    future::plan(original_plan)
  }
}

#' process_contents
#'
#' @param x Object to be processed.
#' @param name Name of the object.
#' @return Processed content as a string.
#' @export

process_contents <- function(x, name, debug = FALSE) {
  if (is(x, "numeric")) {
    return(process_numeric(x, name))
  }
  if (is(x, "character")) {
    return(process_character(x, name))
  }

  if (is(x, "factor")) {
    return(process_character(x, name))
  }

  if (is(x, "logical")) {
    return(process_character(x, name))
  }

  if (is(x, "ggplot")) {
    return(process_ggplot(x, name))
  }

  suppressMessages(process_else(x, name, debug = debug))
}

#' process_numeric
#'
#' @param x Numeric object to be processed.
#' @param name Name of the object.
#' @return Processed numeric content as a string.

process_numeric <- function(x, name) {
  if (length(x) < 2) {
    return(capture.output(print(x)) |> paste(collapse = "\n"))
  }
  if (is.null(name)) name <- ""
  mean_x <- mean(x, na.rm = TRUE) |> round(2)
  median_x <- median(x, na.rm = TRUE) |> round(2)
  sd_x <- sd(x, na.rm = TRUE) |> round(2)
  IQR_x <- IQR(x, na.rm = TRUE) |> round(2)

  range_x <- range(x, na.rm = TRUE) |>
    round(2) |>
    paste(collapse = ", ")
  density_x <- tryCatch(
    {
      if (length(x) > 1000000) {
        x_dens <- sample(x, 1000000)
        msg <- "*Plot based on a random sample\n  of 1,000,000 observations."
      } else {
        x_dens <- x
        msg <- ""
      }

      capture.output(txtplot::txtdensity(na.omit(x_dens), width = 35, height = 15)) |>
        paste(collapse = "\n")
    },
    error = function(e) {
      return("Could not generate density plot.")
    }
  )
  head_x <- head(x, 5) |>
    capture.output() |>
    paste(collapse = "\n")
  tail_x <- tail(x, 5) |>
    capture.output() |>
    paste(collapse = "\n")
  kurtosis_x <- psych::kurtosi(x) |> round(2)
  skewness_x <- psych::skew(x) |> round(2)

  is_missing_x <- sum(is.na(x))
  missing_pc <- (is_missing_x / length(x)) * 100
  missing_pc <- missing_pc |> round(2)

  class_x <- class(x) |> paste(collapse = ", ")

  len_x <- length(x)

  glue::glue("



  Name: `{name}` <{class_x}>

—————————————————————————————————————————————

  head: {head_x}
  tail: {tail_x}

  Observations:  {len_x}
  Unique values: {length(unique(x))}
  Missing:       {is_missing_x} ({missing_pc}%)

  Range:  [{range_x}]
  Mean:   {mean_x} (sd: {sd_x})
  Median: {median_x} (IQR: {IQR_x})

  Kurtosis: {kurtosis_x}
  Skewness: {skewness_x}

—————————————————————————————————————————————

  {density_x}
  {msg}

             ")
}

#' process_character
#'
#' @param x Character object to be processed.
#' @param name Name of the object.
#' @return Processed character content as a string.

process_character <- function(x, name) {
  if (length(x) < 2) {
    return(capture.output(print(x)) |> paste(collapse = "\n"))
  }
  if (is.null(name)) name <- ""

  head_x <- head(x, 5) |>
    capture.output() |>
    paste(collapse = "\n")
  tail_x <- tail(x, 5) |>
    capture.output() |>
    paste(collapse = "\n")

  is_missing_x <- sum(is.na(x))
  missing_pc <- (is_missing_x / length(x)) * 100
  missing_pc <- missing_pc |> round(2)

  len_x <- length(x)

  values <- table(x)
  most_common <- data.frame(values)
  most_common <- most_common[order(-most_common$Freq), ]
  unique_n <- length(na.omit(unique(x)))
  if (unique_n > 1) {
    if (nrow(most_common) > 5) {
      most_common <- most_common[1:5, ]
    }
    most_common_string <- glue::glue("`{most_common[,1]}`: {most_common[,2]}") |>
      paste(collapse = "\n  ")
  } else {
    most_common_string <- x[1]
  }

  class_x <- class(x) |> paste(collapse = ", ")

  glue::glue("



  Name: `{name}` <{class_x}>

—————————————————————————————————————————————

  head: {head_x}
  tail: {tail_x}

  Observations:  {len_x}
  Unique values: {unique_n}
  Missing:       {is_missing_x} ({missing_pc}%)

  Most common values:
  {most_common_string}
             ")
}

process_ggplot <- function(x, name) {
  if (is.null(name)) name <- ""
  class_x <- class(x) |> paste(collapse = ", ")
  mapping <- capture.output(print(x$mapping)) |>
    paste(collapse = "\n  ")
  layers <- capture.output(print(x$layers)) |>
    paste(collapse = "\n  ")


  glue::glue("



  name: `{name}` <{class_x}>

—————————————————————————————————————————————

  {mapping}

—————————————————————————————————————————————

  Layers:

  {layers}
")
}

#' process_else
#'
#' @param x Object to be processed.
#' @param name Name of the object.
#' @return Processed content as a string for objects that are not numeric, character, factor, or logical.

process_else <- function(x, name, debug = FALSE) {
  len_x <- length(x)
  if (debug) browser()

  if (length(x) > 40) {
    x <- x[1:40]
    msg <- "..."
  } else {
    msg <- ""
  }
  print_contents <- capture.output(print(x))

  print_contents <- paste(print_contents, collapse = "\n  ")
  class_x <- class(x) |> paste(collapse = ", ")
  glue::glue("



  name: `{name}` <{class_x}>

—————————————————————————————————————————————

  length: {len_x}

  {print_contents}
  {msg}
")
}
