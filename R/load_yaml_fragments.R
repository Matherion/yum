#' Load all YAML fragments from a file
#'
#' These function extracts all YAML fragments from a file and then
#' calls [yaml::yaml.load()] to parse them. It then returns a list
#' of the parsed fragments.
#'
#' @inheritParams extract_yaml_fragments
#' @param yamlFragments A character vector of class `yamlFragment` where
#' every element corresponds to one line of the YAML fragments, or a list
#' of multiple such character vectors (of class `yamlFragments`). Specify
#' either `yamlFragments` (which, if specified, takes precedence over `file`
#' and `text`), `file`, or `text` (`file` takes precedence over `text`).
#' @param select A vector of regular expressions specifying object names
#' to retain. The default (`.*`) matches everything, so by default, all
#' objects are retained.
#'
#' @return A list of objects, where each object corresponds to one
#' YAML fragment from the source file or text.
#' @examples
#' yum::load_yaml_fragments(text=c(
#' "---",
#' "-",
#' "  id: firstFragment",
#' "---",
#' "Outside of YAML",
#' "---",
#' "-",
#' "  id: secondFragment",
#' "  parentId: firstFragment",
#' "---",
#' "Also outside of YAML"));
#'
#' @export
load_yaml_fragments <- function(file,
                                text,
                                yamlFragments=NULL,
                                select=".*",
                                delimiterRegEx = "^---$",
                                ignoreOddDelimiters = FALSE,
                                encoding="UTF-8",
                                silent=TRUE) {

  if (!requireNamespace("yaml", quietly = TRUE)) {
    stop("To parse YAML content, the \"yaml\" package is required. ",
         "Please install it using `install.packages('yaml');`.",
         call. = FALSE);
  }

  if (!is.null(yamlFragments)) {
    if (("yamlFragment" %in% class(yamlFragments)) ||
        ("yamlFragments" %in% class(yamlFragments))) {
      yamlLineSets <- yamlFragments;
    } else if ("yamlFragmentsFromDir" %in% class(yamlFragments)) {
      return(load_yaml_list(x=yamlFragments,
                            recursive = TRUE,
                            select=select,
                            delimiterRegEx = delimiterRegEx,
                            ignoreOddDelimiters = ignoreOddDelimiters,
                            encoding=encoding,
                            silent=silent));
    } else {
      stop("If passing a list of YAML line sets as produced by ",
           "the yum `extract_yaml_*` functions, they must have the ",
           "class `yamlFragment`, `yamlFragments` or `yamlFragmentsFromDir`.");
    }
  } else if ((!missing(file)) && (!is.null(file))) {
    yamlLineSets <- extract_yaml_fragments(file=file,
                                           delimiterRegEx=delimiterRegEx,
                                           ignoreOddDelimiters=ignoreOddDelimiters,
                                           silent=TRUE);
  } else if (!missing(text)) {
    yamlLineSets <- extract_yaml_fragments(text=text,
                                           delimiterRegEx=delimiterRegEx,
                                           ignoreOddDelimiters=ignoreOddDelimiters,
                                           silent=TRUE);
  } else {
    stop("Provide either a `file` or a `text` to scan!");
  }

  if ("yamlFragment" %in% class(yamlLineSets)) {
    rawSpecs <- yaml::yaml.load(yamlLineSets);
  } else {
    rawSpecs <- lapply(yamlLineSets,
                       yaml::yaml.load);
  }

  if (!silent) {
    if (!missing(file)) {
      cat("Loaded ",
          length(rawSpecs),
          " YAML fragments from file '",
          file,
          "'.\n", sep="");
    } else if (!missing(text) && !silent) {
      cat("Loaded ",
          length(rawSpecs),
          " YAML fragments from the supplied `text` argument.\n",
          sep="");
    }
  }

  specNames <-
    lapply(rawSpecs,
           names);

  if (length(select) > 0) {
    if (!silent) {
      cat("Applying the selection ",
          ifelse(length(select)==1,
                 "criterion",
                 "criteria"),
          " specified in the `select` argument (specifically,",
          vecTxtQ(select),
          ").\n", sep="");
    }
    combinedSelect <-
      paste0(select, collapse="|");
    rawSpecs <-
      lapply(rawSpecs,
             function(spec) {
               selectedElements <-
                 union(which(is.null(names(spec))),
                       grep(combinedSelect,
                            names(spec),
                            perl=TRUE));
               return(spec[selectedElements]);
             });

    if (!silent) {
      cat("Selected ",
          sum(unlist(lapply(rawSpecs, length))),
          " YAML fragments.\n",
          sep="");
    }
  } else {
    if (!silent) {
      cat("No selection criteria were specified, so ",
          "returning all objects.");
    }
  }

  rawSpecs <-
    rawSpecs[unlist(lapply(rawSpecs,
                           length)) > 0];

  class(rawSpecs) <-
    c("yumFromFile", "list");

  return(rawSpecs);

}
