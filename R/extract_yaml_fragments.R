#' Extract all YAML fragments from a file
#'
#' These function extracts all YAML fragments from a file,
#' returning a list of character vectors containing the extracted
#' fragments.
#'
#' @param file The path to a file to scan; if provided, takes precedence
#' over `text`.
#' @param text A character vector to scan, where every element should
#' represent one line in the file; can be specified instead of `file`.
#' @param delimiterRegEx The regular expression used to locate YAML
#' fragments.
#' @param ignoreOddDelimiters Whether to throw an error (FALSE) or
#' delete the last delimiter (TRUE) if an odd number of delimiters is
#' encountered.
#' @param encoding The encoding to use when calling [readLines()]. Set to
#' NULL to let [readLines()] guess.
#' @param silent Whether to be silent (TRUE) or informative (FALSE).
#'
#' @return A list of character vectors, where each vector corresponds to
#' one YAML fragment in the source file or text.
#' @examples
#' yum::extract_yaml_fragments(text=c(
#' "---",
#' "First: YAML fragment",
#' "  id: firstFragment",
#' "---",
#' "Outside of YAML",
#' "---",
#' "Second: YAML fragment",
#' "  id: secondFragment",
#' "  parentId: firstFragment",
#' "---",
#' "Also outside of YAML"));
#' @export
extract_yaml_fragments <- function(file,
                                   text,
                                   delimiterRegEx = "^---$",
                                   ignoreOddDelimiters = FALSE,
                                   encoding="UTF-8",
                                   silent=TRUE) {

  if (missing(file)) {
    if (missing(text)) {
      stop("Provide either a `file` or a `text` to scan!");
    } else {
      allLines <- text;
    }
  } else {
    allLines <- readLines(file,
                          encoding=encoding,
                          warn=FALSE);
  }

  yamlFragments <- grep(delimiterRegEx,
                        allLines);

  if (length(yamlFragments) == 0) {
    return(NULL);
  } else {
    if (!silent) {
      cat("Identified ", length(yamlFragments),
          " lines matching delimiterRegEx '",
          delimiterRegEx, "': ",
          vecTxt(yamlFragments),
          ".\n",
          sep="");
    }
  }

  if (!is.even(length(yamlFragments))) {
    if (ignoreOddDelimiters) {
      yamlFragments <-
        yamlFragments[-length(yamlFragments)];
    } else {
      stop("Extracted an uneven number of lines with specifications ",
           "(the regular expression for the specification ",
           "delimiter that was specified was '", delimiterRegEx,
           "'). To ignore the last delimiter, specify ",
           "'ignoreOddDelimiters=TRUE'.");
    }
  }

  yamlFragmentIndices <- seq_along(yamlFragments);

  if (length(yamlFragmentIndices) == 2) {
    indexSets <-
      list(seq(yamlFragments[1],
               yamlFragments[2]));
  } else {
    indexSets <-
      mapply(seq,
             yamlFragments[is.odd(yamlFragmentIndices)],
             yamlFragments[is.even(yamlFragmentIndices)],
             SIMPLIFY=FALSE);
  }

  res <-
    lapply(indexSets,
           function(i, x=allLines) {
             return(structure(x[i],
                              class="yamlFragment"));
           });

  class(res) <-
    c("yamlFragments", "list");

  return(res);

}
