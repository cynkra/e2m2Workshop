code_chunk <- function(output, language = "r") {
  cat(paste0("```", language, "\n"))
  cat(output)
  cat("\n```\n")
}

print_r_code <- function(path) {
  lines <- readLines(path)
  code_chunk(cat(paste(lines, collapse = "\n")))
}
