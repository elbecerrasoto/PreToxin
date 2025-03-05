re2re <- function(pfam_re, domains, letters) {
  # It only translates PFAMS ids to single letter code
  code_re <- pfam_re
  for (i in seq_along(domains))
  {
    code_re <- str_replace_all(code_re, domains[[i]], letters[[i]])
  }
  code_re
}
