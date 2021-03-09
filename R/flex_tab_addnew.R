#' flex_tab_addnew
#' takes table (cattable) and ouputs it is a formatted table
#' in word to specified output file "outfile." Output file must be
#' format "filename.docx"

#' @param cattable Table to be converted to flextable
#' @param outfile File to export table to
#' @return flextable
#' @export
flex_tab_addnew <- function(cattable, outfile)
{
  catflex <- flextable::qflextable(cattable)
  catflex<-flextable::autofit(catflex)
  doc <- officer::read_docx()
  doc <- flextable::body_add_flextable(doc, value = catflex)
  print(doc, target = outfile)
}
