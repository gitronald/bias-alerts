# Functions for shaping presentable statistics

addline_format <- function(x,...){
  gsub('\\s','\n',x)
}

rounder = function(number, digits, percent = FALSE){
  number = as.numeric(number)
  result = format(round(number, digits), digits = digits, nsmall = digits, scientific = F)
  if(percent){
    result = paste0(as.numeric(result)*100, "%")
  }
  return(result)
}


p_signif = function(p.value, text = F, ns_exact = T, include10 = T){
  # Input P values, Returns significance indicators
  #
  # text : logical, returns P < X text instead of asterisks
  # ns_exact : logical, returns P = X or '' when non-significant
  #
  p.value = as.numeric(p.value)
  if (is.nan(p.value)){
    signif_code = ""
  } else if (p.value < 0.001) {
    signif_code = ifelse(text, "P < 0.001", "***")
  } else if (p.value < 0.01) {
    signif_code = ifelse(text, "P < 0.01", "**")
  } else if(p.value < 0.05){
    signif_code = ifelse(text, "P < 0.05", "*")
  } else if (p.value < 0.1) {
    if(include10){
      signif_code = ifelse(text, "P < 0.10", ".")
    } else {
      signif_code = ''
    }
  } else {
    signif_code = ifelse(ns_exact, paste0("P = ", rounder(p.value, 3)), '')
  }
  
  return(signif_code)
}


# LaTeX -------------------------------------------------------------------

library(xtable)

latex_table = function(data_frame, col_names, align, caption, tab, save_as){
  # Convert an R data.frame to a latex formatted table
  # Requires package `xtable`
  
  # Prepare column names
  colnames(data_frame) = c(col_names)
  
  # Create xtable object
  xt = xtable(data_frame, align=align)
  
  # Capture print
  latex = capture.output(print(xt, include.rownames=FALSE))
  
  
  # Table Edits -------------------------------------------------------------
  
  # Begin Table
  idx = grep('\\\\begin\\{table\\}', latex)
  content = latex[idx]
  replace = "\\begin{table*}[t]"
  latex[idx] = replace
  
  # Swap hlines
  idx = grep("\\\\hline", latex)
  latex[idx[1]] = "\\toprule"
  latex[idx[length(idx)]] = "\\bottomrule"
  latex = gsub("\\\\hline", '\\\\midrule', latex)
  
  # Add to bottom
  idx = grep('\\\\end\\{table\\}', latex)
  latex = c(latex[1:idx-1], 
            paste0("\\caption{",caption,"}"), 
            paste0("\\label{tab:",tab,"}"), 
            "\\end{table*}")
  
  # Write LaTex out
  cat(latex, sep='\n', file = save_as)
}
