# MISC ##########################################################################################################################################
 # install.packages('bit64'); library(bit64)
 # install.packages("moments")

# resids.img = png::readPNG('/Users/robert/Desktop/Screen Shot 2023-12-27 at 12.10.27 AM.png')
# exp.img = png::readPNG('/Users/robert/Desktop/Screen Shot 2024-01-02 at 7.46.02 PM.png')
# stats_1.img = png::readPNG('/Users/robert/Desktop/Screen Shot 2024-01-02 at 7.45.56 PM.png')

# plot(0, cex = 0); grid::grid.raster(resids.img)
# plot(0, cex = 0); grid::grid.raster(exp.img)
# plot(0, cex = 0); grid::grid.raster(stats_1.img)

# rm(resids.img, exp.img, stats_1.img)
# FUNCTIONS ####################################################################################################################################

varSummary <- function(in.dtfr, makePlot = FALSE){                                                       ## RETURNS CONSOLE PRINT
     for(i in 1:ncol(in.dtfr)){
          col_acc.vec = in.dtfr[ ,i]
          var_name.s = names(in.dtfr)[i]
          print(var_name.s, quote = FALSE)   
          print(sum(is.na(col_acc.vec)), quote = FALSE)   
          data_type.s = typeof(col_acc.vec)
          
          if(data_type.s == 'character'){
               print(unique(col_acc.vec), quote = FALSE)
          }
          else if(data_type.s == 'integer' || data_type.s == 'double'){
               print('numeric', quote = FALSE)
               print(mean(col_acc.vec, na.rm = TRUE), quote = FALSE)
               if(makePlot){
                    plot(col_acc.vec, cex = .1, main = var_name.s)
               }
          }
          else if(data_type.s == 'logical'){
               print(unique(col_acc.vec), quote = FALSE)
          }
          else if(data_type.s == 'NULL'){
               print('NULL', quote = FALSE)
          }
          else{
               print(unique(col_acc.vec), quote = FALSE)
          }
          print('', quote = FALSE); print('*', quote = FALSE); print('', quote = FALSE)
     }
}


abbvChecker <- function(long_form.vec, short_form.vec){                                                  ## RETURNS BOOLEAN
     lgfm_values.vec = unique(long_form.vec)
     shfm_values.vec = unique(short_form.vec)
     long_len = length(long_form.vec)
     
     if(length(lgfm_values.vec) != length(shfm_values.vec) || long_len != length(short_form.vec)){
          return(FALSE)
     }
     
     concat_data_in = paste(long_form.vec, short_form.vec, sep = '')
     concat_uniques = paste(lgfm_values.vec, shfm_values.vec, sep = '')
     out.bool = TRUE
     
     for(i in 1:long_len){
          out.bool = concat_data_in[i] %in% concat_uniques
          if(!out.bool){
               return(FALSE)
          }
     }
     return(out.bool)
}


valCounter <- function(in.vec){                                                                          ## RETURNS DATA FRAME
     unq.vec    = unique(in.vec)
     num_unq    = length(unq.vec)
     counts.vec = vector(length = num_unq)
     
     for(i in 1:length(in.vec)){
          for(q in 1:num_unq){
               if(in.vec[i] == unq.vec[q]){
                    counts.vec[q] = counts.vec[q] + 1
               }
          }
     }
     out.dtfr = as.data.frame(unq.vec)
     out.dtfr = cbind(out.dtfr, counts.vec)
     
     return(out.dtfr)
}


colIndex <- function(dtfr, col_name.s){                                                                  ## RETURNS INTEGER
     names.vec = names(dtfr)
     end_loop.int = (ncol(dtfr) + 1)
     for(i in 1:end_loop.int){
          if(names.vec[i] == col_name.s){
               return(i)
          }
          if(i == end_loop.int){
               return(NULL)
          }
     }
}


extractSubset <- function(dtfr, primary_select.s, criteria.s){                                           ## RETURNS DATA FRAME
     col_index.int = colIndex(dtfr = dtfr, col_name.s = primary_select.s)                                ## CALLS colIndex()
     do_return.vec = dtfr[ ,col_index.int] == criteria.s
     dtfr = dtfr[do_return.vec, ]
     return(dtfr)
}


doUse <- function(dtfr, checkVar.vec){                                                                   ## RETURNS DATA FRAME
     dtfr_names.vec = names(dtfr)                                                                        ## CALLS colIndex()
     for(i in 1:length(checkVar.vec)){                                                                   
          hold.s = checkVar.vec[i]
          if(hold.s %in% dtfr_names.vec){
               col.int = colIndex(dtfr = dtfr, hold.s)
               dtfr = dtfr[!is.na(dtfr[ ,col.int]), ]
          }
          else{
               print(paste("VARIABLE NOT FOUND:", hold.s, sep = ' '))
          }
     }
     return(dtfr)
}


weightedProp <- function(dtfr, primary_select.s, criteria.s, col_name.s = 'Data_Value',                  ## RETURNS DOUBLE
                         weight_var.s = 'Sample_Size'){                                                  ## CALLS extractSubset()
                                                                                                         ## CALLS colIndex()
     dtfr = extractSubset(dtfr = dtfr, primary_select.s = primary_select.s, criteria.s = criteria.s)                                                         ## CALLS colIndex()
     weight_index.int = colIndex(dtfr, weight_var.s)
     prop_index.int   = colIndex(dtfr, col_name.s)
     
     weight.vec = dtfr[ ,weight_index.int]
     props.vec  = dtfr[ ,prop_index.int]
     
     total_n.db = sum(weight.vec)
     weight.vec = weight.vec / total_n.db
     props.vec  = props.vec * weight.vec 
     
     return(sum(props.vec) / 100)
}


propDiffSTDERR <- function(a_prop.db, b_prop.db, a_n.int, b_n.int){                                     ## RETURNS DOUBLE
     a_n.int = as.numeric(a_n.int)
     b_n.int = as.numeric(b_n.int)
     
     denom = a_n.int * b_n.int
     num_a = a_prop.db*(1 - a_prop.db)*b_n.int
     num_b = b_prop.db*(1 - b_prop.db)*a_n.int
     out = (num_a + num_b) / denom
     
     return(sqrt(out))
} 
# NOTE: num_a * num_b often results in a 32 bit integer overflow. I considered using bit64 to allow for 64 bit integers, but in order to 
     # do floating point division, the values have to be coerced to a double anyway. 


recycler <- function(dtfr, variable.all, description.s){                                                 ## RETURNS DATA FRAME
     name = deparse(substitute(variable.all))
     new_row = list(description.s, name, variable.all)
     dtfr = rbind(dtfr, new_row)
     return(dtfr)
}


splitInsert <- function(outer.dtfr, inner.dtfr, insert_index.int, horiSplit.bool = TRUE){                ## RETURNS DATA FRAME
     if(horiSplit.bool == TRUE){
          if(insert_index.int > nrow(outer.dtfr)){
               return("INSERTION INDEX BEYOND LENGTH OF DATA FRAME")
          }
          if(ncol(outer.dtfr) != ncol(inner.dtfr)){
               return("UNEQUAL NUMBER OF COLUMNS")
          }
          
          outer_upper.dtfr = outer.dtfr[1:(insert_index.int - 1), ]
          outer_lower.dtfr = outer.dtfr[insert_index.int:nrow(outer.dtfr), ]
          out = rbind(outer_upper.dtfr, inner.dtfr, outer_lower.dtfr)
          return(out)
     }
     if(horiSplit.bool == FALSE){
          if(insert_index.int > ncol(outer.dtfr)){
               return("INSERTION INDEX BEYOND LENGTH OF DATA FRAME")
          }
          if(nrow(outer.dtfr) != nrow(inner.dtfr)){
               return("UNEQUAL NUMBER OF ROWS")
          }
          
          outer_left.dtfr = outer.dtfr[ ,1:(insert_index.int - 1)]
          outer_right.dtfr = outer.dtfr[ ,insert_index.int:ncol(outer.dtfr)]
          out = cbind(outer_left.dtfr, inner.dtfr, outer_right.dtfr)
          return(out)
     }
}
 

tableRow <- function(n.int, prop.db){                                                                    ## RETURNS VECTOR
     out = vector(length = 2)
     out[1] = n.int * prop.db
     out[2] = n.int * (1 - prop.db)
     return(out)
}


`%where%` <- function(y.vec, x.num){                                                                     ## RETURNS INTEGER
     out_index = 1
     for(i in 1:length(y.vec)){
          if(identical(y.vec[i], x.num)){
               break
          }
          else{
               out_index = out_index + 1
          }
     }
     return(out_index)
}


dtfrStretch <- function(dtfr){                                                                           ## RETURNS DATA FRAME
     type_ref = is.numeric(dtfr[ ,1])
     for(i in 1:ncol(dtfr)){
          type_check = (is.numeric(dtfr[ ,i]) != type_ref)
          if(type_check){
               return(NULL)
          }
     }
     
     col_name_ref = names(dtfr)
          total_cols = length(col_name_ref)
     row_name_ref = row.names(dtfr)
          total_rows = length(row_name_ref)
     out.dtfr = as.data.frame(matrix(ncol = 3, nrow = (total_cols * total_rows)))
          names(out.dtfr) = c("Row Name", "Column Name", "Value")
     iter = 1L
               
     for(i in 1:total_rows){
          for(j in 1:total_cols){
               out.dtfr[iter, 1] = row_name_ref[i]
               out.dtfr[iter, 2] = col_name_ref[j]
               out.dtfr[iter, 3] = dtfr[i, j]
               iter = iter + 1
          }
     }
     return(out.dtfr)   
}


XNOR <- function(a.num, b.num, eval.num){                                                                ## RETURNS BOOLEAN
     q.b = a.num > eval.num
     w.b = b.num > eval.num
     out.b = (q.b == w.b)
     
     return(out.b)
}


runCounter <- function(in.vec, threshold.db = 0.0){                                                      ## RETURNS INTEGER
     out = 1L                                                                                            ## CALLS XNOR()
     for(i in 1:(length(in.vec) - 1)){
          out = out + XNOR(in.vec[i], in.vec[i + 1], threshold.db)     
     }
     return(out)
}


curveVec <- function(in.vec, radius.db, direction.int = -1, x_correct.db = 5, y_correct.db = radius.db){ ## RETURNS VECTOR 
     in.vec = direction.int * (sqrt((radius.db ** 2) - ((in.vec - x_correct.db) ** 2)) - y_correct.db)
     return(in.vec)
}c

`%near%` <- function(vec, target.num){                                                                   ## RETURNS INTEGER
     error = abs(vec[1] - target.num)
     index = 1L
     for(i in 2:length(vec)){
          if(abs(vec[i] - target.num) < error){
               error = abs(vec[i] - target.num)
               index = i
          }
     }
     return(index)
}








