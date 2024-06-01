# REDUNDANCY ####################################################################################################################################
# COMMENT - One major source of ambiguity in this data set is whether there is duplicate data in each stratification category or not. To start, 
     # we can test whether each stratification category recorded in different ways. If the sample sizes and obesity rates are the same, then we
     # can assume it's the sample sample being reused. If not, then it is likely that it this data set is composed of multiple different surveys. 
# From recycling_bin.dtfr: 
     # gen_n_by_gender  6.117083e+06
     # gen_n_by_edu  6.105418e+06
     # gen_n_by_age  6.410104e+06
     # gen_n_by_income  4.545000e+06
     # gen_n_by_rcethn  6.012527e+06
# RESULTS - Based off of cleaned data, the samples are different. 
# COMMENT - It is also worth checking the uncleaned data to see whether the difference in n is only a result of cleaning. 

sum(extractSubset(dtfr = obesity_rates_all.dtfr, primary_select.s = 'Strat_criteria', criteria.s = 'Gender')$Sample_Size, na.rm = TRUE)
sum(extractSubset(dtfr = obesity_rates_all.dtfr, primary_select.s = 'Strat_criteria', criteria.s = 'Education')$Sample_Size, na.rm = TRUE)
sum(extractSubset(dtfr = obesity_rates_all.dtfr, primary_select.s = 'Strat_criteria', criteria.s = 'Age (years)')$Sample_Size, na.rm = TRUE)
sum(extractSubset(dtfr = obesity_rates_all.dtfr, primary_select.s = 'Strat_criteria', criteria.s = 'Income')$Sample_Size, na.rm = TRUE)
sum(extractSubset(dtfr = obesity_rates_all.dtfr, primary_select.s = 'Strat_criteria', criteria.s = 'Race/Ethnicity')$Sample_Size, na.rm = TRUE)

nrow(extractSubset(dtfr = obesity_rates_all.dtfr, primary_select.s = 'Strat_criteria', criteria.s = 'Gender'))
nrow(extractSubset(dtfr = obesity_rates_all.dtfr, primary_select.s = 'Strat_criteria', criteria.s = 'Education'))
nrow(extractSubset(dtfr = obesity_rates_all.dtfr, primary_select.s = 'Strat_criteria', criteria.s = 'Age (years)'))
nrow(extractSubset(dtfr = obesity_rates_all.dtfr, primary_select.s = 'Strat_criteria', criteria.s = 'Income'))
nrow(extractSubset(dtfr = obesity_rates_all.dtfr, primary_select.s = 'Strat_criteria', criteria.s = 'Race/Ethnicity'))

# RESULT - They appear to all be different. 
# COMMENT - Treating the data like it has no duplicated values is still an assumption. The variable coordinates could be used to test for 
     # duplicates, but even then we don't know exactly where the coordinates came from. For example: a point could refer the location of a state,
     # or where the survey itself happened. This also applies to the dates as well. 


# STATE VARIABLES ###############################################################################################################################

state_abb.vec = unique(obesity_rates_sorted.dtfr$Location) 
state_data.dtfr = as.data.frame(matrix(nrow = 55, ncol = 3))
     names(state_data.dtfr) = c("Sample Size", "Obesity Rate", "Is State")
     row.names(state_data.dtfr) = state_abb.vec

for(i in 1:55){
     hold.dtfr = extractSubset(dtfr = obesity_rates_sorted.dtfr, primary_select.s = 'Location', criteria.s = state_abb.vec[i])
     n.int = sum(hold.dtfr$Sample_Size)
     p.db = sum(hold.dtfr$Sample_Size * hold.dtfr$Data_Value) / n.int
     in.b = state_abb.vec[i] %in% state.abb
     
     state_data.dtfr[i, 1] = n.int
     state_data.dtfr[i, 2] = p.db / 100
     state_data.dtfr[i, 3] = in.b
}
rm(hold.dtfr, n.int, p.db, in.b)

# COMMENT - The 'US' stratum won't be used as a control, as we don't know where the data came from in the U.S. This is a similar problem to the 
     # "Data not reported" stratum in the income section. However, unlike "Data not reported", it didn't appear drastically different from all 
     # the other strata. 

# BASIC OVERVIEW
plot(state_data.dtfr[ ,2], cex = .3, main = 'All State Obesity Rates', ylab = 'Obesity Rates')
shapiro.test(state_data.dtfr[ ,2])

gen_n_by_state = sum(state_data.dtfr[-2, 1])
gen_prob_by_state = sum(state_data.dtfr[-2, 1] * state_data.dtfr[-2, 2]) / gen_n_by_state

# RELATIVE RISK #################################################################################################################################

state_RR.dtfr = as.data.frame(matrix(nrow = 55, ncol = 5))
     names(state_RR.dtfr) = c("Stratum Obesity Rate", "Non-stratum Obesity Rate","Stratum Size", "Relative Risk", "Difference p Value")
     row.names(state_RR.dtfr) = state_abb.vec

for(i in c(1:55)[-2]){
     state_RR.dtfr[i, 1] = state_data.dtfr$`Obesity Rate`[i]
     non_strat_p = sum(state_data.dtfr$`Obesity Rate`[c(-2, -i)] * state_data.dtfr$`Sample Size`[c(-2, -i)]) / 
                    sum(state_data.dtfr$`Sample Size`[c(-2, -i)])
     state_RR.dtfr[i, 2] = non_strat_p
     state_RR.dtfr[i, 3] = state_data.dtfr$`Sample Size`[i]
     state_RR.dtfr[i, 4] = state_RR.dtfr[i, 1] / non_strat_p
}
     
for(i in c(1:55)[-2]){
     iter_STDERR = propDiffSTDERR(a_prop.db = state_RR.dtfr[i, 1], a_n.int = state_RR.dtfr[i, 3],
                                  b_prop.db = state_RR.dtfr[i, 2], b_n.int = gen_n_by_state - state_RR.dtfr[i, 3])
     diff_CHISQ = ((state_RR.dtfr[i, 1] - state_RR.dtfr[i, 2]) / iter_STDERR)^2

     state_RR.dtfr[i, 5] = pchisq(diff_CHISQ, df = 1, lower.tail = FALSE)
}
state_RR.dtfr[state_RR.dtfr[ ,5] > .05, ]
# RESULT - Maine and Virginia are the only states that did not have a significant difference from the other states. 

state_RR.dtfr = state_RR.dtfr[order(state_RR.dtfr$`Relative Risk`), ]

# STATE INDEPENDENCE ############################################################################################################################

state_TABLE.dtfr = as.data.frame(matrix(ncol = 2, nrow = 54))
     names(state_TABLE.dtfr) = c("Obese", "Not Obese")
     row.names(state_TABLE.dtfr) = row.names(state_RR.dtfr)[1:54]
state_TABLE_STDRSD.dtfr = state_TABLE.dtfr   
state_TABLE_EXP.dtfr = state_TABLE.dtfr

for(i in 1:54){
     state_TABLE.dtfr[i, ] = tableRow(n.int = state_RR.dtfr[i, 3], prop.db = state_RR.dtfr[i, 1])
}
col_ref.vec = c(sum(state_TABLE.dtfr[ ,1]), sum(state_TABLE.dtfr[ ,2]))
state_XSQR = 0; state_GSQR = 0

for(j in 1:2){
     for(i in 1:54){
          state_TABLE_EXP.dtfr[i, j] = (col_ref.vec[j] * state_RR.dtfr[i, 3]) / gen_n_by_state
     }
}
for(j in 1:2){
     for(i in 1:54){
          state_XSQR = state_XSQR + (((state_TABLE.dtfr[i, j] - state_TABLE_EXP.dtfr[i, j])^2) / state_TABLE_EXP.dtfr[i, j])
          state_GSQR = state_GSQR + (2 * state_TABLE.dtfr[i, j] * log(state_TABLE.dtfr[i, j] / state_TABLE_EXP.dtfr[i, j]))
          
          hold_1.db = (1 - (col_ref.vec[j] / gen_n_by_state))
          hold_2.db = (1 - (state_RR.dtfr[i, 3] / gen_n_by_state))
          iter_NULL_STDERR = sqrt(state_TABLE_EXP.dtfr[i, j] * hold_1.db * hold_2.db)
          
          state_TABLE_STDRSD.dtfr[i, j] = (state_TABLE.dtfr[i, j] - state_TABLE_EXP.dtfr[i, j]) / iter_NULL_STDERR
     }
}

state_XSQR_PVAL = pchisq(state_XSQR, df = 53, lower.tail = FALSE)
state_GSQR_PVAL = pchisq(state_GSQR, df = 53, lower.tail = FALSE)

# STATE MEMORY CLEANING #########################################################################################################################
rm(i, j, non_strat_p, iter_NULL_STDERR, hold_1.db, hold_2.db, state_abb.vec, diff_CHISQ, col_ref.vec, iter_STDERR)

all_TABLE.dtfr = rbind(all_TABLE.dtfr, state_TABLE.dtfr)
all_TABLE_EXP.dtfr = rbind(all_TABLE_EXP.dtfr, state_TABLE_EXP.dtfr)
all_TABLE_STDRSD.dtfr = rbind(all_TABLE_STDRSD.dtfr, state_TABLE_STDRSD.dtfr)
rm(state_TABLE.dtfr, state_TABLE_EXP.dtfr, state_TABLE_STDRSD.dtfr)

recycling_bin.dtfr = recycler(dtfr = recycling_bin.dtfr, gen_n_by_state, "Total sample size of all states:")
recycling_bin.dtfr = recycler(dtfr = recycling_bin.dtfr, gen_prob_by_state, "Obesity rate through all state based strata:")
recycling_bin.dtfr = recycler(dtfr = recycling_bin.dtfr, state_GSQR, "G Square statistic for state/obesity independence:") 
recycling_bin.dtfr = recycler(dtfr = recycling_bin.dtfr, state_XSQR, "X Square statistic for state/obesity independence:")
recycling_bin.dtfr = recycler(dtfr = recycling_bin.dtfr, state_GSQR_PVAL, "p value from G Square independence test:")
recycling_bin.dtfr = recycler(dtfr = recycling_bin.dtfr, state_XSQR_PVAL, "p value from X Square independence test:")

rm(gen_n_by_state, gen_prob_by_state, state_XSQR, state_XSQR_PVAL, state_GSQR, state_GSQR_PVAL)

# COMMENT - A mistake was made in the state relative risk table. The names don't quite match and the relative risk and stratum columns are out 
     # of order. A copy is being modified instead of the original because at this point in the script, the original data frames have been 
     # deleted. 

stratum.vec = state_RR.dtfr$`Stratum Size`
rr.vec = state_RR.dtfr$`Relative Risk`
p_val.vec = state_RR.dtfr$`Difference p Value`
left_section.dtfr = state_RR.dtfr[ ,1:2]

reordered.dtfr = cbind(left_section.dtfr, rr.vec, stratum.vec, p_val.vec)
     names(reordered.dtfr) = names(all_RR.dtfr)

all_RR_copy.dtfr = rbind(all_RR.dtfr, reordered.dtfr)
rm(stratum.vec, rr.vec, p_val.vec, left_section.dtfr, reordered.dtfr)

all_RR.dtfr = all_RR_copy.dtfr
rm(all_RR_copy.dtfr)


