# AGE GROUP VARIABLES ###########################################################################################################################
obesity_by_agegrp.dtfr = extractSubset(dtfr = obesity_rates_all.dtfr, primary_select.s = 'Strat_criteria', criteria.s = 'Age (years)')
obesity_by_agegrp.dtfr = doUse(dtfr = obesity_by_agegrp.dtfr, checkVar.vec = c('Valid_Sample_Size', 'Sample_Size'))

age_18_24_p = weightedProp(dtfr = obesity_by_agegrp.dtfr, primary_select.s = 'Stratum', criteria.s = '18 - 24')
age_18_24_n = sum(obesity_by_agegrp.dtfr[obesity_by_agegrp.dtfr$Stratum == '18 - 24', ]$Sample_Size)

age_25_34_p = weightedProp(dtfr = obesity_by_agegrp.dtfr, primary_select.s = 'Stratum', criteria.s = '25 - 34')
age_25_34_n = sum(obesity_by_agegrp.dtfr[obesity_by_agegrp.dtfr$Stratum == '25 - 34', ]$Sample_Size)

age_35_44_p = weightedProp(dtfr = obesity_by_agegrp.dtfr, primary_select.s = 'Stratum', criteria.s = '35 - 44')
age_35_44_n = sum(obesity_by_agegrp.dtfr[obesity_by_agegrp.dtfr$Stratum == '35 - 44', ]$Sample_Size)

age_45_54_p = weightedProp(dtfr = obesity_by_agegrp.dtfr, primary_select.s = 'Stratum', criteria.s = '45 - 54')
age_45_54_n = sum(obesity_by_agegrp.dtfr[obesity_by_agegrp.dtfr$Stratum == '45 - 54', ]$Sample_Size)

age_55_64_p = weightedProp(dtfr = obesity_by_agegrp.dtfr, primary_select.s = 'Stratum', criteria.s = '55 - 64')
age_55_64_n = sum(obesity_by_agegrp.dtfr[obesity_by_agegrp.dtfr$Stratum == '55 - 64', ]$Sample_Size)

age_65_plus_p = weightedProp(dtfr = obesity_by_agegrp.dtfr, primary_select.s = 'Stratum', criteria.s = '65 or older')
age_65_plus_n = sum(obesity_by_agegrp.dtfr[obesity_by_agegrp.dtfr$Stratum == '65 or older', ]$Sample_Size)

gen_n_by_age = sum(age_65_plus_n, age_55_64_n, age_45_54_n, age_35_44_n, age_25_34_n, age_18_24_n)
gen_prob_by_age = ((age_18_24_n * age_18_24_p) 
                 + (age_25_34_n * age_25_34_p) 
                 + (age_35_44_n * age_35_44_p) 
                 + (age_45_54_n * age_45_54_p)
                 + (age_55_64_n * age_55_64_p) 
                 + (age_65_plus_n * age_65_plus_p)) / gen_n_by_age

# AGE RELATIVE RISK #############################################################################################################################

# NULL HYPOTHESIS - The difference in the stratum proportion of obesity is not different than the average proportion of every other stratum. 
age_RR.dtfr = as.data.frame(matrix(ncol = 6, nrow = 4))
     names(age_RR.dtfr) = unique(obesity_by_agegrp.dtfr$Stratum)
     row.names(age_RR.dtfr) = c("Stratum Obesity Rate", "Non-stratum Obesity Rate", "Relative Risk", "Stratum Size")
     
rates.vec = c(age_18_24_p, age_25_34_p, age_35_44_p, age_45_54_p, age_55_64_p, age_65_plus_p)
pops.vec  = c(age_18_24_n, age_25_34_n, age_35_44_n, age_45_54_n, age_55_64_n, age_65_plus_n)

age_RR.dtfr[1, ] = rates.vec
for(i in 1:6){
     non_strat_p = sum(pops.vec[-i] * rates.vec[-i]) / sum(pops.vec[-i])
     age_RR.dtfr[2, i] = non_strat_p
     age_RR.dtfr[3, i] = rates.vec[i] / non_strat_p
}
age_RR.dtfr[4, ] = pops.vec
rm(rates.vec, i)

plot(y = c(age_RR.dtfr[1,]), x = 1:6, cex = .7, col = 'blue', main = "Age Group Obesity Rates", ylim = c(0, .5), ylab = 'Obesity Rate')
     lines(c(rep(gen_prob_by_age, 6)), lwd = 2)
plot(y = c(age_RR.dtfr[3, ]), x = 1:6, cex = .7, main = 'Age Group Relative Risk', col = 'blue', ylim = c(.5, 1.5), ylab = 'Relative Risk')
     lines(c(rep(1, 6)), lwd = 2)

age_RR.dtfr = rbind(age_RR.dtfr, rep(NA, 6))
     row.names(age_RR.dtfr)[5] = "p-value"

for(i in 1:6){
     iter_STDERR = propDiffSTDERR(a_prop.db = age_RR.dtfr[1, i], b_prop.db = age_RR.dtfr[2, i], a_n.int = age_RR.dtfr[4, i], 
                                        b_n.int = (gen_n_by_age - age_RR.dtfr[4, i]))
     diff_CHISQR = ((age_RR.dtfr[1, i] - age_RR.dtfr[2, i]) / iter_STDERR) ** 2

     age_RR.dtfr[5, i] = pchisq(diff_CHISQR, df = 1, lower.tail = FALSE)
}
     
# RESULTS - It appears that all of the differences are significant. 
# COMMENT - I'm noticing all significance tests or any statistics that are evaluated against a probability distribution are returning extreme 
     # values. This is probably just a function of the very large sample size. In my opinion, testing for proportion differences and testing 
     # against independence seems to be more of a formality, rather than coming from genuine doubt. 

# AGE INDEPENDENCE ##############################################################################################################################   
     
# NULL HYPOTHESIS - Age group and obesity are independent. 
age_TABLE.dtfr = as.data.frame(matrix(ncol = 2, nrow = 6))
     names(age_TABLE.dtfr) = c("Obese", "Not Obese")
     row.names(age_TABLE.dtfr) = names(age_RR.dtfr)
# CONTINGENCY TABLE        * OBESE                        * NOT OBESE
     age_TABLE.dtfr[1, ] = c((age_18_24_n * age_18_24_p), (age_18_24_n * (1 - age_18_24_p)))
     age_TABLE.dtfr[2, ] = c((age_25_34_n * age_25_34_p), (age_25_34_n * (1 - age_25_34_p)))
     age_TABLE.dtfr[3, ] = c((age_35_44_n * age_35_44_p), (age_35_44_n * (1 - age_35_44_p)))
     age_TABLE.dtfr[4, ] = c((age_45_54_n * age_45_54_p), (age_45_54_n * (1 - age_45_54_p)))
     age_TABLE.dtfr[5, ] = c((age_55_64_n * age_55_64_p), (age_55_64_n * (1 - age_55_64_p)))
     age_TABLE.dtfr[6, ] = c((age_65_plus_n * age_65_plus_p), (age_65_plus_n * (1 - age_65_plus_p)))

age_TABLE_EXP.mat = matrix(nrow = 6, ncol = 2)
age_TABLE_STDRSD.dtfr = as.data.frame(matrix(nrow = 6, ncol = 2))
     names(age_TABLE_STDRSD.dtfr) = c("Obese", "Not Obese")
     row.names(age_TABLE_STDRSD.dtfr) = names(age_RR.dtfr)

col_ref.vec = c(sum(age_TABLE.dtfr$Obese), sum(age_TABLE.dtfr$`Not Obese`))
age_XSQR = 0; age_GSQR = 0;

for(j in 1:2){
     for(i in 1:6){
          age_TABLE_EXP.mat[i, j] = col_ref.vec[j]*pops.vec[i] / gen_n_by_age
     }
}
for(j in 1:2){
     for(i in 1:6){
          age_XSQR = age_XSQR + (((age_TABLE.dtfr[i, j] - age_TABLE_EXP.mat[i, j])^2) / age_TABLE_EXP.mat[i, j])
          age_GSQR = age_GSQR + (2 * age_TABLE.dtfr[i, j] * log(age_TABLE.dtfr[i, j] / age_TABLE_EXP.mat[i, j]))
          
          hold_1.f = (1 - (col_ref.vec[j] / gen_n_by_age))
          hold_2.f = (1 - (pops.vec[i] / gen_n_by_age))
          iter_NULL_STDERR = sqrt(age_TABLE_EXP.mat[i ,j] * hold_1.f * hold_2.f)
          
          age_TABLE_STDRSD.dtfr[i ,j] = (age_TABLE.dtfr[i, j] - age_TABLE_EXP.mat[i, j]) / iter_NULL_STDERR
     }
}

age_XSQR_PVAL = 1 - pchisq(age_XSQR, df = 5)
age_GSQR_PVAL = 1 - pchisq(age_GSQR, df = 5)

# RESULTS - Age and obesity are not independent. 

# AGE MEMORY CLEANING ###########################################################################################################################
recycling_bin.dtfr = recycler(recycling_bin.dtfr, NA, "*    *    *    *    *    AGE DATA")
recycling_bin.dtfr = recycler(recycling_bin.dtfr, age_18_24_n, "Size of age 18 - 24 stratum:")
recycling_bin.dtfr = recycler(recycling_bin.dtfr, age_18_24_p, "Obesity rate in age 18 - 24 stratum:")
recycling_bin.dtfr = recycler(recycling_bin.dtfr, age_25_34_n, "Size of age 25 - 34 stratum:")
recycling_bin.dtfr = recycler(recycling_bin.dtfr, age_25_34_p, "Obesity rate in age 25 - 34 stratum:")
recycling_bin.dtfr = recycler(recycling_bin.dtfr, age_35_44_n, "Size of age 35 - 44 stratum:")
recycling_bin.dtfr = recycler(recycling_bin.dtfr, age_35_44_p, "Obesity rate in age 35 - 44 stratum:")
recycling_bin.dtfr = recycler(recycling_bin.dtfr, age_45_54_n, "Size of age 45 - 54 stratum:")
recycling_bin.dtfr = recycler(recycling_bin.dtfr, age_45_54_p, "Obesity rate in age 45 - 54 stratum:")
recycling_bin.dtfr = recycler(recycling_bin.dtfr, age_55_64_n, "Size of age 55 - 64 stratum:")
recycling_bin.dtfr = recycler(recycling_bin.dtfr, age_55_64_p, "Obesity rate in age 55 - 64 stratum:")
recycling_bin.dtfr = recycler(recycling_bin.dtfr, age_65_plus_n, "Size of age 65 plus stratum:")
recycling_bin.dtfr = recycler(recycling_bin.dtfr, age_65_plus_p, "Obesity rate in age 65 plus stratum:")
rm(age_18_24_n, age_18_24_p, age_25_34_n, age_25_34_p, age_35_44_n, age_35_44_p, age_45_54_n, age_45_54_p, age_55_64_n, age_55_64_p, 
   age_65_plus_n, age_65_plus_p)

recycling_bin.dtfr = recycler(recycling_bin.dtfr, gen_n_by_age, "Total sample size of all age based strata:")
recycling_bin.dtfr = recycler(recycling_bin.dtfr, gen_prob_by_age, "Mean obesity rate over all age based strata:")
recycling_bin.dtfr = recycler(recycling_bin.dtfr, age_GSQR, "G Square statistic for age/obesity independence test:")
recycling_bin.dtfr = recycler(recycling_bin.dtfr, age_XSQR, "X Square statistic for age/obesity independence test:")
recycling_bin.dtfr = recycler(recycling_bin.dtfr, age_GSQR_PVAL, "p value from age/obesity G Square test:")
recycling_bin.dtfr = recycler(recycling_bin.dtfr, age_XSQR_PVAL, "p value from age/obesity X Square test:")
rm(gen_n_by_age, gen_prob_by_age, age_GSQR, age_GSQR_PVAL, age_XSQR, age_XSQR_PVAL)

rm(i ,j, col_ref.vec, hold_1.f, hold_2.f, iter_NULL_STDERR, non_strat_p, pops.vec)



# INCOME VARIABLES ##############################################################################################################################
obesity_by_income.dtfr = extractSubset(dtfr = obesity_rates_all.dtfr, primary_select.s = 'Strat_criteria', criteria.s = 'Income')
obesity_by_income.dtfr = doUse(dtfr = obesity_by_income.dtfr, checkVar.vec = c('Valid_Sample_Size', 'Sample_Size'))

lt_15k_n = sum(obesity_by_income.dtfr[obesity_by_income.dtfr$Stratum == "Less than $15,000", ]$Sample_Size)
lt_15k_p = weightedProp(dtfr = obesity_by_income.dtfr, primary_select.s = 'Stratum', criteria.s = "Less than $15,000")

from_15k_24999_n = sum(obesity_by_income.dtfr[obesity_by_income.dtfr$Stratum == "$15,000 - $24,999", ]$Sample_Size)
from_15k_24999_p = weightedProp(dtfr = obesity_by_income.dtfr, primary_select.s = 'Stratum', criteria.s = "$15,000 - $24,999")

from_25k_34999_n = sum(obesity_by_income.dtfr[obesity_by_income.dtfr$Stratum == "$25,000 - $34,999", ]$Sample_Size)
from_25k_34999_p = weightedProp(dtfr = obesity_by_income.dtfr, primary_select.s = 'Stratum', criteria.s = "$25,000 - $34,999")

from_35k_49999_n = sum(obesity_by_income.dtfr[obesity_by_income.dtfr$Stratum == "$35,000 - $49,999", ]$Sample_Size)
from_35k_49999_p = weightedProp(dtfr = obesity_by_income.dtfr, primary_select.s = 'Stratum', criteria.s = "$35,000 - $49,999")

from_50k_74999_n = sum(obesity_by_income.dtfr[obesity_by_income.dtfr$Stratum == "$50,000 - $74,999", ]$Sample_Size)
from_50k_74999_p = weightedProp(dtfr = obesity_by_income.dtfr, primary_select.s = 'Stratum', criteria.s = "$50,000 - $74,999")

mt_75k_n = sum(obesity_by_income.dtfr[obesity_by_income.dtfr$Stratum == "$75,000 or greater", ]$Sample_Size)            
mt_75k_p = weightedProp(dtfr = obesity_by_income.dtfr, primary_select.s = 'Stratum', criteria.s = "$75,000 or greater")

inc_NO_DATA_n = sum(obesity_by_income.dtfr[obesity_by_income.dtfr$Stratum == "Data not reported", ]$Sample_Size)
inc_NO_DATA_p = weightedProp(dtfr = obesity_by_income.dtfr, primary_select.s = 'Stratum', criteria.s = "Data not reported")

rates.vec = c(lt_15k_p, from_15k_24999_p, from_25k_34999_p, from_35k_49999_p, from_50k_74999_p, mt_75k_p, inc_NO_DATA_p)
pops.vec  = c(lt_15k_n, from_15k_24999_n, from_25k_34999_n, from_35k_49999_n, from_50k_74999_n, mt_75k_n, inc_NO_DATA_n)

gen_n_by_income = sum(pops.vec)
gen_prob_by_income = sum(rates.vec * pops.vec) / sum(pops.vec)

# INCOME RELATIVE RISK ##########################################################################################################################

rates.vec = c(lt_15k_p, from_15k_24999_p, from_25k_34999_p, from_35k_49999_p, from_50k_74999_p, mt_75k_p, inc_NO_DATA_p)
pops.vec  = c(lt_15k_n, from_15k_24999_n, from_25k_34999_n, from_35k_49999_n, from_50k_74999_n, mt_75k_n, inc_NO_DATA_n)

income_RR.dtfr = as.data.frame(matrix(ncol = 7, nrow = 4))
     names(income_RR.dtfr) = unique(obesity_by_income.dtfr$Stratum)
     row.names(income_RR.dtfr) = c("Stratum Obesity Rate", "Non-stratum Obesity Rate", "Relative Risk", "Stratum Size")

income_RR.dtfr[1, ] = rates.vec
for(i in 1:7){
     non_strat_p = sum(pops.vec[-i] * rates.vec[-i]) / sum(pops.vec[-i])
     income_RR.dtfr[2, i] = non_strat_p
     income_RR.dtfr[3, i] = rates.vec[i] / non_strat_p
}

income_RR.dtfr[4, ] = pops.vec

plot(y = c(income_RR.dtfr[1,]), x = 1:7, cex = .7, col = 'blue', main = "Income Group Obesity Rates", ylim = c(.2, .4), ylab = 'Obesity Rate')
     lines(c(rep(gen_prob_by_income, 7)), lwd = 2)

plot(y = c(income_RR.dtfr[3, ]), x = 1:7, cex = .7, main = 'Income Group Relative Risk', col = 'blue', ylim = c(.5, 1.5), ylab = 'Relative Risk')
     lines(c(rep(1, 7)), lwd = 2)

# COMMENT - What is this demographic of mystery income people with a much lower obesity rate than the other strata? In general, I think removing
     # outliers just for being outliers can be jumping the gun, as they can contain valuable information. However, if we are doing stratum-wise
     # comparisons we can't ignore the fact there are observations not sorted into any stratum. 
     
# PROCEDURE - Test the difference between the population sample mean with the "Data not reported" included and with "Data not reported" removed.
     # - If the difference is significant, then relevant risk and the significance of each stratum mean must be reevaluated without the bad
     #   data. 

# NULL HYPOTHESIS - The population mean obesity rate with "Data not reported" is the same as when "Data not reported" is removed. 
gen_prob_by_income_mod = sum(pops.vec[-7] * rates.vec[-7]) / sum(pops.vec[-7])
diff_STDERR = propDiffSTDERR(a_prop.db = gen_prob_by_income,     a_n.int = gen_n_by_income, 
                             b_prop.db = gen_prob_by_income_mod, b_n.int = sum(pops.vec[-7]))

diff_CHISQ = ((gen_prob_by_income_mod - gen_prob_by_income) / diff_STDERR)^2
sample_diff_PVAL = pchisq(diff_CHISQ, df = 1, lower.tail = FALSE)

# RESULTS - The difference is significant. "Data not collected" should be removed. 

income_RR_mod.dtfr = as.data.frame(matrix(ncol = 6, nrow = 4))
     names(income_RR_mod.dtfr) = names(income_RR.dtfr)[1:6]
     row.names(income_RR_mod.dtfr) = row.names(income_RR.dtfr)
income_RR_mod.dtfr[1, ] = income_RR.dtfr[1, 1:6]
income_RR_mod.dtfr[4, ] = income_RR.dtfr[4, 1:6]

for(i in 1:6){
     non_strat_p = sum(pops.vec[1:6][-i] * rates.vec[1:6][-i]) / sum(pops.vec[1:6][-i])
     income_RR_mod.dtfr[2, i] = non_strat_p
     income_RR_mod.dtfr[3, i] = income_RR_mod.dtfr[1, i] / non_strat_p
}

# VISUALIZATION - The difference it relative risk rises by just under 4%. 
plot(y = c(income_RR.dtfr[1,]), x = 1:7, cex = .7, col = 'blue', main = "Income Group Obesity Rates", ylim = c(.2, .4), ylab = 'Obesity Rate')
     lines(c(rep(gen_prob_by_income, 7)), lwd = 2, col = 'darkblue')
     lines(rep(mean(unlist(income_RR_mod.dtfr[1, ])), 7), lwd = 2, col = 'darkred')

plot(y = c(income_RR.dtfr[3, ]), x = 1:7, cex = .7, main = 'Income Group Relative Risk', col = 'blue', ylim = c(.5, 1.5), ylab = 'Relative Risk')
     lines(c(rep(1, 7)), lwd = 2)
     points(y = unlist(income_RR_mod.dtfr[3, ]), x = 1:6, cex = .7, col = 'red')

# NULL HYPOTHESIS - The difference in the stratum proportion of obesity is not different than the average proportion of every other stratum.
     
income_RR_mod.dtfr = rbind(income_RR_mod.dtfr, rep(NA, 6))
     row.names(income_RR_mod.dtfr)[5] = "Difference p Value"

gen_n_by_income_mod = sum(pops.vec[1:6])

for(i in 1:6){
     iter_STDERR = propDiffSTDERR(a_prop.db = income_RR_mod.dtfr[1, i], a_n.int = income_RR_mod.dtfr[4, i],
                                  b_prop.db = income_RR_mod.dtfr[2, i], b_n.int = gen_n_by_income_mod - income_RR_mod.dtfr[4, i])
     stat = ((income_RR_mod.dtfr[1, i] - income_RR_mod.dtfr[2, i]) / iter_STDERR)^2
     pval = pchisq(stat, df = 1, lower.tail = FALSE)
     income_RR_mod.dtfr[5, i] = pval
}

# RESULTS - All of the differences are still significant. 

gen_n_by_income = gen_n_by_income_mod
gen_prob_by_income = gen_prob_by_income_mod
income_RR.dtfr = income_RR_mod.dtfr
rm(gen_n_by_income_mod, gen_prob_by_income_mod, income_RR_mod.dtfr)

# INCOME INDEPENDENCE ###########################################################################################################################

# NULL HYPOTHESIS - Income and obesity rates are independent. 
income_TABLE.dtfr = as.data.frame(matrix(ncol = 2, nrow = 6))
     names(income_TABLE.dtfr) = c("Obese", "Not Obese")
     row.names(income_TABLE.dtfr) = names(income_RR.dtfr)
     # CONTINGENCY TABLE
     income_TABLE.dtfr[1, ] = tableRow(n.int = lt_15k_n,         prop.db = lt_15k_p)
     income_TABLE.dtfr[2, ] = tableRow(n.int = from_15k_24999_n, prop.db = from_15k_24999_p)
     income_TABLE.dtfr[3, ] = tableRow(n.int = from_25k_34999_n, prop.db = from_25k_34999_p)
     income_TABLE.dtfr[4, ] = tableRow(n.int = from_35k_49999_n, prop.db = from_35k_49999_p)
     income_TABLE.dtfr[5, ] = tableRow(n.int = from_50k_74999_n, prop.db = from_50k_74999_p)
     income_TABLE.dtfr[6, ] = tableRow(n.int = mt_75k_n,         prop.db = mt_75k_p)
     
income_TABLE_EXP.mat = matrix(ncol = 2, nrow = 6)
income_TABLE_STDRSD.dtfr = as.data.frame(matrix(ncol = 2, nrow = 6))
     names(income_TABLE_STDRSD.dtfr) = names(income_TABLE.dtfr)
     row.names(income_TABLE_STDRSD.dtfr) = row.names(income_TABLE.dtfr)

col_ref.vec = c(sum(income_TABLE.dtfr$Obese), sum(income_TABLE.dtfr$`Not Obese`))
row_ref.vec = c(lt_15k_n, from_15k_24999_n, from_25k_34999_n, from_35k_49999_n, from_50k_74999_n, mt_75k_n)
income_GSQR = 0; income_XSQR = 0;

for(j in 1:2){
     for(i in 1:6){
          income_TABLE_EXP.mat[i ,j] = (col_ref.vec[j] * row_ref.vec[i]) / gen_n_by_income
     }
}
for(j in 1:2){
     for(i in 1:6){
          income_XSQR = income_XSQR + (((income_TABLE.dtfr[i , j] - income_TABLE_EXP.mat[i, j])^2) / income_TABLE_EXP.mat[i, j])
          income_GSQR = income_GSQR + (2 * income_TABLE.dtfr[i, j] * log(income_TABLE.dtfr[i, j] / income_TABLE_EXP.mat[i, j]))
          
          hold_1.db = (1 - (col_ref.vec[j] / gen_n_by_income))
          hold_2.db = (1 - (row_ref.vec[i] / gen_n_by_income))
          iter_NULL_STDERR = sqrt(income_TABLE_EXP.mat[i, j] * hold_1.db * hold_2.db)
          
          income_TABLE_STDRSD.dtfr[i, j] = (income_TABLE.dtfr[i, j] - income_TABLE_EXP.mat[i, j]) / iter_NULL_STDERR
     }
}

income_GSQR_PVAL = pchisq(income_GSQR, df = 5, lower.tail = FALSE)
income_XSQR_PVAL = pchisq(income_XSQR, df = 5, lower.tail = FALSE)

# RESULTS - Reject the null hypothesis that obesity and income are independent. 

# INCOME MEMORY CLEANING ########################################################################################################################
rm(i, j, hold_1.db, hold_2.db, col_ref.vec, row_ref.vec, non_strat_p, pval, rates.vec, pops.vec, iter_NULL_STDERR, iter_STDERR, stat, 
   diff_STDERR)

recycling_bin.dtfr = recycler(recycling_bin.dtfr, NA, "*    *    *    *    *    INCOME DATA")
recycling_bin.dtfr = recycler(recycling_bin.dtfr, lt_15k_n, "Size of <$15k stratum:")
recycling_bin.dtfr = recycler(recycling_bin.dtfr, lt_15k_p, "Obesity rate in <$15k stratum:")
recycling_bin.dtfr = recycler(recycling_bin.dtfr, from_15k_24999_n, "Size of $15k - $24999 stratum:")
recycling_bin.dtfr = recycler(recycling_bin.dtfr, from_15k_24999_p, "Obesity rate in $15k - $24999 stratum:")
recycling_bin.dtfr = recycler(recycling_bin.dtfr, from_25k_34999_n, "Size of $25k - $34999 stratum:")
recycling_bin.dtfr = recycler(recycling_bin.dtfr, from_25k_34999_p, "Obesity rate in $25k - $34999 stratum:")
recycling_bin.dtfr = recycler(recycling_bin.dtfr, from_35k_49999_n, "Size of $35k - $49999 stratum:")
recycling_bin.dtfr = recycler(recycling_bin.dtfr, from_35k_49999_p, "Obesity rate in $35k - $49999 stratum:")
recycling_bin.dtfr = recycler(recycling_bin.dtfr, from_50k_74999_n, "Size of $50k - $74999 stratum:")
recycling_bin.dtfr = recycler(recycling_bin.dtfr, from_50k_74999_p, "Obesity rate in $50k - $74999 stratum:")
recycling_bin.dtfr = recycler(recycling_bin.dtfr, mt_75k_n, "Size of >$75k stratum:")
recycling_bin.dtfr = recycler(recycling_bin.dtfr, mt_75k_p, "Obesity rate in >$75k stratum:")

rm(lt_15k_n, lt_15k_p, from_15k_24999_n, from_15k_24999_p, from_25k_34999_n, from_25k_34999_p, from_35k_49999_n, from_35k_49999_p, 
   from_50k_74999_n, from_50k_74999_p, mt_75k_n, mt_75k_p)

recycling_bin.dtfr = recycler(recycling_bin.dtfr, gen_n_by_income, "Total size of all income base strata:")
recycling_bin.dtfr = recycler(recycling_bin.dtfr, gen_prob_by_income, "Mean obesity rate over all income based strata:")
recycling_bin.dtfr = recycler(recycling_bin.dtfr, diff_CHISQ, "Chi-square test statistic for the difference in mean obesity with/without
                                                                 stratum \"Data not reported\" included:")
recycling_bin.dtfr = recycler(recycling_bin.dtfr, sample_diff_PVAL, "p value for the difference in mean obesity with/without stratum 
                                                                 \"Data not reported\" included:")
recycling_bin.dtfr = recycler(recycling_bin.dtfr, inc_NO_DATA_n, "Size of \"Data not reported\" stratum:")
recycling_bin.dtfr = recycler(recycling_bin.dtfr, inc_NO_DATA_p, "Obesity rate in \"Data not reported\" stratum:")

rm(gen_n_by_income, gen_prob_by_income, diff_CHISQ, sample_diff_PVAL, inc_NO_DATA_n, inc_NO_DATA_p)

recycling_bin.dtfr = recycler(recycling_bin.dtfr, income_GSQR, "G Square statistic for income/obesity independence:")
recycling_bin.dtfr = recycler(recycling_bin.dtfr, income_GSQR_PVAL, "p value for income/obesity G Square test:")
recycling_bin.dtfr = recycler(recycling_bin.dtfr, income_XSQR, "X Square statistic for income/obesity independence:")
recycling_bin.dtfr = recycler(recycling_bin.dtfr, income_XSQR_PVAL, "X Square statistic for income/obesity independence:")

rm(income_GSQR, income_GSQR_PVAL, income_XSQR, income_XSQR_PVAL)

# RACE/ETHNICITY VARIABLES ######################################################################################################################
obesity_by_rcethn.dtfr = extractSubset(dtfr = obesity_rates_all.dtfr, primary_select.s = 'Strat_criteria', criteria.s = 'Race/Ethnicity')
     obesity_by_rcethn.dtfr = doUse(dtfr = obesity_by_rcethn.dtfr, checkVar.vec = c('Valid_Sample_Size', 'Sample_Size')) 
     
white_n = sum(obesity_by_rcethn.dtfr[obesity_by_rcethn.dtfr$Stratum == "Non-Hispanic White", ]$Sample_Size)
white_p = weightedProp(dtfr = obesity_by_rcethn.dtfr, primary_select.s = 'Stratum', criteria.s = "Non-Hispanic White")

black_n = sum(obesity_by_rcethn.dtfr[obesity_by_rcethn.dtfr$Stratum == "Non-Hispanic Black", ]$Sample_Size)
black_p = weightedProp(dtfr = obesity_by_rcethn.dtfr, primary_select.s = 'Stratum', criteria.s = "Non-Hispanic Black")

hisp_n = sum(obesity_by_rcethn.dtfr[obesity_by_rcethn.dtfr$Stratum == "Hispanic", ]$Sample_Size)
hisp_p = weightedProp(dtfr = obesity_by_rcethn.dtfr, primary_select.s = 'Stratum', criteria.s = "Hispanic")

native_n = sum(obesity_by_rcethn.dtfr[obesity_by_rcethn.dtfr$Stratum == "American Indian/Alaska Native", ]$Sample_Size)
native_p = weightedProp(dtfr = obesity_by_rcethn.dtfr, primary_select.s = 'Stratum', criteria.s = "American Indian/Alaska Native")

# COMMENT - Mixed will combine "2 or more races" and "other". 
more_n  = sum(obesity_by_rcethn.dtfr[obesity_by_rcethn.dtfr$Stratum == "2 or more races", ]$Sample_Size) 
other_n = sum(obesity_by_rcethn.dtfr[obesity_by_rcethn.dtfr$Stratum == "Other", ]$Sample_Size)
mixed_n = other_n + more_n
mixed_p = (weightedProp(dtfr = obesity_by_rcethn.dtfr, primary_select.s = 'Stratum', criteria.s = "2 or more races") * more_n) +
          (weightedProp(dtfr = obesity_by_rcethn.dtfr, primary_select.s = 'Stratum', criteria.s = "Other") * other_n)
     mixed_p = mixed_p / mixed_n
rm(more_n, other_n)
     
asian_n = sum(obesity_by_rcethn.dtfr[obesity_by_rcethn.dtfr$Stratum == "Asian", ]$Sample_Size)
asian_p = weightedProp(dtfr = obesity_by_rcethn.dtfr, primary_select.s = 'Stratum', criteria.s = "Asian",)

haw_pac_n = sum(obesity_by_rcethn.dtfr[obesity_by_rcethn.dtfr$Stratum == "Hawaiian/Pacific Islander", ]$Sample_Size)
haw_pac_p = weightedProp(dtfr = obesity_by_rcethn.dtfr, primary_select.s = 'Stratum', criteria.s = "Hawaiian/Pacific Islander")

pops.vec  = c(white_n, black_n, hisp_n, native_n, mixed_n, asian_n, haw_pac_n)
rates.vec = c(white_p, black_p, hisp_p, native_p, mixed_p, asian_p, haw_pac_p)
gen_n_by_rcethn = sum(pops.vec)
gen_prob_by_rcethn = sum(pops.vec * rates.vec) / sum(pops.vec)

plot(rates.vec, cex = .7, col = 'blue', main = "Obesity Rates by Race Overview", ylim = c(.1, .4), ylab = 'Obesity Rate')
     lines(c(rep(gen_prob_by_rcethn, 7)), lwd = 2)
     
# RACE RELATIVE RISK ############################################################################################################################     

# COMMENT - Unlike the other relative race data frames, this one will have strata listed vertically so each variable is automatically stored as 
     # a vector instead of a list. 
rcethn_RR.dtfr = as.data.frame(matrix(ncol = 5, nrow = 7))
     names(rcethn_RR.dtfr) = c(row.names(income_RR.dtfr))
     row.names(rcethn_RR.dtfr) = c("White", "Black", "Hispanic", "Native", "Mixed", "Asian", "Hawaiian or Pacific Islander")

rcethn_RR.dtfr[ ,1] = rates.vec
rcethn_RR.dtfr[ ,4] = pops.vec
for(i in 1:7){
     non_strat_p = sum(pops.vec[-i] * rates.vec[-i]) / sum(pops.vec[-i])
     rcethn_RR.dtfr[i, 2] = non_strat_p
     rcethn_RR.dtfr[i, 3] = rates.vec[i] / non_strat_p
}
# VISUALIZATION
plot(rcethn_RR.dtfr[ ,3], cex = .7, col = 'blue', main = "Relative Risk Overview", ylab = "Relative Risk")
     lines(rep(1, 7), lwd = 2)
# NULL HYPOTHESIS - Each statum obesity rate is not different from the mean obesity rate. 
for(i in 1:7){
     diff_STDERR = propDiffSTDERR(a_prop.db = rcethn_RR.dtfr[i, 1], a_n.int = rcethn_RR.dtfr[i, 4],
                    b_prop.db = rcethn_RR.dtfr[i, 2], b_n.int = (gen_n_by_rcethn - rcethn_RR.dtfr[i, 1]))
     diff_CHISQ = ((gen_prob_by_rcethn - rcethn_RR.dtfr[i, 1]) / diff_STDERR)^2
     rcethn_RR.dtfr[i, 5] = pchisq(diff_CHISQ, df = 1, lower.tail = FALSE)
}
# RESULTS - The null hypothesis can be rejected for every stratum. 

# RACE INDEPENDENCE #############################################################################################################################

# NULL HYPOTHESIS - Race and obesity rates are independent. 
     
rcethn_TABLE.dtfr = as.data.frame(matrix(nrow = 7, ncol = 2))
     names(rcethn_TABLE.dtfr) = c("Obese", "Not Obese")
     row.names(rcethn_TABLE.dtfr) = row.names(rcethn_RR.dtfr)
rcethn_TABLE_EXP.mat = matrix(nrow = 7, ncol = 2)
rcethn_TABLE_STDRSD.dtfr = rcethn_TABLE.dtfr

# CONTINGENCY TABLE 
     rcethn_TABLE.dtfr[1, ] = tableRow(n.int = white_n,   prop.db = white_p)
     rcethn_TABLE.dtfr[2, ] = tableRow(n.int = black_n,   prop.db = black_p)
     rcethn_TABLE.dtfr[3, ] = tableRow(n.int = hisp_n,    prop.db = hisp_p)
     rcethn_TABLE.dtfr[4, ] = tableRow(n.int = native_n,  prop.db = native_p)
     rcethn_TABLE.dtfr[5, ] = tableRow(n.int = mixed_n,   prop.db = mixed_p)
     rcethn_TABLE.dtfr[6, ] = tableRow(n.int = asian_n,   prop.db = asian_p)
     rcethn_TABLE.dtfr[7, ] = tableRow(n.int = haw_pac_n, prop.db = haw_pac_p)
     
rcethn_GSQR = 0; rcethn_XSQR  = 0
col_ref.vec = c(sum(rcethn_TABLE.dtfr$Obese), sum(rcethn_TABLE.dtfr$`Not Obese`)) 

for(j in 1:2){
     for(i in 1:7){
          rcethn_TABLE_EXP.mat[i, j] = (col_ref.vec[j] * pops.vec[i]) / gen_n_by_rcethn
     }
}

for(j in 1:2){
     for(i in 1:7){
          rcethn_XSQR = rcethn_XSQR + (((rcethn_TABLE.dtfr[i, j] - rcethn_TABLE_EXP.mat[i, j])^2) / rcethn_TABLE_EXP.mat[i, j])
          rcethn_GSQR = rcethn_GSQR + (2 * rcethn_TABLE.dtfr[i, j] * log(rcethn_TABLE.dtfr[i, j] / rcethn_TABLE_EXP.mat[i, j]))
          
          hold_1.db = (1 - (col_ref.vec[j] / gen_n_by_rcethn))
          hold_2.db = (1 - (pops.vec[i] / gen_n_by_rcethn))
          iter_NULL_STDERR = sqrt(rcethn_TABLE_EXP.mat[i, j] * hold_1.db * hold_2.db)
          
          rcethn_TABLE_STDRSD.dtfr[i, j] = (rcethn_TABLE.dtfr[i, j] - rcethn_TABLE_EXP.mat[i, j]) / iter_NULL_STDERR
     }
}

rcethn_GSQR_PVAL = pchisq(rcethn_GSQR, df = 6, lower.tail = FALSE)
rcethn_XSQR_PVAL = pchisq(rcethn_XSQR, df = 6, lower.tail = FALSE)

# RESULTS - Reject the null hypothesis: obesity and race/ethnicity are not independent.  

# RACE MEMORY CLEANING ##########################################################################################################################

rm(i ,j, col_ref.vec, pops.vec, hold_1.db, hold_2.db, iter_NULL_STDERR, non_strat_p, rates.vec, diff_CHISQ, diff_STDERR)

recycling_bin.dtfr = recycler(recycling_bin.dtfr, NA, "*    *    *    *    *    RACE DATA")
recycling_bin.dtfr = recycler(recycling_bin.dtfr, asian_n, "Size of Asian stratum:")
recycling_bin.dtfr = recycler(recycling_bin.dtfr, asian_p, "Obesity rate in Asian stratum:")
recycling_bin.dtfr = recycler(recycling_bin.dtfr, black_n, "Size of Black stratum:")
recycling_bin.dtfr = recycler(recycling_bin.dtfr, black_p, "Obesity rate in Black stratum:")
recycling_bin.dtfr = recycler(recycling_bin.dtfr, haw_pac_n, "Size of Hawaiian or Pacific Islander startum:")
recycling_bin.dtfr = recycler(recycling_bin.dtfr, haw_pac_p, "Obesity rate in Hawaiian or Pacific Islander stratum:")
recycling_bin.dtfr = recycler(recycling_bin.dtfr, hisp_n, "Size of Hispanic stratum:")
recycling_bin.dtfr = recycler(recycling_bin.dtfr, hisp_p, "Obesity rate in Hispanic stratum:")
recycling_bin.dtfr = recycler(recycling_bin.dtfr, mixed_n, "Size of mixed race stratum:")
recycling_bin.dtfr = recycler(recycling_bin.dtfr, mixed_p, "Obesity rate in mixed race stratum:")
recycling_bin.dtfr = recycler(recycling_bin.dtfr, native_n, "Size of Native American stratum:")
recycling_bin.dtfr = recycler(recycling_bin.dtfr, native_p, "Obesity rate in Native American stratum:")
recycling_bin.dtfr = recycler(recycling_bin.dtfr, white_n, "Size of White stratum:")
recycling_bin.dtfr = recycler(recycling_bin.dtfr, white_p, "Obesity rate in White stratum:")

rm(asian_n, asian_p, black_n, black_p, haw_pac_n, haw_pac_p, hisp_n, hisp_p, mixed_n, mixed_p, native_n, native_p, white_n, white_p)

recycling_bin.dtfr = recycler(recycling_bin.dtfr, gen_n_by_rcethn, "Total sample size of all race based strata:")
recycling_bin.dtfr = recycler(recycling_bin.dtfr, gen_prob_by_rcethn, "Obesity rate over all race based strata:")
recycling_bin.dtfr = recycler(recycling_bin.dtfr, rcethn_GSQR, "G Square statistic for race/obesity independence test:")
recycling_bin.dtfr = recycler(recycling_bin.dtfr, rcethn_GSQR_PVAL, "p value from G Square independence test:")
recycling_bin.dtfr = recycler(recycling_bin.dtfr, rcethn_XSQR, "X Square statistic for race/independence test:")
recycling_bin.dtfr = recycler(recycling_bin.dtfr, rcethn_XSQR_PVAL, "p value from X Square independence test:")

rm(gen_n_by_rcethn, gen_prob_by_rcethn, rcethn_GSQR, rcethn_GSQR_PVAL, rcethn_XSQR, rcethn_XSQR_PVAL)

