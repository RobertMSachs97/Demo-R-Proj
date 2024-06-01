
# GENDER VARIABLES ##############################################################################################################################
# COMMENT - All obesity rates are weighted by the sample size of each observation. This is to ensure that values analyzed in this project are
     # parameters that reflect the population of individuals, not the population of surveys. 
     # - doUse() is the last layer of data cleaning. It is being used here to eliminate surveys that were deemed to have inadequate sample 
     # sizes by the data publisher. 

# DEFINE VARIABLES
obesity_by_gender.dtfr = extractSubset(dtfr = obesity_rates_all.dtfr, primary_select.s = 'Strat_criteria', criteria.s = 'Gender')
obesity_by_gender.dtfr = doUse(dtfr = obesity_by_gender.dtfr, checkVar.vec = c('Valid_Sample_Size', 'Sample_Size'))

male_p   = weightedProp(dtfr = obesity_by_gender.dtfr, primary_select.s = 'Stratum', criteria.s = 'Male')
female_p = weightedProp(dtfr = obesity_by_gender.dtfr, primary_select.s = 'Stratum', criteria.s = 'Female')
male_n   = sum(obesity_by_gender.dtfr[obesity_by_gender.dtfr$Stratum == 'Male',]$Sample_Size)
female_n = sum(obesity_by_gender.dtfr[obesity_by_gender.dtfr$Stratum == 'Female',]$Sample_Size)

gen_n_by_gender = male_n + female_n
# GENDER PROPORTION COMPARISON ##################################################################################################################

# WALD Z TEST OF DIFFERENCE
gender_diff = male_p - female_p
gender_diff_STDERR = propDiffSTDERR(a_prop.db = male_p, b_prop.db = female_p, 
                                    a_n.int   = male_n, b_n.int   = female_n)
gender_diff_PVAL = 1 - pnorm(gender_diff/gender_diff_STDERR)
     # RESULTS - p = < .00001, the difference is significant and we an reject H0. 

# RELATIVE RISK
gender_relative_risk = (male_p / female_p - 1) * 100
     # RESULTS - Males respondents have a 1.75% greater chance of being obese than a female respondent. 

# REMARK - We can see that being male elevates the probability of you being obese, however this change is fairly small. 

# GENDER INDEPENDENCE ###########################################################################################################################
# NULL HYPOTHESIS - Gender and obesity are independent. 
# NOTE - These independence tests were performed *after* the GENDER block. This means they will have their own section in the MEMORY CLEANING
     # block. 

gender_TABLE.dtfr = as.data.frame(matrix(ncol = 2, nrow = 2))
     names(gender_TABLE.dtfr) = c("Obese", "Not Obese")
     row.names(gender_TABLE.dtfr) = c("Male", "Female")
     
     
# CONTINGENCY TABLE           * OBESE                  * NOT OBESE                   *
     gender_TABLE.dtfr[1, ] = c((male_n   * male_p)  , (male_n   * (1 - male_p)))
     gender_TABLE.dtfr[2, ] = c((female_n * female_p), (female_n * (1 - female_p)))

gender_TABLE_EXP.mat = matrix(ncol = 2, nrow = 2)
gender_TABLE_STDRSD.dtfr = as.data.frame(matrix(ncol = 2, nrow = 2))
     names(gender_TABLE_STDRSD.dtfr) = c("Obese", "Not Obese")
     row.names(gender_TABLE_STDRSD.dtfr) = c("Male", "Female")

col_ref.vec = c(sum(gender_TABLE.dtfr$Obese), sum(gender_TABLE.dtfr$`Not Obese`))
row_ref.vec = c(male_n, female_n)
gender_XSQR = 0; gender_GSQR = 0

for(j in 1:2){
     for(i in 1:2){
          gender_TABLE_EXP.mat[i, j] = (col_ref.vec[j] * row_ref.vec[i]) / gen_n_by_gender
     }
}

for(j in 1:2){
     for(i in 1:2){
          gender_XSQR = gender_XSQR + (((gender_TABLE.dtfr[i, j] - gender_TABLE_EXP.mat[i, j])^2) / gender_TABLE_EXP.mat[i, j])
          gender_GSQR = gender_GSQR + (2 * gender_TABLE.dtfr[i, j] * log(gender_TABLE.dtfr[i, j] / gender_TABLE_EXP.mat[i, j]))
          
          hold_1.f = (1 - (col_ref.vec[j] / gen_n_by_gender))
          hold_2.f = (1 - (row_ref.vec[i] / gen_n_by_gender))
          iter_NULL_STDERR = sqrt(gender_TABLE.dtfr[i, j] * hold_1.f * hold_2.f)
          
          gender_TABLE_STDRSD.dtfr[i, j] = (gender_TABLE.dtfr[i, j] - gender_TABLE_EXP.mat[i, j]) / iter_NULL_STDERR
     }
}

gender_XSQR; gender_GSQR

gender_XSQR_PVAL = 1 - pchisq(gender_XSQR, df = 1)
gender_GSQR_PVAL = 1 - pchisq(gender_GSQR, df = 1)


# EDUCATION VARIABLES ###########################################################################################################################
obesity_by_educat.dtfr = extractSubset(dtfr = obesity_rates_all.dtfr, primary_select.s = 'Strat_criteria', criteria.s = 'Education')
obesity_by_educat.dtfr = doUse(dtfr = obesity_by_educat.dtfr, checkVar.vec = c('Valid_Sample_Size', 'Sample_Size'))

lt_hs_p    = weightedProp(dtfr = obesity_by_educat.dtfr, primary_select.s = 'Stratum', criteria.s = 'Less than high school')
hs_grad_p  = weightedProp(dtfr = obesity_by_educat.dtfr, primary_select.s = 'Stratum', criteria.s = 'High school graduate')
some_clg_p = weightedProp(dtfr = obesity_by_educat.dtfr, primary_select.s = 'Stratum', criteria.s = 'Some college or technical school')
clg_grad_p = weightedProp(dtfr = obesity_by_educat.dtfr, primary_select.s = 'Stratum', criteria.s = 'College graduate')

lt_hs_n    = sum(obesity_by_educat.dtfr[obesity_by_educat.dtfr$Stratum == 'Less than high school', ]$Sample_Size)
hs_grad_n  = sum(obesity_by_educat.dtfr[obesity_by_educat.dtfr$Stratum == 'High school graduate', ]$Sample_Size)
some_clg_n = sum(obesity_by_educat.dtfr[obesity_by_educat.dtfr$Stratum == 'Some college or technical school', ]$Sample_Size)
clg_grad_n = sum(obesity_by_educat.dtfr[obesity_by_educat.dtfr$Stratum == 'College graduate', ]$Sample_Size)

pops.vec = c(lt_hs_n, hs_grad_n, some_clg_n, clg_grad_n)
rates.vec = c(lt_hs_p, hs_grad_p, some_clg_p, clg_grad_p)

# EDUCATION RELATIVE RISK #######################################################################################################################
# NULL HYPOTHESIS - The difference in the stratum proportion of obesity is not different than the average proportion of every other stratum. 

gen_n_by_edu = sum(pops.vec)
gen_prob_by_edu = sum(pops.vec * rates.vec) / gen_n_by_edu

edu_RR.dtfr = as.data.frame(matrix(ncol = 5, nrow = 4))
     names(edu_RR.dtfr) = c("Stratum Obesity Rate", "Non-stratum Obesity Rate", "Relative Risk", "Stratum Size", "p Value")
     row.names(edu_RR.dtfr) = unique(obesity_by_educat.dtfr$Stratum)

edu_RR.dtfr[ ,1] = rates.vec
edu_RR.dtfr[ ,4] = pops.vec

for(i in 1:4){
     non_strat_p = sum(pops.vec[-i] * rates.vec[-i]) / sum(pops.vec[-i])
     edu_RR.dtfr[i, 2] = non_strat_p
     edu_RR.dtfr[i, 3] = rates.vec[i] / non_strat_p
}
for(i in 1:4){
     iter_STDERR = propDiffSTDERR(a_prop.db = edu_RR.dtfr[i, 1], a_n.int = edu_RR.dtfr[i, 4],
                                  b_prop.db = edu_RR.dtfr[i, 2], b_n.int = gen_n_by_edu - edu_RR.dtfr[i, 4])
     diff_CHISQ = ((edu_RR.dtfr[i, 1] - edu_RR.dtfr[i, 2]) / iter_STDERR)^2
     edu_RR.dtfr[i, 5] = pchisq(diff_CHISQ, df = 1, lower.tail = FALSE)
}

# RESULT - All differences are statistically significant. 

# EDUCATION INDEPENDENCE ########################################################################################################################
# NULL HYPOTHESIS - Obesity and education are independent and not associated. 

edu_TABLE.dtfr = as.data.frame(matrix(nrow = 4, ncol = 2))
     names(edu_TABLE.dtfr)     = c("Obese", "Not Obese")
     row.names(edu_TABLE.dtfr) = c("Less than high school", "High school graduate", "Some college or technical school", "College graduate")
# CONTINGENCY TABLE 
     # EDUCATION LEVEL     * OBESE                     * NOT OBESE                   *
     edu_TABLE.dtfr[1, ] = c((lt_hs_n    * lt_hs_p)   , (lt_hs_n    * (1 - lt_hs_p)))
     edu_TABLE.dtfr[2, ] = c((hs_grad_n  * hs_grad_p) , (hs_grad_n  * (1 - hs_grad_p)))
     edu_TABLE.dtfr[3, ] = c((some_clg_n * some_clg_p), (some_clg_n * (1 - some_clg_p)))
     edu_TABLE.dtfr[4, ] = c((clg_grad_n * clg_grad_p), (clg_grad_n * (1 - clg_grad_p)))

edu_TABLE_EXP.mat = matrix(nrow = 4, ncol = 2)
edu_TABLE_STDRSD.dtfr = as.data.frame(matrix(nrow = 4, ncol = 2))
     names(edu_TABLE_STDRSD.dtfr) = c("Obese", "Not Obese")
     row.names(edu_TABLE_STDRSD.dtfr) = c("Less than high school", "High school graduate", "Some college or technical school",
                                          "College graduate")

col_ref.vec = c(sum(edu_TABLE.dtfr$Obese), sum(edu_TABLE.dtfr$`Not Obese`))
row_ref.vec = c(lt_hs_n, hs_grad_n, some_clg_n, clg_grad_n)
edu_XSQR = 0; edu_GSQR = 0

for(j in 1:2){ 
     for(i in 1:4){
          edu_TABLE_EXP.mat[i, j] = (col_ref.vec[j]*row_ref.vec[i]) / gen_n_by_edu
     }
}

for(j in 1:2){
     for(i in 1:4){
          edu_XSQR = edu_XSQR + (((edu_TABLE.dtfr[i, j] - edu_TABLE_EXP.mat[i, j])^2) / edu_TABLE_EXP.mat[i, j])
          edu_GSQR = edu_GSQR + (2 * edu_TABLE.dtfr[i, j] * log(edu_TABLE.dtfr[i, j] / edu_TABLE_EXP.mat[i ,j]))
          
          hold_1.f = (1 - (col_ref.vec[j] / gen_n_by_edu))
          hold_2.f = (1 - (row_ref.vec[i] / gen_n_by_edu))
          iter_NULL_STDERR = sqrt(edu_TABLE_EXP.mat[i, j] * hold_1.f * hold_2.f)
          
          edu_TABLE_STDRSD.dtfr[i, j] = (edu_TABLE.dtfr[i, j] - edu_TABLE_EXP.mat[i, j]) / iter_NULL_STDERR
     }
}

edu_GSQR_PVAL = 1 - pchisq(edu_GSQR, df = 3)
edu_XSQR_PVAL = 1 - pchisq(edu_XSQR, df = 3)

# RESULTS - Evidence against independence is very strong with all strata included, with both edu_GSQR_PVAL and edu_XSQR_PVAL returning as zero.
# REMARK - This next section removes the college graduate population from the test, to account for the possibility it is acting like an outlier.

nograd_TABLE.dtfr = as.data.frame(matrix(nrow = 3, ncol = 2))
     names(nograd_TABLE.dtfr) = c("Obese", "Not Obese")
     row.names(nograd_TABLE.dtfr) = c("Less than high school", "High school graduate", "Some college or technical school")
     nograd_TABLE.dtfr = edu_TABLE.dtfr[1:3, ]

nograd_TABLE_STDRSD.dtfr = as.data.frame(matrix(nrow = 3, ncol = 2))
     names(nograd_TABLE_STDRSD.dtfr) = c("Obese", "Not Obese")
     row.names(nograd_TABLE_STDRSD.dtfr) = c("Less than high school", "High school graduate", "Some college or technical school")

nograd_TABLE_EXP.mat = matrix(nrow = 3, ncol = 2)

gen_n = gen_n_by_edu - clg_grad_n
col_ref.vec = col_ref.vec - c(edu_TABLE.dtfr[4, 1], edu_TABLE.dtfr[4, 2])

for(j in 1:2){
     for(i in 1:3){
          nograd_TABLE_EXP.mat[i ,j] = (col_ref.vec[j] * row_ref.vec[i]) / gen_n
     }
}

edu_nograd_XSQR = 0; edu_nograd_GSQR = 0
for(j in 1:2){
     for(i in 1:3){
          edu_nograd_XSQR = edu_nograd_XSQR + (((nograd_TABLE.dtfr[i, j] - nograd_TABLE_EXP.mat[i, j])^2) / nograd_TABLE_EXP.mat[i, j])
          edu_nograd_GSQR = edu_nograd_GSQR + (2 * nograd_TABLE.dtfr[i, j] * log(nograd_TABLE.dtfr[i, j] / nograd_TABLE_EXP.mat[i ,j]))
          
          hold_1.f = (1 - (col_ref.vec[j] / gen_n))
          hold_2.f = (1 - (row_ref.vec[i] / gen_n))
          iter_NULL_STDERR = sqrt(nograd_TABLE_EXP.mat[i, j] * hold_1.f * hold_2.f)
          
          nograd_TABLE_STDRSD.dtfr[i, j] = (nograd_TABLE.dtfr[i, j] - nograd_TABLE_EXP.mat[i, j]) / iter_NULL_STDERR
     }
}

edu_nograd_GSQR_PVAL = 1 - pchisq(edu_nograd_GSQR, df = 2)
edu_nograd_XSQR_PVAL = 1 - pchisq(edu_nograd_XSQR, df = 2)

# RESULTS - There is still strong evidence against independence, however it's worth noting that the XSQR and GSQR statistics are both much lower 
     # than with the college population included. This is reflected by the greatly reduced residuals. 

# MISC
gen_prob_less_clg = ((lt_hs_p * lt_hs_n) + (hs_grad_p * hs_grad_n) + (some_clg_p * some_clg_n)) / gen_n

plot(edu_RR.dtfr[ ,3], col = 'blue', ylab = "Relative Risk", main = "Education Group Relative Risk", ylim = c(0.6, 1.4))
     lines(rep(1, 4), lwd = 2)

# MEMORY CLEANING ###############################################################################################################################
recycling_bin.dtfr = data.frame(matrix(ncol = 3, nrow = 0))

# GENDER 
recycling_bin.dtfr = recycler(recycling_bin.dtfr, NA, "*   *   *   *   *   GENDER DATA")
recycling_bin.dtfr = recycler(recycling_bin.dtfr, male_p, "Proportion of males that are obese:")
recycling_bin.dtfr = recycler(recycling_bin.dtfr, female_p, "Proportion of female that are obese:")
recycling_bin.dtfr = recycler(recycling_bin.dtfr, male_n, "Number of valid male observations:")
recycling_bin.dtfr = recycler(recycling_bin.dtfr, female_n, "Number of valid female observations:")
recycling_bin.dtfr = recycler(recycling_bin.dtfr, gender_diff, "Male obesity rate - female obesity rate:")
recycling_bin.dtfr = recycler(recycling_bin.dtfr, gender_diff_STDERR, "Difference standard error:")
recycling_bin.dtfr = recycler(recycling_bin.dtfr, gender_diff_PVAL, "Gender difference P value:")
recycling_bin.dtfr = recycler(recycling_bin.dtfr, gender_relative_risk, "Gender relative risk:") 

rm(male_p, female_p, male_n, female_n, gender_diff, gender_diff_STDERR, gender_diff_PVAL, gender_relative_risk)
names(recycling_bin.dtfr) = c("Description", "Object Name", "Value")

# EDUCATION
rm(row_ref.vec, col_ref.vec, hold_1.f, hold_2.f, iter_NULL_STDERR, i, j, iter_STDERR, rates.vec, pops.vec, diff_CHISQ, non_strat_p) 

recycling_bin.dtfr = recycler(recycling_bin.dtfr, NA, "*   *   *   *   *   EDUCATION DATA")
recycling_bin.dtfr = recycler(recycling_bin.dtfr, gen_n_by_edu, "Total sample size:")
recycling_bin.dtfr = recycler(recycling_bin.dtfr, gen_n, "Total sample size less college graduates:")
recycling_bin.dtfr = recycler(recycling_bin.dtfr, gen_prob_by_edu, "Obesity rate over all strata:")
rm(gen_n_by_edu, gen_n, gen_prob_by_edu)

recycling_bin.dtfr = recycler(recycling_bin.dtfr, lt_hs_n, "Size of the \"Less than HS education\" stratum:")
recycling_bin.dtfr = recycler(recycling_bin.dtfr, lt_hs_p, "Obesity rate in the \"Less than HS education\" stratum:")
recycling_bin.dtfr = recycler(recycling_bin.dtfr, hs_grad_n, "Size of the \"HS graduate\" stratum:")
recycling_bin.dtfr = recycler(recycling_bin.dtfr, hs_grad_p, "Obesity rate in the \"HS graduate\" stratum")
recycling_bin.dtfr = recycler(recycling_bin.dtfr, some_clg_n, "Size of the \"Some college or trade school\" stratum:")
recycling_bin.dtfr = recycler(recycling_bin.dtfr, some_clg_p, "Obesity rate in the \"Some college or trade school\" stratum:")
recycling_bin.dtfr = recycler(recycling_bin.dtfr, clg_grad_n, "Size of the \"College graduate\" stratum:")
recycling_bin.dtfr = recycler(recycling_bin.dtfr, clg_grad_p, "Obesity rate in the \"College graduate\" stratum:")
rm(lt_hs_n, lt_hs_p, hs_grad_n, hs_grad_p, some_clg_n, some_clg_p, clg_grad_n, clg_grad_p)

recycling_bin.dtfr = recycler(recycling_bin.dtfr, edu_GSQR, "G Square statistic for education/obesity independence test:")
recycling_bin.dtfr = recycler(recycling_bin.dtfr, edu_GSQR_PVAL, "G Square p value for education/obesity independence test:")
recycling_bin.dtfr = recycler(recycling_bin.dtfr, edu_XSQR, "X Square statistic for education/obesity indepencence test:")
recycling_bin.dtfr = recycler(recycling_bin.dtfr, edu_XSQR_PVAL, "X Square p value for education/obesity independence test:")
recycling_bin.dtfr = recycler(recycling_bin.dtfr, edu_nograd_GSQR, "G Square statistic for education/obesity less college grads:")
recycling_bin.dtfr = recycler(recycling_bin.dtfr, edu_nograd_GSQR_PVAL, "G Square p value for education/obeisty less college grads:")
recycling_bin.dtfr = recycler(recycling_bin.dtfr, edu_nograd_XSQR, "X Square statistic for education/obesity less college grads:")
recycling_bin.dtfr = recycler(recycling_bin.dtfr, edu_nograd_XSQR_PVAL, "X Square p value for education/obeisty less college grads:")
rm(edu_GSQR, edu_GSQR_PVAL, edu_XSQR, edu_nograd_XSQR_PVAL, edu_nograd_GSQR, edu_nograd_GSQR_PVAL, edu_nograd_XSQR, edu_nograd_XSQR_PVAL, 
   edu_XSQR_PVAL)

# GENDER PART 2

insert.dtfr = data.frame(matrix(ncol = 3, nrow = 0))
insert.dtfr = recycler(insert.dtfr, gen_n_by_gender, "Total sample size:")
insert.dtfr = recycler(insert.dtfr, gender_GSQR, "G Square statistic for gender/obesity independence test:")
insert.dtfr = recycler(insert.dtfr, gender_GSQR_PVAL, "G Square p value for gender/obesity independence test:")
insert.dtfr = recycler(insert.dtfr, gender_XSQR, "X Square statistic for gender/obesity independence test:")
insert.dtfr = recycler(insert.dtfr, gender_XSQR_PVAL, "X Square p value for gender/obesity independence test:")

names(insert.dtfr) = names(recycling_bin.dtfr)
recycling_bin2.dtfr = splitInsert(outer.dtfr = recycling_bin.dtfr, inner.dtfr = insert.dtfr, insert_index.int = 10)
recycling_bin.dtfr = recycling_bin2.dtfr

rm(recycling_bin2.dtfr, insert.dtfr, gen_n_by_gender, gender_GSQR, gender_GSQR_PVAL, gender_XSQR, gender_XSQR_PVAL)
rm(gen_prob_less_clg)


