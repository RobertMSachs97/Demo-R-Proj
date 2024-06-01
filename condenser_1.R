# RELATIVE RISK TABLES ##########################################################################################################################
# COMMENT - There are two different orientations for the relative risk tables. The first orientation written had each stratum as a column, with 
     # different variables in each row. This was a mistake; it created some issues when accessing variables and makes it more difficult to 
     # to condense all of the data frames into one. The following code was faster to write than to go back and refactor. 

library(data.table)

 age_RR.dtfr.copy = age_RR.dtfr
 age_RR.dtfr = transpose(age_RR.dtfr)
     row.names(age_RR.dtfr) = names(age_RR.dtfr.copy)
     names(age_RR.dtfr) = row.names(age_RR.dtfr.copy)
rm(age_RR.dtfr.copy)

income_RR.dtfr.copy = income_RR.dtfr
income_RR.dtfr = transpose(income_RR.dtfr)
     row.names(income_RR.dtfr) = names(income_RR.dtfr.copy)
     names(income_RR.dtfr) = row.names(income_RR.dtfr.copy)
rm(income_RR.dtfr.copy)

names(age_RR.dtfr)[5]    = 'p Value' 
names(income_RR.dtfr)[5] = 'p Value'
names(rcethn_RR.dtfr)[5] = 'p Value'
all_RR.dtfr = rbind(edu_RR.dtfr, age_RR.dtfr, income_RR.dtfr, rcethn_RR.dtfr)
     rm(edu_RR.dtfr, age_RR.dtfr, income_RR.dtfr, rcethn_RR.dtfr)

# CONTINGENCY TABLES ############################################################################################################################

all_TABLE.dtfr = rbind(gender_TABLE.dtfr, edu_TABLE.dtfr, age_TABLE.dtfr, income_TABLE.dtfr, rcethn_TABLE.dtfr)
     rm(gender_TABLE.dtfr, edu_TABLE.dtfr, age_TABLE.dtfr, income_TABLE.dtfr, rcethn_TABLE.dtfr)

all_TABLE_EXP.dtfr = as.data.frame(rbind(gender_TABLE_EXP.mat, edu_TABLE_EXP.mat, age_TABLE_EXP.mat, income_TABLE_EXP.mat, rcethn_TABLE_EXP.mat))
     names(all_TABLE_EXP.dtfr) = c("Obese", "Not Obese")
     row.names(all_TABLE_EXP.dtfr) = c(row.names(gender_TABLE_STDRSD.dtfr), row.names(edu_TABLE_STDRSD.dtfr), row.names(age_TABLE_STDRSD.dtfr),
                                       row.names(income_TABLE_STDRSD.dtfr), row.names(rcethn_TABLE_STDRSD.dtfr))
     rm(gender_TABLE_EXP.mat, edu_TABLE_EXP.mat, age_TABLE_EXP.mat, income_TABLE_EXP.mat, rcethn_TABLE_EXP.mat)
     rm(nograd_TABLE_STDRSD.dtfr, nograd_TABLE.dtfr, nograd_TABLE_EXP.mat)
# COMMENT - The education table that excludes college graduates is being deleted without being added, as it is just a subset of the education 
     # table. 

all_TABLE_STDRSD.dtfr = rbind(gender_TABLE_STDRSD.dtfr, edu_TABLE_STDRSD.dtfr, age_TABLE_STDRSD.dtfr, income_TABLE_STDRSD.dtfr,
                              rcethn_TABLE_STDRSD.dtfr)
     rm(gender_TABLE_STDRSD.dtfr, edu_TABLE_STDRSD.dtfr, age_TABLE_STDRSD.dtfr, income_TABLE_STDRSD.dtfr, rcethn_TABLE_STDRSD.dtfr)

# MASTER DATA FRAME #############################################################################################################################

obesity_rates_sorted.dtfr = rbind(obesity_by_gender.dtfr, obesity_by_educat.dtfr, obesity_by_agegrp.dtfr, obesity_by_income.dtfr,
                                  obesity_by_rcethn.dtfr)
     rm(obesity_by_gender.dtfr, obesity_by_educat.dtfr, obesity_by_agegrp.dtfr, obesity_by_income.dtfr,
        obesity_by_rcethn.dtfr)
