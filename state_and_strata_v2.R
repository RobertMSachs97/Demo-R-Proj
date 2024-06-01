# DATA EDITING ##################################################################################################################################
# COMMENT - This section edits obesity_rates_sorted.dtfr. It combines the two race strata "2 or more races" and "Other" into a "Mixed" category, 
     # and omits the "Data not reported" stratum from the income data. 
# ANOTHER COMMENT - These were already combined in the previous data analysis, they just weren't combined in the master data frame. 

sorted_rates_edited.dtfr = obesity_rates_sorted.dtfr
sorted_rates_edited.dtfr = sorted_rates_edited.dtfr[sorted_rates_edited.dtfr$Stratum != "Data not reported", ]

for(i in 1:nrow(sorted_rates_edited.dtfr)){
     if(sorted_rates_edited.dtfr[i, 5] == "2 or more races"){
          sorted_rates_edited.dtfr[i, 5] = "Mixed"
     }
     if(sorted_rates_edited.dtfr[i, 5] == "Other"){
          sorted_rates_edited.dtfr[i, 5] = "Mixed"
     }
}

# MAIN DTFR #####################################################################################################################################
STATE_ABB.vec   = row.names(all_RR.dtfr)[24:77]        # i54
STRATA.vec = unique(sorted_rates_edited.dtfr$Stratum)  # j25 

all_rates.dtfr = as.data.frame(matrix(ncol = 25, nrow = 54))
     names(all_rates.dtfr) = STRATA.vec
     row.names(all_rates.dtfr) = STATE_ABB.vec
all_counts.dtfr = all_rates.dtfr
     
for(j in 1:25){
     hold.dtfr = sorted_rates_edited.dtfr[sorted_rates_edited.dtfr[ ,5] == STRATA.vec[j], ]
     for(i in 1:54){
          hold_2.dtfr = hold.dtfr[hold.dtfr[ ,2] == STATE_ABB.vec[i], ]
          pops.vec = hold_2.dtfr$Sample_Size
          rates.vec = hold_2.dtfr$Data_Value / 100
          
          all_rates.dtfr[i, j] = sum(pops.vec * rates.vec) / sum(pops.vec)
          all_counts.dtfr[i, j] = sum(pops.vec)
     }
}

all_rates_stretched.dtfr = dtfrStretch(all_rates.dtfr)
all_rates_stretched.dtfr = all_rates_stretched.dtfr[order(all_rates_stretched.dtfr$Value), ]

# SEPERATION BY NORMALITY #######################################################################################################################
# The purpose of this section to evaluate each state's obesity rate in a given demographic. Ideally, each state will have a z-score, 
     # so if a state has a score of 1, it means it's rate is 1 standard deviation above the mean for the given demographic. Unfortunately, not 
     # all demographics have normal distributions, so the demographics must be separated into normal and non-normal groups before being examined.

all_data_PRCTL.dtfr = as.data.frame(matrix(ncol = 25, nrow = 59))
     names(all_data_PRCTL.dtfr) = names(all_rates.dtfr) 
     row.names(all_data_PRCTL.dtfr) = c("Normal", "Shapiro-Wilks", "Skewness", "Kurtosis", "BREAK", row.names(all_rates.dtfr))

for(i in 1:ncol(all_data_PRCTL.dtfr)){
     all_data_PRCTL.dtfr[2, i] = shapiro.test(all_rates.dtfr[ ,i])$p.value
     all_data_PRCTL.dtfr[3, i] = moments::skewness(all_rates.dtfr[ ,i], na.rm = TRUE)
     all_data_PRCTL.dtfr[4, i] = moments::kurtosis(all_rates.dtfr[ ,i], na.rm = TRUE)
     all_data_PRCTL.dtfr[1, i] = all_data_PRCTL.dtfr[2, i] > 0.05
     
     min.db = min(all_rates.dtfr[ ,i], na.rm = TRUE)
     max.db = max(all_rates.dtfr[ ,i], na.rm = TRUE)
     rng.db = max.db - min.db
     
     # This loops calculates the percentile of each observation based on the range of each stratum. This is used because a z-score statistic
          # assumes there is a normal distribution, which is not true for all strata. These values DO NOT suggest anything about the probability 
          # of the observation, they simple describe where it is relative to the extremes in their stratum. 
     # Originally, I was going to use linear models and use the residual of each observation as an indicator of it's "unusualness", but the 
          # observations at the beginning and ends of the vector would act as high leverage points, and if they were extreme, they would make
          # inferences about other observations less accurate. 
     for(q in 6:58){
          all_data_PRCTL.dtfr[q, i] = (all_rates.dtfr[q - 4, i] - min.db) / rng.db       
     }
}    

rm(min.db, max.db, rng.db, i , q)     
# RANK DEVIANCE ################################################################################################################################

# This block creates a table of standings within each stratum. 
all_rates_rankings.dtfr = all_rates.dtfr
for(i in 1:25){
     for(q in 1:54){
          if(is.na(all_rates.dtfr[q, i])){
               all_rates_rankings.dtfr[q, i] = NA
          }
          else{
               all_rates_rankings.dtfr[q, i] = sum(all_rates.dtfr[ ,i] < all_rates.dtfr[q, i], na.rm = TRUE) + 1
          }
     }
}

# Rank deviance is the difference between the rank that a state group lies in within it's stratum, and the rank of the state overall. For 
     # example, males in Colorado have the second lowest obesity rate for males, but Colorado has the lowest obesity rate out of any state. Thus,
     # the rank deviance for males in Colorado is (2 - 1 = 1), or one position above where it is expected to be given it's state. 
# There are also adjustments in the code to account for strata that have missing values. The mean of every column should be zero, because the 
     # expectation of a stratum's rank deviance is zero. 
     
rank_deviance.dtfr = all_rates_rankings.dtfr
for(i in 1:25){
     if(sum(is.na(all_rates_rankings.dtfr[ ,i])) == 0){
          for(q in 1:54){
               rank_deviance.dtfr[q, i] = all_rates_rankings.dtfr[q, i] - q
          }
     }
     else{
          for(q in 1:54){
               rank_deviance.dtfr[q, i] = all_rates_rankings.dtfr[q, i] - (q - sum(is.na(rank_deviance.dtfr[1:q, i])))
          }
     }
}

# Here, the rank deviance dtfr is turned into a vector and it's distribution is analysed. 
rank_deviance.vec = NULL
for(i in 1:25){ 
     rank_deviance.vec = c(rank_deviance.vec, rank_deviance.dtfr[ ,i])     
}

moments::kurtosis(rank_deviance.vec, na.rm = TRUE) # 8.290
moments::skewness(rank_deviance.vec, na.rm = TRUE) # -0.464
# Unfortunately, while it is reasonably symmetrical, it's not normal. The following section estimates percentiles by using the proportion of 
     # values below a certain point. 
# To start this process, we need:
     rank_deviance.vec = abs(rank_deviance.vec)                            # The absolute value of every rank deviance. 
     percentiles.vec = c(0.10, 0.50, 0.70, 0.80, 0.85, 0.90, 0.95, 0.99)   # The percentiles we want to find. 
     end.int = max(rank_deviance.vec, na.rm = TRUE)                        # The greatest rank deviance in the data. 
     len.int = length(rank_deviance.vec) - sum(is.na(rank_deviance.vec))   # The total number of observations. 

# This matrix shows the percentage of observations that lie below the value in the first column. 
PRCNT_ints.mat = matrix(ncol = 2, nrow = end.int)
PRCNT_ints.mat[ ,1] = c(1:end.int)

for(i in 1:end.int){
     PRCNT_ints.mat[i, 2]  = sum(rank_deviance.vec < i, na.rm = TRUE) / len.int
}
rm(end.int, len.int, rank_deviance.vec)

percentiles.vec = c(0.10, 0.50, 0.70, 0.80, 0.85, 0.90, 0.95, 0.99)
for(i in 1:length(percentiles.vec)){
     percentiles.vec[i] = PRCNT_ints.mat[ ,2] %near% percentiles.vec[i]
}
# The %near% operator finds the closest integer value that satisfies the percentile. For example, 10 is the closest integer to the exact 80th
     # percentile. It's important to consider that some degree of imprecision occurs because we are applying concepts generally used on 
     # continuous variables to a discrete variable. 
percentiles.dtfr = cbind(percentiles.vec, c(0.10, 0.50, 0.70, 0.80, 0.85, 0.90, 0.95, 0.99))


# WEIGHTED RANK DEVIANCE ########################################################################################################################
# The correlation between rank and obesity rank is essential for using rank deviance as a guide. If a vector has a low correlation value, then 
     # lots of extreme rank deviance values won't mean much because there isn't much of a trend to be an outlier on. 

stratum_CORR.vec = vector(length = 25)
for(i in 1:25){
     stratum_CORR.vec[i] = cor(all_rates.dtfr[ ,i], 1:54, use = 'complete.obs')
}

# This new data frame weights the rank deviance values by the correlation for each stratum. Thus, a stratum with a an extreme rank deviance but
     # weak correlation won't distract from the behavior of strata in highly correlated categories. 
weighted_RKDEV.dtfr = rank_deviance.dtfr
for(i in 1:25){
     weighted_RKDEV.dtfr[ ,i] = weighted_RKDEV.dtfr[ ,i] * stratum_CORR.vec[i]
}

long_weighted_RKDEV.dtfr = as.data.frame(matrix(ncol = 4, nrow = 1350))
     names(long_weighted_RKDEV.dtfr) = c("State", "Group", "Weighted RKDEV", "Obesity Rate")
     
for(i in 1:25){
     for(q in 1:54){
          long_weighted_RKDEV.dtfr[(((i - 1) * 54) + q), 1] = row.names(weighted_RKDEV.dtfr)[q]
          long_weighted_RKDEV.dtfr[(((i - 1) * 54) + q), 2] = names(weighted_RKDEV.dtfr)[i]
          long_weighted_RKDEV.dtfr[(((i - 1) * 54) + q), 3] = weighted_RKDEV.dtfr[q, i]
          long_weighted_RKDEV.dtfr[(((i - 1) * 54) + q), 4] = all_rates.dtfr[q ,i]
     }
}

stratum_names.vec = names(weighted_RKDEV.dtfr)
cat_vals.list = list(stratum_names.vec[1:2],
                      stratum_names.vec[3:6],
                      stratum_names.vec[7:12],
                      stratum_names.vec[13:18],
                      stratum_names.vec[19:25]
               )
cat_names.vec = c("sex", "edu", "age", "inc", "rce")
rm(stratum_names.vec)

long_weighted_RKDEV.dtfr = cbind(long_weighted_RKDEV.dtfr, vector(length = 1350))
for(i in 1:1350){
     for(q in 1:5){
          if(long_weighted_RKDEV.dtfr$Group[i] %in% cat_vals.list[[q]]){
               long_weighted_RKDEV.dtfr[i, 5] = cat_names.vec[q]
          }
     }
}
names(long_weighted_RKDEV.dtfr)[5] = "Stratum"
all_RKDEV.list = list(
     sex_RKDEV.dtfr = extractSubset(dtfr = long_weighted_RKDEV.dtfr, primary_select.s = 'Stratum', criteria.s = 'sex'),
     edu_RKDEV.dtfr = extractSubset(dtfr = long_weighted_RKDEV.dtfr, primary_select.s = 'Stratum', criteria.s = 'edu'),
     age_RKDEV.dtfr = extractSubset(dtfr = long_weighted_RKDEV.dtfr, primary_select.s = 'Stratum', criteria.s = 'age'),
     inc_RKDEV.dtfr = extractSubset(dtfr = long_weighted_RKDEV.dtfr, primary_select.s = 'Stratum', criteria.s = 'inc'),
     rce_RKDEV.dtfr = extractSubset(dtfr = long_weighted_RKDEV.dtfr, primary_select.s = 'Stratum', criteria.s = 'rce')
)

plot(NULL, xlim = c(0, 54), ylim = c(-4, 4))
test = vector(length = 54)
for(i in 1:54){
     test[i] = mean(unlist(weighted_RKDEV.dtfr[i, ]), na.rm = TRUE)
}
points(test, cex = .6)
summary(lm(test ~ c(1:54)))
cor(test, c(1:54))

rm(cat_names.vec, test)
# RESULT -  There is a significant trend between average ranking and position. This makes sense if we consider the possible values the weighted
     # rank deviation scores can have. 
# COMMENT - Correlation helps account for dispersion in the rank deviations. If a vector of rank deviations has no trend (the ends don't trend) 
     # any specific direction) then the values are lower and the weighted score is reduced. 

for(i in 1:5){
     print(moments::kurtosis(all_RKDEV.list[[i]][ ,3], na.rm = TRUE))
}
for(i in 1:5){
     print(moments::skewness(all_RKDEV.list[[i]][ ,3], na.rm = TRUE))
}
# COMMENT - None of the collected rank deviance vectors look particularly normal. I'm not convinced fitting them to a distribution would be 
     # particularly useful any, as either way the most interesting information will come from the top / bottom 10. That said, I thinking tagging
     # the state mean and stratum mean onto each observation would provide much needed context. 

# CONTEXT TAGGING ###############################################################################################################################

state_rates.dtfr = as.data.frame(state_RR.dtfr[ ,1])
     row.names(state_rates.dtfr) = row.names(state_RR.dtfr)

strat_rates.dtfr = as.data.frame(all_RR.dtfr[ ,1])
     row.names(strat_rates.dtfr) = row.names(all_RR.dtfr)
     strat_rates.dtfr = rbind(strat_rates.dtfr, (all_TABLE.dtfr[1, 1] / (all_TABLE.dtfr[1, 1] + all_TABLE.dtfr[1, 2])))
     strat_rates.dtfr = rbind(strat_rates.dtfr, (all_TABLE.dtfr[2, 1] / (all_TABLE.dtfr[2, 1] + all_TABLE.dtfr[2, 2])))
     row.names(strat_rates.dtfr)[length(row.names(strat_rates.dtfr))] = "Female"
     row.names(strat_rates.dtfr)[length(row.names(strat_rates.dtfr)) - 1] = "Male"

# "I'll relabel things so that the names look cleaner", I said, not thinking that it may cause me a headache weeks later and require this code 
     # below to fix it. 
for(i in 1:nrow(all_RKDEV.list$rce_RKDEV.dtfr)){
     if(all_RKDEV.list$rce_RKDEV.dtfr[i, 2] == "Non-Hispanic White"){
          all_RKDEV.list$rce_RKDEV.dtfr[i, 2] = "White"
     }
     else if(all_RKDEV.list$rce_RKDEV.dtfr[i, 2] == "Non-Hispanic Black"){
          all_RKDEV.list$rce_RKDEV.dtfr[i, 2] = "Black"
     }
     else if(all_RKDEV.list$rce_RKDEV.dtfr[i, 2] == "American Indian/Alaska Native"){
          all_RKDEV.list$rce_RKDEV.dtfr[i, 2] = "Native"
     }
     else if(all_RKDEV.list$rce_RKDEV.dtfr[i, 2] == "Hawaiian/Pacific Islander"){
          all_RKDEV.list$rce_RKDEV.dtfr[i, 2] = "Hawaiian or Pacific Islander"
     }
     else{}
}

for(i in 1:5){
     hold.dtfr = all_RKDEV.list[[i]]                       
     state_rates.vec = vector(length = nrow(hold.dtfr))     
     strat_rates.vec = state_rates.vec                      
     hold.dtfr = cbind(hold.dtfr, state_rates.vec, strat_rates.vec)
     
     for(q in 1:nrow(hold.dtfr)){
          hold.dtfr[q, 6] = state_rates.dtfr[row.names(state_rates.dtfr) == hold.dtfr[q, 1], 1]
          hold.dtfr[q, 7] = strat_rates.dtfr[row.names(strat_rates.dtfr) == hold.dtfr[q, 2], 1]
     }
     all_RKDEV.list[[i]] = hold.dtfr
}

#  Now there is a series of data frames with each sub-stratum, their state's obesity rate, and their main stratum's obesity rate. 

# DIFFERENCE NORMALITY ##########################################################################################################################
# The presentation spreadsheet has two columns for the difference between the substratum, stratum, and state obesity rate. This leads to the 
     # following question: Are these differences normally distributed, so that we can pair a z-score with the rank deviation to better describe
     # each substratum in the context of their stratum and state? 
for(i in 1:5){
     print(nrow(all_RKDEV.list[[i]]))
}
# State Difference
for(i in 1:5){
     print(shapiro.test(all_RKDEV.list[[i]][ ,4] - all_RKDEV.list[[i]][ ,6]))
     print(moments::skewness((all_RKDEV.list[[i]][ ,4] - all_RKDEV.list[[i]][ ,6]), na.rm = TRUE))
     print(moments::kurtosis((all_RKDEV.list[[i]][ ,4] - all_RKDEV.list[[i]][ ,6]), na.rm = TRUE))
}
# RESULT - None of the vector were normal. They all had a high degree of skew. 

# Stratum Difference
for(i in 1:5){
     print(shapiro.test(all_RKDEV.list[[i]][ ,4] - all_RKDEV.list[[i]][ ,7]))
     print(moments::skewness((all_RKDEV.list[[i]][ ,4] - all_RKDEV.list[[i]][ ,7]), na.rm = TRUE))
     print(moments::kurtosis((all_RKDEV.list[[i]][ ,4] - all_RKDEV.list[[i]][ ,7]), na.rm = TRUE))
}
# RESULT - 3/5 of the state differences were normal.
# I think it may be best to put a pin in this idea for now, given that 3/10 of the needed vectors are non-normal. 

# MISC ##########################################################################################################################################
# This is a quick summary of all   

# MEMORY CLEANING ###############################################################################################################################

rm(i, j, STATE_ABB.vec, hold.dtfr, hold_2.dtfr, pops.vec, rates.vec, STRATA.vec, stratum_CORR.vec, percentiles.dtfr, percentiles.vec)


# SKEWNESS: 
# (-0.5, 0.5) - low or approximately symmetric.
# (-1, -0.5) U (0.5, 1) - moderately skewed.
# Beyond -1 and 1 - Highly skewed
# KURTOSIS
# Mesokurtic distribution (kurtosis = 3, excess kurtosis = 0): perfect normal distribution or very close to it.
# Leptokurtic distribution (kurtosis > 3, excess kurtosis > 0): sharp peak, heavy tails
# Platykurtic distribution (kurtosis < 3, excess kurtosis < 0): flat peak, light tails
