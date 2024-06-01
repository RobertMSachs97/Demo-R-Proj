# INTRODUCTION ################################################################################################################################## 
     # - This modeling process will be continues in income_model_experiment.R. 
     # - The purpose of this script is to understand the general behavior of the data. This means that there is no testing/training split. 
     # - The end goal is NOT to create one unified model with parameters that apply to every state, but rather to create a general model that can
          # be applied to every state. This means that every state will have it's own regression coefficients that can be plugged into a general 
          # formula that can be applied (with significance) to every state. 
     # - All of the parameters and models made in this script get deleted at the end, because it was unclear where I was going with it. In 
          # income_model_formal.R, I approach the problem from a different angle. 

# INITIAL MODELING ##############################################################################################################################

ordinals.dtfr = as.data.frame(matrix(ncol = 4, nrow = 6))
     names(ordinals.dtfr) = c("Rank", "Education", "Age", "Income")

ordinals.dtfr[ ,1] = c(1:6)
ordinals.dtfr[ ,2] = c(all_RR.dtfr[1:4, 1], NA, NA)
ordinals.dtfr[ ,3] = c(all_RR.dtfr[5:10, 1])
ordinals.dtfr[ ,4] = c(all_RR.dtfr[11:16, 1])

ordinal_SLR_COEFS.dtfr = rbind(
     coef(summary(glm(ordinals.dtfr$Education ~ ordinals.dtfr$Rank, family = 'gaussian')))[2, ],
     coef(summary(glm(ordinals.dtfr$Age       ~ ordinals.dtfr$Rank, family = 'gaussian')))[2, ],
     coef(summary(glm(ordinals.dtfr$Income    ~ ordinals.dtfr$Rank, family = 'gaussian')))[2, ]
)

plot(glm(ordinals.dtfr$Age ~ ordinals.dtfr$Rank, family = 'gaussian')$residuals, col = 'darkblue', 
          main = "SLR Model Residuals", ylab = "Residual", type = 'l')
     lines(glm(ordinals.dtfr$Education ~ ordinals.dtfr$Rank, family = 'gaussian')$residuals, col = 'darkgreen')
     lines(glm(ordinals.dtfr$Income ~ ordinals.dtfr$Rank, family = 'gaussian')$residuals, col = 'darkred')
     lines(rep(0, 6), lwd = 2)

# COMMENTS - This is more or less what I expected. The only variable to stay under alpha = .05 was income, which is the only line (red) on the 
     # residuals graph that stays close to zero. Overall it seems to be a good predictor of obesity, though the rate
     # of change isn't very high. 
     # Education shows the sharp change between college and non-college graduates. I don't don't think it would be hard to make a model using
     # only the pre-college education levels, but the change pre-college is only around 3%, and there are only three levels. The change between
     # "Some college or technical school" and "College graduate" is much larger at 8%. I could see treating education as a "college / no college" 
     # binary response. 
     # I think age might be the most interesting one. From young adulthood into middle ages, the model goes from largely undershooting the 
     # obesity rate to overshooting by nearly as much. The reverse happens as the model goes from the middle age groups towards the seniors. 
     # It's clear that a different link function should be used to represent the "arch" shape shown by the age variable. 

# SUMMARY - SLR works for income. There aren't very many education strata to use and the large jump into the college stratum means it may be 
     # simpler to model college/non-college strata separately. Age does not follow a straight trend, but it may be worth trying a different
     # link function. 

# INCOME SLR BY STATE ###########################################################################################################################
# COMMENTS - This block builds an SLR model for each state where the response variable is the obesity rate, and the explanatory variable is the 
     # income class. SLR was used over logistic regression for two reasons: 
          # 1 - R logistic regression function require every observation to have a TRUE/FALSE value for the response variable, so simulated data 
               # would have to be created in order to apply the functions. 
          # 2 - The change in obesity rate is simply not great enough that there is a risk of the output containing probability values outside
               # of the interval [0, 1]. 
     # This means that much of the following code evaluates whether an identity link is sufficient for each state. 
     
state_income_SLR_RSD.dtfr = as.data.frame(matrix(ncol = 54, nrow = 6))
     row.names(state_income_SLR_RSD.dtfr) = unique(sorted_rates_edited.dtfr$Income_Group)[2:7]
     names(state_income_SLR_RSD.dtfr) = row.names(all_RR.dtfr)[24:77]
state_income_SLR_PARAMS.dtfr = as.data.frame(matrix(nrow = 54, ncol = 7))
     row.names(state_income_SLR_PARAMS.dtfr) = names(state_income_SLR_RSD.dtfr)
     names(state_income_SLR_PARAMS.dtfr) = c("Intercept",        # Intercept of the states income SLR model.
                                             "Income_Effect",    # Slope of the states SLR model. 
                                             "Effect_pval",      # Boolean for whether the SLR models meets alpha < .05.  
                                             "AIC",              # AIC used for comparison and identifying states with abnormal income effect. 
                                             "RSD_Beta_pval",    # p value for an SLR trend in the residuals.  
                                             "RSD_shapiro.test", # p value for a shapiro test on the residuals.  
                                             "RSD_Run_GT3")      # Boolean for whether the residual vector has a run over or below 0 >= 3
# COMMENT - The slope p-value and shapiro test p-value for the residuals is being left as a number instead of a significance pass/fail because
     # I don't have any expectations for behavior, and want to be able to observe it directly. 

STATE_ABB.vec = row.names(all_RR.dtfr)[24:77]
STRATUM_NAMES.vec =  unique(sorted_rates_edited.dtfr$Income_Group)[2:7]

for(i in 1:54){
     hold.dtfr = extractSubset(dtfr = sorted_rates_edited.dtfr, primary_select.s = 'Location', criteria.s = STATE_ABB.vec[i])
     hold.dtfr = extractSubset(dtfr = hold.dtfr, primary_select.s = 'Strat_criteria', criteria.s = 'Income')
     strata_MU.vec = vector(length = 6)
     for(q in 1:6){
          strata_MU.vec[q] = weightedProp(dtfr = hold.dtfr, primary_select.s = 'Stratum', criteria.s = STRATUM_NAMES.vec[q])
     }
     
     state_LM.cpx = glm(strata_MU.vec ~ c(1:6))
     state_income_SLR_PARAMS.dtfr[i, 1] = state_LM.cpx$coefficients[1]
     state_income_SLR_PARAMS.dtfr[i, 2] = state_LM.cpx$coefficients[2]
     state_income_SLR_PARAMS.dtfr[i, 3] = summary(state_LM.cpx)$coefficients[8] < .05 
     state_income_SLR_PARAMS.dtfr[i ,4] = state_LM.cpx$aic
     
     state_income_SLR_RSD.dtfr[ ,i] = state_LM.cpx$residuals
}

for(i in 1:54){
     select_col = state_income_SLR_RSD.dtfr[ ,i]
     col_LM.cpx = glm(select_col ~ c(1:6))
     
     state_income_SLR_PARAMS.dtfr[i, 5] = summary(col_LM.cpx)$coefficients[8]
     state_income_SLR_PARAMS.dtfr[i, 6] = shapiro.test(select_col)$p.value
     
     count = 0L
     prev_val = 0.0
     state_income_SLR_PARAMS.dtfr[i, 7] = FALSE
     
     for(q in 1:6){
          if(XNOR(prev_val, state_income_SLR_RSD.dtfr[q, 1], 0)){
               count = count + 1
          }
          if(count == 2){
               state_income_SLR_PARAMS.dtfr[i, 7] == TRUE
               break
          }
          prev_val = state_income_SLR_RSD.dtfr[q, 1]
     }
}

# RESULTS: 
     # Only one model returned a beta one value greater than 0. 
     # 39 out of 54 strata had significant models. 
     # The model with the lowest AIC is MO at -42.72736.
     # The model with the lowest Shapiro-Wilks test p-value was SD, which was surprising because it's effect parameter passed alpha < .05. The   <- Typo?
          # p-value does not pass alpha < .05, but it does pass alpha < .10. I think its worth treating this stratum and any others that pass 
          # alpha < .10 in Shapiro-Wilks separately from the models that do not. 
     # I was surprised that every model passed the runs test. This may be a function of the small data size, but it does reduce my concerns 
          # about "bow_shaped" trends. 

rm(count, prev_val, select_col, q, i, strata_MU.vec, state_LM.cpx, col_LM.cpx, hold.dtfr)
# INCOME MODEL RESIDUAL ANALYSIS ################################################################################################################
# VISUALIZATION

plot(x = 1, y = 1, col = 'white', main = "All Residuals", xlim = c(1, 6), ylim = c(-.1, .1), xlab = "Income Group Index", 
     ylab = "Residual Value (%)")
for(i in 1:54){
     lines(state_income_SLR_RSD.dtfr[ ,i], lwd = .2)
}
# RESULT - There is a noticeable "hump" pattern here. Things to do:
     # 1 - Run the non-significant models residuals in one color, and the Shapiro-Wilks p-value < .10 residuals in another, and see how they 
          # appear on the plot. 
     # 2 - Test all the residuals from the models that passed both tests, then evaluate the residuals form the non-compliant models. 

sbst.dtfr = state_income_SLR_RSD.dtfr[ ,state_income_SLR_PARAMS.dtfr$Effect_pval != TRUE]
for(i in 1:ncol(sbst.dtfr)){
     lines(sbst.dtfr[ ,i], col = 'red', lwd = .3)
}
rm(sbst.dtfr)

lines(state_income_SLR_RSD.dtfr[ ,state_income_SLR_PARAMS.dtfr$RSD_shapiro.test <= .10], col = 'blue', lwd = .5)
# RESULT - Only one model actually met this condition.  

RSD_PARAMS.dtfr = as.data.frame(matrix(ncol = 5, nrow = 6))
     names(RSD_PARAMS.dtfr) = c("Iteration Mean", "Iteration Variance", "Shapiro-Wilks p-value", "Skewness", "Zero Location")
     row.names(RSD_PARAMS.dtfr) = STRATUM_NAMES.vec

for(i in 1:6){
     iter.vec = as.vector(unlist(state_income_SLR_RSD.dtfr[i, ]))
     RSD_PARAMS.dtfr[i, 1] = mean(iter.vec)
     RSD_PARAMS.dtfr[i, 2] = var(iter.vec)
     RSD_PARAMS.dtfr[i, 3] = shapiro.test(iter.vec)$p.value
     RSD_PARAMS.dtfr[i, 4] = moments::skewness(iter.vec)
     RSD_PARAMS.dtfr[i, 5] = RSD_PARAMS.dtfr[i, 1] / sqrt(RSD_PARAMS.dtfr[i, 2])
}

# RESULT - This confirms that there are issues with the SLR model. 3/6 groups have skewness values over 1, and 4/6 reject normality in a 
     # Shapiro-Wilks test. 
# COMMENT - The following code runs all of the same test, but only on the models with significant regression coefficients. 

sig_RSD_PARAMS.dtfr = RSD_PARAMS.dtfr
for(i in 1:6){
     iter.vec = as.vector(unlist(state_income_SLR_RSD.dtfr[i, state_income_SLR_PARAMS.dtfr$Effect_pval]))
     sig_RSD_PARAMS.dtfr[i, 1] = mean(iter.vec)
     sig_RSD_PARAMS.dtfr[i, 2] = var(iter.vec)
     sig_RSD_PARAMS.dtfr[i, 3] = shapiro.test(iter.vec)$p.value
     sig_RSD_PARAMS.dtfr[i, 4] = moments::skewness(iter.vec)
     sig_RSD_PARAMS.dtfr[i, 5] = sig_RSD_PARAMS.dtfr[i, 1] / sqrt(sig_RSD_PARAMS.dtfr[i, 2])
}

# RESULT - There is a significant improvement. None of the iterations can definitively be called "not normal". However, it is still visually 
     # clear that the residuals are skewed on either side of 0 in at least the last 3 iterations. This is supported by the z-score of zero 
     # increasing in this subset. 

# MODIFIED END INCOME SLR #######################################################################################################################
# PROBLEM - The residuals are often non-normal and have a clear trend. 
     #    - There is also a subset of data that appears to have higher that does not fit am SLR model at all. 
# SOLUTION - Record which states had significant models and which did not. 
     #    - Run new models that only apply to groups 1:5, and observe changes in parameters and residuals. 
     #    - Run 80/20 training/testing split on the jump from group 5 to 6. The variance around the jump will need to be observed in order to
     #         be congruent with the rest of the model. In summary Y6 = Y5 + avg_change +/- var(avg_change)

state_income_MDLR_PARAMS.dtfr = state_income_SLR_PARAMS.dtfr
state_income_MDLR_RSD.dtfr = state_income_SLR_RSD.dtfr[1:5, ]


for(i in 1:54){
     hold.dtfr = extractSubset(dtfr = sorted_rates_edited.dtfr, primary_select.s = 'Location', criteria.s = STATE_ABB.vec[i])
     hold.dtfr = extractSubset(dtfr = hold.dtfr, primary_select.s = 'Strat_criteria', criteria.s = 'Income')
     strata_MU.vec = vector(length = 5)
     for(q in 1:5){
          strata_MU.vec[q] = weightedProp(dtfr = hold.dtfr, primary_select.s = 'Stratum', criteria.s = STRATUM_NAMES.vec[q])
     }
     
     state_LM.cpx = glm(strata_MU.vec ~ c(1:5))
     state_income_MDLR_PARAMS.dtfr[i, 1] = state_LM.cpx$coefficients[1]
     state_income_MDLR_PARAMS.dtfr[i, 2] = state_LM.cpx$coefficients[2]
     state_income_MDLR_PARAMS.dtfr[i, 3] = summary(state_LM.cpx)$coefficients[8] < .05 
     state_income_MDLR_PARAMS.dtfr[i ,4] = state_LM.cpx$aic
     
     state_income_MDLR_RSD.dtfr[ ,i] = state_LM.cpx$residuals
}

plot(x = 1, y = 1, col = 'white', main = "Index 1:5 Residuals", xlim = c(1, 5), ylim = c(-.1, .1), xlab = "Income Group Index", 
     ylab = "Residual Value (%)")
for(i in 1:54){
     lines(state_income_MDLR_RSD.dtfr[ ,i], lwd = .2)
}
# RESULTS - There appears to still be a more pronounce bow shape, even thought the hump at the end is gone. 

MDLR_RSD_PARAMS.dtfr = as.data.frame(matrix(ncol = 5, nrow = 5))
     names(MDLR_RSD_PARAMS.dtfr) = names(RSD_PARAMS.dtfr)
     row.names(MDLR_RSD_PARAMS.dtfr) = STRATUM_NAMES.vec[1:5]
     
for(i in 1:5){
     iter.vec = as.vector(unlist(state_income_MDLR_RSD.dtfr[i, ]))
     MDLR_RSD_PARAMS.dtfr[i, 1] = mean(iter.vec)
     MDLR_RSD_PARAMS.dtfr[i, 2] = var(iter.vec)
     MDLR_RSD_PARAMS.dtfr[i, 3] = shapiro.test(iter.vec)$p.value
     MDLR_RSD_PARAMS.dtfr[i, 4] = moments::skewness(iter.vec)
     MDLR_RSD_PARAMS.dtfr[i, 5] = MDLR_RSD_PARAMS.dtfr[i, 1] / sqrt(MDLR_RSD_PARAMS.dtfr[i, 2])
}
# RESULTS - There seems to be a noticeable improvement. Group 2 has a high skew value, but there is a pretty large outlier in it. It is also the
     # only group shown to reject normality in a Shapiro-Wilks test. 

# MEMORY CLEANING ###############################################################################################################################
rm(i, q, iter.vec, strata_MU.vec, state_LM.cpx, hold.dtfr)
rm(MDLR_RSD_PARAMS.dtfr, RSD_PARAMS.dtfr)
rm(state_income_SLR_RSD.dtfr, state_income_MDLR_RSD.dtfr)     
rm(sig_RSD_PARAMS.dtfr, RSD_PARAMS.dtfr)

