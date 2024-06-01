# DATA LOAD #####################################################################################################################################
state_income_MEANS.dtfr = as.data.frame(matrix(ncol = 54, nrow = 6))
     names(state_income_MEANS.dtfr) = STATE_ABB.vec
     row.names(state_income_MEANS.dtfr) = STRATUM_NAMES.vec
for(i in 1:54){
     hold.dtfr = extractSubset(dtfr = sorted_rates_edited.dtfr, primary_select.s = 'Location', criteria.s = STATE_ABB.vec[i])
     hold.dtfr = extractSubset(dtfr = hold.dtfr, primary_select.s = 'Strat_criteria', criteria.s = 'Income')
     
     for(q in 1:6){
          state_income_MEANS.dtfr[q, i] = weightedProp(dtfr = hold.dtfr, primary_select.s = 'Stratum', criteria.s = STRATUM_NAMES.vec[q])
     }
}

# VISUALIZATION
plot(NULL, xlim = c(1, 6), ylim = c(0, .5), main = "Obesity Rates by State & Income")
for(i in 1:54){
     lines(state_income_MEANS.dtfr[ ,i], lwd = .3)
}
means.vec = vector(length = 6) # <- This is the mean of the responses, not the mean of the population. 
for(i in 1:6){
     means.vec[i] = mean(unlist(state_income_MEANS.dtfr[i, ]))
}
lines(means.vec, col = 'red', lwd = 2)
rm(means.vec)

diffs.mat = matrix(ncol = 54, nrow = 5)
for(j in 1:54){
     for(i in 1:5){
          diffs.mat[i, j] = state_income_MEANS.dtfr[i + 1, j] - state_income_MEANS.dtfr[i, j]     
     }
}
rm(j)

plot(NULL, xlim = c(1, 5), ylim = c(-.1, .1))
for(i in 1:54){
     lines(diffs.mat[ ,i], lwd = .5, col = 'red')
}

# EXPECTED CHANGE MODEL #########################################################################################################################

# Rather than calculate E() and V() for all training data, put EVERYTHING into the model, and evaluate a states expected change at an index in 
     # terms of what the distribution would be like if it wasn't in it. 

general_parameters.dtfr = as.data.frame(matrix(ncol = 2, nrow = 5))
     names(general_parameters.dtfr) = c("Expected_Change", "Change_Variance")
for(i in 1:5){
     general_parameters.dtfr[i, 1] = mean(unlist(diffs.mat[i, ]))
     general_parameters.dtfr[i, 2] = var(unlist(diffs.mat[i, ]))
}
diff_scores.dtfr = as.data.frame(matrix(ncol = 54, nrow = 5))
     names(diff_scores.dtfr) = names(state_income_MEANS.dtfr)
for(j in 1:54){
     for(i in 1:5){
          non_strat_EXP = mean(unlist(diffs.mat[i, -j]))
          non_strat_VAR = var(unlist(diffs.mat[i,  -j]))
          diff_scores.dtfr[i, j] = (diffs.mat[i, j] - non_strat_EXP) / sqrt(non_strat_VAR)
     }
}     
diffs.dtfr = as.data.frame(diffs.mat)
     names(diffs.dtfr) = names(state_income_MEANS.dtfr)
     row.names(diffs.dtfr) = c("Increasing to $15k-$24,999", "Increasing to $25k-$34,999", "Increasing to $35k-$49,999", 
                               "Increasing to $50k-$74,999", "Increasing to $75k+")
row.names(diff_scores.dtfr) = row.names(diffs.dtfr)
rm(non_strat_EXP, non_strat_VAR)

# STATE SPECIFIC REGRESSION #####################################################################################################################
# COMMENT - This is a retake on the state regression model, with the expected change value to use as context. 

models.list = vector('list', 54)

for(i in 1:54){
     # Establish model objects.
     line.cpx = summary(lm(state_income_MEANS.dtfr[1:5, i] ~ c(1:5)))
     expo.cpx = summary(lm(state_income_MEANS.dtfr[1:5, i] ~ exp(c(1:5))))
     loge.cpx = summary(lm(state_income_MEANS.dtfr[1:5, i] ~ log(c(1:5))))
     
     # This section creates a vector of booleans for whether each model as both coefficients pass alpha < .05. 
     p_criteria.vec = c(XNOR(line.cpx$coefficients[7], line.cpx$coefficients[8], .05), 
                        XNOR(expo.cpx$coefficients[7], expo.cpx$coefficients[8], .05), 
                        XNOR(loge.cpx$coefficients[7], loge.cpx$coefficients[8], .05))
     out.list = list(line.cpx, expo.cpx, loge.cpx)
     out.list = out.list[p_criteria.vec]
     
     # This vector holds the last predicted value of each model, and it is strained in the same way as the model list. 
     qualified_ends.vec = c(line.cpx$coefficients[1] + (line.cpx$coefficients[2] * 5),
                            expo.cpx$coefficients[1] + (expo.cpx$coefficients[2] * 5),
                            loge.cpx$coefficients[1] + (loge.cpx$coefficients[2] * 5))[p_criteria.vec]
     # The 5th value in the current stratum is selected, and the absolute value of the difference is taken. 
     qualified_ends.vec = abs(qualified_ends.vec - state_income_MEANS.dtfr[5, i])
     # Keeping the 5th value difference because the 5th-6th value gap is already being predicted, so a model that's closest to the real starting
     # point of the gap is best. 

     # This NA object is the object that will be returned to the master list of model objects. If no model has 2 significant coefficients, then
          # the object will stay null to signify this. 
     return_obj.cpx = NULL
     quali_len.int = length(out.list)
     
     if(quali_len.int == 1){  # Model is selected by default. 
          return_obj.cpx == qualified_objs.list[[1]]
     }
     if(quali_len.int == 2){  # If there are 2, the model with the smallest residual on the 5th value is selected. 
          qualified_objs.list = out.list[qualified_ends.vec == min(qualified_ends.vec)]
          return_obj.cpx = qualified_objs.list[[1]]
     }
     
     if(quali_len.int == 3){  # If there are 3 models, the first step is to eliminated the models with the lowest adjusted R-squared value. 
          obj_RSQR.vec = c(line.cpx$adj.r.squared, expo.cpx$adj.r.squared, loge.cpx$adj.r.squared)
          qualified_objs.list = out.list[obj_RSQR.vec != min(obj_RSQR.vec)] # Removes the model with the lowest R-squared. 
          qualified_ends.vec = qualified_ends.vec[obj_RSQR.vec != min(obj_RSQR.vec)]      
          
          # Removes the model with the greatest 5th absolute residual. 
          qualified_objs.list = qualified_objs.list[qualified_ends.vec != max(qualified_ends.vec)]
          return_obj.cpx = qualified_objs.list[[1]]
     }
     if(quali_len.int == 0){
          # Do nothing, the object stays NULL if no model has significant coefficients. 
     }
     if(i == 54 && is.null(return_obj.cpx)){
          return_obj.cpx = NA
     }
     models.list[[i]] = return_obj.cpx
     
}

names(models.list) = STATE_ABB.vec

rm(out.list, line.cpx, loge.cpx, expo.cpx, p_criteria.vec, qualified_ends.vec, qualified_objs.list, obj_RSQR.vec, quali_len.int, return_obj.cpx)


# RESIDUAL ANALYSIS ############################################################################################################################# 

index_nulls.vec = vector(length = 54)
for(i in 1:53){                                        # TRUE means inadequate model. FALSE means adequate. 
     index_nulls.vec[i] = is.null(models.list[[i]])
}
index_nulls.vec[54] = TRUE                             # Final model is manually set as it was saved as NA to keep the model list at the correct
collect_LMs.list = models.list[!index_nulls.vec]       # length. 
inad_LMs.list = models.list[index_nulls.vec]

# PROBLEM - The models that passed selection may still have the bend in the residuals that the test models had.  
# SOLUTION - Start by looking at the normalized residuals.  
     # - Then examine residuals visually to see if the bend effect appears. 
     # - If there is a bend, the normalized residuals will show how significant the miss actually is. 

STDZD_RSD.dtfr = as.data.frame(matrix(nrow = 5, ncol = sum(!index_nulls.vec)))
     names(STDZD_RSD.dtfr) = STATE_ABB.vec[!index_nulls.vec]

# VISUALIZATION
usable_count = ncol(STDZD_RSD.dtfr)

plot(NULL, xlim = c(1, 5), ylim = c(-0.05, .05), main = "Selected Model Residuals")
     lines(rep(0, 5), lwd = 1.5)
for(i in 1:usable_count){                       
     RSD.vec = collect_LMs.list[[i]]$residuals         
     lines(RSD.vec, lwd = .5, col = 'blue')
}                                                      # This looked better than I expected. 
     
for(i in 1:usable_count){
     RSD.vec = collect_LMs.list[[i]]$residuals
     RSD.vec = RSD.vec / sqrt(var(RSD.vec))
     STDZD_RSD.dtfr[ ,i] = RSD.vec
}
rm(RSD.vec)

plot(NULL, xlim = c(1, 5), ylim = c(-2, 2), main = "Normalized Residuals")
     lines(rep(0, 5), lwd = 1.5)
for(i in 1:usable_count){
     lines(STDZD_RSD.dtfr[ ,i], col = 'blue', lwd = .5)
}
run_counts.vec = vector(length = usable_count)
for(i in 1:usable_count){
     run.int = 1L
     for(q in 1:4){
          run.int = run.int + XNOR(STDZD_RSD.dtfr[q, i], STDZD_RSD.dtfr[q + 1, i], 0)
     }
     run_counts.vec[i] = run.int
}

for(i in 1:usable_count){
     if(run_counts.vec[i] == 1){
          col.s = 'blue'
     }
     else if(run_counts.vec[i] == 2){
          col.s = 'green'
     }
     else if(run_counts.vec[i] == 3){
          col.s = 'red'
     }
     lines(STDZD_RSD.dtfr[ ,i], col = col.s, lwd = 0.5)
}

rm(col.s, run.int, run_counts.vec, usable_count, q, i, index_nulls.vec)

# RESULT - I'm not impressed with this approach. 13 out of 24 models had a run of 3 residuals that were on one side of zero. 7 had 2, and only 
     # 4 alternated between positive and negative. If we allow for a run of 2 residuals, this means that only 11 out of 54 strata have usable
     # regression models. 

# COMMENT - The following code takes the mean response from each strata to be used as a reference point. 

# NOTE - The means being evaluated here are not weighted. 
avg_data_val.vec = vector(length = 6)
for(i in 1:6){
     avg_data_val.vec[i] = mean(unlist(state_income_MEANS.dtfr[i, ]))
}
lines(avg_data_val.vec, col = 'red', lwd = 1.5)

# CURVED REGRESSORS #############################################################################################################################
# COMMENT - The goal in the section is to reduce the number of models with a residual run of 3. The linear/log/exponential trial was conducted 
     # as if it were a "blind" process (no visual examination beforehand to suggest a trend). In the models that passed criteria, a consistent 
     # bend was found in the residuals. 
     # As shown above, this was not effective. In fact, the algorithm did not even select any non-linear regressors, as shown by the code below.
for(i in 1:length(models.list)){
     lm = models.list[[i]]$call
     print(lm)
}
rm(lm)

# COMMENT - The code below is a final attempt to make linear models for a relevant number of strata. This is done by introducing a subtle curve,
     # into the regressors. This curve is perfectly circular and it's radius is based on the change between the lowest and highest average
     # response. This is visualized below:

# NOTE - This function is created in this script instead of functions.R because it is only usable for the following code. 
residBendDetect <- function(RSD_in.vec){                                                                 ## RETURNS INTEGER
     bend.bool = XNOR(RSD_in.vec[2], RSD_in.vec[3], 0) &&                                                ## CALLS XNOR()
                 XNOR(RSD_in.vec[2], RSD_in.vec[4], 0)
     if(bend.bool){
          if(RSD_in.vec[3] > 0){
               return(1)
          }
          if(RSD_in.vec[3] < 0){
               return(-1)
          }
     }
     else{
          return(0)
     }
}

# NOTE - The p-value column in this data frame refers to the *slope* coefficient p-value. 
change_log.dtfr = as.data.frame(matrix(ncol = 8, nrow = 54))
     names(change_log.dtfr) = c("Original R-Squared", "Modified R-Squared", "Original p-Value", "Modified p-Value",
                                "Original Intercept", "Modified Intercept", "Original Slope", "Modified Slope")
     row.names(change_log.dtfr) = STATE_ABB.vec
models_2.list = vector('list', 54)

# This loop creates the linear models.      
for(i in 1:54){
     # Starting input data. 
     data_in.vec = state_income_MEANS.dtfr[1:5, i]
     null_LM.cpx = summary(lm(data_in.vec ~  c(1:5)))
     
     # Log Starting Values
     change_log.dtfr[i, 1] = null_LM.cpx$adj.r.squared
     change_log.dtfr[i, 3] = null_LM.cpx$coefficients[8]
     change_log.dtfr[i, 5] = null_LM.cpx$coefficients[1]
     change_log.dtfr[i, 7] = null_LM.cpx$coefficients[2]
     
     # Detect presence and direction of bend.
     bend.int = residBendDetect(null_LM.cpx$residuals)
     radius.db = 4.0
     prev_RSQR.db = 0.0
     new_LM.cpx = NULL
     
     if(bend.int == 1){
          # This loop applies a bend until the residuals are roughly alternating on each side of zero. 
          while(radius.db < 1000.0){ 
               RGSRS.vec = c(1:5) + curveVec(c(1:5), radius.db = radius.db, direction.int = bend.int)
               new_LM.cpx = summary(lm(data_in.vec ~ RGSRS.vec))
               if(residBendDetect(new_LM.cpx$residuals) == 0){
                    print("DONE 1")
                    break
               }
               else{
                    radius.db = radius.db + 1.0
               }
          }
          # This loop refines the model using adjusted r-squared rather than the sign of the residuals. Notice how it will not run if radius.db 
          # has hit the limiter. 
          while(radius.db < 1000.0){
               RGSRS.vec = c(1:5) + curveVec(c(1:5), radius.db = radius.db, direction.int = bend.int)
               new_LM.cpx = summary(lm(data_in.vec ~ RGSRS.vec))
               if(new_LM.cpx$adj.r.squared > prev_RSQR.db){
                    break
               }
               else{
                    radius.db = radius.db + 1
               }
          }
     }
     else if(bend.int == -1){
          while(radius.db < 1000.0){
               RGSRS.vec = c(1:5) + curveVec(c(1:5), radius.db = radius.db, direction.int = bend.int)
               new_LM.cpx = summary(lm(data_in.vec ~ RGSRS.vec))
               if(residBendDetect(new_LM.cpx$residuals) == 0){
                    print("DONE -1")
                    break
               }
               else{
                    radius.db = radius.db + 1.0
               }
          }
          while(radius.db < 1000.0){
               RGSRS.vec = c(1:5) + curveVec(c(1:5), radius.db = radius.db, direction.int = bend.int)
               new_LM.cpx = summary(lm(data_in.vec ~ RGSRS.vec))
               if(new_LM.cpx$adj.r.squared > prev_RSQR.db){
                    break
               }
               else{
                    radius.db = radius.db + 1
               }
          }
     }
     
     else if(bend.int == 0){ # What did I say?? There may be no bend!
     }
     
     if(!is.null(new_LM.cpx)){
          change_log.dtfr[i, 2] = new_LM.cpx$adj.r.squared  
          change_log.dtfr[i, 4] = new_LM.cpx$coefficients[8]
          change_log.dtfr[i, 6] = new_LM.cpx$coefficients[1]
          change_log.dtfr[i, 8] = new_LM.cpx$coefficients[2]
     }
     
     print(i)
     models_2.list[[i]] = new_LM.cpx
}
rm(data_in.vec, null_LM.cpx, new_LM.cpx, radius.db, prev_RSQR.db, bend.int, RGSRS.vec)

# RESULTS #######################################################################################################################################
# It didn't work. 
# Possible reasons:
     # - Incorrect transformation. Rather than try to get the regressor to mimic the explanatory variable, maybe it would have been better to    
          # apply a transformation to the data instead.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           
     # - The explanatory vector is too short. With longer data three residuals on one side of zero are insignificant, but if the input is only 
          # five values long it may appear as if a transformation id needed. 
     # - The data simply doesn't behave in a way that's easy to model with regression. 