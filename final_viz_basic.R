# INCLUDES: all_RR.dtfr, recycling_bin.dtfr, state_RR.dtfr

# BY STRATIFICATION CRITERIA ####################################################################################################################

row.names(recycling_bin.dtfr) = c(1:nrow(recycling_bin.dtfr))

wgtd_MEANS.vec = c(NA,                            # GENDER
                   recycling_bin.dtfr[18, 3],     # EDUCATION
                   recycling_bin.dtfr[49, 3],     # AGE
                   recycling_bin.dtfr[68, 3],     # INCOME
                   recycling_bin.dtfr[99, 3]      # RACE/ETHNICITY
                   )
wgtd_MEANS.vec[1] = ((recycling_bin.dtfr[2, 3] * recycling_bin.dtfr[4, 3]) + 
                    (recycling_bin.dtfr[3, 3] * recycling_bin.dtfr[5, 3])) / recycling_bin.dtfr[10, 3]

rates.list = list(c(recycling_bin.dtfr[2, 3], recycling_bin.dtfr[3, 3]),   # GENDER
                  c(all_RR.dtfr[1:4,   1]),  # EDUCATION
                  c(all_RR.dtfr[5:10,  1]),  # AGE
                  c(all_RR.dtfr[11:16, 1]),  # INCOME
                  c(all_RR.dtfr[17:23, 1]))  # RACE/ETHNICITY

point_labs.list = list(c("Male", "Female"),
                       row.names(all_RR.dtfr)[1:4],
                       row.names(all_RR.dtfr)[5:10],
                       row.names(all_RR.dtfr)[11:16],
                       row.names(all_RR.dtfr)[17:23]
                       )
     point_labs.list[[4]] =  c("Less than $15,000", "$15,000 - $24,999", "$25,000 \n- $34,999", 
                               "$35,000 \n- $49,999", "$50,000 \n- $74,999", "$75,000 +")
     point_labs.list[[5]][7] = "Hawaiian or \nPacific \nIslander"

plot_mains.vec = c("Gender Strata Obesity Rate", "Education Strata Obesity Rate", "Age Strata Obesity Rate", "Income Strata Obesity Rate",
                   "Race/Ethnicity Strata Obesity Rate")

for(i in 2:5){
     plot(rates.list[[i]], ylab = "Rate", xlab = "Stratum", main = plot_mains.vec[i], cex = .8, col = 'blue', 
               xlim = c(1, length(rates.list[[i]]) + 1.5), ylim = c(min(rates.list[[i]]) - 0.025, max(rates.list[[i]]) + 0.025))
          
          lines(rep(wgtd_MEANS.vec[i], length(rates.list[[i]]) + 2), lwd = 1.5, col = 'darkblue')
          text(rates.list[[i]], point_labs.list[[i]], pos = 4, cex = .8)
}

strata_ranges.dtfr = as.data.frame(matrix(ncol = 4, nrow = 5))
     names(strata_ranges.dtfr) = c("Highest", "Rate", "Lowest", "Rate")

for(i in 1:5){
     strata_ranges.dtfr[i, 2] = max(rates.list[[i]])
     strata_ranges.dtfr[i, 1] = point_labs.list[[i]][rates.list[[i]] == max(rates.list[[i]])]
     
     strata_ranges.dtfr[i, 4] = min(rates.list[[i]])
     strata_ranges.dtfr[i, 3] = point_labs.list[[i]][rates.list[[i]] == min(rates.list[[i]])]
}
plot(NULL, ylim = c(0.0, 0.5), xlim = c(.7, 5.5), main = "Highest/Lowest Risk Groups", xlab = "Group", ylab = "Rate")
     points(strata_ranges.dtfr[ ,2], cex = .8, col = 'red')
     text(strata_ranges.dtfr[ ,2], strata_ranges.dtfr[ ,1], cex = .8, pos = 3, col = 'darkred')
     points(strata_ranges.dtfr[ ,4], cex = .8, col = 'blue')
     text(strata_ranges.dtfr[ ,4], strata_ranges.dtfr[ ,3], cex = .8, pos = 1, col = 'darkblue')

# BY STATE ######################################################################################################################################

state_ranges.dtfr = as.data.frame(matrix(ncol = 4, nrow = 5))
     names(state_ranges.dtfr) = names(strata_ranges.dtfr)

state_ranges.dtfr[ ,1] = row.names(state_RR.dtfr)[50:54]
state_ranges.dtfr[ ,2] = state_RR.dtfr[50:54, 1]
state_ranges.dtfr[ ,3] = row.names(state_RR.dtfr)[1:5]
state_ranges.dtfr[ ,4] = state_RR.dtfr[1:5, 1]

plot(NULL, ylim = c(0.15, 0.4), xlim = c(1, 5), main = "Highest/Lowest Risk States", xlab = "State", ylab = "Rate")
     points(state_ranges.dtfr[ ,2], cex = .8, col = 'red')
     text(state_ranges.dtfr[ ,2], state_ranges.dtfr[ ,1], cex = .8, pos = 3, col = 'darkred')
     points(state_ranges.dtfr[ ,4], cex = .8, col = 'blue')
     text(state_ranges.dtfr[ ,4], state_ranges.dtfr[ ,3], cex = .8, pos = 1, col = 'darkblue')
     lines(rep(mean(state_RR.dtfr[ ,1], na.rm = TRUE), 5), lwd = 1.5)

# MEMORY CLEANING ###############################################################################################################################

# NOT : wght_MEANS.vec     
rm(point_labs.list)



