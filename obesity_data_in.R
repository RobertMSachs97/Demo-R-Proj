# DATA IN #######################################################################################################################################

obesity_raw.csv = read.csv('/Users/robert/Downloads/obesity.csv')  
varSummary(obesity_raw.csv)
MAX_DATA_LEN = nrow(obesity_raw.csv)
     
# VARIABLE SELECTION ############################################################################################################################
# This section is used for selecting columns worth examining. Some will be omitted for reasons not covered in this section. Obesity Raw Data 
# Notes will have explanations. 

# QUESTION - Are YearStart and YearEnd duplicates?

sum(obesity_raw.csv$YearEnd - obesity_raw.csv$YearStart)
     # Sum is zero, these will be combined into one variable. 
Year = obesity_raw.csv$YearEnd
obesity.dtfr = as.data.frame(Year)
rm(Year)

# QUESTION - Do all abbreviations match their long form? 
     
     # PAIRS: (Question, QuestionID), (Class, ClassID), (Topic, TopicID), (StratificationCategory1, StratificationCategoryId1) ,
     #         (Stratification1, StratificationID1), (LocationAbbr, LocationDesc)

print(abbvChecker(obesity_raw.csv$LocationAbbr, obesity_raw.csv$LocationDesc))
print(abbvChecker(obesity_raw.csv$Stratification1, obesity_raw.csv$StratificationID1))
print(abbvChecker(obesity_raw.csv$StratificationCategory1, obesity_raw.csv$StratificationCategoryId1))
print(abbvChecker(obesity_raw.csv$Topic, obesity_raw.csv$TopicID))
print(abbvChecker(obesity_raw.csv$Class, obesity_raw.csv$ClassID))
print(abbvChecker(obesity_raw.csv$Question, obesity_raw.csv$QuestionID))
     # All are matches.
     # KEEP: LocationAbbr, Stratification1, StratificationCategory1, Question <- These were kept because their duplicates were vague 
     #    or redundant.
     # Topic and Class will be evaluated further for overlap. There is a chance they may be the same. 

Location       = obesity_raw.csv$LocationAbbr
Stratum        = obesity_raw.csv$Stratification1
Strat_criteria = obesity_raw.csv$StratificationCategory1
Question       = obesity_raw.csv$Question
obesity.dtfr   = cbind(obesity.dtfr, Location, Question, Strat_criteria, Stratum)
rm(Location, Stratum, Strat_criteria, Question)

# QUESTION - What is the difference between Topic and Class

print(abbvChecker(obesity_raw.csv$Topic, obesity_raw.csv$Class))
     # KEEP - Class. Topic, TopicID and ClassID will be condensed into it. 
Class = obesity_raw.csv$Class
obesity.dtfr = cbind(obesity.dtfr, Class)
rm(Class)

# QUESTION - Is there a difference between Data_Value and Data_Value_Alt? 

sum(abs(obesity_raw.csv$Data_Value - obesity_raw.csv$Data_Value_Alt), na.rm = TRUE)
     # KEEP - Data_Value. The two are identical. 
Data_Value = obesity_raw.csv$Data_Value
obesity.dtfr = cbind(obesity.dtfr, Data_Value)
rm(Data_Value)

# QUESTION - Is the '~' symbol always aligned with the "Data not available because sample size is insufficient." statement? 

mismatch.int = 0
for(i in 1:MAX_DATA_LEN){
     if(obesity_raw.csv$Data_Value_Footnote[i] == "" && obesity_raw.csv$Data_Value_Footnote_Symbol[i] != ""){
          mismatch.int = mismatch.int + 1
     }
     else if(obesity_raw.csv$Data_Value_Footnote[i] == "Data not available because sample size is insufficient." &&
             obesity_raw.csv$Data_Value_Footnote_Symbol[i] != "~" ){
          mismatch.int = mismatch.int + 1
     }
}
mismatch.int
     # KEEP - Data_Value_Footnote_Symbol. This will be renamed Valid_Sample_Size and it's "" value will equal TRUE, while "~" will be FALSE. 
Valid_Sample_Size = vector(length = MAX_DATA_LEN)
Valid_Sample_Size = obesity_raw.csv$Data_Value_Footnote_Symbol == ""
obesity.dtfr = cbind(obesity.dtfr, Valid_Sample_Size)
rm(Valid_Sample_Size, i, mismatch.int)

# QUESTION - Is High_Confidence_Limit always greater than Low_Confidence_Limit? (This is checking for data entry errors.)

sum(obesity_raw.csv$High_Confidence_Limit < obesity_raw.csv$Low_Confidence_Limit, na.rm = TRUE)
     # KEEP - High_Confidence_Limit and Low_Confidence_Limit.
High_Confidence_Limit = obesity_raw.csv$High_Confidence_Limit
Low_Confidence_Limit  = obesity_raw.csv$Low_Confidence_Limit
obesity.dtfr = cbind(obesity.dtfr, High_Confidence_Limit, Low_Confidence_Limit)
rm(High_Confidence_Limit, Low_Confidence_Limit)

# INCLUDE - Sample_Size, Age.years., Education level. , Gender, Income, Race.Ethnicity

Sample_Size    = obesity_raw.csv$Sample_Size
Age_Group      = obesity_raw.csv$Age.years.
Edu_Level      = obesity_raw.csv$Education
Gender         = obesity_raw.csv$Gender
Income_Group   = obesity_raw.csv$Income
Race_Ethn      = obesity_raw.csv$Race.Ethnicity
obesity.dtfr   = cbind(obesity.dtfr, Sample_Size, Age_Group, Edu_Level, Gender, Income_Group, Race_Ethn)
rm(Sample_Size, Age_Group, Edu_Level, Gender, Income_Group, Race_Ethn)

# INCLUDE - GeoLocation as Lat_Cord and Long_Cord

Lat_Cord  = vector(length = MAX_DATA_LEN)
Long_Cord = vector(length = MAX_DATA_LEN)
for(i in 1:MAX_DATA_LEN){
     sep_index = 1
     in.string = obesity_raw.csv$GeoLocation[i]
     in.len = nchar(in.string)
     for(sep_index in 1:in.len){
          if(substr(in.string, sep_index, sep_index) == ','){
               break
          }
     }
     Lat_Cord[i]  = substr(in.string, 2, sep_index - 1)
     sep_index    = sep_index + 3
     Long_Cord[i] = substr(in.string, sep_index, (in.len - 1))
}
Lat_Cord  = as.numeric(Lat_Cord)
Long_Cord = as.numeric(Long_Cord)

obesity.dtfr = cbind(obesity.dtfr, Lat_Cord, Long_Cord)
rm(Lat_Cord, Long_Cord, i, in.len, in.string, sep_index)

# QUESTION - Is LocationID a numerical value for LocationAbbr?
abbvChecker(obesity_raw.csv$LocationAbbr, obesity_raw.csv$LocationID)
cbind(unique(obesity_raw.csv$LocationAbbr), unique(obesity_raw.csv$LocationID))
     # CONDENSE - LocationID into LocationAbbr

# FURTHER DATA CLEANING #########################################################################################################################
for(i in 1:18){
     for(q in 1:MAX_DATA_LEN){
          if(!is.na(obesity.dtfr[q ,i]) && obesity.dtfr[q ,i] == ""){
               obesity.dtfr[q, i] = NA
          }
     }
}
for(i in 1:18){
     if(!is.numeric(obesity.dtfr[ ,i])){
          print(unique(obesity.dtfr[ ,i]))
     }
     print('', quote = FALSE)
}

# QUESTION COUNTS ###############################################################################################################################
# The variable Data_Value in obesity.dtfr is a response to multiple different values in the variable Question. UPDATE 12/5/23, QuestionID from
# obesity_raw.csv is being added back in for easier to read code during sorting. 

QuestionID = obesity_raw.csv$QuestionID
obesity.dtfr = cbind(obesity.dtfr, QuestionID)

# TASK - Find counts/sample sizes for each question. 

QuestionID = unique(QuestionID)
question_ref.dtfr = valCounter(obesity.dtfr$Question)
     question_ref.dtfr = cbind(question_ref.dtfr, QuestionID)
rm(QuestionID)

question_ref.dtfr[ ,1]
question_ref.dtfr[ ,2:3]
     # We now have a reference of the sample size of each question.

# MISC #########################################################################################################################################
obesity_rates_all.dtfr = extractSubset(dtfr = obesity.dtfr, primary_select.s = 'QuestionID', criteria.s = "Q036")
rm(MAX_DATA_LEN)

rm(i, q, question_ref.dtfr)

