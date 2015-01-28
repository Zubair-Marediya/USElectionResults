# GROUP MEMBERS:
# ZUBAIRALI MAREDIYA
# ANKIT AGGARWAL
# ALOYSIUS LAI
# TEMI LAL
# DONGPING ZHANG



#######################################################################################################
#                                               STEP 2: Part 1                                        #
#######################################################################################################


require("XML")
url_string="http://www.stat.berkeley.edu/~nolan/data/Project2012/countyVotes2012/xxx.xml"
doc=NULL
candidate_results=NULL
counties=NULL
percentages=NULL
popular_results=NULL
parties=NULL
num_results=NULL
winners=NULL
winning_candidates=NULL
winning_percentages=NULL
winning_parties=NULL
second_place=NULL
second_place_candidates=NULL
second_place_percentages=NULL
second_place_parties=NULL
election_results=NULL


# Deal with State Names

state_names=read.table("http://www.stat.berkeley.edu/users/nolan/data/Project2012/countyVotes2012/stateNames.txt",
                       skip=1)$V1[-2]
cap_first_letters = toupper(substring(state_names,1,1))
state_names2 = paste(cap_first_letters, substring(state_names,2), sep="")
state_names2[grep("-", state_names)] = gsub("-", " ", state_names2[grep("-", state_names)])
#################
states_XML=NULL

for (i in 1:length(state_names2)) {
  states_XML[[i]]=gsub("xxx",state_names[i],url_string) ### in terms of url, state_names is used because all non-cap letters
  
  doc=xmlParse(states_XML[[i]])
  
  candidate_results[[i]]=
    xmlToDataFrame(nodes=getNodeSet(doc,"//th[@class='results-candidate']"),homogeneous=F)$text[-1]
  
  counties[[i]]=xmlToDataFrame(nodes=getNodeSet(doc,"//th[@class='results-county']"),homogeneous=F)$text[-1]
  percentages[[i]]=xmlToDataFrame(nodes=getNodeSet(doc,"//td[@class='results-percentage']"),homogeneous=F)$text
  popular_results[[i]]=xmlToDataFrame(nodes=getNodeSet(doc,"//td[@class='results-popular']"),homogeneous=F)$text
  parties[[i]]=xmlToDataFrame(nodes=getNodeSet(doc,"//td[@class='results-party']/abbr"),homogeneous=F)$text
  num_results[i]=length(percentages[[i]])
  winners[[i]]=seq(1,num_results[i],by=(num_results[i]/length(counties[[i]])))
  winning_candidates[[i]]=candidate_results[[i]][winners[[i]]]
  winning_percentages[[i]]=percentages[[i]][winners[[i]]]
  winning_parties[[i]]=parties[[i]][winners[[i]]]
  second_place[[i]]=seq(2,num_results[i],by=(num_results[i]/length(counties[[i]])))
  second_place_candidates[[i]]=candidate_results[[i]][second_place[[i]]]
  second_place_percentages[[i]]=percentages[[i]][second_place[[i]]]
  second_place_parties[[i]]=parties[[i]][second_place[[i]]]
  # merge everything onto a data frame
  election_results[[i]]=data.frame(State = rep(state_names2[i], length(counties[[i]])),
                                   Counties=counties[[i]],
                                   Winning_Candidates=winning_candidates[[i]],
                                   Winning_Parties=winning_parties[[i]],
                                   Winning_Percentages=winning_percentages[[i]],
                                   Losing_Candidates=second_place_candidates[[i]],
                                   Losing_Parties=second_place_parties[[i]],
                                   Losing_Percentages=second_place_percentages[[i]],
                                   stringsAsFactors = F)
}
election_data = do.call(rbind, election_results)
# View(election_data)






#######################################################################################################
#                                               STEP 2: Part 2                                        #
#######################################################################################################


# Change colnames of B01003

B01003 = read.csv("http://www.stat.berkeley.edu/~nolan/data/Project2012/census2010/B01003.csv")
meta_B01003 = read.csv("http://www.stat.berkeley.edu/users/nolan/data/Project2012/census2010/B01_metadata.txt")
# class(meta_B01003) # data.frame
# class(B01003) # data.frame
# view 2 data frame View(B01003); View(meta_B01003)
explained_colnames_B01003 = as.character(meta_B01003[,2])
explained_colnames_B01003 [3] = "Pop_Id"
colnames(B01003) = c(colnames(meta_B01003)[1], explained_colnames_B01003)
# View(B01003)


# Remove Alaska and Puerto Rico in B01003

A_locs_B01003 = suppressWarnings(grep("Alaska", B01003$Geography))  # 199:259
PR_locs_B01003 = suppressWarnings(grep("Puerto Rico", B01003$Geography)) # 7858:8070

### ON MACS AND LINUX, R CANNOT READ SPANISH CHARACTERS. IT REPLACES THEM WITH A QUESTION MARK SYMBOL.
### THIS CREATES WARNINGS FOR A_locs_B01003 and PR_locs_B01003 AND PREVENTS US FROM PROPERLY REMOVING ALASKA AND
### PUERTO RICO. WE WILL HARDCODE IN THE INDICIES FOUND IN A_locs_B01003 and PR_locs_B01003.

B01003 = B01003[-c(199:259, 7858:8070), ] # A_locs_B01003 = 199:259 and PR_locs_B01003 = 7858:8070

B01003_Id2 = unique(B01003$Id2) # all unique Id2s


# Create a helper function that counts the length of Id2 of each individual

count_repeats_Id2 = function(x){length(which(B01003$Id2==x))}
length_Id2_B01003 = sapply(B01003_Id2, count_repeats_Id2) # length_Id2_B01003 = number of repeats of each Id2
rows_before_prob = sum(length_Id2_B01003[1:1772])
probs_rows = (rows_before_prob+1):(rows_before_prob+1+2) # <Dona Ana County, New Mexico> rows identified


# Switching out Dona Ana County, New Mexico

B01003_geo = B01003$Geography
B01003_geo_character = as.character(B01003_geo)
B01003_geo_character[probs_rows] = "Dona Ana County, New Mexico"
B01003_geo_factor = as.factor(B01003_geo_character)
B01003$Geography = B01003_geo_factor


# Change colnames of DP02

DP02 = read.csv("http://www.stat.berkeley.edu/~nolan/data/Project2012/census2010/DP02.csv")
meta_DP02 = read.csv("http://www.stat.berkeley.edu/users/nolan/data/Project2012/census2010/DP02_metadata.txt")
# class(DP02) # data.frame
# class(meta_DP02) # data.frame
# view 2 data frame View(DP02); View(meta_DP02)
explained_colnames_DP02 = as.character(meta_DP02[,2])
explained_colnames_DP02 [3] = "Pop_Id"
colnames(DP02) = c(colnames(DP02)[1], explained_colnames_DP02)
# View(DP02)


# Remove Alaska in DP02

A_locs_DP02 = suppressWarnings(grep("Alaska", DP02$Geography))

### ON MACS AND LINUX, R CANNOT READ SPANISH CHARACTERS. IT REPLACES THEM WITH A QUESTION MARK SYMBOL.
### THIS CREATES WARNINGS FOR A_locs_DP02  AND PREVENTS US FROM PROPERLY REMOVING ALASKA.
### WE WILL HARDCODE IN THE INDICIES FOUND IN A_locs_DP02.

DP02 = DP02[-(68:96), ] # A_locs_DP02 = 68:96
DP02_geo = DP02$Geography
DP02_geo_character = as.character(DP02_geo)
DP02_geo_character[1773] = "Dona Ana County, New Mexico"
DP02_geo_factor = as.factor(DP02_geo_character)
DP02$Geography = DP02_geo_factor
count_state_DP02 = unlist(strsplit(as.character(DP02$Geography), split = ", "))
DP02_counties = count_state_DP02[seq(1, length(count_state_DP02), by = 2)]
DP02_states = count_state_DP02[seq(2, length(count_state_DP02), by = 2)]

# Change colnames of DP03
DP03 = read.csv("http://www.stat.berkeley.edu/~nolan/data/Project2012/census2010/DP03.csv")
meta_DP03 = read.csv("http://www.stat.berkeley.edu/users/nolan/data/Project2012/census2010/DP03_metadata.txt")
# class(DP03) # data.frame
# class(meta_DP03) # data.frame
# view 2 data frame View(DP03); View(meta_DP03)
explained_colnames_DP03 = as.character(meta_DP03[,2])
explained_colnames_DP03 [3] = "Pop_Id"
colnames(DP03) = c(colnames(DP03)[1], explained_colnames_DP03)
# View(DP03)

# Remove Alaska and Puerto Rico in DP03

A_locs_DP03 = suppressWarnings(grep("Alaska", DP03$Geography))
PR_locs_DP03 = suppressWarnings(grep("Puerto Rico", DP03$Geography))

### ON MACS AND LINUX, R CANNOT READ SPANISH CHARACTERS. IT REPLACES THEM WITH A QUESTION MARK SYMBOL.
### THIS CREATES WARNINGS FOR A_locs_DP03 and PR_locs_DP03 AND PREVENTS US FROM PROPERLY REMOVING ALASKA AND
### PUERTO RICO. WE WILL HARDCODE IN THE INDICIES FOUND IN A_locs_DP03 and PR_locs_DP03

DP03 = DP03[-c(68:96, 3140:3217), ] # A_locs_DP03 = 68:96 and PR_locs_DP03 = 3140:3217


# Switching out Dona Ana County, New Mexico

DP03_geo = DP03$Geography
DP03_geo_character = as.character(DP03_geo)
DP03_geo_character[1773] = "Dona Ana County, New Mexico"
DP03_geo_factor = as.factor(DP03_geo_character)
DP03$Geography = DP03_geo_factor
count_state_DP03 = unlist(strsplit(as.character(DP03$Geography), split = ", "))
DP03_counties = count_state_DP03[seq(1, length(count_state_DP03), by = 2)]
DP03_states = count_state_DP03[seq(2, length(count_state_DP03), by = 2)]


# Now we will clean up B01003. Each county has 2 or 3 rows in the data frame depending on how many different
# POPGROUPs there are. We will make a data frame that will make one row for each county and contain all the
# same information.

pops = B01003$Pop_Id
total_locs = grep(1, pops)
white_locs = grep(2, pops)
black_locs = grep(4, pops)

total_df = B01003[total_locs, ]
white_df = B01003[white_locs, ]
black_df = B01003[black_locs, ]

total_white_df = merge(total_df, white_df, by = "Id2", all = TRUE)
all_pops_df = merge(total_white_df, black_df, by = "Id2", all = TRUE)
all_pops_df = all_pops_df[,-c(4:5, 8:11, 14:17)]
names(all_pops_df)[2:9] = c("GEO.id", "Geography", "Estimate; Total Population",
                            "Margin of Error; Total Population",
                            "Estimate; White alone",
                            "Margin of Error; White alone",
                            "Estimate; Black or African American alone",
                            "Margin of Error; Black or African American alone")


# Now we will merge DP02, DP03, and all_pops_df

DP_merged_df = suppressWarnings(merge(DP02, DP03))
all_csv = merge(all_pops_df, DP_merged_df, sort = FALSE) # arg sort = should the result be sorted by the columns


# Now we are creating two columns, one with County names, and the other
# with State names, to merge the election_data and all_csv data frames.

geo_names = unlist(strsplit(as.character(all_csv$Geography), "( County, | Parish, |, )"))
county_locs = seq(1, length(geo_names), by = 2)
state_locs = seq(2, length(geo_names), by = 2)
counties = geo_names[county_locs]
states = geo_names[state_locs]
counties = gsub(" ", "", counties)
states = gsub(" ", "", states)
merger = toupper(paste(states, counties, sep = ""))



ed_counties = as.character(election_data$Counties)
ed_states = as.character(election_data$State)

ed_counties = unlist(strsplit(ed_counties, "( County | Parish )"))

E_merger=paste(ed_states, ed_counties, sep = "")
E_merger=toupper(gsub(" ", "", E_merger))


# Now we will clean up the merging vectors above to make them sync better.
E_merger = gsub("LOUISIANAJEFFDAVIS", "LOUISIANAJEFFERSONDAVIS", E_merger)
E_merger = gsub("MISSISSIPPIJEFFDAVIS", "MISSISSIPPIJEFFERSONDAVIS", E_merger)
E_merger = gsub("NEWYORKSAINTLAWRENCE", "NEWYORKST.LAWRENCE", E_merger)
E_merger = gsub("ARKANSASSAINTFRANCIS", "ARKANSASST.FRANCIS", E_merger)
E_merger = gsub("NEWYORKMANHATTAN", "NEWYORKNEWYORK", E_merger)
E_merger = gsub("NEWYORKBROOKLYN", "NEWYORKKINGS", E_merger)
E_merger = gsub("NEWYORKSTATENISLAND", "NEWYORKRICHMOND", E_merger)


# Now we will add the "MERGING" column to each data frame.

all_csv$Merger = merger
election_data$Merger = E_merger
merged_df = merge(election_data, all_csv, by = "Merger", all.x = TRUE, all.y = FALSE, sort = FALSE)





#######################################################################################################
#                                               STEP 2: Part 3                                        #
#######################################################################################################

tree = xmlParse("http://www.stat.berkeley.edu/users/nolan/data/Project2012/counties.gml")
root = xmlRoot(tree)


gml_counties = getNodeSet(root, "//doc/state/county/gml:name")
gml_counties = sapply(gml_counties, xmlValue)
gml_counties = gsub("\\\n", "", gml_counties)
gml_counties = gsub(" County", "", gml_counties)
gml_counties = gsub(" Parish", "", gml_counties)


state_rep_numbers = unlist(unname(xmlApply(root, xmlSize))) - 1
# xmlApply(root, xmlSize) tells the size of each child node
# unname function deletes the names of each element in the list, which is state
# have to subtract 1 to discount the very beginning child node which is the state name
gml_states = getNodeSet(root, "//doc/state/gml:name")
gml_states = sapply(gml_states, xmlValue)
gml_states = gsub("\\\n", "", gml_states)
gml_states = rep(gml_states, times = state_rep_numbers)

gml_merger = paste(gml_states, gml_counties, sep = "")
gml_merger = toupper(gsub(" ", "", gml_merger))


# There are counties that have "city" in them within the GML, but not in election_data. We will fix this.
gml_locs = grep("CITY", gml_merger)
remove_gml_locs = c(1,2,3,4,5,7, 17, 19,37,38)
# removes all the counties that actually have "city" in the county name
gml_locs = gml_locs[-remove_gml_locs]
gml_merger[gml_locs] = gsub("CITY", "", gml_merger[gml_locs])


gml_x = getNodeSet(root, "//gml:X")
gml_x = as.numeric(sapply(gml_x, xmlValue))


gml_y = getNodeSet(root, "//gml:Y")
gml_y = as.numeric(sapply(gml_y, xmlValue))

gml_df = data.frame(Merger = gml_merger, GML_X = gml_x, GML_Y = gml_y, stringsAsFactors=F)

# We need to remove Alaska from the gml_df data frame.
alaska_locs = grep("ALASKA", gml_merger)
gml_df = gml_df[-alaska_locs, ]


# Now we merge merged_df and gml_df to give us our final data frame.

final_df = merge(merged_df, gml_df, by = "Merger")
# View(colnames(final_df))


#######################################################################################################
#                                               STEP 3: Part A                                        #
#######################################################################################################


# Dongping and Aloysius worked on the recursive partitioning part.


cv_04 = read.table("http://www.stat.berkeley.edu/~nolan/data/Project2012/countyVotes2004.txt",
                   header=T)


# In cv_04, add a column of winner04 indicating who's the winner in each county in the 04 election

result04=NULL
winner04=NULL
for (i in 1:nrow(cv_04)){
  if (cv_04[i, 2]>cv_04[i, 3]){
    winner04 = "bush"
  } else if (cv_04[i, 2]<cv_04[i, 3]){
    winner04 = "kerry"
  } else {
    winner04 = NA
  }
  result04 = c(result04, winner04)
}
winners04_county = result04
cv_04$winner04 = winners04_county
# View(cv_04)


# adding an additional column to final_df to match and extract data from final_df

Merger_cv_04 = gsub("\\.", "", final_df$Merger)
final_df = cbind(final_df, Merger_cv_04)
# final_df adding a column names: match_cv_04
match_cv04=toupper(gsub(",", "", cv_04$countyName))
match_cv04 = gsub(" ", "", match_cv04)
cv_04$Merger_cv_04 = match_cv04
# adding a column into cv_04 names: Merger_cv_04

# View(cv_04)
# View(final_df[, 771])
super_df = merge(final_df, cv_04, by = "Merger_cv_04", all=T)
super_df = super_df[, c(2:771, 1, 772:775)]


# To prepare for rpart function, we make changes to the column names

colnames(super_df)=gsub(";",replacement="",x=colnames(super_df))
colnames(super_df)=gsub(" ",replacement="_",x=colnames(super_df))
colnames(super_df)=gsub("-",replacement="_",x=colnames(super_df))
colnames(super_df)=gsub("'",replacement="",x=colnames(super_df))
colnames(super_df)=gsub("\\(",replacement="",x=colnames(super_df))
colnames(super_df)=gsub("\\)",replacement="",x=colnames(super_df))
colnames(super_df)= gsub(",", replacement="", x=colnames(super_df))
colnames(super_df)= gsub("\\.", replacement="", x=colnames(super_df))


# Generating 2 Predicators: black percentages and white percentages

Percent_Black_Population = (super_df[, 15]/super_df[, 13])*100
Percent_White_Population = (super_df[, 17]/super_df[, 13])*100
super_df = cbind(super_df, Percent_Black_Population, Percent_White_Population)


# To get rid of the percentage signs in the two columns

super_df$Winning_Percentages = as.numeric(gsub("%", "", super_df$Winning_Percentages))
super_df$Losing_Percentages = as.numeric(gsub("%", "", super_df$Losing_Percentages))


# Try to find appropriate predicators from colnames of final_df
# View(colnames(super_df))

all_pred_ncol = c(
  227, 251, 311, 315, 319,
  331, 343, 351, 359, 387,
  391, 395, 479, 483, 487,
  515, 769, 770, 776, 777
)
# all predictors included except 2012 & 2004 election results
# Done with general predictors column numbers in the super_df#
##############################################################


###################################################
# Create data frame election04 for rpart function #
###################################################

election04 = cbind(super_df[, all_pred_ncol], winner04=super_df$winner04)

# double check factor columns

fac_col_04=NULL
all_col_fac_election04 = NULL
for (i in 1:ncol(election04)){
  if (class(election04[,i])=="factor"){
    fac_col_04 = i
  } else {
    fac_col_04 = NULL
  }
  all_col_fac_election04 = c(all_col_fac_election04, fac_col_04)
}


# Convert to numeric and character

for (i in 1:2){
  election04[, all_col_fac_election04[i]] = as.numeric(election04[, all_col_fac_election04[i]])
}
election04[, all_col_fac_election04[3]] = as.character(election04[, all_col_fac_election04[3]])


# Remove any row that contains NA

NA_vals_04 = apply(election04, 1, function(x) any(is.na(x)))
election04 = election04[!NA_vals_04, ]


########################### Prediction election04 ###########################

# Starting rpart function
require("rpart")
# grow tree
winner2004 = rpart(winner04~., data=election04, method="class",
                   control = rpart.control(minsplit=10, cp=1e-04))
# printcp(winner2004)
# plotcp(winner2004)
winner2004_pruned = prune(winner2004, cp= winner2004$cptable[which.min(winner2004$cptable[,"xerror"]),"CP"])
plot(winner2004_pruned, main="Classification Tree of 2004 Election Result",
     margin = 0.1, uniform=T) # generate election04 result plots
text(winner2004_pruned, use.n=TRUE, all=TRUE, cex=.9)
# View(cv_04)


###################################################
# Create data frame election12 for rpart function #
###################################################

# Now switching to election 2012

election12 = cbind(super_df[,all_pred_ncol], winner12=super_df$Winning_Candidates)


# double check factor columns

fac_col_12=NULL
all_col_fac_election12 = NULL
for (i in 1:ncol(election12)){
  if (class(election12[,i])=="factor"){
    fac_col_12 = i
  } else {
    fac_col_12 = NULL
  }
  all_col_fac_election12 = c(all_col_fac_election12, fac_col_12)
}


# Convert to numeric and character

for (i in 1:2){
  election12[, all_col_fac_election12[i]] = as.numeric(election12[, all_col_fac_election04[i]])
}
election12[, all_col_fac_election12[3]] = as.character(election12[, all_col_fac_election12[3]])


# Remove any row that contains NA

NA_vals_12 = apply(election12, 1, function(x) any(is.na(x)))
election12 = election12[!NA_vals_12, ]


########################### Prediction election12 ###########################

winner2012 = rpart(winner12~., data=election12, method="class",
                   control = rpart.control(minsplit=10, cp=1e-04))
# printcp(winner2012)
winner2012_pruned = prune(winner2012,
                          cp= winner2012$cptable[which.min(winner2012$cptable[,"xerror"]),"CP"])
# which.min gives the index number
# plotcp(winner2012)
plot(winner2012_pruned, main="Classification Tree of 2012 Election Result",
     margin = 0.1, uniform = T) # generate election12 result plots
text(winner2012_pruned, use.n=TRUE, all=TRUE, cex=.9)


#######################################################################################################                                                                                                  
#                                               STEP 3: Part B                                        #
#######################################################################################################
# Zubair, Temi, and Ankit worked on the k nearest neighbor part.

require("class")

########################
#  2004 Election Data  #
########################

predictors = super_df[ , c(1,227, 251, 311, 315, 319, 331, 343, 351, 359, 387, 391, 395, 479, 483, 487, 769, 770, 776, 777, 775)]
NA_vals = apply(predictors, 1, anyNA)
locs = which(NA_vals == FALSE)
temp_super_df = predictors[locs, ]
rownames(temp_super_df) = temp_super_df$Merger
cl = as.factor(temp_super_df$winner04)
temp_super_df = temp_super_df[ , -c(1,ncol(temp_super_df))]


# Some columns in temp_super_df have a dash instead of NA. These are turned into NA's
# with the function call below. We will then remove these rows immediately afterwards.

temp = suppressWarnings(apply(temp_super_df, 2, function(x) as.numeric(as.character(x))))
rownames(temp) = rownames(temp_super_df)
NA_vals = apply(temp, 1, anyNA)
locs = which(NA_vals == FALSE)
temp = temp[locs, ]
cl_04 = cl[locs]

# Now we will standardize each predictor column in our data frame.

temp_norm_04 = apply(temp, 2, scale)
rownames(temp_norm_04) = rownames(temp)

actual_04_df = data.frame(MERGER = super_df$Merger, Actual = super_df$winner04)



########################
#  2012 Election Data  #
########################

predictors = super_df[ , c(1,227, 251, 311, 315, 319, 331, 343, 351, 359, 387, 391, 395, 479, 483, 487, 769, 770, 776, 777, 4)]
NA_vals = apply(predictors, 1, anyNA)
locs = which(NA_vals == FALSE)
temp_super_df = predictors[locs, ]
rownames(temp_super_df) = temp_super_df$Merger
cl = as.factor(temp_super_df$Winning_Candidates)
temp_super_df = temp_super_df[ , -c(1,ncol(temp_super_df))]


# Some columns in temp_super_df have a dash instead of NA. These are turned into NA's
# with the function call below. We will then remove these rows immediately afterwards.

temp = suppressWarnings(apply(temp_super_df, 2, function(x) as.numeric(as.character(x))))
rownames(temp) = rownames(temp_super_df)
NA_vals = apply(temp, 1, anyNA)
locs = which(NA_vals == FALSE)
temp = temp[locs, ]
cl_12 = cl[locs]

# Now we will standardize each predictor column in our data frame.

temp_norm_12 = apply(temp, 2, scale)
rownames(temp_norm_12) = rownames(temp)

actual_12_df = data.frame(MERGER = super_df$Merger, Actual = super_df$Winning_Candidates)


###############################################
#  Now we will find the errors for k_values.  #
###############################################

k_value_error = function(k_value) {
  pred_results_04 = knn.cv(temp_norm_04, cl_04, k = k_value)
  pred_df_04 = data.frame(MERGER = rownames(temp_norm_04), Prediction = pred_results_04)
  
  pred_results_12 = knn.cv(temp_norm_12, cl_12, k = k_value)
  pred_df_12 = data.frame(MERGER = rownames(temp_norm_12), Prediction = pred_results_12)
  
  
  pred_actual_df_04 = merge(pred_df_04, actual_04_df)
  pred_actual_df_12 = merge(pred_df_12, actual_12_df)
  
  pred_actual_df_table_04 = table(pred_actual_df_04$Prediction, pred_actual_df_04$Actual)
  k_error_04 = (pred_actual_df_table_04[1, 2] + pred_actual_df_table_04[2, 1]) / sum(pred_actual_df_table_04)
  
  pred_actual_df_table_12 = table(pred_actual_df_12$Prediction, pred_actual_df_12$Actual)
  k_error_12 = (pred_actual_df_table_12[1, 2] + pred_actual_df_table_12[2, 1]) / sum(pred_actual_df_table_12)
  
  return(list(k_error_04, k_error_12))
}

k_values = 1:50
k_errors = sapply(k_values, k_value_error)

consistent_min_k = function(){
  k_errors = sapply(k_values, k_value_error)
  return(which(k_errors == min(k_errors)))
}


##############################################################################################################
#                                                    Plots                                                   #
##############################################################################################################

k_errors = unlist(k_errors)
k_errors_04 = k_errors[seq(1, length(k_errors), by = 2)]
k_errors_12 = k_errors[seq(2, length(k_errors), by = 2)]

plot(k_values, k_errors_04, type = "h", main = "K Value Errors for 2004", xlab = "K Values", ylab = "K Value Error")
plot(k_values, k_errors_12, type = "h", main = "K Value Errors for 2012", xlab = "K Values", ylab = "K Value Error")


# Now we remove k_values and k_errors for k_values = 1, 2, 3 for 2004, and 
# k_values = 1, 2, 3, 4, 5 and see how well the remaining points fit a linear model. 

x = k_values[-(1:3)]
y = k_errors_04[-(1:3)]

plot(x, y, main = "K Value Errors for 2004", xlab = "K Values", ylab = "K Value Error", pch = 16)
lmfit = lm(y ~ x)
abline(lmfit, col = "red", lwd = 4)
legend(5, 0.165, paste("r = ", round(cor(x,y), 6)), cex = 2)


x = k_values[-(1:5)]
y = k_errors_12[-(1:5)]

plot(x, y, main = "K Value Errors for 2012", xlab = "K Values", ylab = "K Value Error", pch = 16)
lmfit = lm(y ~ x)
abline(lmfit, col = "red", lwd = 4)
legend(5, 0.155, paste("r = ", round(cor(x,y), 6)), cex = 2)


# Now we will make a grouped bar plot to compare error rates of knn vs. rpart 

require("ggplot2")
groupbarplotdf = data.frame(Method = factor(c("knn", "rpart", "knn", "rpart")),
                            Year = factor(c(2004, 2004, 2012, 2012), levels = c(2004, 2012)),
                            Error = c(0.134523, 0.19828, 0.147382, 0.13636))
ggplot(data=groupbarplotdf, aes(x=Year, y=Error, fill=Method)) + geom_bar(stat="identity", position=position_dodge(), width = 0.4) + ggtitle("Error Rate Comparison: knn vs. rpart") + ylim(0, 0.25) + theme(plot.title = element_text(size=30, face="bold", vjust=2), legend.title = element_text(size=16, face="bold"), legend.text = element_text(size=13, face="bold"), legend.position = c(0.9, 0.819), legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"), axis.title.x = element_text(size=30, face="bold", vjust=2),  axis.title.y = element_text(size=30, face="bold", vjust=2),  axis.text.x = element_text(size=20, face="bold", vjust=2),  axis.text.y = element_text(size=20, face="bold", vjust=2))


# Now we will produce a map plot


require(maps)
maps_df = super_df[ , c(3, 772)]
maps_NA = apply(maps_df, 1, anyNA)
maps_locs = which(maps_NA == FALSE)
maps_df = maps_df[maps_locs, ]
par(mfrow=c(1,1),mai=c(0,0,0,0))
new_gml_x=super_df[,769][maps_locs]/1000000
new_gml_y=super_df[,770][maps_locs]/1000000
plot(super_df[,769]/1000000,super_df[,770]/1000000,xlab="Longitude",ylab="Latitude",cex=0.1,type="p",xlim=c(-126,-68),ylim=c(25,49.1))
winning_parties2012=super_df[,5][maps_locs]
losing_parties2012=super_df[,8][maps_locs]
winning_percentages2012=super_df[,6][maps_locs]/100
losing_percentages2012=super_df[,9][maps_locs]/100
Rep2004=super_df$bushVote[maps_locs]/(super_df$bushVote[maps_locs]+super_df$kerryVote[maps_locs])
Dem2004=super_df$kerryVote[maps_locs]/(super_df$bushVote[maps_locs]+super_df$kerryVote[maps_locs])
suppressWarnings(
  for (i in 1:length(maps_locs)) {
    if (winning_parties2012[i]=="GOP" & winning_percentages2012[i]>Rep2004[i]) {
      arrows(x0=new_gml_x[i],y0=new_gml_y[i],x1=new_gml_x[i]+2*(winning_percentages2012[i]-Rep2004[i]),y1=new_gml_y[i]+2*(winning_percentages2012[i]-Rep2004[i]),col="red",length=0.05,lwd=0.01,angle=15)
    } else if (winning_parties2012[i]=="Dem" & winning_percentages2012[i]>Dem2004[i]) {
      arrows(x0=new_gml_x[i],y0=new_gml_y[i],x1=new_gml_x[i]-2*(winning_percentages2012[i]+Dem2004[i]),y1=new_gml_y[i]+2*(winning_percentages2012[i]-Dem2004[i]),col="royalblue",length=0.05,lwd=0.01,angle=15)
    } else if (winning_parties2012[i]=="GOP" & winning_percentages2012[i]<Rep2004[i]) {
      arrows(x0=new_gml_x[i],y0=new_gml_y[i],x1=new_gml_x[i]+2*abs(winning_percentages2012[i]-Rep2004[i]),y1=new_gml_y[i]+2*abs(winning_percentages2012[i]-Rep2004[i]),col="royalblue",length=0.05,lwd=0.01,angle=15)
    } else if (winning_parties2012[i]=="Dem" & winning_percentages2012[i]<Dem2004[i]) {
      arrows(x0=new_gml_x[i],y0=new_gml_y[i],x1=new_gml_x[i]-2*abs(winning_percentages2012[i]-Dem2004[i]),y1=new_gml_y[i]+2*abs(winning_percentages2012[i]-Dem2004[i]),col="red",length=0.05,lwd=0.01,angle=15)
    }
  }
)


#####################################################################################################################
#                                        READ NOTE REGARDING MAP PLOT                                               #
#####################################################################################################################

# WE WROTE THE MAP CODE ON A WINDOWS MACHINE AND IT LOOKS THE WAY IT DISPLAYS
# IN THE HTML REPORT. HOWEVER, WHEN WE RUN THE CODE ON A MAC OR LINUX, IT 
# DID NOT PRODUCE EXPECTED RESULTS. 
