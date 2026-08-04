# Using R for this analysis gives you more flexibility than you have in Excel, 
# and you don't have to download any software! If you don't have/want the 
# desktop version of RStudio, you can sign up for an account and use 
# RStudio Cloud: https://posit.cloud/ 

# I will walk you through everything below.
# I have created the analyses you will need to use; all you need to do is 
# substitute in the names of the variables you want 
# If a step requires your input, I will mark it with "<INPUT>"

# NOTES:
# > You should at least understand the basics of ANOVA and how it works when
#   writing your paper, but you DO NOT have to know how to write code in R.
# > Make sure you've uploaded the squirrel data csv in the same place that you 
#   uploaded this file.
# > Generally, if you Google an error message you've received, you will find
#   posts on StackExchange or the R forums about the same error, and possible
#   solutions.
# > RUN EVERY LINE OF CODE. The only changes you need to make are on the lines marked
#   <INPUT>



# 1. Import and clean the data -------------------------------------------------

# Make sure there are no stored objects in the Environment:
rm(list=ls()) 

# To run a line of code (tell R to do something), you just put your cursor 
# anywhere in that line and hit the "Run" button (or ctrl-Enter, if you like 
# keyboard shortcuts)

# If a line has "#" in front, it is a comment (not code) and R will ignore it.
# When you click "Run", R will automatically find the next line of code 
# (skipping over comments) and run that.

# Try running the following line of code:
sqrrl <- read.csv("squirrel_data_2018-2020.csv", stringsAsFactors = TRUE)
# You should now see an item called "sqrrl" in the Environment tab, which has 
# 1168 observations of 37 variables. This is the cleaned squirrel data from 2018-2020.


# --- Examine the data: ---------------------------------------------------------------

# First, let's look at the names of the columns:
names(sqrrl)
# These are the names you will use to tell R which variables to use in the 
# analysis. See column_descriptions.txt for more info about what the columns are.

# Next, let's look for missing data. This function will tell you how many 
# missing values (NA) are in each column:
colSums(is.na(sqrrl))
# If your chosen variable has missing data, don't worry, we will deal with that.

# The data for these columns should be either "Yes" or "No", but are they?
TFvars <- c(
  "prcp","consp_15m","consp_int","human_15m","human_int","dog_15m","dog_int")
str_list <- vector()
for(v in TFvars){
  str(sqrrl[[v]])
  str_list <- append(str_list,levels(sqrrl[[v]]))
}
cat("these are the input values:",unique(str_list), sep="\n")


# --- Fix the incorrect data entry: ------------------------------------------------

# The data has more input values than just "Yes" and "No", so we fix it.
for(v in TFvars){
  sqrrl <- sqrrl[sqrrl[[v]] != "Yes, No",]
  sqrrl[[v]] <- as.factor(
    gsub('No, NA', 'No',
         gsub('Yes, NA', 'Yes',sqrrl[[v]]
         )))
  sqrrl[[v]][sqrrl[[v]]==""] = NA
}

# NOW, the data for these columns should be either "Yes" or "No":
TFvars <- c(
  "prcp","consp_15m","consp_int","human_15m","human_int","dog_15m","dog_int")
str_list <- vector()
for(v in TFvars){
  str(sqrrl[[v]])
  str_list <- append(str_list,levels(droplevels(sqrrl[[v]])))
}
cat("these are the input values:",unique(str_list), sep="\n")


# --- Convert the numeric columns from integers (0-16) to percentages: -----------

vars <- c("vigil", "forag", "alert", "socl", "other", "not_vis")
for(v in vars){
  newname = paste0(v, "_pct")
  sqrrl[[newname]] = sqrrl[[v]] / 16
}

# Now, if you check the column names, you should see 6 new columns at the end:
names(sqrrl)
# These are the percentages. For example, forag_pct is the *percent* of time
# the animal spent foraging. 
# These are good dependent variables because they are numeric.



# 2. Input your variable names -------------------------------------------------

# Get the names of your variables from the list we just made.

# Dependent variables should be numeric; independent should be categorical.
# For example, you shouldn't use number of humans/dogs/conspecifics as an
# independent variable because it's not categorical.

# NOTE: It's bad practice to "fish" for combinations that give a significant 
# p-value, and don't use the same combination of variables I have below.

# setting up the variables like this is not recommended in general, but for this 
# small script it will make things easier.


# --- <INPUT> your variable names below: ----------------------------------------


# You may have 1 or 2 independent variables, as long as you have at least 3 
# total groups. There is no maximum number of groups.

# If you need to refresh your memory on groups in ANOVA, see the Powerpoint from 11/20

# Erase everything inside the quotes, replace with your variable name, then RUN the line:

dep <- "vigil_pct" # <INPUT 1>
# Example: if I wanted to use percent vigilance, I would change it to the
# following: dep <- "vigil_pct"

ind1 <- "human_15m" # <INPUT 2>
# example: ind1 <- "prox_safe"

# If you only have one independent variable, you DON't need to change input 3
# but you DO still need to run the input 3 line (line 138) for the script to work:

ind2 <- "biome" # <INPUT 3 - optional>
# example: ind2 <- "human_15m"



# Create a new dataframe with only your chosen variables, then remove rows with
# missing values. This way you keep as many observations as possible:

if( ind2 != "name_of_independent_variable_2"){
  sqrrl_subset <- data.frame(sqrrl[[dep]], sqrrl[[ind1]], sqrrl[[ind2]])
  names(sqrrl_subset) = c(dep, ind1, ind2)

# sqrrl[[ind1]]
# sqrrl_subset <- as.data.frame(cbind(sqrrl[[dep]], sqrrl[[ind1]], sqrrl[[ind2]]))
} else {
sqrrl_subset <- data.frame(sqrrl[[dep]], sqrrl[[ind1]])
names(sqrrl_subset) = c(dep, ind1)}

sqrrl_subset <- droplevels(sqrrl_subset[complete.cases(sqrrl_subset),])



# 3. ANOVA ---------------------------------------------------------------------

# Check that your independent variables are factors and view the number of 
# samples per group (group = number of possible combinations of your variables).

# Without extra calculations, there is no concrete rule on the minimum number
# of samples per group, but I would aim for at least 10 per group. If you have 
# fewer than that, mention it in your paper in the discussion section as a 
# caveat for your results.

# We won't worry about unequal sample sizes between groups, unless it causes
# an error or other issue. If your sample sizes are very different, again, you 
# can mention it in the discussion as a caveat.

# NOTE: In Excel, we would need a balanced design, i.e. the same number of samples
# in each group. Since this is not a controlled experiment with balanced groups,
# we are unlikely to achieve that.


# --- Check the variables & groups: ----------------------------------------------

if( ind2 != "name_of_independent_variable_2"){

  cat("\nare the independent variables factors?\n")
  cat(ind1, ":\n")
  str(sqrrl_subset[[ind1]])
  cat("\n", ind2, ":\n")
  str(sqrrl_subset[[ind2]])
  cat("\nfrequency table (number of samples per group):\n")
  table(sqrrl_subset[[ind1]], 
        sqrrl_subset[[ind2]])
} else {
  cat("\nis the independent variable a factor?\n")
  str(sqrrl_subset[[ind1]])
  cat("\nfrequency table (number of samples per group):\n")
  table(sqrrl_subset[[ind1]])}

# --- Run the ANOVA and view the output: -------------------------------------------

if( ind2 != "name_of_independent_variable_2"){
  # out <- aov(V1 ~ V2 * V3, data=sqrrl_subset)
  out <- aov(as.formula(paste(dep, "~", ind1, "*", ind2)), data=sqrrl_subset)
  
  # if you have 2 independent variables and >4 groups, you won't be able to 
  # tell which group mean(s) are different. The Tukey test compares the 
  # means in pairs and tells you which differences are significant:
  anova_post <- TukeyHSD(out, conf.level = 0.95)
  summary(anova_post)
  
  # NOTES:
  # > "p adj" is the p-value for the comparison
  # > if you have 2 independent vars and 4 groups, ignore the Tukey output.
  
} else {
  # in this case there is only one independent variable, so we don't need 
  # the Tukey test:
  out <- aov(as.formula(paste(dep, "~", ind1)), data=sqrrl_subset)}

summary(out)

# If you have a significant interaction term (var1:var2), that means that the
# effect of one independent variable on the dependent variable depends on the 
# value of the other independent variable. Email me if you have further questions.


# --- Double-check ANOVA assumptions: --------------------------------------------

# Check that you fulfilled the assumptions of heteroskedasticity and normality:

# There should be no visible pattern in the residuals:
plot(out, 1)
# And they should roughly follow the line if they are normally distributed:
plot(out, 2)
# another test for normality is the Shapiro-Wilk test, which tests the null
# hypothesis that the data are normally distributed. if p-value < 0.05, the 
# residuals are not normally distributed:
out_res <- residuals(out)
shapiro.test(out_res)
# If you find that your residuals are not normally distributed, you can mention
# that in the discussion as a caveat for your results.


plt <- boxplot(sqrrl[[dep]]~sqrrl[[ind1]]+sqrrl[[ind2]])

