# Procedure 5.1: Frequency tabulation, distributions and crosstabulation

# To open the qci data:
load("qci.Rdata")   # qci.Rdata must be in your working directory 
                    # See READMEdata

# The R code in this Procedure uses functions from the descr package.
# To install the package, run the line of code below that begins with 
# "install.packages" but first remove the #.
# NOTE: You need an internet connection.
# If you are asked for a CRAN mirror, select any convenient mirror.

# install.packages("descr", dependencies = TRUE)


#########################################
#   FREQUENCY TABULATIONS (Table 5.1)   #
#########################################

# A frequency tabulation gives the frequency of each value's occurrence.
# To get summary() to return frequencies for a numeric variable,
# use the as.factor() function. 
 
summary(as.factor(qci$jobsat))

# Alternatively, use the freq() function from the descr package.

library(descr)
freq(qci$jobsat)

# The cumulative percent makes sense only when the values of the variable 
# are ordered. Use the ordered() function to get cumulative percentage. 

freq(ordered(qci$jobsat)) 

# plot = FALSE stops the graph being plotted.
               
freq(ordered(qci$jobsat), plot = FALSE)


####################################
#   CROSSTABULATIONS (Table 5.2)   #
####################################

table(qci$gender, as.factor(qci$jobsat))    # NOTE: gender is already a factor

# dnn contains the names for each dimension in the table

table(qci$gender, as.factor(qci$jobsat), dnn = c("Gender", "Job Satisfaction"))

# useNA indicates whether to include the number of NA (i.e., the number of missing) in the table

table(qci$gender, as.factor(qci$jobsat), 
     dnn = c("Gender", "Job Satisfaction"), 
     useNA = "ifany")

# Alternatively, use the CrossTable() function in the descr package
# to give row, column and total proportions.
# Any one of them can be dropped by setting prop.r, prop.c, or prop.t = FALSE
# In the command below, expected = FALSE and prop.chisq = FALSE turn off 
# features not wanted in a simple crosstabulation. 

library(descr)
CrossTable(qci$jobsat, qci$gender, 
   expected = FALSE, prop.chisq = FALSE, 
   dnn = c("Job Satisfaction", "Gender")) 

# The table can be made to look more like an
# SPSS table by setting: format = "SPSS"
# The default is: format = "SAS"

CrossTable(qci$jobsat, qci$gender, 
   expected = FALSE, prop.chisq = FALSE, 
   dnn = c("Job Satisfaction", "Gender"),
   format = "SPSS")    
