# Procedure 5.7 Standard (z) scores

# To open the qci data:
load("qci.Rdata")   # qci.Rdata must be in your working directory 
                    # See READMEdata


# Standardised scores are given by the scale() function
Zaccuracy = scale(qci$accuracy)

# Given the Z scores, the T scores can be calculated. 
# Or, the T scores can be obtained using the rescale() function in the psych package.
# The results from the two are identical.
 
Tacc.calc = Zaccuracy * 10 + 50
Tacc.psych = rescale(qci$accuracy, mean = 50, sd = 10, df = FALSE)
identical(Tacc.calc, Tacc.psych)

# For stanines, the mean = 5, the standard deviation = 2
rescale(qci$accuracy, mean = 5, sd = 2, df = FALSE)


#################
#   Table 5.5   #
#################

# Calculate the transformed scales, rounding off to two decimal places
Zacc = round(rescale(qci$accuracy, mean = 0, sd = 1, df = FALSE), 2)
Zspeed =  round(rescale(qci$speed, mean = 0, sd = 1, df = FALSE), 2)
Tacc =  round(rescale(qci$accuracy, mean = 50, sd = 10, df = FALSE), 2)
Tspeed =  round(rescale(qci$accuracy, mean = 50, sd = 10, df = FALSE), 2)

# Combine the transformed scales with qci
qci = cbind(qci, Zacc, Zspeed, Tacc, Tspeed)

# Select IDs for the top 22 most accurate inspectors
id = order(qci$accuracy, decreasing = TRUE)[1:22]

# The list in Table 5.5
(qci.top22 = qci[id, c("inspector", "company", "educlev", "gender", "accuracy", "speed", 
   "Zacc", "Zspeed", "Tacc", "Tspeed")])
