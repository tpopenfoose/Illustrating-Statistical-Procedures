


library(vcd)
mytable = table(qci$educlev, qci$gender)
mytable
assocstats(mytable)

mytable = table(qci$educlev, qci$company)
mytable
assocstats(mytable)

detach(package:vcd)

library(descr)
Crosstable(qci$educlev, qci$gender)
CrossTable(qci$educlev, qci$company)
detach(package:descr)


cuts = quantile(na.omit(qci$speed), probs = c(.2,.4,.6,.8), type = 6)
breaks =  c(min(na.omit(qci$speed)), cuts, max(na.omit(qci$speed)))
qci$cat_speed = cut(qci$speed, breaks = breaks, right = FALSE, include.lowest = TRUE)
table(qci$cat_speed, qci$educlev)
library(rpartOrdinal)
ordinal.gamma(qci$educlev, qci$cat_speed)
detach(package:rpartOrdinal)



mytable = table(qci$educlev, qci$company  )
mytable
library(rapport)
lambda.test(mytable)
detach(package:rapport)



lambda.test <- function(table, direction = 0) {

    if (direction != 0)
        as.numeric(sum(apply(table, direction, max)) - ifelse(direction == 1, max(colSums(table)), max(rowSums(table)))) / (sum(table) - ifelse(direction == 1, max(colSums(table)), max(rowSums(table))))
    else
        list(row=lambda.test(table, 1), col=lambda.test(table, 2))

}
#
lambda.test <- function(table, direction = 0) {

    if (!is.numeric(direction))
        stop('direction should be an integer between 0 and 2')
    if (!direction %in% c(0, 1, 2))
        stop('direction should be an integer between 0 and 2')

    if (direction != 0)
        (sum(as.numeric(apply(table, direction, max))) - ifelse(direction == 1, max(colSums(table)), max(rowSums(table)))) / (sum(table) - ifelse(direction == 1, max(colSums(table)), max(rowSums(table))))
    else
        list(row=lambda.test(table, 1), col=lambda.test(table, 2))

}