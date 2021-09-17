
#Load/Import dataset.

complete = read.csv("complete.csv",header = T)


#add one column for split data.

complete$number = c(1:4692)
Train = subset(complete,complete$number < 3754)
Test = subset(complete,complete$number > 3754)


# Apply glm(logistic regression).

glm_diff = setdiff(colnames(complete),list('number'))
glm_formula = as.formula(paste('Total.Confirmed.cases == "Name.of.State...UT"',paste(glm_diff,collapse = '+'),sep = '~'))
glm_mode = glm(glm_formula,data = Train)
glm_pred = predict(glm_mode)


# Apply lm(linear regression).

lm_diff = setdiff(colnames(complete),list('number'))
lm_formula = as.formula(paste('Total.Confirmed.cases == "Name.of.State...UT"',paste(glm_diff,collapse = '+'),sep = '~'))
lm_mode = lm(glm_formula,data = Train)
lm_pred = predict(glm_mode)


Train$Death= ifelse(Train$Death == "0#",0,Train$Death)
lm_formula_Two = as.formula("Death ~ Name.of.State...UT")
lm_model_Two = lm(lm_formula_Two,data = Train)

# most death with state.

classi = subset(complete,complete$Name.of.State...UT == complete$Name.of.State...UT)
done = subset(classi,classi$Death == max(classi$Death))


# Check new cases with state.

define_city = subset(complete,complete$Name.of.State...UT == complete$Name.of.State...UT)
define_newcase_satewise = subset(define_city,define_city$New.cases == max(define_city$New.cases))


# return state which is max recovered.

city = subset(complete,complete$Name.of.State...UT == complete$Name.of.State...UT)
Most_recoverd_state = subset(city,city$New.recovered == max(city$New.recovered))


# plotting graph.

library(ggplot2)
ggplot(complete) + geom_histogram(aes(x = Total.Confirmed.cases),color = c("blue"),fill = "black")

ggplot(complete) + geom_bar(aes(x = Death,fill = factor(Name.of.State...UT)),color = "black",stat = "count")+coord_flip()
