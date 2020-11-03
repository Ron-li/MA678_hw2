library(rstanarm)


beauty <- read.csv("/Users/amelia/Documents/mssp/MA678/hw1/beauty.csv", header=T)
df <- beauty

#Using the Beauty datasets, we will try to predict the course evaluations given by the students, based on a number of factors. In our dataset, each row corresponds in a score. There are many variables included in the dataset, including some characteristic of the professor (i.e. age, female, minority, beauty, minority) and the class (i.e. nonenglish, lower, course_id).
df$course_id <- as.factor(df$course_id)
df$female <- as.factor(df$female)

# boxplot of course_id vs course evaluation
ggplot(data=df, aes(x=course_id, y=eval)) + geom_boxplot() +
  labs(title="Distribution of course evaluation by course_id", x="Course id", y="Course Evaluation")
# boxplot of female vs course evaluation
ggplot(data=df, aes(x = female, y=eval)) + geom_boxplot()

#eval ~ female + beauty
fit1 <- lm(eval~female + beauty + age + minority + nonenglish + lower + course_id, data = beauty)
summary(fit1)["r.squared"]

#eval~ female + beauty + course_id
fit2 <- lm(eval~female + beauty + age + minority + nonenglish + lower, data = beauty)
summary(fit2)["r.squared"]

#eval~ female + beauty + course_id
fit3 <- lm(log(eval)~female + beauty + age + minority + nonenglish + lower, data = beauty)
summary(fit3)["r.squared"]

#
fit4 <- lm(eval~female + beauty + age + minority + nonenglish + lower + female:beauty + beauty:age, data = beauty)
summary(fit4)["r.squared"]

###The intercept......






