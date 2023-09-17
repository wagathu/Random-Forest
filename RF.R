
library(pacman)
p_load(caTools, randomForest, readxl, dplyr, party, rpart, rpart.plot, ggplot2)

pima <- read.csv(("E:/Desktop/Random Forest/pima.csv"))
head(pima)
pima <- pima|>
  mutate(Outcome = factor(Outcome, levels = c(0,1), labels = c("Diabetic", "NotDiabetic")))|>
  na.omit()

glimpse(pima)

#Splitting the data into training and testing data set
set.seed(123)
split.size = .7
sample.size = floor(split.size * nrow(pima))

indi <- sample(seq_len(nrow(pima)), size = sample.size)

Train <- pima[indi,]
Test <- pima[-indi,]
glimpse(Train)

# Fitiing a random Forest 
set.seed(123)
classifier_RF <- randomForest(x =Train[-9], y = as.factor(Train$Outcome), ntree = 500 , importance = TRUE)
y_pred <- predict(classifier_RF, newdata = Test[-9])
summary(classifier_RF)
plot(classifier_RF)

imp <- importance(classifier_RF, type = 1)
featureImportance <- data.frame(Feature=row.names(imp), Importance=imp[,1])

p <- ggplot(featureImportance, aes(x=reorder(Feature, Importance), y=Importance)) +
  geom_bar(stat="identity", fill="#53cfff") +
  coord_flip() + 
  theme_light(base_size=20) +
  xlab("") +
  ylab("Importance") + 
  ggtitle("Random Forest Feature Importance\n") +
  theme(plot.title=element_text(size=18))
# Decision Tree
fit <- rpart(formula = Outcome ~., data = Train, method = 'class')
summary(fit)
rpart.plot(fit,5, extra = 106,tweak = 1.5,
           box.palette = c("royalblue", "green","red","brown","pink"),
           shadow.col = "gray")

#CPI Data

cpi <- read_excel("E:/Documents/R-Studio Programms/Machine Learning/cpi.xlsx")
cpi <- cpi |>
  na.omit()
head(cpi)
cpi <- cpi |>
  mutate(date = as.yearmon(date))

fit <- rpart(CPI ~ funding_rate + CDS + exchange_rate, data = cpi, method = "anova")
j <- summary(fit)
j$variable.importance
j$frame

rpart.plot(fit, 5, extra = 100, box.palette = c("royalblue", "green","gray"), shadow.col = "grey")




# The Project Data set
# 1. Decision tree

student <- read_excel("E:/Documents/R-Studio Programms/Stat Project/Reg.xlsx")
student <- student |>
  filter(`Entry Grade` != 'B') |>
  filter(`Entry Grade` != 'B-')|>
  select(,c(1,3,4,5,6))|>
  mutate(Gender = factor(ifelse(Gender == "Male",1,0), 
                         levels = c(0,1), labels = c("Female","Male")))|>
  mutate(`Entry Grade`  = factor(ifelse(`Entry Grade`  == 'A-',1,0), 
                         levels = c(0,1), labels = c('B+','A-')))|>
  mutate(Sponsor  = factor(ifelse(Sponsor  == "HELB",1,0), 
                                 levels = c(0,1), labels = c("SELF-Sponsored","HELB")))|>
  mutate(Graduated  = factor(ifelse(Graduated  == "YES",1,0), 
                                 levels = c(0,1), labels = c("NO","YES")))|>
  mutate(Course  = factor(ifelse(Course  == "BSc. Statistics",1,0), 
                             levels = c(0,1), labels = c("BSc. Actuarial","BSc. Statistics")))

# Splitting the data
set.seed(123)

SAM <- .7
SAMSIZE <- SAM * nrow(student)

CIES <- sample(seq_len(nrow(student)), size = SAMSIZE)

TRAIN <- student[CIES,]
TEST <- student[-CIES,]


tree <- rpart(Graduated ~., method = 'class', data = student)
rpart.plot(tree)

# 2. Random forest
x = student[-4]
y = student$Graduated
Random <- randomForest(x, y, ntree = 500)
importance(Random)



























