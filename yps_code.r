library(readr)
library(Amelia)
library(magrittr)
suppressMessages(library(dplyr))
library(ggplot2)
suppressMessages(library(tidyr))

responses <- read_csv("C:/Users/meghana/Desktop/dm/dm project/responses.csv")
hni = responses[, 32:63] #hobbies and interests
gen = responses[, 145] #genders
#gen = ifelse(gen == "male", 0, 1) #converting categorical to binary data
data = cbind(hni, gen)

#data visualization
# Helper functions
analyze_grp_diff <- function(dafr, group, start, end) {
  avgs_group <-  dafr %>% 
    dplyr::rename_(group = group) %>% 
    select_("group", paste0("`", start, "`:`", end,"`")) %>% 
    dplyr::group_by(group) %>% 
    dplyr::summarise_all(mean, na.rm = TRUE) %>% 
    na.omit
  
  vars_by_difference <- avgs_group %>% 
    dplyr::select(-group) %>% 
    apply(2, function(x) x[1] - x[2]) %>% 
    sort %>% 
    names
  
  avgs_group %>% 
    tidyr::gather(Variable, `average response`, -group) %>% 
    ggplot(aes(x = Variable, y = `average response`, group = group, colour = group)) + 
    geom_point(size = 5) + 
    scale_x_discrete(limits = vars_by_difference) +
    ylim(1, 5) +
    geom_hline(yintercept = 3) +
    coord_flip() + 
    theme(axis.text.y = element_text(face="bold", color="Black", size=12),
          legend.position="right")
  
}

dafr = data
dafr %>% analyze_grp_diff("Gender", "History", "Pets")

#create train and test data
rnum = nrow(gen)
t = rnum * 0.7 #train set
train = data[1:t,]
test = data[t:rnum,]
gen = ifelse(gen == "male", 0, 1) #converting categorical to binary data
#gen = as.numeric(gen)
data = cbind(hni, gen)

#Find missing data
sapply(data,function(x) sum(is.na(x)))
missmap(data, main = "Missing values vs observed")

#replace missing data
for (i in 1:ncol(data)){
  data[,i][is.na(data[,i])] <- round(mean(data[,i],na.rm=T))
}

train = data[1:t,]
test = data[t:rnum,]

#generalized linear model
fit <- glm(train$Gender~.,data=train,family=gaussian())
anova(fit, test="Chisq")
predict = predict(fit, type = "response")
fitted.results <- predict(fit,test,type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != test$Gender)
accur = 1-misClasificError
print(paste('Accuracy', accur*100,'%'))
