{
# Load the necessary libraries
if (!require(readxl)) {
  install.packages('readxl')
}
library(readxl)
if (!require(caret)) {
  install.packages('caret')
}
library(caret)

if (!require(gbm)) {
  install.packages('gbm')
}
library(gbm)

if (!require(ggplot2)) {
  install.packages('ggplot2')
}
library(ggplot2)

if (!require(GGally)) {
  install.packages('GGally')
}
library(GGally)
if (!require(ggcorrplot)) {
  install.packages('ggcorrplot')
}
library(ggcorrplot)

if (!require(ROCit)) {
  install.packages('ROCit')
}
library(ROCit)

if (!require(pROC)) {
  install.packages('pROC')
}
library(pROC)

if (!require(kimisc)) {
  install.packages('kimisc')
}
library(kimisc)

# Load the data frame
{user_input <- NULL
cat('What is the format of your data set? \n 1. Excel (.xlsx) \n 2. Text Tab Delimited (.txt) \n 3. Comma Separated Text (.csv) \n 4. Other\n')
user_input <- as.numeric(readline(prompt = "Please enter a value: "))

if (user_input == 1) {
  df <- read_xlsx(file.choose())
} else if (user_input == 2) {
  df <- read.delim(file.choose())
} else if (user_input == 3) {
  df <- read.csv(file.choose())
} else {
  cat('Please use Excel, text tab delimited, or comma separated text')
}

}

# Select the column of diagnosis
{user_input<-NULL
cat('Which Variable Contains the Diagnosis of the Patient?\n')
print(data.frame(Variables=colnames(df)))
user_input <- as.numeric(readline(prompt = "Please enter a value: "))
df[,user_input]<-as.character(df[,user_input])
DiagCol<-colnames(df)[user_input]
cat('Which is The Control Group?\n')
print(data.frame(Groups=unique(df[,user_input])))
lvl1 <- as.numeric(readline(prompt = "Please enter a value: "))
lvl2 <- as.numeric(c(1,2)[lvl1!=c(1,2)])

df[,user_input]<-factor(df[,user_input],levels =c(unique(df[,user_input])[lvl1],unique(df[,user_input])[lvl2]) )
}



df2<-df[,!colnames(df)%in%DiagCol]
data.frame(variable=colnames(df2),row.names=1:ncol(df2))

cat('\nIs there any variable to ignore for the analysis?\n')
print(data.frame(Variables=colnames(df2)))
print(data.frame(Answer=c('Yes','No')))
YesNo<- as.numeric(readline(prompt = "Please enter a value: "))

if(YesNo==1){
  cat('\nWhich Variables should be removed?\n')
  remo<-unlist(strsplit(readline(prompt = "List All and Separated With Spaces: ")," "))
  remo<-as.numeric(remo)
  df2<-df2[,-remo]
}else{}

YesNo<- NULL
remo<-NULL

# Select the categorical variables
cat('\nIs there any categorical variable aside of the diagnosis?\n')
print(data.frame(Variables=colnames(df2)))
print(data.frame(Answer=c('Yes','No')))
YesNo<- as.numeric(readline(prompt = "Please enter a value: "))


if(YesNo==1){
  cat('\nWhich is/are categorical variable(s)?\n')
  catego<-unlist(strsplit(readline(prompt = "List All and Separated With Spaces: ")," "))
  catego<-as.numeric(catego)
  NotCatego<-c(1:ncol(df2))[!c(1:ncol(df2))%in%catego]
  for(i in catego){
    df2[,i]<-as.factor(df2[,i])
  }
  
  for(i in NotCatego){
    df2[,i]<-as.numeric(df2[,i])
  }
  
  YesNo<-NULL
}else{
  NotCatego<-1:ncol(df2)
  for(i in NotCatego){
    df2[,i]<-as.numeric(df2[,i])
  }
}



print(data.frame(variable=colnames(df2),row.names=1:ncol(df2)))

# Fill Missing Values With K-nearest neighbors
dfgbm <- preProcess(df2, method = c("center", "scale", "knnImpute"), k = 2)                      
dfknn<-predict(dfgbm , df2)
for (i in 1:ncol(df2)) {
  df2[is.na(df2[,i]),i]<-dfknn[is.na(df2[,i]),i]
  
}

# Create a correlation matrix to filter intercorrelated variables

cor_matrix <- cor(df2[,-catego], use = "complete.obs")


print(ggcorrplot(cor_matrix, method = "circle", lab_size = 3, 
           color = c("dodgerblue", "white", "tomato"), title = "Correlation Matrix of df2"))
ggsave('corrplot.png') # Save correlation plot

cor_long <- as.data.frame(as.table(cor_matrix))

# Rename the columns for clarity
colnames(cor_long) <- c("Variable1", "Variable2", "Correlation")

# Filter out the diagonal (self-correlation) and lower triangle to avoid duplicates
cor_long <- cor_long[abs(cor_long$Correlation)>=0.4, ] # Adjust the value for desired threshold. Note: 0.4 is the suggested threshold 

if(nrow(cor_long)==0){
  cat('\nNo Variables are highly correlated\n')
}else{
  # Filter to find highly correlated pairs
  highly_correlated_pairs <- cor_long[abs(cor_long$Correlation) != 1, ]
  
  # Print the highly correlated pairs
  print(highly_correlated_pairs)
  cat('\nThe following are the Highly correlated pairs (>0.5)\n\n')
  highly_correlated_vars<-unique(c(highly_correlated_pairs$Variable1,
                                   highly_correlated_pairs$Variable2))
 
  cat('\nDo you want to remove any of the variables following?\n')
  print(data.frame(Variable=highly_correlated_vars))
  print(data.frame(Answer=c('Yes','No')))
  YesNo<-NULL
  remo<-NULL
  YesNo<- as.numeric(readline(prompt = "Please enter a value: "))
  
  if(YesNo==1){
    cat('\nWhich Variables should be removed?\n')
    remo<-unlist(strsplit(readline(prompt = "List All and Separated With Spaces: ")," "))
    remo<-as.numeric(remo)
    remo<-highly_correlated_vars[remo]
    df2<-df2[,!colnames(df2)%in%remo]
  }else{}
  
  
}


df2<-data.frame(Diagnosis=df[,DiagCol],df2) #merge the priviously removed column
form<-as.formula(paste0('Diagnosis','~.'))  # Generate the formula for the gbm


YesNoGbm<-1

# Gradient Boosting Machine
# Note: This may require further removal of confounding variables. A manual selection of the variables to remove was added here.
repeat{
  if(!YesNoGbm==2){
    gbmmodeldf2<-train(form,data=na.omit(df2),method='gbm')
    sumar<-summary(gbmmodeldf2)
    rownames(sumar)<-1:nrow(sumar)
    print(sumar)
    cat('\nSometimes Confounding variables introduce noise into the model \n Do you want to re-run the gbm model after removing any variable?\n')
    print(data.frame(Answer=c('Yes','No')))
    YesNo<-NULL
    remo<-NULL
    YesNoGbm<- as.numeric(readline(prompt = "Please enter a value: "))
    if(!YesNoGbm==2){
      cat('\nWhich Variables should be removed?\n')
      print(data.frame(Variable=colnames(df2)[-1]))
      remo<-unlist(strsplit(readline(prompt = "List All and Separated With Spaces: ")," "))
      remo<-as.numeric(remo)
      remo<-colnames(df2)[remo+1]
      df2<-df2[,!colnames(df2)%in%remo]
    }else{}
  }else{break}
}

print(sumar)
cat('\nThe next step is to generate a logistic model \n Which Variables should do you want to include in the logistic model?\n')
print(data.frame(Variable=colnames(df2)[-1]))
logVar<-unlist(strsplit(readline(prompt = "List All and Separated With Spaces: ")," "))
logVar<-as.numeric(logVar)
logVar<-colnames(df2)[logVar+1]
LogMdf2<-df2[,colnames(df2)%in%c('Diagnosis',logVar)]

LogMdf2<-na.omit(LogMdf2)# removing data points with missing values

form<-NULL
xs<-paste0(colnames(LogMdf2)[2:ncol(LogMdf2)],collapse = '+')

form<-as.formula(paste0('Diagnosis','~',xs)) #Creating the formula based on the variables selected
logmod<-glm(form,data=LogMdf2,family = 'binomial') # Logistic model
print(summary(logmod)$coef)


## Creating the Rocit Object
class <- logmod$y
score <- predict(logmod,newdata=LogMdf2,type = 'response')

rocit_emp <- rocit(score = score, 
                   class = class, 
                   method = "emp")
print(summary(rocit_emp)) # Getting the AUC
optimalCutoff<-rocit_emp$Cutoff[which((rocit_emp$TPR-rocit_emp$FPR)==max(rocit_emp$TPR-rocit_emp$FPR))] 
print(optimalCutoff) # getting the optimal Cutoff

lmodcoefs<-data.frame(summary(logmod)$coef)
lmodcoefs$OptimalCutoff<-""
lmodcoefs$OptimalCutoff[1]<-as.character(optimalCutoff)
write.csv(lmodcoefs,'coeficients_and_cutoff.csv') # Saving the Coeficients

# Saving the pretty ROC curve
png(file="pretty_ROC_Plot.png",
    width=619, height=701)
plot(rocit_emp, col = c(1,"gray50"), 
     legend = FALSE, YIndex = FALSE)
dev.off()

n<-NA
n<-factor(n)
levels(n)<-levels(LogMdf2$Diagnosis)

n[score>=optimalCutoff]<-levels(n)[1]
n[score<optimalCutoff]<-levels(n)[2]

#Saving the Sensitivity, Specificity, PPV, and NPV

SensiSpec<-data.frame(Variables=c('Sensitivity','Specificity','PPV','NPV'),
                      Values=c(sensitivity(data = n,reference = as.factor(LogMdf2$Diagnosis),negative=levels(n)[1],positive = levels(n)[2]),
specificity(data = n,reference = as.factor(LogMdf2$Diagnosis),negative=levels(n)[1],positive = levels(n)[2]),
posPredValue(data = n,reference = as.factor(LogMdf2$Diagnosis),negative=levels(n)[1],positive = levels(n)[2]),
negPredValue(data = n,reference = as.factor(LogMdf2$Diagnosis),negative=levels(n)[1],positive = levels(n)[2])))
print(SensiSpec)
write.csv(SensiSpec,'Sensitivity_specificity_PPV_NPV.csv')

#Creating an optimism adjusted plot

auc.adjust <- function(data, fit, B){
  fit.model <- fit
  data$pred.prob <- fitted(fit.model)
  auc.app <- roc(data[,1], data$pred.prob, data=data)$auc # require 'pROC'
  auc.boot <- vector (mode = "numeric", length = B)
  auc.orig <- vector (mode = "numeric", length = B)
  o <- vector (mode = "numeric", length = B)
  for(i in 1:B){    
    boot.sample <- sample.rows(data, nrow(data), replace=TRUE) # require 'kimisc'
    fit.boot <- glm(formula(fit.model), data = boot.sample, family = "binomial")
    boot.sample$pred.prob <- fitted(fit.boot)
    auc.boot[i] <- roc(boot.sample[,1], boot.sample$pred.prob, data=boot.sample)$auc
    data$pred.prob.back <- predict.glm(fit.boot, newdata=data, type="response")
    auc.orig[i] <- roc(data[,1], data$pred.prob.back, data=data)$auc
    o[i] <- auc.boot[i] - auc.orig[i]
  }
  auc.adj <- auc.app - (sum(o)/B)
  png(file="pretty_Optimism_Adjusted_AUC_Plot.png",
      width=619, height=701)
  boxplot(auc.boot, auc.orig, names=c("auc.boot", "auc.orig"))
  title(main=paste("Optimism-adjusted AUC", "\nn of bootstrap resamples:", B), sub=paste("auc.app (blue line)=", round(auc.app, digits=4),"\nadj.auc (red line)=", round(auc.adj, digits=4)), cex.sub=0.8)
  abline(h=auc.app, col="blue", lty=2)
  abline(h=auc.adj, col="red", lty=3)
  dev.off()
}

auc.adjust(LogMdf2,logmod,1000)
cat(paste0('Output files were saved in the following folder: ', getwd())) #displaying the folder where the files were saved
}
