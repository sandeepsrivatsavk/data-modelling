#Data Cleaning
Flipkart_scrapped <- read.csv(file.choose())
flipkart = Flipkart_scrapped

library(tidyr)

f_name <- data.frame(Flipkart_scrapped$Name)
df <- separate(f_name, Flipkart_scrapped.Name, c("Brand", "Model"),sep = "\\s",extra = "merge")

flipkart$Name=df

a = flipkart

Dis <- data.frame(flipkart$Description)
ROM_split <- separate(Dis,flipkart.Description,c("ROM","Other_description"),sep="ROM",extra="merge")

b = ROM_split$ROM

# Filtering only the ROM from the entire column
b = gsub(".*RAM |", "", b)
b <- regmatches(b, gregexpr("[[:digit:]]+", b))
b <- as.numeric(as.character(b))
ROM_split$ROM = b

# including the new column "ROM" to the original dataframe
a$Description = NULL
a$ROM = ROM_split$ROM
a$Description = ROM_split$Other_description

flipkart <- a
write.csv(flipkart,file="flipkart_clean.csv")
flipkart <- read.csv(file.choose())
flipkart <- na.omit(flipkart)
View(flipkart)
str(flipkart)


#Regression Analysis

head(flipkart)
summary(flipkart)

#Straight line regression graph
regression_graph = lm(ROM ~ Cost.in.Rupees., data=flipkart)
plot(flipkart$ROM ~ flipkart$Cost.in.Rupees., pch = 16, col = 59,
     main="Regression Line (ROM vs Cost In Rupees)",col.main=34, col.lab="Blue",
     xlab="Cost In Rupees",ylab="ROM")
abline(regression_graph,col="red")

#Histogram plot
h=hist(flipkart$ROM, main="ROM Size",xlab="ROM",ylim=c(0,100),col="darkmagenta",border="green")
text(h$mids,h$counts,labels=h$counts, adj=c(0.5, -0.5),cex=0.8)

#Density plot
model <- table(flipkart$Screen.size)
p <- density(model,bw = 3)
plot(p, main= "Density plot for Screen Size",)
polygon(p, col="red", border="blue")
rug(jitter(model))

#Box plot
boxplot(flipkart$Cost.in.Rupees.,
        col="orange", col.main="darkmagenta", col.lab="Blue" ,col.axis=34,
        main="BoxPlot for cost of cell phones", ylab="cost")

#Normal Probability Plot
qqnorm(flipkart$Screen.size,
       col="darkgreen", col.main="darkorchid3",col.lab="darkgreen",col.axis=34,
       main="Normal probability plot", xlab="Size",
       pch=16,font=2, cex=1,font.lab=2)



summary(flipkart)
w <- data.frame(flipkart$Cost.in.Rupees., flipkart$ROM, flipkart$Screen.size)
names(w)[1] <- 'Cell Phone Cost'
names(w)[2] <- 'Cell Phone Memory'
names(w)[3] <- 'Cell Phone Screen Size'
w <- na.omit(w)
cor_ob=cor(w)
library(ggplot2)
library(ggcorrplot)
options(digits = 3, width = 300)
ggcorrplot(cor_ob,hc.order = TRUE, type = "lower",outline.col = "white",
           ggtheme = theme_gray,lab = TRUE) + ggtitle("Cell phone on Flipkart")



#Regularization Practices

install.packages("glmnet")
library(glmnet)

flipkart <- read.csv(file.choose())


flipkart_reg <- flipkart[,c(4,5)]
View(flipkart_reg)
x = as.matrix(flipkart_reg)
y = flipkart_reg[,1]


n=223
set.seed(223)
train_rows = sample(1:n, 0.9*n)

x.train=x[train_rows,]
x.test=x[-train_rows,]
y.train=y[train_rows]
y.test=y[-train_rows]

# LASSO
best_lambda_LASSO = cv.glmnet(x.train, y.train, type.measure = "mse", alpha=1)  
best_lambda_LASSO   # to find best lambda value
plot(best_lambda_LASSO)

predicted_LASSO = predict(best_lambda_LASSO, s=best_lambda_LASSO$lambda.1se,newx=x.test)
predicted_LASSO

LASSO_accuracy = mean((y.test/predicted_LASSO)*100)
LASSO_accuracy

# Ridge
best_lambda_ridge = cv.glmnet(x.train, y.train, type.measure = "mse", alpha=0)  
best_lambda_ridge   # to find best lambda value
plot(best_lambda_ridge)

predicted_ridge = predict(best_lambda_ridge, s=best_lambda_ridge$lambda.1se,newx=x.test)
predicted_ridge

ridge_accuracy = mean((y.test/predicted_ridge)*100)
ridge_accuracy

# ElasticNet
best_lambda_elasticNet = cv.glmnet(x.train, y.train, type.measure = "mse", alpha=0.5)  
best_lambda_elasticNet   # to find best lambda value
plot(best_lambda_elasticNet)

predicted_elasticNet = predict(best_lambda_elasticNet, s=best_lambda_elasticNet$lambda.1se,newx=x.test)
predicted_elasticNet

elasticNet_accuracy = mean((y.test/predicted_elasticNet)*100)
elasticNet_accuracy

print(paste(" Accuracy obtained in LASSO:", LASSO_accuracy))
print(paste(" Accuracy obtained in Ridge:", ridge_accuracy))
print(paste(" Accuracy obtained in Elastic Net:", elasticNet_accuracy))


