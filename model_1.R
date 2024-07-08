setwd("C:/Internship")
print(getwd())

# load packages
require(FactoMineR)
require(ggplot2)
require(factoextra)

#load data
data<-read.csv("demographic.csv")

#selecting reuired columns
dem = data[,c("score",	"stay",	"travel_mode",	"travel_dur",	"cont_del",	"exam_mode",	"tech_org",	"extra_curr")]

#categorization into how many unique categories
cats = apply(dem, 2, function(x) nlevels(as.factor(x)))
cats


#prints current datatype
print(sapply(dem, class))

#converting multiple columns into factor datatype

dem[, c("score",	"stay",	"travel_mode",	"travel_dur",	"cont_del",	"exam_mode",	"tech_org",	"extra_curr")] <- lapply(dem[, c("score",	"stay",	"travel_mode",	"travel_dur",	"cont_del",	"exam_mode",	"tech_org",	"extra_curr")], factor)

# apply MCA
mca1 = MCA(dem, graph = TRUE)

# table of eigenvalues
mca1$eig

# data frames for ggplot
mca1_vars_df = data.frame(mca1$var$coord, Variable = rep(names(cats), 
                                                         cats))
mca1_obs_df = data.frame(mca1$ind$coord)

# plot of variable categories
ggplot(data = mca1_vars_df, aes(x = Dim.1, y = Dim.2, label = rownames(mca1_vars_df))) + 
  geom_hline(yintercept = 0, colour = "gray70") + geom_vline(xintercept = 0, 
                                                             colour = "gray70") + geom_text(aes(colour = Variable)) + ggtitle("MCA plot of variables using R package FactoMineR")

# extract variable names and labels
dim_names <- dimdesc(mca1)

# print results
dim_names
dim_names$`Dim 1`

fviz_screeplot(mca1, addlabels = TRUE, ylim = c(0, 20))

print(mca1$var$contrib)

fviz_mca_biplot(mca1, 
                repel = TRUE, # Avoid text overlapping (slow if many point)
                ggtheme = theme_minimal())

fviz_mca_var(mca1, choice = "mca.cor", 
             repel = TRUE, # Avoid text overlapping (slow)
             ggtheme = theme_minimal())


fviz_mca_var(mca1, 
             repel = TRUE, # Avoid text overlapping (slow)
             ggtheme = theme_minimal())

fviz_mca_var(mca1, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE, # Avoid text overlapping
             ggtheme = theme_minimal())
# Cos2 of variable categories on Dim.1 and Dim.2
fviz_cos2(mca1, choice = "var", axes = 1:2)

# Contributions of rows to dimension 1
fviz_contrib(mca1, choice = "var", axes = 1, top = 26)
# Contributions of rows to dimension 2
fviz_contrib(mca1, choice = "var", axes = 2, top = 26)

#individual contribution 
fviz_mca_ind(mca1, 
             label = "none", # hide individual labels
             habillage = "score", # color by groups 
             palette = c("#00AFBB", "#E7B800","red","blue","grey"),
             addEllipses = TRUE, ellipse.type = "confidence",
             ggtheme = theme_minimal()) 

mca_dims <- as.data.frame(mca1$ind$coord)
mca_dims

mca_dims$fe_score <- data$fe_score 

print(sapply(mca_dims, class))

#mca_dims$fe_score <- as.factor(mca_dims$fe_score)
mca_dims


split <- sample.split(mca_dims$fe_score, SplitRatio = 0.75)

#get training and test data
demtrain <- subset(mca_dims, split == TRUE)
demtest <- subset(mca_dims, split == FALSE)

demtest

#length(demtest$)

#logistic regression model
model <- glm(demtrain$fe_score ~ .  , data = demtrain, family = binomial, na.action = "na.exclude")
summary(model)
length(demtrain$fe_score)
length(mca_dims$`Dim 1`)

# Generate predicted probabilities for test data
predicted_probs <- predict(model, newdata = demtest, type = "response")
predicted_prob


# Convert predicted probabilities to predicted binary outcomes
predicted <- ifelse(predicted_probs > 0.5, 1, 0)
predicted

# Calculate accuracy of predicted vs. actual data
accuracy <- mean(predicted == demtest$fe_score)

# Print accuracy
accuracy





#Regenerate

