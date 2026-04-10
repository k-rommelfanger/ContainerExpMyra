# Separate Container Experiment (Nursery) R Script since I can't figure out if Rmd is 
# throwing me off idk

library(readr)
library(tidyverse)
library(lubridate)
library(vroom)
library(readxl)



nurserycontainerexp <- read_csv("E0_ RHMA Container Nursery Experiment Master Data - Organized Data - AJH Edit.csv")

View(nurserycontainerexp)

Date <- mdy(nurserycontainerexp$`Date Processed`)
nurserycontainerexp$`Monitoring Session`<- as.factor(nurserycontainerexp$`Monitoring Session`)
nurserycontainerexp$Location <- as.factor(nurserycontainerexp$Location)
nurserycontainerexp$Treatment<- as.factor(nurserycontainerexp$Treatment)
nurserycontainerexp$`Dead/Alive/Dying` <- as.factor(nurserycontainerexp$`Dead/Alive/Dying`)
nurserycontainerexp$`Epicotyl length (cm)` <- as.numeric(nurserycontainerexp$`Epicotyl length (cm)`)
#random B G and NA labels in the epicotyl length
nurserycontainerexp$`Total leaf count` <- as.numeric(nurserycontainerexp$`Total leaf count`)
view()
unique(nurserycontainerexp$`Epicotyl length (cm)`)
 # Setting all of the main metrics as factors for future analysis                 
str(nurserycontainerexp)

growth <- nurserycontainerexp %>% group_by (`Monitoring Session`, Treatment) %>%
  summarise(mean.growth = mean(`Epicotyl length (cm)`, na.rm = TRUE),
            growth.SEM = sd((`Epicotyl length (cm)`/sqrt(length(`Epicotyl length (cm)`))), na.rm= TRUE))
View(growth)

leaf <- nurserycontainerexp %>% group_by(`Monitoring Session`, Treatment) %>%
  summarise(mean.leaf.count = mean(`Total leaf count`, na.rm = TRUE),
            leaf.count.SEM = sd((`Total leaf count`/sqrt(length(`Total leaf count`))), na.rm = TRUE))
View(leaf)

# Data still to be cleaned, NAs present in Treatment and Epicotyl length columns, but I just wanted to find something

library(dplyr)

#Exploratory plots (data still needs to be cleaned)
# regression plot 

###########
#Total data


#regression leaf count vs node count
ggplot(data = nurserycontainerexp, aes(x=`Total leaf count`, y=`Epicotyl length (cm)`, color = Treatment)) +
  geom_point(alpha = 0.3, position = position_jitter(width = 0.1, height = 0)) +
  geom_smooth(se=FALSE, method = "lm")

## BELOW IS ORIGINAL CODE THAT HAS NOT BEEN ADAPTED FOR OVERALL NURSERY DATA


#Height by treatment
boxplot(Epicotyl.length~Treatment,data=container.growth.data,
        xlab="Treatment",
        ylab="Epicotyl length (cm)",
        col = c("dodgerblue", "goldenrod", "tan4"),
        names = c("Biobag","Conetainer","Tree pot"),
        par(cex.lab=1.5),
        par(cex.axis=1.5))

#Number of leaves by treatment
boxplot(Main.leaf.count~Treatment,data=container.growth.data,
        xlab="Treatment",
        ylab="Leaf count",
        col = c("dodgerblue", "goldenrod", "tan4"),
        names = c("Biobag","Conetainer","Tree pot"),
        par(cex.lab=1.5),
        par(cex.axis=1.5))

#Number of leaf nodes by treatment
boxplot(Main.node.count~Treatment,data=container.growth.data,
        xlab="Treatment",
        ylab="Leaf node count",
        col = c("dodgerblue", "goldenrod", "tan4"),
        names = c("Biobag","Conetainer","Tree pot"),
        par(cex.lab=1.5),
        par(cex.axis=1.5))


#Survival
#Total data survival, three categories
ggplot(data = container.growth.data, aes(x=Treatment, fill=Survival)) +
  geom_bar() +
  scale_x_discrete(labels=c("Biobag","Conetainer","Tree pot")) +
  scale_fill_manual(labels = c("Dead", "Dying", "Alive"),
                    values = c("#3b1704", "goldenrod","olivedrab4")) +
  ylab("Propagule count") +
  theme(text = element_text(size = 18))  +
  theme(axis.text = element_text(size = 16)) +
  theme(legend.text = element_text(size = 16)) 

#Decapitated propagules removed, three survival categories
ggplot(data = container.data.nodecaps, aes(x=Treatment, fill=Survival)) +
  geom_bar() 

#Decapitated propagules removed, two survival categories
ggplot(data = container.data.nodecaps, aes(x=Treatment, fill=Survival.f)) +
  geom_bar() 
``

#One-way ANOVA tests
#Growth
growth.lm <- lm(`Epicotyl length (cm)` ~ Treatment, data = nurserycontainerexp)
summary(growth.lm) # p = 0.121
library(mosaic)
TukeyHSD(growth.lm) #p values
library(emmeans)
emmeans(growth.lm, "Treatment") #confidence limits

#Leaf count
leaf.count.lm <- lm(Total.leaf.count ~ Treatment, data = container.growth.data)
summary(leaf.count.lm) # p = 0.709
leaf.count2.lm <- lm(Main.leaf.count ~ Treatment, data = container.growth.data)
summary(leaf.count2.lm) # p = 0.234

#Node count
node.count.lm <- lm(Total.node.count ~ Treatment, data = container.growth.data)
summary(node.count.lm)  # p = 0.301
node.count2.lm <- lm(Main.node.count ~ Treatment, data = container.growth.data)
summary(node.count2.lm)  # p = 0.026
TukeyHSD(node.count2.lm)
```


```{r}```

PCA
```{r}
library(tidyverse)
container.PCA <- read_csv("E:/Documents/University of the Virgin Islands/Data analysis/Container experiment/Container.PCA.csv",
                          col_types = cols(Tank = col_factor(levels = c("10A", "10B", "10C", "11A", "11B", "11C")),
                                           Treatment = col_factor(levels = c("BB", "CC", "TP")),
                                           Survival = col_factor(levels = c("D", "DY", "A"), ordered = TRUE)))

library(ggfortify)
library(factoextra) #This package allows for visualization and interpretation of PCA
library("FactoMineR")
library("corrplot")

data(container.PCA)
head(container.PCA, 3)

envr<-(container.PCA[,c(1:3)])#columns with growth parameters 1:3 or 1:5
envr.habitat<-container.PCA[,6] #column with habitat
envr.pca<-prcomp(envr,
                 center = TRUE,
                 scale. = TRUE)
print(envr.pca)
summary(envr.pca)

get_eigenvalue(envr.pca) #81.627% of variation is explained by the first two eigenvalues
fviz_eig(envr.pca)#Scree plot
fviz_pca_var(envr.pca, col.var = "cos2"
             ,gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
             ,repel = TRUE) #visualize the cos2 of variables of the PCA
fviz_pca_var(envr.pca, col.var = "contrib"
             ,gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
             ,repel = TRUE) #visualize the most important or contributing variables of the PCA
var <- get_pca_var(envr.pca)
head(var$cos2)   

corrplot(var$contrib, is.corr = FALSE)
fviz_pca_ind(envr.pca,
             geom.ind = "point", # show points only (nbut not "text")
             col.ind = container.PCA$Treatment, # color by groups
             addEllipses = TRUE, # Concentration ellipses
             legend.title = "Treatment"
)

#plot PCA with arrows
PC<-autoplot(envr.pca
             ,data = container.PCA
             ,colour = 'Treatment'
             ,loadings = TRUE
             ,loadings.colour = 'black'
             ,loadings.label = TRUE
             ,loadings.label.size = 5
             ,loadings.label.colour = 'black'
             ,loadings.label.repel = T #moves label overlap so that they can be read
             #,scale_colour_discrete(values = c("black","red","blue","green"))
             ,frame = TRUE
             ,frame.type = 'norm'
)

PC + theme_classic() #Get rid of gridlines


#View loadings
loadings<-envr.pca$rotation * envr.pca$sdev
loadings
```


Survival binomial analysis
```{r}
library(car)
#Make survival factor binary
container.growth.data$Survival2 <- recode(container.growth.data$Survival,"c('D', 'DY') = 'Dead'; else = 'Alive'")
container.growth.data$Survival3 <- recode(container.growth.data$Survival,"c('D') = 'Dead'; else = 'Alive'")

#If dying counts as dead
survival.dead.model <- glm(Survival2 ~ Treatment, family="binomial", data = container.growth.data)
summary(survival.dead.model)
Anova(survival.dead.model, type = 3) #p =  0.1912

#If dying counts as alive
survival.alive.model <- glm(Survival3 ~ Treatment, family="binomial", data = container.growth.data)
summary(survival.alive.model)
Anova(survival.alive.model, type = 3) #p =  0.7463


#Decapitated propagules removed
container.data.nodecaps$Survival2 <- recode(container.data.nodecaps$Survival,"c('D', 'DY') = 'Dead'; else = 'Alive'")
container.data.nodecaps$Survival3 <- recode(container.data.nodecaps$Survival,"c('D') = 'Dead'; else = 'Alive'")

#If dying counts as dead
survival.nodecap.dead.model <- glm(Survival2 ~ Treatment, family="binomial", data = container.data.nodecaps)
summary(survival.nodecap.dead.model)
Anova(survival.nodecap.dead.model, type = 3) #p =  0.3041

#If dying counts as alive
survival.nodecap.alive.model <- glm(Survival3 ~ Treatment, family="binomial", data = container.data.nodecaps)
summary(survival.nodecap.alive.model)
Anova(survival.nodecap.alive.model, type = 3) #p =  0.6244
```


Ordinal analysis
```{r}
library(MASS)
survival.model = polr(Survival ~ Treatment , data = container.growth.data, Hess = TRUE)
summary(survival.model)

#store table
(ctable <- coef(summary(survival.model)))

# calculate and store p values
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2

# combined table
(ctable <- cbind(ctable, "p value" = p))








