# ZULATO DAVIDE mat.876101 Commented Code

## Question 1
# Describe the most interesting variables by plotting distributions, correlations, co-occurrence. 
rm(list=ls())
## i'll start with loading the datasets
setwd("C:/Users/Davide Zulato/Desktop/Esame Economics for Data Science")
getwd()

# Libraries for this issue
library(tidyverse)
library(naniar) # dealing with NAs
library(ggpubr)
library(lubridate) # useful for modeling dates
library(readr)
library(vip) # Variable importance plots

# Data1
data1 <- read_csv("data1.csv")
str(data1) # 80140 obs. of  6 variables
prop.table(table(data1$si2014)) # Si2014: label. 0 for churn
prop.table(table(is.na(data1$abb14))) # renewal date in 2014 (if renewed)
Prop <- prop.table(table(is.na(data1$abb14)))
library(RColorBrewer)
myPalette <- c("cyan3","firebrick1")
# Distribution of Churners
pie(Prop , labels = c("Renovated: 69%","No renovated: 31%"), border="black", col=myPalette, main= "Renewals in 2014")
data1 <- data1 %>% # not informative
  dplyr::select(...1)
# data1 <- data1[,-1]
# I'll change colnames later with the merged dataset

# an13
an13 <- read.csv("an13.csv")
str(an13) # 87052 obs. of  15 variables
prop.table(table(is.na(an13)))
an13 <- an13 %>%
  dplyr::select(-X,-professione) # as we'll see in ISSUE 3 "professione" is a column of NAs
an13$Age <- 2013-as.numeric(an13$data_nascita) # 2 NAs
an13$Age
table(is.na(an13$Age)) # 2 NAs
hist(an13$Age) # the outlier will be removed in data_nascita, a left skew is still expected for demographical characteristics of Italy
hist(log(an13$Age)) # still skewed

# in13
in13 <- read.csv("in13.csv")
str(in13) # 545085 obs. of  8 variables
prop.table(table(is.na(in13))) # no nas
table(duplicated(in13$CodCliente))

# Merged datset
# this dataset will be useful for predicting churners
# data_merge1 <- merge(data1, an13, by = "id") # Applying merge() function
# Colnames for the id variable must be the same for both datasets
colnames(an13)[1] <- "id"
colnames(data1)[1] <- "id"
churn_data = data1 %>% left_join(an13, on="id") # left_join(): includes all rows in x
str(churn_data) # 80,140 x 17
vis_miss(churn_data,warn_large_data= FALSE) 
# I see NAs for the Gender (They'll be replaced randomly)
# The NAs in ultimo_ingresso 
# Nas in 2014 are churners

# write.csv(churn_data,"churn_data.csv")

## PLOT DISTRIBUTIONS
## Correlations between numeric variables
library(GGally)
churn_data$data_nascita <- as.numeric(churn_data$data_nascita) # Removed 2 rows containing missing values
numeric_variables <- data.frame(churn_data$importo,churn_data$data_nascita)
ggpairs(numeric_variables, title="correlation between numeric variables")
churn_data$si2014 <- as.factor(churn_data$si2014)
ggpairs(churn_data, columns = c(7,14), ggplot2::aes(colour=si2014))   

# select some interesting categorical variables for churning
class(churn_data$si2014)
churn_data$si2014 <- as.factor(churn_data$si2014)
p1 <- ggplot(churn_data, aes(x=nuovo_abb,fill=si2014)) + ggtitle("Had no card in 2012") + xlab("New customer") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p2 <- ggplot(churn_data, aes(x=sesso,fill=si2014)) + ggtitle("Gender") + xlab("Gender") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p3 <- ggplot(churn_data, aes(x=agenzia_tipo,fill=si2014)) + ggtitle("Agency type") + xlab("Agency type") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p4 <- ggplot(churn_data, aes(x= tipo_pag,fill=si2014)) + ggtitle("Payment") + xlab("Payment") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p5 <- ggplot(churn_data, aes(x= agenzia,fill=si2014)) + ggtitle("Agency") + xlab("Agency") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p6 <- ggplot(churn_data, aes(x= sconto,fill=si2014)) + ggtitle("Discount") + xlab("Discount") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
grid.arrange(p1, p2, p3, p4, ncol=2)

# in the dataset we have the price for every visit, i create the total price spent in order to understand
# 
conflict_prefer("summarise", "dplyr")
colnames(in13)[8] <- "id"

freq_visits <- in13 %>%
  group_by(id) %>%
  summarise(freq_visits = n())

#importo_medio <- in13 %>%
#  group_by(id) %>%
#  summarise(importo_medio = mean(importo))

importo_totale <- in13 %>%
  group_by(id) %>%
  summarise(importo_totale = sum(importo))

# merge the dataset with the new variables

churn_data <- churn_data %>% left_join(freq_visits, on='id')
# churn_data <- churn_data %>% left_join(importo_medio, on='id')
churn_data <- churn_data %>% left_join(importo_totale, on='id')
churn_data
colnames(churn_data) <- c("id","Churn","Last_visit","start_date_13","renewal_date_2014","Start_date","Price","Discount_type","Reduction_type","Pay_mode","Organization","Organization_type","Gender","Birth_date","City","zip_code","New_member","Visit_frequency","Total_price")
# churn_data <- churn_data[,-1]
churn_data %>% apply(2, function(x) x %>% is.na() %>% sum())

# Starting Dealing with NAs
na_id <- churn_data %>% 
  filter(is.na(Last_visit)) %>% 
  pull(id)

problem=churn_data %>% 
  filter(id %in% na_id)

str(problem)

write.csv(churn_data,"churn_data.csv")
churn_data <- read.csv("churn_data.csv")
churn_data <- read.csv("prova_churn_data.csv") # saved file with Dummy variables

colnames(churn_data)[2] <- "Churn"
colnames(churn_data)[6] <- "City"
colnames(churn_data)[7] <- "zip_code"
colnames(churn_data)[9] <- "Gender"

# Use gander rather than Boy
churn_data$Gender <- as.numeric(churn_data$Gender) # 1 is Male 0 is Female

# Churn and centrality measures
# This Dataset Has been created in ISSUE 5, run the next code after Issue 5
degree_id <- read.csv("degree_id.csv")[,-1]
vis_miss(degree_id)
hist(degree_id$Degree,xlim = c(0,31),breaks = 50000)
churn_data <- churn_data %>% left_join(degree_id, on='id')
churn_data$Churn <- ifelse(churn_data$Churn==TRUE,0,1)
prop.table(table(churn_data$Churn)) # 0 for churn, 1 retained
vis_miss(churn_data,warn_large_data = F)
colnames(churn_data)
library(dplyr)
churn_data <- churn_data %>% # 
  mutate_all(funs(ifelse(is.na(.), 0, .)))

churn_data %>% select(id,Churn,last_visit,card_start,price,City,zip_code,new_sub,boy,age,card_renewal,no_discount,purchase_group,associate,artist,discounted_pass,student_pass,cash,bank,online_payment,n_visits,n_museums_visited,n_provinces_visited,n_cities_visited,avg_price_payed,total_price_payed,Gender,Degree)
str(churn_data)

Processed_data <- churn_data %>% select(id,Churn,price,City,new_sub,age,no_discount,discounted_pass,student_pass,cash,bank,online_payment,n_visits,total_price_payed,Gender,Degree)
vis_miss(Processed_data,warn_large_data = F)
colnames(Processed_data) <- c("id","Churn","Price","City","New_member","Age","No_discount","Discounted_pass","Student_pass","Cash","Bank","Online_payment","Visit_frequency","Total_price","Gender","Degree")
# write.csv(Processed_data,"Processed_data.csv")
str(Processed_data)
# Useful transformation for prediction
Processed_data <- Processed_data %>% mutate_if(is.logical, as.numeric)

## Question 2
# Do you spot some problem with the variables? Are there any specific problems you should 
# take care about?  
rm(list=ls())
data1 <- read_csv("data1.csv")
colnames(data1)[2] <- "id"
an13 <- read.csv("an13.csv")
colnames(an13)[2] <- "id"
churn_data = data1 %>% left_join(an13, on="id") # left_join(): includes all rows in x

# Load ggplot2
library(ggplot2)

# The mpg dataset is natively available
#head(mpg)

churn_data$data_nascita <- as.numeric(churn_data$data_nascita)
churn_data$si2014 <- as.factor(churn_data$si2014)

# geom_boxplot proposes several arguments to custom appearance
ggplot(churn_data, aes(x=si2014, y=data_nascita)) + 
  geom_boxplot(
    
    # custom boxes
    color="blue",
    fill="blue",
    alpha=0.2,
    
    # Notch?
    notch=TRUE,
    notchwidth = 0.8,
    
    # custom outliers
    outlier.colour="red",
    outlier.fill="red",
    outlier.size=3
    
  )

# replace the outliers with the average of the group
summary(churn_data$data_nascita)

# we can replace the outlier values with the mean of the age. 
length(which(churn_data$data_nascita > 2013 )) # 4 individuals are born after the date of recording
## [1] 26
length(which(churn_data$data_nascita < 1901 )) #10 individuals are born in 1900 

churn_data$data_nascita[churn_data$si2014 == 0 & churn_data$data_nascita >= 2013] <- 
  max(as.numeric(churn_data$data_nascita[churn_data$si2014 == 0]))
churn_data$data_nascita[churn_data$si2014 == 1 & churn_data$data_nascita >= 2013] <- 
  mean(churn_data$data_nascita[churn_data$si2014 == 1])

churn_data$data_nascita[churn_data$si2014 == 0 & churn_data$data_nascita  <= 1901] <- 
  mean(churn_data$data_nascita[churn_data$si2014 == 0])
churn_data$data_nascita[churn_data$si2014 == 1 & churn_data$data_nascita <= 1901] <- 
  mean(churn_data$data_nascita[churn_data$si2014 == 1])

str(churn_data$abb14)
prop.table(table(is.na(churn_data$abb14))) # same structure as churners

# after this imputation, go back to ISSUE1 for creating the Dataset processed

# Question 3
# Analyse the pattern of missing values. Is there any variable you should drop from the analysis?
rm(list=ls())

library(readr)

setwd("C:/Users/Davide Zulato/Desktop/Esame Economics for Data Science")
churn_data = read_csv('churn_data.csv')
data1 <- read_csv("data1.csv")
an13 <- read_csv("data1.csv")
in13 <- read_csv("in13.csv")
head(data1)

library(naniar)
colnames(data1) <- c("num","id","Churn","last_visit","starting_date","renewal_date")
vis_miss(data1,warn_large_data = F)
gg_miss_upset(data1)
# how many missings?
n_var_miss(data1)
gg_miss_upset(data1, nsets = n_var_miss(data1))
gg_miss_fct(data1,renewal_date)
gg_miss_fct(data1,Churn)
# This plot shows a set of rectangles that indicate whether there is a missing element in a column or not.
gg_miss_which(data1)


################################################################################
table(is.na(in13))
# how many missings?
n_var_miss(in13) # no missings in this dataset
# This plot shows a set of rectangles that indicate whether there is a missing element in a column or not.
gg_miss_which(in13)

################################################################################

# colnames(an13) <- c("num","id","Churn","Price","Last_visit","start_date_13","renewal_date_2014")
vis_miss(an13[,-1],warn_large_data = F)
gg_miss_upset(an13)
# how many missings?
n_var_miss(an13)
gg_miss_upset(an13, nsets = n_var_miss(an13))
gg_miss_fct(an13,sesso) # professione da rimuovere
gg_miss_fct(an13,nuovo_abb)
gg_miss_fct(an13,data_nascita)
gg_miss_fct(an13,professione)

# This plot shows a set of rectangles that indicate whether there is a missing element in a column or not.
gg_miss_which(an13)


## Dataset Churn cleaned

churn_data <- read.csv("churn_data.csv")
useful <- churn_data %>% 
  select(-zip_code,-Mean_price)
churn_data <- churn_data[complete.cases(churn_data),]
str(churn_data)
vis_miss(churn_data,warn_large_data = F)
# imputation for gender
set.seed(123)
n.na <- length(churn_data$Gender[is.na(churn_data$Gender)])
churn_data$Gender[is.na(churn_data$Gender)] = sample(x=c("M", "F"), n.na, replace=T)
table(is.na(churn_data$Gender)) # No more Nas

# Dummify some variables
churn_data$student_pass <- ifelse(churn_data$Discount_type=="OFFERTA SCUOLE",1,0)
churn_data$cash <- ifelse(churn_data$Pay_mode=="CONTANTI",1,0)
churn_data$bank <- ifelse(churn_data$Discount_type=="BANCOMAT",1,0)

## ISSUE 4 
# Can you cluster the observations? Is there a cluster with most churners?
# prova SOM e poi K-means

rm(list=ls())

library(factoextra)
library(ggplot2)
library(tidyverse)
library(hrbrthemes)
library(ggpubr)
library(kohonen)


setwd("C:/Users/Davide Zulato/Desktop/Esame Economics for Data Science")

data1 <- read_csv("data1.csv")
data1 <- data1[,-1]
colnames(data1) <- c("id", "Churn", "last_visit",
                     "card_start_13", "card_renewal_2014")

Data <- read.csv("Processed_data.csv") #data merged and pre-processed


cluster_data <- Data %>%
  select(-City, -id, -Churn,-X) # i exclude the target variable for an unsupervised exercise

set.seed(123)
# scale data
cluster_data <- cluster_data %>%
  apply(2, function(x){
    return((x - min(x))/(max(x)-min(x)))
  }) %>% as_tibble() %>% sample_n(5000)

# let's try to select the best number of k
fviz_nbclust(cluster_data, kmeans, method = "wss",k.max = 55)+
  geom_vline(xintercept = 8, linetype = 2)
# looking at the elbow we could asses that the correct number of centriods could be 9
# from this plot the correct number of centroids looks 9
train <- cluster_data[sample(nrow(cluster_data),1000),]
# for computational issuer the best number of centroids is calculated with a subsample of the data
fviz_nbclust(train, kmeans, method = "wss")
# lookink at the elbow it looks like 2,5,8 k are ok
geom_vline(xintercept = 3, linetype = 2)

km_out <- kmeans(cluster_data, 8, nstart = 10, iter.max = 200)
km_out$cluster
hist(km_out$cluster)
km_out$centers
km_out$totss
km_out$betweenss
km_out$withinss


cluster <- fviz_cluster(km_out, data = cluster_data,
                        palette = c("lightblue","red","green","indianred","cyan4","aquamarine","bisque","cadetblue"), 
                        geom = "point",
                        shape = 16,
                        point.size = .1,
                        ellipse.type = "norm", 
                        ggtheme = theme_ipsum(),
                        main = "8-means algorithm",
                        stand = F
                        
)

cluster
# we cannot observe a clear separation
# ggarrange(cluster1, cluster2,
#          font.label = list(size = 11, face = "plain"),
#          nrow = 1, ncol =2)

plot(cluster)

cluster_data2 <- Data %>%
  select(-City, -id,-X)

set.seed(123)
# scale data
cluster_data2 <- cluster_data2 %>%
  apply(2, function(x){
    return((x - min(x))/(max(x)-min(x)))
  }) %>% as_tibble() %>% sample_n(40000)
# compute PCA and extract individual coordinates
res.km <- kmeans(scale(cluster_data), centers=8, nstart = 10, iter.max = 200)
res.pca <- prcomp(cluster_data,  scale = F, center = F) # principal components analysis
# Coordinates of individuals
ind.coord <- as.data.frame(get_pca_ind(res.pca)$coord)
# Add clusters obtained using the K-means algorithm
ind.coord$cluster <- factor(res.km$cluster)
# Add Species groups from the original data sett
ind.coord$Churn <- as.factor(Data$Churn)
# Data inspection
head(ind.coord)

# Percentage of variance explained by dimensions
eigenvalue <- round(get_eigenvalue(res.pca), 1)
variance.percent <- eigenvalue$variance.percent
head(eigenvalue)

Data <- Data %>% mutate_if(is.logical, as.numeric)
str(Data)
Data <- Data[,-5] # Omitt City

## Self Organizing Map
cluster_data <- scale(cluster_data)
set.seed(123)
g <- somgrid(xdim = 5, ydim = 5, topo = "rectangular" )
g <- somgrid(xdim = 5, ydim = 5, topo = "hexagonal" )

map = som(X = cluster_data, 
          grid=g, 
          rlen=250, 
          alpha = c(0.05, 0.01),
          radius = 1,
          mode="batch", 
          dist.fcts = "euclidean")


plot(map) 
plot(map, type = "codes") # identical, for each node(16 for this grid) 
plot(map, type = "changes") # see early stopping
plot(map, type = "dist.neighbours")
plot(map, type = "quality")

par(mfrow=(c(1,2)))
plot(map, type = "mapping") # compare with the numerosity inside the single node/neuron
plot(map, type = "counts")
# see clusters on the map

## Supervised Self-Organizing Maps
# Data Split
set.seed(123)
ind <- sample(2, nrow(Data), replace = T, prob = c(0.7, 0.3))
train <- Data[ind == 1,]
test <- Data[ind == 2,]

# Normalization
train$Churn <- as.numeric(train$Churn)
trainX <- scale(train[,-3])
testX <-  scale(test[,-3],
                center = attr(trainX, "scaled:center"),
                scale = attr(trainX, "scaled:scale"))
trainY <- factor(train[,3])
Y <- factor(test[,3])
test[,3] <- 0
testXY <- list(independent = testX, dependent = test[,1])

# Classification & Prediction Model
set.seed(222)
map1 <- xyf(trainX,
            classvec2classmat(factor(trainY)),
            grid = somgrid(5, 5, "hexagonal"),
            rlen = 250) # extension of self-organising maps (SOMs) to multiple data layers
plot(map1)

# Prediction
pred <- predict(map1, newdata = testXY)
table(Predicted = pred$predictions[[2]], Actual = Y) # confusion matrix

# Cluster Boundaries
par(mfrow = c(1,2))
plot(map1, 
     type = 'codes',
     main = c("Codes X", "Codes Y"))
map1.hc <- cutree(hclust(dist(map1$codes[[2]])), 2)
add.cluster.boundaries(map1, map1.hc)
par(mfrow = c(1,1))



# Visualising cluster results
## use hierarchical clustering to cluster 
som_cluster <- cutree(hclust(dist(map$codes[[1]])), 2)
# plot these results:
pal <- c( "firebrick1","cyan4")
plot(map, type="mapping", main = "Clusters", pchs = NULL,
     shape = "straight") 
add.cluster.boundaries(map, som_cluster, col = "red")

plot(map, type="codes", bgcol = pal[som_cluster], main = "Clusters")
plot(map, type="mapping", bgcol = pal[som_cluster], main = "Clusters")
add.cluster.boundaries(map, som_cluster, col = "black") # black is more clear

# Factor analysis could be an alternative to PCA
# remind that you have to deal with non- numeric features

## 5. Consider as connected, customers who visited the same museum at the same time more than 
# twice. Draw a customers' network and compute measure of centralities for each node.

library(tidyverse)
library(vctrs) # for vec_c

setwd("C:/Users/Davide Zulato/Desktop/Esame Economics for Data Science")
in13 <- read_csv("in13.csv")

network_data = in13 %>% # it will be a grouped_df [372,884 x 5]
  group_by(museo,datai,orai) %>% # grouped tbl 
  summarize(n = n(),id = CodCliente)%>% # n() gives the current group size
  filter(n >= 2) # filter for group size >=2

# in13$mus_data <- paste(in13$museo,in13$orai,in13$datai,sep="")
# museum_data <- in13 %>% select(CodCliente,mus_data) # if i wanted to use two columns ang
# one variable for grouping

i = 1
links = matrix(nrow=1,ncol=2) # initialize the matrix of the links
while(i < nrow(network_data)){ # for every row (i.e.for every visit)
  
  print(paste(round((i/372884)*100),"%")) # display progression
  
  d = network_data$n[i]
  c = d + i - 1 # ultimo 
  
  links = rbind(links, t(combn(network_data$id[i:c],m = 2))) # will be from-to (but undirected)
  i = i+d
}

links <- data.frame(from = links[,1], to = links[,2])
links <- links[-1,]  # eventually remove the first row (NA) 
# write.csv(links, "links.csv")
# links <- read.csv("links.csv")


links <- links %>%
  group_by(from, to) %>% # grouped tbl 
  summarise(n = n()) %>%  # group size
  filter(n >= 2) # more than twice

links <- as.data.frame(links); str(links) # 82789 obs. of  3 variables
nodes <- unique(c(links$from, links$to)) # clients, they will be the vertices of the network
summary(links) # n max = 31
head(links) # the loops will be removed

library(igraph)
net <- graph_from_data_frame(d = links, vertices = nodes, directed = F) # using graph_from_data_frame
net # observe the relations: Set of related pairs of units
E(net)       # The edges of the "net" object
V(net)       # The vertices of the "net" object
write_graph(net, "net.net", format="pajek")
# from the pajek format i can import the network in pajek
## simplify the network
# links <- read.csv("links.csv")[-1,]
deg <- degree(net, mode="all") # look at the distribution of the degree
degree_id <- as.data.frame(as.table(deg)) # i want to create a dataset with centrality measures
colnames(degree_id) <- c("id","Degree")
write.csv(degree_id,"degree_id.csv")
hist(deg, breaks=1:vcount(net)-1, main="Histogram of node degree",xlim=c(0,32),col="steelblue4")
hist(deg, breaks=1:vcount(net)-1, main="Histogram of node degree",xlim=c(0,40),border="steelblue4",col="white")
## Network and node descriptives
## Density
# The proportion of present edges from all possible edges in the network.
edge_density(net, loops=F)

## Transitivity
# global - ratio of triangles (direction disregarded) to connected triples.
# local - ratio of triangles to connected triples each vertex is part of.
transitivity(net, type="global")  # net is treated as an undirected network

transitivity(as.undirected(net, mode="collapse")) # same as above

transitivity(net, type="local")

## Diameter
# A network diameter is the longest geodesic distance (length of the shortest path between two nodes) in the network. In igraph, diameter() returns the distance, while get_diameter() returns the nodes along the first found path of that distance.
# Note that edge weights are used by default, unless set to NA.
diameter(net, directed=F, weights=NA)
## [1] 46
diameter(net, directed=F)

## Degree distribution
deg.dist <- degree_distribution(net, cumulative=T, mode="all")
# deg.dist <- degree_distribution(net, cumulative=F, mode="all")
plot( x=0:max(deg), y=1-deg.dist, pch=19, cex=0.6, col="indianred", 
      
      xlab="Degree", ylab="Cumulative Frequency",main = "Cumulative Degree distribution")

## Degree (number of ties)
degree(net, mode="in")
centr_degree(net, mode="in", normalized=T)
# Closeness (centrality based on distance to others in the graph)
# Inverse of the node's average geodesic distance to others in the network.
closeness(net, mode="all", weights=NA) 
centr_clo(net, mode="all", normalized=T) 
# Eigenvector (centrality proportional to the sum of connection centralities)
# Values of the first eigenvector of the graph matrix.
eigen_centrality(net, directed=F, weights=NA)
centr_eigen(net, directed=F, normalized=T) 
# Betweenness (centrality based on a broker position connecting others)
# number of geodesics that pass through the node or the edge.

betweenness <- betweenness(net, directed=F, weights=NA)
mean(betweenness)

edge_betweenness(net, directed=F, weights=NA)

centr_betw(net, directed=F, normalized=T)

# simplify network
net <- igraph::simplify(net, remove.multiple = F, remove.loops = T) # not removing multiple edges, removing loop edges
# loops must be removed because the customer bought more tikets (e.g. Venaria Reale)
l <- layout_with_fr(net)
l2 <- layout.kamada.kawai(net)
deg <- degree(net2, mode="all")
plot(density(deg))
plot(net, edge.arrow.mode=0, layout=l, vertex.label = NA,
     curved = T)
plot(net, edge.arrow.mode=0, vertex.size=deg/30, layout=l, vertex.label = NA,
     curved = T)
plot(net, edge.arrow.mode=0, vertex.size=deg/30, layout=l2, vertex.label = NA,
     curved = T)
plot(net, vertex.size = sqrt(degree(net)), vertex.label = NA, edge.width = 4*E(net2)$weight/max(E(net2)$weight))

write_graph(net, "net.net", format="pajek")
title(main = "Graph from simplified network", sub = "vertex.size=deg/30")
legend("right",paste("vertex.size=",expression(deg/30)),lty=0,)
# with the pajek file i am able to move the net and looking for a better rapresentation

centr_eigen(net, directed=T, normalized=T) 
# Hub Score
hs <- hub_score(net, weights=NA)$vector # how much info goes trough or out from a network
# Authority Score
as <- authority_score(net, weights=NA)$vector # arrow out means influence (autority, e.g twitter many following vs many followers)
# plotting both
par(mfrow=c(1,2))
plot(net, vertex.size=hs*50, main="Hubs")
plot(net, vertex.size=as*30, main="Authorities")
# Mean distance
mean_distance(net, directed=F)
# Mean distance, it is not directed but let's try
mean_distance(net, directed=T)
distances(net) # with edge weights, looking for isolated component (0 already there, in far away)
distances(net, weights=NA) # ignore weights

### reduced net for a nice visualization
links_redux <- links[which(links$n>=21),]
nodes_redux <- unique(c(links_redux$from, links_redux$to))
# For example client 93720 has n=31, i remove loops with igraph::simplify
net_redux <- graph_from_data_frame(d = links_redux, vertices = nodes_redux, directed = F)
# simplify network
net_redux <- igraph::simplify(net_redux, remove.multiple = F, remove.loops = T) # not removing multiple edges, removing loop edges
# loops must be removed because the customer bought more tikets (e.g. Venaria Reale)
deg_redux <- degree(net_redux, mode="all") # number of connections

# choosing the best layout
layouts <- grep("^layout_", ls("package:igraph"), value=TRUE)[-1]
layouts <- layouts[!grepl("bipartite|merge|norm|sugiyama|tree", layouts)]

for (layout in layouts) {
  
  print(layout)
  
  l <- do.call(layout, list(net_redux)) 
  
  plot(net_redux, edge.arrow.mode=0, layout=l, main=layout) }

plot(net_redux, edge.arrow.mode=0,vertex.size=deg_redux*15, layout=layout_with_kk,curved = T) 
plot(net_redux, edge.arrow.mode=0,vertex.size=deg_redux*15, layout=layout_randomly,curved = T)
title(main = "Graph of simple reduced network", sub = "vertex.size=deg*15")
# NOdes and net measures

centr_degree(net_redux, mode="in", normalized=T) 

closeness(net_redux, mode="all", weights=NA) 

centr_clo(net_redux, mode="all", normalized=T) 

eigen_centrality(net_redux, directed=T, weights=NA)

centr_eigen(net_redux, directed=F, normalized=T) 

betweenness(net_redux, directed=F, weights=NA) # how many times a nod is in the path of the connection

edge_betweenness(net, directed=F, weights=NA)

centr_betw(net_redux, directed=F, normalized=T)

hs <- hub_score(net_redux, weights=NA)$vector # how much info goes trough or out from a network

as <- authority_score(net_redux, weights=NA)$vector # arrow out means influence (autority, e.g twitter many following vs many followers)

# community
# High-betweenness edges are removed sequentially (recalculating at each step) 
# and the best partitioning of the network is selected.
ceb <- cluster_edge_betweenness(net_redux) 
dendPlot(ceb, mode="hclust")
plot(ceb, net_redux)
title(main="Cluster of reduced Net",sub="Loops included")

## ISSUE 6
# Is there a causal impact of gender on the probability of churning? identify a suitable model 
# to create a counterfactual group with observational data

library(readr)
library(dplyr)
library(ggplot2)

# I will use propensity score matching to create counterfactual group to assess 
# potential churning
# The statistical quantity of interest is the causal effect of the treatment (gender)
# on probability of churning

setwd("C:/Users/Davide Zulato/Desktop/Esame Economics for Data Science")
Data = read_csv('Processed_data.csv')
head(Data)
Data <- Data %>% mutate_if(is.logical, as.numeric)
str(Data)

Data$Churn <- as.numeric(Data$Churn)
Data$Gender <- as.numeric(Data$Gender) # 1 if male, 0 if Female

Data <- Data[,-1]

# packages MatchIt and optmatch
# install.packages("optmatch")
# install.packages("MatchIt")
library("MatchIt")


# churner is the dummy variable for churning
# "Male" can be considered as the treatment variable, "churner" is the outcome, 
# and the others are pre-treatment covariates.

## is the probability of churning of the treated higher?
# is it a randomized experiment? May yes
# because subject doesn't self- selected, gender obviously pre-exist

# The regression should include relevalt variables
fit0 <- lm(Churn ~ Gender+ Age +Price+Degree, data = Data)
# fit0 <- lm(Churn ~ ., data = churn_data)
summary(fit0)
# Treatment "Male"
prova <- glm(Gender ~ Churn + Age +Price+Degree, data=Data) # try to leverage out response
summary(prova)

# treat is a dummy variable, dependent is a dummy varable (logit/probit but ols is interpretable)
# Gender has an impact on the outcome => matching in order to combine similar observation

# matching, there are various matching mechanism

# variable that doesn't change over time or thet does't change the outcome (check theory)
# nearest neighbor propensity score matching.
# 1:1 NN PS matching w/o replacement
m.out1 <- matchit(Gender ~ Churn + Age +Price+Degree+New_member, data=Data,
                  method = "nearest", distance = "glm")

summary(m.out1)
plot(m.out1, type = "jitter", interactive = FALSE) # distribution of the propensity score (similar treated and control)
plot(m.out1, type = "density", which.xs = c("Age", "Price","Churn"),
     subclass = 1)
library(cobalt)
# Love plot of chosen balance statistics
love.plot(m.out1, binary = "std")
# Density plot for continuous variables
# displays distributional balance for a single covariate, similar to plot.matchit(). 
# Its default is to display kernel density plots for continuous variables and bar 
# graphs for categorical variables. It can also display eCDF plots and histograms
bal.plot(m.out1, var.name = "Age", which = "both")
#Bar graph for categorical variables
bal.plot(m.out1, var.name = "Price", which = "both")
#Mirrored histogram
bal.plot(m.out1, var.name = "Age", which = "both",
         type = "histogram", mirror = TRUE)


# unmatched have propensity score close to zero
plot(m.out1, type = "qq", interactive = FALSE,
     which.xs = c("Age", "Price","Degree")) # if perfect mach each observation has a twin (all on diagonal)
plot(summary(m.out1)) # absolute standardized mean difference, if the control is well done it should be about zero
# a good match reduces the difference, check the common support # # very well, close to zero
plot(m.out1, type = "hist") # expected to be common support, another way to assess the goodness of matching
# is looking to the distribution

## Estimating the Treatment Effect

m.data1 <- match.data(m.out1)
m.data1$Gender<-as.factor(m.data1$Gender)

library(viridis)
m.data1 %>%
  ggplot( aes(x=Gender, y=Churn, fill=Gender)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size=0.4, alpha=0.9)  +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Treated vs. Matched controls") +
  xlab("")


Data %>%
  ggplot( aes(x=Gender, y=Churn, fill=Gender)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size=0.4, alpha=0.9)  +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Treated vs. all controls") 

## Different matching
# full matching, which matches every treated unit to at least one control and every control 
# to at least one treated unit. We'll also try a 
# different link (probit) for the propensity score model.
# m.out2 <- matchit(Gender ~ Churn + Age +Price, data=Data,
#                  method = "full", distance = "glm", link = "probit")

# plot(summary(m.out2)) # the matched have a ASMD close to zero (cool)
# plot(m.out2, type = "hist")

library("lmtest") #coeftest
library("sandwich") #vcovCL


m.data1 <- match.data(m.out1) # not the original data
fit1 <- lm(Churn ~ Gender+ Age +Price+Degree, data = m.data1, weights = weights)

coeftest(fit1, vcov. = vcovCL, cluster = ~subclass)
#  performing z and (quasi-)t Wald tests of estimated coefficients
# vcov is the specification for the covariance matrix

summary(fit1)
#no covariates, effect is higher, if i run the same model only with treated (comparing the means basically)+
# see the effect without covariates
fit2 <- lm(Churn ~ Gender, data = m.data1, weights = weights)
summary(fit2)


m.data1$Gender<-as.factor(m.data1$Gender)

# DIFFERENCE-IN-MEANS
# The means below indicate that we have attained a high degree of balance on 
# the five covariates included in the model.


# ESTIMATING TREATMENT EFFECTS
# Estimating the treatment effect on matched sample, t.test
with(m.data1, t.test(Churn ~ Gender)) # reject null hypothesis
# gender has effect 

# see the effect
m.data1$Gender <- as.factor(m.data1$Gender)
m.data1$Churn <- as.factor(m.data1$Churn)
logit_Male_match <- m.data1 %>%
  glm(formula = Churn ~ Gender, family = "binomial")
library(jtools)
library(hrbrthemes)
jtools::summ(logit_Male_match, digits = 5, exp = T)
effect_plot(logit_Male_match, pred = Gender, plot.points = TRUE,
            jitter = c(0.1, 0.05), point.alpha = 0.1, colors = "cyan4") +
  ylab("Pr(Churner = 0)")+
  xlab("Exposure=Male")

## ISSUE 7. Which models could you use to predict churners? Run at least three prediction models and 
# show the ROC curves for them. Compute the predicted probability on the test-set and show 
# its distribution. 

# check if useless variables are removed
# some models works with NAs other may be affected (https://www.tmwr.org/)

rm(list=ls())
# Libraries for this Task
library(ggplot2)
library(caret)
library(rpart)
library(ROCR)
library(randomForest)
library(gbm)
library(foreach)

# Dataset 
setwd("C:/Users/Davide Zulato/Desktop/Esame Economics for Data Science")
getwd()

## split
# set.seed(123)
# require(rsample) # there are a lot of methods for random splitting, i'll use initial_split
# split <- initial_split(data = churn_data, prop = 0.7, strata = Churn)
# train <- training(split)
# test  <- testing(split)
# check if the split is good for the target variable
# prop.table(table(train$Churn))
# prop.table(table(test$Churn)) # the distribution is kept between training and test
#you can also check for distribution
# ks.test(train$Price, test$Price) # we are looking for high p-values in order to reject H0


# churn_data = read_csv('churn_data.csv')
Data <- read_csv('Processed_data.csv') # data with also Degree, freq_visit, total_price, Age
library(dplyr)
Data <- Data %>% mutate_if(is.logical, as.numeric)
str(Data)
Data$Churn <- as.factor(Data$Churn) # I am doing classification
# Dataset set-up
# Check for NA's
test_df_na <- apply(Data, 2, function(x) any(is.na(x)))
print(paste("# columns that contains NAs --> ",
            length(test_df_na[test_df_na==TRUE]),sep = ""))

## Feature engineering
# in the previous ISSUES i created the 
# Degree variable in ISSUE 5
# Total_price created in ISSUE1
# churn_data$ <- ifelse(==) Dummy variables

# Target distribution
table(Data$Churn)/dim(Data)[1]
prop.table(table(Data$Churn))

# Create training and test data set (as you wish)
set.seed(123)
perc_train_set <- 0.7
index <- createDataPartition(Data$Churn,
                             p=perc_train_set,
                             list = FALSE)
train <- Data[index,] # 
test <- Data[-index,] #

# Check that the target distribution is kept over the train set
table(train$Churn)/dim(train)[1] # about 30% of churners (retain=0)
#easy and not perfect task to check whether in the creation we are doing right.
#you can also check for distribution
ks.test(train$Price, test$Price) # we are looking for high p-values in order to not reject H0
# not Rejected
# Models
# Let's construct some model built on decision trees
# Out of the box the methods can handle both categorical and numeric variables
# without the need to properly normalize them

# Decide which features we do need and define a formula
columns <- colnames(Data)
target <- "Churn"
features <- columns[!columns %in% c(target)]

print(paste(paste(target," ~"), paste(features, collapse = " + ")))
formula <- as.formula(paste(paste(target," ~"), 
                            paste(features, collapse = " + ")))

# CART Modeling via rpart
# method=	"class" for a classification tree 
train_CART <- train %>% select(-City)
formula_CART <- as.formula(paste(paste(target," ~"), 
                                 paste(features[-4], collapse = " + ")))
cart_model <- rpart(formula_CART,
                    method="class", data=train_CART,
                    parms = list(prior = c(.78,.22), 
                                 split = "information"))
# Let's predict new data
# using the functon (method) predict
prediction_cart <- predict(cart_model,test,type = "class")

# Summarize the result with Confusion Matrix
confusionMatrix(prediction_cart,test$Churn)
#confusionMatrix(prediction_cart,as.factor(test$default.payment.next.month))

# Summarize the results with AUROC
prediction_prob_cart <- predict(cart_model, test, type = 'prob')[,2]
roc_cart <- ROCR::prediction(predictions = prediction_prob_cart, 
                             labels = test$Churn)
perf.roc_cart <- performance(roc_cart, measure = "tpr", x.measure = "fpr")
perf.auc_cart <- performance(roc_cart, measure = "auc")
ROC_df_cart <- data.frame(unlist(perf.roc_cart@x.values),
                          unlist(perf.roc_cart@y.values))
colnames(ROC_df_cart) <- c("fpr","tpr")

print(paste("AUC (CART) -->",format((perf.auc_cart@y.values)[[1]]*100,digits = 4),"%"))

# ROC plot
xline <- seq(0,1,0.02)
yline <- seq(0,1,0.02)
xyline <- data.frame(xline,yline)

ggplot() + 
  geom_line(data=ROC_df_cart, aes(x=fpr, y=tpr)) +
  geom_line(data=xyline, aes(x=xline, y=yline), color='black',linetype = "dashed") +
  xlab("FPR") + ylab("TPR") +
  ggtitle("ROC")


#let's check prediction with distribution

plot <- data.frame(prediction_prob_cart, test$Churn)


gg3 <- ggplot(data=plot, aes(x=prediction_prob_cart, 
                             group=test$Churn,
                             fill=test.Churn)) +
  geom_density(adjust=1.5, alpha=.4)+
  ggtitle("Predicted Probability CART")



# Ensemble Models: (Bagging and Boosting)
# Random Forest (Bagging)
rf_model <- randomForest(formula,
                         data = train,
                         do.trace = TRUE,
                         sampsize=c(1000),
                         mtry = 5,
                         ntree = 500,
                         importance = TRUE
)

prediction_rf <- predict(rf_model,test,type = "class")

confusionMatrix(prediction_rf,test$Churn)

prediction_prob_rf <- predict(rf_model, test, type = 'prob')[,2]
roc_rf <- ROCR::prediction(predictions = prediction_prob_rf, 
                           labels = test$Churn)
perf.roc_rf <- performance(roc_rf, measure = "tpr", x.measure = "fpr")
perf.auc_rf <- performance(roc_rf, measure = "auc")
ROC_df_rf <- data.frame(unlist(perf.roc_rf@x.values),
                        unlist(perf.roc_rf@y.values))
colnames(ROC_df_rf) <- c("fpr","tpr")

print(paste("AUC (RandomForest) -->",format((perf.auc_rf@y.values)[[1]]*100,digits = 4),
            "%"))


plot <- data.frame(prediction_prob_rf, test$Churn)
gg1 <- ggplot(data=plot, aes(x=prediction_prob_rf, group=test$Churn, fill=test$Churn)) +
  geom_density(adjust=1.5, alpha=.4)+
  ggtitle("Predicted Probability RF")


ggplot() + 
  geom_line(data=ROC_df_cart, aes(x=fpr, y=tpr, color="CART")) +
  geom_line(data=ROC_df_rf, aes(x=fpr, y=tpr, color="RF")) +
  geom_line(data=xyline, aes(x=xline, y=yline), color='black',linetype = "dashed") +
  xlab("FPR") + ylab("TPR") +
  scale_colour_manual("Models",
                      values=c("CART"="blue","RF"="red")) +
  ggtitle("ROC")

# AdaBoost (gbm) (Boosting)
ntree_ada = 100
formula_ADA <- formula_CART
train_ADA <- train_CART
train_ADA$New_member <- as.numeric(train_ADA$New_member)

ada_model <- gbm(formula_ADA, 
                 data = train_ADA, 
                 distribution = "adaboost", 
                 n.trees = 500,
                 interaction.depth = 10,
                 bag.fraction = 0.5,
                 train.fraction = 0.8,
                 verbose = TRUE,
                 n.cores = 4
)




prediction_ada <- predict(ada_model, test, n.trees = ntree_ada, type = 'response')
roc_ada <- ROCR::prediction(predictions = prediction_ada, 
                            labels = test$Churn)
perf.roc_ada <- performance(roc_ada, measure = "tpr", x.measure = "fpr")
perf.auc_ada <- performance(roc_ada, measure = "auc")
ROC_df_ada <- data.frame(unlist(perf.roc_ada@x.values),
                         unlist(perf.roc_ada@y.values))
colnames(ROC_df_ada) <- c("fpr","tpr")

print(paste("AUC (AdaBoost) -->",format((perf.auc_ada@y.values)[[1]]*100,digits = 4),"%"))

ggplot() + 
  geom_line(data=ROC_df_cart, aes(x=fpr, y=tpr, color="CART")) +
  geom_line(data=ROC_df_rf, aes(x=fpr, y=tpr, color="RF")) +
  geom_line(data=ROC_df_ada, aes(x=fpr, y=tpr, color="AdaBoost")) +
  geom_line(data=xyline, aes(x=xline, y=yline), color='black',linetype = "dashed") +
  xlab("FPR") + ylab("TPR") +
  scale_colour_manual("Models",
                      values=c("CART"="blue","RF"="red","AdaBoost"="green")) +
  ggtitle("ROC")


plot <- data.frame(prediction_ada, test$Churn)
gg2 <- ggplot(data=plot, aes(x=prediction_ada, group=test$Churn, fill=test$Churn)) +
  geom_density(adjust=1.5, alpha=.4)+
  ggtitle("Predicted Probability Adaboost")


# Model Validation adn Optimization for Random Forest
# library(foreach)
# number of cores for parallel caret training
# doParallel::registerDoParallel(cores = 4)
# levels(Data$Churn)[1] <- "Churn"
# levels(Data$Churn)[2] <- "Retain"

# Cross Validation For tuning RF hiperparameters
# model_ctrl <- trainControl(method = "repeatedcv",
#                            number = 5,
#                           classProbs = TRUE,
#                           summaryFunction = twoClassSummary,
#                           verboseIter = TRUE)

# rf_model_grid <- expand.grid(mtry = c(2,3,4,5,6))

# rf_model <- train(formula,
#                  data = Data,
#                  do.trace = TRUE,
#                  method="rf",
#                  sampsize=c(1000),
#                  ntree = 100,
#                  trControl = model_ctrl,
#                  tuneGrid = rf_model_grid,
#                  metric = "ROC",
#                  allowParallel=TRUE)

# Tuning The RF parameters
#prediction_prob_rf <- predict(rf_model, test, type = 'prob')[,2]
#roc_rf <- ROCR::prediction(predictions = prediction_prob_rf, 
#                           labels = test$Churn)
# perf.roc_rf <- performance(roc_rf, measure = "tpr", x.measure = "fpr")
# perf.auc_rf <- performance(roc_rf, measure = "auc")
# ROC_df_rf <- data.frame(unlist(perf.roc_rf@x.values),
#                        unlist(perf.roc_rf@y.values))
# colnames(ROC_df_rf) <- c("fpr","tpr")

# print(paste("AUC (RandomForest) -->",format((perf.auc_rf@y.values)[[1]]*100,digits = 4),
#            "%"))

# ggplot() + 
#  geom_line(data=ROC_df_cart, aes(x=fpr, y=tpr, color="CART")) +
#  geom_line(data=ROC_df_rf, aes(x=fpr, y=tpr, color="RF")) +
#  geom_line(data=ROC_df_ada, aes(x=fpr, y=tpr, color="AdaBoost")) +
#  geom_line(data=xyline, aes(x=xline, y=yline), color='black',linetype = "dashed") +
#  xlab("FPR") + ylab("TPR") +
#  scale_colour_manual("Models",
#                      values=c("CART"="blue","RF"="red","AdaBoost"="green")) +
#  ggtitle("ROC")

# Predicted Probabilities for the models
ggarrange(gg1,gg2,gg3)

###GAIN PLOT
gain <- performance(roc_rf, "tpr", "rpp")
plot(gain, main = "Gain Chart")

## ISSUE 8
# Consider a marketing campaign addressing directly single customers. We know that each 
# contact costs 2 euro. We can also compute the consumer value for each single customer. We 
# can reasonably assume that a churner, contacted for the campaign has a probability of 10% 
# of not churning. Non-churners contacted are simply a cost of 2 euro. With this additional
# information, generate a profit curve of each prediction model. Discuss your results.

#Assume a cost of alpha for false positive and a cost of beta for false
#negative.  let's say 0 for benefit.
# remember target distribution     0.3061268 and 1 0.6938732 = is churning, 1 is not churning
# benefit is = we just have to look at the cost associated of being wrong
# false positive/positive* alpha  and false neg/negative* beta.
#unbalance population
#   rate real positiv( ) + rate real negative ()
# 0.9(tpr*b-fpr*alpha ) + 0.1(tnr*b-fnr*beta)
# 0.9(-fpr*alpha ) + 0.1(-fnr*beta)  since benefit is zero


# 0.9* (false positive/positive* alpha) + 0.1* (false neg/negative* beta)
### 0.9* (1-TPR)*alpha + 0.1*(1-TNR)*beta
#### alpha/beta
###assumption  Profit. if I act on a positive I get Gamma (already cost take into account). if I act on negative I get -cost
###gamma could also be person specific

testnew<-test
# 0.2 is the contribution for every visit
gamma = 0.9 * 0 + .1*(testnew$Price + 0.2*testnew$Visit_frequency - 2) # gamma <- benefit of getting the right person 
cost<-2 # the cost of making a mistake is 2 euros (contact a churner without retention)

testnew$Churn<-as.numeric(as.character(testnew$Churn))
testnew$profit<-testnew$Churn*gamma - (1-testnew$Churn)*cost
testnew$rf_prediction<-unlist(roc_rf@predictions)
testnew$ada_prediction<-unlist(roc_ada@predictions)
testnew$cart_prediction<-unlist(roc_cart@predictions)

library('dplyr')
testnew<- testnew %>% mutate(rf_rank = rank(desc(rf_prediction),ties.method = "first"))
testnew<- testnew %>% mutate(ada_rank = rank(desc(ada_prediction), ties.method = "first"))
testnew<- testnew %>% mutate(cart_rank = rank(desc(cart_prediction),ties.method = "first"))


#cumuprofit

testnew <- transform(testnew[order(testnew$cart_rank, decreasing = F), ], cumsum_cart = ave(profit, FUN=cumsum))
testnew <- transform(testnew[order(testnew$rf_rank, decreasing = F), ], cumsum_rf = ave(profit, FUN=cumsum))
testnew <- transform(testnew[order(testnew$ada_rank, decreasing = F), ], cumsum_ada = ave(profit, FUN=cumsum))

ggplot() + 
  geom_line(data=testnew, aes(x=cart_rank, y=cumsum_cart, color="CART"))+
  geom_line(data=testnew, aes(x=rf_rank, y=cumsum_rf, color="RF")) +
  geom_line(data=testnew, aes(x=ada_rank, y=cumsum_ada, color="AdaBoost")) +
  xlab("instances") + ylab("profit") +
  scale_colour_manual("Models",
                      values=c("CART"="blue","RF"="red","AdaBoost"="green")) +
  ggtitle("Cumulative profit")


