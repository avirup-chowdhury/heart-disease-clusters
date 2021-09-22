# Initial loading in ----
# Load in library
library(tidyverse)

# Load in files
names_vector <- c("age", "sex", "chest_pain_type", "resting_bp", "cholesterol",
                  "fasting_glucose", "resting_ecg", "max_hr_achieved",
                  "exercise_induced_angina", "st_depression_exercise",
                  "exercise_st_slope", "n_vessels_fluoroscopy", 
                  "wall_defect_type", "diagnosis")

cleveland <- read_csv("D:/Heart disease patient data/processed.cleveland.data",
                      col_names = names_vector, na = "?") 
switzerland <- read_csv("D:/Heart disease patient data/processed.switzerland.data",
                        col_names = names_vector, na = "?")

hungary <- read_csv("D:/Heart disease patient data/processed.hungarian.data",
                    col_names = names_vector, na = "?")

longbeach <- read_csv("D:/Heart disease patient data/processed.va.data",
                      col_names = names_vector, na = "?")

patients <- full_join(cleveland, switzerland) %>%
  full_join(., hungary) %>%
  full_join(., longbeach)

rm(cleveland, hungary, longbeach, switzerland, names_vector)

# Data checking----
glimpse(patients)
summary(patients)

cropped <- patients %>%
  # Drop columns with lots of NAs/ nominal data types (w/ multiple categories)
  select(!c(n_vessels_fluoroscopy, exercise_st_slope, wall_defect_type,
            diagnosis)) %>%
  drop_na()

summary(cropped)

scaled <- scale(cropped) # To normalise data
summary(scaled)

rm(patients, cropped)

# PCA ----
pca_model <- prcomp(scaled, scale = FALSE)

# Summarise variance per PC and plot
components <- tibble(PC = c(1:10), 
                     var = pca_model$sdev^2/sum(pca_model$sdev^2))

ggplot(components, aes(x = PC, y = var)) + geom_point() + geom_line()

biplot(pca_model)

rm(components)

# k-means ----
sumsq <- data.frame(matrix(data = NA_integer_, nrow = 15, ncol = 2))

# Identify best number of clusters
set.seed(42)

for (i in 1:15) {
  output <- kmeans(scaled, centers = i, nstart = 20)
  sumsq[i, ] <- c(i, output$tot.withinss)
}

ggplot(sumsq, aes(x = X1, y = X2)) + 
  geom_line() + geom_point() +
  labs(title = "Scree plot (k-means)") +
  xlab("Number of clusters") +
  ylab("Total within-cluster sum of squares")

# Build model with 5 clusters
set.seed(42)
km_model <- kmeans(scaled, centers = 5, nstart = 20)

clustered <- bind_cols(scaled, km_cluster = km_model$cluster)

ggplot(clustered, aes(x = age, y = cholesterol, col = as.factor(km_cluster))) + 
  geom_point()

rm(sumsq, i, output)

# Hierarchical clustering ----
hier_model <- hclust(dist(scaled[-638, ]), method = "complete")
# 638 is in their own cluster, hence omitted for now

plot(hier_model) 

# Get cluster assignments based on number of selected clusters
hcm_cut <- cutree(hier_model, k = 5)

clustered <- bind_cols(clustered, 
                       
                       # Fill patient 638 with result "NA"
                       hm_cluster = c(hcm_cut[1:637], 
                                      NA, 
                                      hcm_cut[638:length(hcm_cut)]))

# Comparing methods ----
table(clustered$km_cluster, clustered$hm_cluster)
