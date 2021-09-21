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
