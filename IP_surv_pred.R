# Package Installation
list.of.packages <- c("caret","tidyverse", "yardstick","plotly", "forcats",
                      "kernlab","e1071", "mlbench", "mice", "ggplot2", "GGally") 

new.package <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.package)){
  install.packages(new.package)}

#Load library
invisible(lapply(list.of.packages, library, character.only = TRUE))

#Loading Dataset
ip_data <- read.csv("C:/Users/merry/Downloads/archive (3)/dataset.csv")

#View the dataset
dim(ip_data)   
"This dataset contains 91713 rows and 85 columns"

glimpse(ip_data)
head(ip_data)

#Count Unique values
apply(ip_data, 2, function(x) length(unique(x)))
"Seems there are no dublicate records"

#Data Transformation
"converting Column with character datatype to numeric"

#Ethnicity
ip_data$ethnicity <- ifelse(ip_data$ethnicity == "Caucasian", 1, 
                           ifelse(ip_data$ethnicity == "Hispanic", 2,
                           ifelse(ip_data$ethnicity == "African American", 3,
                                  ifelse(ip_data$ethnicity == "Asian", 4,
                                         ifelse(ip_data$ethnicity == "Native American", 5,
                                                ifelse(ip_data$ethnicity == "Other/Unknown", 6, NA
                                                       ))))))
table(ip_data$ethnicity)

#Gender
ip_data$gender <- ifelse(ip_data$gender == "M", 1, 
                            ifelse(ip_data$gender == "F", 0, NA))
                                   
#icu_admit_score
ip_data$icu_admit_source <- ifelse(ip_data$icu_admit_source == "Accident & Emergency", 1, 
                            ifelse(ip_data$icu_admit_source == "Floor", 2,
                            ifelse(ip_data$icu_admit_source == "Operating Room / Recovery", 3,
                            ifelse(ip_data$icu_admit_source == "Other Hospital", 4,
                            ifelse(ip_data$icu_admit_source == "Other ICU", 5 , NA
                                                        )))))

#icu_stay_type
ip_data$icu_stay_type <- ifelse(ip_data$icu_stay_type == "admit", 1, 
                                   ifelse(ip_data$icu_stay_type == "readmit", 2,
                                          ifelse(ip_data$icu_stay_type == "transfer", 3,NA)))

#icu_type
ip_data$icu_type <- ifelse(ip_data$icu_type == "Cardiac ICU", 1, 
                    ifelse(ip_data$icu_type == "CCU-CTICU", 2,
                    ifelse(ip_data$icu_type == "CSICU", 3,
                    ifelse(ip_data$icu_type == "CTICU", 4,
                    ifelse(ip_data$icu_type == "Med-Surg ICU", 5,
                    ifelse(ip_data$icu_type == "MICU", 6,             
                    ifelse(ip_data$icu_type == "Neuro ICU", 7,
                    ifelse(ip_data$icu_type == "SICU", 8, NA ))))))))

#apache_3j_bodyayatem
ip_data$apache_3j_bodysystem <- ifelse(ip_data$apache_3j_bodysystem == "Cardiovascular", 1, 
                    ifelse(ip_data$apache_3j_bodysystem == "Gastrointestinal", 2,
                    ifelse(ip_data$apache_3j_bodysystem == "Genitourinary", 3,
                    ifelse(ip_data$apache_3j_bodysystem == "Gynecological", 4,
                    ifelse(ip_data$apache_3j_bodysystem == "Hematological", 5,
                    ifelse(ip_data$apache_3j_bodysystem == "Metabolic", 6,             
                    ifelse(ip_data$apache_3j_bodysystem == "Musculoskeletal/Skin", 7,
                    ifelse(ip_data$apache_3j_bodysystem == "Neurological", 8,
                    ifelse(ip_data$apache_3j_bodysystem == "Respiratory", 9,
                    ifelse(ip_data$apache_3j_bodysystem == "Sepsis", 10,
                    ifelse(ip_data$apache_3j_bodysystem == "Trauma", 11, NA
                           )))))))))))

#ip_data$apache_3j_bodysystem

"Undefined diagnosis have 2 spellings, both will be considered one"

ip_data$apache_2_bodysystem <- ifelse(ip_data$apache_2_bodysystem == "Cardiovascular", 1, 
                                ifelse(ip_data$apache_2_bodysystem == "Gastrointestinal", 2,
                                ifelse(ip_data$apache_2_bodysystem == "Haematologic", 3,
                                ifelse(ip_data$apache_2_bodysystem == "Metabolic", 4,
                                ifelse(ip_data$apache_2_bodysystem == "Neurologic", 5,
                                ifelse(ip_data$apache_2_bodysystem == "Renal/Genitourinary", 6,             
                                ifelse(ip_data$apache_2_bodysystem == "Respiratory", 7,
                                ifelse(ip_data$apache_2_bodysystem == "Undefined diagnoses", 8,
                                ifelse(ip_data$apache_2_bodysystem == "Undefined Diagnoses", 8, NA
                                      )))))))))

"Relabeling the class-hospital_death with 0 as 'no_death' and 1 as 'death'"

ip_data$hospital_death <- factor(ip_data$hospital_death, 
                                 levels = c(1,0), 
                                 labels = c("death", "no_death"))

ip_data$hospital_death <- fct_relevel(ip_data$hospital_death, "death", "no_death")

"As column 84 -'X' does not have any value, it can be removed along with 'id' columns"
ip_data <- ip_data %>%
              select(-c(X,encounter_id, patient_id,hospital_id, icu_id))


#Explanatory Data Analysis (EDA)

#Missing Values
sum(is.na(ip_data))

#Class distribution
table(ip_data$hospital_death)

"death - 7915
 no_death - 83798"

#Hist for labeled class
ggplot(ip_data, aes(y = hospital_death)) + 
  geom_bar(aes(fill = hospital_death)) +
  xlab("Hospital Death") +
  ylab("Count") +
  theme(legend.position = "top")

"Class is unbalanced"

"Analysing the binary valued columns"
bin <- c("elective_surgery", "gender", "apache_post_operative", 
         "arf_apache","gcs_unable_apache", "intubated_apache",
         "ventilated_apache", "aids", "cirrhosis", "diabetes_mellitus",
         "hepatic_failure", "immunosuppression", "leukemia",
         "lymphoma", "solid_tumor_with_metastasis")

#For proportion of death and no_death of BINARY VARIABLES
prop = aggregate(x = ip_data[,bin],
                 by = list(ip_data$hospital_death),
                 FUN = sum)

prop[1, 2:16] <- prop[1, 2:16] / sum(ip_data$hospital_death == 'death')
prop[2, 2:16] <- prop[2, 2:16] / sum(ip_data$hospital_death == 'no_death')
prop













