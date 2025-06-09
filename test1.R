# my git hub

# testing


#test 2


---
  title: "National Point Prevalence Stats"
author: "Armstrong Obale"
output:word_document:
  toc: true
toc_depth: '3'
html_document:
  toc: true
toc_depth: 3
number_sections: true
fig_caption: true
df_print: paged
highlight: default
keep_md: false
---
  
  
  ## Objective
  
  1\) Prevalence – across hospitals, province ward type….

2\) Common antibiotics – across hospitals, province ward type, and hospital type….

3\) Indications for antibiotic use – across hospitals, province ward type…. 

4\) Microbiologic data associated.

## Abstract

```{r setup, include=FALSE}
library(knitr)
knitr::opts_chunk$set(echo=FALSE, message=FALSE, warning=FALSE)

#| echo: false
# Load packages

library(tidyverse)
library(gtsummary)
library(gt)
library(readxl)
library(ggwordcloud)
library(stringr)
library(officer)
library(flextable)

```


```{r}

#| echo: false
# Load data

data_no_labels <- read.csv("C:\\Users\\Vasana\\Desktop\\NPPS Coding\\NationalPointPrevale_DATA_2025-03-26_0354.csv")

data_labels <- read.csv("C:\\Users\\Vasana\\Desktop\\NPPS Coding\\NationalPointPrevale_DATA_LABELS_2025-03-26_0356.csv")

names(data_labels) <- names(data_no_labels) # Get labels and right variable names
```

### Prepare data

#### Function for removing duplicates in forms

```{r}

#| echo: false
remove_duplicate_suffix_columns <- function(data) {
  # Identify columns with the suffix "_1" or "__1"
  cols_with_suffix <- grep("(_1$|__1$)", colnames(data), value = TRUE)
  # Remove the suffixes "_1" and "__1" to find base column names
  base_cols <- sub("(_1$|__1$)", "", cols_with_suffix)
  # Iterate over base column names and check for duplicates
  for (base_col in base_cols) {
    # Check if both the base column and its "_1" or "__1" counterparts exist
    if (base_col %in% colnames(data)) {
      suffixes <- c("_1", "__1")
      for (suffix in suffixes) {
        col_with_suffix <- paste0(base_col, suffix)
        if (col_with_suffix %in% colnames(data)) {
          # Check if the values in both columns are identical, accounting for NA
          if (all(data[[base_col]] == data[[col_with_suffix]], na.rm = TRUE)) {
            # Remove the column with the suffix
            data[[col_with_suffix]] <- NULL
          }
        }
      }
    }
  }
  return(data)
}
```

```{r}

#| echo: false
# At this point we have to save medical_information_complete given that the wrangling below removes it
med_complete <- data_labels %>% select(study_id, medical_information_complete) %>% filter(medical_information_complete != "")

data_labels <- data_labels %>%
  select(-redcap_repeat_instance)

# Replace empty cells with NA
data_labels[data_labels == ""] <- NA

data_labels <- data_labels %>%
  group_by(study_id) %>%
  summarise(across(everything(), ~ {
    non_na_values <- na.omit(.x)
    if (length(non_na_values) == 0) {
      list(NA)  # Return a list containing NA for empty groups
    } else {
      # Assign the original variable name to the first value and subsequent names as variable_n
      list(c(setNames(non_na_values[], names(.x)), 
             setNames(non_na_values[-2], paste0(names(.x), "_", seq_along(non_na_values[-2])))))
    }
  }), .groups = "drop") %>%
  unnest_wider(everything(), names_sep = "_")

# Identify and remove duplicate columns
data_labels <- data_labels[, !duplicated(as.list(data_labels))]


# Remove "_1" from the ends of variable names, giving them their actual names
colnames(data_labels) <- ifelse(
  grepl("__1$", colnames(data_labels)),
  colnames(data_labels),
  sub("_1", "", names(data_labels))
)

# Finally, trim off all spaces before and after data entries
# Ensure column names are unique
names(data_labels) <- make.unique(names(data_labels))


data_labels <- remove_duplicate_suffix_columns(data_labels)


# Trim spaces in character and numeric columns
data_labels <- data_labels %>% 
  mutate(across(where(~ is.character(.) || is.numeric(.)), ~ str_trim(as.character(.))))

# Remove every column that starts with redcap_event_name
data_labels <- data_labels %>% select(-starts_with("redcap"))

# Now pack back medical information complete
data_labels <- left_join(data_labels, med_complete, by = "study_id")
```

```{r}

#| echo: false
# See if all variables are represented 
setdiff(names(data_no_labels), names(data_labels))
```

Because of the above codes, some changes have been made to the data. Let us fix it. We shall rename chronic_med_condition_type\_\_0_1 to its original name chronic_med_condition\_\_\_10

```{r}

#| echo: false
# Because of the above codes, some changes have been made to the data. Let us fix it. We shall rename chronic_med_condition_type__0_1 to its original name chronic_med_condition___10

names(data_labels)[names(data_labels) == "chronic_med_condition_type__0_1"] <- "chronic_med_condition_type___10"
```

```{r}

#| echo: false
#Before we proceed, we shall replace any Unchecked with "No", Checked with "Yes", and Unknown to NA Let is look again.

setdiff(names(data_no_labels), names(data_labels))
```

```{r}

#| echo: false
# Cool! We are good to go
data_labels[data_labels == "Unchecked"] <- "No" # Replace every 'Unchecked with "No"
data_labels[data_labels == "Checked"] <- "Yes" # Replace every "Checked with "Yes"
data_labels[data_labels == "Unknown"] <- NA # Replace every "Unknown" with NA
```

```{r}

#| echo: false
# Let us get the number of antibiotics for each participants
data_labels <- data_labels %>%
  mutate(number_of_antibiotics = rowSums(across(starts_with("med_name"), ~ !is.na(.)), na.rm = TRUE))

data_labels <- data_labels %>%
  mutate(
    province = case_when(
      str_detect(study_id, "[ABC]") ~ "North", 
      str_detect(study_id, "[DEF]") ~ "Central",
      str_detect(study_id, "[GHI]") ~ "Eastern",
      str_detect(study_id, "[JKL]") ~ "North Central",
      str_detect(study_id, "[MNO]") ~ "South",
      str_detect(study_id, "[PQR]") ~ "Western",
      TRUE ~ "Other"))
```

```{r}
# creating new variable to categorize hospitals (new categories: Primary, Secondary and Tertiary hospitals)
# TH, NH and DG -> Tertiary, BH -> Secondary, DH -> Primary
data_labels <- data_labels %>%
  mutate(
    Hospital_Grade = case_when(
      antibiotic_study == "Teaching : (TH)" ~ "Tertiary Healthcare", 
      antibiotic_study == "National: (NH)" ~ "Tertiary Healthcare",
      antibiotic_study == "Base: (BH)" ~ "Secondary Healthcare",
      antibiotic_study == "Divisional Hospital: (DH)" ~ "Primary Healthcare",
      antibiotic_study == "District General Hospital: (DG)" ~ "Tertiary Healthcare",
      TRUE ~ "Other"))

```










## Prevalence – across hospitals, province, ward type….

### Hospitals

Antibiotic therapies across hospitals

```{r}

#| echo: false
hospital_data <- data_labels %>%   
  select(antibiotic_therapy, Hospital_Grade, number_of_antibiotics) %>% 
  group_by(Hospital_Grade) %>%  
  summarise(    
    total = sum(antibiotic_therapy %in% c("Yes", "No"), na.rm = TRUE),  
    positives = sum(antibiotic_therapy == "Yes", na.rm = TRUE),    
    prevalence = paste0(round((mean(antibiotic_therapy == "Yes", na.rm = TRUE)) * 100, 2), "%"),  # Formatted prevalence    
    sum_antibiotics = sum(number_of_antibiotics, na.rm = TRUE), 
    min_number_of_antibiotics = min(number_of_antibiotics[number_of_antibiotics > 0], na.rm = TRUE),   # Min, excluding zeros
    max_number_of_antibiotics = max(number_of_antibiotics[number_of_antibiotics > 0], na.rm = TRUE),  # Max, excluding zeros
    median_number_of_antibiotics = median(number_of_antibiotics[number_of_antibiotics > 0], na.rm = TRUE),  # Median, excluding zeros
    iqr_25th = quantile(number_of_antibiotics[number_of_antibiotics > 0], 0.25, na.rm = TRUE),  # 25th percentile
    iqr_75th = quantile(number_of_antibiotics[number_of_antibiotics > 0], 0.75, na.rm = TRUE)  # 75th percentile
    
  ) %>% 
  arrange(prevalence) # Sort using numeric prevalence before formatting it

ft_hospital <-  hospital_data %>% 
  gt() %>%  # Convert to gt table format
  tab_header(
    title = "Antibiotic Use Summary by Hospital"
  ) %>%
  fmt_number(
    columns = c(min_number_of_antibiotics, max_number_of_antibiotics, median_number_of_antibiotics, iqr_25th, iqr_75th),
    decimals = 0  # Format counts
  ) %>%
  cols_label(
    Hospital_Grade = "Healthcare",
    total = "Total Patients",
    positives = "Positive Cases",
    prevalence = "Prevalence",
    sum_antibiotics = "Total number of Antibiotic",
    min_number_of_antibiotics = "Min number of Antibiotics",
    max_number_of_antibiotics = "Max number of Antibiotics",
    median_number_of_antibiotics = "Median number of Antibiotics",
    iqr_25th = "25th Percentile",
    iqr_75th = "75th Percentile"
  ) %>%
  tab_options(
    table.font.size = px(14),
    heading.align = "center"
  ) 
ft_hospital

```



### Province

Antibiotic therapies across provinces

```{r}
#| echo: false

province_data <- data_labels %>%   
  select(antibiotic_therapy, province, number_of_antibiotics) %>% 
  group_by(province) %>%  
  summarise(    
    total = sum(antibiotic_therapy %in% c("Yes", "No"), na.rm = TRUE),  
    positives = sum(antibiotic_therapy == "Yes", na.rm = TRUE),    
    prevalence = paste0(round((mean(antibiotic_therapy == "Yes", na.rm = TRUE)) * 100, 2), "%"),  # Formatted prevalence    
    sum_antibiotics = sum(number_of_antibiotics, na.rm = TRUE), 
    min_number_of_antibiotics = min(number_of_antibiotics[number_of_antibiotics > 0], na.rm = TRUE),   # Min, excluding zeros
    max_number_of_antibiotics = max(number_of_antibiotics[number_of_antibiotics > 0], na.rm = TRUE),  # Max, excluding zeros
    median_number_of_antibiotics = median(number_of_antibiotics[number_of_antibiotics > 0], na.rm = TRUE),  # Median, excluding zeros
    iqr_25th = quantile(number_of_antibiotics[number_of_antibiotics > 0], 0.25, na.rm = TRUE),  # 25th percentile
    iqr_75th = quantile(number_of_antibiotics[number_of_antibiotics > 0], 0.75, na.rm = TRUE)  # 75th percentile
    
  ) %>% 
  arrange(prevalence)  # Sort using numeric prevalence before formatting it

ft_province <- province_data %>% 
  gt() %>%  # Convert to gt table format
  tab_header(
    title = "Antibiotic Use Summary by Province"
  ) %>%
  fmt_number(
    columns = c(min_number_of_antibiotics, max_number_of_antibiotics, median_number_of_antibiotics, iqr_25th, iqr_75th),
    decimals = 0  # Format counts
  ) %>%
  cols_label(
    total = "Total Patients",
    positives = "Positive Cases",
    prevalence = "Prevalence",
    sum_antibiotics = "Total number of Antibiotic",
    min_number_of_antibiotics = "Min number of Antibiotics",
    max_number_of_antibiotics = "Max number of Antibiotics",
    median_number_of_antibiotics = "Median number of Antibiotics",
    iqr_25th = "25th Percentile",
    iqr_75th = "75th Percentile"
  ) %>%
  tab_options(
    table.font.size = px(14),
    heading.align = "center"
  )
ft_province
```


### Ward type

Antibiotic therapies across wards

```{r}

#| echo: false
ward_data <- data_labels %>%   
  select(antibiotic_therapy, ward, number_of_antibiotics) %>% 
  group_by(ward) %>%  
  summarise(    
    total = sum(antibiotic_therapy %in% c("Yes", "No"), na.rm = TRUE),  
    positives = sum(antibiotic_therapy == "Yes", na.rm = TRUE),    
    prevalence = paste0(round((mean(antibiotic_therapy == "Yes", na.rm = TRUE)) * 100, 2), "%"),  # Formatted prevalence    
    sum_antibiotics = sum(number_of_antibiotics, na.rm = TRUE), 
    min_number_of_antibiotics = min(number_of_antibiotics[number_of_antibiotics > 0], na.rm = TRUE),   # Min, excluding zeros
    max_number_of_antibiotics = max(number_of_antibiotics[number_of_antibiotics > 0], na.rm = TRUE),  # Max, excluding zeros
    median_number_of_antibiotics = median(number_of_antibiotics[number_of_antibiotics > 0], na.rm = TRUE),  # Median, excluding zeros
    iqr_25th = quantile(number_of_antibiotics[number_of_antibiotics > 0], 0.25, na.rm = TRUE),  # 25th percentile
    iqr_75th = quantile(number_of_antibiotics[number_of_antibiotics > 0], 0.75, na.rm = TRUE)  # 75th percentile
    
  ) %>% 
  arrange(prevalence)  # Sort using numeric prevalence before formatting it

ft_ward <- ward_data %>% 
  gt() %>%  # Convert to gt table format
  tab_header(
    title = "Antibiotic Use Summary by Ward"
  ) %>%
  fmt_number(
    columns = c(min_number_of_antibiotics, max_number_of_antibiotics, median_number_of_antibiotics, iqr_25th, iqr_75th),
    decimals = 0  # Format counts
  ) %>%
  cols_label(
    total = "Total Patients",
    positives = "Positive Cases",
    prevalence = "Prevalence",
    sum_antibiotics = "Total number of Antibiotic",
    min_number_of_antibiotics = "Min number of Antibiotics",
    max_number_of_antibiotics = "Max number of Antibiotics",
    median_number_of_antibiotics = "Median number of Antibiotics",
    iqr_25th = "25th Percentile",
    iqr_75th = "75th Percentile"
  ) %>%
  tab_options(
    table.font.size = px(14),
    heading.align = "center"
  )
ft_ward
```





## Common antibiotics – across hospitals, province and ward type....


```{r}
#| echo: false
# cleaning common antibiotic names

# Reshape the data so all medication names fall under "medications"
medications_df <- data_labels %>%
  select(study_id, Hospital_Grade, province, ward, starts_with("med_name"), starts_with("other_med")) %>%
  pivot_longer(cols = -c(Hospital_Grade, province, ward, study_id), 
               names_to = "medication_type", values_to = "medications") %>%
  filter(!is.na(medications))  # Remove NA values


```

```{r}
# cleaning antibiotics types
# spelling errors, duplicates and similar antibiotics

cleaned_medications <- medications_df %>%
  mutate(
    medications_clean = medications %>%
      str_to_title() %>%
      str_trim() %>%
      
      # -- HANDLING FOR DRUGS THAT COULD HAVE PARTIAL MATCHES --
      str_replace_all(
        "Piperacillin[ ]*[/]?[ ]*Tazobactam|Piperacillin Tazobactam|Piperacillin   Tazobactam", "Piperacillin/Tazobactam") %>%
      str_replace_all("^Piperacillin$|^piperacillin$", "Piperacillin/Tazobactam") %>% 
      str_replace_all("Trimethoprim (&|/) Sulfamethox|Cotrim(oxazole)?","Trimethoprim/Sulfamethoxazole") %>%
      
      str_replace_all("^C Penicillin$|^C Penicliin$|^C\\.Penicillin$|^Penicillin$|^C.penicillin$", "Penicillin") %>%
      str_replace_all("Amoxicillin/Ampicillin", "Amoxicillin/Ampicillin") %>%
      str_replace_all("Cefoperazone(/| )Sulbactam", "Cefoperazone/Sulbactam") %>%
      str_replace_all("Teicoplanin|Teicoplenin", "Teicoplanin") %>%
      str_replace_all("(?i)\\b(Vancomycin|Teicoplanin|Teicoplenin)\\b", "Vancomycin/Teicoplanin") %>% 
      str_replace_all("Erythromycin/ Azithromycin", "Erythromycin/Azithromycin") %>%
      str_replace_all("(?i)Fluoroquinolone[ ]*\\(?(i\\.e\\. )?Cipro\\)?|Ciprofloxacin",
                      "Fluoroquinolone (I.e. Cipro)") %>%
      str_replace_all("Colistin|Colisin|Colistin Nebulize", "Colistin") %>%
      str_replace_all("(?i)(Aminogylcoside\\s*\\(?i\\.e\\.\\s*Gentamicin\\)?|Aminogylcoside\\(Gentamicin\\)|Gentamicin)",  "Aminoglycoside (I.e. Gentamicin)") %>%
      str_replace_all("^Doxycyclin$", "Doxycycline") %>%
      str_replace_all("Linozolid|Linozoild", "Linezolid") %>%
      str_replace_all("Rifaximin|Rifaxime", "Rifaximin") %>%
      
      # -- FINAL CLEANUP --
      str_replace_all(" {2,}", " ") %>%  # Replace multiple spaces with single space
      str_replace_all("/ ", "/") %>%     # Fix bad spacing in combinations
      str_replace_all(" /", "/")
  )

# Verify results
medication_counts = cleaned_medications %>%
  count(medications_clean) %>%
  arrange(desc(n))
```





### Hospitals

Common antibiotics across hospitals
```{r}
# Count medication occurrences
medication_freq_hospital <- cleaned_medications %>%
  select(-c(province, ward,study_id)) %>% 
  group_by(Hospital_Grade, medications_clean) %>%
  summarise(freq = n(), .groups = "drop") %>%
  filter(!str_starts(medications_clean, "Other")) %>%
  arrange(desc(freq))

# View top medications
head(medication_freq_hospital)
```


#### Actual count

```{r results='asis', warning=FALSE, message=FALSE}

#| echo: false


# Create a list to store tables
med_hos_table_list <- list()

# Loop through each hospital and generate a separate gt table
for (hospital in unique(medication_freq_hospital$Hospital_Grade)) {
  hospital_med_data <- medication_freq_hospital %>%
    filter(Hospital_Grade == hospital) %>%
    mutate(percentage = round((freq / sum(freq)) * 100, 2)) %>%  # Calculate percentage
    select(medications_clean, freq, percentage) %>%  # Keep only relevant columns
    arrange(desc(freq))  # Sort by frequency
  gt_table <- hospital_med_data %>%
    gt() %>%
    tab_header(
      title = md(paste("Medication Frequency -", hospital))
    ) %>%
    cols_label(
      medications_clean = "Medication",
      freq = "Frequency",
      percentage = "Percentage (%)"
    ) %>%
    fmt_number(columns = everything(), decimals = 1)  # Format all numeric columns
  
  # Store table in the list
  med_hos_table_list[[hospital]] <- gt_table
  
  print(gt_table)
}
```



### Hospitals _ Antibiotic Categorization

```{r}
cleaned_medications_N <- cleaned_medications %>%
  mutate(
    Medication_Category = case_when(
      medications_clean %in% c(
        "Colistin", "Linezolid", "Rifampicin", "Sulbactam",
        "Levofloxacin", "Cefoperazone/Sulbactam", "Cefepime", "Ceftazidime"
      ) ~ "Reserve Group",
      
      medications_clean %in% c(
        "3rd Gen Ceph (I.e. Ceftriaxone)", "Vancomycin/Teicoplanin",
        "Carbapenem (I.e. Meropenem)", "Aminoglycoside (I.e. Gentamicin)",
        "Erythromycin/Azithromycin", "Imipenem", "Ceftriaxon + Salbactam",
        "Fluoroquinolone (I.e. Cipro)", "Rifaximin", "Amikacin",
        "Piperacillin/Tazobactam", "Vancomycin/Teicoplanin", "Cefixime"
      ) ~ "Watch Group",
      
      medications_clean %in% c(
        "Erythromycin/Azithromycin", "Clarithromycin", "2nd Gen Ceph (I.e. Cefuroxime)",
        "Tetracycline/Doxycycline", "Amoxicillin/Ampicillin", "Doxycycline",
        "Aminoglycoside (I.e. Gentamicin)", "1st Gen Ceph (I.e. Cephalexin)",
        "Amoxicillin & Clavulanic Acid", "Metronidazole", "Nitrofurantoin",
        "Flucloxacillin/Cloxacillin/Nafcillin", "Clindamycin", "Penicillin",
        "Extended Penicillin (I.e. Ticarcillin)"
      ) ~ "Access Group",
      
      TRUE ~ "Other"
    )
  )

```


```{r}
# Categorize Antibiotics
cleaned_medications_category = cleaned_medications_N %>%
  select(-c(province,ward, study_id)) %>% 
  group_by(Hospital_Grade, Medication_Category) %>%
  summarise(freq = n(), .groups = "drop") %>%
  filter(!str_starts(Medication_Category, "Other")) %>%
  arrange(desc(freq))

# View top medications
head(cleaned_medications_category)
```

```{r results='asis', warning=FALSE, message=FALSE}

#| echo: false


# Create a list to store tables
med_hos_table_list_N <- list()

# Loop through each hospital and generate a separate gt table
for (hospital in unique(cleaned_medications_category$Hospital_Grade)) {
  hospital_med_data_N <- cleaned_medications_category %>%
    filter(Hospital_Grade == hospital) %>%
    mutate(percentage = round((freq / sum(freq)) * 100, 2)) %>%  # Calculate percentage
    select(Medication_Category, freq, percentage) %>%  # Keep only relevant columns
    arrange(desc(freq))  # Sort by frequency
  gt_table <- hospital_med_data_N %>%
    gt() %>%
    tab_header(
      title = md(paste("Medication Frequency -", hospital))
    ) %>%
    cols_label(
      Medication_Category = "Medication Category",
      freq = "Frequency",
      percentage = "Percentage (%)"
    ) %>%
    fmt_number(columns = everything(), decimals = 1)  # Format all numeric columns
  
  # Store table in the list
  med_hos_table_list_N[[hospital]] <- gt_table
  
  print(gt_table)
}
```



### Province

Common antibiotics across provinces

```{r}

# Count medication occurrences
medication_freq_province <- cleaned_medications %>%
  select(-c(Hospital_Grade, ward,study_id)) %>% 
  group_by(province, medications_clean) %>%
  summarise(freq = n(), .groups = "drop") %>%
  filter(!str_starts(medications_clean, "Other")) %>%
  arrange(desc(freq))

# View top medications
head(medication_freq_province)
```


#### Actual count

```{r results='asis', warning=FALSE, message=FALSE}

#| include: false
# Create a list to store tables
med_prov_table_list <- list()

# Loop through each province and generate a separate gt table
for (Province in unique(medication_freq_province$province)) {
  province_med_data <- medication_freq_province %>%
    filter(province == Province) %>%
    mutate(percentage = round((freq / sum(freq)) * 100,2)) %>%  # Calculate percentage
    select(medications_clean, freq, percentage) %>%  # Keep only relevant columns
    arrange(desc(freq))  # Sort by frequency
  gt_table <- province_med_data %>%
    gt() %>%
    tab_header(
      title = md(paste("Medication Frequency -", Province, "Province"))
    ) %>%
    cols_label(
      medications_clean = "Medication",
      freq = "Frequency",
      percentage = "Percentage (%)"
    ) %>%
    fmt_number(columns = vars(percentage), decimals = 1)  # Format percentage
  # Store table in the list
  med_prov_table_list[[Province]] <- gt_table
  # Print each table
  print(gt_table)
}


```




### Ward type

Common antibiotics across ward types

```{r}

# Count medication occurrences
medication_freq_ward <- cleaned_medications %>%
  select(-c(Hospital_Grade,province, study_id)) %>% 
  group_by(ward, medications_clean) %>%
  summarise(freq = n(), .groups = "drop") %>%
  filter(!str_starts(medications_clean, "Other")) %>%
  arrange(desc(freq))

# View top medications
head(medication_freq_ward)
```



#### Frequency table

```{r results='asis', warning=FALSE, message=FALSE}

#| include: false
# Create a list to store tables
med_ward_table_list <- list()

# Loop through each ward and generate a separate gt table
for (Ward in unique(medication_freq_ward$ward)) {
  ward_med_data <- medication_freq_ward %>%
    filter(ward == Ward) %>%
    mutate(percentage = round((freq / sum(freq)) * 100,2)) %>%  # Calculate percentage
    select(medications_clean, freq, percentage) %>%  # Keep only relevant columns
    arrange(desc(freq))  # Sort by frequency
  gt_table <- ward_med_data %>%
    gt() %>%
    tab_header(
      title = md(paste("Medication Frequency -", Ward))
    ) %>%
    cols_label(
      medications_clean = "Medication",
      freq = "Frequency",
      percentage = "Percentage (%)"
    ) %>%
    fmt_number(columns = vars(percentage), decimals = 1)  # Format percentage
  # Store table in the list
  med_ward_table_list[[Ward]] <- gt_table
  # Print each table
  print(gt_table)
}

```



#### Combining medical and Surgical wards to get Common Antibiotics-------------

```{r}

# Count medication occurrences
medication_freq_ward_combined <- cleaned_medications %>%
  select(-c(Hospital_Grade,province,study_id)) %>% 
  mutate(ward = case_when(
    ward == "Surgical ward" ~ "Medical and Surgical Ward",
    ward == "Medical ward" ~ "Medical and Surgical Ward",
    TRUE ~ ward)) %>% 
  group_by(ward, medications_clean) %>%
  summarise(freq = n(), .groups = "drop") %>%
  filter(!str_starts(medications_clean, "Other")) %>%
  arrange(desc(freq))

# View top medications
head(medication_freq_ward_combined)
```


```{r results='asis', warning=FALSE, message=FALSE}

#| include: false
# Create a list to store tables
med_ward_table_list_combined <- list()

# Loop through each ward and generate a separate gt table
for (Ward in unique(medication_freq_ward_combined$ward)) {
  ward_data_c <- medication_freq_ward_combined %>%
    filter(ward == Ward) %>%
    mutate(percentage = round((freq / sum(freq)) * 100,2)) %>%  # Calculate percentage
    select(medications_clean, freq, percentage) %>%  # Keep only relevant columns
    arrange(desc(freq))  # Sort by frequency
  gt_table <- ward_data_c %>%
    gt() %>%
    tab_header(
      title = md(paste("Medication Frequency -", Ward))
    ) %>%
    cols_label(
      medications_clean = "Medication",
      freq = "Frequency",
      percentage = "Percentage (%)"
    ) %>%
    fmt_number(columns = vars(percentage), decimals = 1)  # Format percentage
  # Store table in the list
  med_ward_table_list_combined[[Ward]] <- gt_table
  # Print each table
  print(gt_table)
}


```






# Indications for antibiotic use – across hospitals, province ward type…. 

```{r}

#| include: false
med_indication <- data_labels %>%
  select(study_id, Hospital_Grade,province, ward, starts_with("antibiotic_indication"), starts_with("other_indication")) %>%
  pivot_longer(cols = -c(Hospital_Grade, province, ward, study_id), 
               names_to = "indication_type", values_to = "indication") %>%
  filter(!is.na(indication))
```


```{r}
# cleaning Indications for antibiotic use
# spelling errors, duplicates and similar antibiotics

cleaned_med_indication <- med_indication %>%
  mutate(
    indication_clean = indication %>%
      tolower() %>%                          # Convert to lowercase
      str_trim() %>%                         # Remove leading/trailing whitespace
      str_replace_all("[?.]", "") %>% 
      str_replace_all("\\s+", " ") %>%       # Collapse multiple spaces
      str_replace_all("[^a-z0-9 /]", "") %>% 
      
      str_replace_all("\\bprophylaxis\\b", "prophylaxis") %>%
      str_replace_all("\\buti\\b", "urinary tract infection") %>%
      str_replace_all("\\blrti\\b", "lower respiratory tract infection") %>%
      str_replace_all("\\burti\\b", "upper respiratory tract infection") %>%
      str_replace_all("\\bssti\\b", "skin and soft tissue infection") %>%
      str_replace_all("\\bgi\\b", "gastrointestinal") %>%
      str_replace_all("\\bcns\\b", "central nervous system") %>%
      str_replace_all("\\bsbp\\b", "spontaneous bacterial peritonitis") %>%
      
      str_replace_all("\\bappendis\\b", "appendicitis") %>%
      str_replace_all("\\bendocardities\\b", "endocarditis") %>%
      str_replace_all("\\bperitonities\\b", "peritonitis") %>%
      str_replace_all("\\bha?emorrh?oids\\b", "hemorrhoids") %>%
      str_replace_all("\\bba\\b", "bronchial asthma") %>%
      
      str_remove_all("\\bprophylaxis for\\b") %>%
      str_remove_all("\\bsuspect\\b") %>%
      str_remove_all("\\bsecondary\\b") %>%
      str_remove_all("\\bprimary\\b") %>%
      str_trim(),
    
    indication_clean = case_when(
      str_detect(indication_clean, "trauma.*mening") ~ "trauma with meningitis prophylaxis",
      str_detect(indication_clean, "wound.*prophylaxis") ~ "wound prophylaxis",
      str_detect(indication_clean, "spontaneous bacterial peritonitis") ~ "spontaneous bacterial peritonitis",
      TRUE ~ indication_clean
    )
  )

# Verify results
indication_counts = cleaned_med_indication %>%
  count(indication_clean) %>%
  arrange(desc(n))

indication_counts
```


### Hospitals

```{r}
# Count medication occurrences
indication_hospital <- cleaned_med_indication %>%
  select(-c(province, ward, study_id)) %>% 
  group_by(Hospital_Grade, indication_clean) %>%
  summarise(freq = n(), .groups = "drop") %>%
  filter(!str_starts(indication_clean, "other"), !indication_clean == "complete") %>%
  arrange(desc(freq))

# View top medications
head(indication_hospital)
```

```{r results='asis', warning=FALSE, message=FALSE}

#| include: false
# Get unique hospital categories
hospital_table_list <- unique(indication_hospital$Hospital_Grade)  

# Initialize a list to store formatted gt tables
ind_hospital_tables_gt <- list()

# Loop through each hospital and generate a separate table
for (hospital in hospital_table_list) {
  hospital_table_data <- indication_hospital %>% 
    filter(Hospital_Grade == hospital)
  
  # Compute the total sum of freq for this specific hospital category
  total_freq <- sum(hospital_table_data$freq, na.rm = TRUE)
  
  # Create summary table
  t <- hospital_table_data %>%
    select(indication_clean, freq) %>%
    group_by(indication_clean) %>%
    summarise(
      count = sum(freq, na.rm = TRUE),  # Sum frequency for each indication_clean
      percentage = round((count / total_freq) * 100, 2)  # Percentage within the hospital category
      
    ) %>%
    
    arrange(desc(count))  # Sort by frequency
  
  # Convert to gt table
  gt_table <- t %>%
    gt() %>%
    tab_header(
      title = paste("Medication Indication Summary -", hospital),
      subtitle = paste("Total Medication Indications:", total_freq)
    ) %>%
    fmt_number(
      columns = percentage,
      decimals = 2
    ) %>%
    cols_label(
      indication_clean = "Medication Indication",
      count = "Total Count",
      percentage = "Percentage (%)"
    ) %>%
    tab_options(
      table.font.size = "medium",
      heading.title.font.size = "large",
      heading.subtitle.font.size = "medium"
    )
  
  # Store the gt table in the list
  
  ind_hospital_tables_gt[[hospital]] <- gt_table
  
}

# Display all tables

ind_hospital_tables_gt


```



### Provinces

```{r}
# Count medication occurrences
indication_province <- cleaned_med_indication %>%
  select(-c(Hospital_Grade, ward, study_id)) %>% 
  group_by(province, indication_clean) %>%
  summarise(freq = n(), .groups = "drop") %>%
  filter(!str_starts(indication_clean, "other"), !indication_clean == "complete") %>%
  arrange(desc(freq))

# View top medications
head(indication_province)
```

```{r results='asis', warning=FALSE, message=FALSE}

#| include: false
# Get unique province categories
province_table_list <- unique(indication_province$province)  

# Initialize a list to store formatted gt tables
ind_province_tables_gt <- list()

# Loop through each province and generate a separate table
for (Province in province_table_list) {
  province_table_data <- indication_province %>% 
    filter(province == Province)
  
  # Compute the total sum of freq for this specific hospital category
  total_freq <- sum(province_table_data$freq, na.rm = TRUE)
  
  # Create summary table
  t <- province_table_data %>%
    select(indication_clean, freq) %>%
    group_by(indication_clean) %>%
    summarise(
      count = sum(freq, na.rm = TRUE),  # Sum frequency for each indication_clean
      percentage = round((count / total_freq) * 100, 2)  # Percentage within the hospital category
      
    ) %>%
    
    arrange(desc(count))  # Sort by frequency
  
  # Convert to gt table
  gt_table <- t %>%
    gt() %>%
    tab_header(
      title = paste("Medication Indication Summary -", Province),
      subtitle = paste("Total Medication Indications:", total_freq)
    ) %>%
    fmt_number(
      columns = percentage,
      decimals = 2
    ) %>%
    cols_label(
      indication_clean = "Medication Indication",
      count = "Total Count",
      percentage = "Percentage (%)"
    ) %>%
    tab_options(
      table.font.size = "medium",
      heading.title.font.size = "large",
      heading.subtitle.font.size = "medium"
    )
  
  # Store the gt table in the list
  
  ind_province_tables_gt[[Province]] <- gt_table
  
}

# Display all tables

ind_province_tables_gt
```


### Ward types


```{r}
# Count medication occurrences
indication_ward <- cleaned_med_indication%>%
  select(-c(Hospital_Grade, province, study_id)) %>% 
  group_by(ward, indication_clean) %>%
  summarise(freq = n(), .groups = "drop") %>%
  filter(!str_starts(indication_clean, "other"), !indication_clean == "complete") %>%
  arrange(desc(freq))

# View top medications
head(indication_ward)
```

```{r results='asis', warning=FALSE, message=FALSE}

#| include: false
# Get unique province categories
ward_table_list <- unique(indication_ward$ward)  

# Initialize a list to store formatted gt tables
ind_ward_tables_gt <- list()

# Loop through each province and generate a separate table
for (Ward in ward_table_list) {
  ward_table_data <- indication_ward %>% 
    filter(ward == Ward)
  
  # Compute the total sum of freq for this specific hospital category
  total_freq <- sum(ward_table_data$freq, na.rm = TRUE)
  
  # Create summary table
  t <- ward_table_data %>%
    select(indication_clean, freq) %>%
    group_by(indication_clean) %>%
    summarise(
      count = sum(freq, na.rm = TRUE),  # Sum frequency for each indication_clean
      percentage = round((count / total_freq) * 100, 2)  # Percentage within the hospital category
      
    ) %>%
    
    arrange(desc(count))  # Sort by frequency
  
  # Convert to gt table
  gt_table <- t %>%
    gt() %>%
    tab_header(
      title = paste("Medication Indication Summary -", Ward),
      subtitle = paste("Total Medication Indications:", total_freq)
    ) %>%
    fmt_number(
      columns = percentage,
      decimals = 2
    ) %>%
    cols_label(
      indication_clean = "Medication Indication",
      count = "Total Count",
      percentage = "Percentage (%)"
    ) %>%
    tab_options(
      table.font.size = "medium",
      heading.title.font.size = "large",
      heading.subtitle.font.size = "medium"
    )
  
  # Store the gt table in the list
  
  ind_ward_tables_gt[[Ward]] <- gt_table
  
}

# Display all tables

ind_ward_tables_gt
```



### All Indications 
(regardless of hospital, province and ward)


```{r}

#| include: false
med_indication_all <- cleaned_med_indication %>%
  select(-c(Hospital_Grade,province, ward, study_id)) %>% 
  group_by(indication_clean) %>%
  summarise(freq = n(), .groups = "drop") %>%
  filter(!str_starts(indication_clean, "other"), !indication_clean == "complete") %>%
  arrange(desc(freq))

# View top medications
head(med_indication_all)
```



```{r results='asis', warning=FALSE, message=FALSE}

#| include: false

# Compute the total sum of freq for this specific hospital category
total_freq <- sum(med_indication_all$freq, na.rm = TRUE)

# Create summary table
t <- med_indication_all %>%
  group_by(indication_clean) %>%
  summarise(
    count = sum(freq, na.rm = TRUE),
    percentage = round((count / total_freq) * 100, 2)) %>%
  arrange(desc(count))  # Sort by frequency

# Convert to gt table
gt_table <- t %>%
  gt() %>%
  tab_header(
    title = paste("Medication Indication Summary - All"),
    subtitle = paste("Total Medication Indications:", total_freq)
  ) %>%
  fmt_number(
    columns = percentage,
    decimals = 2
  ) %>%
  cols_label(
    indication_clean = "Medication Indication",
    count = "Total Count",
    percentage = "Percentage (%)"
  ) %>%
  tab_options(
    table.font.size = "medium",
    heading.title.font.size = "large",
    heading.subtitle.font.size = "medium"
  )



# Display all tables

gt_table

```



# Microbiologic data associated

### Hospitals

```{r}

#| echo: false
culture_hospital_data <- data_labels %>%   
  select(positive_culture, relevant_culture, Hospital_Grade) %>% 
  group_by(Hospital_Grade) %>%  
  summarise(  
    Total_Culture = sum(relevant_culture %in% c("Yes", "No"), na.rm = TRUE),
    Culture_count = sum(relevant_culture == "Yes",na.rm = TRUE),
    Culture_Percentage = paste0(round((mean(relevant_culture == "Yes", na.rm = TRUE)) * 100, 2), "%"),
    total = sum(positive_culture %in% c("Yes", "No", "Culture pending"), na.rm = TRUE),  
    positives = sum(positive_culture == "Yes", na.rm = TRUE),    
    percentage = paste0(round((mean(positive_culture == "Yes", na.rm = TRUE)) * 100, 2), "%")  # Formatted prevalence
  )

culture_hospital_table <- culture_hospital_data %>% 
  gt() %>%  # Now properly connected to the summarised data
  tab_header(
    title = "Culture micro organism data by Healthcare"
  ) %>%
  cols_label(
    Hospital_Grade = "Healthcare",
    Total_Culture = "Total Cases",
    Culture_count = "Total Ordered Culture",
    Culture_Percentage = "Ordered Culture Percentage",
    total = "Culture Complete",
    positives = "Positive Culture",
    percentage = "Percentage"
  ) %>%
  tab_options(
    table.font.size = px(14),
    heading.align = "center"
  )
culture_hospital_table

```


### Provinces

```{r}

#| echo: false
culture_province_data <- data_labels %>%   
  select(positive_culture, relevant_culture, province) %>% 
  group_by(province) %>%  
  summarise( 
    Total_Culture = sum(relevant_culture %in% c("Yes", "No"), na.rm = TRUE),
    Culture_count = sum(relevant_culture == "Yes",na.rm = TRUE),
    Culture_Percentage = paste0(round((mean(relevant_culture == "Yes", na.rm = TRUE)) * 100, 2), "%"),
    total = sum(positive_culture %in% c("Yes", "No", "Culture pending"), na.rm = TRUE),  
    positives = sum(positive_culture == "Yes", na.rm = TRUE),    
    percentage = round((mean(positive_culture == "Yes", na.rm = TRUE)) * 100, 2)
  ) %>%
  arrange(desc(percentage)) %>%  # Now sorts correctly by numeric value
  mutate(percentage = paste0(percentage, "%"))

culture_province_table <- culture_province_data %>% 
  gt() %>%  # Now properly connected to the summarised data
  tab_header(
    title = "Culture micro organism data by Province"
  ) %>%
  cols_label(
    province = "Province",
    Total_Culture = "Total Cases",
    Culture_count = "Total Ordered Culture",
    Culture_Percentage = "Ordered Culture Percentage",
    total = "Culture Complete",
    positives = "Positive Culture",
    percentage = "Percentage"
  ) %>%
  tab_options(
    table.font.size = px(14),
    heading.align = "center"
  )
culture_province_table

```


### Ward types

```{r}

#| echo: false
culture_ward_data <- data_labels %>%   
  select(positive_culture, relevant_culture, ward) %>% 
  group_by(ward) %>%  
  summarise(
    Total_Culture = sum(relevant_culture %in% c("Yes", "No"), na.rm = TRUE),
    Culture_count = sum(relevant_culture == "Yes",na.rm = TRUE),
    Culture_Percentage = paste0(round((mean(relevant_culture == "Yes", na.rm = TRUE)) * 100, 2), "%"),
    total = sum(positive_culture %in% c("Yes", "No", "Culture pending"), na.rm = TRUE),  
    positives = sum(positive_culture == "Yes", na.rm = TRUE),    
    percentage = round((mean(positive_culture == "Yes", na.rm = TRUE)) * 100, 2)) %>%
  arrange(desc(percentage)) %>%  # Now sorts correctly by numeric value
  mutate(percentage = paste0(percentage, "%"))


culture_ward_table <- culture_ward_data %>% 
  gt() %>%  # Now properly connected to the summarised data
  tab_header(
    title = "Culture micro organism data by Ward"
  ) %>%
  cols_label(
    ward = "Ward",
    Total_Culture = "Total Cases",
    Culture_count = "Total Ordered Culture",
    Culture_Percentage = "Ordered Culture Percentage",
    total = "Culture Complete",
    positives = "Positive Culture",
    percentage = "Percentage"
  ) %>%
  tab_options(
    table.font.size = px(14),
    heading.align = "center"
  )

culture_ward_table

gtsave(culture_ward_table, "Culture_Ward_Summary.docx")
```




## Culture Organism

```{r}

#| echo: false
# Reshape the data so all micro organisms names fall under "culture_organism"
microbiology_df <- data_labels %>%
  select(study_id, Hospital_Grade, province, ward, culture_organism, 
         culture_organism_2, culture_organism_3, culture_organism_4) %>%
  pivot_longer(cols = -c(Hospital_Grade, province, ward, study_id),
               names_to = "culture_num", values_to = "culture_organism") %>%
  filter(!is.na(culture_organism))  # Remove NA values
```


```{r}
cleaned_microbiology <- microbiology_df %>%
  mutate(
    # First clean and standardize the text
    organism_clean = str_to_lower(culture_organism) %>%
      str_trim() %>%
      str_remove_all("\\d\\+?|\\d\\.|colony count.*|>.*|\\(.*\\)") %>%
      str_replace_all("[^[:alnum:]]", " ") %>%
      str_replace_all("\\s+", " ") %>%
      str_trim(),
    
    organism_clean = case_when(
      str_detect(organism_clean, "acinetobacter") ~ "Acinetobacter spp",
      
      str_detect(organism_clean, "e[ .-]?coli|escherichia|eschirichia") ~ "Escherichia coli",
      str_detect(organism_clean, "esbl") ~ "ESBL-producing organisms",
      str_detect(organism_clean, "coliform|choliform") ~ "Coliform spp",
      str_detect(organism_clean, "klebsiella") ~ "Klebsiella spp",
      str_detect(organism_clean, "proteus") ~ "Proteus spp",
      str_detect(organism_clean, "gram negative|gnb") ~ "Gram-negative bacilli",
      
      str_detect(organism_clean, "pseudomonas|p.?aeruginosa|auriginosa") ~ "Pseudomonas aeruginosa",
      
      str_detect(organism_clean, "mrsa|methicillin") ~ "MRSA",
      str_detect(organism_clean, "mssa") ~ "MSSA",
      str_detect(organism_clean, "staphylococcus aureus|staph aureus|ss aureus") ~ "Staphylococcus aureus",
      str_detect(organism_clean, "coagulase negative|cns|staphylococcus epidermidis") ~ "Coagulase-negative staphylococci",
      
      str_detect(organism_clean, "enterococcus|fuecalis") ~ "Enterococcus spp",
      
      str_detect(organism_clean, "streptococcus|strep|milleri") ~ "Streptococcus spp",
      
      str_detect(organism_clean, "candida|yeast") ~ "Candida spp",
      
      str_detect(organism_clean, "corynebacterium|diphtheroid") ~ "Corynebacterium spp",
      str_detect(organism_clean, "clostridium|difficile") ~ "Clostridioides difficile",
      str_detect(organism_clean, "tuberculosis") ~ "Mycobacterium tuberculosis",
      
      str_detect(organism_clean, "mixed|polymicrobial|hmg|multiple") ~ "Mixed growth",
      
      str_detect(organism_clean, "no growth|negative|sterile|pending") ~ "No growth",
      
      str_detect(organism_clean, "normal flora|contaminated|spore bearer") ~ "Normal flora/contaminants",
      
      TRUE ~ "Other"
    )
  )


# View results
count(cleaned_microbiology, organism_clean, sort = TRUE)
```



### Hospitals

```{r}
# Count micro organism occurrences
microbiology_hospital <- cleaned_microbiology %>%
  select(-c(ward, province, study_id)) %>% 
  group_by(Hospital_Grade, organism_clean) %>%
  summarise(freq = n(), .groups = "drop") %>%
  filter(!str_starts(organism_clean, "Other")) %>%
  arrange(desc(freq))

# View top micro organisms
head(microbiology_hospital)

```

```{r results='asis', warning=FALSE, message=FALSE}

#| echo: false
# Create a list to store tables
micro_hos_table_list <- list()

# Loop through each hospital and generate a separate gt table
for (hospital in unique(microbiology_hospital$Hospital_Grade)) {
  hospital_data <- microbiology_hospital %>%
    filter(Hospital_Grade == hospital) %>%
    mutate(percentage = round((freq / sum(freq)) * 100,2)) %>%  # Calculate percentage
    select(organism_clean, freq, percentage) %>%  # Keep only relevant columns
    arrange(desc(freq))  # Sort by frequency
  gt_table <- hospital_data %>%
    gt() %>%
    tab_header(
      title = md(paste("Culture micro organism count -", hospital))
    ) %>%
    cols_label(
      organism_clean = "Micro organism",
      freq = "Frequency",
      percentage = "Percentage (%)"
    ) %>%
    fmt_number(columns = vars(percentage), decimals = 1)  # Format percentage
  # Store table in the list
  micro_hos_table_list[[hospital]] <- gt_table
  # Print each table
  print(gt_table)
}

```



### Provinces
```{r}

#| include: false
# Count micro organism occurrences
microbiology_province <- cleaned_microbiology %>%
  select(-c(ward, Hospital_Grade, study_id)) %>% 
  group_by(province, organism_clean) %>%
  summarise(freq = n(), .groups = "drop") %>%
  filter(!str_starts(organism_clean, "Other")) %>%
  arrange(desc(freq))

# View top micro organisms
head(microbiology_province)
```


```{r results='asis', warning=FALSE, message=FALSE}

#| echo: false
# Create a list to store tables
micro_prov_table_list <- list()

# Loop through each province and generate a separate gt table
for (Province in unique(microbiology_province$province)) {
  province_data <- microbiology_province %>%
    filter(province == Province) %>%
    mutate(percentage = round((freq / sum(freq)) * 100,2)) %>%  # Calculate percentage
    select(organism_clean, freq, percentage) %>%  # Keep only relevant columns
    arrange(desc(freq))  # Sort by frequency
  gt_table <- province_data %>%
    gt() %>%
    tab_header(
      title = md(paste("Culture micro organism count -", Province, "Province"))
    ) %>%
    cols_label(
      organism_clean = "Micro organism",
      freq = "Frequency",
      percentage = "Percentage (%)"
    ) %>%
    fmt_number(columns = vars(percentage), decimals = 1)  # Format percentage
  # Store table in the list
  micro_prov_table_list[[Province]] <- gt_table
  # Print each table
  print(gt_table)
}

```

### Ward types

```{r}

#| include: false
# Count micro organism occurrences
microbiology_ward <- cleaned_microbiology %>%
  select(-c(province, Hospital_Grade, study_id)) %>% 
  group_by(ward, organism_clean) %>%
  summarise(freq = n(), .groups = "drop") %>%
  filter(!str_starts(organism_clean, "Other")) %>%
  arrange(desc(freq))

# View top micro organisms
head(microbiology_ward)
```


```{r results='asis', warning=FALSE, message=FALSE}

#| echo: false
# Create a list to store tables
micro_ward_table_list <- list()

# Loop through each province and generate a separate gt table
for (Ward in unique(microbiology_ward$ward)) {
  ward_data <- microbiology_ward %>%
    filter(ward == Ward) %>%
    mutate(percentage = round((freq / sum(freq)) * 100,2)) %>%  # Calculate percentage
    select(organism_clean, freq, percentage) %>%  # Keep only relevant columns
    arrange(desc(freq))  # Sort by frequency
  gt_table <- ward_data %>%
    gt() %>%
    tab_header(
      title = md(paste("Culture micro organism count -", Ward))
    ) %>%
    cols_label(
      organism_clean = "Micro organism",
      freq = "Frequency",
      percentage = "Percentage (%)"
    ) %>%
    fmt_number(columns = vars(percentage), decimals = 1)  # Format percentage
  # Store table in the list
  micro_ward_table_list[[Ward]] <- gt_table
  # Print each table
  print(gt_table)
}

```



# NHSL Summary

### prevalence 

For whole NHSL patient group
```{r}
#| echo: false
nhsl_data <- data_labels %>%   
  select(study_id, antibiotic_therapy, number_of_antibiotics) %>% 
  filter(str_detect(study_id, "PP_P_")) %>%  # selecting data from NHSL hospital
  summarise(    
    total = sum(antibiotic_therapy %in% c("Yes", "No"), na.rm = TRUE),  
    positives = sum(antibiotic_therapy == "Yes", na.rm = TRUE),    
    prevalence = paste0(round((mean(antibiotic_therapy == "Yes", na.rm = TRUE)) * 100, 2), "%"),  # Formatted prevalence    
    sum_antibiotics = sum(number_of_antibiotics, na.rm = TRUE), 
    min_number_of_antibiotics = min(number_of_antibiotics[number_of_antibiotics > 0], na.rm = TRUE),   # Min, excluding zeros
    max_number_of_antibiotics = max(number_of_antibiotics[number_of_antibiotics > 0], na.rm = TRUE),  # Max, excluding zeros
    median_number_of_antibiotics = median(number_of_antibiotics[number_of_antibiotics > 0], na.rm = TRUE),  # Median, excluding zeros
    iqr_25th = quantile(number_of_antibiotics[number_of_antibiotics > 0], 0.25, na.rm = TRUE),  # 25th percentile
    iqr_75th = quantile(number_of_antibiotics[number_of_antibiotics > 0], 0.75, na.rm = TRUE)  # 75th percentile
    
  ) %>% 
  arrange(prevalence) # Sort using numeric prevalence before formatting it

ft_nhsl <-  nhsl_data %>% 
  gt() %>%  # Convert to gt table format
  tab_header(
    title = "Antibiotic Use Summary of NHSL"
  ) %>%
  fmt_number(
    columns = c(min_number_of_antibiotics, max_number_of_antibiotics, median_number_of_antibiotics, iqr_25th, iqr_75th),
    decimals = 0  # Format counts
  ) %>%
  cols_label(
    total = "Total Patients",
    positives = "Positive Cases",
    prevalence = "Prevalence",
    sum_antibiotics = "Total number of Antibiotic",
    min_number_of_antibiotics = "Min number of Antibiotics",
    max_number_of_antibiotics = "Max number of Antibiotics",
    median_number_of_antibiotics = "Median number of Antibiotics",
    iqr_25th = "25th Percentile",
    iqr_75th = "75th Percentile"
  ) %>%
  tab_options(
    table.font.size = px(14),
    heading.align = "center"
  ) 
ft_nhsl

```


For ward types

```{r}
#| echo: false
nhsl_ward_data <- data_labels %>%   
  select(study_id,ward, antibiotic_therapy, number_of_antibiotics) %>% 
  filter(str_detect(study_id, "PP_P_")) %>% 
  group_by(ward) %>% 
  summarise(    
    total = sum(antibiotic_therapy %in% c("Yes", "No"), na.rm = TRUE),  
    positives = sum(antibiotic_therapy == "Yes", na.rm = TRUE),    
    prevalence = paste0(round((mean(antibiotic_therapy == "Yes", na.rm = TRUE)) * 100, 2), "%"),  # Formatted prevalence    
    sum_antibiotics = sum(number_of_antibiotics, na.rm = TRUE), 
    min_number_of_antibiotics = min(number_of_antibiotics[number_of_antibiotics > 0], na.rm = TRUE),   # Min, excluding zeros
    max_number_of_antibiotics = max(number_of_antibiotics[number_of_antibiotics > 0], na.rm = TRUE),  # Max, excluding zeros
    median_number_of_antibiotics = median(number_of_antibiotics[number_of_antibiotics > 0], na.rm = TRUE),  # Median, excluding zeros
    iqr_25th = quantile(number_of_antibiotics[number_of_antibiotics > 0], 0.25, na.rm = TRUE),  # 25th percentile
    iqr_75th = quantile(number_of_antibiotics[number_of_antibiotics > 0], 0.75, na.rm = TRUE)  # 75th percentile
    
  ) %>% 
  arrange(prevalence) # Sort using numeric prevalence before formatting it

ft_nhsl_ward <-  nhsl_ward_data %>% 
  gt() %>%  # Convert to gt table format
  tab_header(
    title = "Antibiotic Use Summary of NHSL"
  ) %>%
  fmt_number(
    columns = c(min_number_of_antibiotics, max_number_of_antibiotics, median_number_of_antibiotics, iqr_25th, iqr_75th),
    decimals = 0  # Format counts
  ) %>%
  cols_label(
    total = "Total Patients",
    ward = "Ward",
    positives = "Positive Cases",
    prevalence = "Prevalence",
    sum_antibiotics = "Total number of Antibiotic",
    min_number_of_antibiotics = "Min number of Antibiotics",
    max_number_of_antibiotics = "Max number of Antibiotics",
    median_number_of_antibiotics = "Median number of Antibiotics",
    iqr_25th = "25th Percentile",
    iqr_75th = "75th Percentile"
  ) %>%
  tab_options(
    table.font.size = px(14),
    heading.align = "center"
  ) 
ft_nhsl_ward

```




### common antibiotics 

For whole NHSL patient group
```{r}
# Count medication occurrences
nhsl_medication <- cleaned_medications %>%
  select(-c(province, ward,Hospital_Grade)) %>% 
  filter(str_detect(study_id,"PP_P_")) %>% 
  group_by(medications_clean) %>%
  summarise(freq = n(), .groups = "drop") %>%
  filter(!str_starts(medications_clean, "Other")) %>%
  arrange(desc(freq))

# View top medications
head(nhsl_medication)
```

```{r results='asis', warning=FALSE, message=FALSE}

#| echo: false


# Loop through each hospital and generate a separate gt table
nhsl_hospital_data <- nhsl_medication %>%
  mutate(percentage = round((freq / sum(freq)) * 100, 2)) %>% 
  select(medications_clean, freq, percentage) %>%  
  arrange(desc(freq))

nhsl_gt_table <- nhsl_hospital_data %>%
  gt() %>%
  tab_header(
    title = md(paste("Medication Frequency - NHSL"))
  ) %>%
  cols_label(
    medications_clean = "Medication",
    freq = "Frequency",
    percentage = "Percentage (%)"
  ) %>%
  fmt_number(columns = everything(), decimals = 1)  # Format all numeric columns

nhsl_gt_table

```



ward
```{r}
# Count medication occurrences
nhsl_medication_ward <- cleaned_medications %>%
  select(-c(province, Hospital_Grade)) %>% 
  filter(str_detect(study_id,"PP_P_")) %>% 
  group_by(ward, medications_clean) %>%
  summarise(freq = n(), .groups = "drop") %>%
  filter(!str_starts(medications_clean, "Other")) %>%
  arrange(desc(freq))

# View top medications
head(nhsl_medication_ward)
```

```{r results='asis', warning=FALSE, message=FALSE}

#| include: false
# Create a list to store tables
nhsl_ward_table_list <- list()

# Loop through each ward and generate a separate gt table
for (Ward in unique(nhsl_medication_ward$ward)) {
  ward_data <- nhsl_medication_ward %>%
    filter(ward == Ward) %>%
    mutate(percentage = round((freq / sum(freq)) * 100,2)) %>%  # Calculate percentage
    select(medications_clean, freq, percentage) %>%  # Keep only relevant columns
    arrange(desc(freq))  # Sort by frequency
  nhsl_ward_gt_table <- ward_data %>%
    gt() %>%
    tab_header(
      title = md(paste("Medication Frequency -", Ward))
    ) %>%
    cols_label(
      medications_clean = "Medication",
      freq = "Frequency",
      percentage = "Percentage (%)"
    ) %>%
    fmt_number(columns = vars(percentage), decimals = 1)  # Format percentage
  # Store table in the list
  nhsl_ward_table_list[[Ward]] <- nhsl_ward_gt_table
  # Print each table
  print(nhsl_ward_gt_table)
}

```


### antibiotic group

For whole NHSL patient group
```{r}
# Categorize Antibiotics
cleaned_nhsl_medications_category = cleaned_medications_N %>%
  select(-c(Hospital_Grade, ward, province)) %>% 
  filter(str_detect(study_id,"PP_P_")) %>% 
  group_by(Medication_Category) %>%
  summarise(freq = n(), .groups = "drop") %>%
  filter(!str_starts(Medication_Category, "Other")) %>%
  arrange(desc(freq))

# View top medications
head(cleaned_nhsl_medications_category)
```
```{r results='asis', warning=FALSE, message=FALSE}

#| echo: false

nhsl_hospital_data_cat <- cleaned_nhsl_medications_category %>%
  mutate(percentage = round((freq / sum(freq)) * 100, 2)) %>%  # Calculate percentage
  select(Medication_Category, freq, percentage) %>%  # Keep only relevant columns
  arrange(desc(freq))  # Sort by frequency
nhsl_med_cat_gt_table <- nhsl_hospital_data_cat %>%
  gt() %>%
  tab_header(
    title = md(paste("Medication (category) Frequency - NHSL"))
  ) %>%
  cols_label(
    Medication_Category = "Medication Category",
    freq = "Frequency",
    percentage = "Percentage (%)"
  ) %>%
  fmt_number(columns = percentage, decimals = 1)  # Format all numeric columns

nhsl_med_cat_gt_table

```

ward
```{r}
# Categorize Antibiotics
cleaned_nhsl_ward_medications_category = cleaned_medications_N %>%
  select(-c(Hospital_Grade, province)) %>% 
  filter(str_detect(study_id,"PP_P_")) %>% 
  group_by(ward,Medication_Category) %>%
  summarise(freq = n(), .groups = "drop") %>%
  filter(!str_starts(Medication_Category, "Other")) %>%
  arrange(desc(freq))

# View top medications
head(cleaned_nhsl_ward_medications_category)
```

```{r results='asis', warning=FALSE, message=FALSE}

#| echo: false


# Create a list to store tables
nhsl_medication_ward_table_list <- list()

# Loop through each hospital and generate a separate gt table
for (wards in unique(cleaned_nhsl_ward_medications_category$ward)) {
  ward_data <- cleaned_nhsl_ward_medications_category %>%
    filter(ward == wards) %>%
    mutate(percentage = round((freq / sum(freq)) * 100, 2)) %>%  # Calculate percentage
    select(Medication_Category, freq, percentage) %>%  # Keep only relevant columns
    arrange(desc(freq))  # Sort by frequency
  gt_table <- ward_data %>%
    gt() %>%
    tab_header(
      title = md(paste("Medication (category) Frequency -", wards))
    ) %>%
    cols_label(
      Medication_Category = "Medication Category",
      freq = "Frequency",
      percentage = "Percentage (%)"
    ) %>%
    fmt_number(columns = everything(), decimals = 1)  # Format all numeric columns
  
  # Store table in the list
  nhsl_medication_ward_table_list[[wards]] <- gt_table
  # Convert to LaTeX for proper PDF rendering and print
  #cat(as.character(as_latex(gt_table)), sep = "\n")
  print(gt_table)
}
```




### indications 

For whole NHSL patient group
```{r}
# Count medication occurrences
nhsl_indication <- cleaned_med_indication %>%
  select(-c(Hospital_Grade, ward, province)) %>% 
  filter(str_detect(study_id,"PP_P_")) %>%
  group_by(indication_clean) %>%
  summarise(freq = n(), .groups = "drop") %>%
  filter(!str_starts(indication_clean, "other"), !indication_clean == "complete") %>%
  arrange(desc(freq))

# View top medications
head(nhsl_indication)
```

```{r results='asis', warning=FALSE, message=FALSE}

#| include: false
#| 
# Compute the total sum of freq for this specific hospital category
total_freq <- sum(nhsl_indication$freq, na.rm = TRUE)

# Create summary table
nhsl_indication_gt_table <- nhsl_indication %>%
  select(indication_clean, freq) %>%
  group_by(indication_clean) %>%
  summarise(
    count = sum(freq, na.rm = TRUE),  # Sum frequency for each indication_clean
    percentage = round((count / total_freq) * 100, 2)
  ) %>%
  arrange(desc(count)) %>% 
  gt() %>%
  tab_header(
    title = paste("Medication Indication Summary NHSL"),
    subtitle = paste("Total Medication Indications:", total_freq)
  ) %>%
  fmt_number(
    columns = percentage,
    decimals = 2
  ) %>%
  cols_label(
    indication_clean = "Medication Indication",
    count = "Total Count",
    percentage = "Percentage (%)"
  ) %>%
  tab_options(
    table.font.size = "medium",
    heading.title.font.size = "large",
    heading.subtitle.font.size = "medium"
  )


# Display the table
nhsl_indication_gt_table

```


ward
```{r}
# Count medication occurrences
nhsl_indication_ward <- cleaned_med_indication %>%
  select(-c(Hospital_Grade, province)) %>% 
  filter(str_detect(study_id,"PP_P_")) %>%
  group_by(ward, indication_clean) %>%
  summarise(freq = n(), .groups = "drop") %>%
  filter(!str_starts(indication_clean, "other"), !indication_clean == "complete") %>%
  arrange(desc(freq))

# View top medications
head(nhsl_indication_ward)
```

```{r results='asis', warning=FALSE, message=FALSE}

#| include: false
# Get unique province categories
nhsl_indication_ward_table_list <- unique(nhsl_indication_ward$ward)  

# Initialize a list to store formatted gt tables
nhsl_ward_tables_gt <- list()

# Loop through each province and generate a separate table
for (Ward in nhsl_indication_ward_table_list) {
  nhsl_ward_table_data <- nhsl_indication_ward %>% 
    filter(ward == Ward)
  
  # Compute the total sum of freq for this specific hospital category
  total_freq <- sum(nhsl_ward_table_data$freq, na.rm = TRUE)
  
  # Create summary table
  t <- nhsl_ward_table_data %>%
    select(indication_clean, freq) %>%
    group_by(indication_clean) %>%
    summarise(
      count = sum(freq, na.rm = TRUE),  # Sum frequency for each indication_clean
      percentage = round((count / total_freq) * 100, 2)  # Percentage within the hospital category
    ) %>%
    arrange(desc(count))  # Sort by frequency
  
  # Convert to gt table
  gt_table <- t %>%
    gt() %>%
    tab_header(
      title = paste("Medication Indication Summary -", Ward),
      subtitle = paste("Total Medication Indications:", total_freq)
    ) %>%
    fmt_number(
      columns = percentage,
      decimals = 2
    ) %>%
    cols_label(
      indication_clean = "Medication Indication",
      count = "Total Count",
      percentage = "Percentage (%)"
    ) %>%
    tab_options(
      table.font.size = "medium",
      heading.title.font.size = "large",
      heading.subtitle.font.size = "medium"
    )
  
  # Store the gt table in the list
  
  nhsl_ward_tables_gt[[Ward]] <- gt_table
  
}

# Display all tables

nhsl_ward_tables_gt
```




### microbiologic data 

For whole NHSL patient group
```{r}

#| echo: false
culture_nhsl_data <- data_labels %>%   
  select(study_id,positive_culture, relevant_culture) %>% 
  filter(str_detect(study_id,"PP_P_")) %>%  
  summarise(  
    Total_Culture = sum(relevant_culture %in% c("Yes", "No"), na.rm = TRUE),
    Culture_count = sum(relevant_culture == "Yes",na.rm = TRUE),
    Culture_Percentage = paste0(round((mean(relevant_culture == "Yes", na.rm = TRUE)) * 100, 2), "%"),
    total = sum(positive_culture %in% c("Yes", "No", "Culture pending"), na.rm = TRUE),  
    positives = sum(positive_culture == "Yes", na.rm = TRUE),    
    percentage = paste0(round((mean(positive_culture == "Yes", na.rm = TRUE)) * 100, 2), "%")  # Formatted prevalence
  )

culture_nhsl_table <- culture_nhsl_data %>% 
  gt() %>%  # Now properly connected to the summarised data
  tab_header(
    title = "Culture micro organism data - NHSL"
  ) %>%
  cols_label(
    Total_Culture = "Total Cases",
    Culture_count = "Total Ordered Culture",
    Culture_Percentage = "Ordered Culture Percentage",
    total = "Culture Complete",
    positives = "Positive Culture",
    percentage = "Percentage"
  ) %>%
  tab_options(
    table.font.size = px(14),
    heading.align = "center"
  )
culture_nhsl_table
```




```{r}
# Count micro organism occurrences
microbiology_nhsl_freq <- cleaned_microbiology %>%
  select(-c(ward,province, Hospital_Grade)) %>% 
  filter(str_detect(study_id,"PP_P_")) %>%  
  group_by(organism_clean) %>%
  summarise(freq = n(), .groups = "drop") %>%
  filter(!str_starts(organism_clean, "Other")) %>%
  arrange(desc(freq))

# View top micro organisms
head(microbiology_nhsl_freq)
```


```{r results='asis', warning=FALSE, message=FALSE}

#| echo: false

nhsl_culture_data <- microbiology_nhsl_freq %>%
  mutate(percentage = round((freq / sum(freq)) * 100,2)) %>%  # Calculate percentage
  select(organism_clean, freq, percentage) %>%  # Keep only relevant columns
  arrange(desc(freq))  # Sort by frequency
nhsl_culture_gt_table <- nhsl_culture_data %>%
  gt() %>%
  tab_header(
    title = md(paste("Culture micro organism count - NHSL"))
  ) %>%
  cols_label(
    organism_clean = "Micro organism",
    freq = "Frequency",
    percentage = "Percentage (%)"
  ) %>%
  fmt_number(columns = vars(percentage), decimals = 1)  # Format percentage

nhsl_culture_gt_table

```


ward
```{r}

#| echo: false
culture_nhsl_data <- data_labels %>%   
  select(study_id, ward, positive_culture, relevant_culture) %>% 
  filter(str_detect(study_id,"PP_P_")) %>%  
  group_by(ward) %>% 
  summarise(  
    Total_Culture = sum(relevant_culture %in% c("Yes", "No"), na.rm = TRUE),
    Culture_count = sum(relevant_culture == "Yes",na.rm = TRUE),
    Culture_Percentage = paste0(round((mean(relevant_culture == "Yes", na.rm = TRUE)) * 100, 2), "%"),
    total = sum(positive_culture %in% c("Yes", "No", "Culture pending"), na.rm = TRUE),  
    positives = sum(positive_culture == "Yes", na.rm = TRUE),    
    percentage = paste0(round((mean(positive_culture == "Yes", na.rm = TRUE)) * 100, 2), "%")  # Formatted prevalence
  )

culture_nhsl_table <- culture_nhsl_data %>% 
  gt() %>%  # Now properly connected to the summarised data
  tab_header(
    title = "Culture micro organism data - NHSL"
  ) %>%
  cols_label(
    ward = "Ward",
    Total_Culture = "Total Cases",
    Culture_count = "Total Ordered Culture",
    Culture_Percentage = "Ordered Culture Percentage",
    total = "Culture Complete",
    positives = "Positive Culture",
    percentage = "Percentage"
  ) %>%
  tab_options(
    table.font.size = px(14),
    heading.align = "center"
  )
culture_nhsl_table

```


```{r}
# Count micro organism occurrences
microbiology_nhsl_ward_freq <- cleaned_microbiology %>%
  select(-c(province, Hospital_Grade)) %>% 
  filter(str_detect(study_id,"PP_P_")) %>%  
  group_by(ward, organism_clean) %>%
  summarise(freq = n(), .groups = "drop") %>%
  filter(!str_starts(organism_clean, "Other")) %>%
  arrange(desc(freq))

# View top micro organisms
head(microbiology_nhsl_ward_freq)
```

```{r results='asis', warning=FALSE, message=FALSE}

#| echo: false
# Create a list to store tables
nhsl_culture_ward_table_list <- list()

# Loop through each province and generate a separate gt table
for (Ward in unique(microbiology_nhsl_ward_freq$ward)) {
  nhsl_ward_data <- microbiology_nhsl_ward_freq %>%
    filter(ward == Ward) %>%
    mutate(percentage = round((freq / sum(freq)) * 100,2)) %>%  # Calculate percentage
    select(organism_clean, freq, percentage) %>%  # Keep only relevant columns
    arrange(desc(freq))  # Sort by frequency
  nhsl_culture_gt_table <- nhsl_ward_data %>%
    gt() %>%
    tab_header(
      title = md(paste("Culture micro organism count -", Ward))
    ) %>%
    cols_label(
      organism_clean = "Micro organism",
      freq = "Frequency",
      percentage = "Percentage (%)"
    ) %>%
    fmt_number(columns = vars(percentage), decimals = 1)  # Format percentage
  # Store table in the list
  nhsl_culture_ward_table_list[[Ward]] <- nhsl_culture_gt_table
  # Print each table
  print(nhsl_culture_gt_table)
}

```