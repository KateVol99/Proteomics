peaks_data<- read.csv("C:/Users/Екатерина/Downloads/peaks_data.csv", header=FALSE)
View(peaks_data)
library(limma)
library(dplyr)
library(ggplot2)
library(ggfortify)

clear_peaks_data <- na.omit(peaks_data)

text_columns <- clear_peaks_data[, 1:3]
row <- clear_peaks_data[1, ]
row_without_text <- row[, 1:3]

normalized_data <- numeric_data %>%
  mutate(across(where(is.numeric), ~log(. + 1)))

text_columns_clean <- text_columns[-1, ]


combined_columns<- cbind(text_columns_clean, normalized_data)
combined_data <- rbind(row, combined_columns)

combined_data[, 4:36] <- lapply(combined_data[, 4:36], as.numeric)
class_of_values <- sapply(combined_data, class)
print(class_of_values)

t_test_results <- list()

for (i in 4:(ncol(combined_data) - 1)) {
  if (is.numeric(combined_data[[i]])) {
    for (j in (i+1):ncol(combined_data)) {
      if (is.numeric(combined_data[[j]])) {
        t_test_result <- t.test(combined_data[[i]], combined_data[[j]])
        t_test_results[[paste0(names(combined_data)[i], "_vs_", names(combined_data)[j])]] <- t_test_result
      }
    }
  }
}

for (result_name in names(t_test_results)) {
  print(result_name)
  print(t_test_results[[result_name]])
  cat("\n")
}
