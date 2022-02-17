# Samantha Deokinanan
# This code cleans and format new student data for upload onto CC
# Last updated 8/27/2019

library(tidyverse)

raw_data = read_csv("CPDI001 Report.csv")

temp = subset(raw_data, select = -c(middle_init,
                                    email_preferred,
                                    strm,
                                    acad_plan1,
                                    unt_taken_prgrss,
                                    XfrCollege))

temp = temp[!duplicated(temp$emplid),] %>%
  rename(degree_level = acad_career,
         gpa = cum_gpa,
         credits_completed = tot_cumulative,
         class_level = acad_level_bot_descr)
                
temp = temp %>%
  mutate(class_level = str_remove_all(class_level, "Lower |Upper ")) %>%
  mutate(class_level = gsub("Prof.*\\d", "PHD", class_level)) %>%
  mutate(class_level = gsub("Graduate", "Grad Student", class_level)) %>%
  mutate(degree_level = recode(degree_level, 
                               'DOCT' = 'Doctorate', 'GRAD' = 'Masters', 'UGRD' = 'Bachelors')) 

# Cleaning Major
remove_list <- paste(c("Non Degree", "CUNY", "Cert"), collapse = '|') 
temp = temp %>% filter(!grepl(remove_list, acad_plan_descr1)) 

major_list = read_csv("major new student.csv")

data = merge(temp, major_list, by = "acad_plan_descr1")

missing = data %>% anti_join(temp, by = "emplid") %>% select(c('acad_plan_descr1'))

if (nrow(missing) != 0) {
  print("Not all majors are recognized. Missing include:")
  print(unique(missing))
}

data['applicant_type'] = rep('Current Student', nrow(data))
data['All Students'] = rep('Yes', nrow(data))
data['02/10/2022 Upload'] = rep('Yes', nrow(data))  # <--------- Change Flag date of upload

write.csv(data[-c(1)],'new_student_upload_Spring_2022.csv', row.names = FALSE) # <---- Change file name for semester and year
