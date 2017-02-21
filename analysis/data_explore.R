# Data exploration
source('munge/0-load_libraries.R')
source('munge/01- load inspection data.R')



##Facts
ggplot(df, aes(x= boro)) + 
  geom_bar()+
  theme_minimal()+
  ggtitle('Boro distribution of inspections')

##Most inspected Businesses
individual <- df %>%
  group_by(dba, camis) %>%
  summarise(n=n()) %>%
  arrange(desc(n))


##Number of distinct business
solo <- df %>%
  summarise(n_distinct(camis))
print(as.numeric(solo))


##violation code dictionary
dictionary <- df %>%
  select(violation.code,violation.description, critical.flag)
dictionary <- dictionary[!duplicated(dictionary$violation.description),]


##restaurant grade distribution
grades <- df %>%
  filter(!is.na(grade)) 

ggplot(grades, aes(x= grade)) + 
  geom_bar()+
  theme_minimal()+
  ggtitle('Grade distribution')

ggplot(grades, aes(x=score)) +
  geom_density() +
  theme_minimal() + 
  ggtitle('score distribution')+
  xlim(0,35)

ggplot(grades, aes(x= grade, y = score)) + geom_boxplot()+
  theme_minimal() + ggtitle('distribution of score by grade')

grade_df <- grades %>%
  group_by(score, grade) %>%
  summarise(n=n()) %>%
  arrange(desc(score), desc(n))

grade_df <- grade_df[!duplicated(grade_df$score),]
color_table = tribble(
  ~grade, ~color,
  "A", "red",
  "B", "blue",
  "C","yellow",
  "Not Yet Graded","purple")


grade_df <- grade_df %>%
  left_join(color_table)

grade_df2 <- grades %>%
  filter(cuisine.description=="Spanish") %>%
  group_by(camis, inspection.date, score, grade) %>%
  summarise(n = n())

ggplot(grade_df2, aes(x=score, y = n, fill = grade)) +
  geom_bar(stat = "identity")+ 
  theme_minimal()+
  xlim(0,50)+
  ggtitle("Distribution of scores by grade")


##Inspection types by commonness.


x <- df %>%
  group_by(inspection.type) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) #creates order


#based on above, we may wish to only look at initial inspections and/or re-inspections


viol_per_inspection <- df %>%
  group_by(inspection.date, camis, dba, score) %>%
  summarise(n=n()) %>%
  arrange(camis, desc(inspection.date),desc(n))


###
zip_df <- df %>% 
  group_by(zipcode, camis, inspection.date,score) %>%
  summarise(n=n()) %>%
  group_by(zipcode) %>%
  summarise(avg_grade = mean(score, na.rm=T),
            number_inspections = n()) %>%
  arrange(desc(avg_grade))

ggplot(zip_df, aes(x = avg_grade)) + 
  geom_histogram(binwidth = 1, fill="blue", color = "black")+
  theme_minimal()+
  ggtitle("Distribution of Average Scores by Zipcode")
