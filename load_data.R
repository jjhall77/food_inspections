#pluto <- read_csv('MN.csv')


setwd("~/DS") # set to your data file

#you have to install the below packages:
# install.packages('tidyverse')
# install.packages('readr')
# install.packages('lubridate')
# install.packages('lubridate')
# install.packages('DiagrammeR')
#install.packages('stringR')

library(DiagrammeR)
library(tidyverse) #may have to install these packages
library(readr)
library(lubridate)
library(stringr)

df <- read_csv('DOHMH_New_York_City_Restaurant_Inspection_Results.csv')



#munging
names(df) <-make.names(names(df))
names(df) <- tolower(names(df))

##add dates
df$inspection.date <- mdy(df$inspection.date)
df$record.date <- mdy(df$record.date)
df$grade.date <- mdy(df$grade.date)


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
print(as/numeric(solo))


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



##---------------------------------------GANT-----------------------------------

mermaid("
        gantt
        
        title Food Inspection Project
        
        section The work
        Data Exploration              :done,          first_1,    2017-01-01, 5d
        Generate Data Questions       :done,        first_2,    2017-01-01, 6d
        Meet with DOH data people reg. assumptions  : first_3, after first_2,1d
        Learn Data Collection Process :    first_4, after first_3   ,2d
        Clean data : active,first_5, after first_2, 4d
        Transform Data: first_6, after first_5, 3d
        Model Data: first_7, after extra_4, 3d
        
        
        
        section Extra
        Explore additional datasets     :     extra_1,   2017-01-01,7d
        Geocode, if necessary      :active,    extra_2,   after extra_1, 7d
        Clean/Transform Add. Data : extra_3,   after extra_2, 5d
        Merge Data for modeling            :active,          extra_4,   after extra_3, 4d
        
        section The presentation 
        Research Similar Approaches : present1, 2017-01-03,7d
        Create Background Document: present2, after present1, 7d
        Obtain & refine graphics from data exploration : present3,after first_1, 3d
        Obtain & refine graphics from modelling : present4, after first_7, 3d
        Create powerpoint : present5, after present4,5d
        Edit spelling and style :  present6, after present5, 1d
        Dry Run : present7, after present6, 2d
        
        
        
        
        
        
        
        ")



##Address matching
#LOAD Manhattan PLUTO & truncate df for ease of reading.
# % unmatched funtion
unmatched <- function(df){
  unmatch <- sum(is.na(df$block))
  total <- nrow(df)
  x <- unmatch/total*100
  return(x)
}






pad <- read_csv('bobaadr.txt')
pad <- pad %>% select(boro, block, lot, bin, lhnd,hhnd,stname)


pad$lhnd <-  gsub(pattern = "-", replacement = "", pad$lhnd)
pad$hhnd <-  gsub(pattern = "-", replacement = "", pad$hhnd)
#HORACE HARDING Servic elimination
pad$stname <-  gsub(pattern = " SR N", replacement = "", pad$stname)
pad$stname <-  gsub(pattern = " SR S", replacement = "", pad$stname)

#get high & low versions
pad2 <-pad

pad$Address <- paste0(pad$lhnd," ", pad$stname)
pad2$Address <- paste0(pad2$hhnd," ", pad$stname)

pad <- bind_rows(pad,pad2)
pad <- unique(pad)


pad3 <- pad
pad3$lhnd <- as.numeric(pad3$lhnd)
pad3$hhnd <- as.numeric(pad3$hhnd)
pad3$diff <- pad3$hhnd-pad3$lhnd
pad3 <- pad3 %>%
  filter(!is.na(lhnd)) %>%
  filter(diff>0) %>%
  select(-diff)

datalist = list()
for(i in 1:nrow(pad3)){
  expand_seq <- seq(from=pad3$lhnd[i]+1, to=pad3$hhnd[i]-1)
  df2 <- pad3[i,]
  df2 <-df2[rep(seq_len(nrow(df2)), length(expand_seq)), ]
  df2$hhnd <- expand_seq
  print(i)
  datalist[[i]] <- df2 # add it to your list
  
}

big_data = bind_rows(datalist)
big_data$lhnd <- as.character(big_data$lhnd)
big_data$hhnd <- as.character(big_data$hhnd)
big_data$Address <- paste0(big_data$hhnd," ", big_data$stname)

pad <- bind_rows(pad,big_data)






#create address field
#1st we have to create a function to create 1 space from multiple spaces
trim <- function(x) return(gsub("^ *|(?<= ) | *$", "", x, perl=T))

#get rid of address dashes
df$building <- gsub(pattern = "-", replacement = "", df$building)
df$Address <- paste0(df$building," ",df$street) 
df$Address <-trim(df$Address)
pad$Address <- trim(pad$Address)


pad$boro <- ifelse(pad$boro==1,"MANHATTAN", pad$boro)
pad$boro <- ifelse(pad$boro==2,"BRONX", pad$boro)
pad$boro <- ifelse(pad$boro==3,"BROOKLYN", pad$boro)
pad$boro <- ifelse(pad$boro==4,"QUEENS", pad$boro)
pad$boro <- ifelse(pad$boro==5,"STATEN ISLAND", pad$boro)




#create manahattan subset of inspections data
mn_df <- df 

mn_df <- mn_df[!duplicated(mn_df$camis),] #just for address matching purposes, remove dupe restaurants
mn_df$building <- gsub(pattern = "-", replacement = "", mn_df$building)



####Transform ave and st to addresses
mn_df$Address <- toupper(mn_df$Address)
mn_df$Address <- paste0(mn_df$Address," ")

mn_df$Address <- gsub(pattern = " ST ", replacement = " STREET", mn_df$Address)
mn_df$Address <- gsub(pattern = " AVE ", replacement = " AVENUE", mn_df$Address)
mn_df$Address <- gsub(pattern = " W ", replacement = " WEST ", mn_df$Address)
mn_df$Address <- gsub(pattern = " E ", replacement = " EAST ", mn_df$Address)


mn_df$Address <- gsub(pattern = " 1ST ", replacement = " 1 ", mn_df$Address)
mn_df$Address <- gsub(pattern = " 2ND ", replacement = " 2 ", mn_df$Address)
mn_df$Address <- gsub(pattern = " 3RD ", replacement = " 3 ", mn_df$Address)
mn_df$Address <- gsub(pattern = " 4TH ", replacement = " 4 ", mn_df$Address)
mn_df$Address <- gsub(pattern = " 5TH ", replacement = " 5 ", mn_df$Address)
mn_df$Address <- gsub(pattern = " 6TH ", replacement = " 6 ", mn_df$Address)
mn_df$Address <- gsub(pattern = " 7TH ", replacement = " 7 ", mn_df$Address)
mn_df$Address <- gsub(pattern = " 8TH ", replacement = " 8 ", mn_df$Address)
mn_df$Address <- gsub(pattern = " 9TH ", replacement = " 9 ", mn_df$Address)
mn_df$Address <- gsub(pattern = " 10TH ", replacement = " 10 ", mn_df$Address)
mn_df$Address <- gsub(pattern = " FIRST ", replacement = " 1 ", mn_df$Address)
mn_df$Address <- gsub(pattern = " SECOND ", replacement = " 2 ", mn_df$Address)
mn_df$Address <- gsub(pattern = " THIRD ", replacement = " 3 ", mn_df$Address)
mn_df$Address <- gsub(pattern = " FIFTH ", replacement = " 5 ", mn_df$Address)
mn_df$Address <- gsub(pattern = " SIXTH ", replacement = " 6 ", mn_df$Address)
mn_df$Address <- gsub(pattern = " SEVENTH ", replacement = " 7 ", mn_df$Address)
mn_df$Address <- gsub(pattern = " EIGHTH ", replacement = " 8 ", mn_df$Address)
mn_df$Address <- gsub(pattern = " NINTH ", replacement = " 9 ", mn_df$Address)
mn_df$Address <- gsub(pattern = " TENTH ", replacement = " 10 ", mn_df$Address)
mn_df$Address <- gsub(pattern = " ELEVENTH ", replacement = " 11 ", mn_df$Address)
mn_df$Address <- gsub(pattern = "MACDOUGAL", replacement = "MAC DOUGAL", mn_df$Address)
mn_df$Address <- gsub(pattern = " PLZ", replacement = " PLAZA", mn_df$Address)
mn_df$Address <- gsub(pattern = " MALCOLM X BLVD", replacement = " LENOX AVENUE", mn_df$Address)
mn_df$Address <- gsub(pattern = " BLVD", replacement = " BOULEVARD", mn_df$Address)
mn_df$Address <- gsub(pattern = " PL ", replacement = " PLACE", mn_df$Address)
mn_df$Address <- gsub(pattern = " DR ", replacement = " DRIVE", mn_df$Address)
mn_df$Address <- gsub(pattern = " RD ", replacement = " ROAD", mn_df$Address)
mn_df$Address <- gsub(pattern = " PKWY ", replacement = " PARKWAY", mn_df$Address)
mn_df$Address <- gsub(pattern = " EXPY ", replacement = " EXPRESSWAY", mn_df$Address)
mn_df$Address <- gsub(pattern = " LN ", replacement = " LANE", mn_df$Address)
#6 avenue in Brooklyn Too!!
mn_df$Address<-ifelse(mn_df$boro=="MANHATTAN",gsub(pattern = " 6 AVENUE", 
                                                   replacement = " AVENUE OF THE AMERICAS", mn_df$Address), mn_df$Address)

mn_df$Address <- gsub(pattern = " TPKE", replacement = " TURNPIKE", mn_df$Address)
mn_df$Address <- gsub(pattern = "STREETNICHOLAS", replacement = "ST NICHOLAS", mn_df$Address)
mn_df$Address <- gsub(pattern = " SAINT ", replacement = " ST ", mn_df$Address)
mn_df$Address <- gsub(pattern = "STREETMARKS", replacement = "ST MARKS", mn_df$Address)
mn_df$Address <- gsub(pattern = " FORT ", replacement = " FT ", mn_df$Address)
mn_df$Address <- gsub(pattern = "CROSSBAY", replacement = "CROSS BAY", mn_df$Address)
mn_df$Address <- gsub(pattern = " HWY", replacement = " HIGHWAY", mn_df$Address)
mn_df$Address <- gsub(pattern = "0 LAGUARDIA AIRPORT", replacement = "NA LA GUARDIA AIRPORT", mn_df$Address)
mn_df$Address <- gsub(pattern = "NKA JFK INTERNATIONAL AIRPORT", replacement = "NA JFK AIRPORT", mn_df$Address)
mn_df$Address <- gsub(pattern = "0 JFK INTERNATIONAL AIRPORT", replacement = "NA JFK AIRPORT", mn_df$Address)
mn_df$Address <- gsub(pattern = " AVENUES", replacement = " AVENUE SOUTH", mn_df$Address)
mn_df$Address <- gsub(pattern = " 9TH AVENUE", replacement = " 9 AVENUE", mn_df$Address)
mn_df$Address <- gsub(pattern = " DOUGLAS ", replacement = " DOUGLASS ", mn_df$Address)
mn_df$Address <- gsub(pattern = "GUNHILL", replacement = "GUN HILL", mn_df$Address)



#citifield

mn_df$Address <- gsub(pattern = "126 ROOSEVELT AVENUE", replacement = "12301 ROOSEVELT AVENUE", mn_df$Address)

mn_df$Address <- str_replace(mn_df$Address, "(?<=[0-9])(?:ST|ND|RD|TH)","")




mn_df$Address <- str_trim(mn_df$Address, "both")
mn_df <- mn_df %>%
  left_join(pad, by = c("Address","boro")) 







#print % of unmatched addresses
unmatched(mn_df)



x <- mn_df %>%
  filter(is.na(block)) %>%
  arrange(street)

y <- x %>% group_by(street) %>% summarise(n=n()) %>% arrange(desc(n))

pbmn_pad <- pad %>% filter(boro=="MANHATTAN")
bx_pad <- pad %>% filter(boro=="BRONX")
bk_pad <- pad %>% filter(boro=="BROOKLYN")
qn_pad <- pad %>% filter(boro=="QUEENS")
si_pad <- pad %>% filter(boro=="STATEN ISLAND")


#write.csv(pbmn_pad, 'mn_pad.csv')
#write.csv(bx_pad, 'bx_pad.csv')
#write.csv(bk_pad, 'bk_pad.csv')
#write.csv(qn_pad, 'qn_pad.csv')
#write.csv(si_pad, 'si_pad.csv')




#34.7, #32.7, #23.69, #29.5, #29.28, 28.139, 7.05, 6.83, 4.87, 4.78, 4.4, 4.19, 4.04, 3.87, 3.79, 3.68
#3.59, 3.27, 3.18, 3.16, 3.13


df2 <-df2[rep(seq_len(nrow(df2)), length(expand_seq)), ]
df2$hhnd <- expand_seq
df2

