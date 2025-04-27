###importing necessary libraries###
install.packages("tidyverse")
library(tidyverse)
library(dplyr)

###Loading dataset###
df<-read.csv("immigration-detention.csv")



#######################     CLEANING   ######################################



###removing the header in the first few rows.###
df<- read.csv("immigration-detention.csv",skip = 6) # we have around 6 lines of header which we are skipping
head(df)
colnames(df) # shows what columns are there in the dataset
dim(df) #give the overall dimension of the dataframe. we have 137*8 dim i.e 137 rows and 8 columns

###removing special character such as @,\,&,$ etc###
view(df)
#I face error while compiling the above code because the csv file contains non-UTF-8(or non standard) characters
#So im converting file into UTF standard
df$Name<- iconv(df$Name,from = "",to = "UTF-8", sub = "")
df$Name <- gsub("[^A-Za-z ]", "", df$Name)#this ensures only A to Z and a to z character is in the dataframe
df$Name
has_special <- grepl("[^A-Za-z]",df$Name)
df$Name<-trimws(df$Name) #removing leading and trailing white spaces
df$Name


###Handling Blanks###
#Blank in Name of detention center will be replaced by "Unknown"
sum(df$Name == "")#after checking there are two blank spaces in the Name column
sum(is.na(df$Name)) # there are no any N/A field in the Name column
#lets replace blank section in Name section by "Unknown" keyword
df <- df %>% 
  mutate(Name = if_else(Name == "","Unknown",Name))
#checking whether the blank field is replaced by Unknown
sum(df$Name == "")
sum(df$Name =="N/A")
checking<- df%>% 
  filter(Name == "Unknown") %>% 
  select(Name,City)
checking #okay we carefully handled the blank space in the name column

#Now lets check if City section has any blank space or not

df %>% 
  filter(City == "") %>% 
  select(Name,City,State)
df$City<-trimws(df$City)#first eliminating extra spaces if any
sum(df$City =="") 
sum(df$City =="N/A")

#so we have one section where the field is blank
#If we had many blank fields, it would take a lot of time, so I would definitely have applied the 'Unknown' label to the entire column. But for now, since there is only one, I can handle it manually
#I m manually filling the blank section by researching from the google by using the detention center name and its state code.
df %>% 
  filter(City=="") %>% 
  select(Name,City)
#Geauga County jail's city location is unknown 
#From google i get to know the location of the Jail is Chardon link:- https://ohiocountyjails.org/ohio/geauga-county-jail/
#Now lets add the city of the respective jail
df <- df %>%
  mutate(City = if_else(Name == "GEAUGA COUNTY JAIL" & City == "", "CHARDON", City))

df %>% 
  filter(Name=="GEAUGA COUNTY JAIL") %>% 
  select(Name,City)
# we successfully handled it
#Now, lets clean the State section 
#First lets check how many blanks or NA section are there 
df$State<-trimws(df$State)#eliminating extra spaces if any
view(df)
sum(df$State=="")
sum(df$State=="NA")
df %>%
  filter(State == "") %>%
  select(Name, City, State)

#we get to know the two detention center's have missing state code
#Since there is only two blanks for the state, I can handle it manually
#For this we are using the City and Name of detetion center and searching it in google which states it is located at.
#WE need state code for  "ATLANTA" and "ENCINAL"
#For Atlanta, State Code = GA link:-https://prisonprofessors.com/institution/usp-atlanta-federal-prison/
#For Encinal, State Code = TX link:- https://www.cfiaus.com/ice-detention-facilities/texas/la-salle-county-regional-detention-center/

df <- df %>%
  mutate(State = if_else(City == "ATLANTA", "GA", State))
df <- df %>%
  mutate(State = if_else(City == "ENCINAL", "TX", State))
df %>%
  filter(State == "") %>%
  select(Name, City, State)
view(df)


#Now lets check if there any blank values in Level A, Level B, Level C, Level D
colnames(df)
df %>% 
  summarise(across(Level.A:Level.D,function(x) sum(is.na(x))))

#we see that there are no any NA values for all 4 levels

###Handling the misinformed dates###
class(df$Last.Inspection.End.Date)
#the last inspection end data is in character format 
my_date<- as.Date("2025-04-26")
class(my_date) #since our date is in numeric format which must be converted into date data type
# Since the dates are in an inappropriate format (days format), they must be converted into the yyyy-mm-dd format.
# After some research, I found that Excel usually takes the origin date as 1899-12-30.
# As a trial, I converted one value (45673) into the yyyy-mm-dd format in Excel, and it converted to 2025-01-16.
# Eventually, it was confirmed that Excel uses 1899-12-30 as the origin date.
# All the other dates were then converted in the same way.

df <- df %>%
  mutate(
    Last.Inspection.End.Date = as.Date(as.numeric(Last.Inspection.End.Date), origin = "1899-12-30")
  )
df$Last.Inspection.End.Date
view(df)
sum(is.na(df$Last.Inspection.End.Date)) #Total 18 NA field 
#as Nevada had appropriate date format but during conversion the date gets erroded and labeled as NA which is quite wrong
#so lets convert that date into initial format which was correct
df <- df %>%
  mutate(
    Last.Inspection.End.Date = if_else(
      Name == "NEVADA SOUTHERN DETENTION CENTER",
      as.Date("2024-09-19"),#this was the original date in the original csv file
      Last.Inspection.End.Date
    )
  )
view(df)
sum(is.na(df$Last.Inspection.End.Date)) #Total 17 NA field

# as we have total 17 not available data for Last Inspection End Date.
# we do not have any way to deal with let leave it NA




################################    ANALYZE    ##########################################



# GOAL : Sum the “Level A”, “Level B”, “Level C”, and “Level D” columns to make a new “Total
# Population” column. Then subset that column so that you only have the top ten largest
# detention facilities in the dataset .

#lets first add all the levels value and put the ans in new column called "TOtal Population"

df<-df %>% 
  mutate(Total.Population = Level.A + Level.B + Level.C + Level.D)

#as Total population is in decimal we can round of to nearest integer but there is no any requirement mentioned in the task-assessment so im keeping as it is for now
# df <- df %>%
#   mutate(Total.Population = round(Total.Population)) #if rounding off is needed

view(df)

#subsetting the above Total Population column so that we only have the top ten largest detention facilities in the dataset

top_10_facilities <- df %>%
  arrange(desc(Total.Population)) %>%
  slice_head(n = 10)
view(top_10_facilities)

#we can also find with different features like which state has how many detention center
Each_state_detention_center <- df %>% 
  group_by(State) %>% 
  summarise(Detention_center_count=n()) %>% 
  arrange(desc(Detention_center_count)) %>% 
  slice_head(n = 10)
Each_state_detention_center # we can see texas or TX has the highest number of detention center
view(Each_state_detention_center)

#We can also find which state has the highest number of total population
State_total_population <- df %>% 
  group_by(State) %>% 
  summarize(Total= sum(Total.Population,na.rm = TRUE)) %>% 
  arrange(desc(Total)) %>% 
  slice_head(n = 10)
State_total_population
view(State_total_population)
#we can still see texas has the highest number of people in detention center is highest in the Texas



#################       VISUALIZATION      ##############################

# Creating a bar plot showing the top 10 largest center
 
ggplot(data = top_10_facilities,mapping =  aes(x = reorder(Name, Total.Population), y = Total.Population)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = round(Total.Population)), hjust = -0.2, size = 3) + # <-- ADDING TEXT HERE
  coord_flip() + #it flips the axes as when names were not clear.
  labs(title = "Top 10 Detention Facilities by Total Population",
       x = "Detention Facility",
       y = "Total Population") +
  theme_minimal()
ggsave(
  filename = "Top_10_Largest_Detention_Center.png",
  plot = last_plot(),      # Or you can specify the exact plot if needed
  dpi = 600,               # Higher resolution
  width = 12,              # Wider
  height = 8,              # Taller
  units = "in"             # Inches
)


ggplot(data = top_10_facilities, mapping =  aes(x = reorder(Name, Total.Population), y = Total.Population)) +
  geom_bar(stat = "identity", fill = "black") +
  geom_text(aes(label = round(Total.Population)), hjust = -0.5, size = 3) +
  coord_flip() +
  labs(title = "Top 10 Detention Facilities by Total Population",
       x = "Detention Facility",
       y = "Total Population") +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white"),#while saving image i wasn't getting clear plotting so fixing it white solve the issue
    plot.background = element_rect(fill = "white")
  )
ggsave(
  filename = "Top 10 Detention Facilities by Total Population.png",
  plot = last_plot(),
  dpi = 600,
  width = 12,
  height = 8,
  units = "in"
)
#creating a bar graph for showing the to 10 states with highest number of detention center
ggplot(data = Each_state_detention_center,mapping =  aes(x = reorder(State, Detention_center_count), y = Detention_center_count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = Detention_center_count), hjust = -0.2, size = 3) + #adding text on each bar
  coord_flip() + #it flips the axes as when names were not clear.
  labs(title = "Top 10 States With Highest Number Of Detention center",
       x = "State",
       y = "Number Of Detention Center") +
  theme_minimal()+
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white"),#while saving image i wasn't getting clear plotting so fixing it white solve the issue
    plot.background = element_rect(fill = "white")
  )
ggsave(
  filename = "Top 10 States With Highest Number Of Detention center.png",
  plot = last_plot(),
  dpi = 600,
  width = 12,
  height = 8,
  units = "in"
)

#Creating a bar graph for showing the States with the Highest number of population in detention center
ggplot(data = State_total_population,mapping =  aes(x = reorder(State, Total), y = Total)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = round(Total)), hjust = -0.2, size = 3) + #adding text on each bar
  coord_flip() + #it flips the axes as when names were not clear.
  labs(title = "Top 10 States With Highest Number Of Population in Detention center",
       x = "State",
       y = "Total Population In Detention Center") +
  theme_minimal()+
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white"),#while saving image i wasn't getting clear plotting so fixing it white solve the issue
    plot.background = element_rect(fill = "white")
  )
ggsave(
  filename = "Top 10 States With Highest Number Of Population in Detention center.png",
  plot = last_plot(),
  dpi = 600,
  width = 12,
  height = 8,
  units = "in"
)
