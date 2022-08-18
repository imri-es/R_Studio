# Momunov Imran
#TP056103


#install packages
install.packages("readr")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("gridExtra")
install.packages("reshape2")

library(readr)
library(ggplot2)
library(gridExtra)
library(dplyr)
library(reshape2)

#read file with data
stdata<-read_delim(file = "C:\\Users\\momun\\Documents\\Uni\\Degree_Level_2\\Semester 1\\PDFA\\Placement_Data_Full_Class.csv", 
                   delim = ",", col_names = TRUE)
View(stdata)

#data pre-processing

placedSt <- subset(stdata, status == "Placed")
notplacedSt <- subset(stdata, status == "Not Placed")

#question 1

#analysis 1-1: Find total number of students.
nrow(stdata)


#analysis 1-2: Find relationship between students who did not get placed and those who did.

ggplot(stdata, aes(fill=status, x = ""))+
  geom_bar()+
  geom_text(stat = 'count',
            aes(label =stat(count)),
            position = position_dodge(width = 1),
            vjust = -0.5)+
  coord_polar("y", start=0)+
  scale_fill_manual("Status", values = c("Not Placed" = "#1e7883", "Placed" = "#ee7256"))+
  labs(title = "Number of Placed and Not Placed students", x = "", y = "")+  
  theme_minimal()


  
#Question 2

#Analysis 2-1 Find what are genders

stdata%>%
  count(gender)

#Analysis 2-2 Find relationship between gender and placement

ggplot(stdata, aes(x = status, fill = gender))+
  geom_bar(position = "dodge")+
  geom_text(stat = 'count',
            aes(label =stat(count)),
            position = position_dodge(width = 1),
            vjust = -0.5)+
  scale_fill_manual("Gender", values = c("F" = "#ee2e21", "M" = "#383f75"))+
  labs(title = "Number of Placed and Not Placed students", 
       x = "Status", y = "Number of students")+  
  theme_minimal()

#Analysis 2-3  Find the age range of students who got placed and those who did not.
stdata%>%
  count(age)

#Analysis 2-4  Difference by age between number of students who get placed and those who not

df3<- data.frame(count(stdata, status == "Not Placed", age))
df3
df4 <- data.frame()
df4
totalrows <- nrow(df3)
totalrows = totalrows/2
totalrows

while(totalrows > 0){
  df4 = rbind(df4, df3[totalrows, 3] - df3[totalrows+6, 3])
  totalrows = totalrows - 1
}
colnames(df4) = c("Dif")
df4 = cbind(df4, count(notplacedSt, age))
df4


ggplot(df4, aes(y = Dif, x = age))+
  geom_smooth(method = "loess", color = "#fd475e")+
  labs(title = "Difference by age between number of students who get placed\nand those who did not",
       x = "Age", y = "Difference Number")+
  theme_minimal()

#Question 3 Is family of a student affecting to the placement?

#Analysis 3-1 Find what are parents' occupations is there

df3 <- data.frame(count(placedSt, Fjob))
df4 <- data.frame(count(placedSt, Mjob))
colnames(df4)[2] = "n2"
df3 <- cbind(df3, df4)
df3
  
#Analysis 3-2 Find relationship between parent's occupation and students who has placed
ggplot(df3, aes(x = Fjob, y= n, group =1, colour  = "Father"))+
  geom_smooth(method = "loess")+
  geom_smooth( aes(x = Mjob, y = n2, colour = "Mother"), method = "loess")+
  labs(title = "Types of occupations between parents and numbers of them",
       x = "Jobs", y = "Number of parents")+
  theme_minimal()

#Analysis 3-3	Find relationship between parent's occupation and students who get financial support
m1 <- ggplot(stdata, aes(x = Mjob, fill = famsup))+
  geom_bar(position = "dodge")+
  geom_text(stat = 'count',
            aes(label =stat(count)),
            position = position_dodge(width = 1),
            vjust = -0.5)+
  labs(title = "Number of students who get financial support by Mother's occupations", 
       x = "", y = "Number of students")+
  theme_minimal()

f1 <- ggplot(stdata, aes(x = Fjob, fill = famsup))+
  geom_bar(position = "dodge")+
  geom_text(stat = 'count',
            aes(label =stat(count)),
            position = position_dodge(width = 1),
            vjust = -0.5)+
  labs(title="Number of students who get financial support by Father's occupations", 
       x="", y= "Number of students")+
  theme_minimal()

grid.arrange(m1,f1)

#Analysis 3-4 	Find what are level of educations there
count(stdata, Fedu)
count(stdata, Medu)


#Analysis 3-5 Find relationship between parent's education and number of students who has placed
df3 <- data.frame(count(placedSt, Fedu))
df4 <- data.frame(count(placedSt, Medu))
colnames(df4)[2] = "n2"
df3 <- cbind(df3, df4)
df4 <- data.frame(count(notplacedSt, Fedu))
colnames(df4) <- c("nFedu", "n3")
df3 <- cbind(df3, df4)
df4 <- data.frame(count(notplacedSt, Medu))
colnames(df4) <- c("nMedu", "n4")
df3 <- cbind(df3, df4)
df3

ggplot(df3, aes(x = Fedu, y= n, group =1))+
  geom_smooth(aes(colour = "Placed student, \nFather level of education\n"))+
  geom_smooth( aes(x = Medu, y = n2, colour = "Placed student, \nMother level of education\n"))+
  geom_smooth( aes(x = nFedu, y = n3, colour  = "Not Placed student, \nFather level of education\n"))+
  geom_smooth( aes(x = nMedu, y = n4, colour = "Not Placed student, \nMother level of education\n"))+
  labs(title = "Relationship between level of education and students' placement", 
       x = "Level of Education", y = "Number of Students")+
  theme_minimal()

#Analysis 3-6 Find relationship between parent's education and students who get financial support

m1 <- ggplot(stdata, aes(x = Medu, fill = famsup))+
  geom_bar(position = "dodge")+
  geom_text(stat = 'count',
            aes(label =stat(count)),
            position = position_dodge(width = 1),
            vjust = -0.5)+
  scale_fill_manual("Family support", values = c("no" = "#2a4dc1", "yes" = "#935bca"))+
  labs(title="Number of students who get financial support by Mother's level of education", 
       x="Level of Education\n", y= "Number of students")+
  theme_minimal()

f1 <- ggplot(stdata, aes(x = Fedu, fill = famsup))+
  geom_bar(position = "dodge")+
  geom_text(stat = 'count',
            aes(label =stat(count)),
            position = position_dodge(width = 1),
            vjust = -0.5)+
  scale_fill_manual("Family support", values = c("no" = "#2a4dc1", "yes" = "#935bca"))+
  labs(title="Number of students who get financial support by Father's level of education",
       x="Level of education", y= "Number of students")+
  theme_minimal()

grid.arrange(m1,f1)


#Analysis 3-7 Find relationship between family support and students' placement
ggplot(stdata, aes(x = status, fill= famsup))+
  geom_bar(position = "dodge")+
  labs(title = "Relationship between family support and students' placement",
       x = "Placement", y = "Number of Students")+
  geom_text(stat = 'count',
            aes(label =stat(count)),
            position = position_dodge(width = 1),
            vjust = -0.5)+
  theme_minimal()


#Question 4
#Analysis 4-1 Find relationship between paid courses and salaries of the students who get placed

ggplot(placedSt, aes(x = sl_no, y = salary, fill = paid, colour = paid))+
  geom_smooth(position = position_dodge(width =1))+
  scale_y_continuous(labels = scales::comma)+
  labs(title="Salaries of students who did additional paid courses", x="Students", y= "Salary")+
  theme_minimal()
#Analysis 4-2 	Find number of students did paid courses and did extra activities  
ggplot(placedSt, aes(x = paid, y = activities))+
  geom_count()+
  labs(title="Number of students did paid courses and did extra activities ",
       x="Paid courses", y= "Extra activities")+
  theme_minimal()


#Question 5 	Is students with higher grades at school got higher salaries?

#Analysis 5-1 Relationship between students' grades at school and salaries
df3 <-data.frame(count(placedSt, ssc_p, salary))
m1 <- ggplot(df3, aes(x = ssc_p,y = salary))+
  geom_point(position = position_dodge(width =1))+
  geom_smooth(method = "loess")+
  scale_y_continuous(labels = scales::comma)+
  labs(title="Salary of students based on their secondary school grades ",
       x="Secondary school grade", y= "Salary")+
  theme_minimal()
df4 <-data.frame(count(placedSt, hsc_p, salary))
f1 <- ggplot(df4, aes(x = hsc_p,y = salary))+
  geom_point(position = position_dodge(width =1))+
  geom_smooth(method = "loess")+
  scale_y_continuous(labels = scales::comma)+
  labs(title="Salary of students based on their high school grades ", 
       x="High school grade", y= "Salary")+
  theme_minimal()

grid.arrange(m1, f1)

# Relationship between salary and grades at school 
ggplot(df4, aes(x = hsc_p,y = salary))+
  geom_smooth(data = df3, aes(x = ssc_p, colour = "Secondary school grade"), method = "loess")+
  geom_smooth(aes(colour = "High school grade"),method = "loess")+
  scale_y_continuous(labels = scales::comma)+
  labs(title="Salary of students based on their school grades \n(both high and secondary)", 
       x="School grade", y= "Salary")+
  theme_minimal()

#Question 6 Is final grades affected?

#Analysis 6-1 What is the number of students who got high grades at degree

count(stdata, degree_p >= 70)

ggplot(stdata, aes(x = degree_p))+
  geom_freqpoly(binwidth = 2)+
  theme_minimal()

#Analysis 6-2 what is the percentage of placed students with high grades and not placed with high grades?

percentage <- data.frame(count(placedSt, degree_p >= 70))
((percentage[2,2] * 100) / (percentage[1,2] + percentage[2,2]))
percentage <- data.frame(count(notplacedSt, degree_p >= 70))
((percentage[2,2] * 100) / (percentage[1,2] + percentage[2,2]))
 

#Analysis 6-3 Relationship between grades at university and salaries

ggplot(stdata)+
  geom_smooth(aes(x = degree_p, y = salary, colour = "Degree grades"), 
              method = "loess", position = position_dodge(width =1))+
  geom_smooth(aes(x = mba_p, y = salary, colour = "MBA grades"), 
              method = "loess",position = position_dodge(width =1))+
  scale_y_continuous(labels = scales::comma)+
  labs(title="Relationship between grades at university and salaries ", x="Grades", y= "Salary")+
  theme_minimal()

#Q7
#Analysis 7-1	Find number of students who got internet connection was from urban or rural area

ggplot(stdata, aes(x = internet, fill = address))+
  geom_bar(position = "dodge")+
  geom_text(stat = 'count',
            aes(label =stat(count)),
            position = position_dodge(width = 1),
            vjust = -0.5)+
  scale_fill_manual("Living area", values = c("R" = "#6a47c7", "U" = "#5ee0d3"))+
  labs(title="Address of living of students with internet access and without ",
       x="Internet Acess", y= "Number of students")+
  theme_minimal()

#Analysis 7-2	Find relationship between students who got internet and their grades at university

f1 <- ggplot(stdata, aes(x = internet, y = degree_p, color = internet))+
  geom_boxplot(show.legend = TRUE, varwidth = TRUE)+
  labs(title="Degree grades based on internet access", 
       x="Internet Acess", y= "Degree grades of students")+
  theme_minimal()

m1 <- ggplot(stdata, aes(x = internet, y = mba_p, color = internet))+
  geom_boxplot(show.legend = TRUE, varwidth = TRUE)+
  labs(title="MBA grades based on internet access ", 
       x="Internet Acess", y= "MBA grades of students")+
  theme_minimal()

grid.arrange(f1, m1)

#Q8	Does work experience affected salary in a positive way?
# Analysis 8-1 	Find number of students who experienced a work
df3 <-data.frame(count(stdata, workex))
df3
ggplot(stdata, aes(x = workex, fill = workex))+
  geom_bar(position = "dodge")+
  geom_text(stat = 'count',
            aes(label =stat(count)),
            position = position_dodge(width = 1),
            vjust = -0.5)+
  scale_fill_manual("Work Experience", values = c("No" = "#f47595", "Yes" = "#643ba2"))+
  labs(title="Number of students who experienced a work", x="Work Experience", y= "Number of students")+
  theme_minimal()

#Analysis 8-2 	Find relationship between students who worked and number of students who get placed


ggplot(data = stdata, aes(x = status, fill = workex))+
  geom_bar(data = placedSt, position = "dodge")+
  geom_bar(data = notplacedSt, position = "dodge")+
  geom_text(stat = 'count',
            aes(label =stat(count)),
            position = position_dodge(width = 1),
            vjust = -0.5)+
  scale_fill_manual("workex", values = c("No" = "#f3a332", "Yes" = "#a7cb8e"))+
  labs(title="Relationship between placement and work experience", 
       x="Placement", y= "Number of students")+
  theme_minimal()
  
#Analysis 8-3 	Find relationship between students with experience and placed students' salary
df3 = subset(stdata, workex == "Yes")
df4 <- data.frame(count(df3, workex, salary))
df3 = subset(stdata, workex == "No")
df4 <- cbind(df4, data.frame(count(df3, workex, salary)))
colnames(df4) <- c("workex", "salary", "n", "Nworkex", "Nsalary", "Nn")

ggplot()+
  geom_smooth(data = df4, aes(x = salary, y = n, colour = "With work experience"),
              method = "loess",  position = position_dodge(width =1))+
  geom_smooth(data = df4, aes(x = Nsalary, y = Nn, colour = "Without work experience"),
              method = "loess",  position = position_dodge(width =1))+
  labs(title="Relationship between salary and work experience", x="Salary", y= "Number of students")+
  scale_x_continuous(labels = scales::comma)+
  theme_minimal()


