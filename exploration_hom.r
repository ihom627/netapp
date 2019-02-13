library("RSQLite")
library(DBI)
con = dbConnect(RSQLite::SQLite(), dbname="FoodInspectionsExercise.db")
alltables = dbListTables(con)
print(alltables)
# [1] "inspections" "restaurants"

#load restaurants into dataframe
restaurants_table = dbGetQuery( con,'select * from restaurants' )

#get dimensions
dim(restaurants_table)

#get features
str(restaurants_table)

#get summary
summary(restaurants_table)

#get colnames
colnames(restaurants_table)
[1] "OBJECTID"           "HSISID"             "NAME"              
[4] "CITY"               "STATE"              "POSTALCODE"        
[7] "RESTAURANTOPENDATE" "FACILITYTYPE"     

#head
head(restaurants_table)

#tail
tail(restaurants_table)

#drop off column OBJECTID
restaurant_keeps <- c("HSISID", "NAME", "CITY", "STATE", "POSTALCODE", "RESTAURANTOPENDATE", "FACILITYTYPE")
restaurants_table_final <- restaurants_table[restaurant_keeps]
colnames(restaurants_table_final)
[1] "HSISID"             "NAME"               "CITY"              
[4] "STATE"              "POSTALCODE"         "RESTAURANTOPENDATE"
[7] "FACILITYTYPE" 


#load inspections into dataframe
inspections_table = dbGetQuery( con,'select * from inspections' )

#get colnames
colnames(inspections_table)
[1] "OBJECTID"    "HSISID"      "SCORE"       "DESCRIPTION" "TYPE"       
[6] "INSPECTOR"   "INSPECTDATE"

#drop off column OBJECTID
inspections_keeps <- c("HSISID", "SCORE", "DESCRIPTION", "TYPE", "INSPECTOR", "INSPECTDATE")
inspections_table_final <- inspections_table[inspections_keeps]
colnames(inspections_table_final)
[1] "HSISID"      "SCORE"       "DESCRIPTION" "TYPE"        "INSPECTOR"  
[6] "INSPECTDATE"


#inner join tables on key HSISID
joined_table <- merge(restaurants_table_final, inspections_table_final, by="HSISID")

#dim joined_table
dim(joined_table)
#[1] 24733    12

#get colnames
colnames(joined_table)
[1] "HSISID"             "NAME"               "CITY"              
[4] "STATE"              "POSTALCODE"         "RESTAURANTOPENDATE"
[7] "FACILITYTYPE"       "SCORE"              "DESCRIPTION"       
[10] "TYPE"               "INSPECTOR"          "INSPECTDATE"      


#get unique values of a column
unique(joined_table$FACILITYTYPE)
 [1] "Restaurant"                        "Food Stand"                       
 [3] "Mobile Food Units"                 "Pushcarts"                        
 [5] "Private School Lunchrooms"         "Elderly Nutrition Sites (catered)"
 [7] "Public School Lunchrooms"          "Limited Food Service"             
 [9] "Institutional Food Service"        "Meat Market"   


#filter only restaurant type in dataframe
restaurant_only <- subset(joined_table, FACILITYTYPE == "Restaurant")

#check restaurant only dataframe
dim(restaurant_only)
#[1] 16013    12
unique(restaurant_only$FACILITYTYPE)
#[1] "Restaurant"

#transform SCORE to numeric
restaurant_only_transformed <- transform(restaurant_only, SCORE_NUMERIC = as.numeric(SCORE))

#Question 2a

#get mean 
summary(restaurant_only_transformed$SCORE_NUMERIC)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#  71.50   94.50   96.00   95.73   97.50  100.00 

#filter only food stand type in dataframe
food_stand_only <- subset(joined_table, FACILITYTYPE == "Food Stand")

#check food stand only dataframe
dim(food_stand_only)
#[1] 4416    12
unique(food_stand_only$FACILITYTYPE)
#[1] "Food Stand"

#transform SCORE to numeric
food_stand_only_transformed <- transform(food_stand_only, SCORE_NUMERIC = as.numeric(SCORE))

#get mean 
summary(food_stand_only_transformed$SCORE_NUMERIC)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
#  80.00   95.50   97.50   96.87   98.50  100.00 


#Question 2b
unique(joined_table$INSPECTOR)
# [1] "Angela Myers"         "Brittny Thomas"       "James Smith"         
# [4] "Christy Klaus"        "Maria Powell"         "Jason Dunn"          
# [7] "Lucy Schrum"          "Karla Crowder"        "Joshua Volkan"       
#[10] "Dimitri Parker"       "Naterra McQueen"      "Christopher Walker"  
#[13] "Andrea Anover"        "Ginger Johnson"       "Laura McNeill"       
#[16] "Shakera Robbins"      "Michael Robinson"     "Daryl Beasley"       
#[19] "Johanna Hill"         "Chris Askew"          "Caroline Suggs"      
#[22] "David Adcock"         "Jennifer Edwards"     "Meghan Scott"        
#[25] "Pamela Scott"         "Melissa Harrison"     "Tim Bass"            
#[28] "Jessica Andrews"      "Anne-Kathrin Bartoli" "James Salter"        
#[31] "Ashley Whittington"   "Lisa McCoy"           "Jordan Jernigan"     
#[34] "Marion Wearing"       "Melodee Johnson"      "Thomas Jumalon"      
#[37] "Lauren Plis"          "Angela Jacobs"        "Kaitlyn Yow"         
#[40] "John Wulffert"        "Deborah Peterson"     "Frances Breedlove"   
#[43] "Glenn Johnson"  

#check the SCORE is the same mean for each INSPECTOR 
#perform one-way ANOVA to compare whether the means of each INSPECTOR is by random 

#remap the INSPECTOR levels into categorical values
joined_table$INSPECTOR = factor(joined_table$INSPECTOR, labels = c("Angela Myers", "Brittny Thomas", "James Smith", "Christy Klaus", "Maria Powell", "Jason Dunn", "Lucy Schrum" , "Karla Crowder" , "Joshua Volkan",  "Dimitri Parker" , "Naterra McQueen" ,     "Christopher Walker" , "Andrea Anover" , "Ginger Johnson" ,    "Laura McNeill" , "Shakera Robbins" , "Michael Robinson" , "Daryl Beasley" , "Johanna Hill" , "Chris Askew" , "Caroline Suggs" , "David Adcock" , "Jennifer Edwards" , "Meghan Scott" , "Pamela Scott" , "Melissa Harrison" ,  "Tim Bass" , "Jessica Andrews" ,  "Anne-Kathrin Bartoli",  "James Salter" , "Ashley Whittington" ,  "Lisa McCoy" , "Jordan Jernigan" , "Marion Wearing" , "Melodee Johnson" , "Thomas Jumalon" , "Lauren Plis" , "Angela Jacobs" , "Kaitlyn Yow" , "John Wulffert" , "Deborah Peterson" , "Frances Breedlove" , "Glenn Johnson" ))

#now plot
require(ggplot2)

#takes too long
ggplot(joined_table, aes(x = INSPECTOR, y = SCORE)) +
  geom_boxplot(fill = "grey80", colour = "blue") +
  scale_x_discrete() + xlab("inspectors") +
  ylab("score")

#build linear model
joined_table_model = lm(SCORE ~ INSPECTOR, data = joined_table)

summary(joined_table_model)
Call:
lm(formula = SCORE ~ INSPECTOR, data = joined_table)

Residuals:
    Min      1Q  Median      3Q     Max 
-97.186  -1.186   0.435   1.761   6.325 

Coefficients:
                              Estimate Std. Error  t value Pr(>|t|)    
(Intercept)                   94.68244    0.07685 1232.000  < 2e-16 ***
INSPECTORBrittny Thomas        1.25130    0.20187    6.199 5.79e-10 ***
INSPECTORJames Smith           0.47679    0.10772    4.426 9.63e-06 ***
INSPECTORChristy Klaus         3.35456    0.12076   27.779  < 2e-16 ***
INSPECTORMaria Powell          1.89821    0.27546    6.891 5.67e-12 ***
INSPECTORJason Dunn            2.21520    0.16935   13.081  < 2e-16 ***
INSPECTORLucy Schrum           2.46920    0.11410   21.641  < 2e-16 ***
INSPECTORKarla Crowder         1.74967    0.13007   13.452  < 2e-16 ***
INSPECTORJoshua Volkan         1.66828    0.12464   13.384  < 2e-16 ***
INSPECTORDimitri Parker        1.20470    0.16421    7.336 2.26e-13 ***
INSPECTORNaterra McQueen       2.38217    0.11947   19.939  < 2e-16 ***
INSPECTORChristopher Walker    2.55630    0.10647   24.010  < 2e-16 ***
INSPECTORAndrea Anover         1.56756    1.47479    1.063  0.28783    
INSPECTORGinger Johnson        1.56414    0.12927   12.100  < 2e-16 ***
INSPECTORLaura McNeill         2.81756    1.47479    1.910  0.05608 .  
INSPECTORShakera Robbins       1.16737    0.15183    7.688 1.54e-14 ***
INSPECTORMichael Robinson      3.81756    2.94657    1.296  0.19513    
INSPECTORDaryl Beasley         2.58199    0.21822   11.832  < 2e-16 ***
INSPECTORJohanna Hill          2.00068    0.13105   15.266  < 2e-16 ***
INSPECTORChris Askew           2.33970    0.15285   15.307  < 2e-16 ***
INSPECTORCaroline Suggs        1.15312    0.12820    8.995  < 2e-16 ***
INSPECTORDavid Adcock          2.80913    0.14331   19.602  < 2e-16 ***
INSPECTORJennifer Edwards      1.96647    0.13055   15.063  < 2e-16 ***
INSPECTORMeghan Scott          1.31756    2.08425    0.632  0.52729    
INSPECTORPamela Scott          1.99543    0.17381   11.481  < 2e-16 ***
INSPECTORMelissa Harrison      1.73273    0.19295    8.980  < 2e-16 ***
INSPECTORTim Bass              3.71756    0.76442    4.863 1.16e-06 ***
INSPECTORJessica Andrews       1.13725    0.11147   10.202  < 2e-16 ***
INSPECTORAnne-Kathrin Bartoli  1.26407    0.12179   10.379  < 2e-16 ***
INSPECTORJames Salter          2.07947    0.64735    3.212  0.00132 ** 
INSPECTORAshley Whittington    1.70961    0.23496    7.276 3.53e-13 ***
INSPECTORLisa McCoy           -0.12417    0.10825   -1.147  0.25138    
INSPECTORJordan Jernigan       1.61992    0.17748    9.127  < 2e-16 ***
INSPECTORMarion Wearing        1.35277    0.21596    6.264 3.82e-10 ***
INSPECTORMelodee Johnson       2.74136    0.17060   16.069  < 2e-16 ***
INSPECTORThomas Jumalon        0.29233    0.21379    1.367  0.17152    
INSPECTORLauren Plis           1.33608    0.57206    2.336  0.01952 *  
INSPECTORAngela Jacobs         2.90997    0.18585   15.657  < 2e-16 ***
INSPECTORKaitlyn Yow           1.82732    0.10933   16.714  < 2e-16 ***
INSPECTORJohn Wulffert         2.50324    0.12820   19.526  < 2e-16 ***
INSPECTORDeborah Peterson      2.83902    0.18585   15.276  < 2e-16 ***
INSPECTORFrances Breedlove    -2.50725    0.19383  -12.935  < 2e-16 ***
INSPECTORGlenn Johnson         1.38574    0.32327    4.287 1.82e-05 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 2.946 on 24690 degrees of freedom
Multiple R-squared:  0.1086,	Adjusted R-squared:  0.107 
F-statistic: 71.59 on 42 and 24690 DF,  p-value: < 2.2e-16


#build anova model
#H0 is all means are equal
#below shows the p-value is smaller than the significance level, indicating 
#we should reject H0, so the means are significantly different
anova(joined_table_model)
Analysis of Variance Table

Response: SCORE
             Df Sum Sq Mean Sq F value    Pr(>F)    
INSPECTOR    42  26089  621.18  71.594 < 2.2e-16 ***
Residuals 24690 214220    8.68                      
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


#Question 2c
#only time to do a spot check 

#need to coerce INSPECTDATE to a date, failed!
install.packages("lubridate")
library(lubridate)
joined_table$INSPECTDATE2 <- mdy(joined_table$INSPECTDATE)


#use as.date()
joined_table2 <- transform(joined_table, INSPECTDATE2 = as.Date(INSPECTDATE))

#there are 3K restaurant names
unique(joined_table2$NAME)

#count occurences
sum(joined_table2$NAME == "Angus Barn")
#[1] 8

#select rows
angus_barn_only <- subset(joined_table2, NAME == "Angus Barn")

#order based on date, failed
angus_barn_only[order(angus_barn_only$INSPECTDATE2)]

#install dplyr
install.packages("dplyr")
library(dplyr)
arrange(angus_barn_only, INSPECTDATE2)
  FACILITYTYPE SCORE 
1   Restaurant  94.5
2   Restaurant  97.5
3   Restaurant    91
4   Restaurant  94.5
5   Restaurant  91.5
6   Restaurant    96
7   Restaurant    95
8   Restaurant  97.5
        TYPE    INSPECTOR INSPECTDATE INSPECTDATE2
1 Inspection Johanna Hill  2013-06-25   2013-06-25
2 Inspection  James Smith  2014-04-17   2014-04-17
3 Inspection  James Smith  2014-12-30   2014-12-30
4 Inspection  James Smith  2015-09-15   2015-09-15
5 Inspection  James Smith  2016-03-17   2016-03-17
6 Inspection   Jason Dunn  2017-05-11   2017-05-11
7 Inspection   Jason Dunn  2017-08-07   2017-08-07
8 Inspection   Jason Dunn  2018-01-10   2018-01-10


#count occurences
sum(joined_table2$NAME == "JUICE VIBES")
#[1] 2 

#select rows
JUICE_VIBES_only <- subset(joined_table2, NAME == "JUICE VIBES")

#install dplyr
install.packages("dplyr")
library(dplyr)
arrange(JUICE_VIBES_only, INSPECTDATE2)
      FACILITYTYPE SCORE
15841   Restaurant    99
15842   Restaurant    99
            TYPE        INSPECTOR INSPECTDATE INSPECTDATE2
15841 Inspection  Jessica Andrews  2017-08-07   2017-08-07
15842 Inspection Deborah Peterson  2018-03-20   2018-03-20


#count occurences
sum(joined_table2$NAME == "EL RINCONCITO")
#[1] 8 

#select rows
EL_RINCONCITO_only <- subset(joined_table2, NAME == "EL RINCONCITO")

#install dplyr
install.packages("dplyr")
library(dplyr)
arrange(EL_RINCONCITO_only, INSPECTDATE2)
  FACILITYTYPE SCORE
1   Food Stand    98
2   Food Stand  99.5
3   Food Stand    98
4   Food Stand    93
5   Food Stand    90
6   Food Stand    95
7   Food Stand    93
8   Food Stand    91
        TYPE       INSPECTOR INSPECTDATE INSPECTDATE2
1 Inspection   Christy Klaus  2015-05-29   2015-05-29
2 Inspection   Christy Klaus  2015-10-06   2015-10-06
3 Inspection   Christy Klaus  2016-03-04   2016-03-04
4 Inspection     James Smith  2016-07-11   2016-07-11
5 Inspection     James Smith  2017-01-11   2017-01-11
6 Inspection     James Smith  2017-04-26   2017-04-26
7 Inspection Jessica Andrews  2017-08-16   2017-08-16
8 Inspection Jessica Andrews  2017-11-27   2017-11-27


sum(joined_table2$NAME == "CASABLANCA MARKET")
#[1] 9 

#select rows
CASABLANCA_MARKET_only <- subset(joined_table2, NAME == "CASABLANCA MARKET")

#install dplyr                                                                                                   install.packages("dplyr")                                                                                        library(dplyr)
arrange(CASABLANCA_MARKET_only, INSPECTDATE2)
  FACILITYTYPE SCORE
1  Meat Market  94.5
2  Meat Market    95
3  Meat Market  93.5
4  Meat Market  91.5
5  Meat Market  95.5
6  Meat Market  93.5
7  Meat Market    96
8  Meat Market  97.5
9  Meat Market  96.5
        TYPE         INSPECTOR INSPECTDATE INSPECTDATE2
1 Inspection Frances Breedlove  2013-07-05   2013-07-05
2 Inspection        Lisa McCoy  2014-04-14   2014-04-14
3 Inspection        Lisa McCoy  2014-08-27   2014-08-27
4 Inspection        Lisa McCoy  2015-04-10   2015-04-10
5 Inspection      Johanna Hill  2015-11-06   2015-11-06
6 Inspection   Jessica Andrews  2016-08-30   2016-08-30
7 Inspection   Jessica Andrews  2016-12-21   2016-12-21
8 Inspection   Jessica Andrews  2017-06-23   2017-06-23
9 Inspection   Jessica Andrews  2017-11-03   2017-11-03


#put into a graphic
show number of decrease/improve SCORE with the last two visits

#Question 3, predict SCORE less than 93 based on features

#transform SCORE to numeric
joined_table2 <- transform(joined_table2, SCORE_NUMERIC = as.numeric(SCORE))

#transform POSTALCODE to factor 
joined_table2 <- transform(joined_table2, POSTALCODE_FACTOR = as.factor(POSTALCODE))

#transform FACILITYTYPE to factor
joined_table2 <- transform(joined_table2, FACILITYTYPE_FACTOR = as.factor(FACILITYTYPE))

#transform RESTAURANTOPENDATE to date 
joined_table2 <- transform(joined_table2, RESTAURANTOPENDATE2 = as.Date(RESTAURANTOPENDATE))

#transform NAME to factor
joined_table2 <- transform(joined_table2, NAME_FACTOR = as.factor(NAME))

#transform TYPE to factor
joined_table2 <- transform(joined_table2, TYPE_FACTOR = as.factor(TYPE))

#change SCORE to factors "below" for <93 and "equal_above" for >= 93
joined_table2$SCORE_FACTOR <- factor(ifelse(joined_table2$SCORE_NUMERIC>=93, "equal_above", "below"))


#split into training/test data
set.seed(999) # Set Seed so that same sample can be reproduced in future also
# Now Selecting 75% of data as sample from total 'n' rows of the data  
sample <- sample.int(n = nrow(joined_table2), size = floor(.75*nrow(joined_table2)), replace = F)
train2 <- joined_table2[sample, ]
test2  <- joined_table2[-sample, ]
dim(train2)
#[1] 18549    13
dim(test2)
#[1] 6184   13
dim(joined_table2)
#[1] 24733    13

#check distribution
table(joined_table2$SCORE_FACTOR)/nrow(joined_table2)  
#     below equal_above 
# 0.09521692  0.90478308 
table(train2$SCORE_FACTOR)/nrow(train2)
#     below equal_above 
# 0.09385951  0.90614049 
table(test2$SCORE_FACTOR)/nrow(test2)
#     below equal_above 
# 0.09928849  0.90071151 


#create random forest classification model
#randomForest fails due to categorical values over 53!
library(randomForest)  
rf = randomForest(SCORE_FACTOR ~ POSTALCODE_FACTOR,  ntree = 100, data = train2)

#try gbm (error with dimensions)
install.packages("gbm")
library(gbm)
fit <- gbm(SCORE_FACTOR ~ POSTALCODE_FACTOR, data=train2, distribution="multinomial")

#try C50 (FAILED due to dimensions)
library(C50)
fit <- C5.0(SCORE_FACTOR ~ ., data=joined_table2, trials=1)
predictions <- predict(fit, joined_table2)
table(predictions, joined_table2$SCORE_FACTOR)


#CART
library(rpart)
foo_df <- select(joined_table2, POSTALCODE_FACTOR, FACILITYTYPE_FACTOR, TYPE_FACTOR, NAME_FACTOR, SCORE_FACTOR)
fit <- rpart(SCORE_FACTOR ~ POSTALCODE_FACTOR + FACILITYTYPE_FACTOR + TYPE_FACTOR + NAME_FACTOR , data=foo_df )
predictions <- predict(fit, foo_df[,1:4], type="class")
table(predictions, foo_df$SCORE_FACTOR)

predictions   below equal_above
  below         554         240
  equal_above  1801       22138


#### NEXT STEP to make test and train dataset ######


sample <- sample.int(n = nrow(foo_df), size = floor(.75*nrow(foo_df)), replace = F)
foo_df_train2 <- foo_df[sample, ]
foo_df_test2  <- foo_df[-sample, ]
dim(foo_df_train2)
#[1] 18549   5 
dim(foo_df_test2)
#[1] 6184   5 
dim(foo_df)
[1] 24733     5


#check distribution
table(foo_df$SCORE_FACTOR)/nrow(foo_df)
#     below equal_above
# 0.09521692  0.90478308
table(foo_df_train2$SCORE_FACTOR)/nrow(foo_df_train2)
#     below equal_above
# 0.09553076  0.90446924 
table(foo_df_test2$SCORE_FACTOR)/nrow(foo_df_test2)
#     below equal_above
# 0.09427555  0.90572445 


fit <- rpart(SCORE_FACTOR ~ POSTALCODE_FACTOR + FACILITYTYPE_FACTOR + TYPE_FACTOR + NAME_FACTOR , data=foo_df_train2 )
predictions <- predict(fit, foo_df_test2[,1:4], type="class")
table(predictions, foo_df_test2$SCORE_FACTOR)

predictions   below equal_above
  below          90         103
  equal_above   493        5498


#merge predictions back into dataframe
foo_final <- cbind(foo_df_test2, predictions)

summry(foo_final)
POSTALCODE_FACTOR                 FACILITYTYPE_FACTOR        TYPE_FACTOR  
 27511  : 276      Restaurant                :3990     Inspection   :6093  
 27587  : 271      Food Stand                :1128     Re-Inspection:  91  
 27609  : 248      Public School Lunchrooms  : 537                         
 27603  : 218      Meat Market               : 217                         
 27502  : 213      Institutional Food Service: 105                         
 27610  : 207      Mobile Food Units         :  99                         
 (Other):4751      (Other)                   : 108                         
            NAME_FACTOR        SCORE_FACTOR       predictions  
 Subway           :  30   below      : 583   below      : 193  
 Taco Bell        :  16   equal_above:5601   equal_above:5991  
 LOS TRES MAGUEYES:  14                                        
 Boston Market    :  13                                        
 Wang`s Kitchen   :  13                                        
 Applebee`s       :  11                                        
 (Other)          :6087   


#export test results to csv
write.csv(foo_final, file = "question3_test_output.csv")

#save memory image
save.image()   #output in .RData


#to load use
load(".RData")

###################### OLD ######################


#create multi-variate regression model
summary(lm(SCORE_NUMERIC ~ POSTALCODE_FACTOR, data = joined_table2))

#add FACILITYTYPE_FACTOR
summary(lm(SCORE_NUMERIC ~ POSTALCODE_FACTOR + FACILITYTYPE_FACTOR, data = joined_table2))

#add TYPE_FACTOR
summary(lm(SCORE_NUMERIC ~ POSTALCODE_FACTOR + FACILITYTYPE_FACTOR + TYPE_FACTOR, data = joined_table2))

#add NAME_FACTOR
summary(lm(SCORE_NUMERIC ~ POSTALCODE_FACTOR + FACILITYTYPE_FACTOR + TYPE_FACTOR + NAME_FACTOR, data = joined_table2))

#add RESTAURANTOPENDATE2 
summary(lm(SCORE_NUMERIC ~ POSTALCODE_FACTOR + FACILITYTYPE_FACTOR + TYPE_FACTOR + NAME_FACTOR + RESTAURANTOPENDATE2, data = joined_table2))

