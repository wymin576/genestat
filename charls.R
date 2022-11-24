rm(list=ls());library(haven);library(tidyverse);library(labelled)
setwd('D:\\charls')
# dir()
# 
# # find common variable
# name_file <- intersect(dir('CHARLS2011'),
#                        intersect(tolower(dir('CHARLS2013')),
#                                       intersect(tolower(dir('CHARLS2015r')),
#                                                 tolower(dir('CHARLS2018r')))))
# name_file
# 
# myfunc1 <- function(x){
# path <- paste0('D:\\charls\\CHARLS20',x);setwd(path)
#  merge(Reduce(function(x,y) merge(x,y,by=c("ID","householdID", "communityID")),
#                 list(read_dta(name_file[1]), read_dta(name_file[2]),
#                      read_dta(name_file[3]), read_dta(name_file[4]),
#                      read_dta(name_file[5]), read_dta(name_file[7]) )),
#               read_dta(name_file[6]),by=c('householdID', "communityID"))
# }
# myfunc2 <- function(x){
#   path <- paste0('D:\\charls\\CHARLS20',x);setwd(path)
#   Reduce(function(x,y) merge(x,y,by=c("ID","householdID", "communityID")),
#                list(read_dta(name_file[1]), read_dta(name_file[2]),
#                     read_dta(name_file[3]), read_dta(name_file[4]),
#                     read_dta(name_file[5]), read_dta(name_file[6]),
#                     read_dta(name_file[7]) ))
# }
# # name_file[6]) has no ID in survey11 and survey18
# df11 <- myfunc1(11);dim(df11);df13 <- myfunc2(13);dim(df13)
# df15 <- myfunc2('15r');dim(df15);df18 <- myfunc1('18r');dim(df18)
# 
# setwd('D:\\charls')
# library(readr)
# write_csv(df11,file='df11.csv');write_csv(df13,file='df13.csv')
# write_csv(df15,file='df15.csv');write_csv(df18,file='df18.csv')

df11 <- read_csv('df11.csv');df13 <- read_csv('df13.csv');
df15 <- read_csv('df15.csv');df18 <- read_csv('df18.csv')

# define a variable called 'social' to indicate if a respondent
# was involved in at least one social activity.
# options(max.print = .Machine$integer.max)
# capture.output(var_label(df11), file = "my_list.csv") 


dat11 <- df11 %>% mutate(householdID = paste0(householdID,'0'),
                         ID = paste0(householdID, substr(ID,9,10)),
                         wave = 2011,age = 2011-ba002_1,
                         SRH = ifelse(da001 %in% c(1:3)|da002 %in% c(1:2),1,0)) %>% 
  select(ID,wave,age,rgender,ba002_1,bc001,bd001,be001,SRH,
         starts_with('da056'),starts_with('da057')) 
table(duplicated(dat11))

dat13 <- df13 %>% rename(rgender = ba000_w2_3) %>% 
          mutate(wave = 2013,age = 2013-ba002_1,
          SRH = ifelse(da001 %in% c(1:3)| da002 %in% c(1:2),1,0)) %>%
           select(ID,wave,age,rgender,ba002_1,bc001,bd001,be001,SRH,
                starts_with('da056'),starts_with('da057')) 
table(duplicated(dat13))


dat15 <- df15 %>% rename(rgender = ba000_w2_3) %>% 
  mutate(wave=2015,age=2015-ba002_1,SRH = ifelse(da001 %in% c(1:3)|da002 %in% c(1:2),1,0)) %>%
  select(ID,wave,age,rgender,ba002_1,bc001_w3_2,bd001_w2_4,be001,SRH,
                         da056s1:da056s12,starts_with('da057'))   %>% 
                          rename(bc001=bc001_w3_2,bd001=bd001_w2_4)

dat18 <- df18 %>% rename(rgender = ba000_w2_3) %>% mutate(wave=2018,age=2018-ba002_1,
                         SRH = ifelse(da002 %in% c(1:2),1,0)) %>% 
  select(ID,wave,age,rgender,ba002_1,bc001_w3_2,bd001_w2_4,be001,SRH,
                         da056_s1:da056_s12,starts_with('da057'))  %>% 
                         rename(bc001=bc001_w3_2,bd001=bd001_w2_4) 


names(dat18)[grep("da056_",names(dat18),fixed=TRUE)] <- paste0('da056s',1:12)


dat <- rbind(dat11,dat13,dat15,dat18) %>% arrange(ID,wave) %>% 
   mutate(across(da056s1:da056s12,~ifelse(is.na(.x),0,1)),
          across("da057_1_":"da057_11_", ~replace_na(.,0)),
          hukou = as.factor(bc001),
          social = as.factor(1-da056s12),
          marital= as.factor(ifelse(be001 <= 3,1,0)),
          freq = rowSums(across(starts_with('da057')))) %>% filter(age >= 60) 

dat$edu[dat$bd001==1] <- 0;dat$edu[dat$bd001==2] <- 3;
dat$edu[dat$bd001==3] <- 5;dat$edu[dat$bd001==4] <- 5.5;
dat$edu[dat$bd001==5] <- 8.5;dat$edu[dat$bd001==6] <- 11.5;
dat$edu[dat$bd001==7] <- 12;dat$edu[dat$bd001==8] <- 14.5;
dat$edu[dat$bd001==9] <- 15.5;dat$edu[dat$bd001==9] <- 18.5


df <- dat %>% select(ID,SRH,wave,rgender,hukou,social,marital,age,freq,edu)

mice::md.pattern(df)


summary(glm(as.factor(SRH) ~ social,family=binomial(),df))
summary(glm(as.factor(SRH) ~ as.factor(wave)+as.factor(rgender)+hukou+social+
              marital+age+edu,family=binomial(),df))
summary(glm(as.factor(SRH) ~ as.factor(wave)+as.factor(rgender)+hukou+freq+
              marital+age+edu,family=binomial(),df))

library(lme4)

fit <- glmer(SRH ~ as.factor(rgender)+hukou+freq+
              marital+age+edu+(1|ID) ,family=binomial("logit"),df)
summary(fit)

fit <- glmer(SRH~ as.factor(rgender)+hukou+social+
               marital+age+edu+(1|ID),family=binomial("logit"),df)
summary(fit)
 
