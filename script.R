
# script final thesis

# by Jennifer
library(tidyverse)
library(readxl)
library(lme4)
library(lmerTest)
library(here)
library(ggplot2)
library(ggpubr)
library(broom)
library(AICcmodavg)


# cargar base de datos
df= read_csv('data/pragmatic.csv')
df1= read_csv('data/semantic.csv')
df2= read_csv('data/spcom1.csv')
df3= read_csv('data/spcom2.csv')
df4= read_csv('data/aycom1.csv')
df5= read_csv('data/aycom2.csv')
df6= read_csv('data/spr1.csv')
df7= read_csv('data/own.csv')
df8= read_csv('data/other.csv')
df9= read_csv('data/falseb.csv')
df10= read_csv('data/false1.csv')
df11= read_csv('data/false2.csv')
dfcom= read_csv('data/aycom.csv')
dfaypr= read_csv('data/aypr1.csv')
spcom12= read_csv('data/spcom1y2.csv')
sppr12= read_csv('data/sppr12.csv')
spr2= read_csv('data/spr2.csv')
filspr2= read_csv('data/filspr2.csv')
meanage= read_csv('data/mean_age.csv')

# modelo con false 1 y false 2

mmylogitf1 = glm( answer~ age, data = , df11 = "binomial")



# EPT SP modelo

dfoh = df6 %>%
  filter(group == 'H')

mod6= glmer(
  variability ~ 0  + age + ayexhoc + ayexsch +
    (1 | participant) + 
    (1 | item),
  data = dfoh,
  family = 'binomial',
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=3e5)))

summary(mod6)




# producion2 sp

# Tab

aggregate(spr2$simple, by=list(group=spr2$group), FUN=sum)

spr2 %>% 
group_by(., group) %>% 
summarize(., mean = mean(na.omit(ntoken)), sd = sd(na.omit(ntoken)))

# plot 

filspr2  %>%
  ggplot(aes(x = verb , y = response, color = verb)) + 
  facet_grid(. ~ group) + 
  geom_hline(yintercept = 0.5, color = 'white', size = 2) + 
  stat_summary(fun = mean, geom = 'pointrange', size = 1, 
               position = position_dodge(width = 0.5)) + 
  scale_color_brewer(palette = "Set1", name = "") +
  #theme(legend.position = "none") +
  ylim(c(0,1)) +
  labs(x = 'task', y = 'Proportion of accurate responses', caption = '', 
       title = 'Proportion of accurate Spanish comprehension task responses across group') 


filspr2  %>%
  group_by(response, verb, group) %>%
  summarise(count=n())  %>%
  ggplot(aes(x = group, y = count, fill = verb)) +  # cuando estoy en ggplot cambio de la %>% al +
  geom_bar(position="dodge",stat = "identity", width=0.5) 

# modelo spr2

mod6= glmer(
  answer ~ 0  + aylevel + splevel + age +
    (1 | participant) + 
    (1 | item),
  data = sppr12,
  family = 'binomial',
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=3e5)))

summary(mod6)

mylogit6 = glm(answer~ age, data = comH, family = "binomial")

summary(mylogitc)


# priduccion 1 espanol

df6 %>% 
group_by(., group) %>% 
  summarize(., mean = mean(na.omit(variability)), sd = sd(na.omit(variability)))

df6 %>% 
group_by(group) %>% filter(simplepast==1)%>%  summarise(simplepast=n())



# produccion espanol 12

sppr12 %>%
filter(response !='var') %>%
ggplot(aes(x = response , y = answer, color = response)) + 
  facet_grid(. ~ group) + 
  geom_hline(yintercept = 0.5, color = 'white', size = 2) + 
  stat_summary(fun = mean, geom = 'pointrange', size = 1, 
               position = position_dodge(width = 0.5)) + 
  scale_color_brewer(palette = "Set1", name = "") +
  #theme(legend.position = "none") +
  ylim(c(0,1)) +
  labs(x = 'response ', y = 'Proportion of accurate responses', caption = '', 
       title = 'Proportion of variability responses across group')

# retelling task sp

spr2 %>%
  ggplot(aes(x = age, y = answer, color = task)) + 
  geom_hline(yintercept = 0.5, color = 'white', size = 2) + 
  stat_summary(fun = mean, geom = 'pointrange', size = 1, 
               position = position_dodge(width = 0.5)) + 
  scale_color_brewer(palette = "Set1", name = "") +
  #theme(legend.position = "none") +
  ylim(c(0,1)) +
  labs(x = 'age', y = 'Proportion of accurate responses', caption = '', 
       title = 'Figure 2: Proportion of accurate type 2 responses across age by H') 



df6 %>%
  select(simplepast, group) %>%
  filter(simplepast == 1 ) %>%
  group_by(simplepast, group) %>%
  summarise(count=n())  %>%
  ggplot(aes(x = group, y = count, fill = simplepast)) +  # cuando estoy en ggplot cambio de la %>% al +
  geom_bar(position="dodge",stat = "identity", width=0.5) 




sppr12  %>%
select(response, verb, group)%>%
group_by(verb, group) %>%
summarise(count=n())  %>%
ggplot(aes(x = group, y = count, fill = verb)) +  # cuando estoy en ggplot cambio de la %>% al +
geom_bar(position="dodge",stat = "identity", width=0.5) 


table(df6$estapres)


table(df1$Frequent, df1$EPT)


mod7= glmer(
  answer ~ 0  + splevel + aylevel +
    (1 | participant) + 
    (1 | item),
  data = sppr12,
  family = 'binomial',
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=3e5)))

summary(mod7)

mylogit6 = glm(answer~ age, data = comH, family = "binomial")

# spcom1 y2 

df2 %>% 
  group_by(., group) %>% 
  summarize(., mean = mean(na.omit(answer)), sd = sd(na.omit(answer)))

df3 %>% 
  group_by(., group) %>% 
  summarize(., mean = mean(na.omit(answer)), sd = sd(na.omit(answer)))


spcom12 %>%
  ggplot(aes(x = task , y = answer, color = task)) + 
  facet_grid(. ~ group) + 
  geom_hline(yintercept = 0.5, color = 'white', size = 2) + 
  stat_summary(fun = mean, geom = 'pointrange', size = 1, 
               position = position_dodge(width = 0.5)) + 
  scale_color_brewer(palette = "Set1", name = "") +
  #theme(legend.position = "none") +
  ylim(c(0,1)) +
  labs(x = 'task', y = 'Proportion of accurate responses', caption = '', 
       title = 'Figure 9: Proportion of accurate Spanish comprehension task responses across group') 


mod6= glmer(
  answer ~ 0  + aylevel + splevel + age +
    (1 | participant) + 
    (1 | item),
  data = df3,
  family = 'binomial',
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=3e5)))

summary(mod6)

mylogit6 = glm(answer~ age, data = comH, family = "binomial")

summary(mylogitc)


dfoh = df3 %>%
  select(answer, age, aylevel, splevel, group) %>% # seleccionar las variables q necesitamos 
  filter(group == 'H')


mylogit6 = glm(answer~ age, data = dfoh, family = "binomial")

summary(mylogit6)

# aypr1

dfaypr %>%
  select(morpheme, group, tay, ya) %>%
  group_by(morpheme, group) %>%
  summarise(count=n())  %>%
  ggplot(aes(x = morpheme, y = count, fill = group)) +  # cuando estoy en ggplot cambio de la %>% al +
  geom_bar(position="dodge",stat = "identity", width=0.5) 
facet_wrap(~group_lan) +
  theme(legend.position="none") +
  title = ('Figure 1: Accurate AJT_SP responses by group') # pregunta a Ricardo por que no sale el titulo

# Tab
table(dfaypr$tay, dfaypr$morpheme) 
group_by(group) 


table(df1$Frequent, df1$FCFTAGR)
table(df1$Frequent, df1$EPT)

 
# own 
 
df7 %>% 
  filter(task == 'ownver') %>% 
  group_by(., group ) %>% 
  summarize(., mean = mean(na.omit(answer)), sd = sd(na.omit(answer)))

df7 %>% 
  filter(task == 'ownesc') %>% 
  group_by(., group) %>% 
  summarize(., mean = mean(na.omit(answer)), sd = sd(na.omit(answer)))


df8 %>% 
  filter(task == 'otherver') %>% 
  group_by(., group ) %>% 
  summarize(., mean = mean(na.omit(answer)), sd = sd(na.omit(answer)))

df8 %>% 
  filter(task == 'otheresc') %>% 
  group_by(., group) %>% 
  summarize(., mean = mean(na.omit(answer)), sd = sd(na.omit(answer)))

df7%>% 
  group_by(group) %>% 
  summarize(., mean = mean(na.omit(age)), sd = sd(na.omit(age))) %>% 
  
  
  meanage%>% 
  group_num <- as.numeric(group)
  
t.test(age ~ group, data = meanage)

ggplot(meanage, aes(group, age)) +
  geom_boxplot()



# plot ajt by H by task by age 

df7 %>%
  ggplot(aes(x = age, y = answer, color = task)) + 
  facet_grid(. ~ group) + 
  geom_hline(yintercept = 0.5, color = 'white', size = 2) + 
  stat_summary(fun = mean, geom = 'pointrange', size = 1, 
               position = position_dodge(width = 0.5)) + 
  scale_color_brewer(palette = "Set1", name = "") +
  #theme(legend.position = "none") +
  ylim(c(0,1)) +
  labs(x = 'age', y = 'Proportion of accurate responses', caption = '', 
       title = 'Figure 4: Proportion of accurate type 1 and type 2 source of information responses across age by group') 
df7 %>%
  ggplot(aes(x = age, y = answer, color = task)) + 
  geom_hline(yintercept = 0.5, color = 'white', size = 2) + 
  stat_summary(fun = mean, geom = 'pointrange', size = 1, 
               position = position_dodge(width = 0.5)) + 
  scale_color_brewer(palette = "Set1", name = "") +
  #theme(legend.position = "none") +
  ylim(c(0,1)) +
  labs(x = 'age', y = 'Proportion of accurate responses', caption = '', 
       title = 'Figure 2: Proportion of accurate type 2 responses across age by H') 


#modelo con own

mylogit1 = glm(answer ~ 0 + age , data = df7, family = "binomial") # logit cada row es independ. glmm sabe las row que pertenecen a un individuo

summary(mylogit1) 

mylogit2 = glm(answer ~ 0 + group , data = df7, family = "binomial") # logit cada row es independ. glmm sabe las row que pertenecen a un individuo

summary(mylogit2) 

# others

df8 %>%
  ggplot(aes(x = age, y = answer, color = task)) + 
  facet_grid(. ~ group) + 
  geom_hline(yintercept = 0.5, color = 'white', size = 2) + 
  stat_summary(fun = mean, geom = 'pointrange', size = 1, 
               position = position_dodge(width = 0.5)) + 
  scale_color_brewer(palette = "Set1", name = "") +
  #theme(legend.position = "none") +
  ylim(c(0,1)) +
  labs(x = 'age', y = 'Proportion of accurate responses', caption = '', 
       title = 'Figure 2: Proportion of accurate type 2 source of information responses across age') 



mylogitc = glm(resplr01 ~ condition , data = df1, family = "binomial")



mylogit3 = glm(answer ~ 0 + age , data = df8, family = "binomial") # logit cada row es independ. glmm sabe las row que pertenecen a un individuo

summary(mylogit3) 

mylogit4 = glm(answer ~ 0 + group , data = df8, family = "binomial") # logit cada row es independ. glmm sabe las row que pertenecen a un individuo

summary(mylogit4) 

# language level 

df7 %>% 
  group_by(., group) %>% 
  summarize(., mean = mean(na.omit(splevel)), sd = sd(na.omit(splevel)))

df7 %>% 
  group_by(., group) %>% 
  summarize(., mean = mean(na.omit(aylevel)), sd = sd(na.omit(aylevel)))

dfh = df8 %>%
  select(answer, aylevel, splevel, group) %>% # seleccionar las variables q necesitamos 
  filter(group == 'HCU')

mylogit30 = glm(answer ~ 0 + aylevel , data = dfh, family = "binomial") # logit cada row es independ. glmm sabe las row que pertenecen a un individuo

summary(mylogit30)

mylogit31 = glm(answer ~ 0 + splevel , data = dfh, family = "binomial") # logit cada row es independ. glmm sabe las row que pertenecen a un individuo

summary(mylogit31)



mylogit5 = glm(answer ~ 0 + aylevel , data = df7, family = "binomial") # logit cada row es independ. glmm sabe las row que pertenecen a un individuo

summary(mylogit5)

mylogit6 = glm(answer ~ 0 + splevel , data = df7, family = "binomial") # logit cada row es independ. glmm sabe las row que pertenecen a un individuo

summary(mylogit6)


mylogit7 = glm(answer ~ 0 + aylevel , data = df8, family = "binomial") # logit cada row es independ. glmm sabe las row que pertenecen a un individuo

summary(mylogit7)

mylogit8 = glm(answer ~ 0 + splevel , data = df8, family = "binomial") # logit cada row es independ. glmm sabe las row que pertenecen a un individuo

summary(mylogit8)


# false belief 

df9 %>% 
  group_by(., group, age ) %>% 
  summarize(., mean = mean(na.omit(answer)), sd = sd(na.omit(answer)))

df9 %>% 
  group_by(., group, task) %>% 
  summarize(., mean = mean(na.omit(answer)), sd = sd(na.omit(answer)))

t.test(answer ~ group, data = df9)


df9 %>%
  ggplot(aes(x = age, y = answer, color = task)) + 
  facet_grid(. ~ group) + 
  geom_hline(yintercept = 0.5, color = 'white', size = 2) + 
  stat_summary(fun = mean, geom = 'pointrange', size = 1, 
               position = position_dodge(width = 0.5)) + 
  scale_color_brewer(palette = "Set1", name = "") +
  #theme(legend.position = "none") +
  ylim(c(0,1)) +
  labs(x = 'age', y = 'Proportion of accurate responses', caption = '', 
       title = 'Proportion of accurate FB responses across age by group') 



mylogit9 = glm(answer ~ 0 + age , data = df9, family = "binomial") # logit cada row es independ. glmm sabe las row que pertenecen a un individuo

summary(mylogit9)

mylogit10 = glm(answer ~ 0 + task , data = df9, family = "binomial") # logit cada row es independ. glmm sabe las row que pertenecen a un individuo

summary(mylogit10)


mylogit11 = glm(answer ~ 0 + aylevel , data = df9, family = "binomial") # logit cada row es independ. glmm sabe las row que pertenecen a un individuo

summary(mylogit11)

mylogit12 = glm(answer ~ 0 + splevel , data = df9, family = "binomial") # logit cada row es independ. glmm sabe las row que pertenecen a un individuo

summary(mylogit12)

df1HCU = df9 %>%
  select(answer, tybil, aylevel, splevel) %>% # seleccionar las variables q necesitamos 
  filter(tybil == 'L2')

mylogitHCU = glm(answer ~ 0 + aylevel , data = df1HCU, family = "binomial") # logit cada row es independ. glmm sabe las row que pertenecen a un individuo

summary(mylogitHCU)

mylogit13 = glm(answer ~ 0 +  , data = df9, family = "binomial") # logit cada row es independ. glmm sabe las row que pertenecen a un individuo

summary(mylogit9)


# semantic 

df1 %>% 
  filter(tybil == 'H') %>% 
  group_by(., tybil ) %>% 
  summarize(., mean = mean(na.omit(answer)), sd = sd(na.omit(answer)))

df1 %>% 
  filter(tybil == 'AH') %>% 
  group_by(., tybil ) %>% 
  summarize(., mean = mean(na.omit(answer)), sd = sd(na.omit(answer)))

df1 %>% 
  filter(tybil == 'H') %>% 
  group_by(., age ) %>% 
  summarize(., mean = mean(na.omit(answer)), sd = sd(na.omit(answer)))


df1 %>%
  filter( tybil== 'H') %>% 
  ggplot(aes(x = age, y = answer)) + 
  geom_hline(yintercept = 0.5, color = 'white', size = 2) + 
  stat_summary(fun = mean, geom = 'pointrange', size = 1, 
               position = position_dodge(width = 0.5)) + 
  scale_color_brewer(palette = "Set1", name = "") +
  #theme(legend.position = "none") +
  ylim(c(0,1)) +
  labs(x = 'age', y = 'Proportion of accurate responses', caption = '', 
       title = 'Figure 6: Proportion of accurate semantic task responses across age by HCH') 

df1 %>% 
  filter(tybil != 'L2') %>% 
  group_by(., tybil) %>% 
  summarize(., mean = mean(na.omit(ownver)), sd = sd(na.omit(ownver)))

df1 %>% 
  filter(tybil != 'L2') %>% 
  group_by(., tybil) %>% 
  summarize(., mean = mean(na.omit(ownesc)), sd = sd(na.omit(ownesc)))

df1 %>% 
  filter(tybil != 'L2') %>% 
  group_by(., tybil) %>% 
  summarize(., mean = mean(na.omit(otherver)), sd = sd(na.omit(otherver)))

df1 %>% 
  filter(tybil != 'L2') %>% 
  group_by(., tybil) %>% 
  summarize(., mean = mean(na.omit(otheresc)), sd = sd(na.omit(otheresc)))

df1 %>% 
  filter(tybil != 'L2') %>% 
  group_by(., tybil) %>% 
  summarize(., mean = mean(na.omit(falsebelief)), sd = sd(na.omit(falsebelief)))

# preparar data

df10 = df1 %>%
  select(answer, age, ownver, ownesc, otherver, otheresc, tybil) %>% # seleccionar las variables q necesitamos 
  filter(tybil == 'H')


mylogit13 = glm(answer ~ 0 + age , data = df10, family = "binomial") # logit cada row es independ. glmm sabe las row que pertenecen a un individuo

summary(mylogit13)


 mylogit14 = glm(answer ~ 0 + ownver , data = df10, family = "binomial") # logit cada row es independ. glmm sabe las row que pertenecen a un individuo

summary(mylogit14)

mylogit15 = glm(answer ~ 0 + ownesc , data = df10, family = "binomial") # logit cada row es independ. glmm sabe las row que pertenecen a un individuo

summary(mylogit15)

mylogit16 = glm(answer ~ 0 + otherver , data = df10, family = "binomial") # logit cada row es independ. glmm sabe las row que pertenecen a un individuo

summary(mylogit16)

mylogit17 = glm(answer ~ 0 + otheresc , data = df1, family = "binomial") # logit cada row es independ. glmm sabe las row que pertenecen a un individuo

summary(mylogit17)

mylogit17 = glm(answer ~ 0 + falsebelief , data = df1, family = "binomial") # logit cada row es independ. glmm sabe las row que pertenecen a un individuo

summary(mylogit17)

# mean pragmatic

df%>%
  group_by(tybil) %>%
  summarise(mean_prg = mean(answer, na.rm = T),  sd_prg = sd(answer, na.rm = T))

df %>%
  filter( tybil == 'H') %>%
  ggplot(aes(x = age, y = answer)) + 
  geom_hline(yintercept = 0.5, color = 'white', size = 2) + 
  stat_summary(fun = mean, geom = 'pointrange', size = 1, 
               position = position_dodge(width = 0.5)) + 
  scale_color_brewer(palette = "Set1", name = "") +
  #theme(legend.position = "none") +
  ylim(c(0,1)) +
  labs(x = 'age', y = 'Proportion of accurate responses', caption = '', 
       title = ' Figure 7: Proportion of accurate responses acroos age by HCH in pragmatic task ') 
#perepare data 

df11 = df %>%
  select(answer, age, ownver, ownesc, otherver, otheresc, falsebelief, tybil) %>% # seleccionar las variables q necesitamos 
  filter(tybil == 'H')


mylogit18 = glm(answer ~ 0 + age , data = df11, family = "binomial") # logit cada row es independ. glmm sabe las row que pertenecen a un individuo

summary(mylogit18)


mylogit19 = glm(answer ~ 0 + ownver , data = df11, family = "binomial") # logit cada row es independ. glmm sabe las row que pertenecen a un individuo

summary(mylogit19)

mylogit20 = glm(answer ~ 0 + ownesc , data = df11, family = "binomial") # logit cada row es independ. glmm sabe las row que pertenecen a un individuo

summary(mylogit20)

mylogit21 = glm(answer ~ 0 + otherver , data = df11, family = "binomial") # logit cada row es independ. glmm sabe las row que pertenecen a un individuo

summary(mylogit21)

mylogit22 = glm(answer ~ 0 + otheresc , data = df11, family = "binomial") # logit cada row es independ. glmm sabe las row que pertenecen a un individuo

summary(mylogit22)



 
mylogit23 = glm(answer ~ 0 + falsebelief , data = df, family = "binomial") # logit cada row es independ. glmm sabe las row que pertenecen a un individuo

summary(mylogit23)

# semantic con ayamra y espannol

df1 %>% 
  filter(tybil != 'AH') %>% 
  group_by(., tybil ) %>% 
  summarize(., mean = mean(na.omit(answer)), sd = sd(na.omit(answer)))

df1 %>%
  filter(tybil != 'AH') %>% 
  ggplot(aes(x = lang, y = answer)) + 
  facet_grid(. ~ tybil) + 
  geom_hline(yintercept = 0.5, color = 'white', size = 2) + 
  stat_summary(fun = mean, geom = 'pointrange', size = 1, 
               position = position_dodge(width = 0.5)) + 
  scale_color_brewer(palette = "Set1", name = "") +
  #theme(legend.position = "none") +
  ylim(c(0,1)) +
  labs(x = 'language', y = 'Proportion of accurate responses', caption = '', 
       title = 'Figure 3: Proportion of accurate semantic responses across language') 

df12 = df1 %>%
  select(answer,lang,tybil,splevel, aylevel) %>% # seleccionar las variables q necesitamos 
  filter(tybil != 'AH')

mod= glmer(
  answer ~ 0 + tybil + lang +
    (1 | participant) + 
    (1 | item),
  data = df11,
  family = 'binomial',
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=3e5)))
summary(mod)

mylogitc = glm(answer~ splevel , data = df12, family = "binomial")

summary(mylogitc)

# pragmatic con H y HCU

df %>% 
  filter(tybil != 'AH') %>% 
  group_by(., tybil ) %>% 
  summarize(., mean = mean(na.omit(answer)), sd = sd(na.omit(answer)))

mod= glmer(
  answer ~ 0 + tybil + lang +
    (1 | participant) + 
    (1 | item),
  data = df,
  family = 'binomial',
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=3e5)))
summary(mod)

mylogitc = glm(answer~ lang , data = df, family = "binomial")

summary(mylogitc)

# comp aymara

df4 %>% 
  group_by(., group ) %>% 
  summarize(., mean = mean(na.omit(answer)), sd = sd(na.omit(answer)))

df5 %>% 
  group_by(., group ) %>% 
  summarize(., mean = mean(na.omit(answer)), sd = sd(na.omit(answer)))


dfcom %>%
  ggplot(aes(x = group, y = answer)) + 
  facet_grid(. ~ task) + 
  geom_hline(yintercept = 0.5, color = 'white', size = 2) + 
  stat_summary(fun = mean, geom = 'pointrange', size = 1, 
               position = position_dodge(width = 0.5)) + 
  scale_color_brewer(palette = "Set1", name = "") +
  #theme(legend.position = "none") +
  ylim(c(0,1)) +
  labs(x = 'group', y = 'Proportion of accurate responses', caption = '', 
       title = 'Figure 8: Proportion of accurate responses across group in Ay com tasks') 

# modelo aycom1 a

mod= glmer(
  answer ~ 0  + ayexhoc + ayexhop + ayexsch + age + 
    (1 | participant) + 
    (1 | item),
  data = df4,
  family = 'binomial',
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=3e5)))
summary(mod)


mod2= glmer(
  answer ~ 0  + matherborn + fathertborn +
    (1 | participant) + 
    (1 | item),
  data = df4,
  family = 'binomial',
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=3e5)))
summary(mod2)

mod4= glmer(
  answer ~ 0  + aylevel + splevel +
    (1 | participant) + 
    (1 | item),
  data = df4,
  family = 'binomial',
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=3e5)))
summary(mod4)


mylogit = glm(answer~ matherborn, data = df4, family = "binomial")

summary(mylogit)

# modelos comhearcond

mod1= glmer(
  answer ~ 0 + ayexhoc + ayexhop + ayexsch + age + group +
    (1 | participant) + 
    (1 | item),
  data = df5,
  family = 'binomial',
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=3e5)))
summary(mod1)


mod3= glmer(
  answer ~ 0 + matherborn + fathertborn +
    (1 | participant) + 
    (1 | item),
  data = df5,
  family = 'binomial',
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=3e5)))
summary(mod3)

mod5= glmer(
  answer ~ 0  + aylevel + splevel +
    (1 | participant) + 
    (1 | item),
  data = df5,
  family = 'binomial',
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=3e5)))
summary(mod5)

mylogitc = glm(answer~ matherborn, data = df5, family = "binomial")

summary(mylogitc)

 # spanish retelling task


one.way <- aov(ntoken ~ group, data = spr2)

summary(one.way)


