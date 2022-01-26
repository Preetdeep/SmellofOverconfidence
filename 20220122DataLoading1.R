
library(dplyr)
library(stargazer)
library(bookdown)
library(ARTofR)
library(ggplot2)
library(readr)

df1 <- read_csv("NGS.csv")

df1<-NGS

df1$
df1$score<-df1$totcorr-df1$SELF_RATE_SMELL
df2<-subset(df1,NO_LOSS==1)### THIS IS TO BE CHANGED
gendersummary1<-df2%>%group_by(SEX)%>%summarise(meanright=mean(totcorr,na.rm=TRUE),meanscore=mean(score,na.rm=TRUE)) #people seem to be similar on the whole
t.test(subset(df2,SEX==1)$totcorr,subset(df2,SEX==2)$totcorr) #1 female is higher than male by a significant amount
t.test(subset(df2,SEX==1)$score,subset(df2,SEX==2)$score) #On the whole they were both more wrong than right but Women had a higher estimate of their ability
t.test(subset(df2,SEX==1)$SELF_RATE_SMELL,subset(df2,SEX==2)$SELF_RATE_SMELL)# Women rated themselves higher. 

dfcountry<-data.frame()
for (i in 1:84){
  df3<-subset(df2,COUNTRY==i)
  dfcountry[i,1]<-i
  dfcountry[i,2]<-mean(subset(df3,SEX==1)$totcorr,na.rm = TRUE)
  dfcountry[i,3]<-mean(subset(df3,SEX==2)$totcorr,na.rm = TRUE)
  dfcountry[i,4]<-tryCatch(t.test(subset(df3,SEX==1)$totcorr,subset(df3,SEX==2)$totcorr)$p.value,error = function(e) e)
  dfcountry[i,5]<-mean(subset(df3,SEX==1)$score,na.rm = TRUE)
  dfcountry[i,6]<-mean(subset(df3,SEX==2)$score,na.rm = TRUE)
  dfcountry[i,7]<-tryCatch(t.test(subset(df3,SEX==1)$score,subset(df3,SEX==2)$score)$p.value,error = function(e) e)
  dfcountry[i,8]<-mean(subset(df3,SEX==1)$SELF_RATE_SMELL,na.rm = TRUE)
  dfcountry[i,9]<-mean(subset(df3,SEX==2)$SELF_RATE_SMELL,na.rm = TRUE)
  dfcountry[i,10]<-tryCatch(t.test(subset(df3,SEX==1)$SELF_RATE_SMELL,subset(df3,SEX==2)$SELF_RATE_SMELL)$p.value,error = function(e) e)

}
colnames(dfcountry)<-c("CountryCode","Male_correct","Female_correct","Pvalue_correct","Male_score","Female_score","Pvalue_score","Male_self","Female_self","Pvalue_self")
write.csv(dfcountry,"CountryWiseScoreValuesLost.csv")

summary(lm(totcorr~as.factor(SEX)+as.factor(COUNTRY)+AGE,data=df2))
summary(lm(score~as.factor(SEX)+as.factor(COUNTRY)+AGE,data=df2))
summary(lm(SELF_RATE_SMELL~as.factor(SEX)+as.factor(COUNTRY)+AGE,data=df2))

t.test(subset(subset(df4,COUNTRY==28),SEX==1)$SELF_RATE_SMELL,subset(subset(df4,COUNTRY==32),SEX==1)$SELF_RATE_SMELL) ## Men from Israel have a higher estimation of their smelling ability as compared to men from Oman. The same was not statistically significant about women from these two countries. 
t.test(subset(subset(df4,COUNTRY==26),SEX==2)$totcorr,subset(subset(df4,COUNTRY==84),SEX==2)$totcorr)



df4<-subset(df1,NO_LOSS==0)### THIS IS TO BE CHANGED

#df4<-subset(df1,REGAIN_SMELL!=2) #this is leading to No Difference in OverConfidence between genders.
gendersummary2<-df4%>%group_by(SEX)%>%summarise(meanright=mean(totcorr,na.rm=TRUE),meanscore=mean(score,na.rm=TRUE)) #people seem to be similar on the whole
t.test(subset(df4,SEX==1)$totcorr,subset(df4,SEX==2)$totcorr) #1 female is higher than male
t.test(subset(df4,SEX==1)$score,subset(df4,SEX==2)$score) #On the whole they were both more wrong than right but Women overestimated
t.test(subset(df4,SEX==1)$SELF_RATE_SMELL,subset(df4,SEX==2)$SELF_RATE_SMELL)# Women rated themselves higher. 

dfcountry<-data.frame()
for (i in 1:84){
  df3<-subset(df4,COUNTRY==i)
  dfcountry[i,1]<-i
  dfcountry[i,2]<-mean(subset(df3,SEX==1)$totcorr,na.rm = TRUE)
  dfcountry[i,3]<-mean(subset(df3,SEX==2)$totcorr,na.rm = TRUE)
  dfcountry[i,4]<-tryCatch(t.test(subset(df3,SEX==1)$totcorr,subset(df3,SEX==2)$totcorr)$p.value,error = function(e) e)
  dfcountry[i,5]<-mean(subset(df3,SEX==1)$score,na.rm = TRUE)
  dfcountry[i,6]<-mean(subset(df3,SEX==2)$score,na.rm = TRUE)
  dfcountry[i,7]<-tryCatch(t.test(subset(df3,SEX==1)$score,subset(df3,SEX==2)$score)$p.value,error = function(e) e)
  dfcountry[i,8]<-mean(subset(df3,SEX==1)$SELF_RATE_SMELL,na.rm = TRUE)
  dfcountry[i,9]<-mean(subset(df3,SEX==2)$SELF_RATE_SMELL,na.rm = TRUE)
  dfcountry[i,10]<-tryCatch(t.test(subset(df3,SEX==1)$SELF_RATE_SMELL,subset(df3,SEX==2)$SELF_RATE_SMELL)$p.value,error = function(e) e)
  
}
colnames(dfcountry)<-c("CountryCode","Male_correct","Female_correct","Pvalue_correct","Male_score","Female_score","Pvalue_score","Male_self","Female_self","Pvalue_self")
write.csv(dfcountry,"CountryWiseScoreValuesNotLost.csv")


summary(lm(totcorr~as.factor(SEX)+as.factor(COUNTRY)+AGE,data=df4))
summary(lm(score~as.factor(SEX)+as.factor(COUNTRY)+AGE,data=df4))
summary(lm(SELF_RATE_SMELL~as.factor(SEX)+as.factor(COUNTRY)+AGE,data=df4))


\subsection{Average Estimate of Ability}
Mean estimate of ability of the respondents to smell, as self reported based on gender.

\subsection{Calculation of Correct Responses}

Self-rating of smelling ability is a variable from 1-5. 1 being the lowest and 5 representing an excellent assessment of the one's ability.

