
library('dplyr')
library('ggplot2')
library('rstudioapi')

current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))

#setwd("F:\\My_Docs\\univer\\DataScience\\Case_Studies\\Case Study 1\\casestudy1")

print( getwd())

df_beer = read.csv("Beers.csv",header = TRUE)
df_brew = read.csv("Breweries.csv",header = TRUE)

#Assign same name to brewID
colnames(df_beer)[grep("^Brew.*$", colnames(df_beer))]='BrewID'
colnames(df_brew)[grep("^Brew.*$", colnames(df_brew))]='BrewID'

#Answer Q1
df = group_by(df_brew,df_brew$State) 
BrewByState=summarize(df, count = n() )
BrewByState

#Answer Q2
final_brew=merge(df_beer,df_brew,by="BrewID")
head(final_brew)
tail(final_brew)

#Answer Q3
na_count=sapply(final_brew, function(y) sum(length(which(is.na(y)))))
na_count

#Answer Q4
BrewByState_all = group_by(final_brew,final_brew$State,na.rm=TRUE) 

BrewByState_ABV=as_data_frame(summarize(BrewByState_all,Median = median(ABV, na.rm = T) ,Max = max(ABV, na.rm = T)))
BrewByState_IBU=as_data_frame(summarize(BrewByState_all,Median = median(IBU, na.rm = T) ,Max = max(IBU, na.rm = T)))

colnames(BrewByState_ABV)[grep(".*State.*$", colnames(BrewByState_ABV))]='State'
colnames(BrewByState_IBU)[grep(".*State.*$", colnames(BrewByState_IBU))]='State'

select(BrewByState_ABV,State,Median)
select(BrewByState_IBU,State,Median)

par(las=2)
barplot(BrewByState_ABV$Median,names.arg=BrewByState_ABV$State,
        main = "Beer Median ABV by state",
        horiz=FALSE,col='darkblue',space=1)

par(las=2)
barplot(BrewByState_IBU$Median,names.arg=BrewByState_IBU$State,
        main = "Beer Median IBU by state",
        horiz=FALSE,col='darkblue',space=1)

#Answer Q5
#BrewByState_ABV_m=na.omit(BrewByState_ABV)
#BrewByState_IBU_m=na.omit(BrewByState_IBU)

#BrewByState_ABV_max=summarize(BrewByState_all,Max = max(ABV, na.rm = T)  )
#BrewByState_IBU_max=summarize(BrewByState_all,Max = max(IBU, na.rm = T)  )

#colnames(BrewByState_ABV)[grep(".*State.*$", colnames(BrewByState_ABV))]='State'
#colnames(BrewByState_IBU)[grep(".*State.*$", colnames(BrewByState_IBU))]='State'


#BrewByState_ABV=na.omit(BrewByState_ABV)
#BrewByState_IBU=na.omit(BrewByState_IBU)


BrewByState_ABV$State[which(BrewByState_ABV$Max == max(BrewByState_ABV$Max))]
BrewByState_IBU$State[which(BrewByState_IBU$Max == max(BrewByState_IBU$Max))]

#BrewByState_ABV
#BrewByState_IBU

#Answer Q6
summary(final_brew$ABV)

#Answerr Q7
ggplot(data = final_brew, mapping =aes(x=final_brew$ABV, y=final_brew$IBU )) +
  geom_point(shape = 16, size = 2, show.legend = FALSE,colour="#CC0000") +
  theme(
    plot.title = element_text(color="red", size=14, face="bold.italic"),
    axis.title.x = element_text(color="blue", size=14, face="bold"),
    axis.title.y = element_text(color="blue", size=14, face="bold"))+
  geom_smooth(method = lm)+
  ggtitle("Plot of ABV vs IBU") +
    xlab("ABV") + ylab("IBU")


corr=cor.test(final_brew$ABV,final_brew$IBU)
corr
Rsquared=corr$estimate^2
Rsquared
