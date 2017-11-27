rm(list = ls())

library(tidyverse)
library(httr)
library(jsonlite)
library(plyr)
library(tmap)

api.key <- '53D17844-D92E-3CF5-8520-6E1E45CA2A49'
api.url4acres <- paste(
  'http://quickstats.nass.usda.gov/api/api_GET/?key=', api.key,
  '&source_desc=SURVEY',
  '&agg_level_desc=STATE',
  '&commodity_desc=PEANUTS',
  '&reference_period_desc=YEAR',
  '&year__GE=2015',
  '&short_desc=PEANUTS - ACRES HARVESTED',
  sep=''
)
api.return4acres <- GET(api.url4acres)

peanutHarvest <-
  fromJSON(content(api.return4acres, 'text'))$data 
peanutHarvest

dat<-subset(peanutHarvest, year==2016)
dat$value<-as.numeric(gsub(",", "", dat$Value))
dat$value<-as.numeric(dat$value)
dat$percent<-round(dat$value/sum(dat$value)*100,1)
dat2<-cbind(dat$state_name,dat$value, dat$percent)  
dat3<-as.data.frame(dat2)
colnames(dat3)<-c("State", "acres", "% Total")
dat4<-subset(dat3, State!= "OTHER STATES")
dat4$Rank<-rank(-as.numeric(as.character(dat4$acres)))
write.csv(dat4,"dat4table1.csv")

######

api.url4dsfw <- paste(
  'http://quickstats.nass.usda.gov/api/api_GET/?key=', api.key,
  '&source_desc=SURVEY',
  '&commodity_desc=FIELDWORK',
  '&short_desc=FIELDWORK - DAYS SUITABLE, MEASURED IN DAYS / WEEK ',
  sep=''
)
api.return4dsfw <- GET(api.url4dsfw)

DSFW <-
  fromJSON(content(api.return4dsfw, 'text'))$data 
DSFW
DSFW<-subset(DSFW, state_name!="US TOTAL")

api.url4plant <- paste(
  'http://quickstats.nass.usda.gov/api/api_GET/?key=', api.key,
  '&source_desc=SURVEY',
  '&year__GE=2005',
  '&short_desc=PEANUTS - PROGRESS, MEASURED IN PCT PLANTED',
  sep=''
)
api.return4plant <- GET(api.url4plant)

peanutPlantProg <-
  fromJSON(content(api.return4plant, 'text'))$data

peanutPlantProg
peanutPlantProg<-subset(peanutPlantProg, state_name!="US TOTAL")

api.url4harv <- paste(
  'http://quickstats.nass.usda.gov/api/api_GET/?key=', api.key,
  '&source_desc=SURVEY',
  '&year__GE=2005',
  '&short_desc=PEANUTS - PROGRESS, MEASURED IN PCT HARVESTED',
  sep=''
)
api.return4harv <- GET(api.url4harv)

peanutHarvProg <-
  fromJSON(content(api.return4harv, 'text'))$data 
peanutHarvProg
peanutHarvProg<-subset(peanutHarvProg, state_name!="US TOTAL")

tab4desc<-matrix("NA", nrow=nlevels(factor(peanutPlantProg$state_name)), ncol=5)
mat4lm<-matrix("NA", nrow=nlevels(factor(peanutPlantProg$state_name)), ncol = 7)

# by state
for(i in 1:nlevels(factor(peanutPlantProg$state_name))){
  #  i=5
  state<-levels(factor(peanutPlantProg$state_name))[i]
  DSFWdat<-subset(DSFW, state_name==state)
  plantdat<-subset(peanutPlantProg, state_name==state)
  last5yearsp<-subset(plantdat, year>=2014)
  last5yearsp$value<-as.numeric(as.character(last5yearsp$Value))
  last5yearsp$begin_code1<-as.numeric(as.character(last5yearsp$begin_code))
  last10yearsp<-subset(plantdat, year>=2008)
  dat4graph5yrsp<-aggregate(value~begin_code1, data=last5yearsp, FUN = "mean")
  #dat4graph10yrsp<-aggregate(Value~begin_code, data=last10yearsp, FUN = "mean")
  
  png(paste("myPlot", state,".png", sep=""), 
      width=6, height=6, units="in", res=720)
  par(mfrow=c(2,2))
      
  #by begin codes
  numRows<-nlevels(factor(DSFWdat$begin_code))-as.numeric(min(levels(factor(DSFWdat$begin_code))))+1
  mat4longterm<-matrix("NA", nrow = numRows, ncol = 4)
  for(k in as.numeric(min(levels(factor(DSFWdat$begin_code)))):nlevels(factor(DSFWdat$begin_code))){
    #k=11
    WOY<-k
    j<-k-as.numeric(min(levels(factor(DSFWdat$begin_code))))+1
    dat4longtermDSFW<-subset(DSFWdat, begin_code==WOY)
    q4chart<-quantile(as.numeric(as.character(dat4longtermDSFW$Value)), probs=c(.2, .5, .8))
    mat4longterm[j,1]<-k
    mat4longterm[j,2]<-q4chart[1]
    mat4longterm[j,3]<-q4chart[2]
    mat4longterm[j,4]<-q4chart[3]
    
  }
  
  colnames(mat4longterm)<-c("WOY", "Bad20th", "Median50th", "Good80th")
  mat4LT<-as.data.frame(mat4longterm)
  mat4LT$Bad20th<-   as.numeric(as.character(mat4LT$Bad20th))
  mat4LT$Median50th<-as.numeric(as.character(mat4LT$Median50th))
  mat4LT$Good80th<-  as.numeric(as.character(mat4LT$Good80th)) 
  
  plot(mat4longterm[,1], mat4longterm[,4], xlim=c(1,50), ylim=c(0,7), type="l", main="Fieldwork probabilities",
       xlab="Week of Year", ylab="Days per week", lty=1, col="green")
  lines(mat4longterm[,1], mat4longterm[,3], col="yellow", lty=2)
  lines(mat4longterm[,1], mat4longterm[,2], col="red", lty=3)
  legend(5, 1,  c("80th", "50th", "20th"), col=c("green", "yellow","red"), lty=c(1,2,3), horiz=TRUE, cex = .7, bty = "n") 
  
  #dev.off()
  
  begin=15
  end=85
  beginPeriodp<-min(which(abs(dat4graph5yrsp$value-begin)==min(abs(dat4graph5yrsp$value-begin))))
  begin15p<-dat4graph5yrsp$begin_code1[beginPeriodp]+1
  endPeriodp<-min(which(abs(dat4graph5yrsp$value-end)==min(abs(dat4graph5yrsp$value-end))))
  end85p<-dat4graph5yrsp$begin_code1[endPeriodp]
  
  tab4desc[i,1]<-state
  tab4desc[i,2]<-begin15p
  tab4desc[i,3]<-end85p
  
  numWeeks<-end85p-begin15p+1

    pDSFWdat<-subset(DSFWdat, begin_code>=begin15p & begin_code<=end85p)
    pDSFWdat$value<-as.numeric(as.character(pDSFWdat$Value))
    pDSFWdat$year<-as.numeric(as.character(pDSFWdat$year))
    DSFWdatptest<-aggregate(value~year, data=pDSFWdat, FUN = "length")
    DSFWdatp<-aggregate(value~year, data=pDSFWdat, FUN = "sum")
    DSFWdat2<-merge(DSFWdatp,DSFWdatptest,by="year")
    DSFWdat3<-subset(DSFWdat2, value.y==numWeeks)
    
    numYearsp<-length(DSFWdat3$year)
    
      
    mat4lm[i,1]<-state
    mat4lm[i,2:4]<-round(summary(lm(DSFWdat3$value.x~DSFWdat3$year))$coefficients[2,c(1,2,4)],2)
    
    hdat<-subset(peanutHarvProg, state_name==state)
    
    hlast5years<-subset(hdat, year>=2014)
    hlast5years$value<-as.numeric(as.character(hlast5years$Value))
    #last5yearsp$begin_code1<-as.numeric(as.character(last5yearsp$begin_code))
    #hlast10years<-subset(hdat, year>=2008)
  hdat4graph5yrs<-aggregate(value~begin_code, data=hlast5years, FUN = "mean")
  #hdat4graph10yrs<-aggregate(Value~begin_code, data=hlast10years, FUN = "mean")
  
  plot(dat4graph5yrsp$begin_code1, dat4graph5yrsp$value, 
       ylab="Percentile", xlab="Week of Year", type="l", col="blue",  
       main="Crop progress", xlim=c(0,52), ylim=c(0,100))
  lines(hdat4graph5yrs$begin_code, hdat4graph5yrs$value, type="l", col="purple", lty=3)
  abline(h =c(15,85), lty=3, col="grey")
  legend(0, 80,  c("planting", "harvest"), col=c("blue", "purple"), lty=c(1,3), cex=.7, bg="transparent", bty = "n") 
         
  
  beginPeriod<-min(which(abs(hdat4graph5yrs$value-begin)==min(abs(hdat4graph5yrs$value-begin))))
  hbegin15<-as.numeric(hdat4graph5yrs$begin_code[beginPeriod])+1
  endPeriod<-min(which(abs(hdat4graph5yrs$value-end)==min(abs(hdat4graph5yrs$value-end))))
  hend85<-as.numeric(hdat4graph5yrs$begin_code[endPeriod])
  
  tab4desc[i,4]<-as.numeric(as.character(hbegin15))
  tab4desc[i,5]<-as.numeric(as.character(hend85))
  
  hDSFWdat<-subset(DSFWdat, begin_code>=hbegin15 & begin_code<=hend85)
  hDSFWdat$value<-as.numeric(as.character(hDSFWdat$Value))
  hDSFWdat$year<-as.numeric(as.character(hDSFWdat$year))
  
  hDSFWdattest<-aggregate(value~year, data=hDSFWdat, FUN = "length")
  
  hDSFWdath<-aggregate(value~year, data=hDSFWdat, FUN = "sum")
  hnumWeeks<-hend85-hbegin15+1
  hDSFWdat2<-merge(hDSFWdath,hDSFWdattest,by="year")
  hDSFWdat3<-subset(hDSFWdat2, value.y==hnumWeeks)
  hDSFWdat2<-aggregate(value.x~year, data=hDSFWdat3, FUN = "sum")
  
  mat4lm[i,5:7]<-round(summary(lm(hDSFWdat2$value.x~hDSFWdat2$year))$coefficients[2,c(1,2,4)],2)
  
  numYearsh<-length(hDSFWdat2$year)
  
  
  
  hist(DSFWdat3$value.x, 
       xlim=c(0,60), breaks=seq(0,60, by=2), 
       col="grey", border="black",
       xlab="fieldwork days" , ylab=paste("Count: n= ", numYearsp, " years", sep=""),
       main="Days suitable for planting")
  
  hist(hDSFWdat2$value.x, 
       xlim=c(0,60), breaks=seq(0,60, by=2), 
       col="grey", border="black",
       xlab="fieldwork days", ylab=paste("Count: n= ", numYearsh, " years", sep=""),
       main="Days suitable for harvest")
  
   dev.off()
  
  }
 
colnames(tab4desc)<-c("State", "Begin Plant", "End Plant", "Begin Harvest", "End Harvest")
tab4desc
write.table(tab4desc, "tab4desc.txt", sep=",")
write.csv(tab4desc, "tab4desc.csv")
tab4<-as.data.frame(tab4desc)
table1<-merge(dat4,tab4,by="State")
table1$State<-tolower(table1$State)
write.csv(table1, "table1.csv")

colnames(mat4lm)<-c("State", "Planting Time Slope", "Planting Time SE", "Planting Time p-value","Harvest Time Slope", "Harvest Time SE", "Harvest Time p-value")
mat4lm<-as.data.frame(mat4lm)
mat4lm$State<-tolower(mat4lm$State)
write.csv(mat4lm, "slopestats.csv")
###

#####
# creates word doc for tables

library(ReporteRs)
doc = docx( title = 'GriffinPeanuts' )

doc = addTitle( doc , 
                'Table 1.  2016 USDA-NASS harvested acreage and typical fieldwork dates.', 
                level = 3)
myTable1<-vanilla.table(table1)
doc = addFlexTable( doc , myTable1 )


doc = addPageBreak(doc)
doc = addTitle( doc ,
'Table 2.  Slope and significance of trends in DSFW during most active harvest dates over time.', 
                level = 3)
doc = addFlexTable( doc , vanilla.table(mat4lm) )

doc = addImage(doc, "myPlotALABAMA.png", width=6, height=6 )
doc = addTitle( doc , 'Figure 1. Plots for Alabama', level = 3)
doc = addPageBreak(doc)
doc = addImage(doc, "myPlotARKANSAS.png", width=6, height=6 )
doc = addTitle( doc , 'Figure 2. Plots for Arkansas', level = 3)
doc = addPageBreak(doc)
doc = addImage(doc, "myPlotFLORIDA.png", width=6, height=6 )
doc = addTitle( doc , 'Figure 3. Plots for Florida', level = 3)
doc = addPageBreak(doc)
doc = addImage(doc, "myPlotGEORGIA.png", width=6, height=6 )
doc = addTitle( doc , 'Figure 4. Plots for Georgia', level = 3)
doc = addPageBreak(doc)
doc = addImage(doc, "myPlotMISSISSIPPI.png", width=6, height=6 )
doc = addTitle( doc , 'Figure 5. Plots for Mississippi', level = 3)
doc = addPageBreak(doc)
doc = addImage(doc, "myPlotNORTH CAROLINA.png", width=6, height=6 )
doc = addTitle( doc , 'Figure 6. Plots for North Carolina', level = 3)
doc = addPageBreak(doc)
doc = addImage(doc, "myPlotNEW MEXICO.png", width=6, height=6 )
doc = addTitle( doc , 'Figure 7. Plots for New Mexico', level = 3)
doc = addPageBreak(doc)
doc = addImage(doc, "myPlotOKLAHOMA.png", width=6, height=6 )
doc = addTitle( doc , 'Figure 8. Plots for Oklahoma', level = 3)
doc = addPageBreak(doc)
doc = addImage(doc, "myPlotSOUTH CAROLINA.png", width=6, height=6 )
doc = addTitle( doc , 'Figure 9. Plots for South Carolina', level = 3)
doc = addPageBreak(doc)
doc = addImage(doc, "myPlotVIRGINIA.png", width=6, height=6 )
doc = addTitle( doc , 'Figure 10. Plots for Virginia', level = 3)
doc = addPageBreak(doc)
doc = addImage(doc, "myPlotTEXAS.png", width=6, height=6 )
doc = addTitle( doc , 'Figure 11. Plots for Texas', level = 3)
doc = addPageBreak(doc)

writeDoc(doc, 'tables4peanuts.docx')


library(choroplethr)
dat4map<-as.data.frame(cbind(tolower(dat3$State), as.numeric(as.character(dat3$acres))))
colnames(dat4map)<-c("region", "value")
dat4map$value<-as.numeric(as.character(dat4map$value))
data(continental_us_states)
state_choropleth(dat4map, title="", legend="Harvested acres", num_colors = 3, zoom = continental_us_states)

c=StateChoropleth$new(dat4map)
c$title = ""
c$legend = "Peanut production (acres)"
c$set_num_colors(4)
c$set_zoom(continental_us_states)
c$show_labels = FALSE
without_abbr = c$render()
