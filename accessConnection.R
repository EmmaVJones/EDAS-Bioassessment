# R 3.5.1

# This file walks you through connecting MS Access to R
# MS Access is dumb and needs to be run in 32bit R

# packages
library(tidyverse)
library(RODBC)
library(dplyr)
library(DT)
library(ggplot2)
library(scales)


# Make connection with Access
channel <- odbcConnectAccess2007('data/EDASGenus.accdb')

# Pull tables you need
SCIquery <- sqlQuery(channel, 'SELECT * FROM SCIQuery')

Stationsquery<- sqlQuery(channel, 'SELECT * FROM Stations')

IR<-sqlQuery(channel, 'SELECT * FROM IR2020')


totalHab <- sqlQuery(channel,'SELECT * FROM HabValues')

totHabCon <- sqlQuery(channel,'SELECT * FROM HabSamps')
# Lucy: pull in Stations table and practice joining to other tables

## Lucy assignment:
#function to filter by 1 station
# lucy stretch: filter above by date rance, or year(s)

#Select Station from stations query to add stream name and location info to markdown
UserStation<-"4ASRE015.43"
startyear<- 2009
endyear<- 2014
method<-IR$AssessmentMethod

stationqry<- function(UserStation){
  filter(Stationsquery,StationID==UserStation)
}
#StationQRY<-stationqry(UserStation)

IRqry<- function(UserStation){
  filter(IR,StationID==UserStation)
}

IRquery<-IRqry(UserStation)

#Function to filter by 1 station and range of years



selectStation<-function(UserStation, startyear, endyear){
  station<- filter(SCIquery,StationID==UserStation)
  ###station<-subset(station, RepNum==1)#only selects Rep 1's
  subset(station, Year>= as.integer(startyear) & Year<= as.integer(endyear))

  }


#annualdata<-selectStation(UserStation, startyear, endyear)

#Display impaired or non-impaired message

VSCItester <- function(VSCI){
  
  message1 <- NA
  
  for (i in 1:length(VSCI)){
    
    if(is.na(VSCI[i])){
      message1[i] <- 'Not sampled'
    }else{
      if(VSCI[i] >= 60){message1[i] <- 'Not Impaired'}
      if(VSCI[i] < 60){message1[i] <- 'Impaired'}
    }
    
  }
  return(message1)
  
}


VCPMItester <- function(VSCI){
  
  message1 <- NA
  
  for (i in 1:length(VSCI)){
    
    if(is.na(VSCI[i])){
      message1[i] <- 'Not sampled'
    }else{
      if(VSCI[i] >= 40){message1[i] <- 'Not Impaired'}
      if(VSCI[i] < 40){message1[i] <- 'Impaired'}
    }
    
  }
  return(message1)
  
}

#Table function for VSCI metrics, had to add conditional statement if data had spring or fall data only. 




basicTable <- function(annualdata){
  # grab info we want from input dataframe
  if (method=="VCPMI"){ 
    tableData <- select(annualdata, Year, Season, IBI, RepNum) %>%
      spread(Season, IBI) %>%
      mutate(`Spring VCPMI` = as.numeric(format(Spring, digits=3)),
                               `Spring Assessment` = VSCItester(`Spring VCPMI`),
                               `Fall VCPMI` = as.numeric(format(Fall, digits=3)),
                               `Fall Assessment` = VCPMItester(`Fall VCPMI`)) %>%
      select(Year, RepNum, `Spring VCPMI`, `Spring Assessment`, `Fall VCPMI`, `Fall Assessment`)
    
  }else{
    tableData <- select(annualdata, Year, Season, IBI, RepNum) %>%
      spread(Season, IBI) %>%
      mutate(`Spring VSCI` = as.numeric(format(Spring, digits=3)),
                     `Spring Assessment` = VSCItester(`Spring VSCI`),
                     `Fall VSCI` = as.numeric(format(Fall, digits=3)),
                     `Fall Assessment` = VSCItester(`Fall VSCI`)) %>%
      select(Year, RepNum,  `Spring VSCI`, `Spring Assessment`, `Fall VSCI`, `Fall Assessment`)
  }
   
  #make table the way we want
  datatable(tableData, rownames=F, class = 'cell-border stripe',
            options = list(pageLength= nrow(tableData), dom = 't',
                                                    initComplete = JS(
                                                      "function(settings, json) {",
                                                      "$(this.api().table().header()).css({'background-color': '#636363', 'color': '#fff', 'font-size': '170%'});",
                                                      "}"))) %>%
    formatStyle('Fall Assessment', textAlign = 'center') %>%
    formatStyle('Spring Assessment', textAlign = 'center')%>%
    formatStyle(c('Fall Assessment', 'Fall VSCI'), 'Fall Assessment', backgroundColor = styleEqual(c('Not Sampled'), c('gray'), default = 'white'))%>%
    formatStyle(c('Spring Assessment', 'Spring VSCI'), 'Spring Assessment', backgroundColor = styleEqual(c('Not sampled'), c('gray'), default = 'white'))%>%
    formatStyle(columns=c(1,2,3,4,5,6), fontSize='150%')
  
}


SpringTable <- function(annualdata){
  # grab info we want from input dataframe
  if (method=="VCPMI"){ 
    tableData <- select(annualdata, Year, Season, IBI, RepNum) %>%
      spread(Season, IBI) %>%
      mutate(`Spring VCPMI` = as.numeric(format(Spring, digits=3)),
             `Spring Assessment` = VCPMItester(`Spring VCPMI`),
             `Fall VCPMI` = NA,
             `Fall Assessment` = VSCItester(`Fall VCPMI`)) %>%
      select(Year, RepNum,  `Spring VCPMI`, `Spring Assessment`, `Fall VCPMI`, `Fall Assessment`)
    
  }else{
    tableData <- select(annualdata, Year, Season, IBI, RepNum) %>%
      spread(Season, IBI) %>%
      mutate(`Spring VSCI` = as.numeric(format(Spring, digits=3)),
             `Spring Assessment` = VSCItester(`Spring VSCI`),
             `Fall VSCI` = NA,
             `Fall Assessment` = VSCItester(`Fall VSCI`)) %>%
      select(Year, RepNum, `Spring VSCI`, `Spring Assessment`, `Fall VSCI`, `Fall Assessment`)
  }
  #make table the way we want
  datatable(tableData, rownames=F,class = 'cell-border stripe', options = list(pageLength= nrow(tableData), dom = 't',
                                                  initComplete = JS(
                                                    "function(settings, json) {",
                                                    "$(this.api().table().header()).css({'background-color': '#636363', 'color': '#fff', 'font-size': '170%'});",
                                                    "}"))) %>%
    formatStyle('Fall Assessment', textAlign = 'center') %>%
    formatStyle('Spring Assessment', textAlign = 'center')%>%
    formatStyle(c('Fall Assessment', 'Fall VSCI'), 'Fall Assessment', backgroundColor = styleEqual(c('Not Sampled'), c('gray'), default = 'white'))%>%
    formatStyle(c('Spring Assessment', 'Spring VSCI'), 'Spring Assessment', backgroundColor = styleEqual(c('Not sampled'), c('gray'), default = 'white'))%>%
    formatStyle(columns=c(1,2,3,4,5,6), fontSize='150%')
  
}

FallTable <- function(annualdata){
  # grab info we want from input dataframe
  if (method=="VCPMI"){ 
    tableData <- select(annualdata, Year, Season, IBI, RepNum) %>%
      spread(Season, IBI) %>%
      mutate(`Spring VCPMI` = NA,
             `Spring Assessment` = VCPMItester(`Spring VCPMI`),
             `Fall VCPMI` = as.numeric(format(Fall, digits=3)),
             `Fall Assessment` = VSCItester(`Fall VCPMI`)) %>%
      select(Year,  RepNum, `Spring VCPMI`, `Spring Assessment`, `Fall VCPMI`, `Fall Assessment`)
    
  }else{
    tableData <- select(annualdata, Year, Season, IBI, RepNum) %>%
      spread(Season, IBI) %>%
      mutate(`Spring VSCI` = NA,
             `Spring Assessment` = VSCItester(`Spring VSCI`),
             `Fall VSCI` = as.numeric(format(Fall, digits=3)),
             `Fall Assessment` = VSCItester(`Fall VSCI`)) %>%
      select(Year, RepNum, `Spring VSCI`, `Spring Assessment`, `Fall VSCI`, `Fall Assessment`)
  }
  #make table the way we want
  datatable(tableData, rownames=F,class = 'cell-border stripe', options = list(pageLength= nrow(tableData), dom = 't', 
                                                  initComplete = JS( 
                                                    "function(settings, json) {",
                                                    "$(this.api().table().header()).css({'background-color': '#636363', 'color': '#fff', 'font-size': '170%'});",
                                                    "}"))) %>%
    formatStyle('Fall Assessment', textAlign = 'center') %>%
    formatStyle('Spring Assessment', textAlign = 'center')%>%
    formatStyle(c('Fall Assessment', 'Fall VSCI'), 'Fall Assessment', backgroundColor = styleEqual(c('Not Sampled'), c('gray'), default = 'white'))%>%
    formatStyle(c('Spring Assessment', 'Spring VSCI'), 'Spring Assessment', backgroundColor = styleEqual(c('Not sampled'), c('gray'), default = 'white'))%>%
    formatStyle(columns=c(1,2,3,4,5,6), fontSize='150%')
  
}


Table<- function (annualdata){
  if("Spring" %in% annualdata$Season && "Fall" %in% annualdata$Season){
    basicTable(annualdata)
  }else{
    if("Spring" %in% annualdata$Season){
      SpringTable(annualdata)
    }else{
      FallTable(annualdata)
    }
  }
}

#Table(annualdata)
 

#Summary Table of average of Seasons


SumTable<- function(annualdata){
  if (method=="VCPMI"){
    total<- select(annualdata, Year, Season,IBI)%>%
      summarise(`Average VCPMI`=mean(IBI), `Number of samples`= n())%>%
      mutate(`Average VCPMI`= as.numeric(format(`Average VCPMI`, digits=3)), 
      `Season` ="Total Average")%>%
      select(Season, `Average VCPMI`, `Number of samples`)
    two<-subset(annualdata, Year>= endyear-2)%>%
      summarise(`Average VCPMI`=mean(IBI), `Number of samples`=n())%>%
      mutate(`Average VCPMI`= as.numeric(format(`Average VCPMI`, digits=3)), 
           `Season` ="2-year Average")
    twospr<- subset(annualdata, Season=="Spring" & Year>= endyear-2)%>%
      summarise(`Average VCPMI`=mean(IBI), `Number of samples`=n())%>%
      mutate(`Average VCPMI`= as.numeric(format(`Average VCPMI`, digits=3)), 
           `Season` ="2-year Spring Average")
    twofall<-subset(annualdata, Season=="Fall" & Year>= endyear-2)%>%
      summarise(`Average VCPMI`=mean(IBI), `Number of samples`=n())%>%
      mutate(`Average VCPMI` = as.numeric(format(`Average VCPMI`, digits=3)), 
           `Season` ="2-year Fall Average")
    six<-subset(annualdata, Year>= endyear-6)%>%
      summarise(`Average VCPMI`=mean(IBI), `Number of samples`=n())%>%
      mutate(`Average VCPMI`= as.numeric(format(`Average VCPMI`, digits=3)), 
           `Season` ="6-year Average")
    sixspr<-subset(annualdata, Season=="Spring" & Year>= endyear-6)%>%
      summarise(`Average VCPMI`=mean(IBI), `Number of samples`=n())%>%
      mutate(`Average VCPMI` = as.numeric(format(`Average VCPMI`, digits=3)), 
           `Season` ="6-year Spring Average")
    sixfall<-subset(annualdata, Season=="Fall" & Year>= endyear-6)%>%
      summarise(`Average VCPMI`=mean(IBI), `Number of samples`=n())%>%
      mutate(`Average VCPMI` = as.numeric(format(`Average VCPMI`, digits=3)), 
           `Season` ="6-year Fall Average")
    FinalTable<-rbind(twospr, twofall, sixspr, sixfall , six, two, total)%>%
      select(Season, `Average VCPMI`, `Number of samples`)
  }else {
    total<- select(annualdata, Year, Season,IBI)%>%
      summarise(`Average VSCI`=mean(IBI), `Number of samples`= n())%>%
      mutate(`Average VSCI`= as.numeric(format(`Average VSCI`, digits=3)), 
             `Season` ="Total Average")%>%
      select(Season, `Average VSCI`, `Number of samples`)
    two<-subset(annualdata, Year>= endyear-2)%>%
      summarise(`Average VSCI`=mean(IBI), `Number of samples`=n())%>%
      mutate(`Average VSCI`= as.numeric(format(`Average VSCI`, digits=3)), 
             `Season` ="2-year Average")
    twospr<- subset(annualdata, Season=="Spring" & Year>= endyear-2)%>%
      summarise(`Average VSCI`=mean(IBI), `Number of samples`=n())%>%
      mutate(`Average VSCI`= as.numeric(format(`Average VSCI`, digits=3)), 
             `Season` ="2-year Spring Average")
    twofall<-subset(annualdata, Season=="Fall" & Year>= endyear-2)%>%
      summarise(`Average VSCI`=mean(IBI), `Number of samples`=n())%>%
      mutate(`Average VSCI` = as.numeric(format(`Average VSCI`, digits=3)), 
             `Season` ="2-year Fall Average")
    six<-subset(annualdata, Year>= endyear-6)%>%
      summarise(`Average VSCI`=mean(IBI), `Number of samples`=n())%>%
      mutate(`Average VSCI`= as.numeric(format(`Average VSCI`, digits=3)), 
             `Season` ="6-year Average")
    sixspr<-subset(annualdata, Season=="Spring" & Year>= endyear-6)%>%
      summarise(`Average VSCI`=mean(IBI), `Number of samples`=n())%>%
      mutate(`Average VSCI` = as.numeric(format(`Average VSCI`, digits=3)), 
             `Season` ="6-year Spring Average")
    sixfall<-subset(annualdata, Season=="Fall" & Year>= endyear-6)%>%
      summarise(`Average VSCI`=mean(IBI), `Number of samples`=n())%>%
      mutate(`Average VSCI` = as.numeric(format(`Average VSCI`, digits=3)), 
             `Season` ="6-year Fall Average")
    FinalTable<-rbind(twospr, twofall, sixspr, sixfall,  six, two, total)%>%
      select(Season, `Average VSCI`, `Number of samples`)
  }
  
  datatable(FinalTable, rownames=F,class = 'cell-border stripe', options = list(pageLength= nrow(FinalTable), dom = 't',
                                                   initComplete = JS( 
                                                     "function(settings, json) {",
                                                     "$(this.api().table().header()).css({'background-color': '#636363', 'color': '#fff', 'font-size': '170%'});",
                                                     "}"))) %>%
    formatStyle(names(FinalTable), textAlign = 'center', `font-family` = 'Arial')%>%
    formatStyle(columns= c(1,2,3), fontSize = '150%')%>%
    formatStyle(
      4,
      target = "row",
      fontWeight = styleEqual(1, 'bold' ))

    #formatStyle( names(FinalTable [3, ]),target = 'row',
     # backgroundColor = styleEqual( FinalTable[3, ], c('gray')))
}


#Example of how to run function: 

#SumTable(annualdata)

#Make a plot function to plot yearly VSCI scores- stole code from stressor report code, this won't plot data but I'm not sure why not! 

bugplot<-function(annualdata) {
  if(method=="VCPMI"){
    ggplot(annualdata, aes(x=CollDate, y=IBI, fill=Season))+
      geom_col()+
      scale_fill_manual("Season", values = c("Fall" = "black", "Spring" = "dark grey"))+
      labs(x="Collection Year", y="VCPMI")+
      theme(axis.text=element_text(size=14, face="bold"), legend.text=element_text(size=14),
            legend.title = element_text(size=14, face="bold"),
            axis.title=element_text(size=14, face="bold")) +
      scale_y_continuous(name="VCPMI", breaks=seq(0, 100, 10),limits=c(0,100))+
      scale_x_datetime(date_breaks='1 year', date_labels =  "%Y")+
      geom_hline(yintercept=40, color="red", size=1)+
      theme(axis.text.x=element_text(angle=45,hjust=1))
  }else{
    ggplot(annualdata, aes(x=CollDate, y=IBI, fill=Season))+
      geom_col()+
      scale_fill_manual("Season", values = c("Fall" = "black", "Spring" = "dark grey"))+
      labs(x="Collection Year", y="VSCI")+
      theme(axis.text=element_text(size=14, face="bold"), legend.text=element_text(size=14),
            legend.title = element_text(size=14, face="bold"),
            axis.title=element_text(size=14, face="bold")) +
      scale_y_continuous(name="VSCI", breaks=seq(0, 100, 10),limits=c(0,100))+
      scale_x_datetime(date_breaks='1 year', date_labels =  "%Y")+
      geom_hline(yintercept=60, color="red", size=1)+
      theme(axis.text.x=element_text(angle=45,hjust=1))
  }
  
}

#bugplot(annualdata)

bugparms<- function(annualdata){
  # grab info we want from input dataframe
  if(method=='VCPMI'){
    tableData <-select(annualdata, Year, Season, IBI, TotTaxa, EPTTax, `%Ephem`, `%PT - Hydropsychidae`, `%Scrap`, `%Chiro`,
                       `%2Dom`, `HBI`)%>%
      rename(VCPMI=IBI)
    datatable(tableData, rownames=F, class = 'cell-border stripe',options =list(pageLength = nrow(tableData),
                                                                                 dom = 't',scrollX=TRUE))%>%
      formatRound(columns=c("VCPMI",'%Ephem', '%PT - Hydropsychidae','%Scrap', '%Chiro', 
                            '%2Dom', 'HBI'), digits=1) %>%
      formatStyle(names(tableData), textAlign = 'center', `font-family` = 'Arial')
      
  }else{
  tableData <-select(annualdata, Year, Season, IBI, TotTaxa, EPTTax, `%Ephem`, `%PT - Hydropsychidae`, `%Scrap`, `%Chiro`,
                                                                        `%2Dom`, `HBI`)%>%
    rename(VSCI=IBI)
  datatable(tableData, rownames=F, class = 'cell-border stripe',options =list(pageLength = nrow(tableData), dom = 't' ,scrollX=TRUE))%>%
    formatRound(columns=c("VSCI",'%Ephem', '%PT - Hydropsychidae','%Scrap', '%Chiro', 
                          '%2Dom', 'HBI'), digits=1) %>%
    formatStyle(names(tableData), textAlign = 'center', `font-family` = 'Arial')
  
  }
  #make table the way we want
  
}

#bugparms(annualdata)

# Total hab mess around
totalHabitat <- suppressWarnings(left_join(totalHab,totHabCon, by=c('HabSampID', 'Comments','EnterDate')))


rm(totHabCon); rm(totalHab)
# test manipulation zone



# plot seasonal/annual total hab scores


#function to select station and years from habitat query
selecthabitatStation<-function(UserStation, startyear,endyear){
  station<- filter(totalHabitat,StationID==UserStation)
  date1<-as.Date(paste(startyear, 01, 01, sep = "-"))
  date2<-as.Date(paste(endyear, 01, 01, sep = "-"))
  subset(station, CollDate>= date1 & CollDate<= date2)
}

hab<-selecthabitatStation(UserStation, startyear, endyear)

totHab <- hab %>%
  group_by(StationID, CollDate ) %>%
  mutate(TotalHabitat = sum(HabValue))




####totHabWide <- select(totHab, StationID, CollDate, `Total Habitat`, HabParameter, HabValue) %>%
 ## spread(HabParameter,HabValue)
# make table of high/low gradient variables:
# in BenthicStressorReportTemplate_VSCI.Rmd ctrl+F habData ~line 611


#totHabData <- select(totHab,StationID,CollDate,TotalHabitat)[!duplicated(select(totHab,StationID,CollDate,TotalHabitat)),]
HabitatDT <- function(totHab){
  totHab$CollDate<- format(totHab$CollDate, "%m-%d-%Y")
    poor<-seq(0,20, 1)
    poorclrs<-c('firebrick', 'firebrick','firebrick','firebrick','firebrick','firebrick', "#F0E442","#F0E442","#F0E442","#F0E442","#F0E442", 
                "#009E73","#009E73","#009E73","#009E73","#009E73", "#0072B2","#0072B2","#0072B2","#0072B2","#0072B2")
    if("POOLSUB" %in% unique(totHab$HabParameter) | 'POOLVAR' %in% unique(totHab$HabParameter)){ 
      # Low Gradient Habitat Method

      totHabData <- select(totHab,StationID,CollDate,TotalHabitat)[!duplicated(select(totHab,StationID,CollDate,TotalHabitat)),]# get unique sample dates
      if(!"SUBSTRATE" %in% names(totHab$HabParameter)){
        totHab$SUBSTRATE<-0 
      }
      habData1 <- select(totHab,-c(TotalHabitat, HabSampID, Comments, EnterDate,HSampIndex, CollTime,`Field Team`)) %>% # these two fields can cause multiple rows for each date in the spread() step, so get rid of them for now
        filter(HabParameter %in% c("ALTER","BANKS","BANKVEG","FLOW","POOLSUB","POOLVAR","RIPVEG","SEDIMENT","SINUOSITY",
                                   "SUBSTRATE","COVER")) %>% # get only low gradient parameters in case both happened to be sampled for site) and using filter is safer than select at a later step
        spread(HabParameter,HabValue) %>%
        filter(!is.na(POOLSUB) & !is.na(POOLVAR) & !is.na(SINUOSITY)) # if both methods used then get rid of sample dates not sampled as low gradient
      
      habData1<-habData1[ , c("StationID", "CollDate", "ALTER","BANKS","BANKVEG","FLOW","POOLSUB","POOLVAR","RIPVEG","SEDIMENT","SINUOSITY",
                              "SUBSTRATE","COVER")]
      # Deal with COVER terminology if present
      if("COVER" %in% names(habData1)){
        habData1 <- habData1 %>% rowwise()%>%
          mutate(SUBSTRATE=sum(COVER,SUBSTRATE,na.rm=T)) %>% select(-c(COVER)) %>% # Get rid of old cover terminology
          left_join(totHabData, by=c("StationID","CollDate")) %>%
          dplyr::rename(TotalHabitat = TotalHabitat) %>%
          arrange(CollDate)
        habData1<-habData1[ , c("StationID", "CollDate", "ALTER","BANKS","BANKVEG","FLOW","POOLSUB","POOLVAR","RIPVEG","SEDIMENT","SINUOSITY",
                                "SUBSTRATE","TotalHabitat")]
      }else{
        habData1 <- habData1 %>% rowwise()%>%
          left_join(totHabData, by=c("StationID","CollDate")) %>%
          dplyr::arrange(CollDate)
        habData1<-habData1[ , c("StationID", "CollDate", "ALTER","BANKS","BANKVEG","FLOW","POOLSUB","POOLVAR","RIPVEG","SEDIMENT","SINUOSITY",
                                "SUBSTRATE","TotalHabitat")]
      }
      DT::datatable(habData1,escape=F,rownames = F, class = 'cell-border stripe',
 
                    colnames = c('Station ID','Date','Channel Alteration','Bank Stability','Bank Vegetation', 
                                 'Flow', 'Pool Substrate','Pool Variability', 'Riparian Vegetation', 
                                 'Sediment', 'Sinuosity', 'Substrate', 'Total Habitat'),
                    options=list(pageLength=nrow(habData1),dom= 'Bt', scrollX=TRUE, 
                                 initComplete = JS( 
                                   "function(settings, json) {",
                                   "$(this.api().table().header()).css({'background-color': '#636363', 'color': '#fff', 'font-size': '100%'});",
                                   "}"))) %>%
        formatStyle(columns= c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15), fontSize = '100%') %>%
        formatStyle(names(habData1)[3:12], textAlign = 'center', `font-family` = 'Arial') %>%
        formatStyle(names(habData1)[3:12], backgroundColor = styleEqual(poor, poorclrs), alpha=1, textAlign = 'center', `font-family` = 'Arial') %>% 
        formatStyle(names(habData1), fontWeight = styleInterval(10, c('bold','normal'))) %>% 
        # Make table look like this:
        #formatStyle(names(habData1)[3:12], backgroundColor = styleEqual(10, c('gray', 'yellow')), 
        #            textAlign = 'center', `font-family` = 'Arial') %>%
        formatStyle('TotalHabitat', backgroundColor = "lightgray")
    }
    

      totHabData <- select(totHab,StationID,CollDate,TotalHabitat)[!duplicated(select(totHab,StationID,CollDate,TotalHabitat)),] # get unique sample dates
      if(!"SUBSTRATE" %in% names(totHab)){
        totHab$SUBSTRATE<-0 
      }
      habData1 <- select(totHab,-c(TotalHabitat,HabSampID,Comments, EnterDate,HSampIndex, CollTime,`Field Team`)) %>% # these two fields can cause multiple rows for each date in the spread() step, so get rid of them for now
        filter(HabParameter %in% c('ALTER','BANKS','BANKVEG','COVER','EMBED','FLOW','RIFFLES','RIPVEG','SEDIMENT',
                                   'SUBSTRATE','VELOCITY')) %>% # get only high gradient parameters in case both happened to be sampled for site) and using filter is safer than select at a later step
        spread(HabParameter,HabValue) %>%
        filter(!is.na(EMBED) & !is.na(RIFFLES) & !is.na(VELOCITY)) # if both methods used then get rid of sample dates not sampled as high gradient
      
      # Deal with COVER terminology if present
      if("COVER" %in% names(habData1)){
        habData1 <- habData1 %>% rowwise() %>%
          mutate(SUBSTRATE=sum(COVER,SUBSTRATE,na.rm=T)) %>% select(-c(COVER)) %>% # Get rid of old cover terminology
          left_join(totHabData, by=c("StationID","CollDate")) %>%
          dplyr::arrange(CollDate)
        habData1<-habData1[ , c("StationID", "CollDate", "ALTER","BANKS","BANKVEG", "EMBED", "FLOW","RIFFLES","RIPVEG","SEDIMENT","SUBSTRATE",
                                "VELOCITY","TotalHabitat")]
      }else{
        habData1 <- habData1 %>% rowwise()%>%
          left_join(totHabData, by=c("StationID","CollDate")) %>%
          dplyr::arrange(CollDate)
        habData1<-habData1[ , c("StationID", "CollDate", "ALTER","BANKS","BANKVEG", "EMBED", "FLOW","RIFFLES","RIPVEG","SEDIMENT","SUBSTRATE",
                                "VELOCITY","TotalHabitat")]
      }
      DT::datatable(habData1,escape=F, rownames = F, class = 'cell-border stripe', colnames = c('Station ID','Date','Channel Alteration','Banks', 
                                                                  'Bank Vegetation', 'Embeddedness', 
                                                                  'Flow', 'Riffles', 'Riparian Vegetation', 
                                                                  'Sediment', 'Substrate','Velocity', 'Total Habitat'),
                    options=list(pageLength=nrow(habData1),dom= 'Bt', scrollX=TRUE,
                                 initComplete = JS( 
                                   "function(settings, json) {",
                                   "$(this.api().table().header()).css({'background-color': '#636363', 'color': '#fff', 'font-size': '100%'});",
                                   "}"))) %>%
        formatStyle(columns = colnames(.), fontSize = '100%')%>%
        formatStyle(names(habData1),  textAlign = 'center', `font-family` = 'Arial') %>%
        formatStyle(names(habData1)[3:12],  backgroundColor = styleEqual(poor, poorclrs), alpha=0.1,
                    textAlign = 'center', `font-family` = 'Arial') %>% 
        formatStyle(names(habData1), fontWeight = styleInterval(10, c('bold','normal'))) %>% 
        formatStyle('TotalHabitat', backgroundColor = "lightgray")
  
}





Test <-function(totHab){
  if (nrow(totHab)== 0){
    print("No Habitat Data")
    
  }else{
    HabitatDT(totHab)
  }
}

# How to Run Function:
#HabitatDT(totHab)



habitatplot<-function(totHabData) {
  # force time on exact day of collection to fix POSIXct subtracting a day
  totHabData$CollDate <- as.POSIXct(paste(totHabData$CollDate,' 01:00:00', sep=''),format="%Y-%m-%d %H:%M:%S", tz="US/Eastern")
  
  # add min and max dates to make rectagle plotting easier, starting at 6 month buffer by can play with
  minDate <- min(totHabData$CollDate) - months(6)
  maxDate <- max(totHabData$CollDate) + months(6)
  
  ggplot(totHabData, aes(x=CollDate , y=TotalHabitat))+

    labs(x="Collection Year", y="Total Habitat")+
    theme(axis.text=element_text(size=14, face="bold"), legend.text=element_text(size=14),
          legend.title = element_text(size=14, face="bold"),
          axis.title=element_text(size=14, face="bold")) +
    scale_y_continuous(name="Total Habitat (unitless)", breaks=seq(0, 200, 25),limits=c(0,200)) +
    scale_x_datetime(date_breaks='1 year', date_labels =  "%Y")+
    theme(axis.text.x=element_text(angle=45,hjust=1))+
    annotate("rect", xmin=minDate, xmax=maxDate, ymin=150, ymax=Inf, alpha=1, fill="#0072B2") +
    annotate("rect",xmin=minDate, xmax=maxDate, ymin=130, ymax=150, alpha=1, fill="#009E73" ) +
    annotate("rect",xmin=minDate, xmax=maxDate, ymin=100, ymax=130, alpha=1, fill="#F0E442") +
    annotate("rect",xmin=minDate, xmax=maxDate, ymin=-Inf, ymax=100, alpha=1, fill="firebrick" )+
    geom_bar(stat="identity")
    
}





TestPlot <-function(totHab){
  if (nrow(totHab)== 0){
    print("No Habitat Data")
    
  }else{
    habitatplot(totHab)
  }
}

# How to Run Function:
#habitatplot(totHab)
