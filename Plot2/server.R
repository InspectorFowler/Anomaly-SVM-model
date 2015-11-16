## Shiny server file
library(ggplot2)
library(muStat)
library(scales)
library(grid)

## Change this path to replicate the dashboard
path="C:/Users/nemecys/Downloads/Data Analytics.csv"
data<-read.csv(path)
data$Period<-as.Date(data$Period,format="%m/%d/%y")

## Converting period column to just months and some simple manipulations
data$Period <- factor(format(data$Period, "%B"),levels = month.name)
data<-data[,c(1,3,5,6,7,13)]
data$Amount=gsub(",","",data$Amount)
data$Amount=as.numeric(as.character(data$Amount))
data<-data[-c(which.na(data$Amount)),]
data$Period<-as.character(data$Period)
data$level=ifelse(data$Amount>=0,"Pos","Neg")

## SUbset data into +/- amount values for stacked representation
data.pos<-subset(data,Amount>=0)
data.neg<-subset(data,Amount<0)

## server function
shinyServer(function(input, output) {
  
  datacr<-reactive({
    if (input$selectmonth=="Total"){
      datact<-rbind(data.neg[(data.neg$Account.Description==input$selectAcc & 
                                data.neg$Category==input$selectCat),],
                    data.pos[(data.pos$Account.Description==input$selectAcc & 
                                data.pos$Category==input$selectCat),])
    }
    else {
      datact<-rbind(data.neg[(data.neg$Account.Description==input$selectAcc &
                                data.neg$Category==input$selectCat &
                                data.neg$Period==input$selectmonth),],
                    data.pos[(data.pos$Account.Description==input$selectAcc & 
                                data.pos$Category==input$selectCat & 
                                data.pos$Period==input$selectmonth),])
                                        
    }
  })
  
  output$plot<-renderPlot({
    datac<-datacr()
    if (nrow(datac)==0){
      ggplot(datac,aes(x=Location.Description,y=Amount))+geom_blank()+
        geom_text(aes(1,10000,label="No Data Available"))+
        theme(axis.text.x=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks=element_blank(),
              axis.title.x=element_blank(),
              axis.title.y=element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank())
    }
    else{
      ggplot(datac, aes(Location.Description, abs(Amount)),width=2)+
        ylab("Absolute Total Transaction")+
        xlab("Location")+
        geom_bar(aes(fill = level),position = "dodge", stat="identity")+
        scale_y_continuous(labels=comma,breaks=round(seq(min(abs(datac$Amount)),max(abs(datac$Amount)),
                                                         by=(max(abs(datac$Amount))-min(abs(datac$Amount)))/10)))
    }
  })
})