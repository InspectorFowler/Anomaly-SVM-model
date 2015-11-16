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
  
  data1r<-reactive({
    if (input$select=="Total"){
      data1t<-data.pos
    }
    else{
      data1t<-data.pos[data.pos$Period==input$select,]
    }
  })
  data2r<-reactive({
    if (input$select=="Total"){
      data2t<-data.neg
    }
    else{
      data2t<-data.neg[data.neg$Period==input$select,]
    }
  })
  datacr<-reactive({
    if (input$select=="Total"){
      datact<-rbind(data.neg,data.pos)
    }
    else{
      datact<-rbind(data.neg[data.neg$Period==input$select,],
                    data.pos[data.pos$Period==input$select,])
    }
  })
  
  output$plot<-renderPlot({
    if(input$stack){
      data1<-data1r()
      data2<-data2r()
      ggplot()+xlab("Location")+ylab("Total Transactions")+
        geom_bar(data = data1, aes(x=Location.Description, y=Amount, fill=level),stat = "identity")+
        geom_bar(data = data2, aes(x=Location.Description, y=Amount, fill=level),stat = "identity")+
        scale_fill_manual(values=c("lightgreen","red1"))+
        scale_y_continuous(name="Total Transactions", labels = comma,
                           breaks=round(seq(-max(sum(data1$Amount),abs(sum(data2$Amount))),
                                            +max(sum(data1$Amount),abs(sum(data2$Amount))),
                                            by=max(sum(data1$Amount),abs(sum(data2$Amount)))/10)))
    }
    else{
      datac<-datacr()
      ggplot(datac, aes(Location.Description, abs(Amount)))+
        ylab("Absolute Total Transaction")+
        xlab("Location")+
        geom_bar(aes(fill = level),position = "dodge", stat="identity")+
        scale_y_continuous(labels=comma,breaks=round(seq(min(abs(datac$Amount)),max(abs(datac$Amount)),
                                                         by=(max(abs(datac$Amount))-min(abs(datac$Amount)))/10)))
    }
    
    })
})