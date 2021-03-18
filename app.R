#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#develop code by Tanaphum Wichaita


library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Randomization list"),


        # Show a plot of the generated distribution
        mainPanel(
            fileInput("datafile", "Choose CSV File",
                      accept = c(
                          "text/csv",
                          "text/comma-separated-values,text/plain",
                          ".csv")
            ),
            downloadButton('downloadData', 'Download'),
            h2("Output"),
            tableOutput('table')
        )
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    data<-reactive({
        if (is.null(input$datafile))
            return(NULL)                
        read.csv(input$datafile$datapath)

    })
    
    output$downloadData <- downloadHandler(filename = function() {paste0("Listed - ",basename(input$datafile$name)) }
                                           ,content = function(file) {write.csv(table_out(), file, row.names = FALSE)})
    
    table_out <- reactive({
        req (!is.null(input$datafile))
        
        data <- data()
        week=2 # Week ##
        cut_raw=numeric(week)
        cut=numeric(week)
        for (i in 1:2 )# Week #
        {
            di2=data[which(data[,2]==i),]
            cut_raw[i]=round((mean(di2[,5])))
            if (cut_raw[i]<=10) {
                cut[i]=round(0.5*cut_raw[i])
            } else {
                cut[i]=10
            }
        }
        nd=cut[2]
        ran_day=sample(seq(1,4,1),1,replace=F)
        ran_matrix=matrix(0,ran_day,nd)# No of working days for next week and nd= No-of patients ( no-of patients should be greater than 10)
        
        for (i in ran_day:ran_day) # no-of working days in the next week
        {
            
            
            ran_matrix[i,]= sort(sample(seq(1,cut_raw[week],1),nd,replace=F))
            ran_matrix[i,]=sapply(ran_matrix[i,],function(x)paste("OPD_DOC",x,sep="_"))
        }
        day=c("Monday", "Tuesday","Wednesday","Thursday","Friday")
        t(ran_matrix)
        out=cbind(rep(day[ran_day],nd),ran_matrix[ran_day,])
        
        colnames(out) <- c("Random Day","Patients")
        out
    })
    
    
    output$table <- renderTable(table_out(),bordered =T)
}

# Run the application 
shinyApp(ui = ui, server = server)
