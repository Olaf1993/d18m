#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(RSQLite)
library(shiny)
library(shinyjs)
library(ggplot2)
library(shinydashboard)

sqlitePath <- "db/diesel.db"
table <- "tbl1"

# Define the fields we want to save from the form
fields <- c("Datum", "Liter", "Preis","DeutschlandwPproL", "Km")

# Shiny app with 3 fields that the user can submit data for
shinyApp(
  ui = fluidPage(
    
    titlePanel("d18m"),
    
    
    
    # without Shinyjs reset() does not work...
    useShinyjs(),
    
    
    navbarPage("Menu",
               tabPanel("Eingabe",
                        mainPanel("Willkommen.",
                                  
                                  tags$b("Gib hier nach jedem Tanken die Daten ein!"),
                                  
                                  tags$body("Wann hast du getankt? Wie viel hast du getankt? Was hast du bezahlt? Wie war zu diesem Zeitpunkt der deutschlandweite Durchschnittspreis pro Liter Diesel? Wie ist der aktuelle Kilometerstand des Autos?"),
                                  
                                  #textInput("Datum", "Datum", ""),
                                  dateInput("Datum","Datum",Sys.Date(),"2000-01-01",Sys.Date(),format = "dd.mm.yyyy",NULL, weekstart = 0, language = "de", width= NULL ),
                                  textInput("Liter","Liter",""),
                                  #checkboxInput("lit", "I've built a Shiny app in R before", FALSE),
                                  textInput("Preis", "Gesamtpreis", ""),
                                  textInput("DeutschlandwPproL","Deutschlandweiter Preis pro Liter","0"),
                                  textInput("Km","Kilometerstand", ""),
                                  actionButton("submit", "Speichern")
                                  
                                  
                        )    
               ) ,
               
               tabPanel("Datenbank",
                        
                        tags$b("Meine Tankdatenbank."),
                        tags$body("Hier werden alle Einträge gespeichert."),
                        tabsetPanel(
                          tabPanel("Tankdaten",
                                 
                                   DT::dataTableOutput("responses", width = 500), tags$hr()
                          )
                        )
               ),
               tabPanel("Charts",
                        
                        
                        
                        #checkboxGroupInput("checkGroup",label=h3("Checkbox group"),choices= list("Preis pro Liter"="dbplot","Preis pro Liter deutschlandweit"="dbplot2"),selected=1),
                        #hr(),
                        #fluidRow(column(3,verbatimTextOutput("value"))),
                        
                        # plotOutput("dbplot",
                        #            click = "plot_click",
                        #            dblclick = "plot_dbclick",
                        #            hover = "plot_hover",
                        #            brush = "plot_brush"
                        #  ),
                        # verbatimTextOutput("info3")
                        #     
                        tabsetPanel(
                          
                          tabPanel("Preis pro Liter",
                                   
                                   tags$b("Was ich beim Tanken für einen liter Diesel bezahlt habe:" ),
                                   plotOutput("dbplot" ,
                                              click = "plot_click",
                                              dblclick = "plot_dbclick",
                                              hover = "plot_hover",
                                              brush = "plot_brush"
                                   ),
                                   verbatimTextOutput("info")
                                   
                                   
                          ),
                          tabPanel("Preis pro Liter deutschlandweit",
                                   
                                  
                                   tags$b("Was war der deutschlandweite Durschschnitspreis pro Liter Diesel?" ),
                                   
                                   plotOutput("dbplot2" ,
                                              click = "plot_click",
                                              dblclick = "plot_dbclick",
                                              hover = "plot_hover",
                                              brush = "plot_brush"
                                   ),
                                   verbatimTextOutput("info2")
                          ),
                          
                          tabPanel("Vergleich",
                                   
                                  
                                   tags$b("Deutschlandweiter Durchschnittspreis(rot) und mein getankter Preis(blau) im Vergleich." ),
                                   plotOutput("dbplot3" ,
                                              click = "plot_click",
                                              dblclick = "plot_dbclick",
                                              hover = "plot_hover",
                                              brush = "plot_brush"
                                   ),
                                   verbatimTextOutput("info3")
                          ),
                          tabPanel("Differenz ",
                                   
                                   tags$b(" Wann war ich über/unter dem deutschlandweiten Preis?" ),
                                   
                                   tags$body("Das Schaubild zeigt die Differenz Marktpreis MINUS mein Preis. Sind die Punkte unter der roten Null-Linie, so habe ich unter dem deutschlandweiten Durchschnittspreis getankt."),
                                   
                                   plotOutput("differenz" ,
                                              click = "plot_click",
                                              dblclick = "plot_dbclick",
                                              hover = "plot_hover",
                                              brush = "plot_brush"
                                   ),
                                   "Was hätte ich gespart, wenn ich immer den deutschlandweiten Preis gezahlt hätte?",
                                   verbatimTextOutput("differenztext")
                          ),
                          tabPanel("Boxplot",
                                   
                                  
                                   tags$b("Boxplot: Mein persönlicher Preis pro Liter und der deutschlandweite Durchschnittspreis:"),
                                   plotOutput("boxplot"
                                              )
                                 # verbatimTextOutput("infos_boxplot")
                                  
                          )
                        ),
                          fluidRow(
                          infoBox("Info", , icon = icon("credit-card"), fill=TRUE),
                          infoBoxOutput("infobox1"),
                          infoBoxOutput("gesamtpreis")
                          )
                        
                        
                        
               ),
               
               tabPanel("Verbrauch",
                        tags$b("Wie ist meine Fahrweise?"),
                        tags$body("Verbrauch in Liter pro 100 Kilometer in Abhängigkeit zum Gesamtkilometerstand des Autos."),
                        
                        tabsetPanel(
                          tabPanel("Verlauf",
                            plotOutput("verbrauchplot")
                            ),
                          tabPanel("Boxplot",
                            plotOutput("boxplot_verbrauch")
                            )
                        )   
               ),
               
               tabPanel("weitere Kosten",
                        
                        tags$body("Alle Kosten, die das Auto verursacht im Verhältnis. Einmal mit und einmal ohne Dieselkosten."),
                        tags$b("Was kostet mich am meisten?"),
                        
                        tabsetPanel(
                          tabPanel("Diagramm",
                                   plotOutput("kostenkreisdiagramm"),
                                   plotOutput("kostenkreisdiagrammdiesel")
                                   )
                          
                        )
                ),
               navbarMenu("More",
                          tabPanel("About",
                                   helpText("2018 by hitbear"),
                                   HTML("Si tacuisses, philosophus mansisses.")
                          )   
                          
               )
               
               
    )
    
    
    
  ),
  server = function(input, output, session) {
    
    # Connect to the database
    db <- dbConnect(SQLite(), sqlitePath)
    # Construct the fetching query
    query <- paste0("SELECT * FROM tbl1")
    # Submit the fetch query and disconnect
    data <- dbGetQuery(db, query)
    
    query2 <- paste0("SELECT * FROM tbl2")
    data_tbl2 <- dbGetQuery(db, query2)
    
    dbDisconnect(db)
    
    
    output$differenztext <- renderText({
      
      
     a <-  sum(data$Preis)-sum(data$DeutschlandwPproL * data$Liter) 
     if (a < 0){
       b <- -1*a
      paste ("Nichts, ich hätte soviel mehr bezahlt:", b)
     }
     else 
     {
       paste("Einsparung",a)
     }
     
    })
    
    output$dbplot <- renderPlot({
      data2 <- data[,1]
      #plot(as.Date(as.numeric(data[,1]),"1970-01-01"),data$Preis / data$Liter)
      d <- data.frame(Datum =as.Date(data$Datum, origin = "1970-01-01"), Preis = (data$Preis / data$Liter))
      ggplot(d,aes(x=Datum)) + geom_point(aes(y=Preis)) + geom_smooth(aes(y = Preis)) 
     
    })
    
    output$dbplot2 <- renderPlot({
      #data2 <- data[,1]
      #plot(as.Date(as.numeric(data[,1]),"1970-01-01"),data$Preis / data$Liter)
      d <- data.frame(Datum =as.Date(data$Datum, origin = "1970-01-01"), Preis = data$DeutschlandwPproL)
      ggplot(d,aes(x=Datum, y= Preis )) + geom_smooth(aes(y = Preis)) + geom_point() 
    })
    
    output$dbplot3 <- renderPlot({
      #data2 <- data[,1]
      #plot(as.Date(as.numeric(data[,1]),"1970-01-01"),data$Preis / data$Liter)
      d <- data.frame(Datum =as.Date(data$Datum, origin = "1970-01-01"), Preis = (data$Preis / data$Liter))
      ggplot(d,aes(x=Datum)) + geom_line(aes(y= Preis),color= "blue") + geom_smooth(aes(y = data$DeutschlandwPproL),color="red") 
    })   
    
    output$boxplot <- renderPlot({
      #data2 <- data[,1]
      d1 <- data.frame(X="Preis pro Liter", Preis = (data$Preis / data$Liter))
      d2 <- data.frame(X="deutschlandweit", Preis = data$DeutschlandwPproL)
      d <- rbind(d1,d2)
      ggplot(d, aes(x = X)) + geom_boxplot(aes(y= Preis, fill = X)) 

    })
    
    output$differenz <- renderPlot({
      d <- data.frame(Datum =as.Date(data$Datum, origin = "1970-01-01"), Differenz =(data$Preis / data$Liter) - data$DeutschlandwPproL)
      ggplot(d,aes(x=Datum)) + geom_point(aes(y= Differenz),color= "blue") + geom_smooth(aes(y=Differenz),color="green") + geom_line(y=0, color ="red")
      
    })
    
    
    output$infobox1 <- renderInfoBox({
      infoBox(
        "Mittelwert", paste0(mean(data$Preis/data$Liter),"€"), 
        color = "yellow", fill = TRUE
      )
   })
    output$gesamtpreis <- renderInfoBox({
      infoBox(
        "Gesamtkosten", paste0(sum(data$Preis),"€"),
        color = "purple", fill = TRUE
      )
    })
    
    output$verbrauchplot <- renderPlot({
      v <- NULL
      for ( i in c(1:length(data$Liter))){
        v <- append(v,100*sum(data$Liter[1:i])/data$Km[i])
      }
      d <- data.frame(Kilometer = data$Km, Verbrauch= v)
      ggplot(d,aes(x = Kilometer)) + geom_smooth(aes(y = Verbrauch),color="yellow") + geom_point(aes(y = Verbrauch),color="red") 
    })
    
    output$boxplot_verbrauch <- renderPlot({
      v <- NULL
      for ( i in c(1:length(data$Liter))){
        v <- append(v,100*sum(data$Liter[1:i])/data$Km[i])
      }
      d <- data.frame(X = "Verbrauch", Verbrauch = v)
      ggplot(d, aes(x = X)) + geom_boxplot(aes(y= Verbrauch, fill = X)) 
    })
    
    output$kostenkreisdiagramm <- renderPlot({
      kost <- data_tbl2$Art
      eur <- data_tbl2$Preis
      pie(eur,kost,main="Kosten")
    })
    
    output$kostenkreisdiagrammdiesel <- renderPlot({
      kost <- data_tbl2$Art
      eur <- data_tbl2$Preis
      kost <- append(kost,"Diesel")
      eur <- append(eur,sum(data$Preis))
      pie(eur,kost,main="Kosten inklusive Tankkosten")
    })
    
    
    # output$info <- renderText({
    #   xy_str <- function(e){
    #     if(is.null(e)) return ("NULL\n")
    #     paste0 ("x=", round(e$x,1), "y=", round(e$y,1),"\n")
    #   }
    #   xy_range_str <- function(e){
    #     if (is.null(e)) return ("NULL\n")
    #     paste0("xmin=", round(e$xmin,1), "xmax=", round(e$xmax,1),
    #            "ymin=", round(e$ymin,1), " ymax=", round(e$ymax,1))
    #   }
    #   
    #   paste0(
    #     "click: ", xy_str(input$plot_click),
    #     "dblclick: ", xy_str(input$plot_dblclick),
    #     "hover: ", xy_str(input$plot_hover),
    #     "brush: ", xy_str(input$plot_brush)
    #   )
    # })
    
    output$infos_boxplot <- renderText(
      median((data$Preis / data$Liter))
    )
     
    
    
    # Whenever a field is filled, aggregate all form data
    formData <- reactive({
      #data <- loadData()
      data <- sapply(fields, function(x) input[[x]])
      data
    })
    
    # When the Submit button is clicked, save the form data
    observeEvent(input$submit, {
      saveData(formData())
      
      db <- dbConnect(SQLite(), sqlitePath)
      # Construct the fetching query
      query <- paste0("SELECT * FROM tbl1")
      # Submit the fetch query and disconnect
      data <- dbGetQuery(db, query)
      
      query2 <- paste0("SELECT * FROM tbl2")
      data_tbl2 <- dbGetQuery(db, query2)
      
      dbDisconnect(db)
      
      
      
      output$dbplot <- renderPlot({
        data2 <- data[,1]
        #plot(as.Date(as.numeric(data[,1]),"1970-01-01"),data$Preis / data$Liter)
        d <- data.frame(Datum = data$Datum, Preis = (data$Preis / data$Liter))
        ggplot(d,aes(x=as.Date(data$Datum, origin = "1970-01-01"), y= Preis)) + geom_smooth(aes(y = Preis)) + geom_point()
        #ggplot(d,aes(x=Datum)) + geom_point(aes(y=Preis)) + geom_smooth(aes(y = Preis))
        
      })
      
      output$dbplot2 <- renderPlot({
        data2 <- data[,1]
        #plot(as.Date(as.numeric(data[,1]),"1970-01-01"),data$Preis / data$Liter)
        d <- data.frame(Datum = data$Datum, Preis = data$DeutschlandwPproL)
        ggplot(d,aes(x=as.Date(data$Datum, origin = "1970-01-01"), y= Preis)) + geom_smooth(aes(y = Preis)) + geom_point()
        
      })
      
      output$dbplot3 <- renderPlot({
        data2 <- data[,1]
        #plot(as.Date(as.numeric(data[,1]),"1970-01-01"),data$Preis / data$Liter)
        d <- data.frame(Datum =as.Date(data$Datum, origin = "1970-01-01"), Preis = (data$Preis / data$Liter))
        ggplot(d,aes(x=Datum)) + geom_line(aes(y= Preis),color= "blue") + geom_line(aes(y = data$DeutschlandwPproL),color="red")
      })  
      
      output$boxplot <- renderPlot({
        data2 <- data[,1]
        d1 <- data.frame(X="Preis pro Liter", Preis = (data$Preis / data$Liter))
        d2 <- data.frame(X="deutschlandweit", Preis = data$DeutschlandwPproL)
        d <- rbind(d1,d2)
        ggplot(d, aes(x = X)) + geom_boxplot(aes(y= Preis, fill = X)) 
        
      })
      
      output$differenz <- renderPlot({
        d <- data.frame(Datum =as.Date(data$Datum, origin = "1970-01-01"), Differenz =(data$Preis / data$Liter) - data$DeutschlandwPproL)
        ggplot(d,aes(x=Datum)) + geom_point(aes(y= Differenz),color= "blue") + geom_smooth(aes(y=Differenz),color="green") + geom_line(y=0, color ="red")
        
      })
      
      output$infobox1 <- renderInfoBox({
        infoBox(
          "Mittelwert", paste0(mean(data$Preis/data$Liter),"€"), 
          color = "yellow", fill = TRUE
        )
      })
      output$gesamtpreis <- renderInfoBox({
        infoBox(
          "Gesamtkosten", paste0(sum(data$Preis),"€"),
          color = "purple", fill = TRUE
        )
      })
      
      
      output$verbrauchplot <- renderPlot({
        v <- NULL
        for ( i in c(1:length(data$Liter))){
          v <- append(v,100*sum(data$Liter[1:i])/data$Km[i])
        }
        d <- data.frame(Kilometer = data$Km, Verbrauch= v)
       # ggplot(d,aes(x = Kilometer)) + geom_line(aes(y = Verbrauch),color="blue") + geom_point(aes(y = Verbrauch),color="red")
        ggplot(d,aes(x = Kilometer)) + geom_smooth(aes(y = Verbrauch),color="yellow") + geom_point(aes(y = Verbrauch),color="red") 
      })
      
      output$boxplot_verbrauch <- renderPlot({
        v <- NULL
        for ( i in c(1:length(data$Liter))){
          v <- append(v,100*sum(data$Liter[1:i])/data$Km[i])
        }
        d <- data.frame(X = "Verbrauch", Verbrauch = v)
        ggplot(d, aes(x = X)) + geom_boxplot(aes(y= Verbrauch, fill = X)) 
      })
      
      output$kostenkreisdiagramm <- renderPlot({
        kost <- data_tbl2$Art
        eur <- data_tbl2$Preis
        pie(eur,kost,main="Kosten")
      })
      
      output$kostenkreisdiagrammdiesel <- renderPlot({
        kost <- data_tbl2$Art
        eur <- data_tbl2$Preis
        kost <- append(kost,"Diesel")
        eur <- append(eur,sum(data$Preis))
        pie(eur,kost,main="Kosten inklusive Tankkosten")
      })
      
      #set fields to defaul values
      reset("Datum")
      reset("Liter")
      reset("Preis")
      reset("DeutschlandwPproL")
      
    })
    
    # Show the previous responses
    # (update with current response when Submit is clicked)
    output$responses <- DT::renderDataTable({
      input$submit
      loadData()
    }) 
    


    
    saveData <- function(data) {
      # Connect to the database
      db <- dbConnect(SQLite(), sqlitePath)
      # Construct the update query by looping over the data fields
      query <- sprintf(
        "INSERT INTO %s (%s) VALUES ('%s')",
        table, 
        paste(names(data), collapse = ", "),
        paste(data, collapse = "', '")
      )
      # Submit the update query and disconnect
      dbGetQuery(db, query)
      dbDisconnect(db)
    }
    
    loadData <- function() {
      # Connect to the database
      db <- dbConnect(SQLite(), sqlitePath)
      # Construct the fetching query
      query <- sprintf("SELECT * FROM %s", table)
      # Submit the fetch query and disconnect
      data <- dbGetQuery(db, query)
      dbDisconnect(db)
      data
    }
    
    
    
  }
)


# Run the application 
#shinyApp(ui = ui, server = serve