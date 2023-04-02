function(input,output,session){
  
  firstLoad <- reactiveVal(TRUE)
  
  #create ui output main_paload a reactive UI depending on conditions
  output$main_page <- renderUI({
    if(firstLoad()) {
      uiOutput("disclaimer")
    }
    else {
      uiOutput("standard")
    }
    
  })
  
  #create each UI
  output$disclaimer <- renderUI({
    tagList(
      tags$link(rel = "stylesheet", type = "text/main/css", href = "css/main.css"),
      wellPanel(div(style = "text-align: center;",
                    h4(HTML("If this is your first time visiting, please read <b>'About'</b> before proceeding.
                            If you have visited before, thanks for returning!"))),
                br(),
                br(),
                div(style = "text-align: center;",
                    actionButton("action_ID", h4(HTML("Continue to <b> Australian heartworm transmission dashboard </b>")),
                                 width = "100%"))
      ),
      fluidRow(column(12, style = "margin-bottom: 5px;",
                      wellPanel(HTML("<b> About </b>"),
                                h4(HTML("Canine heartworm disease is a mosquito-transmitted disease
                                                      affecting domestic dogs, wild canids, cats and very occasionally people. <br> <br>")),
                                HTML("<i> <b> Transmission suitability </i> </b> <br>
The disease relies on mosquitoes for transmission and completion of its lifecycle. If a mosquito feeds on an infected dog, it may ingest larvae that are circulating in the dog’s bloodstream. For the larvae and mosquito to become infective to other dogs, the larvae must develop and move into the mosquito mouth-parts. This process will only occur when the temperature is above 14°C, and the rate of development increases as temperature increases from this threshold. For development to complete, the larvae must experience 130 degree-days, which are accumulated at a rate of 1 degree-day per day above the threshold.
Additionally, the mosquito vector lives for a maximum of 30 days. Therefore, for the larvae to become infective they need to experience their 130 degree-days within a 30 day window. If these constraints are not met, transmission is not possible.
If transmission does occur, the implanted larvae migrate through the dog and develop into adult heartworms. This process takes 6 months. Monthly or long-acting medications can be administered that target the migrating larvae to prevent development into adults, and possible disease. <br> <br>

<i> <b> Dashboard information </i> </b> <br>
 This dashboard combines nationally collected weather data with the temperature requirement of heartworm larval transmission to show regions where and when development within the mosquito can be completed. It also shows regions where development cannot be completed. Maps on this dashboard are colour-coded. Blue zones indicate regions where development would not have been completed within the preceding 30 days. Red zones show where development would have been possible. Orange zones show a ‘shoulder’ season, and if there is more warm weather in following days in these zones, development may be able to complete.
This dashboard is not designed to show exactly where transmission events may occur. It shows regions where temperature is not sufficient to allow development within the mosquito, and where preventative medications are unlikely to be necessary.
"
                                )))))
    
  })
  
  output$standard <- renderUI({
    tagList(
      tags$link(
        rel = "stylesheet",
        type = "text/css",
        href = "css/main.css"
      ),
      
      fluidRow(column(
        12, style = "margin-bottom: 5px;",
        wellPanel(style = "text-align: center;",
                  h4(
                    HTML(
                      "If this is your first time visiting, please read <b>'About'</b> before proceeding.
                            If you have visited before, thanks for returning!"
                    )
                  ))
      )),
      fluidRow(
        class = "css/flexbox",
        style = "height: 800px",
        column(9, 
               wellPanel(style = "height: 800px",
                         h3(
                           textOutput("selecteddatemap"),
                           br(),
                           dateInput(
                             "dates",
                             label = NULL,
                             value = (Sys.Date() - 2),
                             min = min(dseq),
                             max = max(dseq)
                           ),
                           shinycssloaders::withSpinner(
                             leafletOutput("leaflet_chdu", height = 600, width ="100%"),
                             color = getOption("spinner.color", default = "darkgrey")
                           ))
               )),
        column(3, 
               wellPanel(style = "height: 800px",
                         h4(strong(
                           'What is happening in the capital cities?'
                         )), tableOutput("capital.cities")
               ))
      ),
      fluidRow(column(
        12,
        wellPanel((h3("Location")),
                  #if slow loading times, consider making this in server
                  selectizeInput(
                    "postcode",
                    "Enter your postcode:",
                    choices = poa.list,
                    selected = "5371"
                  ),
                  textOutput("postcode"),
                  div(class = "output-container",
                      shinycssloaders::withSpinner(plotOutput("locationplot"),
                                                   color = getOption("spinner.color", default = "darkgrey")))
        )
      ),
      fluidRow(
        div(
          style = "margin-left: 15px; margin-right: 15px",
          column(6,
                 wellPanel(p(
                   strong(paste(
                     as.Date((Sys.Date() - 2), format = "%d-%m-%Y"), "'s", " status:", sep =
                       ""
                   )),
                   textOutput(("dailystatus"))
                 )),
                 wellPanel(dataTableOutput("cutofftable"))),
          column(6,
                 wellPanel(dataTableOutput(
                   "percentagetable"
                 )))
        )
      ),
      fluidRow(style = "margin-left: 15px; margin-right: 15px",
               wellPanel(
                 style = "height: 1000px",
                 column(
                   3,
                   radioButtons(
                     inputId = "summaryselection",
                     label = h4(strong("10 year summary of transmission zones")),
                     choiceNames = c("1970-1979", "1980-1989", "1990-1999", "2000-2009", "2010-2019", "summary GIF", "51-year summary"),
                     choiceValues = c("1.7079sum.png", "2.8089sum.png", "3.9099sum.png", "4.0009sum.png", "5.1019sum.png",
                                      "summary.gif", "51yearsumm.png"),
                     selected = NULL
                   )
                 ),
                 column(
                   9,
                   br(),
                   br(),
                   imageOutput("summaryImage", height = "auto", width = "75%")
                 )
               )))
    )
  })
  
  observeEvent(
    input$action_ID,
    {if(firstLoad()) {firstLoad(FALSE)} })
  
  #create server objects - graphs etc
  output$selecteddatemap <- renderText({
    (paste("As of", format(input$dates, format = "%d %b %Y"), 
           ", where can heartworm be transmitted?"))
  })
  
  output$leaflet_chdu <- renderLeaflet({
    chdu <- paste("C:/Users/a1667856/Box/PhD/HDU Mapping/hdu_mapping/hdumaps/",
                  "chdu", format(input$dates, format = "%Y%m%d"),
                  ".tif", sep="")
    
    chdu.r <- raster(chdu)
    
    values <- getValues(chdu.r)
    
    pal <- colorBin(c("royalblue3", "goldenrod2", "firebrick3"), bins=c(0, 120, 130, Inf),
                    na.color = "transparent")
    
    leaflet(data = chdu.r, options = leafletOptions(zoomControl=FALSE)) %>%
      addTiles()%>%
      setView(lng = 134.5, lat = -25.5, zoom = 4)%>%
      addRasterImage(x=chdu.r, opacity=0.55, col=pal) %>%
      addLegend(pal = pal, position = "bottomright", values = values,
                labels = c("Transmission not possible", "Transmission unlikely", "Transmission possible"),
                title = "cHDU")
  })
  
  
  output$summaryImage <- renderImage({
    
    filename <- normalizePath(file.path('./www',
                                        paste(input$summaryselection)))
    
    # Return a list containing the filename and alt text
    list(src = filename,
         alt = paste("Image number", input$summaryselection),
         #height=400,
         width="100%")
    
  }, deleteFile = FALSE)
  
  
  
  output$location <- renderText({
    paste("Your postcode:", input$postcode)
  })
  
  output$capital.cities <- renderTable({
    capital.df
  })
  
  output$locationplot <- renderPlot(locationplotdata())
  
  locationplotdata <- reactive({
    req(input$postcode != "")
    postcode <- input$postcode
    z <- which(poa.list==postcode)
    
    trial <- data.frame(dseq, {if(length(all_of(z))!=0) dplyr::select(postcodes.all, (all_of(z)))
      else return(NULL)})
    
    trial[,3] <- ifelse(trial[,2] > 130, 1, 0)
    trial[,4] <- as.numeric(format(trial[,1], format="%Y"))
    trial[,5] <- format(as.POSIXct(trial[,1]), "%m-%d")
    trial[,6] <- cut(trial[,2],
                     breaks=c(0,120,130,1000),
                     labels=c("Transmission unlikely", 
                              "Shoulder", "Transmission season"))
    trial[,7] <- str_c(get_fy(trial[,1], offset_period = -1),"/",get_fy(trial[,1]))
    trial[,8] <- day_of_year(trial[,1], type = "financial")
    
    yearbreaks <- seq((as.numeric(format(min(dseq), format="%Y"))+0.5), 
                      (as.numeric(format(max(dseq), format="%Y"))+0.5), by=1)
    
    
    f <- seq.Date(min(dseq), max(dseq), by="month")
    f <- as.Date(format(f, format="%m-%d"), format="%m-%d")
    
    g <- (seq.Date(min(dseq), max(dseq), by="month"))+14
    g <- as.Date(format(g, format="%m-%d"), format="%m-%d")
    
    fnx = function(x) {
      unlist(strsplit(as.character(x), '[19|20][0-9]{2}-', fixed=FALSE))[2]
    }
    
    dm1 = sapply(f, fnx)
    dm2 = sapply(g, fnx)
    
    new_col = c(as.factor(dm1), as.factor(dm2))
    
    colours <- c("Transmission season" = "firebrick3", "Transmission unlikely" = "royalblue3", 
                 "Shoulder" = "goldenrod2")
    
    years <- seq(as.numeric(format(min(dseq), format="%Y")), as.numeric(format(max(dseq), format="%Y")), by=1)
    
    #calendar year
    ggplot(trial, aes(trial[,5], y=trial[,4]))+
      geom_tile(aes(fill=trial[,6]))+
      scale_fill_manual(values=colours)+
      geom_hline(yintercept=yearbreaks)+
      scale_y_reverse(breaks=years)+
      scale_x_discrete(breaks=new_col)+
      labs(title=postcode, x="Date", y="Year", fill="Status")+
      theme(plot.title= element_text(face="bold", size=20),
            axis.title.x = element_text(face="bold", size=16),
            axis.text.x = element_text(size=14),
            axis.title.y = element_text(face="bold", size=16),
            axis.text.y = element_text(size=14),
            legend.title = element_text(face="bold", size=16),
            legend.position = "bottom",
            legend.text = element_text(size=14))
    
  })
  
  output$dailystatus <- renderText(statusdata())
  
  statusdata <- reactive({
    req(input$postcode != "")
    postcode <- input$postcode
    z <- which(poa.list==postcode)
    
    todaystatus.df <- data.frame(dseq, {if(length(all_of(z))!=0) dplyr::select(postcodes.all, (all_of(z)))
      else return(NULL)})
    todaystatus.df[,2] <- round(todaystatus.df[,2], digits=2)
    todaystatus.df[,3] <- cut(todaystatus.df[,2],
                              breaks=c(0,120,130,1000),
                              labels=c(", transmission is unlikely",
                                       ", the transmission season may be starting soon if there is more warm weather", ", transmission is possible, consult your veterinarian"))
    t <- paste((todaystatus.df[nrow(todaystatus.df),2]), "HDUs", todaystatus.df[nrow(todaystatus.df),3], sep="")
    t <- as.character(t)
    t
    
  })
  
  output$cutofftable <- renderDataTable(cutoffdata(), rownames = FALSE, options = list(searching = FALSE, pageLength = 25, orderSequence = list(1, 'asc')))
  
  cutoffdata <- reactive({
    req(input$postcode != "")
    postcode <- input$postcode
    z <- which(poa.list==postcode)
    
    status.df <- data.frame(dseq, {if(length(all_of(z))!=0) dplyr::select(postcodes.all, (all_of(z)))
      else return(NULL)})
    
    status.df[,3] <- ifelse(status.df[,2] > 130, 1, 0)
    status.df[,4] <- NA
    
    for (n in 1:nrow(status.df)){
      status.df[n,4] <- ifelse(status.df[(n+1),2] < 130 & status.df[n,2]>130, 1, 0)}
    
    a <- which(status.df[,4]==1)
    validate(need(length(a) > 0, message=paste("If this message is being shown, there are no cutoff dates. All dates of each year since 2015 have the same status")))
    
    cutoffdata <- data.frame(status.df[a,1], "season stops")
    
    status.df[,5] <- NA
    status.df[,5] <- ifelse(status.df[,2] < 130, 1, 0)
    status.df[,6] <- NA
    
    for (n in 1:nrow(status.df)){
      status.df[n,6] <- ifelse(status.df[(n+1),2] > 130 & status.df[n,2]<130, 1, 0)
      
    }
    
    b <- which(status.df[,6]==1)
    
    df1 <- data.frame(status.df[b,1], "season starts")
    df2 <- data.frame(status.df[a,1], "season stops")
    
    #how many days since transmission was possible?
    for (k in 1:(nrow(df1))){
      df1[k,3] <- day_of_year(df1[k,1]) - day_of_year(df2[k,1])
      
    }
    
    df2[,3] <- c("NA")
    
    colnames(df1) <- c("Date", paste("Status for ", input$postcode, sep=""), "Days since last transmission")
    colnames(df2) <- c("Date", paste("Status for ", input$postcode, sep=""), "Days since last transmission")
    
    cutoff.df <- rbind(df1, df2)
    cutoff.df <- data.frame(cutoff.df[order(as.Date(cutoff.df[,1], format="%Y-%m-%d")),])
    #cutoff.df <- ifelse ((length(a)*length(b) != 0), cutoff.df, data.frame(c("Not applicable for this postcode")))
    
    colnames(cutoff.df) <- c("Date", paste("Status for ", input$postcode, sep=""), "Days since last transmission")
    #"pageLength":100
    cutoff.df
    
    
    
  })
  
  outputOptions(output, 'cutofftable', suspendWhenHidden=TRUE)
  
  output$percentagetable <- renderDataTable(percentagetabledata(), rownames = FALSE, options = list(searching = FALSE))
  
  percentagetabledata <- reactive({
    req(input$postcode != "")
    postcode <- input$postcode
    z <- which(poa.list==postcode)
    
    perc.df <- data.frame(dseq, {if(length(all_of(z))!=0) dplyr::select(postcodes.all, (all_of(z)))
      else return(NULL)})
    
    perc.df[,3] <- ifelse(perc.df[,2] > 130, 1, 0)
    perc.df[,4] <- NA
    
    for (n in 1:nrow(perc.df)){
      perc.df[n,4] <- ifelse(perc.df[(n+1),2] < 130 & perc.df[n,2]>130, 1, 0)
      
    }
    
    a <- which(perc.df[,4]==1)
    validate(need(length(a) > 0, message=paste("If this message is being shown, it indicates 100% of each year has the same status")))
    
    percdata <- data.frame(perc.df[a,1], "season stops")
    perc.df[,5] <- NA
    perc.df[,5] <- ifelse(perc.df[,2] < 130, 1, 0)
    perc.df[,6] <- NA
    
    for (n in 1:nrow(perc.df)){
      perc.df[n,6] <- ifelse(perc.df[(n+1),2] > 130 & perc.df[n,2]<130, 1, 0)
      
    }
    
    b <- which(perc.df[,6]==1)
    
    df1 <- data.frame(perc.df[b,1], "season starts")
    df2 <- data.frame(perc.df[a,1], "season stops")
    
    
    for (k in 1:(nrow(df1))){
      c <- day_of_year(df1[k,1]) - day_of_year(df2[k,1])
      df1[k,3] <- ifelse(c>=0, c, NA)
      #df1[k,3] <- day_of_year(df1[k,1]) - day_of_year(df2[k,1])
      
    }
    
    df2[,3] <- c(NA)
    
    percentagetabledata <- data.frame(yseq.df[,1], NA)
    for (i in 1:length(yseq.df[,1])){
      
      tdf <- subset(df1, format(as.Date(df1[,1]), "%Y")==yseq.df[i,1])
      percentagetabledata[i,2] <- (yseq.df[i,2] - sum(tdf[,3]))/yseq.df[i,2] * 100
      
    }
    
    percentagetabledata[,2] <- as.numeric(percentagetabledata[,2])
    percentagetabledata[,2] <- round(percentagetabledata[,2], digits=2)
    
    percentagetabledata[nrow(percentagetabledata),2] <- ifelse(yseq.df[nrow(yseq.df),2] >= 365, percentagetabledata[nrow(percentagetabledata),2], NA)
    
    colnames(percentagetabledata) <- c("Year", "Percentage of the year at risk")
    percentagetabledata
    
  })
  outputOptions(output, 'percentagetable', suspendWhenHidden=TRUE)
  
}




