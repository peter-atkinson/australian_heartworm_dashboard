function(input,output,session){
  
  firstLoad <- reactiveVal(TRUE)
  
  #create ui output main_paload a reactive UI depending on conditions
  output$postcode_page <- renderUI({
    if(firstLoad()) {
      uiOutput("disclaimer")
    }
    else {
      uiOutput("postcode")
    }
    
  })
  
  #create each UI
  output$disclaimer <- renderUI({
    tagList(
      tags$link(rel = "stylesheet", type = "text/main/css", href = "css/main.css"),
      wellPanel(div(style = "text-align: center;",
                    h4(HTML("If this is your first time visiting, please read <b>'Dashboard background'</b>, found on the <b>'About'</b> page before proceeding.
                            If you have visited before, thanks for returning!"))),
                br(),
                br(),
                div(style = "text-align: center;",
                    actionButton("action_ID", h4(HTML("Continue to <b> Australian heartworm transmission dashboard </b>")),
                                 width = "100%"))
      ),
      fluidRow(column(12, style = "margin-bottom: 5px;",
                      wellPanel(h4(HTML("<b> Dashboard background </b> <br> <br>
                                      Researchers at the University of Adelaide have developed a novel tool to help veterinarians and pet owners to manage heartworm infections in practice. Read the background information, then explore our dashboard 
                                      and find out your local risk! <br><br>")),
                                HTML("<i> <b> Canine heartworm biology and lifecycle </i> </b> <br>
                                     Canine heartworm disease is caused by infection with a parasite called <i>Dirofilaria immitis</i>. These are nematodal parasites, and adults live in the pulmonary arteries and occasionally in the right atrium. Adult worms can sexually reproduce and release immature larvae, called microfilaria or L1 larvae. The infection is transmitted by mosquitoes. A feeding mosquito can ingest microfilaria when feeding, and transmit them to another dog. Once transmitted, the larvae require approximately six months to mature, during which they migrate from the peripheral bite site to the pulmonary arteries.
Clinical signs of infection include coughing and exercise intolerance, which can progress to right-sided heart failure, although infected dogs can also be subclinical. Clinical signs result from impedance to the blood flow through the pulmonary arteries. Therefore, the severity of clinical signs relies on the worm burden, the size of the pulmonary arteries and time, with worms causing vascular inflammation and hypertension with time.

<br><br> <i> <b> Extrinsic incubation period of  D. immitis </i> </b> <br>
<i>D. immitis</i> larvae require a mosquito to be transmitted to another dog. The larvae also undertake crucial developmental stages within the mosquito to progress from microfilariae (L1 larvae) into infective L3 larvae. This is known as the extrinsic incubation period (EIP), and is a process controlled by temperature of the larvae. As mosquitoes are poikilothermic (cold-blooded), the temperature of the larvae within the mosquito is driven by the environmental temperature. Development of larvae completely ceases when the temperature is less than 14°C, and increases linearly with temperatures above this (Fortin and Slocombe, 1981). Development is completed when the larvae have experienced sufficient heat for a sufficient time, and this can be assessed by using a measure of degree-days, referred to as heartworm development units (HDUs). One HDU is accumulated for every 24 hours the temperature is above the 14°C threshold. For larvae to complete their development, they need at least 130 HDUs (Slocombe et al., 1989). The development from microfilaria to L3 larvae needs to be completed within the lifetime of the mosquito, which is approximately 30 days (Knight and Lok, 1998; Brown et al., 2012).
 
<br><br> <i> <b> Modelling of extrinsic incubation period </i> </b> <br>
The EIP of <i>D. immitis</i> can be modelled by using temperature data, and calculating areas where the required 130 HDUs could be accumulated by would-be larvae, over a 30 day period. We have modelled daily temperature data to calculate which areas would have experienced sufficient heat to allow the completion of the EIP. In these regions, transmission of <i>D. immitis</i> between dogs is possible, provided there are mosquitoes to act as vectors, and other infected dogs to act as a source of microfilaria. In regions where EIP cannot be completed, transmission of <i>D. immitis</i> is not possible.
Daily calculation of HDUs accumulated was based on the methods outlined by Baskerville and Emin (1969).

<br><br> <i> <b> Dashboard information </i> </b> <br>
This dashboard collates weather data and provides information based on where and when the extrinsic incubation period of <i>D. immitis</i> could be completed. Maps on this dashboard are colour-coded. Blue zones indicate regions where development would not have been completed within the preceding 30 days. Red zones show where development would have been possible. Orange zones show a ‘shoulder’ season, and if there is more warm weather in following days in these zones, development may be able to complete.

We also provide postcode-based location information for users to select their postcode of interest and look at EIP trends since 2015.

<br><br> <i> <b> Model limitations </i> </b> <br>
We acknowledge the limitations of modelling weather data to predict transmission events. This model <i>does not</i> predict exactly where transmission will occur. Transmission requires mosquitoes to act as vectors, and other infected dogs to provide microfilaria. In addition, this model provides an overestimate of possible areas where EIP can be completed, by simultaneously <i>overestimating</i> the HDUs accumulated per mosquito lifespan, and <i>underestimating</i> the required HDUs for EIP to be completed: 
<br><br>-	EIP completion is based on 130 HDUs being accumulated within one mosquito’s lifespan. We estimated the lifespan to be 30 days, although the main two mosquitoes responsible for transmission in Australia live for approximately 17 days (<i>Ochlerotatus notoscriptus</i>) and 22 days (<i>Culex annulirostris</i>) when supporting <i>D. immitis</i> larvae (Russell and Geary, 1996)
<br>-	Most mosquito species require more than 130 HDUs for EIP to be completed (Slocombe et al., 1989)
<br><br>Therefore, we believe we have accounted for fine-scale differences in temperature and inaccuracies in weather data collection.
<br><br><i><b> More information</b></i><br>"),
tags$p("For more information, please read our open-access publication in the Internation Journal of Parasitology ",
        tags$a(href = "https://doi.org/10.1016/j.ijpara.2024.02.001", "here.")),
tags$p("To access a discussion about how to apply this work in your clinical practice, please see our open-access publication in the Australian Veterinary Journal ",
       tags$a(href = "https://doi.org/10.1111/avj.13379", "here.")),
HTML("<br> <i> <b> Data sources </i> </b> <br>"),
tags$p("Weather data is sourced through the open source ",
       tags$a(href = "https://www.longpaddock.qld.gov.au/silo/", "SILO program,"),"offered by the Queensland Government"),
HTML("<br> <i> <b> Contact us </i> </b> <br>
     Got questions? Send us an email: peter.atkinson@adelaide.edu.au"),
HTML("
     <br><br><br><br> <i> References </i>
     <br>-Baskerville, G.L., Emin, P., 1969. Rapid Estimation of Heat Accumulation from Maximum and Minimum Temperatures. Ecology 50, 514-517.
<br>-Brown, H.E., Harrington, L.C., Kaufman, P.E., McKay, T., Bowman, D.D., Nelson, C.T., Wang, D., Lund, R., 2012. Key factors influencing canine heartworm, Dirofilaria immitis, in the United States. Parasites Vectors 5, 245-245.
<br>-Fortin, J.F., Slocombe, J.O.D., 1981. Temperature requirements for the development of Dirofilaria immitis in Aedes triseriatus and Ae. vexans. Mosq. News 41, 625-633.
<br>-Knight, D.H., Lok, J.B., 1998. Seasonality of heartworm infection and implications for chemoprophylaxis. Clin. Tech. Small. Anim. Pract. 13, 77-82.
<br>-Russell, R.C., Geary, M.J., 1996. The influence of microfilarial density of dog heartworm Dirofilaria immitis on infection rate and survival of Aedes notoscriptus and Culex annulirostris from Australia. Med Vet Entomol 10, 29-34.
<br>-Slocombe, J.O.D., Surgeoner, G.A., Srivastava, B., 1989. Determination of the heartworm transmission period and its used in diagnosis and control. In, Proceedings of the Heartworm Symposium '89, Charleston, South Carolia, USA.

"),

     )
                                )))
    
  })
  
  output$postcode <- renderUI({
    div(
      fluidRow(column(
        12, style = "margin-bottom: 5px;",
        wellPanel(style = "text-align: center;",
                  h4(HTML("If this is your first time visiting, please read <b>'Dashboard background'</b>, found on the <b>'About'</b> page before proceeding.
                            If you have visited before, thanks for returning!")),
                  br(),
                  div(actionButton("action_about", label = HTML("Take me to <b>'About'</b>"))))
        
      )),
      fluidRow(
        column(
          12,
          wellPanel(fluidRow(h2("Location")),
                    fluidRow(selectizeInput(
                      "postcode",
                      "Enter your postcode:",
                      choices = poa.list,
                      selected = 5371,
                      width = "200px"
                    )),
                    fluidRow(
                      div(
                        class = "output-container",
                        shinycssloaders::withSpinner(
                          plotOutput("simplelocationplot"),
                          color = getOption("spinner.color", default = "darkgrey")
                        )
                      )),
                    actionButton("postcode_modal", strong("What does this graph show?"), icon = icon("info-circle")),
                    br(),
                    br(),
                    h4(strong("Want more information?")),
                    fluidRow(selectizeInput(
                      "yearselect",
                      "Select 2 years to compare",
                      choices = yseq.df$year,
                      multiple = TRUE,
                      selected = max(yseq.df$year),
                      options = list(maxItems = 2)
                    )),
                    fluidRow(textOutput("postcode"),
                    div(
                      class = "output-container",
                      shinycssloaders::withSpinner(
                        plotlyOutput("locationplot"),
                        color = getOption("spinner.color", default = "darkgrey")
                      )
                    )),
                    actionButton("postcodeyearly_modal", strong("What does this graph show?"), icon = icon("info-circle"))
          )
    )),
    
    fluidRow(
      div(
        #style = "margin-left: 15px; margin-right: 15px",
        column(6,
               wellPanel(p(
                 strong(paste(
                   as.Date((Sys.Date() - 7), format = "%d-%m-%Y"), "'s", " status:", sep ="")),
                 textOutput(("dailystatus"))
               ))
               #,
               #wellPanel(dataTableOutput("cutofftable"))
               ),
        column(6,
               wellPanel(
                   tags$h2("Export data"),
                   downloadButton("export", "Download My Location Report")
                 )
               ))
      )
    )
  })
  
  output$aus_page <- renderUI({
    tagList(
      tags$link(
        rel = "stylesheet",
        type = "text/css",
        href = "css/main.css"
      ),
      fluidRow(column(
        12, style = "margin-bottom: 5px;",
        wellPanel(style = "text-align: center;",
                  h4(HTML("If this is your first time visiting, please read <b>'Dashboard background'</b>, found on the <b>'About'</b> page before proceeding.
                            If you have visited before, thanks for returning!")),
                  br(),
                  div(actionButton("action_about", label = HTML("Take me to <b>'About'</b>"))))
        
      )),
      fluidRow(
        class = "css/flexbox",
        style = "height: 800px",
        column(9, 
               wellPanel(style = "height: 900px",
                         h3(
                           textOutput("selecteddatemap"),
                           br(),
                           dateInput(
                             "dates",
                             label = NULL,
                             value = (Sys.Date() - 7),
                             min = min(dseq),
                             max = max(dseq)
                           ),
                           shinycssloaders::withSpinner(
                             leafletOutput("leaflet_chdu", height = 600, width ="100%"),
                             color = getOption("spinner.color", default = "darkgrey")
                         )),
                         br(),
                         wellPanel(textOutput("mapcaption"))
               )),
        column(3, 
               wellPanel(style = "height: 900px",
                         h4(strong(
                           'What is happening in the capital cities?'
                         )), 
                         dataTableOutput("capital.cities"),
                         br(),
                         wellPanel(textOutput("tablecaption"))
                         #,
                         #actionButton("capitalcity_modal", strong("What does this table show?"), icon = icon("info-circle"))

               ))
      )
      )
  })
  
  output$historical_page <- renderUI({
    fluidRow(style = "margin-left: 15px; margin-right: 15px",
             wellPanel(
               style = "height: 1000px",
               column(
                 3,
                 radioButtons(
                   inputId = "summaryselection",
                   label = h4(strong("10 year summary of transmission zones")),
                   choiceNames = c("1970-1979", "1980-1989", "1990-1999", "2000-2009", "2010-2019"),
                   choiceValues = c("1.7079sum.png", "2.8089sum.png", "3.9099sum.png", "4.0009sum.png", "5.1019sum.png"),
                   selected = NULL
                 )
               ),
               column(
                 9,
                 br(),
                 br(),
                 imageOutput("summaryImage", height = "auto", width = "75%")
               )
             ))
    
  })
  
  observeEvent(
    input$action_ID,
    {if(firstLoad()) {firstLoad(FALSE)} })
  
  observeEvent(
    input$action_about,
    {updateNavbarPage(inputId ="app", selected = "About")}
  )
  
  #create server objects - graphs etc
  output$selecteddatemap <- renderText({
    (paste0("As of ", format(input$dates, format = "%d-%m-%Y"), 
           ", where can heartworm complete its extrinsic incubation period?"))
  })
  
    output$leaflet_chdu <- renderLeaflet({
    #chdu <- paste("/Users/a1667856/Library/CloudStorage/Box-Box/PhD/HDU Mapping/hdu_mapping/hdumaps/", "chdu", format(input$dates, format = "%Y%m%d"), ".tif", sep="") #local running
    chdu <- paste("./hdumaps/", "chdu", format(input$dates, format = "%Y%m%d"), ".tif", sep="") #docker running
    
    chdu.r <- raster(chdu)
    
    values <- getValues(chdu.r)
    
    pal <- colorBin(c("royalblue3", "firebrick3"), bins=c(0, 130, Inf),
                    na.color = "transparent")
    
    leaflet(data = chdu.r, options = leafletOptions(zoomControl=FALSE)) %>%
      addTiles()%>%
      setView(lng = 134.5, lat = -25.5, zoom = 4)%>%
      addRasterImage(x=chdu.r, opacity=0.55, col=pal) %>%
      addLegend(pal = pal, position = "bottomright", values = values,
                labels = c("EIP not possible", "EIP possible"),
                title = "cHDU")
  })
  
    output$mapcaption <- renderText({
      "The above map of Australia shows regions where heartworm can complete its extrinsic incubation period (EIP). This is where there has been
    sufficient heat in the environment for larvae to become infectious. Red zones are where EIP can be completed, which may support transmission, depending on the occurrence of both a mosquito population and proximity to another infected host. Blue zones are areas where it has been too cold for EIP to complete, so 
    transmission of heartworm is not possible."
    })
    
    output$tablecaption <- renderText({
      "The above table displays the most recent data for each capital city of Australia. It shows whether heartworm extrinsic incubation period (EIP) 
      is possible to complete, and therefore whether heartworm preventatives may be required to be administered."
      
    })
  
  
  output$location <- renderText({
    paste("Your postcode:", input$postcode)
  })
  
  output$capital.cities <- renderDataTable(capital.df, rownames = FALSE, options = list(searching = FALSE, 
                                                                                        lengthMenu = list(c(-1), c("All")), 
                                                                                        dom = "t"))

  # output$capital.status <- renderDataTable({
  #   capital.status.df <- round(capital.status.df, digits = 2)
  #   recent.date <- as.Date(max(dseq), format = "%d%M%Y")
  #   
  #   print(format(input$dates, format = "%d-%m-%Y"))
  #   browser()
  #   
  #   capital.chdu <- currentmax.df[,(which(dseq == input$dates))]
  #   
  #   preventatives <- c(0)
  #   for (i in 1:8){
  #     capital.chdu[2,i] <- if_else(capital.chdu[1,i] >= 130, 'On', 'Off')
  #     preventatives[i] <- if_else(capital.chdu[2,i]=="On", "Preventatives may be required", "Preventatives not necessary")
  # 
  #   }
  #   capital.chdu <- rbind(capital.names, capital.chdu, preventatives)
  #   capital.df <- as.data.frame(t(capital.chdu))
  #   colnames(capital.df) <- c("Capital city", (paste(max(dseq), "'s", " cHDUs", sep="")), (paste(max(dseq), "'s", " status", sep="")), "Are preventatives necessary?")
  # 
  #   capital.hdu
  # 
  # })
  
# Postcode subsetting -----------------------------------------------------

  
  postcodedata <- reactive({
    req(input$postcode != "")
    postcode <- input$postcode
    z <- which(poa.list==postcode)
    
    todaystatus.df <- data.frame(dseq, {if(length(all_of(z))!=0) dplyr::select(postcodes.all, (all_of(z)))
      else return(NULL)})
    
    todaystatus.df$year <- year(todaystatus.df$dseq)
    
    return(todaystatus.df)
    
  })
  
  simplelocationplotdata <- reactive({
      trial <- postcodedata()
      trial[,3] <- ifelse(trial[,2] > 130, 1, 0)
      trial[,4] <- as.numeric(format(trial[,1], format="%Y"))
      trial[,5] <- format(as.POSIXct(trial[,1]), "%m-%d")
      trial[,6] <- cut(trial[,2],
                       breaks=c(0,130,1000),
                       labels=c("Transmission unlikely", 
                              "Transmission possible"))
      
      colnames(trial) <- c("Date", "cHDUs", "EIP Status", "Year", "Date-Day", "EIP Status for Transmission")
      
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
      
      colours <- c("Transmission possible" = "firebrick3", "Transmission unlikely" = "royalblue3")
      
      years <- seq(as.numeric(format(min(dseq), format="%Y")), as.numeric(format(max(dseq), format="%Y")), by=1)
      
      #calendar year
      plot <- ggplot(trial, aes(trial[,5], y=trial[,4]))+
        geom_tile(aes(fill=trial[,6]))+
        scale_fill_manual(values=colours)+
        geom_hline(yintercept=yearbreaks)+
        scale_y_reverse(breaks=years)+
        scale_x_discrete(breaks=new_col)+
        labs(title=input$postcode, x="Date", y="Year", fill="Status")+
        theme_classic()+
        theme(plot.title= element_text(face="bold", size=20),
              axis.title.x = element_text(face="bold", size=16),
              axis.text.x = element_text(size=14),
              axis.title.y = element_text(face="bold", size=16),
              axis.text.y = element_text(size=14),
              legend.title = element_text(face="bold", size=16),
              legend.position = "bottom",
              legend.text = element_text(size=14))
      
      plot
  })
  
  output$simplelocationplot <- renderPlot(simplelocationplotdata())
  
  
  postcodeyeardata <- reactive({
    req(postcodedata())
    req(!is.null(input$yearselect))
    dplyr::filter(postcodedata(), year %in% input$yearselect)
    
  })
  
  output$locationplot <- renderPlotly(locationplotdata())
  
  locationplotdata <- reactive({
    trial <- postcodeyeardata()
    #browser()
    trial[,3] <- ifelse(trial[,2] > 130, 1, 0)
    trial[,4] <- as.numeric(format(trial[,1], format="%Y"))
    trial[,5] <- format(as.POSIXct(trial[,1]), "%m-%d")
    trial[,6] <- cut(trial[,2],
                     breaks=c(0,130,1000),
                     labels=c("Transmission unlikely", "Transmission possible"))
    
    colnames(trial) <- c("Date", "cHDUs", "EIP Status", "Year", "Date-Day", "EIP Status for Transmission")
    
    # yearbreaks <- seq((as.numeric(format(min(dseq), format="%Y"))+0.5), 
    #                   (as.numeric(format(max(dseq), format="%Y"))+0.5), by=1)
    # 
    
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
    
    colours <- c("Transmission possible" = "firebrick3", "Transmission unlikely" = "royalblue3")
    
    years <- unique(trial$Year)
    
    yearbreaks <- c(min(years)+(max(years)-min(years))/2, max(years)+(max(years)-min(years))/2)
    
    cols <- c("royalblue3", "firebrick3")
    
    #calendar year
    postcodeplot <- ggplot(trial, aes(trial[,5], y=trial[,4]))+
      geom_tile(aes(fill=trial[,6]))+
      scale_fill_manual(values=colours)+
      geom_hline(yintercept=yearbreaks)+
      scale_y_reverse(breaks=years)+
      scale_x_discrete(breaks=new_col)+
      labs(title=input$postcode, x="Date", y="Year", fill="Status")+
      theme_classic()+
      theme(plot.title= element_text(face="bold", size=20),
            axis.title.x = element_text(face="bold", size=16),
            axis.text.x = element_text(size=14),
            axis.title.y = element_text(face="bold", size=16),
            axis.text.y = element_text(size=14),
            legend.title = element_text(face="bold", size=16),
            legend.position = "bottom",
            legend.text = element_text(size=14))
    
    postcodeplot
    
    postcode_plotly <- ggplotly(postcodeplot,
                     tooltip = c("x", "fill"))
    
    postcode_plotly <- layout(postcode_plotly, legend = list(x = 0.3, y = -0.6, orientation = "h", 
                                                             title = (list(text = "<b> Status </b>"))))
    
  })
  
 
  output$dailystatus <- renderText(statusdata())
  
  statusdata <- reactive({
    todaystatus.df <- postcodedata()
      
    todaystatus.df[,2] <- round(todaystatus.df[,2], digits=2)
    todaystatus.df[,3] <- cut(todaystatus.df[,2],
                              breaks=c(0,120,130,1000),
                              labels=c(", transmission is unlikely",
                                       ", transmission may become possible if there is more warm weather", ", transmission is possible, use preventatives where appropriate"))
    t <- paste((todaystatus.df[nrow(todaystatus.df),2]), "HDUs", todaystatus.df[nrow(todaystatus.df),3], sep="")
    t <- as.character(t)
    t
    
  })
  
  

  output$cutofftable <- renderDataTable(cutoffdata(), rownames = FALSE, options = list(searching = FALSE, pageLength = 25, orderSequence = list(1, 'asc')))
  
  cutoffdata <- reactive({
    status.df <- postcodedata()
    
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
    
    df1 <- data.frame(status.df[b,1], "EIP can complete")
    df2 <- data.frame(status.df[a,1], "EIP cannot complete")
    
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
    
    df1 <- data.frame(perc.df[b,1], "EIP can complete")
    df2 <- data.frame(perc.df[a,1], "EIP cannot complete")
    
    
    for (k in 1:(nrow(df1))){
      c <- day_of_year(df1[k,1]) - day_of_year(df2[k,1])
      df1[k,3] <- ifelse(c>=0, c, NA)
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
  
  #export button
  output$export <- downloadHandler(
    filename = function() {
      return("HW_EIP_locationreport.pdf")
    },
    content = function(file) {
      rmarkdown::render(
        "report.qmd",
        output_file = file,
        output_format = "pdf_document",
        envir = new.env(),
        clean = TRUE
      )
    }
  )
  
  
  output$summaryImage <- renderImage({
    
    filename <- normalizePath(file.path('./www',
                                        paste(input$summaryselection)))
    
    # Return a list containing the filename and alt text
    list(src = filename,
         alt = paste("Image number", input$summaryselection),
         #height=400,
         width="100%")
    
  }, deleteFile = FALSE)
 
  #modals
  observeEvent(input$postcode_modal, {
    showModal(modalDialog(
      title = h3("Postcode graph"),
      "This is a graph showing, for your selected postcode input, what days of the year the extrinsic incubation period (EIP)
      of heartworm could be completed (in red). The days that are blue show when EIP could not be completed. Those days shown
      in orange indicate a time of the year where EIP may be able to complete if there is warmer weather.",
      easyClose = TRUE,
      size = "m",
      fade = TRUE
    ))
  })
  
  
  observeEvent(input$postcodeyearly_modal, {
    showModal(modalDialog(
      title = h3("Postcode graph"),
      "This is a graph showing, for your selected postcode input, what days of the year the extrinsic incubation period (EIP)
      of heartworm could be completed (in red). The days that are blue show when EIP could not be completed. Those days shown
      in orange indicate a time of the year where EIP may be able to complete if there is warmer weather. This graph has more detail, 
      and allows you to compare between years the dates where EIP becomes possible, or otherwise.",
      easyClose = TRUE,
      size = "m",
      fade = TRUE
    ))
  })
  
}




