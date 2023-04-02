navbarPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "css/main.css")
  ),
  title = (strong("Transmission Tracker - Dirofilaria")),
  #position = "static-top",
  inverse = TRUE,
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "main.css")
  ),
  tabPanel("Australia",
           uiOutput("main_page")),
  tabPanel("My Location",
           uiOutput("postcode_page")),
  tabPanel("Long term trends",
           uiOutput("historical_page")),
  tabPanel("About",
           fluidRow(
             column(12, style = "margin-bottom: 5px;",
                    wellPanel(
                      h4(
                        HTML(
                          "Canine heartworm disease is a mosquito-transmitted disease
                                                      affecting domestic dogs, wild canids, cats and very occasionally people. <br> <br>"
                        )
                      ),
                      HTML(
                        "<i> <b> Transmission suitability </i> </b> <br>
The disease relies on mosquitoes for transmission and completion of its lifecycle. If a mosquito feeds on an infected dog, it may ingest larvae that are circulating in the dog’s bloodstream. For the larvae and mosquito to become infective to other dogs, the larvae must develop and move into the mosquito mouth-parts. This process will only occur when the temperature is above 14°C, and the rate of development increases as temperature increases from this threshold. For development to complete, the larvae must experience 130 degree-days, which are accumulated at a rate of 1 degree-day per day above the threshold.
Additionally, the mosquito vector lives for a maximum of 30 days. Therefore, for the larvae to become infective they need to experience their 130 degree-days within a 30 day window. If these constraints are not met, transmission is not possible.
If transmission does occur, the implanted larvae migrate through the dog and develop into adult heartworms. This process takes 6 months. Monthly or long-acting medications can be administered that target the migrating larvae to prevent development into adults, and possible disease. <br> <br>

<i> <b> Dashboard information </i> </b> <br>
 This dashboard combines nationally collected weather data with the temperature requirement of heartworm larval transmission to show regions where and when development within the mosquito can be completed. It also shows regions where development cannot be completed. Maps on this dashboard are colour-coded. Blue zones indicate regions where development would not have been completed within the preceding 30 days. Red zones show where development would have been possible. Orange zones show a ‘shoulder’ season, and if there is more warm weather in following days in these zones, development may be able to complete.
This dashboard is not designed to show exactly where transmission events may occur. It shows regions where temperature is not sufficient to allow development within the mosquito, and where preventative medications are unlikely to be necessary.
"
                      )
                    ))
           ))
)
