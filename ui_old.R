navbarPage(id="app",
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "css/main.css")
  ),
  title = (strong("Transmission Tracker - Dirofilaria")),
  #position = "static-top",
  inverse = TRUE,
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "main.css")
  ),
  tabPanel("Australia-wide",
           uiOutput("main_page")),
  tabPanel("My Location",
           uiOutput("postcode_page")),
  tabPanel("Australian long term trends",
           uiOutput("historical_page")),
  tabPanel("About",
           fluidRow(
             column(12, style = "margin-bottom: 5px;",
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

<br><br> <i> <b> Data sources </i> </b> <br>"),
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

"))
           ))
)
)
