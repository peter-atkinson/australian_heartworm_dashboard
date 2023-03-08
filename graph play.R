library(dplyr); library(cropgrowdays); library(stringr)

trial <- data.frame(dseq, postcodes.all[,1000])

dseq <- seq(from = as.Date("01-01-2015", format = "%d-%m-%Y"), to = as.Date(Sys.Date(), format = "%d-%m-%Y"), by = 1)
list <- readRDS("list")
postcodes.all <- readRDS("poa20152021max.RDS")
postcodes.all[nrow(postcodes.all)+(length(dseq) - nrow(postcodes.all)),] <- NA

postcode <- 5067
req(input$postcode != "")
postcode <- ifelse(input$postcode < 1000, paste0("0", input$postcode), input$postcode)
z <- which(list==postcode)

status.df <- data.frame(dseq, postcodes.all[,which(list==postcode)])

status.df[,3] <- ifelse(status.df[,2] > 130, 1, 0)
status.df[,4] <- NA

for (n in 1:nrow(status.df)){
  status.df[n,4] <- ifelse(status.df[(n+1),2] < 130 & status.df[n,2]>130, 1, 0)
  
}

a <- which(status.df[,4]==1)

cutoffdata <- data.frame(status.df[a,1], "season stops")

status.df[,5] <- NA
status.df[,5] <- ifelse(status.df[,2] < 130, 1, 0)
status.df[,6] <- NA

for (n in 1:nrow(status.df)){
  status.df[n,6] <- ifelse(status.df[(n+1),2] > 130 & status.df[n,2]<130, 1, 0)
  
}

b <- which(status.df[,6]==1)

df1 <- data.frame(status.df[b,1], "season starts")
colnames(df1) <- c("date", "status")

df2 <- data.frame(status.df[a,1], "season stops")
colnames(df2) <- c("date", "status")

cutoff.df <- rbind(df1, df2)
cutoff.df <- data.frame(cutoff.df[order(as.Date(cutoff.df[,1], format="%Y-%m-%d")),])

df1[,3] <- c(day_of_year(df1[,1]) - day_of_year(df2[,1]))
df2[,3] <- NA

poa <- 0800
postcode <- as.character(ifelse(poa < 1000, paste0("0", poa), poa))
z <- which(list==postcode)

trial <- data.frame(dseq, {if(length(z)!=0) select(postcodes.all, (z))
  else return(NULL)})

trial[,3] <- ifelse(trial[,2] > 130, 1, 0)
trial[,4] <- as.numeric(format(trial[,1], format="%Y"))
trial[,5] <- format(as.POSIXct(trial[,1]), "%m-%d")
trial[,6] <- cut(trial[,2],
                 breaks=c(0,100,130,1000),
                 labels=c("Transmission unlikely", 
                          "Shoulder", "Transmission season"))

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

colours <- c("Transmission season" = "brown3", "Transmission unlikely" = "blue3", 
             "Shoulder" = "darkorange2")

years <- seq(as.numeric(format(min(dseq), format="%Y")), as.numeric(format(max(dseq), format="%Y")), by=1)

ggplot(trial, aes(trial[,5], y=trial[,4]))+
  geom_tile(aes(fill=trial[,6]))+
  scale_fill_manual(values=colours)+
  geom_hline(yintercept=yearbreaks)+
  scale_y_reverse(breaks=years)+
  scale_x_discrete(breaks=new_col)+
  labs(title=postcode, x="Date", y="Year", fill="Status")+
  theme(plot.title= element_text(face="bold", size=20),
        axis.title.x = element_text(size=16),
        axis.text.x = element_text(size=14),
        axis.title.y = element_text(size=16),
        axis.text.y = element_text(size=14))


postcode <- 5067
req(input$postcode != "")
#postcode <- as.character(ifelse(input$postcode < 1000, paste0("0", input$postcode), input$postcode))
postcode <- input$postcode
z <- which(list==postcode)

status.df <- data.frame(dseq, {if(length(z)!=0) select(postcodes.all, (z))
  else return(NULL)})

status.df[,3] <- ifelse(status.df[,2] > 130, 1, 0)
status.df[,4] <- NA
#status.df[,4] <- cut(status.df[,2],
#breaks = c(0, 120, 130, 1000),
#labels = c("Transmission unlikely", "Shoulder", "Transmission possible"))


for (n in 1:nrow(status.df)){
  status.df[n,4] <- ifelse(status.df[(n+1),2] < 130 & status.df[n,2]>130, 1, 0)
  
}

a <- which(status.df[,4]==1)

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
df1[,3] <- c(day_of_year(df1[,1]) - day_of_year(df2[,1]))
df2[,3] <- c("NA")
colnames(df1) <- c("Date", paste("Status for ", input$postcode, sep=""), "Days since last transmission")
colnames(df2) <- c("Date", paste("Status for ", input$postcode, sep=""), "Days since last transmission")

cutoff.df <- rbind(df1, df2)
cutoff.df <- data.frame(cutoff.df[order(as.Date(cutoff.df[,1], format="%Y-%m-%d")),])
#cutoff.df <- ifelse ((length(a)*length(b) != 0), cutoff.df, data.frame(c("Not applicable for this postcode")))

colnames(cutoff.df) <- c("Date", paste("Status for ", input$postcode, sep=""), "Days since last transmission")
cutoff.df

error.df <- data.frame("Not applicable", "for this postcode")
cutoffdata <- if_else ((length(a)*length(b) != 0), cutoff.df, error.df)

df2015 <- subset(df1, format(as.Date(df1[,1]), "%Y")==2015)
df2016 <- subset(df1, format(as.Date(df1[,1]), "%Y")==2016)

percentagetabledata <- data.frame(yseq, NA)

for (i in 1:length(yseq)){
  tdf <- subset(df1, format(as.Date(df1[,1]), "%Y")==yseq[i])
  percentagetabledata[i,2] <- max(tdf[,3])/yseq.df[i,2]
  
}

max(df2016[,3])/365
percentage.c <- c(df2015[,3]/365)
percentagetabledata <- data.frame(yseq, )

df1[,3] <- NA

for (k in 1:(nrow(df1))){
  df1[k,3] <- day_of_year(df1[k,1]) - day_of_year(df2[k,1])
  
}

df1[,3] <- as.numeric(c(day_of_year(df1[,1]) - day_of_year(df2[,1])))
df2[,3] <- c("NA")


req(input$postcode != "")
postcode <- input$postcode
z <- which(list==postcode)

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

trialsubs <- filter(trial, trial[,7]=="2015/2016")

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

new_col <- c(as.factor(dm1))

colours <- c("Transmission season" = "brown3", "Transmission unlikely" = "blue3", 
             "Shoulder" = "darkorange2")

years <- seq(as.numeric(format(min(dseq), format="%Y")), as.numeric(format(max(dseq), format="%Y")), by=1)

newnew_col <- new_col

ddm1 <- shift(dm1, delta=7)
ddm1

trial <- trial[-(1:181),]

ggplot(trial, aes(trial[,8], y=trial[,7]))+
  geom_tile(aes(fill=trial[,6]))+
  scale_fill_manual(values=colours)+
  geom_hline(yintercept=yearbreaks)+
  #scale_y_reverse(breaks=years)+
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


date <- as.Date(Sys.Date()-2, format = "%d-%m-%Y")

chdu <- paste("C:/Users/a1667856/Box/PhD/HDU Mapping/hdu_mapping/hdumaps/", 
              "chdu", format(date, format = "%Y%m%d"), 
              ".tif", sep="")

chdu.r <- rast(chdu)

bbox_new <- st_bbox(chdu.r)
xrange <- bbox_new$xmax - bbox_new$xmin # range of x values
yrange <- bbox_new$ymax - bbox_new$ymin # range of y values

# bbox_new[1] <- bbox_new[1] - (0.25 * xrange) # xmin - left
bbox_new[3] <- bbox_new[3] + (0.25 * xrange) # xmax - right
# bbox_new[2] <- bbox_new[2] - (0.25 * yrange) # ymin - bottom
bbox_new[4] <- bbox_new[4] + (0.2 * yrange) # ymax - top
bbox_new <- bbox_new %>%  # take the bounding box ...
  st_as_sfc() #and make it a sf polygon

subs.mat <- matrix(data=c(0, 120, 0, 120, 130, 1, 130, 1000, 2),
                   ncol=3, byrow=TRUE)
Rn <- classify(chdu.r, subs.mat, include.lowest=TRUE, right=FALSE)
plot(Rn)

pal <-c("royalblue3", "goldenrod2","firebrick3")
plot(Rn, col=pal)        

tmap_mode("plot")
tm_shape(chdu.r, bbox=bbox_new)+
  tm_raster(n=3,
            palette=pal,
            breaks = c(0, 120, 130, Inf))+
  tm_layout(legend.position = c("right", "bottom"), 
            title= paste('Cumulative HDUs', date, sep=" "), 
            title.position = c('right', 'top'))

##_______________________
#Bind the dataframes

newmax <- readRDS("C:/Users/a1667856/Box/PhD/HDU Mapping/hdu_mapping/newpoa20152021max.RDS")

poa2022max <- poa20152022max[-c(1:2557),]

newpoa20152022max <- rbind(newmax, poa2022max)


med <- readRDS("2022med.RDS")
med <- rbind(med, readRDS("newmed.RDS"))
saveRDS(med, "poa20152022med.RDS")



##
newpoa20152022max <- newpoa20152022max[-2874,]


#____________________________
library(plotly); library(sf)

nc <- sf::st_read()

date <- as.Date("2022-02-01")

output$binaryoutput <- renderPlot({
  chdu <- paste("C:/Users/a1667856/Box/PhD/HDU Mapping/hdu_mapping/hdumaps/", 
                "chdu", format(date, format = "%Y%m%d"), 
                ".tif", sep="")
  
  chdu.r <- rast(chdu)
  
  bbox_new <- st_bbox(chdu.r)
  xrange <- bbox_new$xmax - bbox_new$xmin # range of x values
  yrange <- bbox_new$ymax - bbox_new$ymin # range of y values
  
  # bbox_new[1] <- bbox_new[1] - (0.25 * xrange) # xmin - left
  bbox_new[3] <- bbox_new[3] + (0.15 * xrange) # xmax - right
  # bbox_new[2] <- bbox_new[2] - (0.25 * yrange) # ymin - bottom
  bbox_new[4] <- bbox_new[4] + (0.15 * yrange) # ymax - top
  bbox_new <- bbox_new %>%  # take the bounding box ...
    st_as_sfc() #and make it a sf polygon
  
  # subs.mat <- matrix(data=c(0, 120, 0, 120, 130, 1, 130, 1000, 2),
  #                    ncol=3, byrow=TRUE)
  # Rn <- classify(chdu.r, subs.mat, include.lowest=TRUE, right=FALSE)
  pal <-c("royalblue3", "goldenrod2","firebrick3")
  
  tmap_mode("plot")
  tm_shape(chdu.r, bbox=bbox_new)+
    tm_raster(n=3,
              palette=pal,
              breaks = c(0, 120, 130, Inf))+
    tm_layout(legend.position = c("right", "top"), 
              title= paste('30-day cumulative HDUs on', date, sep=" "), 
              title.position = c('right', 'top'))
})


library(raster);library(leaflet);library(terra)

chdu.r <- raster(chdu)

#chdu.r <- leaflet::projectRasterForLeaflet(chdu.r, method="bilinear")
plot(chdu.r)

values <- getValues(chdu.r)


pal <- colorBin(c("royalblue3", "goldenrod2", "firebrick3"), bins=c(0, 120, 130, Inf),
                    na.color = "transparent")

x <- leaflet() %>% addTiles() %>%
  addRasterImage(chdu.r, colors = pal, opacity = 0.6) %>%
  addLegend(pal = pal,
            position = "bottomright",
            values = values,
            labels = c("<120" = "royalblue3", "120-130" = "goldenrod2", ">130" = "firebrick3"),
            title = "cHDU")



#_________________________________________
library(shiny); library(lubridate); library(DT); library(ggplot2); library(sf);
library(sp); library(cropgrowdays); library(RColorBrewer); library(dplyr); library(scales);
library(maptools); library(raster); library(sf); library(sp); library(rgeos); library(rgdal); library(devtools)
library(terra); library(rasterVis); library(tmap); library(tmaptools)
library(plyr); library(busdater); library(stringr); library(dplyr); library(leaflet)

dates <- as.Date(Sys.Date()-2)

chdu <- paste("C:/Users/a1667856/Box/PhD/HDU Mapping/hdu_mapping/hdumaps/",
                "chdu", format(dates, format = "%Y%m%d"),
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
            labels = c("Transmission not possible" = "royalblue3", "Transmission unlikely" = "goldenrod2", "Transmission possible" = "firebrick3"),
            title = "cHDU")

