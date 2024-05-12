#############################################################################################
#############################################################################################
##Package
library(raster)
library(stats)
library(ggplo2)
library(corrplot)
library(dply) ## Data Manipulation
library(tmap)
##Package Machine Learning
library(MASS)
library(randomForest)
library(caret)
library(e1071)
library(stats)
library(kernlab)
library(clue)
library(gbm)
library(mlr)
library(ranger)
library(parallelMap)
library(cmaes)
library(sf)            # classes and functions for vector data
library(raster)        # classes and functions for raster data
library(spData)        # load geographic data
library(psych)
library(spDataLarge)   # load larger geographic data
#############################################################################################

##Memanggil file
#bisa memanggil dengan seperti ini
#Variable < - (lokasi)
#variable baru <- (memanggil file)
fileSurvey <- "./XYSurvey.csv"

shpjakarta <- shapefile("./Jakarta.shp")
rsLandsat <- stack("./LandsatJakarta.tif")
vcSurvey <- read.csv(fileSurvey, sep=",")


plotRGB(rsLandsat, b = 2, g=3, r=4, stretch="lin")



new.jakarta.shp <- st_set_crs(shpjakarta,4326)

st_crs(shpjakarta)
st_crs(rsLandsat)

rsLandsatClip = mask(rsLandsat, shpjakarta)
plotRGB(rsLandsatClip, b=2, g=3, r=4, stretch="lin")

#mengganti nama attribut
names(rsLandsatClip) <- c('Coastal', 'Blue', 'Green'
                          ,'Red', 'NIR', 'SWIR', 'SWIR2','Panchromatic', 'Cirrus')

plotRGB(rsLandsatClip, b=2, g=3, r=4, stretch="lin")


##Memanggil Coordinat
coordinates(vcSurvey) <- c("POINT_X", "POINT_Y")
projection(vcSurvey)  <-CRS("+proj=utm +zone=48 +south +datum=WGS84")

spTransform(vcSurvey, CRS(" +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))


df.Survey <-  as.data.frame(extract(x= rsLandsatClip, y= vcSurvey,
                                    methods='bilinear'), df=TRUE,
                                        na.rm=F)
df.Survey$buildingDens <- vcSurvey$PBD
df.Survey <- na.omit(df.Survey)

pairs(df.Survey)

#Grafik menarik
pairs.panels(df.Survey,
             #bg=c("red","yellow","black")[df.Survey$ClassID],
             method = "pearson",
             main = "Data Landsat8",
             hist.col = "#00AFBB",
             density = T,
             ellipses = T)

#CARET
#Tidak perlu uji akurasi menggunakan code ct caret::
#hanya run model sudah dapat
train.control <- trainControl(method = "LOOCV")
model <- caret::train(buildingDens ~. , data = df.Survey, method = "ranger",
                      trControl = train.control)
model
rsMod <- predict(rsLandsatClip, model)
plot(rsMod)
model2 <- caret::train(Class ~SPOTGiliAir.1 + SPOTGiliAir.2 + 
                         SPOTGiliAir.3 + SPOTGiliAir.4, data = df.eksPL, method = "vglmAdjCat",
                       trControl = train.control)


model
#############################################################################################
##Model Bayes
#Membangun Fungsi untuk membagi data menjadi taining n test
traintest <- function(data, nsample){
  ## 75% of the sample size
  smp_size <- floor(nsample * (nrow(data)*0.01))
  
  ## set the seed to make your partition reproducible
  set.seed(123)
  train_ind <- sample(seq_len(nrow(data)), size = smp_size)
  
  train <- data[train_ind, ]
  test  <- data[-train_ind, ]
  
  return(list(train = train, test = test))
}

trainRaster <- traintest(df.Survey, 70)

trainRaster$train$buildingDens <- as.character(trainRaster$train$buildingDens)
trainRaster$test$buildingDens<- as.character(trainRaster$test$buildingDens)

model.rs.nb <- naiveBayes(buildingDens ~., data = trainRaster$train)


summary(model.rs.nb)

predict.rs.nb <- raster::predict(rsLandsatClip, model.rs.nb)

plot(predict.rs.nb)

##SVMN
model.rs.svm <- svm(buildingDens ~., data = trainRaster$train)
summary(model.rs.svm)

predict.rs.svm <- predict(model.rs.svm, traintesteksPL$test)
rsSPOT$svm <- raster::predict(rsSPOT, model.rs.svm)
plot(rsSPOT$svm)
############################################################################################
predict.rs.nb <- predict(model.rs.nb, traintesteksPL$test)
rsSPOT$nb <- raster::predict(rsSPOT, model.rs.nb)


############################################################################################
tmap_mode("view")
m_tmview = map_Jakarta
tmap_leaflet(m_tmview)
tm_shape(rsMod) + tm_raster(alpha = 0.5) + tm_shape(shpjakarta) + tm_borders()
