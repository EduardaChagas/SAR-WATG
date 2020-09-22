############################################# Packages #################################################

if(!require(gtools)) install.packages("gtools")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(ggthemes)) install.packages("ggthemes")
if(!require(ggpubr)) install.packages("ggpubr")
if(!require(raster)) install.packages("raster")

###################################### Plots Generator #################################################
hilbertcurve = unlist(read.table("../Data/Hilbert/HilbertCurves.txt")) + 1

#Guatemala
dimen.guatemala = c(5150, 128, 2700, 128)
sar_data = raster(paste("../../Data/", "Guatemala", "/HHHH", ".grd", sep = ""))
img = getValuesBlock(sar_data, row = dimen.guatemala[1], nrows = dimen.guatemala[2], col = dimen.guatemala[3], ncols = dimen.guatemala[4], format = "matrix")
ts = img[hilbertcurve]/max(img[hilbertcurve])
guatemala.data = data.frame(index = c(1:length(ts)), observation = ts)

pdf("tsguatemala.pdf", width = 4, height = 4)
p.guatemala = ggplot(guatemala.data, aes(x = index, y = observation)) +
              geom_line() + 
              labs(x="Index", y="Observation") +
              #ggtitle("Forest Area") + 
              theme_few(base_size = 14, base_family = "serif")  + 
              theme(plot.title = element_text(hjust=0.5)) + 
              scale_colour_few("Dark")
p.guatemala
dev.off() 

#Cape 1
dimen.canaveral.behavior1 = c(50, 128, 1700, 128)
sar_data = raster(paste("../../Data/", "Cape", "/HHHH", ".grd", sep = ""))
img = getValuesBlock(sar_data, row = dimen.canaveral.behavior1[1], nrows = dimen.canaveral.behavior1[2], col = dimen.canaveral.behavior1[3], ncols = dimen.canaveral.behavior1[4], format = "matrix")
ts = img[hilbertcurve]/max(img[hilbertcurve])
cape.data.1 = data.frame(index = c(1:length(ts)), observation = ts)

pdf("tscape1.pdf", width = 4, height = 4) 
p.cape.1 = ggplot(cape.data.1, aes(x = index, y = observation)) +
          geom_line() + 
          labs(x="Index", y="Observation") +
          #ggtitle("Ocean Area - Contrast type 1") + 
          theme_few(base_size = 14, base_family = "serif")  + 
          theme(plot.title = element_text(hjust=0.5)) + 
          scale_colour_few("Dark")
p.cape.1
dev.off() 

#Cape 2
dimen.canaveral.behavior2 = c(150, 128, 550, 128)
sar_data = raster(paste("../../Data/", "Cape", "/HHHH", ".grd", sep = ""))
img = getValuesBlock(sar_data, row = dimen.canaveral.behavior2[1], nrows = dimen.canaveral.behavior2[2], col = dimen.canaveral.behavior2[3], ncols = dimen.canaveral.behavior2[4], format = "matrix")
ts = img[hilbertcurve]/max(img[hilbertcurve])
cape.data.2 = data.frame(index = c(1:length(ts)), observation = ts)

pdf("tscape2.pdf", width = 4, height = 4) 
p.cape.2 = ggplot(cape.data.2, aes(x = index, y = observation)) +
          geom_line() + 
          labs(x="Index", y="Observation") +
          #ggtitle("Ocean Area - Contrast type 2") + 
          theme_few(base_size = 14, base_family = "serif")  + 
          theme(plot.title = element_text(hjust=0.5)) + 
          scale_colour_few("Dark")
p.cape.2
dev.off() 

#Munich
dimen.munich= c(3000, 128, 400, 128)
sar_data = raster(paste("../../Data/", "Munich", "/HHHH", ".grd", sep = ""))
img = getValuesBlock(sar_data, row = dimen.munich[1], nrows = dimen.munich[2], col = dimen.munich[3], ncols = dimen.munich[4], format = "matrix")
ts = img[hilbertcurve]/max(img[hilbertcurve])
munich.data = data.frame(index = c(1:length(ts)), observation = ts)

pdf("tsmunich.pdf", width = 4, height = 4) 
p.munich = ggplot(munich.data, aes(x = index, y = observation)) +
          geom_line() + 
          labs(x="Index", y="Observation") +
          #ggtitle("Urban Area") + 
          theme_few(base_size = 14, base_family = "serif")  + 
          theme(plot.title = element_text(hjust=0.5)) + 
          scale_colour_few("Dark")
p.munich
dev.off() 

#Pasture
dimen.pasture = c(1, 128, 910, 128)
sar_data = raster(paste("../../Data/", "Guatemala", "/HHHH", ".grd", sep = ""))
img = getValuesBlock(sar_data, row = dimen.pasture[1], nrows = dimen.pasture[2], col = dimen.pasture[3], ncols = dimen.pasture[4], format = "matrix")
ts = img[hilbertcurve]/max(img[hilbertcurve])
pasture.data = data.frame(index = c(1:length(ts)), observation = ts)

pdf("tspasture.pdf", width = 4, height = 4) 
p.pasture = ggplot(pasture.data, aes(x = index, y = observation)) +
  geom_line() + 
  labs(x="Index", y="Observation") +
  #ggtitle("Pasture Area") + 
  theme_few(base_size = 14, base_family = "serif")  + 
  theme(plot.title = element_text(hjust=0.5)) + 
  scale_colour_few("Dark")
p.pasture
dev.off() 

#pdf("SAR_TS.pdf", width = 18, height = 4) 
#ggarrange(p.guatemala, p.cape.1, p.cape.2, p.munich, p.pasture,
#          ncol = 5, nrow = 1, common.legend = TRUE, legend = "right") + 
#  theme_few() + theme(text = element_text(size = 14, family="Times", face="italic"), plot.title = element_text(hjust = 0.5)) + 
#  guides(colour = guide_legend(override.aes = list(size = 3)))
#dev.off() 


dimen.guatemala = c(5150, 128, 2700, 128)
sar_data = raster(paste("../../Data/", "Guatemala", "/HHHH", ".grd", sep = ""))
img = getValuesBlock(sar_data, row = dimen.guatemala[1], nrows = dimen.guatemala[2], col = dimen.guatemala[3], ncols = dimen.guatemala[4], format = "matrix")
ts = img[hilbertcurve]/max(img[hilbertcurve])
guatemala.data = data.frame(index = c(1:length(ts)), observation = ts)

pdf("guatemala-hist.pdf", width = 12, height = 10)
p = ggplot(guatemala.data) +
  #ggtitle("First Component Histogram") +
  xlab("") +
  ylab("") +
  geom_histogram(aes(x = observation, y = ..density..), binwidth = 0.01, fill="#212529", color="#343a40") +
  theme_few(base_size = 35, base_family = "serif") +  
  theme(plot.title = element_text(hjust=0.5)) 
#coord_cartesian(xlim = c(-10, 10))
print(p)
dev.off()

dimen.canaveral.behavior1 = c(50, 128, 1700, 128)
sar_data = raster(paste("../../Data/", "Cape", "/HHHH", ".grd", sep = ""))
img = getValuesBlock(sar_data, row = dimen.canaveral.behavior1[1], nrows = dimen.canaveral.behavior1[2], col = dimen.canaveral.behavior1[3], ncols = dimen.canaveral.behavior1[4], format = "matrix")
ts = img[hilbertcurve]/max(img[hilbertcurve])
cape.data.1 = data.frame(index = c(1:length(ts)), observation = ts)

pdf("cape1-hist.pdf", width = 12, height = 10)
p = ggplot(cape.data.1) +
  #ggtitle("First Component Histogram") +
  xlab("") +
  ylab("") +
  geom_histogram(aes(x = observation, y = ..density..), binwidth = 0.01, fill="#212529", color="#343a40", alpha=0.9) +
  theme_few(base_size = 35, base_family = "serif") +  
  theme(plot.title = element_text(hjust=0.5)) 
#coord_cartesian(xlim = c(-10, 10))
print(p)
dev.off()

dimen.canaveral.behavior2 = c(150, 128, 550, 128)
sar_data = raster(paste("../../Data/", "Cape", "/HHHH", ".grd", sep = ""))
img = getValuesBlock(sar_data, row = dimen.canaveral.behavior2[1], nrows = dimen.canaveral.behavior2[2], col = dimen.canaveral.behavior2[3], ncols = dimen.canaveral.behavior2[4], format = "matrix")
ts = img[hilbertcurve]/max(img[hilbertcurve])
cape.data.2 = data.frame(index = c(1:length(ts)), observation = ts)

pdf("cape2-hist.pdf", width = 12, height = 10)
p = ggplot(cape.data.2) +
  #ggtitle("First Component Histogram") +
  xlab("") +
  ylab("") +
  geom_histogram(aes(x = observation, y = ..density..), binwidth = 0.01, fill="#212529", color="#343a40", alpha=0.9) +
  theme_few(base_size = 35, base_family = "serif") +  
  theme(plot.title = element_text(hjust=0.5)) 
#coord_cartesian(xlim = c(-10, 10))
print(p)
dev.off()

dimen.munich= c(3000, 128, 400, 128)
sar_data = raster(paste("../../Data/", "Munich", "/HHHH", ".grd", sep = ""))
img = getValuesBlock(sar_data, row = dimen.munich[1], nrows = dimen.munich[2], col = dimen.munich[3], ncols = dimen.munich[4], format = "matrix")
ts = img[hilbertcurve]/max(img[hilbertcurve])
munich.data = data.frame(index = c(1:length(ts)), observation = ts)

pdf("munich-hist.pdf", width = 12, height = 10)
p = ggplot(munich.data) +
  #ggtitle("First Component Histogram") +
  xlab("") +
  ylab("") +
  geom_histogram(aes(x = observation, y = ..density..), binwidth = 0.01, fill="#212529", color="#343a40", alpha=0.9) +
  theme_few(base_size = 35, base_family = "serif") +  
  theme(plot.title = element_text(hjust=0.5)) 
#coord_cartesian(xlim = c(-10, 10))
print(p)
dev.off()

dimen.pasture = c(1, 128, 910, 128)
sar_data = raster(paste("../../Data/", "Guatemala", "/HHHH", ".grd", sep = ""))
img = getValuesBlock(sar_data, row = dimen.pasture[1], nrows = dimen.pasture[2], col = dimen.pasture[3], ncols = dimen.pasture[4], format = "matrix")
ts = img[hilbertcurve]/max(img[hilbertcurve])
pasture.data = data.frame(index = c(1:length(ts)), observation = ts)

pdf("pasture-hist.pdf", width = 12, height = 10)
p = ggplot(pasture.data) +
  #ggtitle("First Component Histogram") +
  xlab("") +
  ylab("") +
  geom_histogram(aes(x = observation, y = ..density..), binwidth = 0.01, fill="#212529", color="#343a40", alpha=0.9) +
  theme_few(base_size = 35, base_family = "serif") +  
  theme(plot.title = element_text(hjust=0.5)) 
#coord_cartesian(xlim = c(-10, 10))
print(p)
dev.off()