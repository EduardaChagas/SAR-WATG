############################################# Packages #################################################

if(!require(gtools)) install.packages("gtools")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(ggthemes)) install.packages("ggthemes")
if(!require(ggpubr)) install.packages("ggpubr")

###################################### Plots Generator #################################################
hilbertcurve = unlist(read.table("../Data/Hilbert/HilbertCurves.txt")) + 1

#Guatemala
dimen.guatemala = c(5150, 128, 2700, 128)
sar_data = raster(paste("../Data/", "guatemala", "/HHHH", ".grd", sep = ""))
img = getValuesBlock(sar_data, row = dimen.guatemala[1], nrows = dimen.guatemala[2], col = dimen.guatemala[3], ncols = dimen.guatemala[4], format = "matrix")
ts = img[hilbertcurve]/max(img[hilbertcurve])
guatemala.data = data.frame(index = c(1:length(ts)), observation = ts)

p.guatemala = ggplot(guatemala.data, aes(x = index, y = observation)) +
              geom_line() + 
              labs(x="Index", y="Observation") +
              ggtitle("Guatemala") + 
              theme_few(base_size = 14, base_family = "serif")  + 
              theme(plot.title = element_text(hjust=0.5)) + 
              scale_colour_few("Dark")

#Cape 1
dimen.canaveral.behavior1 = c(50, 128, 1700, 128)
sar_data = raster(paste("../Data/", "cape", "/HHHH", ".grd", sep = ""))
img = getValuesBlock(sar_data, row = dimen.canaveral.behavior1[1], nrows = dimen.canaveral.behavior1[2], col = dimen.canaveral.behavior1[3], ncols = dimen.canaveral.behavior1[4], format = "matrix")
ts = img[hilbertcurve]/max(img[hilbertcurve])
cape.data.1 = data.frame(index = c(1:length(ts)), observation = ts)

p.cape.1 = ggplot(cape.data.1, aes(x = index, y = observation)) +
          geom_line() + 
          labs(x="Index", y="Observation") +
          ggtitle("Canaveral Ocean - Contrast type 1") + 
          theme_few(base_size = 14, base_family = "serif")  + 
          theme(plot.title = element_text(hjust=0.5)) + 
          scale_colour_few("Dark")

#Cape 2
dimen.canaveral.behavior2 = c(250, 128, 1, 128)
sar_data = raster(paste("../Data/", "cape", "/HHHH", ".grd", sep = ""))
img = getValuesBlock(sar_data, row = dimen.canaveral.behavior2[1], nrows = dimen.canaveral.behavior2[2], col = dimen.canaveral.behavior2[3], ncols = dimen.canaveral.behavior2[4], format = "matrix")
ts = img[hilbertcurve]/max(img[hilbertcurve])
cape.data.2 = data.frame(index = c(1:length(ts)), observation = ts)

p.cape.2 = ggplot(cape.data.2, aes(x = index, y = observation)) +
          geom_line() + 
          labs(x="Index", y="Observation") +
          ggtitle("Canaveral Ocean - Contrast type 2") + 
          theme_few(base_size = 14, base_family = "serif")  + 
          theme(plot.title = element_text(hjust=0.5)) + 
          scale_colour_few("Dark")
#Munich
dimen.munich= c(3000, 128, 400, 128)
sar_data = raster(paste("../Data/", "munich", "/HHHH", ".grd", sep = ""))
img = getValuesBlock(sar_data, row = dimen.munich[1], nrows = dimen.munich[2], col = dimen.munich[3], ncols = dimen.munich[4], format = "matrix")
ts = img[hilbertcurve]/max(img[hilbertcurve])
munich.data = data.frame(index = c(1:length(ts)), observation = ts)

p.munich = ggplot(munich.data, aes(x = index, y = observation)) +
          geom_line() + 
          labs(x="Index", y="Observation") +
          ggtitle("Munich Urban Area") + 
          theme_few(base_size = 14, base_family = "serif")  + 
          theme(plot.title = element_text(hjust=0.5)) + 
          scale_colour_few("Dark")

pdf("SARTS.pdf", width = 18, height = 5) 
ggarrange(p.guatemala, p.cape.1, p.cape.2, p.munich,
          ncol = 4, nrow = 1, common.legend = TRUE, legend = "right") + 
  theme_few() + theme(text = element_text(size = 14, family="Times", face="italic"), plot.title = element_text(hjust = 0.5)) + 
  guides(colour = guide_legend(override.aes = list(size = 3)))
dev.off() 