# Analysis and Classification of SAR Textures using Information Theory

#### [Eduarda C. Chagas](mailto:eduarda.chagas@dcc.ufmg.br), [Alejandro C. Frery](mailto:acfrery@laccan.ufal.br), [Osvaldo A. Rosso](mailto:oarosso@gmail.com), and [Heitor S. Ramos](mailto:ramosh@dcc.ufmg.br)


### This repository contains all the data and code used to develop our research in the related paper submitted to J-STARS. 

---

#### Abstract

The use of Bandt-Pompe probability distributions and descriptors of Information Theory has been presenting satisfactory results with low computational cost in the time series analysis literature. However, these tools have limitations when applied to data without time dependency. Given this context, we present a newly proposed technique for texture analysis and classification based on the Bandt-Pompe symbolization for SAR data. It consists of (i) linearize a 2-D patch of the image using the Hilbert-Peano curve, (ii) build an Ordinal Pattern Transition Graph that considers the data amplitude encoded into the weight of the edges; (iii) obtain a probability distribution function derived from this graph; (iv) compute Information Theory descriptors (Permutation Entropy and Statistical Complexity) from this distribution and use them as features to feed a classifier. The ordinal pattern graph we propose considers that the weight of the edges is related to the absolute difference of observations, which encodes the information about the data amplitude. This modification takes into account the scattering properties of the target and leads to the characterization of several types of textures. Experiments with data from Munich urban areas, Guatemala forest regions and Cape Canaveral ocean samples show the effectiveness of our technique in homogeneous areas, which achieves satisfactory levels of separability. The two descriptors chosen in this work are easy and quick to calculate and are used as input for a k-nearest neighbor classifier. Experiments show that this technique presents results similar to state-of-the-art techniques that employ a much larger number of features and, consequently, require a higher computational cost.

#### Methodology

Our procedure consists of the following steps:

	1. extract the HHHH backscatter magnitudes of quad-polarimetric L-band SAR image, obtaining the image texture;

	2. linearizing a 2-D patch of data using the Hilbert-Peano curve;

	3. employing the Bandt-Pompe symbolization to generate the set of ordinal patterns for each data sequence;

	4. building the Ordinal Pattern Transition Graph with weighted edges to obtain a probability distribution function derived from this graph;

	5. computing the Entropy and Statistical Complexity of this distribution and, finally, classify regions.
	
<img src="/Figures/AnalysisSARTextures.png" />


#### Datasets

For this analysis, we used the HH backscatter magnitudes of three quad-polarimetric L-band SAR images from the NASA Jet Propulsion Laboratory’s (JPL’s) uninhabited aerial vehicle synthetic aperture radar (UAVSAR) sensor with L = 36 nominal looks, available at <a href="https://uavsar.jpl.nasa.gov/cgi-bin">jet propulsion laboratory</a>:

- Forest and pasture region of <a href="https://uavsar.jpl.nasa.gov/cgi-bin/product.pl?jobName=Lacand_30202_15043_006_150410_L090_CX_01#dados">Sierra del Lacandón National Park, Guatemala, (acquired on April 10, 2015)</a>. The image has 8917 x 3300 pixels with 10m x 2m resolution.

- Ocean regions from Cape Canaveral Ocean (acquired on September 22, 2016). The image has 7038 x 3300 pixels with 10m x 2m resolution.

- Urban area of the city of <a href="https://uavsar.jpl.nasa.gov/cgi-bin/product.pl?jobName=munich_19417_15088_002_150605_L090_CX_01#data">Munich, Germany (acquired on June 5, 2015)</a>. The image has 5773 x 3300 pixels with 10m x 3m resolution.

We manually selected 200 samples of size 128 x 128 to compose the dataset used in the experiments. It is organized as follows:

- 40 samples from Guatemalan forest regions;
- 40 samples from Guatemalan pasture regions;
- 80 samples from the oceanic regions of Cape Canaveral, divided into two types with different contrast; and
- 40 samples of urban regions of the city of Munich.

#### The repository is organized as follows:
- `/Code` - the scripts used to develop our research; 
- `/Data` - the auxiliary data used during analysis; 
- `/Figures`- Illustrations used in final report; 
- `/Images`- Illustration of the results obtained throughout the research, alongside the methodology files corresponding to our *overview* figure; 
- `/Publications`- the scientific reports developed during the study. 

### Software requirements

This code version is tested on the Linux operating system Ubuntu 18.10.

**Installing R 3.6.0 on Ubuntu 18.10**

```sh
$ sudo add-apt-repository 'deb https://cloud.r-project.org/bin/linux/ubuntu disco-cran35/'
$ sudo apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E298A3A825C0D65DFD57CBB651716619E084DAB9
$ sudo apt update
$ sudo apt install r-base-dev
```

### Installation Guide

Prior to running the script experiments, we need to install the following required packages for R: 

``
install.packages(c('devtools', 'ggplot2', 'ggthemes', 'ggpubr', 'gtools', 'igraph', 'statcomp'))
``

The latest version of these packages was used by October 2019:

```
devtools       2.2.2 
ggplot2        3.3.0       
ggthemes       4.2.0      
ggpubr         0.2.5  
gtools         3.8.2      
igraph         1.2.4.1       
statcomp       0.0.1.1000  
caret          6.0.84
EnvStats       2.3.1
MLmetrics      1.1.1
mltest         1.0.1
ggrepel        0.8.2
latex2exp      0.4.0
raster         2.9.5
```

### Additional files

- [Submitted file](<https://github.com/EduardaChagas/SAR-WATG/blob/master/Publications/JSTARS%202020/SARTexture-IT.R1.pdf>)

- [References](<http://htmlpreview.github.io/?https://github.com/EduardaChagas/SAR-WATG/blob/master/Publications/JSTARS%202020/ReferencesR0.html>)

---


Finally, if you have any questions or you want to report anything, feel free to reach me at: eduarda.chagas@dcc.ufmg.br. 






