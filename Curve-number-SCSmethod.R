#Convert land use and soil to curve number to use in SCS method, 
#conversion of rain to effective runoff. Method developed by the USDA 
#(https://www.nrcs.usda.gov/Internet/FSE_DOCUMENTS/stelprdb1044171.pdf)

#Data:
  #Land use from the Mapbiomas Landsat classification collection, from GEE: https://mapbiomas.org/
  
library(terra)    

dir.create(path = 'temp', showWarnings = FALSE) #To create a temp directory to exclude in the final

terraOptions(tempdir = 'temp')

lista <- list.files('MAPBIOMAS-EXPORT', full.names = TRUE, pattern = "saopaulo")

uses_orig = terra::rast(lista)

#Can be adjusted for an area based on interest
grid_standard = terra::rast(xmin=-49.270, xmax =-48.843, ymin =-21.490, ymax =-21.147, resolution = c(0.001, 0.001), vals = 0)

#Reduce the amount of data for easier processing, adjustable as needed (grid). 
mapbio_resample = terra::resample(uses_orig, grid_standard, method="near") 

# Values reclass/Land use	----- (MAPBIOMAS) 
# There are different classes depending on the collection. 

#10	Urban (24)
#20	Agriculture (18, 19, 20, 21, 36, 39, 41)
#30	Forest (3, 4, 5)
#40	Silviculture (9)
#50	Grassland  (10, 11, 12, 13, 32)
#60	Uncover (23,25, 29, 30)
#70	Pasture (15)
#100 Water (27,31,33)


# multiple replacements
class_map_scs <- rbind(c(24, 10), c(20, 20), c(21, 20), c(36, 20), c(39, 20), c(41, 20),
                       c(3, 30), c(4, 30), c(5, 30), c(9, 40), c(11, 50), c(12, 50), 
                       c(13, 50), c(32, 50), c(23, 60), c(25, 60), c(29, 60), c(30, 60),
                       c(15, 70), c(31, 100), c(33, 100))


#Reclassify uses based in the matrix
rc_map_scs = classify(mapbio_resample, class_map_scs)

#Remove 0 
rc_map_scs[rc_map_scs <= 0] = NA

#plot(rc_map_scs$classification_2000, type = "classes")


################### Soil raster ###############

#SCS Soil group from SCS method	A = (1)	B = (2)	C = (3)	D = (4) 
# Layers from Soil Grid
# See my script to download in Google Earth Engine
# https://github.com/lvsantarosa/Soil-Grid-on-Google-Earth-Engine

Clay <- terra::rast('clay.tif')
Sand <- terra::rast('sand.tif')
Silt <- terra::rast('silt.tif')

#Convert to percentage

Clay <- terra::mean(Clay)/1000
Sand <- terra::mean(Sand)/1000
Silt <- terra::mean(Silt)/1000

# Reclassify the values into groups occording to textural triangle
# https://hess.copernicus.org/preprints/hess-2017-13/hess-2017-13.pdf

Sand_m <- matrix(c(0.55, 1   , 10,
                   0.01, 0.55, 20), ncol = 3, byrow=TRUE)

Sand_rec <- classify(Sand, Sand_m, include.lowest=TRUE)


Clay_m <- matrix(c(0.01, 0.25, 100,
                   0.25, 0.35, 200,
                   0.35, 1   , 300), ncol = 3, byrow=TRUE)

Clay_rec <- classify(Clay, Clay_m, include.lowest=TRUE)


Silt_m <- matrix(c(0.01, 0.75, 1000,
                   0.75, 1   , 2000), ncol = 3, byrow=TRUE)

Silt_rec <- classify(Silt, Silt_m, include.lowest=TRUE)


#Sum the values
Sum_ly <- Sand_rec + Clay_rec + Silt_rec

#Discriminate the groups based in the sum result
grupos <- rbind(c(1110, 1),
                c(1220, 1),
                c(1310, 1),
                c(1120, 2),
                c(2120, 2),
                c(1210, 3),
                c(1320, 4))

Soil_Hidro <- classify(Sum_ly, grupos)

crs <- "+proj=longlat +datum=WGS84"
Soil_Hidro <- project(Soil_Hidro, crs)

#terra::writeRaster(Soil_Hidro, "Soil_Hidro.tif", overwrite = TRUE)

Soil_resample= resample(Soil_Hidro, grid_standard, method="near")

Soil_resample[Soil_resample <= 0] = NA

#plot(Soil_resample)

sum_soil_uses = Soil_resample + rc_map_scs

#plot(sum_soil_uses$lyr1, type = "classes")


#####################CN final##########################

# Values based in the method => https://www.nrcs.usda.gov/Internet/FSE_DOCUMENTS/stelprdb1044171.pdf

#	SCS method values 				
#	Uses/Soil     A	  B	  C	  D
#	Urban	        89	92	94	95
#	Crops	        64	75	82	85
#	Wood	        32	58	72	79
#	Silviculture	32	58	72	79
#	Herbaceus	    48	62	71	85
#	Bare Soil	    77	86	91	94
#	Pasture	      39	61	74	80
#	No Data	      0	  0	  0	  0

#	Sum Soil and Land Use -> to reclassify using the SCS values above				
#	Uses/Soil	    A	  B	  C	  D
#	Urban	        11	12	13	14
#	Crops	        21	22	23	24
#	Florest	      31	32	33	34
#	Silviculture	41	42	43	44
#	Herbaceus	    51	52	53	54
#	Baresoil	    61	62	63	64
#	Pasture	      71	72	73	74
#	No Data	      101	102	103	104


class_cn <- rbind(c(11,89), c(12,64), c(13,32), c(14,32), c(21,48),
                  c(22,92), c(23,75), c(24,58), c(31,58), c(32,62),
                  c(33,94), c(34,82), c(41,72), c(42,72), c(43,71),
                  c(44,95), c(51,85), c(52,79), c(53,79), c(54,85),
                  c(61,77), c(62,86), c(63,91), c(64,94), c(71,39), 
                  c(72,61), c(73,74), c(74,80), c(101,0), c(102,0),
                  c(103,0), c(104,0))

#Reclassify
cn_final = classify(sum_soil_uses, class_cn)

#Names
Nomes = seq(from = 2019, to = 2020)
names(cn_final) = Nomes

cn_final[cn_final <= 0] = NA

#plot(cn_final$`2020`, type = "classes")

library(glue)
library(dplyr)

#Export in different archives
filename = glue("Results/CN_{Nomes}.tif")
cn_final %>% writeRaster(filename = filename, overwrite = TRUE)

###########################################################
unlink(x = list.files('temp', full.names = TRUE)) #apaga os temporararios
