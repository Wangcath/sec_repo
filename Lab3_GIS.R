##############################################################
## Topic: Practice 1: GIS Basic and Some Packages in R
## Date: September 15, 2025
##############################################################

# Load the required libraries

library(sf)
library(raster)
library(crsuggest)
library(osmdata)
library(cancensus)
library(readr)
library(dplyr)
library(magrittr)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggplot2)
library(viridis)

# 设置中文字体支持函数

if (Sys.info()["sysname"] == "Darwin") {
  # macOS系统
  theme_set(theme_minimal() + theme(text = element_text(family = "PingFang SC")))
  par(family = "PingFang SC")
} else if (Sys.info()["sysname"] == "Windows") {
  # Windows系统
  theme_set(theme_minimal() + theme(text = element_text(family = "SimHei")))
  par(family = "SimHei")
}
  

#####################################################
#                  1. Vector Data 
#####################################################


# 1.1 Point

# a single point with XY coordinates of 1,3 in unspecified coordinate system

a_single_point <- st_point(x = c(1,3)) # create a single sfg point object
a_single_point # view the content of the object

attributes(a_single_point) #view the different components of an sfg object

#create five sfg point objects

point1 <- st_point(x = c(1,3)) 
point2 <- st_point(x = c(2, 4))
point3 <- st_point(x = c(3, 3))
point4 <- st_point(x = c(4, 3))
point5 <- st_point(x = c(5, 2))

points <- st_sfc(point1,point2,point3,point4,point5) # create a single sfc points object

points

# define the CRS of the points object

## EPSG code = 4326  WGS 84

points_wgs <- st_sfc(point1,point2,point3,point4,point5,crs=4326) # create a single sfc points object, and define the CRS

points_wgs


attributes(points)

plot(points,col="red",pch=16) # map the sfc object using the plot function
plot(points_wgs,col="red",pch=16) # map the sfc object using the plot function

# attributes(points_wgs) # view the different components of an sfc object

points_attribute_data <- data.frame(transport_mode = c("Bicycle","Pedestrian","Motor Vehicle","Motor Vehicle","Motor Vehicle"))

points_attribute_data


### merge with the points object

points_sf <- st_sf(points_attribute_data,geometry=points) # merge the attribute data with the points object

points_sf

plot(points_sf,pch=16)



################################## 1.2 Line #############################


a_single_line_matrix <- rbind(c(1,1), 
                              c(2, 4), 
                              c(3, 3), 
                              c(4, 3), 
                              c(5,2))# create a matrix with 

a_single_line_matrix

a_single_line <-  st_linestring(a_single_line_matrix)
a_single_line

attributes(a_single_line)


plot(a_single_line,col="darkblue")



### with CRS 

line1 <- st_linestring(rbind(c(1,1), 
                             c(2,4), 
                             c(3,3), 
                             c(4,3), 
                             c(5,2)))

line2 <- st_linestring(rbind(c(3,3), 
                             c(3,5), 
                             c(4,5)))

line3 <- st_linestring(rbind(c(2,4), 
                             c(-1,4), 
                             c(-2,-2)))

lines <- st_sfc(line1,line2,line3,crs=4326)

lines



plot(lines,col="darkblue")


### Atributte data

line_attribute_data <- data.frame(road_name = c("Vinmore Avenue","Williams Road","Empress Avenue"), 
                                  speed_limit = c(30,50,40))

line_attribute_data


### attach the attribute data to the lines object

lines_sf <- st_sf(line_attribute_data,geometry=lines)

lines_sf


plot(lines_sf)

plot(lines_sf[1]) # plot the first attribute 
plot(lines_sf[2]) # plot the second attribute
plot(lines_sf['speed_limit']) # plot the third attribute



######################## 1.3 Polygon ##########################

a_polygon_vertices_matrix <- rbind(c(1,1), c(2, 4), c(3, 3), c(4, 3), c(5,2),c(1,1))
a_polygon_list = list(a_polygon_vertices_matrix)
a_polygon_list

a_polygon <-  st_polygon(a_polygon_list)
plot(a_polygon,col="forestgreen")


#### with a hole

lake_vertices_matrix <- rbind(c(1.5, 1.5),c(1.5,1.75),c(1.75,1.75),c(1.75,1.5),c(1.5,1.5))
lake_vertices_matrix


a_single_polygon_with_a_hole_list = list(a_polygon_vertices_matrix,lake_vertices_matrix)
a_single_polygon_with_a_hole_list


a_single_polygon_with_a_hole <-  st_polygon(a_single_polygon_with_a_hole_list)

plot(a_single_polygon_with_a_hole,col="forestgreen")

###### with CRS


park1 <- a_single_polygon_with_a_hole
park2 <- st_polygon(list(
  rbind(
    c(6,6),c(8,7),c(11,9), c(10,6),c(8,5),c(6,6)
  )
))

park_attributes <- data.frame(park_name = c("Zijinshan","Xuanwu Park"))

parks_sf <- st_sf(park_attributes, geometry = st_sfc(park1,park2,crs=4326))

plot(parks_sf)


################################ NOTE ####################################################################
# 1. sfg objects store XY information on a single feature geometry including POINT,LINESTRING, AND POLYGON
# 2. sfc objects store many sfg objects as well as information on the coordinate system the XY information refers to
# 3. sf objects can store attribute data in addition to the spatial information represented in the sfc
##########################################################################################################



#####################################################
#                  2. Raster Data 
#####################################################

library(sf)
library(raster)


# Define the geographic extent for Nanjing City
# Approximate bounding box
min_lon <- 118.5
max_lon <- 119.3
min_lat <- 31.5
max_lat <- 32.5

# Create an sf polygon for the extent

library(sf)

nj_bbox <- st_as_sfc(
  st_bbox(c(
    xmin = min_lon, ymin = min_lat,
    xmax = max_lon, ymax = max_lat
  ), crs = 4326)
)
# Create a simple raster within the bounding box

nj_raster <- raster(
  xmn = min_lon, xmx = max_lon,
  ymn = min_lat, ymx = max_lat,
  res = 0.1, 
  crs = "+proj=longlat +datum=WGS84"
)


nj_raster

num_cells <- ncell(nj_raster)
num_cells  


# Assign random values to the raster cells
values_cells <- runif(ncell(nj_raster))
values_cells

values(nj_raster) <- values_cells

# Plot the raster
plot(nj_raster)

# Add the bounding box to the raster plot
plot(nj_bbox, add = TRUE, border = "red", lwd = 2)

# Print raster information
print(nj_raster)

#################################################################
#     3. Read Spatial Data from Shapefile  of China Maps
###############################################################

#####################################################
### 3.1 GeoPackage 数据读取和操作案例
#####################################################

# GeoPackage是一种开放的地理空间数据格式，可以存储多种几何类型
# 它是SQLite数据库的扩展，支持矢量、栅格和属性数据

# 示例1：创建和读取GeoPackage数据
# 首先加载必要的包

#install.packages("rnaturalearth")
#install.packages("rnaturalearthdata")

#library(sf)  # 确保sf包已加载，包含st_write函数
#library(rnaturalearth)
#library(rnaturalearthdata)

# 获取中国省级数据

china_provinces_gpkg <- ne_states(country = "China", returnclass = "sf")

# 将数据保存为GeoPackage格式
gpkg_file <- "china_provinces.gpkg"
st_write(china_provinces_gpkg, gpkg_file, delete_dsn = TRUE)

# 读取GeoPackage文件

gpkg_data <- read_sf(gpkg_file)
str(gpkg_data)

# 绘制GeoPackage数据
plot(gpkg_data["name"], main = "从GeoPackage读取的中国省级数据")

# 使用ggplot2绘制
ggplot(gpkg_data) +
  geom_sf(aes(fill = name), color = "white", size = 0.3) +
  scale_fill_viridis_d(option = "plasma") +
  labs(title = "从GeoPackage读取的中国省级数据", 
       subtitle = "China Provinces Data from GeoPackage",
       fill = "省份") +
  theme_minimal() +
  theme(text = element_text(family = "chinese"))

# Chinese font support 

library(showtext)
showtext_auto()
showtext_opts(dpi = 96)  
font_add_google("Noto Sans SC","chinese")

# 示例3：多图层GeoPackage操作
# 创建城市点数据
major_cities <- data.frame(
  city = c("北京", "上海", "广州", "深圳", "杭州", "南京", "武汉", "成都", "西安", "重庆"),
  lon = c(116.4, 121.5, 113.3, 114.1, 120.2, 118.8, 114.3, 104.1, 108.9, 106.5),
  lat = c(39.9, 31.2, 23.1, 22.5, 30.3, 32.1, 30.6, 30.7, 34.3, 29.6)
)

cities_sf <- st_as_sf(major_cities, coords = c("lon", "lat"), crs = 4326)

# 保存多图层GeoPackage
multi_layer_file <- "china_multi_layer.gpkg"
st_write(china_provinces_gpkg, multi_layer_file, layer = "provinces", delete_dsn = TRUE)
st_write(cities_sf, multi_layer_file, layer = "cities", append = TRUE)

# 读取多图层GeoPackage
provinces_from_gpkg <- read_sf(multi_layer_file, layer = "provinces")
cities_from_gpkg <- read_sf(multi_layer_file, layer = "cities")

print(paste("省份图层包含", nrow(provinces_from_gpkg), "个要素"))
print(paste("城市图层包含", nrow(cities_from_gpkg), "个要素"))

# 绘制多图层地图并标注城市名称（中文，橘色）
ggplot() +
  geom_sf(data = provinces_from_gpkg, fill = "navy", color = "white", size = 0.3) +
  geom_sf(data = cities_from_gpkg, color = "yellow", size = 2) +
  labs(title = "多图层GeoPackage数据：省份+主要城市", 
       subtitle = "Multi-layer GeoPackage: Provinces + Major Cities") +
  theme_minimal() +
  theme(text = element_text(family = "chinese"))

# 计算各个城市到上海的直线距离

shanghai <- cities_from_gpkg[cities_from_gpkg$city == "上海", ]
distances <- st_distance(cities_from_gpkg, shanghai)
cities_from_gpkg$distance_to_shanghai <- as.numeric(distances) / 1000
print(cities_from_gpkg[order(cities_from_gpkg$distance_to_shanghai), c("city", "distance_to_shanghai")])

# 计算各个城市到上海的球面距离
install.packages("geosphere")
library(geosphere)

# 从geometry列提取经纬度坐标
cities_coords <- st_coordinates(cities_from_gpkg)
shanghai_coords <- st_coordinates(shanghai)

distances_haversine <- distHaversine(cities_coords, shanghai_coords)
cities_from_gpkg$distance_to_shanghai2 <- distances_haversine / 1000
print(cities_from_gpkg[order(cities_from_gpkg$distance_to_shanghai2), c("city", "distance_to_shanghai")])


# 将两个距离结果进行比较

print(cities_from_gpkg[order(cities_from_gpkg$distance_to_shanghai2), c("city", "distance_to_shanghai2")])
print(cities_from_gpkg[order(cities_from_gpkg$distance_to_shanghai), c("city", "distance_to_shanghai")])


#################################################################
#     3.2 Read Spatial Data from Shapefiles of Official China Maps
###############################################################

### define chinese CRS

mycrs <- "+proj=aea +lat_0=0 +lon_0=105 +lat_1=25 +lat_2=47 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"  # 定义中国常用的Albers等面积投影CRS

### 3.1 Read Shapefile

### base path

wd<-getwd() # get the working directory

shapes_path <- paste(wd, "Data/Shapes", sep = "/") # get the path of the shapefile
shapes_path

### Prov shapes file path 

provmap_path <- paste(shapes_path, "chinaprov2021mini/chinaprov2021mini.shp", sep = "/")
provmap_path
# read the shapefile and filter the NA values
provmap <- read_sf(provmap_path) %>% filter(!is.na(省代码))

provmap


plot(provmap["省代码"])


### 3.3 Read GeoJSON

# GeoJSON是一种基于JSON的地理空间数据交换格式
# 它支持点、线、面等几何类型，以及属性数据

# 示例1：创建和读取GeoJSON数据
# 首先创建一些示例地理数据



#####################################################################
## 4. PM2.5 Levels in 2010 and 2018 across the world from World Bank
#####################################################################


###### load packages if not installed

if (!require("wbstats")) install.packages("wbstats")


# 加载包
library(wbstats)

###### generate map of countries

map <- ne_countries(returnclass = "sf")

str(map)
head(map)


######################## plot map ##########################

ggplot(map) + geom_sf()

ggplot(map) + geom_sf(fill = "lightblue", color = "black") # plot the map

map



### Mollweide projection

ggplot(map) + 
  geom_sf(fill = "lightblue", color = "black") + 
  coord_sf(crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs") +  # 使用WGS84地理坐标系，避免投影失真
  labs(title = "World Map (WGS84 Geographic Projection)") +
  theme_minimal()


### Robinson projection

ggplot(map) + 
  geom_sf(fill = "lightblue", color = "black") + 
  coord_sf(crs = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs") +
  labs(title = "World Map (Robinson Projection)") +
  theme_minimal()

 
 ### download wbstats data from World Bank


#install.packages("wbstats")

indicators <- wb_search(pattern = "pollution")

str(indicators)
head(indicators)


d2010 <- wb_data(indicator = "EN.ATM.PM25.MC.M3",
                start_date = 2010, end_date = 2010)


d2018 <- wb_data(indicator = "EN.ATM.PM25.MC.M3",
             start_date = 2018, end_date = 2018)


head(d2010)
head(d2018)



### use left_join to join data with map

map2010 <- left_join(map, d2010, by = c("iso_a3" = "iso3c"))
map2018 <- left_join(map, d2018, by = c("iso_a3" = "iso3c"))


str(map2010)
head(map2010)

ggplot(map2010) + geom_sf(aes(fill = EN.ATM.PM25.MC.M3)) +
  scale_fill_viridis() + labs(fill = "PM2.5") + theme_bw()


### change the color of the map and add title


ggplot(map2010) + 
  geom_sf(aes(fill = EN.ATM.PM25.MC.M3)) +
  scale_fill_gradient(low = "green", high = "red") +
    labs(title = "PM2.5 Levels in 2010", fill = "PM2.5") + 
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5))


ggplot(map2018) + 
  geom_sf(aes(fill = EN.ATM.PM25.MC.M3)) +
  scale_fill_gradient(low = "green", high = "red") +
    labs(title = "PM2.5 Levels in 2018", fill = "PM2.5") + 
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5))


######################################################################
### 5. Open Street Map Data for Nanjing City
######################################################################

library(osmdata)

nanjing_osm <- getbb("Nanjing")

nanjing_osm

## boundary of the osm data

nanjing_osm_boundary <- opq(bbox = nanjing_osm) 


# highway 

nanjing_osm_highway <- add_osm_feature(opq = nanjing_osm_boundary, key = "highway")


# osmdata_sf convert the osm data to sf object

assign("has_internet_via_proxy", TRUE, environment(curl::has_internet)) # assign the proxy to the environment

nanjing_osm_highway_sf <- osmdata_sf(nanjing_osm_highway) # convert the osm data to sf object

nanjing_osm_highway_sf

### street network by OSM_lines

nanjing_osm_network <- nanjing_osm_highway_sf$osm_lines # get the street network from the osm data

nanjing_osm_network


nanjing_osm_network <- st_transform(nanjing_osm_network,crs=4326) # WGS84 or 4490(CGCS2000)

plot(nanjing_osm_network["highway"],key.pos = 1) # plot the street network



# ggplot

ggplot(data = nanjing_osm_network) +
  geom_sf(aes(color = "Road Network in Nanjing")) +
  theme_minimal() +
  labs(title = "Road Network in Nanjing", color = "Legend") +
  theme(legend.position = "bottom")


### subway network by OSM_lines

# 检查OSM中南京地铁数据的情况

nanjing_osm_subway <- add_osm_feature(opq =nanjing_osm_boundary, key = "railway", value = "subway")
nanjing_osm_subway_sf <- osmdata_sf(nanjing_osm_subway)



# 检查数据结构

cat("osm_points (地铁站点):", nrow(nanjing_osm_subway_sf$osm_points), "\n")
cat("osm_lines (地铁线路):", nrow(nanjing_osm_subway_sf$osm_lines), "\n")
cat("osm_polygons (地铁区域):", nrow(nanjing_osm_subway_sf$osm_polygons), "\n")


# 绘制南京地铁网络

plot(nanjing_osm_subway_sf$osm_lines["railway"], key.pos = 1) 

# please plot the subway network with ggplot
ggplot(nanjing_osm_subway_sf$osm_lines) +
  geom_sf(aes(color = "Subway Network in Nanjing")) +
  theme_minimal() +
  labs(title = "Subway Network in Nanjing", color = "Legend") +
  theme(legend.position = "bottom")

