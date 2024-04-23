
#题目1
rm(list = ls())
# 加载所需的库
library(sf)
library(dplyr)
library(tidyr)
library(geosphere)

# 设置工作目录
setwd("/Users/shirleychen/learn_git/github/a11-geo-shirleychen0124/assigment_geo")

# 读取地理边界信息
development_area <- st_read("G341022合肥经济技术开发区.txt")

# 查看数据结构
str(development_area)

# 查看投影坐标系
st_crs(development_area)


# 读取企业信息文件
enterprises <- read.table("hefei.txt", header = TRUE, sep = "\t", quote = "", stringsAsFactors = FALSE)

# 查看数据结构
str(enterprises)

# 删除双引号
enterprises <- lapply(enterprises, function(x) gsub('"', '', x))

# 将字符向量转换为数据框
enterprises <- as.data.frame(enterprises, stringsAsFactors = FALSE)

# 查看列名
colnames(enterprises)

# 查看缺失值情况
summary(is.na(enterprises$X.lng.))
summary(is.na(enterprises$X.lat.))

# 重命名经度和纬度列
colnames(enterprises)[colnames(enterprises) == "X.lng."] <- "lng"
colnames(enterprises)[colnames(enterprises) == "X.lat."] <- "lat"
colnames(enterprises)[colnames(enterprises) == "X.pripid."] <- "pripid"
colnames(enterprises)[colnames(enterprises) == "X.city."] <- "city"

# 查看经度和纬度列的数据类型
class(enterprises$lng)
class(enterprises$lat)

# 将经度和纬度列转换为数值型
enterprises$lng <- as.numeric(enterprises$lng)
enterprises$lat <- as.numeric(enterprises$lat)


# 将企业信息转换为sf对象
enterprises_sf <- st_as_sf(enterprises, coords = c("lng", "lat"), crs = 4326)


# 计算开发区内的企业数量
enterprises_within_area <- st_intersection(enterprises_sf, development_area)
num_enterprises_within_area <- nrow(enterprises_within_area)

cat("企业数量（开发区内）:", num_enterprises_within_area, "\n")



st_crs(development_area)
st_crs(enterprises_sf)

# 定义函数计算指定范围内的企业数量
count_enterprises_within_radius <- function(center_point, enterprises_sf, radius) {
  # 计算每个企业到中心点的距离
  distances <- st_distance(enterprises_sf, center_point)
  # 计算距离小于等于半径的企业数量
  count <- sum(distances <= radius)
  return(count)
}

# 计算开发区不同半径范围内的企业数量
center_point <- st_centroid(development_area)
radii <- c(1000, 3000, 5000)  # 单位：米
for (radius in radii) {
  # 将 radius 转换为 "units" 对象
  radius_m <- units::set_units(radius, "m")
  num_enterprises <- count_enterprises_within_radius(center_point, enterprises_sf, radius_m)
  cat(paste("企业数量（开发区", radius / 1000, "km范围内）:", num_enterprises, "\n"))
}



#题目2
# 加载所需的库
library(ggplot2)
library(tmap)
# 读取地理边界信息
development_area1 <- st_read("G342020合肥高新技术产业开发区区块一.txt")
development_area2 <- st_read("G342020合肥高新技术产业开发区区块二.txt")


# 绘制地图
print(ggplot() +
        geom_sf(data = development_area1, fill = "transparent", color = "blue") +
        geom_sf(data = development_area2, fill = "transparent", color = "green") + # 绘制开发区边界
        geom_sf(data = enterprises_sf, color = "red", size = 1) +  # 绘制企业点
        theme_minimal() +
        coord_sf() +
        labs(title = "保定市的开发区和企业分布地图", x = "经度", y = "纬度")
)







