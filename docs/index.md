# 期末報告

## 資料探索

1. [鄉村、都市地區進出站人數比較：春節返鄉現象的假設初探](./explore/)

1. [都市化指標與初步視覺化：2018](./urban_idx/)

1. [各縣市的都市化程度：2005 - 2018](./urbanizing/)

## 處理過後之 shapefile

[下載](./taiwan_railway_urban_idx_2018_shp.7z)

### 內容

- `taiwan_county.shp`: 臺灣縣市 shapefile
    - 縣市名稱
    - 地理資料: 臺灣縣市 (polygon)
- `station_urban_idx_2018.shp` (各站): 資料內容 (+ `taiwan_county.shp`) 同[都市化指標與初步視覺化](https://liao961120.github.io/railwayViz/urban_idx/)最下方三張圖
    - `nye_idx`: 除夕當日指標
    - `avg_idx`: 工作日指標平均
    - `urban_idx`: 都市化指標
    - 地理資料: 車站位置 (point)
- `county_urban_idx_2018.shp` (各縣市): 資料內容同[各縣市的都市化程度](https://liao961120.github.io/railwayViz/urbanizing/)中間三張圖
    - `nye_idx`: 除夕當日指標
    - `avg_idx`: 工作日指標平均
    - `urban_idx`: 都市化指標
    - 地理資料: 臺灣縣市 (polygon)
