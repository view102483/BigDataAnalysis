1928-1969間，小兒麻痺在美國各州的發生率變化
-----------
### 資料前處理
__把資料讀進來__
```{r}
data<-read.csv("POLIO_Incidence.csv")
if(!require(ggplot2)) {
  install.packages(ggplot2, dep=TRUE)
  library(ggplot2)
}
if(!require(reshape2)) {
  install.packages(reshape2, dep=TRUE)
  library(reshape2)
}
```
```
##   YEAR WEEK ALABAMA ALASKA ARIZONA ARKANSAS CALIFORNIA COLORADO
## 1 1928    1       0      -       0        0       0.17     0.39
## 2 1928    2       0      -       0        0       0.15      0.2
## 3 1928    3    0.04      -       0        0       0.11        0
## 4 1928    4       0      -    0.24     0.11       0.07      0.2
## 5 1928    5       0      -    0.24        0       0.32        0
## 6 1928    6       0      -       0        0       0.22      0.1
##   CONNECTICUT DELAWARE DISTRICT.OF.COLUMBIA FLORIDA GEORGIA HAWAII IDAHO
## 1           0        0                    -       0    0.03      -     0
## 2           0        0                    -       0       0      -     0
## 3        0.06        0                    -       0       -      -     0
## 4        0.06        0                    0       0       0      -     0
## 5        0.13        0                    0       0       0      -     0
## 6           0        0                    0       0       0      -     -
##   ILLINOIS INDIANA IOWA KANSAS KENTUCKY LOUISIANA MAINE MARYLAND
## 1     0.03    0.03 0.08      0        0         0     0     0.06
## 2     0.01    0.03    -   0.22        0      0.05  0.13     0.06
## 3     0.03    0.03    -      0        0         0     0        0
## 4     0.05    0.12    0      0        0         0     0        0
## 5     0.04       0 0.04      0        0         0  0.38     0.12
## 6     0.03       0    0      0        0         0     0        0
##   MASSACHUSETTS MICHIGAN MINNESOTA MISSISSIPPI MISSOURI MONTANA NEBRASKA
## 1          0.14     0.04         0           0     0.03    0.18     0.07
## 2          0.14     0.04      0.04           0     0.06       0     0.07
## 3          0.07     0.02         0           0     0.03    0.18        0
## 4          0.02     0.02         0           0     0.06       0        0
## 5          0.02     0.04         0           0        0       0     0.15
## 6          0.05     0.06         0           0        0       0     0.07
##   NEVADA NEW.HAMPSHIRE NEW.JERSEY NEW.MEXICO NEW.YORK NORTH.CAROLINA
## 1      -             -       0.08          0     0.08              0
## 2      -             -       0.03          0     0.05           0.03
## 3      -             -          0          0     0.03              0
## 4      -             0       0.03          0     0.06              0
## 5      -             0       0.03       0.48     0.07              0
## 6      -             0          0          0     0.03              0
##   NORTH.DAKOTA OHIO OKLAHOMA OREGON PENNSYLVANIA RHODE.ISLAND
## 1            - 0.02        0   0.64            0            0
## 2         0.45    -     0.04   0.43         0.03            0
## 3            0 0.06        0   1.07         0.02            0
## 4         0.15    0     0.09   0.53         0.02            0
## 5            0 0.03        0   0.32            0            0
## 6            0 0.05     0.04   0.21         0.04            0
##   SOUTH.CAROLINA SOUTH.DAKOTA TENNESSEE TEXAS UTAH VERMONT VIRGINIA
## 1           0.06            0      0.04  0.05    0       0        -
## 2           0.06            0      0.04  0.04    0       0        -
## 3           0.35            0         0     0    0       0        -
## 4           0.23            0      0.04  0.05    0       0        -
## 5           0.17         0.15         0  0.05    0       0        -
## 6           0.06         0.29      0.04     0  0.2       0     0.04
##   WASHINGTON WEST.VIRGINIA WISCONSIN WYOMING
## 1       0.26          0.06      0.03       0
## 2       0.39          0.24      0.03       0
## 3       0.13          0.12      0.03       0
## 4       0.06          0.12         0       0
## 5       0.13          0.06      0.03       0
## 6       0.06             0      0.14       0
```
__將寬表格轉為長表格__
```{r}
data.m <- melt(data, id.vars = c("YEAR", "WEEK"))
```
```
##   YEAR WEEK variable value
## 1 1928    1  ALABAMA     0
## 2 1928    2  ALABAMA     0
## 3 1928    3  ALABAMA  0.04
## 4 1928    4  ALABAMA     0
## 5 1928    5  ALABAMA     0
## 6 1928    6  ALABAMA     0
```
__處理缺值__
```{r}
data.m[data.m$value=="-",]$value<-NA
```

__計算年度發生率__
```{r}
data.m$value<-as.numeric(data.m$value)
```
```
##   YEAR variable value
## 1 1928  ALABAMA  2.39
## 2 1929  ALABAMA  2.25
## 3 1930  ALABAMA  2.57
## 4 1931  ALABAMA  2.07
## 5 1932  ALABAMA  1.38
## 6 1933  ALABAMA  1.12
```
## 視覺化呈現

__解釋如何選擇圖形種類__
 我使用Heat Map進行作圖，因為折線圖的憲華在一起會很複雜，不好看清變化，原本想使用地圖方式做圖，但是各年皆需要呈現，圖太多無法請楚表達

__程式碼：__
```{r}
ggplot(data.sumYear,aes(YEAR,variable))+#x軸為year，y軸為variable
  geom_tile(aes(fill=value),colour="white")+#將區塊進行著色
  scale_fill_gradient(low="white",high="blue")#白色部分是數值低的，高的為藍色
```

__結果：__
![alt text](https://github.com/view102483/BigDataAnalysis/photo.png)

__解釋圖形__
小兒麻痺一開始在1928年並不沒有很嚴重，到約1945左右開始爆發，1955年左右突然趨近於零，是因為小兒麻痺疫苗開始接種，發現在賓州、西維吉尼亞洲及維吉尼亞洲的小兒麻痺較其他州少很多，是源於疫苗於賓州匹茲堡的阿森納小學（Arsenal Elementary School）與華生兒童之家（Watson Home for Children）展開試驗。後又於維吉尼亞洲展開大規模的試驗，所以相比於其他州，這兩州患有小兒麻痺的較少
