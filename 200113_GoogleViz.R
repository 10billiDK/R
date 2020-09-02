setwd("/Users/kimdongkyu/Desktop/Data(f)")
carcinogen <- read.csv("carcinogen.csv")
name <- c("Seoul","Busan","Daegu","Incheon","Gwangju","Daejeon","Sejong", "Ulsan","Gyeonggi", "Gangwon", "Chungbuk","Chungnam","Jeonbuk","Jeoonnam","Gyeongbuk","Gyeongnam")
region <- paste("KR",c(11,26,27,28,29,30,31,32,41,42,43,44,45,46,47,48),sep="-")

Korea <- data.frame(region, carcinogen[,2], name)
colnames(Korea) <- c("region","carcinogen","name")

install.packages("googleVis")
library("googleVis")

Map <- gvisGeoChart(Korea, locationvar = "region", colorvar = "carcinogen", hovervar = "name",
                    options = list(region = "KR", displayMode = "regions", resolution = "provinces", 
                                   colorAxis="{colors:['lightgreen','red']}", backgroundColor = "darkblue",
                                   datalessRegionColor = "gray"))
plot(Map)


#############구글 비즈 사용 예시 ###############

#꺾은 선형 차트
df=data.frame(country=c("US", "GB", "BR"), 
              val1=c(10,13,14), 
              val2=c(23,12,32))
Line <- gvisLineChart(df)
plot(Line)

#두 개의 축이 있는 꺾은 선형 차트
Line2 <- gvisLineChart(df, "country", c("val1","val2"),
                       options=list(
                         series="[{targetAxisIndex: 0},
                                 {targetAxisIndex:1}]",
                         vAxes="[{title:'val1'}, {title:'val2'}]"
                       ))
plot(Line2)
#막대 차트
Bar <- gvisBarChart(df)
plot(Bar)
#기둥 형 차트
Column <- gvisColumnChart(df)
plot(Column)
#면적 차트
Area <- gvisAreaChart(df)
plot(Area)
#단계별 영역 차트
SteppedArea <- gvisSteppedAreaChart(df, xvar="country", 
                                    yvar=c("val1", "val2"),
                                    options=list(isStacked=TRUE))
plot(SteppedArea)
#콤보 차트
Combo <- gvisComboChart(df, xvar="country",
                        yvar=c("val1", "val2"),
                        options=list(seriesType="bars",
                                     series='{1: {type:"line"}}'))
plot(Combo)
#분산형 차트
Scatter <- gvisScatterChart(women, 
                            options=list(
                              legend="none",
                              lineWidth=2, pointSize=0,
                              title="Women", vAxis="{title:'weight (lbs)'}",
                              hAxis="{title:'height (in)'}", 
                              width=300, height=300))
plot(Scatter)
#버블 차트
Bubble <- gvisBubbleChart(Fruits, idvar="Fruit", 
                          xvar="Sales", yvar="Expenses",
                          colorvar="Year", sizevar="Profit",
                          options=list(
                            hAxis='{minValue:75, maxValue:125}'))
plot(Bubble)
#라인 사용자 정의
Dashed <-  gvisLineChart(df, xvar="country", yvar=c("val1","val2"),
                         options=list(
                           series="[{color:'green', targetAxisIndex: 0, 
                          lineWidth: 1, lineDashStyle: [2, 2, 20, 2, 20, 2]}, 
                          {color: 'blue',targetAxisIndex: 1, 
                          lineWidth: 2, lineDashStyle: [4, 1]}]",
                           vAxes="[{title:'val1'}, {title:'val2'}]"
                         ))
plot(Dashed)
#포인트 커스터마이징
M <- matrix(nrow=6,ncol=6)
M[col(M)==row(M)] <- 1:6
dat <- data.frame(X=1:6, M)
SC <- gvisScatterChart(dat, 
                       options=list(
                         title="Customizing points",
                         legend="right",
                         pointSize=30,
                         series="{
                              0: { pointShape: 'circle' },
                              1: { pointShape: 'triangle' },
                              2: { pointShape: 'square' },
                              3: { pointShape: 'diamond' },
                              4: { pointShape: 'star' },
                              5: { pointShape: 'polygon' }
                              }"))
plot(SC)
#즉시 사용자 정의를 위한 편집 버튼 추가
Line4 <-  gvisLineChart(df, "country", c("val1","val2"),
                        options=list(gvis.editor="Edit me!"))
plot(Line4)
#많은 옵션이 설정된 차트
Line3 <-  gvisLineChart(df, xvar="country", yvar=c("val1","val2"),
                        options=list(
                          title="Hello World",
                          titleTextStyle="{color:'red', 
                                           fontName:'Courier', 
                                           fontSize:16}",                         
                          backgroundColor="#D3D3D3",                          
                          vAxis="{gridlines:{color:'red', count:3}}",
                          hAxis="{title:'Country', titleTextStyle:{color:'blue'}}",
                          series="[{color:'green', targetAxisIndex: 0}, 
                                   {color: 'orange',targetAxisIndex:1}]",
                          vAxes="[{title:'val1'}, {title:'val2'}]",
                          legend="bottom",
                          curveType="function",
                          width=500,
                          height=300                         
                        ))
plot(Line3)
#촛대 차트
Candle <- gvisCandlestickChart(OpenClose, 
                               options=list(legend='none'))
plot(Candle)
#파이 차트
Pie <- gvisPieChart(CityPopularity)
plot(Pie)
#계량기
Gauge <-  gvisGauge(CityPopularity, 
                    options=list(min=0, max=800, greenFrom=500,
                                 greenTo=800, yellowFrom=300, yellowTo=500,
                                 redFrom=0, redTo=300, width=400, height=300))
plot(Gauge)
#강도 맵
Intensity <- gvisIntensityMap(df)
plot(Intensity)
#지리 차트
Geo=gvisGeoChart(Exports, locationvar="Country", 
                 colorvar="Profit",
                 options=list(projection="kavrayskiy-vii"))
plot(Geo)
#주별 미국 데이터를 보여주는 예
require(datasets)
states <- data.frame(state.name, state.x77)
GeoStates <- gvisGeoChart(states, "state.name", "Illiteracy",
                          options=list(region="US", 
                                       displayMode="regions", 
                                       resolution="provinces",
                                       width=600, height=400))
plot(GeoStates)
#마커가 있는 허리케인 앤드류 (1992) 폭풍 트랙 표시
GeoMarker <- gvisGeoChart(Andrew, "LatLong", 
                          sizevar='Speed_kt',
                          colorvar="Pressure_mb", 
                          options=list(region="US"))
plot(GeoMarker)
#구글 지도
AndrewMap <- gvisMap(Andrew, "LatLong" , "Tip", 
                     options=list(showTip=TRUE, 
                                  showLine=TRUE, 
                                  enableScrollWheel=TRUE,
                                  mapType='terrain', 
                                  useMapTypeControl=TRUE))
plot(AndrewMap)
#표
Table <- gvisTable(Stock, 
                   formats=list(Value="#,###"))
plot(Table)
#페이지가 있는 테이블
PopTable <- gvisTable(Population, 
                      formats=list(Population="#,###",
                                   '% of World Population'='#.#%'),
                      options=list(page='enable'))
plot(PopTable)
#조직도
Org <- gvisOrgChart(Regions, 
                    options=list(width=600, height=250,
                                 size='large', allowCollapse=TRUE))
plot(Org)
#트리 맵
Tree <- gvisTreeMap(Regions,  
                    "Region", "Parent", 
                    "Val", "Fac", 
                    options=list(fontSize=16))
plot(Tree)
#주석 차트
Anno <- gvisAnnotationChart(Stock, 
                            datevar="Date",
                            numvar="Value", 
                            idvar="Device",
                            titlevar="Title", 
                            annotationvar="Annotation",
                            options=list(
                              width=600, height=350,
                              fill=10, displayExactValues=TRUE,
                              colors="['#0000ff','#00ff00']")
)
plot(Anno)
#산키 차트
datSK <- data.frame(From=c(rep("A",3), rep("B", 3)),
                    To=c(rep(c("X", "Y", "Z"),2)),
                    Weight=c(5,7,6,2,9,4))

Sankey <- gvisSankey(datSK, from="From", to="To", weight="Weight",
                     options=list(
                       sankey="{link: {color: { fill: '#d799ae' } },
                            node: { color: { fill: '#a61d4c' },
                            label: { color: '#871b47' } }}"))
plot(Sankey)
#히스토그램
set.seed(123)
datHist=data.frame(A=rpois(100, 20),
                   B=rpois(100, 5),
                   C=rpois(100, 50))

Hist <- gvisHistogram(datHist, options=list(
  legend="{ position: 'top', maxLines: 2 }",
  colors="['#5C3292', '#1A8763', '#871B47']",
  width=400, height=360))
plot(Hist)
#캘린더 차트
Cal <- gvisCalendar(Cairo, 
                    datevar="Date", 
                    numvar="Temp",
                    options=list(
                      title="Daily temperature in Cairo",
                      height=320,
                      calendar="{yearLabel: { fontName: 'Times-Roman',
                               fontSize: 32, color: '#1A8763', bold: true},
                               cellSize: 10,
                               cellColor: { stroke: 'red', strokeOpacity: 0.2 },
                               focusedCellColor: {stroke:'red'}}")
)
plot(Cal)
#타임라인 차트
datTL <- data.frame(Position=c(rep("President", 3), rep("Vice", 3)),
                    Name=c("Washington", "Adams", "Jefferson",
                           "Adams", "Jefferson", "Burr"),
                    start=as.Date(x=rep(c("1789-03-29", "1797-02-03", 
                                          "1801-02-03"),2)),
                    end=as.Date(x=rep(c("1797-02-03", "1801-02-03", 
                                        "1809-02-03"),2)))

Timeline <- gvisTimeline(data=datTL, 
                         rowlabel="Name",
                         barlabel="Position",
                         start="start", 
                         end="end",
                         options=list(timeline="{groupByRowLabel:false}",
                                      backgroundColor='#ffd', 
                                      height=350,
                                      colors="['#cbb69d', '#603913', '#c69c6e']"))
plot(Timeline)
#워드 트리 차트
wt1 <- gvisWordTree(Cats, textvar = "Phrase")
plot(wt1)
#차트 병합
G <- gvisGeoChart(Exports, "Country", "Profit", 
                  options=list(width=300, height=300))
T <- gvisTable(Exports, 
               options=list(width=220, height=300))

GT <- gvisMerge(G,T, horizontal=TRUE) 
plot(GT)
#플래시 차트
Motion=gvisMotionChart(Fruits, 
                       idvar="Fruit", 
                       timevar="Year")
plot(Motion)
#지리지도
AndrewGeo <- gvisGeoMap(Andrew, 
                        locationvar="LatLong", 
                        numvar="Speed_kt", 
                        hovervar="Category", 
                        options=list(height=350, 
                                     region="US", 
                                     dataMode="markers"))
plot(AndrewGeo)
#주석이 달린 타임 라인 차트
AnnoTimeLine  <- gvisAnnotatedTimeLine(Stock, 
                                       datevar="Date",
                                       numvar="Value", 
                                       idvar="Device",
                                       titlevar="Title", 
                                       annotationvar="Annotation",
                                       options=list(displayAnnotations=TRUE,
                                                    width="600px", height="350px"))
plot(AnnoTimeLine)