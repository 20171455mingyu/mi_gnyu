install.packages("ggmap")
install.packages("ggplot2")

library(ggmap)
library(ggplot2)

# 구글 지도 API 인증키를 등록
register_google(key = "AIzaSyBBwtCmgEWvdsI7oBABia3_ZLfxatD-X9w")

# 저장된 데이터셋 불러오기
busan <- read_xlsx("부산2호선.xlsx")

# 결측값 제거
na.busan <- na.omit(busan)

# 데이터셋 확인
head(x = na.busan, n = 6L)

# 지도 얻기, 기본적으로 구글지도를 통해 소스가 제공되는 형태
busan_map <- get_map("부산", # 구글 지도에 표시되는 지역 중 가능
               zoom = 11, # 지도의 줌의 정도를 정하는 부분
               maptype = "terrain") # 지도의 종류 중 하나

# 얻은 지도 활성화 및 그리기
busan_map_2 <- ggmap(busan_map) + # get_map()함수로 얻은 객체를 입력해서 지도를 활성화 시키는 함수
  
  geom_point(data = na.busan, # x, y에 따라 점 그리기
             mapping = aes(x = lon, y = lat, # x에 경도 y에 위도
                           color = 정류장, # color이 미학요소면 자동으로 변수값에 따라 색이 바뀜
                           size = 합계, # 지정한 변수에 따라 크기가 바뀜
                           alpha = 0.5)) + # 투명도
  
  geom_text(data = na.busan, # 나타낼 그림 표시(정류장 이름)
            mapping = aes(x = lon, y = lat + 0.01, # x는 경도 그대로 , y에 위도를 넣고 정류장을 0.01 위로 표시 겹침 방지
                          label = 정류장), # 무엇을 그릴지를 정하는 부분
            size = 2.5) + # 사이즈는 말 그대로 사이즈
  
  geom_path(data = na.busan, # 점 선 그리기
            mapping = aes(x = lon, y = lat),
            size = 1, 
            linetype = 2, # 선의 종류
            color = "blue") # 선의 색깔

busan_map_2

# 그래프 기리기
ggplot(data = na.busan, 
       mapping = aes(x = 정류장순번, y = 합계)) + # 그래프 x와 y축에 나타낼 부분 지정
  
  geom_bar(mapping = aes(fill = 정류장), stat = "identity", colour="black") + # fill 부분에 따라 그래프의 색상 변화, 
  
  geom_label(aes(label = 합계), nudge_y = 2, size = 3.5) + 
  
  geom_text(aes(y = 합계, label = 정류장), position = position_stack(vjust = 0.5), col = "white") + # 그래프 안에 텍스트를 넣고 포지션 및 색 지정, y축 글자를 vjust 값이 0이면 아래로 1이면 위로
  
  coord_cartesian(ylim = c(0, 1000000)) # 그래프의 범위를 지정