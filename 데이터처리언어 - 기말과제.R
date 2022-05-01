install.packages("igraph")
library(igraph)

# 3. 데이터셋 파일을 read.table() 사용
df.eu <- read.table(file.choose(), header = F)
head(df.eu)
tail(df.eu)

# 4. 중심화(연결정도/근접/중개)분석, 각각의 결과값을 출력
CD <- centralization.degree(G.eu, normalized = FALSE) # 연결정도 중심화(비정규형)
CD$centralization
Tmax <- centralization.degree.tmax(G.eu) # 이론적인 최대 연결정도 중심화
CD$centralization / Tmax # 연결정도 중심화(정규형)
CB <- centralization.closeness(G.eu, normalized = FALSE) # 근접 중심화(비정규형)
n <- vcount(G.eu)
CB$centralization / (n-1) # 근접 중심화
CB$theoretical_max / (n-1) # 이론적인 최대 근접 중심화
CB$centralization / CB$theoretical_max # 근접 중심화(정규형)
CB <- centralization.betweenness(G.eu, normalized = FALSE) # 중개 중심화(비정규형)
CB$centralization # 중개 중심화
CB$theoretical_max # 이론적인 최대 중개 중심화화
CB$centralization / CB$theoretical_max # 중개 중심화(정규형)

# 5. 연결정도 분포를 차트로 출력 plot() 사용
G.eu <- graph.data.frame(df.eu, directed = FALSE)
par(mar = c(0,0,0,0))
plot(G.eu, vertex.label = NA, vertex.size = 10)
dev.off()

# 6. 데이터셋의 항목(내용)에 맞도록 x,y축 레이블 표시
plot(degree(G.eu), xlab = "사용자", ylab = "연결정도", main = "사용자별 연결정도", type = 'h')

# 7. 연결정도가 가장 큰 10개 노드 추출하고 노드 리스트 출력
list_node10 <- list(sort(degree(G.eu, normalized = F), decreasing = TRUE)[0:10])
list_node10

# 8. 연결정도가 가장 큰 노드 붉은색, 나머지 녹색
v.max <- V(G.eu)$name[degree(G.eu) == max(degree(G.eu))] # 연결정도가 가장 큰 노드
degree(G.eu, v.max) # 연결정도
v.max.idx <- which(V(G.eu)$name == v.max) # 연결정도가 가증 큰 노드 위치
v.set <- neighbors(G.eu, v = v.max.idx) # 연결정도가 가장 큰 노드의 이웃 노드
v1 <- c(v.max.idx, v.set) # 노드 통합
G.eu.max <- induced_subgraph(G.eu, v1) # 서브 네트워크
V(G.eu.max)$color <- ifelse(V(G.eu.max)$name == v.max, "red", "green")
V(G.eu.max)$label <- ifelse(V(G.eu.max)$name == v.max, v.max, NA)
V(G.eu.max)$size <- ifelse(V(G.eu.max)$name == v.max, 30, 5)
plot(G.eu.max)

# 9. 노드의 size를 연결정도에 따라 다르게 출력
V(G.eu.max)$size <- degree(G.eu.max) * 1
plot(G.eu.max)