# UCLA 데이터 획득
ucla = read.csv('https://stats.idre.ucla.edu/stat/data/binary.csv')
str(ucla)

# 데이터 factor형 변환
ucla$rank = factor(ucla$rank)

# 학습데이터와 테스트데이터 분리
n = nrow(ucla)
i = 1:n
train_list = sample(i, n * 0.6)
test_list = setdiff(i, train_list)
ucla_train = ucla[train_list, ]
ucla_test = ucla[test_list, ]

# 학습데이터로 모델을 만드는 과정
control = trainControl(method = 'cv', number = 5)
r = train(admit~., data = ucla_train, method = 'rpart', trControl = control)
f50 = train(admit~., data = ucla_train, method = 'rf', ntree = 50, trControl = control)
f1000 = train(admit~., data = ucla_train, method = 'rf', ntree = 1000, trControl = control)
s_Radial = train(admit~., data = ucla_train, method = 'svmRadial', trControl = control)
s_Poly =train(admit~., data = ucla_train, method = 'svmPoly', trControl = control)
k = train(admit~., data = ucla_train, method = 'knn',, trControl = control)

resamp = resamples(list(결정트리 = r, 포레스트50 = f50, 포레스트1000 = f1000, svmRadial = s_Radial, svmPoly = s_Poly, knn = k))
summary(resamp)

# 테스트데이터로 예측하고, table 함수를 사용하여 혼동행렬 출력
table(predict(r, ucla_test), ucla_test$admit)
table(predict(f50, ucla_test), ucla_test$admit)
table(predict(f1000, ucla_test), ucla_test$admit)
table(predict(s_Radial, ucla_test), ucla_test$admit)
table(predict(s_Poly, ucla_test), ucla_test$admit)
table(predict(k, ucla_test), ucla_test$admit)

# 혼동 행렬로부터 정확도를 계산
caret::confusionMatrix(predict(r, ucla_test), ucla_test$admit)
caret::confusionMatrix(predict(f50, ucla_test), ucla_test$admit)
caret::confusionMatrix(predict(f1000, ucla_test), ucla_test$admit)
caret::confusionMatrix(predict(s_Radial, ucla_test), ucla_test$admit)
caret::confusionMatrix(predict(s_Poly, ucla_test), ucla_test$admit)
caret::confusionMatrix(predict(k, ucla_test), ucla_test$admit)