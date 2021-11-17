###필요 패키지 불러오기
pkgs <- c('tidyverse','magrittr','cluster','cluster.datasets','cowplot','NbClust',
          'clValid','ggfortify','clustree','dendextend','factoextra','FactoMineR',
          'corrplot','GGally','ggiraphExtra','knitr','kableExtra','plyr','ClusterR')
for (pkg in pkgs) if (!pkg %in% installed.packages()[,1]) install.packages(pkg)
invisible (lapply(pkgs,library,character.only=T))

#군집분석을 할 때마다 군집이 바뀌므로 랜덤성 고정
set.seed(1004)

#AHP에 따른 변수별 가중치 명명
weight <- c(0.1393,0.2034,0.1024,0.1342,0.2538,0.1359)


###군집분석 수행에 필요한 형태로 데이터 만들기
#데이터 불러오기
df <- read.csv('최종데이터.csv 저장 경로')
#NA 칸이 많아 분석 시 방해가 되는 행 제거
df <- df[-which(df$ADM_DR_NM=='둔촌1동'),]
df <- df[-which(df$ADM_DR_NM=='항동'),]
#데이터 살펴보기
summary(df)
head(df)
str(df)
#수치형임에도 문자형으로 저장된 열 수정 (문자형 -> 수치형)
df$gender_ratio <- as.numeric(df$gender_ratio)
df$gym_ratio <- as.numeric(df$gym_ratio)
#행정동 코드를 행 이름으로 설정
row.names(df) <- df$ADM_DR_CD
#불필요한 열 제거
new_df <- df[,!names(df) %in% c('ADM_DR_CD','ADM_DR_NM', 'gym')]
#데이터 Scaling 함수 생성 (MinMax)
minmax <- function(x){
  result <- (x-min(x))/(max(x)-min(x))
  return(result)
}
#데이터 Scaling 수행
df_scaled <- apply(new_df,2,minmax)


### K-Menas
##군집분석 수행
#최적의 K 구하기 : 팔꿈치 (k = 경사가 급변하는 곳)
fviz_nbclust(df_scaled, kmeans, method = "wss", k.max = 24) + theme_minimal() + ggtitle("the Elbow Method")
#k=4
all_k.means4 <- kmeans(df_scaled, centers=4, nstart = 20)
table(all_k.means4$cluster)
#군집분석 결과를 Scaling 전 데이터와 결합
all_kmeans4 <- cbind(new_df,all_k.means4$cluster)
colnames(all_kmeans4) <- c(colnames(df_scaled),'cluster')
all_kmeans4$cluster <- as.factor(all_kmeans4$cluster)

##군집분석 결과 시각화 
#PCA 시각화
fviz_cluster(all_k.means4, df_scaled, frame=F, geom='point',frame.type='norm')
#원변수 2개 시각화
pairs(~ gender_ratio+purchase+gym_ratio+reside_pop+life_pop+infra,data=all_kmeans4, col=c('red','green','blue','purple')[all_kmeans4$cluster], pch = c(16,17,15,3)[all_kmeans4$cluster])
#수행 결과 저장
#write.csv(all_kmeans4, '원하는 저장 경로')

##군집 별 변수의 형태 살펴보기 위한 과정
#각 변수의 평균을 해당 군집의 수치로 설정
#kmeans4_viz : 수치를 Scaling하지 않았을 경우 군집별 변수 수치
kmeans4_viz <- ddply(all_kmeans4, .(cluster), summarise,
                     gender_ratio =mean(gender_ratio),
                     purchase = mean(purchase),
                     gym_ratio = mean(gym_ratio),
                     reside_pop = mean(reside_pop),
                     life_pop = mean(life_pop),
                     infra = mean(infra))
kmeans4_viz <- cbind(kmeans4_viz,table(all_k.means4$cluster))
#Var1이란 열이 Cluster와 겹치는 정보이므로 삭제
kmeans4_viz <- kmeans4_viz[,!names(kmeans4_viz) %in% 'Var1']
#scaled_kmeans4_viz를 수행하기 위한 작업
scaled_kmeans4_clust <- cbind(df_scaled, all_k.means4$cluster)
colnames(scaled_kmeans4_clust) <- c(colnames(df_scaled),'cluster')
scaled_kmeans4_clust <- as.data.frame(scaled_kmeans4_clust)
scaled_kmeans4_clust$cluster <- as.factor(scaled_kmeans4_clust$cluster)
#scaled_kmeans4_viz : 수치를 Scaling 했을 경우 군집별 변수 수치
scaled_kmeans4_viz <- ddply(scaled_kmeans4_clust, .(cluster), summarise,
                            gender_ratio =mean(gender_ratio),
                            purchase = mean(purchase),
                            gym_ratio = mean(gym_ratio),
                            reside_pop = mean(reside_pop),
                            life_pop = mean(life_pop),
                            infra = mean(infra))
scaled_kmeans4_viz <- cbind(scaled_kmeans4_viz,table(all_k.means4$cluster))
#Var1이란 열이 Cluster와 겹치는 정보이므로 삭제
scaled_kmeans4_viz <- scaled_kmeans4_viz[,!names(scaled_kmeans4_viz) %in% 'Var1']
#불필요한 열 제거
kmeans4_viz <- kmeans4_viz[,c(1,8,2:7)]
scaled_kmeans4_viz <- scaled_kmeans4_viz[,c(1,8,2:7)]
#AHP 가중치를 곱한 값을 구하기 위한 함수 생성
rank_sum <- data.frame()
for (i in 1:NROW(scaled_kmeans4_viz)){
  for (j in 1:6){
    rank_sum[i,j] <- scaled_kmeans4_viz[i,j+2]*weight[j]
  }
}
rank_sum$score <- apply(rank_sum,1,sum)
#kmeans4_final_viz : scaled_kmeans4_viz에 군집별 점수를 더한 데이터프레임
kmeans4_final_viz <- cbind(scaled_kmeans4_viz,rank_sum$score)
colnames(kmeans4_final_viz) <- c(colnames(kmeans4_viz),'score')

kmeans4_viz
scaled_kmeans4_viz
kmeans4_final_viz

###결과 저장
#write.csv(kmeans4_final_viz,'원하는 저장 경로')



###K-Medoids
##군집분석 수행
#최적의 K 구하기 : 팔꿈치 (k = 경사가 급변하는 곳)
fviz_nbclust(df_scaled, pam, method = "wss", k.max = 24) + theme_minimal() + ggtitle("the Elbow Method")
#k=3
all_k.medoids3 <- pam(df_scaled, k=3)
table(all_k.medoids3$clustering)
#군집분석 결과를 Scaling 전 데이터와 결합
all_kmedoids3 <- cbind(new_df,all_k.medoids3$clustering)
colnames(all_kmedoids3) <- c(colnames(df_scaled),'cluster')
all_kmedoids3$cluster <- as.factor(all_kmedoids3$cluster)

##군집분석 결과 시각화 
#PCA 시각화
fviz_cluster(all_k.medoids3, df_scaled, frame=F, geom='point',frame.type='norm')
#원변수 2개 시각화
pairs(~ gender_ratio+purchase+gym_ratio+reside_pop+life_pop+infra,data=all_kmedoids3, col=c('red','green','blue')[all_kmedoids3$cluster], pch = c(16,17,15)[all_kmedoids3$cluster])
#수행 결과 저장
#write.csv(all_kmedoids3, '원하는 저장 경로')

##군집 별 변수의 형태 살펴보기 위한 과정
#각 변수의 평균을 해당 군집의 수치로 설정
#kmedoids3_viz : 수치를 Scaling하지 않았을 경우 군집별 변수 수치
kmedoids3_viz <- ddply(all_kmedoids3, .(cluster), summarise,
                       gender_ratio =mean(gender_ratio),
                       purchase = mean(purchase),
                       gym_ratio = mean(gym_ratio),
                       reside_pop = mean(reside_pop),
                       life_pop = mean(life_pop),
                       infra = mean(infra))
kmedoids3_viz <- cbind(kmedoids3_viz,table(all_k.medoids3$cluster))
#Var1이란 열이 Cluster와 겹치는 정보이므로 삭제
kmedoids3_viz <- kmedoids3_viz[,!names(kmedoids3_viz) %in% 'Var1']
#scaled_kmedoids3_viz를 수행하기 위한 작업
scaled_kmedoids3_clust <- cbind(df_scaled, all_k.medoids3$cluster)
colnames(scaled_kmedoids3_clust) <- c(colnames(df_scaled),'cluster')
scaled_kmedoids3_clust <- as.data.frame(scaled_kmedoids3_clust)
scaled_kmedoids3_clust$cluster <- as.factor(scaled_kmedoids3_clust$cluster)
#scaled_kmedoids3_viz : 수치를 Scaling 했을 경우 군집별 변수 수치 
scaled_kmedoids3_viz <- ddply(scaled_kmedoids3_clust, .(cluster), summarise,
                              gender_ratio =mean(gender_ratio),
                              purchase = mean(purchase),
                              gym_ratio = mean(gym_ratio),
                              reside_pop = mean(reside_pop),
                              life_pop = mean(life_pop),
                              infra = mean(infra))
scaled_kmedoids3_viz <- cbind(scaled_kmedoids3_viz,table(all_k.medoids3$cluster))
#Var1이란 열이 Cluster와 겹치는 정보이므로 삭제
scaled_kmedoids3_viz <- scaled_kmedoids3_viz[,!names(scaled_kmedoids3_viz) %in% 'Var1']
#불필요한 열 제거
kmedoids3_viz <- kmedoids3_viz[,c(1,8,2:7)]
scaled_kmedoids3_viz <- scaled_kmedoids3_viz[,c(1,8,2:7)]
#AHP 가중치를 곱한 값을 구하기 위한 함수 생성
rank_sum <- data.frame()
for (i in 1:NROW(scaled_kmedoids3_viz)){
  for (j in 1:6){
    rank_sum[i,j] <- scaled_kmedoids3_viz[i,j+2]*weight[j]
  }
}
rank_sum$score <- apply(rank_sum,1,sum)
#kmedoids3_final_viz : Scaled_kmedoids3_viz에 군집별 점수를 더한 데이터프레임
kmedoids3_final_viz <- cbind(scaled_kmedoids3_viz,rank_sum$score)
colnames(kmedoids3_final_viz) <- c(colnames(kmedoids3_viz),'score')

kmedoids3_viz
scaled_kmedoids3_viz
kmedoids3_final_viz

###결과 저장
#write.csv(kmedoids3_final_viz,'원하는 저장 경로')


###GMM
##군집분석 수행
#급감하는 지역 (AIC와 BIC 모두에서 벌점이 금감하는 지점)
opt_gmm_AIC <- Optimal_Clusters_GMM(df_scaled, max_clusters = 10, criterion = "AIC", 
                                    
                                    dist_mode = "eucl_dist", seed_mode = "random_subset",
                                    
                                    km_iter = 10, em_iter = 10, var_floor = 1e-10, 
                                    
                                    plot_data = T)
opt_gmm_BIC <- Optimal_Clusters_GMM(df_scaled, max_clusters = 10, criterion = "BIC", 
                                    
                                    dist_mode = "maha_dist", seed_mode = "random_subset",
                                    
                                    km_iter = 10, em_iter = 10, var_floor = 1e-10, 
                                    
                                    plot_data = T)
#k=4
all.gmm4 <- GMM(df_scaled, 4, dist_mode='eucl_dist',seed_mode='random_subset', km_iter=10, em_iter=10, verbose = F)
all_pr4 <- predict_GMM(df_scaled, all.gmm4$centroids, all.gmm4$covariance_matrices, all.gmm4$weights)
table(all_pr4$cluster_labels)
#군집분석 결과를 Scaling 전 데이터와 결합
all_gmm4 <- cbind(df_scaled,all_pr4$cluster_labels)
colnames(all_gmm4) <- c(colnames(df_scaled),'cluster')
all_gmm4 <- as.data.frame(all_gmm4)
all_gmm4$cluster <- as.factor(all_gmm4$cluster)

##군집분석 결과 시각화 
#gmm은 앞서 수행한 패키지 모듈이 작동하지 않아 별도로 pca를 수행해서 그려주어야 함
pca <- prcomp(df_scaled,center=T,scale=T)
summary(pca)
new_df_gmm <- cbind(all_gmm4,pca$x[,c(1,2)])
ggplot(new_df_gmm) + geom_point(aes(x=PC1,y=PC2, col=cluster, pch=cluster)) + ggtitle('Clusterp plot') + labs(x= "Dim1 (38.3%)", y= "Dim2 (26.4%)")
#원변수 2개 시각화
pairs(~ gender_ratio+purchase+gym_ratio+reside_pop+life_pop+infra,data=all_gmm4, col=c('red','green','blue','purple')[all_gmm4$cluster], pch = c(16,17,15,3)[all_gmm4$cluster])
#수행결과 저장
#write.csv(all_gmm4, '원하는 저장 경로')

##군집 별 변수의 형태 살펴보기 위한 과정
#각 변수의 평균을 해당 군집의 수치로 설정
#gmm4_viz : 수치를 Scaling하지 않았을 경우 군집별 변수 수치
gmm4_viz <- ddply(all_gmm4, .(cluster), summarise,
                  gender_ratio =mean(gender_ratio),
                  purchase = mean(purchase),
                  gym_ratio = mean(gym_ratio),
                  reside_pop = mean(reside_pop),
                  life_pop = mean(life_pop),
                  infra = mean(infra))
gmm4_viz <- cbind(gmm4_viz,table(all_gmm4$cluster))
#Var1이란 열이 Cluster와 겹치는 정보이므로 삭제
gmm4_viz <- gmm4_viz[,!names(gmm4_viz) %in% 'Var1']
#scaled_gmm4_viz를 수행하기 위한 작업
scaled_gmm4_clust <- cbind(df_scaled, all_gmm4$cluster)
colnames(scaled_gmm4_clust) <- c(colnames(df_scaled),'cluster')
scaled_gmm4_clust <- as.data.frame(scaled_gmm4_clust)
scaled_gmm4_clust$cluster <- as.factor(scaled_gmm4_clust$cluster)
#scaled_gmm4_viz : 수치를 Scaling 했을 경우 군집별 변수 수치 
scaled_gmm4_viz <- ddply(scaled_gmm4_clust, .(cluster), summarise,
                         gender_ratio =mean(gender_ratio),
                         purchase = mean(purchase),
                         gym_ratio = mean(gym_ratio),
                         reside_pop = mean(reside_pop),
                         life_pop = mean(life_pop),
                         infra = mean(infra))
scaled_gmm4_viz <- cbind(scaled_gmm4_viz,table(all_gmm4$cluster))
#Var1이란 열이 Cluster와 겹치는 정보이므로 삭제
scaled_gmm4_viz <- scaled_gmm4_viz[,!names(scaled_gmm4_viz) %in% 'Var1']
#불필요한 열 제거
gmm4_viz <- gmm4_viz[,c(1,8,2:7)]
scaled_gmm4_viz <- scaled_gmm4_viz[,c(1,8,2:7)]
#AHP 가중치를 곱한 값을 구하기 위한 함수 생성
rank_sum <- data.frame()
for (i in 1:NROW(scaled_gmm4_viz)){
  for (j in 1:6){
    rank_sum[i,j] <- scaled_gmm4_viz[i,j+2]*weight[j]
  }
}
rank_sum$score <- apply(rank_sum,1,sum)
gmm4_final_viz <- cbind(scaled_gmm4_viz,rank_sum$score)
colnames(gmm4_final_viz) <- c(colnames(gmm4_viz),'score')

gmm4_viz
scaled_gmm4_viz
gmm4_final_viz

###결과 저장
#write.csv(gmm4_final_viz, '원하는 저장 경로')

