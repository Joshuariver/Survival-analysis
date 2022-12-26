# 콕스 회귀분석 (Cox-regression )

# 생존시간과 하나 이상의 예측변수 간의 관계를 분석하는 일종의 회귀분석모델
# 캐플란-마이어 분석과 달리 범주형 변수 뿐만 아니라, 연속형 변수가 생존기간에 미치는
# 영향을 분석할 수 있는 다변량 분석(multivariate analysis) 기법

# 일반적인 다중회귀분석에서처럼 복수의 요인을 동시에 통계모델에 투입함으로써
# 모델에 포함된 다른 예측변수들이 일정하다는 가정하에 각 예측변수가 사건발생률
# (예를 들면, 사망률)에 미치는 영향을 분석

# Cox 회귀분석에서는 위험비(hazard ratio)를 중시
# 위험비란 예측변수 한 단위가 증가할 때 변화하는 위험률
# 위험비가 >1 면 위험률이 증가,  위험비=1 이면 영향없음. 위험비가 <1면 위험률 감소

# cox 회귀분석은 위험비가 생존기간 내내 일정하다고 가정하며 이를 비례위험가정
# (proportional hazards assumption)이라고 함.
# 위험비 계산 식에는 시간변수가 포함되어 있지 않음.
# 만일 시간의 흐름에 따라 위험비가 바뀐다면 Cox 회귀분석으로 분석할 수 없음.
# 비례위험가정


rm(list=ls())

library(survival)
require(flexsurv)
library(survminer)
require(optimx)
library(tidyverse)
require(numDeriv)

# 1.데이터 로드

str(lung)
head(lung)

lung$sex <- factor(lung$sex, levels = c(1,2),
                   labels = c("male","female"))

Surv(time=lung$time, event=lung$status)
class(Surv(time=lung$time, event=lung$status))

# 2.Cox 회귀분석

cox <- coxph(Surv(time,status) ~ age + sex + ph.ecog ,
             data=lung)
cox
summary(cox)


# 3. 그래프로 표시

ggforest(cox, data = lung)


# 4. 생존함수를 추정

cox.fit <- survfit(cox, data = lung)
cox.fit

ggsurvplot(cox.fit, palette = "cornflowerblue",
           ggtheme=theme_minimal(),
           legend = "none", xlab = "Days", ylab = "Overall Survival Probability",
           )

# 예측변수의 값에 따라 달라지는 생존확률 곡선 비교
# 예를 들어 성별

sex.df <- with(lung,
               data.frame(sex=c("male","female"),
                          age=rep(mean(age, na.rm = TRUE)),
                          ph.ecog=rep(mean(ph.ecog, na.rm = TRUE), 2)))

sex.df

sex.fit <- survfit(cox, newdata=sex.df, data=lung)
sex.fit

ggsurvplot(sex.fit, conf.int=FALSE,
           ggtheme=theme_minimal(),
           legend.labs = c("Male","Female"),
           legent.title="",
           xlab = "Days", ylab = "Survival Probability")



# ecog 변수만을 고정해서 보여주기
range(lung$ph.ecog, na.rm=TRUE)


ph.df <- with(lung,
               data.frame(sex=c("male","female"),
                          age=rep(mean(age, na.rm = TRUE), 4),
                          ph.ecog=c(0,1,2,3)))

ph.df
ph.fit <- survfit(cox, newdata = ph.df, data = lung)

ggsurvplot(ph.fit, conf.int=FALSE,
           ggtheme=theme_minimal(),
           legend.labs = c(0:3),
           legent.title="ECOG Performance Score (0=good)",
           xlab = "Days", ylab = "Survival Probability")

# Cox 의 비례위험 가정을 평가하는 cox.zph 함수
# 잔차와 시간간의 상관관계 존재여부를 통해 비례위험가정 추정

cox.zph(cox)

# 여기에는 p값이 모두 0.05보다 크므로 잔차와 시간간의 상관관계가 없는 것으로 해석
# 따라서 비례위험가정이 없다고 판단할 수 있음.