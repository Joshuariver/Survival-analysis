rm(list=ls())
setwd("~/R/Survival Analysis")

# 0. 패키지 로드

library(survival)
require(flexsurv)
library(survminer)
require(optimx)
library(tidyverse)
require(numDeriv)

str(lung)  # 폐암환자의 상태, 특성, 생존시간은 1단위로 측정

lung$sex <- factor(lung$sex, levels = c(1,2),
                      labels = c("male","female"))

# 1. 탐색적 데이터 분석 -----

?Surv
Surv(time=lung$time, event=lung$status)
class(Surv(time=lung$time, event=lung$status))



# order_df  <- read_csv("data/next_order_clean.csv") %>%
#  mutate_if(is.character, as.factor)

# 2. 캐플란 마이어 분석 -----

km.fit <- survfit(Surv(time,status) ~ 1, data=lung)
km.fit

names(km.fit)  # 캐플란 마이어 분석 결과에 포함된 항목들 확인


km.df <- data.frame(time=km.fit$time,
                    n.risk=km.fit$n.risk,
                    n.event=km.fit$n.event,
                    n.censor=km.fit$n.censor,
                    surv=km.fit$surv)  # 보기 좋게 표로 만들기

head(km.df)  # 표 내용 확인


library(survminer)  # survminer 패키지를 사용하여 생존곡선 그리기

ggsurvplot(km.fit,
           xlab = "Days",
           ylab = "Overall Survival Probability")



ggsurvplot(km.fit, censor = FALSE,  # 중도절단 곡선을 제외하고 보여주기
           xlab = "Days",
           ylab = "Overall Survival Probability")


summary(km.fit, times=c(180,360))  # 180일까지 생존할 확률과 360까지 생존할 확률 계산

# 평균 생존 시간 계산 통상 중위수로 나타내며 일반적으로 우측으로 치우친 그래프
km.fit  # 여기에서는 median 인 310일

# 생존율이 X일때 살아남은 인원 수를 계산하는 것은 quantile 함수를 사용
quantile(km.fit, probs = 1-c(0.7, 0.3, 0.5))  # 각 생존확률이 70%, 30%, 50% 일 때 생존인원 수


# 성별에 따른 생존분석

km.group <- survfit(Surv(time,status) ~ sex, data=lung)
km.group


summary(km.group)
summary(km.group)$table  # 테이블 형태로 성별에 따라 생존확률분석결과 확인 가능
summary(km.group, times = c(180,360))  # 180일, 360일까지 생존확률을 남/녀 분리해서 알아보기

km.summary <- surv_summary(km.group, data = lung)
head(km.summary)
attr(km.summary, "table")

# 로그 순위 검정으로 성별에 따른 생존률의 차이가 있는지를 확인할 수 있음

survdiff(Surv(time,status) ~ sex, data=lung)

# 성별에 따른 집단별 생존확률을 그래프로 표현

?ggsurvplot() 
ggsurvplot(km.group, pval=TRUE, conf.int=TRUE,
           risk.table="absolute", risk.table.col="strata",
           linetype = "strata", surv.median.line = "hv",
           ggtheme = theme_bw(), pallette=c("royalblue","salmon"))


ggsurvplot(km.group, pval=TRUE, conf.int=TRUE,
           conf.int.style = "step",
           xlab = "Days", break.time.by = 180,
           risk.table="abs_pct", risk.table.col="strata",
           risk.table.fontsize = 3.5, risk.table.y.text=FALSE,
           ncensor.plot=TRUE,
           legned.labs = c("Male","Female"), legend.title = "",
           linetype = "strata", surv.median.line = "hv",
           ggtheme = theme_light(), pallette=c("royalblue","salmon"))


ggsurvplot(km.group, conf.int=TRUE,
           linetype = "strata", xlim=c(0,600),
           ggtheme = theme_bw(), pallette=c("royalblue","salmon"))


# fun 에 "event"변수를 입력하면 누적사건곡선을 그릴 수 있음. 특정 시점까지 사망할 확률을 볼 수 있다.

ggsurvplot(km.group, conf.int=TRUE,
           linetype = "strata", fun="event",
           ggtheme = theme_bw(), pallette=c("royalblue","salmon"))


# 누적 위험함수곡선 생성: 특정시점까지의 사건 발생 횟수

ggsurvplot(km.group, conf.int=TRUE,
           linetype = "strata", fun="cumhaz",
           ggtheme = theme_bw(), pallette=c("royalblue","salmon"))



# 다중집단에 대한 캐플란-마이어 분석: 집단영향 변수가 2개 이상일 경우에도 분석가능


rm(list=ls())
