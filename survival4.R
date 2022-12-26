# 다중집단에 대한 캐플란-마이어 분석: 집단영향 변수가 2개 이상일 경우에도 분석가능


rm(list=ls())

# 0. 패키지 로드

library(survival)
require(flexsurv)
library(survminer)
require(optimx)
library(tidyverse)
require(numDeriv)

# 1.데이터 로드

str(colon)
head(colon)


# 사망인 경우 (etype=2 인경우)에 대해서만 분석

colon.death <- colon[colon$etype==2,]
colon.death$sex <- factor(colon.death$sex,
                          levels = c(0,1),
                          labels = c("female","male"))

colon.death$differ <- factor(colon.death$differ,
                             levels = c(1,2,3),
                             labels = c("well","moderate","poor"))

# 2. 생존함수 추정

km.fit <- survfit(Surv(time, status) ~ sex + rx + differ, data = colon)

# 3. 생존함수 곡선

ggsurvplot(km.fit, conf.int = TRUE,
           conf.int.style = "step",
           ggtheme=theme_bw())

# 너무 복잡하므로 치료법과 종양분화상태에 의한 남녀 생존곡선을 다시 그림


ggsurv <- ggsurvplot(km.fit, conf.int = TRUE,
                     conf.int.style = "step",
                     ggtheme=theme_bw())

ggsurv$plot + theme_bw() + 
  theme(legend.position = "right", legend.title=element_blank()) + 
        facet_grid(rx ~ differ, labeller=label_both)
