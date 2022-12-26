rm(list=ls())
setwd("~/R/Survival Analysis")

# 0. 환경설정 -----
library(tidyverse)
library(survival)
library(survminer)
library(janitor)
library(extrafont)
loadfonts()

# 1. 데이터 -----
# data(ovarian)

# 2. 데이터 전처리 -----
ovarian <- ovarian %>% 
  tbl_df %>% 
  mutate(rx = factor(rx, levels = c("1", "2"), labels = c("A", "B")),
         resid.ds = factor(resid.ds, levels = c("1", "2"), labels = c("no", "yes")),
         ecog.ps = factor(ecog.ps, levels = c("1", "2"), labels = c("good", "bad"))) %>% 
  mutate(age_group = ifelse(age >=50, "old", "young") %>% as.factor)

DT::datatable(ovarian)


# 2.2 단변량 분석 - Kaplan-Meier 추정
# 이론에서 나온 생존확률(S(t))를 계산하기 위해서 Kaplan-Meier 추정값을 사용한다.

# 이론: S(t)=1−F(t)=Pr(T>t)
# 추정: S^(t)=∏i;ti<tni−dini
# 여기서, ni는 해당 시점(i) 관측점수가 되고 di는 해당 시점(i) 사건발생 건수가 된다.

# Kaplan-Meier Survival Estimates에 나온 데이터를 바탕으로 캐플란-마이어 추정작업을 수행한다.

test_df <- tribble(~time, ~censor, 
                   143,    1, 
                   165,    1, 
                   188,    1, 
                   188,    1, 
                   190,    1, 
                   192,    1, 
                   206,    1, 
                   208,    1, 
                   212,    1, 
                   216,    0, 
                   216,    1, 
                   220,    1, 
                   227,    1, 
                   230,    1, 
                   235,    1, 
                   244,    0, 
                   246,    1, 
                   265,    1, 
                   303,    1)

calc_km <- survfit(Surv(time=time, event=censor) ~ 1, data = test_df)

ggsurvplot(calc_km, conf.int = FALSE,
           risk.table = "nrisk_cumevents",
           tables.height = 0.3,
           legend = "none")


# 3. 예측모형 -----
## 3.0. 생존모형 객체 -----
surv_object <- Surv(time = ovarian$futime, event = ovarian$fustat)

## 3.1. Kaplan-Meier -----
# ovarian_km <- survfit(surv_object ~ 1)
ovarian_km <- survfit(Surv(time=futime, event=fustat) ~ 1, data = ovarian)

ggsurvplot(ovarian_km, 
           conf.int = TRUE,
           palette = "darkgreen",
           risk.table = "nrisk_cumevents",
           cumevents = TRUE,
           cumcensor = TRUE,
           linetype = 1,
           tables.height = 0.2,
           surv.median.line = "hv",
           legend = "none")


# 3. 예측모형 -----

## 3.2. 생존모형: 단변량 -----
### log rank 검정(rx)
km_rx_survfit <- survfit(surv_object ~ rx, data = ovarian)

summary(km_rx_survfit)

ggsurvplot(km_rx_survfit, data = ovarian, pval = TRUE)


### log rank 검정(resid.ds)
km_resid_survfit <- survfit(surv_object ~ resid.ds, data = ovarian)

summary(km_resid_survfit)


ggsurvplot(km_resid_survfit, data = ovarian, pval = TRUE)



# 2.4 단변량 생존모형 - 와이블
# Kaplan-Meier 모형을 시각화하면 계단모양으로 나타나는데 이를 부드러운 함수로 근사하고자 할 경우 
# 와이블(weibull) 모형을 활용하면 좋다


weibull_survfit <- survreg(Surv(time=futime, event=fustat) ~ 1, data = ovarian, dist="weibull")

predict(weibull_survfit, type="quantile", p=1-0.5, newdata = data.frame(1))

predict(weibull_survfit, type="quantile", p=1-0.9, newdata = data.frame(1))



# 2.4.1 단변량 생존모형 시각화 - 와이블
# 생존확률을 시각화할 경우 별도 데이터프레임을 제작해야 한다. 생존확률 숫자순열을 생성하고 나서 
# 이를 weibull_survfit 모형 예측값으로 넣어 예상 생존시간을 산출해낸다. 그리고 나서 이를 데이터 프레임으로 
# 만들고 ggsurvplot_df에 넣어 시각화한다

surv_prob <- seq(.99, .01, by = -.01)

surv_time <- predict(weibull_survfit, type = "quantile", p = 1 - surv_prob, newdata = data.frame(1))

surv_weibull_df <- tibble(time = surv_time, 
                          surv = surv_prob, 
                          upper = NA, 
                          lower = NA, 
                          std.err = NA) %>% as.data.frame()

ggsurvplot_df(fit = surv_weibull_df, surv.geom = geom_line)


## 3.3. 생존모형: 다변량 -----
### 변수선택
coxph_full <- coxph(surv_object ~ rx + resid.ds + age_group + ecog.ps, 
                    data = ovarian)

coxph_step <- step(coxph_full, direction = "both", trace = 0)

### 최종모형
coxph_survfit <- coxph(surv_object ~ rx + resid.ds + age_group, data = ovarian)

### 가설검정
cox.zph(coxph_survfit)

par(mfrow=c(2,2))
plot(cox.zph(coxph_survfit))

### 모형 시각화
ggforest(coxph_survfit, data = ovarian)

