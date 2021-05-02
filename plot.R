library(tidyverse)
theme_set(theme_gray(base_family='NanumGothic'))

# 인구 추계
pop_est <- read.csv("pop_estimation.csv")  # 통계청 장래인구추계
head(pop_est)

# 전체 인구 추계
ggplot(data = pop_est) +
  geom_line(aes(x = year, y = total_pop)) +
  scale_y_continuous(breaks = seq(0, 55000000, by = 1000000)) +
  xlab("연도") + ylab("전체 인구 예측")

# 만19세 인구 추계 (징병 신체검사 대상)
library(reshape2)
pop_est2 <- pop_est[, c(1, 3, 4, 5)]
names(pop_est2) <- c("year", "19세_전체", "19세_남성", "19세_여성")
pop_est2 <- melt(pop_est2, id = "year")
pop_est2$year <- as.Date(as.character(pop_est2$year), format = "%Y")

ggplot(data = pop_est2, aes(x = year, y = value, colour = variable)) +
  geom_line(aes(linetype = variable)) +
  geom_point(aes(shape = variable), size = 2.5) +
  theme(text = element_text(size = 20),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_date(date_labels = "%Y", date_breaks = "3 year") +
  scale_y_continuous(breaks = seq(0, 700000, by = 50000)) +
  labs(x = "연도", y = "만19세 인구 예측",
       caption = "출처: 국가통계포털(https://kosis.kr) - 장래인구추계(전국, 2017년 기준)")



# 한국과 주변국(미, 중, 일, 러, 북)의국가역량종합지수
cinc <- read.csv("NMC_5_0.csv")
head(cinc)

cinc %>%
  filter(year >= 1953 & ccode == c(2, 365, 710, 731, 732, 740)) %>%
  ggplot(aes(x = year, y = cinc, color = stateabb)) +
  geom_line(aes(linetype = stateabb)) +
  geom_point(aes(shape = stateabb), size = 2.5) +
  theme(text = element_text(size = 20),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "연도", y = "CINC 점수",
       caption = "출처: The Correlates of War Project - National Material Capabilites")


