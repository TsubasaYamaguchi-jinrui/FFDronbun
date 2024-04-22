## -------------------------------------------------------------------------------------------------------------------------
## 確認した時間  
female_time <- read_csv("../Data/data/others/female_pre_time.csv") %>% 
  mutate(date = as_date(date))


## -------------------------------------------------------------------------------------------------------------------------
datatable(female_time,
          options = list(scrollX = 50))


## -------------------------------------------------------------------------------------------------------------------------
#個体の属性情報
att <- read_csv("../Data/data/others/attributes_sp_over6.csv")


## -------------------------------------------------------------------------------------------------------------------------
datatable(att)


## -------------------------------------------------------------------------------------------------------------------------
female_time %>% 
  pivot_longer(cols = Kur:Cur,
               names_to = "femaleID",
               values_to = "presence") %>% 
  mutate(date = as_date(date)) %>% 
  ## 2022交尾期のデータは含めない  
  filter(date <= "2022-08-31") %>% 
  ## 調査期間の情報を追加  
  left_join(group_all %>% select(date, study_period) %>% distinct(date, study_period)) %>% 
  left_join(att, by = c("study_period","femaleID")) -> female_time_long
  
female_time_long %>% 
  ## 死亡個体(DD)と確認時間不明個体(?)、非対象個体(6歳未満)(NS)は除く
  filter((presence != "?" & presence != "DD" & presence != "NS")| is.na(presence)) %>% 
  ## 確認時刻をdatetime形式に
  mutate(found_time = str_c(date," ", presence)) %>% 
  mutate(found_time = as_datetime(found_time)) %>%    
  ## 確認の有無についての列を作成  
  mutate(confirmed = ifelse(is.na(presence),0,1)) %>% 
  mutate(date = as_date(date)) %>% 
  ## 1日前の日付を表す列を作成  
  mutate(date_pre = date-1) -> female_time_long_b


## -------------------------------------------------------------------------------------------------------------------------
## 2つ以上の集団を観察した日に小さかった集団のID
exc_groupID <- c("m19_23","m19_24","m19_41","m19_53","m19_54","nm21_20","nm21_22","nm21_43","m20_52","m20_57","m21_35","m21_54")

## 各観察日の最大個体と確認個体数(確認時刻が記録できているメスのみ)を算出
female_time %>% 
  pivot_longer(cols = Kur:Cur,
               names_to = "femaleID",
               values_to = "presence") %>% 
  filter(is.na(presence)|(presence != "DD" & presence != "NS")) %>% 
  group_by(date) %>% 
  mutate(max_female = n()) %>% 
  distinct(date, max_female)  -> max_female

group_all %>% 
  select(groupID, study_period, date, Kur:Yun) %>% 
  select(-c(TY,IT, LK,KR,KM,TG)) %>% 
  pivot_longer(cols = Kur:Yun,
               names_to = "femaleID",
               values_to = "presence") %>% 
  left_join(female_time_long_b %>% select(-study_period, -presence), by = c("date","femaleID")) %>% 
  ## 6歳以上でかつ確認時刻が記録できている個体のみを抽出
  filter(age >= 6, confirmed == 1) %>% 
  ## groupIDごとに個体数を算出
  group_by(date, groupID) %>% 
  summarise(no_female = sum(presence, na.rm = TRUE)) %>% 
  ungroup() %>% 
  ## groupIDが上記のものは除く  
  filter(!(groupID %in% exc_groupID)) %>% 
  left_join(max_female, by = "date")  -> no_female

## 確認時刻データを結合する  
female_time_long_b %>% 
  ## 前日に全頭いたかの列を作成  
  left_join(no_female %>% select(-groupID), by = c("date_pre" = "date")) %>% 
  mutate(inc = ifelse(no_female == max_female,1,0)) %>% 
  select(-max_female, -no_female) %>% 
  ## 当日に全頭いたかの列を作成
  left_join(no_female, by = "date") %>% 
  mutate(inc2 = ifelse(max_female == no_female,1,0)) %>% 
  ## 前日も当日も全頭確認できている日のみを抽出  
  filter(inc == 1 & inc2 == 1)  -> female_time_anal
  
## 中断があり、中断以前に全頭が見つけられていない日は除く  
female_time_anal %>% 
  group_by(date, groupID) %>% 
  summarise(last_found = max(found_time)) %>% 
  ungroup() %>% 
  left_join(group_all %>% select(1:6), by = c("groupID", "date")) %>% 
  mutate(exclude = ifelse(is.na(suspend),0,
                          ifelse(last_found < suspend, 0, 1))) %>% 
  select(-last_found) -> suspend_exclude

female_time_anal %>% 
  left_join(suspend_exclude, by = c("date", "groupID")) %>% 
  ## 中断以前に全頭見つかっていない日は除く（結果的に0）
  filter(exclude != 1) %>% 
  ## 各個体が開始から何分で見つかったか
  mutate(dur = as.numeric(found_time - start)/60) %>% 
  ## 各個体は何番目に見つかったか  
  group_by(date) %>% 
  arrange(dur, .by_group = TRUE) %>% 
  mutate(N = 1:n()) %>% 
  ## その時間まで見つかっていた割合
  mutate(prop = N/max_female) %>% 
  ungroup() %>% 
  ## 2018年交尾期のデータは用いない  
  filter(study_period != "m18") -> female_time_anal_b


## -------------------------------------------------------------------------------------------------------------------------
datatable(female_time_anal_b,
          options = list(scrollX = 60,
                         filter = "top"))


## -------------------------------------------------------------------------------------------------------------------------
exc_date <- c(as_date("2019-09-28"),as_date("2019-09-29"), as_date("2019-09-30"), 
              as_date("2021-12-08"), as_date("2022-03-28"))

group_all %>% 
  select(groupID, study_period, start:fin, date, Kur:Yun) %>% 
  select(-c(TY,IT, LK,KR,KM,TG)) %>% 
  ## 縦型に変換  
  pivot_longer(cols = Kur:Yun,
               names_to = "femaleID",
               values_to = "presence") %>% 
  right_join(female_time_long_b %>% select(-study_period, -presence), by = c("date","femaleID")) %>% 
  ## 2つ以上の集団を観察した場合、小さい方は除く  
  filter(!(groupID %in% exc_groupID)) %>% 
  ## 上記の日付は除く  
  filter(!(date %in% exc_date)) %>% 
  ## 中断がある日は除く  
  filter(is.na(suspend)) %>% 
  ## 調査のない日は除く %>% 
  filter(!is.na(start)) %>% 
  ## 追跡時間
  mutate(dur_follow = as.numeric(difftime(fin, start, units = "mins"))) %>% 
  ## 2018年交尾期のデータは用いない  
  filter(study_period != "m18") %>% 
  ## 各個体が開始から何分で見つかったか
  ## 見つかっていない場合は追跡時間  
  mutate(dur = ifelse(presence == 1,
                      as.numeric(difftime(found_time, start, units = "mins")),
                      dur_follow)) %>% 
  ## 各個体は何番目に見つかったか  
  group_by(date) %>% 
  arrange(dur, .by_group = TRUE) %>% 
  mutate(N = 1:n()) %>% 
  ungroup() %>% 
  left_join(no_female, by = c("date","groupID")) %>% 
  mutate(prop = N/max_female) %>% 
  ## 複数の集団を発見したとき、小さい集団にいた方の個体のconfirmedを0にする  
  mutate(confirmed = ifelse(confirmed == 1 & presence == 0,
                            presence, confirmed)) -> female_time_anal_all


## -------------------------------------------------------------------------------------------------------------------------
datatable(female_time_anal_all,
          options = list(scrollX = 60,
                         filter = "top"))


## -------------------------------------------------------------------------------------------------------------------------
exc_date <- c(as_date("2019-09-28"),as_date("2019-09-29"), as_date("2019-09-30"), 
              as_date("2021-12-08"), as_date("2022-03-28"))

group_all %>% 
  select(groupID, study_period, start:fin, date, Kur:Yun) %>% 
  select(-c(TY,IT, LK,KR,KM,TG)) %>% 
  ## 縦型に変換  
  pivot_longer(cols = Kur:Yun,
               names_to = "femaleID",
               values_to = "presence") %>% 
  right_join(female_time_long_b %>% select(-study_period, -presence), by = c("date","femaleID")) %>% 
  ## 2つ以上の集団を観察した場合、小さい方は除く  
  filter(!(groupID %in% exc_groupID)) %>% 
  ## 上記の日付は除く  
  filter(!(date %in% exc_date)) %>% 
  ## 中断以後に見つけた個体は除く    
  filter(is.na(suspend)|found_time <= suspend) %>% 
  ## 調査のない日は除く %>% 
  filter(!is.na(start)) %>% 
  ## 追跡時間
  mutate(dur_follow = as.numeric(difftime(fin, start, units = "mins"))) %>% 
  ## 2018年交尾期のデータは用いない  
  filter(study_period != "m18") %>% 
  ## 各個体が開始から何分で見つかったか
  ## 見つかっていない場合は追跡時間  
  mutate(dur = ifelse(presence == 1,
                      as.numeric(difftime(found_time, start, units = "mins")),
                      dur_follow)) %>% 
  left_join(no_female, by = c("date","groupID")) %>% 
  ## 複数の集団を発見したとき、小さい集団にいた方の個体のconfirmedを0にする  
  mutate(confirmed = ifelse(confirmed == 1 & presence == 0,
                            presence, confirmed)) %>% 
  filter(confirmed == 1) -> female_time_anal_all_pre


## -------------------------------------------------------------------------------------------------------------------------
datatable(female_time_anal_all_pre,
          options = list(scrollX = 60,
                         filter = "top"))


## ----fig.dim = c(12,4.5)--------------------------------------------------------------------------------------------------
female_time_anal_b %>% 
  ggplot(aes(x= dur, y = N))+
  geom_step(aes(color = study_period, group = date))+
  theme_bw(base_size = 16)+
  labs(y = "Cumulative number of \nconfirmed individuals")+
  theme(aspect.ratio = 1,
        axis.title.y = element_text(family = "Times New Roman"),
        legend.position = "none") -> p_cumsum

female_time_anal_b %>% 
  ggplot(aes(x= dur, y = prop))+
  geom_step(aes(color = study_period, group = date))+
  theme_bw(base_size = 16)+
  labs(y = "Cumulative proportion of \nconfirmed individuals",
       color = "Study period")+
  theme(aspect.ratio = 1,
        axis.title = element_text(family = "Times New Roman"),
        axis.text = element_text(family = "Times New Roman")) -> p_prop

p_cumsum_allind <- p_cumsum + p_prop

p_cumsum_allind

# ggsave("figures/p_cumsum_allind.png", p_cumsum_allind,
#        width = 240, height = 120, dpi = 600, units = "mm")


## ----fig.height = 4.5-----------------------------------------------------------------------------------------------------
female_time_anal_b %>% 
  group_by(date) %>% 
  summarise(last_time = max(dur)) %>% 
  ungroup() -> last_time
  
count <- hist(last_time$last_time,breaks = seq(0,620,10), plot = F)
cumsum <- data.frame(x = count$breaks[-63],
                     cumsum = (cumsum(count$counts)/max(cumsum(count$counts)))*10)

ggplot(last_time, aes(x = last_time))+
  geom_histogram(fill = "white",
                 color = "black",
                 binwidth = 10)+
  geom_line(data = cumsum, aes(x = x, y = cumsum),
            color = "red2", linetype = "dashed")+
  theme_bw(base_size = 16)+
  theme(aspect.ratio = 1)+
  scale_y_continuous(name = "Frequency", breaks = seq(0,10.2),
                     sec.axis= sec_axis(name = "Cumulative density", trans = ~.*0.1,
                                        breaks = seq(0,1,0.1)))+
  labs(x = "Time elapsed until the last individual")


## -------------------------------------------------------------------------------------------------------------------------
female_time_anal_b %>% 
  mutate(date = as.factor(date)) -> female_time_anal_b

mod_cox   <- brm(dur | cens(1 - confirmed) ~ 0 + study_period + age + rank + 
                   (1|date) + (1|femaleID),
                 data = female_time_anal_b,
                 iter = 5000,
                 warmup = 2500,
                 family = brmsfamily("cox"),
                 backend = "cmdstanr",
                 file = "model/mod_cox")


## -------------------------------------------------------------------------------------------------------------------------
posterior_summary(mod_cox) %>% 
  head(8) %>% 
  data.frame() %>% 
  mutate(across(1:4, ~round(.,3)))


## -------------------------------------------------------------------------------------------------------------------------
posterior_samples(mod_cox) %>% 
  select(1:6) %>% 
  mutate(m19_m20 = b_study_periodm19 - b_study_periodm20,
         m19_m21 = b_study_periodm19 - b_study_periodm21,
         m19_nm19 = b_study_periodm19 - b_study_periodnm19,
         m19_nm21 = b_study_periodm19 - b_study_periodnm21,
         m19_nm22 = b_study_periodm19 - b_study_periodnm22,
         m20_m21 = b_study_periodm20 - b_study_periodm21,
         m20_nm19 = b_study_periodm20 - b_study_periodnm19,
         m20_nm21 = b_study_periodm20 - b_study_periodnm21,
         m20_nm22 = b_study_periodm20 - b_study_periodnm22,
         m21_nm19 = b_study_periodm21 - b_study_periodnm19,
         m21_nm21 = b_study_periodm21 - b_study_periodnm21,
         m21_nm22 = b_study_periodm21 - b_study_periodnm22,
         nm19_nm21 = b_study_periodnm19 - b_study_periodnm21,
         nm19_nm22 = b_study_periodnm19 - b_study_periodnm22,
         nm21_nm22 = b_study_periodnm21 - b_study_periodnm22) %>% 
  select(-(1:6)) %>% 
  pivot_longer(everything()) %>% 
  group_by(name) %>% 
  summarise(mean = mean(value),
            lower = quantile(value, 0.025),
            upper = quantile(value, 0.975)) %>% 
  mutate(across(where(is.numeric), ~round(.,3))) %>% 
  separate(name, sep = "_", into = c("Level1", "Level2"))


## -------------------------------------------------------------------------------------------------------------------------
female_time_anal_b %>% 
  mutate(max_dur = as.numeric(fin - start)*60) %>% 
  group_by(date, femaleID) %>% 
  tidyr::expand(time = 1:max_dur) %>% 
  ungroup() -> female_dur_all

female_dur_all %>% 
  left_join(female_time_anal_b, by = c("date","femaleID")) %>% 
  filter(time <= dur) %>% 
  mutate(confirmed = ifelse(time == dur,1,0)) -> female_time_dcr


## -------------------------------------------------------------------------------------------------------------------------
female_time_dcr <- female_time_dcr %>% 
  mutate(date = as.factor(date)) %>% 
  mutate(age_std = standardize(age),
         time_scaled = time/max(time)) %>% 
  group_by(study_period) %>% 
  mutate(rank_scaled = rank/max(rank)) %>% 
  ungroup() %>% 
  mutate(rank_std = standardize(rank_scaled))

## 1次のみ
mod_dcr <- brm(confirmed ~ study_period + time  + rank_std + age_std + (1|date) + (1|femaleID),
               data = female_time_dcr,
               family = "bernoulli",
               iter = 11000, warmup = 6000, seed = 13,
               prior = c(prior(student_t(4,0,10),class = Intercept),
               prior(student_t(4,0,10), class = b),
               prior(student_t(4,0,5), class = sd)),
               control=list(adapt_delta = 0.999, max_treedepth = 20),
               backend = "cmdstanr",
               file = "model/mod_dcr")

## 2次まで  
mod_dcr2 <- brm(confirmed ~ study_period + poly(time, 2, raw = FALSE) + rank_std + age_std +
                  (1|date) + (1|femaleID),
               data = female_time_dcr,
               family = "bernoulli",
               iter = 11000, warmup = 6000, seed = 123,
               prior = c(prior(student_t(4,0,10),class = Intercept),
               prior(student_t(4,0,10), class = b),
               prior(student_t(4,0,5), class = sd, lb = 0)),
               control=list(adapt_delta = 0.99, max_treedepth = 15),
               backend = "cmdstanr",
               file = "model/mod_dcr2")

## 3次まで  
mod_dcr3 <- brm(confirmed ~ study_period + poly(time, 3, raw = FALSE) + rank_std + age_std +
                  (1|date) + (1|femaleID),
               data = female_time_dcr,
               family = "bernoulli",
               iter = 11000, warmup = 6000, seed = 123,
               prior = c(prior(student_t(4,0,10),class = Intercept),
               prior(student_t(4,0,10), class = b),
               prior(student_t(4,0,5), class = sd, lb = 0)),
               control=list(adapt_delta = 0.99, max_treedepth = 15),
               backend = "cmdstanr",
               file = "model/mod_dcr3")


## ----fig.height = 4-------------------------------------------------------------------------------------------------------
dharma_dcr <- dh_check_brms(mod_dcr, quantreg = TRUE)
dharma_dcr2 <- dh_check_brms(mod_dcr2, quantreg = TRUE)
dharma_dcr3 <- dh_check_brms(mod_dcr3, quantreg = TRUE)


## ----fig.height = 5-------------------------------------------------------------------------------------------------------
ppcheck_dcr <- pp_check(mod_dcr, ndraws = 100)+
  theme_bw()+
  theme(aspect.ratio = 1)+
  guides(color = "none")

ppcheck_dcr2 <- pp_check(mod_dcr2, ndraws = 100)+
  theme_bw()+
  theme(aspect.ratio = 1)+
  guides(color = "none")

ppcheck_dcr3 <- pp_check(mod_dcr3, ndraws = 100)+
  theme_bw()+
  theme(aspect.ratio = 1)

ppcheck_dcr + ppcheck_dcr2 + ppcheck_dcr3


## -------------------------------------------------------------------------------------------------------------------------
check_collinearity(mod_dcr)
check_collinearity(mod_dcr2)
check_collinearity(mod_dcr3)


## -------------------------------------------------------------------------------------------------------------------------
model_parameters(mod_dcr) %>% 
  data.frame() %>% 
  mutate("95%CI" = str_c("[",sprintf("%.2f",CI_low),",",sprintf("%.2f",CI_high),"]")) %>%
  select(c(1,2,9,7,8))

model_parameters(mod_dcr2) %>% 
  data.frame() %>% 
  mutate("95%CI" = str_c("[",sprintf("%.2f",CI_low),",",sprintf("%.2f",CI_high),"]")) %>%
  select(c(1,2,9,7,8))

model_parameters(mod_dcr3) %>% 
  data.frame() %>% 
  mutate("95%CI" = str_c("[",sprintf("%.2f",CI_low),",",sprintf("%.2f",CI_high),"]")) %>%
  select(c(1,2,9,7,8))


## -------------------------------------------------------------------------------------------------------------------------
estimate_contrasts(mod_dcr, contrast = "study_period") %>%
  data.frame() %>%
  select(1:6) %>%
  mutate(across(where(is.numeric), ~round(., 3))) %>% 
  mutate(model = "model1") %>% 
  bind_rows(estimate_contrasts(mod_dcr2, contrast = "study_period") %>%
  data.frame() %>%
  select(1:6) %>%
  mutate(across(where(is.numeric), ~round(., 3))) %>% 
  mutate(model = "model2")) %>% 
  bind_rows(estimate_contrasts(mod_dcr3, contrast = "study_period") %>%
  data.frame() %>%
  select(1:6) %>%
  mutate(across(where(is.numeric), ~round(., 3))) %>% 
  mutate(model = "model3")) %>% 
  select(model, everything(), -pd) %>% 
  flextable() %>% 
  colformat_double(digits=2) %>% 
  set_table_properties(layout="autofit",width = 1) %>% 
  autofit(add_w = 0.2) %>% 
  merge_v(j = "model") %>% 
  theme_zebra() 


## -------------------------------------------------------------------------------------------------------------------------
# nd <- tidyr::crossing(time = seq(0,750,by = 1),
#                rank_scaled = c(0,0.5,1),
#                age_std = mean(female_time_dcr$age_std),
#                study_period = c("m19","m20","m21","nm19","nm21","nm22")) %>% 
#   mutate(rank_std = (rank_scaled - mean(female_time_dcr$rank_scaled))/sd(female_time_dcr$rank_scaled)) 
# 
# fit_dcr <- fitted(mod_dcr,
#            newdata = nd,
#            scale = "response",
#            re_formula = NA) %>%
#   bind_cols(nd) %>%
#   arrange(study_period, rank_scaled, time)
# 
# fit_dcr2 <- fitted(mod_dcr2,
#            newdata = nd,
#            scale = "response",
#            re_formula = NA) %>%
#   bind_cols(nd) %>%
#   arrange(study_period, rank_scaled, time)
# 
# fit_dcr3 <- fitted(mod_dcr3,
#            newdata = nd,
#            scale = "response",
#            re_formula = NA) %>%
#   bind_cols(nd) %>%
#   arrange(study_period, rank_scaled, time)
# 
# fit_dcr %>% 
#   mutate(rank_scaled = as.factor(rank_scaled)) %>% 
#   ggplot(aes(x = time, y = Estimate))+
#   geom_line(aes(color = rank_scaled, linetype = rank_scaled),
#             linewidth = 1)+
#   scale_linetype_manual(values = c("dotted","solid","dashed"))+
#   geom_ribbon(aes(ymin = Q2.5, ymax = Q97.5,
#                   fill = rank_scaled),
#               alpha = 0.1)+
#   theme_bw(base_size = 15)+
#   theme(aspect.ratio = 1,
#         strip.background = element_blank(),
#         axis.title = element_text("Times New Roman"),
#         axis.text = element_text("Times New Roman"),
#         strip.text = element_text(hjust = 0))+
#   labs(x = "Time spent", y = "Probability of being confirmed") +
#   facet_rep_wrap(~study_period,
#                  repeat.tick.labels = TRUE)+
#   labs(title = "一次の項のみ")  -> p_dcr1
# 
# fit_dcr2 %>%
#   mutate(rank_scaled = as.factor(rank_scaled)) %>%
#   ggplot(aes(x = time, y = Estimate))+
#   geom_line(aes(color = rank_scaled, linetype = rank_scaled),
#             linewidth = 1)+
#   scale_linetype_manual(values = c("dotted","solid","dashed"))+
#   geom_ribbon(aes(ymin = Q2.5, ymax = Q97.5,
#                   fill = rank_scaled),
#               alpha = 0.1)+
#   theme_bw(base_size = 15)+
#   theme(aspect.ratio = 1,
#         strip.background = element_blank(),
#         axis.title = element_text("Times New Roman"),
#         axis.text = element_text("Times New Roman"),
#         strip.text = element_text(hjust = 0))+
#   labs(x = "Time spent", y = "Probability of being confirmed") +
#   facet_rep_wrap(~study_period,
#                  repeat.tick.labels = TRUE)+
#   labs(title = "二次の項まで")  -> p_dcr2
# 
# fit_dcr3 %>% 
#   mutate(rank_scaled = as.factor(rank_scaled)) %>% 
#   ggplot(aes(x = time, y = Estimate))+
#   geom_line(aes(color = rank_scaled, linetype = rank_scaled),
#             linewidth = 1)+
#   scale_linetype_manual(values = c("dotted","solid","dashed"))+
#   geom_ribbon(aes(ymin = Q2.5, ymax = Q97.5,
#                   fill = rank_scaled),
#               alpha = 0.1)+
#   theme_bw(base_size = 15)+
#   theme(aspect.ratio = 1,
#         strip.background = element_blank(),
#         axis.title = element_text("Times New Roman"),
#         axis.text = element_text("Times New Roman"),
#         strip.text = element_text(hjust = 0))+
#   labs(x = "Time spent", y = "Probability of being confirmed") +
#   facet_rep_wrap(~study_period,
#                  repeat.tick.labels = TRUE)+
#   labs(title = "三次の項まで")  -> p_dcr3
# 
# p_dcr <- p_dcr1 / p_dcr2 / p_dcr3
# 
# ggsave("figures/p_dcr.png", p_dcr, dpi = 600,
#        width = 220, height = 360, units = "mm")


## ----p-dcr, out.width = "80%", fig.align = "center", echo = FALSE, fig.cap = "その時間まで見つからなかった各メスの発見確率"---------------------
knitr::include_graphics("figures/p_dcr.png")


## -------------------------------------------------------------------------------------------------------------------------
# fit_dcr_b <- fit_dcr %>% 
#   arrange(study_period, rank_scaled, time) %>% 
#   group_by(study_period, rank_scaled) %>% 
#   mutate(mean = 1 - cumprod(1-Estimate),
#          upper = 1 - cumprod(1-Q97.5),
#          lower = 1 - cumprod(1 - Q2.5)) %>% 
#   ungroup()
# 
# fit_dcr2_b <- fit_dcr2 %>% 
#   arrange(study_period, rank_scaled, time) %>% 
#   group_by(study_period, rank_scaled) %>% 
#   mutate(mean = 1 - cumprod(1-Estimate),
#          upper = 1 - cumprod(1-Q97.5),
#          lower = 1 - cumprod(1 - Q2.5)) %>% 
#   ungroup()
# 
# fit_dcr3_b <- fit_dcr3 %>% 
#   arrange(study_period, rank_scaled, time) %>% 
#   group_by(study_period, rank_scaled) %>% 
#   mutate(mean = 1 - cumprod(1-Estimate),
#          upper = 1 - cumprod(1-Q97.5),
#          lower = 1 - cumprod(1 - Q2.5)) %>% 
#   ungroup()
# 
# fit_dcr_b %>%
#   mutate(rank_scaled = as.factor(rank_scaled)) %>%
#   ggplot(aes(x = time, y = mean))+
#   geom_line(aes(color = rank_scaled,
#                 linetype = rank_scaled),
#             linewidth = 1)+
#   scale_linetype_manual(values = c("dotted", "solid", "dashed"))+
#   geom_ribbon(aes(ymin = lower,
#                   ymax = upper,
#                   fill = rank_scaled),
#               alpha = 0.1)+
#   theme_bw(base_size = 16)+
#   theme(aspect.ratio = 1,
#         strip.background = element_blank(),
#         axis.title = element_text("Times New Roman"),
#         axis.text = element_text("Times New Roman"),
#         strip.text = element_text(hjust = 0))+
#   facet_rep_wrap(~study_period, repeat.tick.labels = TRUE)+
#   scale_x_continuous(breaks = seq(0,700, 200))+
#   scale_y_continuous(breaks = seq(0,1,0.2))+
#   labs(x = "Time spent", y = "Cumulative probability of being confirmed",
#        color = "scaled rank", fill = "scaled rank",
#        linetype = "scaled rank")+
#   labs(title = "一次の項まで")+
#   guides(linetype = guide_legend(override.aes = list(linewidth = 0.6))) -> p_dcr1_cum
# 
# fit_dcr2_b %>%
#   mutate(rank_scaled = as.factor(rank_scaled)) %>%
#   ggplot(aes(x = time, y = mean))+
#   geom_line(aes(color = rank_scaled,
#                 linetype = rank_scaled),
#             linewidth = 1)+
#   scale_linetype_manual(values = c("dotted", "solid", "dashed"))+
#   geom_ribbon(aes(ymin = lower,
#                   ymax = upper,
#                   fill = rank_scaled),
#               alpha = 0.1)+
#   theme_bw(base_size = 16)+
#   theme(aspect.ratio = 1,
#         strip.background = element_blank(),
#         axis.title = element_text("Times New Roman"),
#         axis.text = element_text("Times New Roman"),
#         strip.text = element_text(hjust = 0))+
#   facet_rep_wrap(~study_period, repeat.tick.labels = TRUE)+
#   scale_x_continuous(breaks = seq(0,700, 200))+
#   scale_y_continuous(breaks = seq(0,1,0.2))+
#   labs(x = "Time spent", y = "Cumulative probability of being confirmed",
#        color = "scaled rank", fill = "scaled rank",
#        linetype = "scaled rank")+
#   labs(title = "二次の項まで") +
#   guides(linetype = guide_legend(override.aes = list(linewidth = 0.6))) -> p_dcr2_cum
# 
# fit_dcr3_b %>%
#   mutate(rank_scaled = as.factor(rank_scaled)) %>%
#   ggplot(aes(x = time, y = mean))+
#   geom_line(aes(color = rank_scaled,
#                 linetype = rank_scaled),
#             linewidth = 1)+
#   scale_linetype_manual(values = c("dotted", "solid", "dashed"))+
#   geom_ribbon(aes(ymin = lower,
#                   ymax = upper,
#                   fill = rank_scaled),
#               alpha = 0.1)+
#   theme_bw(base_size = 16)+
#   theme(aspect.ratio = 1,
#         strip.background = element_blank(),
#         axis.title = element_text("Times New Roman"),
#         axis.text = element_text("Times New Roman"),
#         strip.text = element_text(hjust = 0))+
#   facet_rep_wrap(~study_period, repeat.tick.labels = TRUE)+
#   scale_x_continuous(breaks = seq(0,700, 200))+
#   scale_y_continuous(breaks = seq(0,1,0.2))+
#   labs(x = "Time spent", y = "Cumulative probability of being confirmed",
#        color = "scaled rank", fill = "scaled rank",
#        linetype = "scaled rank") +
#   labs(title = "三次の項まで") +
#   guides(linetype = guide_legend(override.aes = list(linewidth = 0.6))) -> p_dcr3_cum
# 
# p_dcr_cum <- p_dcr1_cum / p_dcr2_cum / p_dcr3_cum
#
# ggsave("figures/p_dcr_cum.png", p_dcr_cum, dpi = 600,
#         width = 220, height = 360, units = "mm")
# 
# ggsave("figures/p_dcr1_cum.png", p_dcr1_cum, dpi = 600,
#          width = 220, height = 150, units = "mm")


## ----p-dcr-cum, out.width = "80%", fig.align = "center", echo = FALSE, fig.cap = "それぞれの時間までの累積発見確率"-----------------------
knitr::include_graphics("figures/p_dcr_cum.png")  


## ----fig.dim = c(12,4.5)--------------------------------------------------------------------------------------------------
female_time_anal_all %>% 
  filter(presence == 1) %>% 
  ggplot(aes(x= dur, y = N))+
  geom_step(aes(color = study_period, group = date))+
  theme_bw(base_size = 16)+
  labs(y = "Cumulative number of \nconfirmed individuals",
       x = "Time elapsed since the \ntroop was found")+
  theme(aspect.ratio = 1,
        axis.title = element_text(family = "Times New Roman"),
        axis.text = element_text(family = "Times New Roman"),
        legend.position = "none") -> p_cumsum_all

female_time_anal_all %>% 
  filter(presence == 1) %>% 
  ggplot(aes(x= dur, y = prop))+
  geom_step(aes(color = study_period, group = date))+
  theme_bw(base_size = 16)+
  labs(y = "Cumulative proportion of \nconfirmed individuals",
       color = "Study period",
       x = "Time elapsed since the \ntroop was found")+
  theme(aspect.ratio = 1,
        axis.title = element_text(family = "Times New Roman"),
        axis.text = element_text(family = "Times New Roman")) -> p_prop_all

p_cumsum_all_b <- p_cumsum_all + p_prop_all

p_cumsum_all_b
# 
# ggsave("figures/p_cumsum_all_b.png", p_cumsum_all_b,
#        width = 240, height = 120, dpi = 600, units = "mm")


## -------------------------------------------------------------------------------------------------------------------------
female_time_anal_all %>% 
  mutate(date = as.factor(date)) -> female_time_anal_all_b

mod_cox_all <- brm(dur | cens(1 - confirmed) ~ 0 + study_period + age + rank + 
                   (1|date) + (1|femaleID),
                 data = female_time_anal_all_b,
                 iter = 11000,
                 warmup = 6000,
                 family = brmsfamily("cox"),
                 backend = "cmdstanr",
                 file = "model/mod_cox_all")


## -------------------------------------------------------------------------------------------------------------------------
posterior_summary(mod_cox_all) %>% 
  head(8) %>% 
  data.frame() %>% 
  mutate(across(1:4, ~round(.,3)))


## -------------------------------------------------------------------------------------------------------------------------
posterior_samples(mod_cox_all) %>% 
  select(1:6) %>% 
  mutate(m19_m20 = b_study_periodm19 - b_study_periodm20,
         m19_m21 = b_study_periodm19 - b_study_periodm21,
         m19_nm19 = b_study_periodm19 - b_study_periodnm19,
         m19_nm21 = b_study_periodm19 - b_study_periodnm21,
         m19_nm22 = b_study_periodm19 - b_study_periodnm22,
         m20_m21 = b_study_periodm20 - b_study_periodm21,
         m20_nm19 = b_study_periodm20 - b_study_periodnm19,
         m20_nm21 = b_study_periodm20 - b_study_periodnm21,
         m20_nm22 = b_study_periodm20 - b_study_periodnm22,
         m21_nm19 = b_study_periodm21 - b_study_periodnm19,
         m21_nm21 = b_study_periodm21 - b_study_periodnm21,
         m21_nm22 = b_study_periodm21 - b_study_periodnm22,
         nm19_nm21 = b_study_periodnm19 - b_study_periodnm21,
         nm19_nm22 = b_study_periodnm19 - b_study_periodnm22,
         nm21_nm22 = b_study_periodnm21 - b_study_periodnm22) %>% 
  select(-(1:6)) %>% 
  pivot_longer(everything()) %>% 
  group_by(name) %>% 
  summarise(mean = mean(value),
            lower = quantile(value, 0.025),
            upper = quantile(value, 0.975)) %>% 
  mutate(across(where(is.numeric), ~round(.,3))) %>% 
  separate(name, sep = "_", into = c("Level1", "Level2"))


## -------------------------------------------------------------------------------------------------------------------------
female_time_anal_all %>% 
  group_by(date, femaleID) %>% 
  tidyr::expand(time = 1:dur_follow) %>% 
  ungroup() -> female_dur_all_b

female_dur_all_b %>% 
  left_join(female_time_anal_all, by = c("date","femaleID")) %>% 
  filter(time <= dur) %>% 
  mutate(confirmed = ifelse(confirmed == 1, 
                            ifelse(time == dur,1,0),
                            0)) -> female_time_dcr_all


## -------------------------------------------------------------------------------------------------------------------------
female_time_dcr_all <- female_time_dcr_all %>% 
  mutate(date = as.factor(date)) %>% 
  mutate(age_std = standardize(age),
         time_scaled = time/max(time),
         time_std = standardize(time)) %>% 
  group_by(study_period) %>% 
  mutate(rank_scaled = rank/max(rank)) %>% 
  ungroup() %>% 
  mutate(rank_std = standardize(rank_scaled))

## 2次まで  
mod_dcr2_all <- brm(confirmed ~ study_period + poly(time, 2, raw = FALSE) + rank_std + age_std +
                  (1|date) + (1|femaleID),
               data = female_time_dcr_all,
               family = "bernoulli",
               iter = 11000, warmup = 6000, seed = 123,
               prior = c(prior(student_t(4,0,10),class = Intercept),
               prior(student_t(4,0,10), class = b),
               prior(student_t(4,0,5), class = sd, lb = 0)),
               control=list(adapt_delta = 0.99, max_treedepth = 15),
               backend = "cmdstanr",
               file = "model/mod_dcr2_all")

## 3次まで  
mod_dcr3_all <- brm(confirmed ~ study_period + poly(time, 3, raw = FALSE) + rank_std + age_std +
                  (1|date) + (1|femaleID),
               data = female_time_dcr_all,
               family = "bernoulli",
               iter = 11000, warmup = 6000, seed = 123,
               prior = c(prior(student_t(4,0,10),class = Intercept),
               prior(student_t(4,0,10), class = b),
               prior(student_t(4,0,5), class = sd, lb = 0)),
               control=list(adapt_delta = 0.99, max_treedepth = 15),
               backend = "cmdstanr",
               file = "model/mod_dcr3_all")


## ----fig.height = 4-------------------------------------------------------------------------------------------------------
# dharma_dcr2_all <- dh_check_brms(mod_dcr2_all, quantreg = TRUE)
# saveRDS(dharma_dcr2_all, "model/dharma_dcr2_all.rds")


## ----fig.height = 4-------------------------------------------------------------------------------------------------------
# dharma_dcr3_all <- dh_check_brms(mod_dcr3_all, quantreg = TRUE)
# saveRDS(dharma_dcr3_all, "model/dharma_dcr3_all.rds")


## ----fig.height = 5-------------------------------------------------------------------------------------------------------
# ppcheck_dcr2_all <- pp_check(mod_dcr2_all, ndraws = 100)+
#   theme_bw()+
#   theme(aspect.ratio = 1)+
#   guides(color = "none")
# 
# ppcheck_dcr3_all <- pp_check(mod_dcr3_all, ndraws = 100)+
#   theme_bw()+
#   theme(aspect.ratio = 1)
# 
# ppcheck_dcr2 + ppcheck_dcr3


## -------------------------------------------------------------------------------------------------------------------------
check_collinearity(mod_dcr2_all)
check_collinearity(mod_dcr3_all)


## -------------------------------------------------------------------------------------------------------------------------
model_parameters(mod_dcr2_all) %>% 
  data.frame() %>% 
  mutate("95%CI" = str_c("[",sprintf("%.2f",CI_low),",",sprintf("%.2f",CI_high),"]")) %>%
  select(c(1,2,9,7,8))

model_parameters(mod_dcr3_all) %>% 
  data.frame() %>% 
  mutate("95%CI" = str_c("[",sprintf("%.2f",CI_low),",",sprintf("%.2f",CI_high),"]")) %>%
  select(c(1,2,9,7,8))


## -------------------------------------------------------------------------------------------------------------------------
nd_all <- tidyr::crossing(time = seq(0,750,by = 1),
               rank_scaled = c(0,0.5,1),
               age_std = mean(female_time_dcr_all$age_std),
               study_period = c("m19","m20","m21","nm19","nm21","nm22")) %>%
  mutate(rank_std = (rank_scaled - mean(female_time_dcr_all$rank_scaled))/sd(female_time_dcr_all$rank_scaled))

## 二次の項までを含むモデル  
# fit_dcr2_all <- fitted(mod_dcr2_all,
#                        newdata = nd_all,
#                        scale = "response",
#                        re_formula = NA) %>%
#                 bind_cols(nd_all) %>%
#                 arrange(study_period, rank_scaled, time)
# 
# saveRDS(fit_dcr2_all, "model/fit_dcr2_all.rds")

fit_dcr2_all <- readRDS("model/fit_dcr2_all.rds")


fit_dcr2_all %>%
  mutate(rank_scaled = as.factor(rank_scaled)) %>%
  ggplot(aes(x = time, y = Estimate))+
  geom_line(aes(color = rank_scaled, linetype = rank_scaled),
            linewidth = 1)+
  scale_linetype_manual(values = c("dotted","solid","dashed"))+
  geom_ribbon(aes(ymin = Q2.5, ymax = Q97.5,
                  fill = rank_scaled),
              alpha = 0.1)+
  theme_bw(base_size = 15)+
  theme(aspect.ratio = 1,
        strip.background = element_blank(),
        axis.title = element_text("Times New Roman"),
        axis.text = element_text("Times New Roman"),
        strip.text = element_text(hjust = 0))+
  labs(x = "Time elapsed since the \ntroop was found", 
       y = "Probability of being confirmed",
       color = "Scaled rank", linetype = "Scaled rank",
       fill = "Scaled rank") +
  facet_rep_wrap(~study_period,
                 repeat.tick.labels = TRUE)+
  labs(title = "二次の項まで")  -> p_dcr2_all

p_dcr2_all

## 3次までの項  
# fit_dcr3_all <- fitted(mod_dcr3_all,
#            newdata = nd_all,
#            scale = "response",
#            re_formula = NA) %>%
#   bind_cols(nd_all) %>%
#   arrange(study_period, rank_scaled, time)
# 
# saveRDS(fit_dcr3_all, "model/fit_dcr3_all.rds")
# 
fit_dcr3_all <- readRDS("model/fit_dcr3_all.rds")

fit_dcr3_all %>%
  mutate(rank_scaled = as.factor(rank_scaled)) %>%
  ggplot(aes(x = time, y = Estimate))+
  geom_line(aes(color = rank_scaled, linetype = rank_scaled),
            linewidth = 1)+
  scale_linetype_manual(values = c("dotted","solid","dashed"))+
  geom_ribbon(aes(ymin = Q2.5, ymax = Q97.5,
                  fill = rank_scaled),
              alpha = 0.1)+
  theme_bw(base_size = 15)+
  theme(aspect.ratio = 1,
        strip.background = element_blank(),
        axis.title = element_text("Times New Roman"),
        axis.text = element_text("Times New Roman"),
        strip.text = element_text(hjust = 0))+
  labs(x = "Time spent", y = "Probability of being confirmed") +
  facet_rep_wrap(~study_period,
                 repeat.tick.labels = TRUE)+
  labs(title = "三次の項まで")  -> p_dcr3_all

p_dcr3_all

# ggsave("figures/p_dcr2_all.png",
#        p_dcr2_all, width = 220, height = 120, units = "mm",
#        dpi = 600)
# 
# ggsave("figures/p_dcr3_all.png",
#        p_dcr3_all, width = 220, height = 120, units = "mm",
#        dpi = 600)


## -------------------------------------------------------------------------------------------------------------------------
fit_dcr2_all_b <- fit_dcr2_all %>%
  arrange(study_period, rank_scaled, time) %>%
  group_by(study_period, rank_scaled) %>%
  mutate(mean = 1 - cumprod(1-Estimate),
         upper = 1 - cumprod(1-Q97.5),
         lower = 1 - cumprod(1 - Q2.5)) %>%
  ungroup()

fit_dcr3_all_b <- fit_dcr3_all %>%
  arrange(study_period, rank_scaled, time) %>%
  group_by(study_period, rank_scaled) %>%
  mutate(mean = 1 - cumprod(1-Estimate),
         upper = 1 - cumprod(1-Q97.5),
         lower = 1 - cumprod(1 - Q2.5)) %>%
  ungroup()

fit_dcr2_all_b %>%
  mutate(rank_scaled = as.factor(rank_scaled)) %>%
  ggplot(aes(x = time, y = mean))+
  geom_line(aes(color = rank_scaled,
                linetype = rank_scaled),
            linewidth = 1)+
  scale_linetype_manual(values = c("dotted", "solid", "dashed"))+
  geom_ribbon(aes(ymin = lower,
                  ymax = upper,
                  fill = rank_scaled),
              alpha = 0.1)+
  theme_bw(base_size = 16)+
  theme(aspect.ratio = 1,
        strip.background = element_blank(),
        axis.title.y = element_text(family = "Times New Roman",
                                    size = 13),
        axis.title.x = element_text(family = "Times New Roman"),
        axis.text = element_text(family = "Times New Roman"),
        strip.text = element_text(family = "Times New Roman",
                                  hjust = 0),
        legend.title = element_text(family = "Times New Roman"),
        legend.text = element_text(family = "Times New Roman"))+
  facet_rep_wrap(~study_period, repeat.tick.labels = TRUE)+
  scale_x_continuous(breaks = seq(0,700, 200))+
  scale_y_continuous(breaks = seq(0,1,0.2))+
  labs(x = "Time elapsed since the troop was found", 
       y = "Cumulative probability of being confirmed",
       color = "Scaled rank", fill = "Scaled rank",
       linetype = "Scaled rank")+
  labs(title = "二次の項まで") +
  guides(linetype = guide_legend(override.aes = list(linewidth = 0.6))) -> p_dcr2_all_cum

p_dcr2_all_cum

fit_dcr3_all_b %>%
  mutate(rank_scaled = as.factor(rank_scaled)) %>%
  ggplot(aes(x = time, y = mean))+
  geom_line(aes(color = rank_scaled,
                linetype = rank_scaled),
            linewidth = 1)+
  scale_linetype_manual(values = c("dotted", "solid", "dashed"))+
  geom_ribbon(aes(ymin = lower,
                  ymax = upper,
                  fill = rank_scaled),
              alpha = 0.1)+
  theme_bw(base_size = 16)+
  theme(aspect.ratio = 1,
        strip.background = element_blank(),
        axis.title = element_text(family = "Times New Roman"),
        axis.text = element_text(family = "Times New Roman"),
        strip.text = element_text(family = "Times New Roman",
                                  hjust = 0))+
  facet_rep_wrap(~study_period, repeat.tick.labels = TRUE)+
  scale_x_continuous(breaks = seq(0,700, 200))+
  scale_y_continuous(breaks = seq(0,1,0.2))+
  labs(x = "Time elapsed since the troop was found", 
       y = "Cumulative probability of being confirmed",
       color = "Scaled rank", fill = "Scaled rank",
       linetype = "Scaled rank") +
  labs(title = "三次の項まで") +
  guides(linetype = guide_legend(override.aes = list(linewidth = 0.6))) -> p_dcr3_all_cum

p_dcr3_all_cum

# ggsave("figures/p_dcr2_all_cum.png", p_dcr2_all_cum, dpi = 900,
#         width = 220, height = 120, units = "mm")
# 
# ggsave("figures/p_dcr3_all_cum.png", p_dcr3_all_cum, dpi = 600,
#          width = 220, height = 120, units = "mm")


## -------------------------------------------------------------------------------------------------------------------------
female_time_anal_all_pre %>% 
  mutate(date = as.factor(date)) -> female_time_anal_all_pre_b

mod_cox_all_pre <- brm(dur | cens(1 - confirmed) ~ 0 + study_period + age + rank + 
                   (1|date) + (1|femaleID),
                 data = female_time_anal_all_pre_b,
                 iter = 11000,
                 warmup = 6000,
                 family = brmsfamily("cox"),
                 backend = "cmdstanr",
                 file = "model/mod_cox_all_pre")


## -------------------------------------------------------------------------------------------------------------------------
posterior_summary(mod_cox_all_pre) %>% 
  head(8) %>% 
  data.frame() %>% 
  mutate(across(1:4, ~round(.,3)))


## -------------------------------------------------------------------------------------------------------------------------
posterior_samples(mod_cox_all_pre) %>% 
  select(1:6) %>% 
  mutate(m19_m20 = b_study_periodm19 - b_study_periodm20,
         m19_m21 = b_study_periodm19 - b_study_periodm21,
         m19_nm19 = b_study_periodm19 - b_study_periodnm19,
         m19_nm21 = b_study_periodm19 - b_study_periodnm21,
         m19_nm22 = b_study_periodm19 - b_study_periodnm22,
         m20_m21 = b_study_periodm20 - b_study_periodm21,
         m20_nm19 = b_study_periodm20 - b_study_periodnm19,
         m20_nm21 = b_study_periodm20 - b_study_periodnm21,
         m20_nm22 = b_study_periodm20 - b_study_periodnm22,
         m21_nm19 = b_study_periodm21 - b_study_periodnm19,
         m21_nm21 = b_study_periodm21 - b_study_periodnm21,
         m21_nm22 = b_study_periodm21 - b_study_periodnm22,
         nm19_nm21 = b_study_periodnm19 - b_study_periodnm21,
         nm19_nm22 = b_study_periodnm19 - b_study_periodnm22,
         nm21_nm22 = b_study_periodnm21 - b_study_periodnm22) %>% 
  select(-(1:6)) %>% 
  pivot_longer(everything()) %>% 
  group_by(name) %>% 
  summarise(mean = mean(value),
            lower = quantile(value, 0.025),
            upper = quantile(value, 0.975)) %>% 
  mutate(across(where(is.numeric), ~round(.,3))) %>% 
  separate(name, sep = "_", into = c("Level1", "Level2"))


## -------------------------------------------------------------------------------------------------------------------------
female_time_anal_all_pre %>% 
  group_by(date, femaleID) %>% 
  tidyr::expand(time = 1:dur_follow) %>% 
  ungroup() -> female_dur_all_pre_b

female_dur_all_pre_b %>% 
  left_join(female_time_anal_all_pre, by = c("date","femaleID")) %>% 
  filter(time <= dur) %>% 
  mutate(confirmed = ifelse(confirmed == 1, 
                            ifelse(time == dur,1,0),
                            0)) -> female_time_dcr_all_pre


## -------------------------------------------------------------------------------------------------------------------------
female_time_dcr_all_pre <- female_time_dcr_all_pre %>% 
  mutate(date = as.factor(date)) %>% 
  mutate(age_std = standardize(age),
         time_scaled = time/max(time),
         time_std = standardize(time)) %>% 
  group_by(study_period) %>% 
  mutate(rank_scaled = rank/max(rank)) %>% 
  ungroup() %>% 
  mutate(rank_std = standardize(rank_scaled))

## 2次まで  
mod_dcr2_all_pre <- brm(confirmed ~ study_period + poly(time, 2, raw = FALSE) +
                          rank_std + age_std + (1|date) + (1|femaleID),
               data = female_time_dcr_all_pre,
               family = "bernoulli",
               iter = 11000, warmup = 6000, seed = 123,
               prior = c(prior(student_t(4,0,10),class = Intercept),
               prior(student_t(4,0,10), class = b),
               prior(student_t(4,0,5), class = sd, lb = 0)),
               control=list(adapt_delta = 0.99, max_treedepth = 15),
               backend = "cmdstanr",
               file = "model/mod_dcr2_all_pre")

## 3次まで  
mod_dcr3_all_pre <- brm(confirmed ~ study_period + poly(time, 3, raw = FALSE)
                        + rank_std + age_std +
                  (1|date) + (1|femaleID),
               data = female_time_dcr_all_pre,
               family = "bernoulli",
               iter = 11000, warmup = 6000, seed = 123,
               prior = c(prior(student_t(4,0,10),class = Intercept),
               prior(student_t(4,0,10), class = b),
               prior(student_t(4,0,5), class = sd, lb = 0)),
               control=list(adapt_delta = 0.99, max_treedepth = 15),
               backend = "cmdstanr",
               file = "model/mod_dcr3_all_pre")


## ----fig.height = 4-------------------------------------------------------------------------------------------------------
dharma_dcr2_all_pre <- dh_check_brms(mod_dcr2_all_pre, quantreg = TRUE)

dharma_dcr3_all_pre <- dh_check_brms(mod_dcr3_all_pre, quantreg = TRUE)


## ----fig.height = 5-------------------------------------------------------------------------------------------------------
ppcheck_dcr2_all_pre <- pp_check(mod_dcr2_all_pre)+
  theme_bw()+
  theme(aspect.ratio = 1)+
  guides(color = "none")

ppcheck_dcr3_all_pre <- pp_check(mod_dcr3_all_pre)+
  theme_bw()+
  theme(aspect.ratio = 1)

ppcheck_dcr2_all_pre + ppcheck_dcr3_all_pre


## -------------------------------------------------------------------------------------------------------------------------
check_collinearity(mod_dcr2_all_pre)
check_collinearity(mod_dcr3_all_pre)


## -------------------------------------------------------------------------------------------------------------------------
model_parameters(mod_dcr2_all_pre) %>% 
  data.frame() %>% 
  mutate("95%CI" = str_c("[",sprintf("%.2f",CI_low),",",sprintf("%.2f",CI_high),"]")) %>%
  select(c(1,2,9,7,8))

model_parameters(mod_dcr3_all_pre) %>% 
  data.frame() %>% 
  mutate("95%CI" = str_c("[",sprintf("%.2f",CI_low),",",sprintf("%.2f",CI_high),"]")) %>%
  select(c(1,2,9,7,8))


## ----fig.height = 6-------------------------------------------------------------------------------------------------------
nd_all_pre <- tidyr::crossing(time = seq(0,750,by = 1),
               rank_scaled = c(0,0.5,1),
               age_std = mean(female_time_dcr_all_pre$age_std),
               study_period = c("m19","m20","m21","nm19","nm21","nm22")) %>%
  mutate(rank_std = (rank_scaled - mean(female_time_dcr_all_pre$rank_scaled))/
           sd(female_time_dcr_all_pre$rank_scaled))

# 二次の項までを含むモデル
# fit_dcr2_all_pre <- fitted(mod_dcr2_all_pre,
#                        newdata = nd_all_pre,
#                        scale = "response",
#                        re_formula = NA) %>%
#                 bind_cols(nd_all_pre) %>%
#                 arrange(study_period, rank_scaled, time)
# 
# saveRDS(fit_dcr2_all_pre, "model/fit_dcr2_all_pre.rds")

fit_dcr2_all_pre <- readRDS("model/fit_dcr2_all_pre.rds")


fit_dcr2_all_pre %>%
  mutate(rank_scaled = as.factor(rank_scaled)) %>%
  ggplot(aes(x = time, y = Estimate))+
  geom_line(aes(color = rank_scaled, linetype = rank_scaled),
            linewidth = 1)+
  scale_linetype_manual(values = c("dotted","solid","dashed"))+
  geom_ribbon(aes(ymin = Q2.5, ymax = Q97.5,
                  fill = rank_scaled),
              alpha = 0.1)+
  theme_bw(base_size = 15)+
  theme(aspect.ratio = 1,
        strip.background = element_blank(),
        axis.title = element_text("Times New Roman"),
        axis.text = element_text("Times New Roman"),
        strip.text = element_text(hjust = 0))+
  labs(x = "Time spent", y = "Probability of being confirmed") +
  facet_rep_wrap(~study_period,
                 repeat.tick.labels = TRUE)+
  labs(title = "二次の項まで")  -> p_dcr2_all_pre

p_dcr2_all_pre

# 3次までの項
# fit_dcr3_all_pre <- fitted(mod_dcr3_all_pre,
#            newdata = nd_all_pre,
#            scale = "response",
#            re_formula = NA) %>%
#   bind_cols(nd_all_pre) %>%
#   arrange(study_period, rank_scaled, time)

# saveRDS(fit_dcr3_all_pre, "model/fit_dcr3_all_pre.rds")

fit_dcr3_all_pre <- readRDS("model/fit_dcr3_all_pre.rds")

fit_dcr3_all_pre %>%
  mutate(rank_scaled = as.factor(rank_scaled)) %>%
  ggplot(aes(x = time, y = Estimate))+
  geom_line(aes(color = rank_scaled, linetype = rank_scaled),
            linewidth = 1)+
  scale_linetype_manual(values = c("dotted","solid","dashed"))+
  geom_ribbon(aes(ymin = Q2.5, ymax = Q97.5,
                  fill = rank_scaled),
              alpha = 0.1)+
  theme_bw(base_size = 15)+
  theme(aspect.ratio = 1,
        strip.background = element_blank(),
        axis.title = element_text("Times New Roman"),
        axis.text = element_text("Times New Roman"),
        strip.text = element_text(hjust = 0))+
  labs(x = "Time spent", y = "Probability of being confirmed") +
  facet_rep_wrap(~study_period,
                 repeat.tick.labels = TRUE)+
  labs(title = "三次の項まで")  -> p_dcr3_all_pre

p_dcr3_all_pre

# ggsave("figures/p_dcr2_all_pre.png",
#        p_dcr2_all_pre, width = 220, height = 120, units = "mm",
#        dpi = 600)
# 
# ggsave("figures/p_dcr3_all_pre.png",
#        p_dcr3_all_pre, width = 220, height = 120, units = "mm",
#        dpi = 600)


## ----fig.height = 5-------------------------------------------------------------------------------------------------------
fit_dcr2_all_pre_b <- fit_dcr2_all_pre %>%
  arrange(study_period, rank_scaled, time) %>%
  group_by(study_period, rank_scaled) %>%
  mutate(mean = 1 - cumprod(1-Estimate),
         upper = 1 - cumprod(1-Q97.5),
         lower = 1 - cumprod(1 - Q2.5)) %>%
  ungroup()

fit_dcr3_all_pre_b <- fit_dcr3_all_pre %>%
  arrange(study_period, rank_scaled, time) %>%
  group_by(study_period, rank_scaled) %>%
  mutate(mean = 1 - cumprod(1-Estimate),
         upper = 1 - cumprod(1-Q97.5),
         lower = 1 - cumprod(1 - Q2.5)) %>%
  ungroup()

fit_dcr2_all_pre_b %>%
  mutate(rank_scaled = as.factor(rank_scaled)) %>%
  ggplot(aes(x = time, y = mean))+
  geom_line(aes(color = rank_scaled,
                linetype = rank_scaled),
            linewidth = 1)+
  scale_linetype_manual(values = c("dotted", "solid", "dashed"))+
  geom_ribbon(aes(ymin = lower,
                  ymax = upper,
                  fill = rank_scaled),
              alpha = 0.1)+
  theme_bw(base_size = 16)+
  theme(aspect.ratio = 1,
        strip.background = element_blank(),
        axis.title = element_text(family = "Times New Roman"),
        axis.text = element_text(family = "Times New Roman"),
        strip.text = element_text(family = "Times New Roman",
                                  hjust = 0),
        legend.title = element_text(family = "Times New Roman"),
        legend.text = element_text(family = "Times New Roman"))+
  facet_rep_wrap(~study_period, repeat.tick.labels = TRUE)+
  scale_x_continuous(breaks = seq(0,700, 200))+
  scale_y_continuous(breaks = seq(0,1,0.2))+
  labs(x = "Time elapsed since the troop was found", 
       y = "Cumulative probability of being confirmed",
       color = "Scaled rank", fill = "Scaled rank",
       linetype = "Scaled rank")+
  labs(title = "二次の項まで") +
  guides(linetype = guide_legend(override.aes = list(linewidth = 0.6))) -> p_dcr2_all_pre_cum

p_dcr2_all_pre_cum

fit_dcr3_all_pre_b %>%
  mutate(rank_scaled = as.factor(rank_scaled)) %>%
  ggplot(aes(x = time, y = mean))+
  geom_line(aes(color = rank_scaled,
                linetype = rank_scaled),
            linewidth = 1)+
  scale_linetype_manual(values = c("dotted", "solid", "dashed"))+
  geom_ribbon(aes(ymin = lower,
                  ymax = upper,
                  fill = rank_scaled),
              alpha = 0.1)+
  theme_bw(base_size = 16)+
  theme(aspect.ratio = 1,
        strip.background = element_blank(),
        axis.title = element_text(family = "Times New Roman"),
        axis.text = element_text(family = "Times New Roman"),
        strip.text = element_text(family = "Times New Roman",
                                  hjust = 0))+
  facet_rep_wrap(~study_period, repeat.tick.labels = TRUE)+
  scale_x_continuous(breaks = seq(0,700, 200))+
  scale_y_continuous(breaks = seq(0,1,0.2))+
  labs(x = "Time elapsed since the troop was found", 
       y = "Cumulative probability of being confirmed",
       color = "Scaled rank", fill = "Scaled rank",
       linetype = "Scaled rank")+
  labs(title = "三次の項まで") +
  guides(linetype = guide_legend(override.aes = list(linewidth = 0.6))) -> p_dcr3_all_pre_cum

p_dcr3_all_pre_cum


# ggsave("figures/p_dcr2_all_pre_cum.png", p_dcr2_all_pre_cum, dpi = 600,
#         width = 220, height = 120, units = "mm")
# 
# ggsave("figures/p_dcr3_all_pre_cum.png", p_dcr3_all_pre_cum, dpi = 600,
#          width = 220, height = 120, units = "mm")

