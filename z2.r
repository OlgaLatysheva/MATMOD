#Латышева Ольга - создайте модель множественной линейной регрессии ночных потоков углекислого газа за период 2013 года по данным измерений методом турбулентной пульсации
setwd()
getwd()

library("tidyverse") 
library("stringr") 
library("dplyr") 
library("ggplot2") 
library("readr")

#Считываем данные из файла; Пропускаем первую строчку,заменяем текстовые NA, заменяем все не числовые значения на NA и ингорим строчки с "["
eddypro = read.csv("eddypro.csv", skip =  1, na= c("","NA" , "-9999" , "-9999.0" ), comment= c("["))
#пропускаем перрвую строчку
eddypro = eddypro[- 1,]
#удаляем ненужный пустой столбец
eddypro = select(eddypro,-(roll))

#Преобразуем строковые значения в факторные
eddypro = eddypro %>% mutate_if(is.character, factor)

#Заменяем символы в названии столбцов на допустимые для переменных названия
names(eddypro) = names(eddypro) %>% 
  str_replace_all("[!]", "_exclam_") %>% 
  str_replace_all("[?]", "_quest_") %>% 
  str_replace_all("[*]", "_star_") %>% 
  str_replace_all("[+]", "_plus_") %>%
  str_replace_all("[-]", "_minus_") %>%
  str_replace_all("[@]", "_at_") %>%
  str_replace_all("[$]", "_dollar_") %>%
  str_replace_all("[#]", "_hash_") %>%
  str_replace_all("[/]", "_slash_") %>%
  str_replace_all("[%]", "__pecent_") %>%
  str_replace_all("[&]", "_amp_") %>%
  str_replace_all("[\\^]", "_power_") %>%
  str_replace_all("[()]", "_")

#Превращаем столбцы таблицы в виде векторов для проверки
glimpse(eddypro)

#Убераем NA
eddypro = drop_na(eddypro)

#Отфильтруем данные для ночного периода
eddypro = filter(eddypro, daytime==F)

#Получение таблицы состоящей из цифр
eddypro_num = eddypro[,sapply(eddypro,is.numeric)]
#Получение таблицы с остальными колонками
eddypro_n_num = eddypro[,!sapply(eddypro,is.numeric)]

#Модель регрессивного анализа

#Создание непересекающихся выборок
row_num = 1:length(eddypro_num$co2_flux)
teach = sample(row_num, floor(length(eddypro_num$co2_flux)*.7))
test = row_num[-teach]
#Обучающая выборка
teaching_tbl = eddypro_num[teach,]
#Тестирующая выборка
testing_tbl = eddypro_num[test,]

#1 модель по обучающей выборке
mod1 = lm(co2_flux~ (.), data =  teaching_tbl)

#Информация о модели
summary(mod1)
#Коэффициенты модели
coef(mod1)
#Дисперсионный анализ модели
anova(mod1)
#Графическое представление модели:
plot(mod1)

#2 модель 
mod2 = lm ( co2_flux~  DOY + file_records + Tau + qc_Tau + rand_err_Tau + H + qc_H + rand_err_H + LE + qc_LE + qc_co2_flux 
            + rand_err_co2_flux + h2o_flux +rand_err_h2o_flux + H_strg +co2_v.adv + co2_molar_density + co2_mole_fraction + co2_mixing_ratio
            + h2o_molar_density + h2o_mole_fraction + h2o_mixing_ratio + h2o_time_lag + sonic_temperature + air_temperature + air_pressure 
            + air_density + air_heat_capacity + air_molar_volume + water_vapor_density + e + es + specific_humidity + RH + VPD + Tdew
            + u_unrot + w_rot + max_speed + wind_dir + yaw + pitch + u. +  TKE +  L +  X.z.d..L + bowen_ratio + T. + x_peak + x_offset 
            + x_10. + x_30. + x_50. +x_70. + x_90. + un_Tau + Tau_scf + un_H + H_scf + un_LE + LE_scf +un_co2_flux + un_h2o_flux + v_spikes 
            + w_spikes + mean_value + ts_var + co2_var + w.co2_cov + w.h2o_cov + co2 + h2o.1 + h2o_signal_strength_7200 + flowrate , data = teaching_tbl)

#Информация о модели
summary(mod2)
#Коэффициент модели
coef(mod2)
#Остатки модели
resid(mod2)
#Доверительный интервал
confint(mod2)
#Дисперсионный анализ
anova(mod2)
#Сравнение двух моделей
anova(mod2, mod1)
#Графическое представление 2 модели
plot(mod2) 

#3 модель

mod3 = lm ( co2_flux~ DOY + Tau + qc_Tau + rand_err_Tau + H + qc_H + rand_err_H + LE + qc_LE + qc_co2_flux 
            + rand_err_co2_flux + h2o_flux +rand_err_h2o_flux + H_strg +co2_v.adv + co2_molar_density + co2_mole_fraction + co2_mixing_ratio
            + h2o_molar_density + h2o_mole_fraction + h2o_mixing_ratio + h2o_time_lag + sonic_temperature + air_temperature + air_pressure 
            + air_density + air_heat_capacity + air_molar_volume + water_vapor_density + e + es + specific_humidity + RH + VPD + Tdew
            + u_unrot + w_rot + wind_dir + yaw + pitch + u. + TKE + X.z.d..L + bowen_ratio + T. + x_peak + x_offset 
            + x_10. + x_30. + x_50. +x_70. + x_90. + un_Tau + Tau_scf + un_H + H_scf + un_LE + LE_scf +un_co2_flux + un_h2o_flux
            + w_spikes + mean_value + co2_var + w.co2_cov + w.h2o_cov + co2 ,data = teaching_tbl)


#Информация о модели
summary(mod3)
#Коэффициент модели
coef(mod3)
#Остатки модели
resid(mod3)
#Доверительный интервал
confint(mod3)
#Дисперсионный анализ
anova(mod3)
#Сравнение двух моделей
anova(mod3, mod2)
#Графическое представление 3 модели
plot(mod3) 

#Корреляционный анализ переменных участвующих в линейной модели
cor_teaching_tbl = select(teaching_tbl, Tau, H, DOY, LE, qc_LE, h2o_flux, H_strg, h2o_mole_fraction, h2o_mixing_ratio, specific_humidity, Tdew, u_unrot, w_rot, u., X.z.d..L, T., x_peak, x_offset, x_10., x_70., x_90., un_Tau, Tau_scf, un_H, H_scf, un_LE, LE_scf, un_co2_flux, un_h2o_flux, mean_value, co2_var, w.co2_cov, w.h2o_cov, co2)

#Получение таблицы коэффициентов корреляций
cor_td = cor(cor_teaching_tbl) %>% as.data.frame


#Построение графиков по полученной модели
#Построение точек по значениями обучающей выборки и наложение предсказанных значений по 3 модели
qplot(co2_flux , co2_flux, data = teaching_tbl) + geom_line(aes(y = predict(mod3, teaching_tbl)))
#Построение точек по значением тестирующей выборки и наложение предсказанных значений по 3 модели
qplot(co2_flux , co2_flux, data = testing_tbl) + geom_line(aes(y = predict(mod3, testing_tbl)))

#Примеры
qplot(Tau, co2_flux, data = testing_tbl) + geom_line(aes(y = predict(mod3, testing_tbl)))
qplot(H, co2_flux, data = testing_tbl) + geom_line(aes(y = predict(mod3, testing_tbl)))
qplot(LE, co2_flux, data = testing_tbl) + geom_line(aes(y = predict(mod3, testing_tbl)))