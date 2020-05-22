# Латышева Ольга – для региона 74 рассчитайте урожайность пшеницы в 2014 году, взяв для рассчета средние суммы активных температур за текущий год, с 20 ближайших метеостанций но рассчитав колонку di самостоятельно, как долю месяца, когда среднедневные температуры были выше 7 градусов, но учитывая, что посев не может начаться раньше середины апреля, а вегетация составляет 4 месяца

library(tidyverse)
library(rnoaa)
library(lubridate)

# Создадим векторы с данными для расчета:
ai = c(0.00,0.00,0.00,32.11, 26.31,25.64,23.20,18.73,16.30,13.83,0.00,0.00)
bi = c(0.00, 0.00, 0.00, 11.30, 9.26, 9.03,8.16, 6.59, 5.73, 4.87, 0.00, 0.00)
Kf = 300 # Коэффициент использования ФАР
Qj = 1600 # калорийность урожая культуры
Lj = 2.2 # сумма частей основной и побочной продукции
Ej = 25 # стандартная влажность культуры
y=1 #коэффициент для экпозиции склона - считаем что все поля идеально ровные

#Скачивание  списка метеостанций
station_data = ghcnd_stations() 
station_data = read.csv("station_data.csv")

#Формируем список метеостанций
#После получения списка всех станций, выбираем из него список 20 станций ближайших к 
#столице региона (Челябинск),создав таблицу с именем региона и координатами его столицы
chely = data.frame(id = "CHELYABINSK", latitude =  55.160283, longitude =  61.400856)
#Выбираем конечное число станций, которые имеют необходимые данные
#в заданный временной период, и выбрать переменные, которые обязательно должны быть в наличии
chely_around = meteo_nearby_stations(lat_lon_df = chely, station_data = station_data,
                                     limit = 20, var = c("PRCP", "TAVG"),
                                     year_min = 2014, year_max = 2014)
#chely_around это список единственным элементом которого является таблица, содержащая идентификаторы метеостанций отсортированных по их 
#удалленности от Челябинска.

#Получение индентификатора метеостанций Челябинска
chely_id = chely_around[["CHELYABINSK"]][["id"]][1]
chely_id
#Создаем таблицу всех метеостанций Челябинска, выбрав целиком первый объект из списка 
chely_table = chely_around[[1]]
summary (chely_table)
all_i = data.frame()
all_chely_meteodata = data.frame()
#Цикл для всех метеостанций
for(i in 1:20)
{
  print(i)
  print(chely_id)
  #Выберем нужные свойства 
  all_i = meteo_tidy_ghcnd(stationid = chely_table$id [i])
  all_i=all_i[,c("id","date","tavg")]
  #Соединяем данные, полученные на предыдущих и данном этапах цикла
  all_chely_meteodata=rbind (all_chely_meteodata, all_i)
}

#Создаем строки год, месяц, день в таблице
all_chely_meteodata=mutate(all_chely_meteodata, year=year(date),month=month(date),day=day(date))

#Отфильтруем данные за 2014 год
all_data=filter(all_chely_meteodata, year %in% 2014)


#Вычислим di для каждого месяца
#Произведем обработку полученных данных
#обнулим значение температуры в невегетативный период
#обнулим значение температуры в невегетационный период
all_data[(all_data$month < 4),"tavg"] = 0
all_data[(all_data$month > 8),"tavg"] = 0
all_data[(all_data$month == 4 & all_data$day < 15),"tavg"] = 0
all_data[(all_data$month == 8 & all_data$day > 15),"tavg"] = 0
#Сгруппируем по годам и месяцам 
all_data = all_data %>% group_by(month)
#Вычислим di для каждого месяца
di1 = summarise(all_data, di = length(tavg[tavg>70])/length(tavg))[-12,][-2,][-1,]
di=di1[[2]]
all_data2 = all_data
all_chely = all_data2 %>% 
  #Сгруппируем с учетом id метеостанций
  group_by(month, id) %>% 
  #Выберем активные температуры (более 5 градусов Цельсия)
  mutate(tavg=tavg/10) %>% filter (tavg>5)%>%
  #Сгруппируем по годам и месяцам 
  summarise(sum = sum (tavg, na.rm = TRUE)) %>%
  group_by(month) %>%
  summarise(S = mean(sum,na.rm = TRUE))

Y = ((ai + bi * y * all_chely$S * di) * Kf) / (Qj * Lj * (100 - Ej))
#Вычислим суммарную урожайность
Yield = sum(Y); Yield
#Урожайность для Челябинской области в 2014 году составила 9.302985 ц/га