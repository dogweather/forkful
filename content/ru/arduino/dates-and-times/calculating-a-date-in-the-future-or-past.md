---
title:                "Расчет даты в будущем или прошлом"
aliases:
- ru/arduino/calculating-a-date-in-the-future-or-past.md
date:                  2024-01-28T23:55:53.955790-07:00
model:                 gpt-4-0125-preview
simple_title:         "Расчет даты в будущем или прошлом"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/arduino/calculating-a-date-in-the-future-or-past.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и почему?
Вычисление даты в будущем или в прошлом включает определение конкретного дня до или после заданной даты. Программисты делают это для функций, таких как планирование событий, напоминаний или расчет сроков.

## Как:

Arduino не имеет встроенных функций даты и времени, но вы можете использовать библиотеку "TimeLib.h" для обработки расчетов даты. Убедитесь, что вы установили библиотеку перед использованием приведенных ниже примеров.

```Arduino
#include <TimeLib.h>

void setup() {
  Serial.begin(9600);
  setTime(10, 0, 0, 25, 3, 2023); // Установить время на 25 марта 2023 года, 10:00:00
}

void loop() {
  // Рассчитать 10 дней в будущем
  time_t futureTime = now() + 10 * SECS_PER_DAY;
  
  // Вывести будущую дату
  Serial.print(day(futureTime));
  Serial.print("/");
  Serial.print(month(futureTime));
  Serial.print("/");
  Serial.println(year(futureTime));

  // Рассчитать 10 дней в прошлом
  time_t pastTime = now() - 10 * SECS_PER_DAY;
  
  // Вывести прошлую дату
  Serial.print(day(pastTime));
  Serial.print("/");
  Serial.print(month(pastTime));
  Serial.print("/");
  Serial.println(year(pastTime));

  // Избежать постоянной печати
  delay(10000);
}
```
Пример вывода:
```
4/4/2023
15/3/2023
```

## Подробнее

До появления модулей реального времени (RTC) и библиотек, таких как TimeLib, учет времени на Arduino был первобытным и обычно реализовывался вручную. Существуют различные способы вычисления будущих или прошлых дат, но использование специализированной библиотеки, как TimeLib, значительно упрощает процесс.

Альтернативы TimeLib включают более комплексную "RTClib.h" для использования с аппаратными RTC, или встроенную функцию `millis()` для более коротких временных интервалов (с ручным управлением датами). TimeLib обрабатывает високосные годы и часовые пояса, а также предоставляет утилиты для легкой манипуляции с датами.

При вычислении будущих или прошлых дат обратите внимание на часовые пояса и изменения связанные с переходом на летнее время, если вы работаете с реальными часами или внешними источниками времени. На Arduino, без RTC или подключения к Интернету, обычно время устанавливается вручную или через внешний сигнал (например, GPS или радиосигналы времени).

## Смотри также

- Документация библиотеки времени:
  https://www.arduino.cc/reference/en/libraries/time/
- RTClib, популярная библиотека для работы с реальным временем часов:
  https://github.com/adafruit/RTClib
- Функция millis() на Arduino и ее применения:
  https://www.arduino.cc/reference/en/language/functions/time/millis/
