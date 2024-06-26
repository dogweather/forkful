---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 23:59:47.948309-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: \u041F\u0440\u0435\u043E\u0431\u0440\u0430\u0437\u0443\u0435\u043C\
  \ \u0441\u0442\u0440\u043E\u043A\u0443 \u0432 \u0434\u0430\u0442\u0443."
lastmod: '2024-03-13T22:44:45.548074-06:00'
model: gpt-4-0125-preview
summary: "\u041F\u0440\u0435\u043E\u0431\u0440\u0430\u0437\u0443\u0435\u043C \u0441\
  \u0442\u0440\u043E\u043A\u0443 \u0432 \u0434\u0430\u0442\u0443."
title: "\u0410\u043D\u0430\u043B\u0438\u0437 \u0434\u0430\u0442\u044B \u0438\u0437\
  \ \u0441\u0442\u0440\u043E\u043A\u0438"
weight: 30
---

## Как это сделать:
Преобразуем строку в дату:

```Arduino
#include <Wire.h>
#include <RTClib.h>

RTC_DS1307 rtc;

void setup() {
  Serial.begin(9600);
  if (!rtc.begin()) {
    Serial.println("Не найден RTC");
    while (1);
  }
  
  // Предположим, что строка даты имеет формат "ДД/ММ/ГГГГ"
  String dateString = "24/12/2023"; 
  
  int day = dateString.substring(0, 2).toInt();
  int month = dateString.substring(3, 5).toInt();
  int year = dateString.substring(6).toInt();
  
  rtc.adjust(DateTime(year, month, day));
  
  Serial.print("Дата установлена на: ");
  Serial.print(day);
  Serial.print("/");
  Serial.print(month);
  Serial.print("/");
  Serial.println(year);
}

void loop() {
  // Здесь ничего не делаем
}
```

Пример вывода:
```
Дата установлена на: 24/12/2023
```

## Подробнее
Разбор дат является обычной задачей с самых ранних дней программирования. Исторически обработка дат была специфичной для платформы и подвержена ошибкам. Arduino с его многочисленными библиотеками, такими как RTClib, значительно упрощает этот процесс.

Альтернативы RTClib для разбора дат включают использование встроенных функций или написание пользовательского кода для проверки и преобразования строк с датами. Детали реализации, такие как проверка високосных лет или работа с различными форматами дат, могут усложнить разбор. Обеспечение ввода строк в ожидаемых форматах и проверка ошибок разобранных значений критически важны для избежания сбоев.

## Смотрите также
- RTClib на GitHub: https://github.com/adafruit/RTClib
- Библиотека времени Arduino: https://www.arduino.cc/reference/en/libraries/time/
- Справочник по классу DateTime Arduino: https://github.com/adafruit/RTClib/blob/master/DateTime.h
