---
date: 2024-01-20 17:30:56.027160-07:00
description: "\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\u0438\
  : \u041E\u0431\u0447\u0438\u0441\u043B\u0435\u043D\u043D\u044F \u0434\u0430\u0442\
  \u0438 \u0432 \u043C\u0438\u043D\u0443\u043B\u043E\u043C\u0443 \u0447\u0438 \u043C\
  \u0430\u0439\u0431\u0443\u0442\u043D\u044C\u043E\u043C\u0443 \u043A\u043E\u0440\u0456\
  \u043D\u0438\u0442\u044C\u0441\u044F \u0432 \u043F\u043E\u0442\u0440\u0435\u0431\
  \u0456 \u0432\u0456\u0434\u0441\u043B\u0456\u0434\u043A\u043E\u0432\u0443\u0432\u0430\
  \u0442\u0438 \u043F\u043E\u0434\u0456\u0457 \u0442\u0430 \u043F\u043B\u0430\u043D\
  \u0443\u0432\u0430\u0442\u0438 \u043D\u0430\u043F\u0435\u0440\u0435\u0434. \u0423\
  \ \u0432\u0431\u0443\u0434\u043E\u0432\u0430\u043D\u043E\u043C\u0443 \u043F\u0440\
  \u043E\u0433\u0440\u0430\u043C\u0443\u0432\u0430\u043D\u043D\u0456, \u044F\u043A\
  \u2026"
lastmod: '2024-04-05T22:51:02.741175-06:00'
model: gpt-4-1106-preview
summary: "\u041E\u0431\u0447\u0438\u0441\u043B\u0435\u043D\u043D\u044F \u0434\u0430\
  \u0442\u0438 \u0432 \u043C\u0438\u043D\u0443\u043B\u043E\u043C\u0443 \u0447\u0438\
  \ \u043C\u0430\u0439\u0431\u0443\u0442\u043D\u044C\u043E\u043C\u0443 \u043A\u043E\
  \u0440\u0456\u043D\u0438\u0442\u044C\u0441\u044F \u0432 \u043F\u043E\u0442\u0440\
  \u0435\u0431\u0456 \u0432\u0456\u0434\u0441\u043B\u0456\u0434\u043A\u043E\u0432\u0443\
  \u0432\u0430\u0442\u0438 \u043F\u043E\u0434\u0456\u0457 \u0442\u0430 \u043F\u043B\
  \u0430\u043D\u0443\u0432\u0430\u0442\u0438 \u043D\u0430\u043F\u0435\u0440\u0435\u0434\
  ."
title: "\u041E\u0431\u0447\u0438\u0441\u043B\u0435\u043D\u043D\u044F \u0434\u0430\u0442\
  \u0438 \u0443 \u043C\u0430\u0439\u0431\u0443\u0442\u043D\u044C\u043E\u043C\u0443\
  \ \u0430\u0431\u043E \u043C\u0438\u043D\u0443\u043B\u043E\u043C\u0443"
weight: 26
---

## Як це зробити:
```Arduino
#include <Wire.h>
#include <RTClib.h>

RTC_DS1307 rtc; // Ініціалізація RTC модуля

void setup() {
  Serial.begin(9600);
  if (!rtc.begin()) {
    Serial.println("Couldn't find RTC");
    while (1);
  }

  if (!rtc.isrunning()) {
    Serial.println("RTC is NOT running!");
    rtc.adjust(DateTime(F(__DATE__), F(__TIME__))); // Сетап часу і дати
  }
}

void loop() {
  DateTime now = rtc.now(); // Отримання поточної дати і часу

  Serial.print("Now: ");
  printDateTime(now);

  DateTime futureDate = now + TimeSpan(30,0,0,0); // Додавання 30 днів
  Serial.print("Future: ");
  printDateTime(futureDate);

  delay(10000);
}

void printDateTime(const DateTime& dt) {
  Serial.print(dt.year(), DEC);
  Serial.print('/');
  Serial.print(dt.month(), DEC);
  Serial.print('/');
  Serial.print(dt.day(), DEC);
  Serial.print(" ");
  Serial.print(dt.hour(), DEC);
  Serial.print(':');
  Serial.print(dt.minute(), DEC);
  Serial.print(':');
  Serial.print(dt.second(), DEC);
  Serial.println();
}
```
Вихідні дані:
```
Now: 2023/3/15 10:23:30
Future: 2023/4/14 10:23:30
```

## Поглиблений огляд:
Обчислення дати в минулому чи майбутньому коріниться в потребі відслідковувати події та планувати наперед. У вбудованому програмуванні, як на Arduino, використовується модуль реального часу (RTC) для точного відслідковування часу. Існують альтернативи, як-от використання інтернет-сервісів часу або внутрішніх таймерів, але RTC дає високу точність та незалежність від зовнішніх з'єднань. Обчислення дати реалізується через вирахування або додавання часових проміжків до поточного моменту.

## Додатково:
- Документація по RTClib: https://github.com/adafruit/RTClib.
- Про модулі реального часу: http://playground.arduino.cc/Main/DS1307RTC.
- Курс "Робота з часом на Arduino": https://learn.adafruit.com/ds1307-real-time-clock-breakout-board-kit/overview.
