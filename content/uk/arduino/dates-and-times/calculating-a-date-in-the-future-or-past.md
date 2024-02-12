---
title:                "Обчислення дати у майбутньому або минулому"
aliases:
- /uk/arduino/calculating-a-date-in-the-future-or-past/
date:                  2024-01-20T17:30:56.027160-07:00
model:                 gpt-4-1106-preview
simple_title:         "Обчислення дати у майбутньому або минулому"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/arduino/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Що і для чого?
Обчислення майбутніх чи минулих дат – це визначення точної дати до чи після заданого періоду часу. Програмісти роблять це для задач із плануванням, відслідковуванням часу чи створенням розкладів.

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
