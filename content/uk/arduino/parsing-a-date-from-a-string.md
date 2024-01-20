---
title:                "Аналіз дати з рядка"
date:                  2024-01-20T15:34:30.318791-07:00
html_title:           "Arduino: Аналіз дати з рядка"
simple_title:         "Аналіз дати з рядка"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/arduino/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Що і Чому?
Парсинг дати з рядка – це процес видобування інформації про дату з текстового формату. Програмісти це роблять для обробки і використання дат у програмах, як день народження або термін придатності.

## Як це зробити:
```Arduino
#include <Wire.h>
#include <RTClib.h>

RTC_DS3231 rtc; // Ініціалізація об’єкту реального часу

void setup() {
  Serial.begin(9600);
  if (!rtc.begin()) {
    Serial.println("Couldn't find RTC");
    while (1);
  }

  if (rtc.lostPower()) {
    Serial.println("RTC lost power, let's set the time!");
    // Наступний рядок налаштовує дату і час при втраті живлення
    rtc.adjust(DateTime(F(__DATE__), F(__TIME__)));
  }
}

void loop() {
  DateTime now = rtc.now(); // Отримання поточного часу і дати

  // Форматування дати в рядок
  char dateStr[11];
  sprintf(dateStr, "%02u/%02u/%04u", now.day(), now.month(), now.year());
  
  Serial.println(dateStr); // Виведення рядка з датою
  delay(1000); // Затримка перед наступним виведенням
}
```
Прикладний вивід: `20/04/2023`

## Поглиблення:
Раніше для парсингу дати були менш зручні бібліотеки або програмісти писали власні функції. Сьогодні ми використовуємо бібліотеку `RTClib`, яка спрощує взаємодію з RTC модулями, такими як DS3231. Парсинг залежить від формату дати, що використовується. У нашому прикладі ми показали `dd/mm/yyyy`. Існують інші формати, як `ISO 8601` (yyyy-mm-dd). Обираєте, що краще для вашого проекту.

## Додатково:
- Документація по бібліотеці RTClib: https://github.com/adafruit/RTClib
- Все про RTC модуль DS3231: https://datasheets.maximintegrated.com/en/ds/DS3231.pdf
- Arduino Time Library для часових функцій: https://www.arduino.cc/en/Reference/Time