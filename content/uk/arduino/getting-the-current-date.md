---
title:                "Отримання поточної дати"
date:                  2024-01-20T15:13:30.132449-07:00
html_title:           "Bash: Отримання поточної дати"
simple_title:         "Отримання поточної дати"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/arduino/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Що & Навіщо?
Отримання поточної дати – це процес зчитування реального часу з годинника. Програмісти роблять це для логування подій, ажіотажу за часом чи визначення періодів активності у програмах.

## Як це зробити:
В Arduino немає вбудованого годинника реального часу (RTC), тому для отримання поточної дати підключіть RTC модуль типу DS3231. Ось як код може виглядати:

```Arduino
#include <Wire.h>
#include <RTClib.h>

RTC_DS3231 rtc;

void setup() {
  Serial.begin(9600);
  if (!rtc.begin()) {
    Serial.println("Couldn't find RTC");
    while (1);
  }
  if (rtc.lostPower()) {
    Serial.println("RTC lost power, let's set the time!");
    // Наступний рядок налаштовує час на 9 березня 2023 10:33:00
    rtc.adjust(DateTime(2023, 3, 9, 10, 33, 0));
  }
}

void loop() {
  DateTime now = rtc.now();
  Serial.print(now.year(), DEC);
  Serial.print('/');
  Serial.print(now.month(), DEC);
  Serial.print('/');
  Serial.println(now.day(), DEC);
  delay(1000);
}
```
Зразок виводу:
```
2023/3/9
```

## Поглиблений розгляд:
RTC модулі, як DS3231, використовуються для точного тримання часу. Інші модулі, такі як DS1307, також популярні, але менш точні. Чіп DS3231 має термокомпенсований кварцовий годинник, забезпечуючи високу точність.

Встановлення часу на RTC за допомогою `rtc.adjust()` зазвичай робиться один раз. Потім модуль підтримує відрахування часу самостійно, навіть якщо відключити живлення Arduino.

Програмуючи, завжди перевіряйте `rtc.lostPower()` для ідентифікації втрати живлення, що може призвести до скидання часу.

## Дивіться також:
1. Бібліотека RTClib – https://github.com/adafruit/RTClib
2. Огляд модуля DS3231 – https://lastminuteengineers.com/ds3231-rtc-arduino-tutorial/
3. Аналогічна бібліотека для DS1307 – https://www.arduino.cc/en/Reference/DS1307RTC