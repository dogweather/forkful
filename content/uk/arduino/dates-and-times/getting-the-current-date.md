---
title:                "Отримання поточної дати"
date:                  2024-02-03T19:09:08.865725-07:00
model:                 gpt-4-0125-preview
simple_title:         "Отримання поточної дати"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/arduino/getting-the-current-date.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Що і чому?
Отримання поточної дати в проектах Arduino передбачає отримання інформації в реальному часі, яка може бути вирішальною для ведення журналів, маркування часом або планування завдань. Програмістам часто потрібна ця можливість для підвищення функціональності, забезпечення актуальності даних та спрощення операцій з часом у їх IoT та вбудованих проектах.

## Як:
Сам Arduino не має вбудованого методу для безпосереднього отримання поточної дати, оскільки він не має годинника реального часу (RTC). Однак, це можна здійснити за допомогою зовнішніх модулів RTC, таких як DS3231, та бібліотек, наприклад, `RTClib`, розробленої Adafruit, яка спрощує взаємодію з цими модулями.

Спочатку переконайтеся, що бібліотека `RTClib` встановлена у вашу Arduino IDE. Потім підключіть ваш модуль RTC до Arduino відповідно до його документації.

Ось простий приклад для початку:

```cpp
#include <Wire.h>
#include "RTClib.h"

RTC_DS3231 rtc;

void setup() {
  Serial.begin(9600);

  if (!rtc.begin()) {
    Serial.println("Модуль RTC не знайдено");
    while (1);
  }

  if (rtc.lostPower()) {
    Serial.println("RTC втратив живлення, давайте встановимо час!");
    // Коли час потрібно встановити на новому пристрої або після втрати живлення, ви можете налаштувати його тут.
    // rtc.adjust(DateTime(F(__DATE__), F(__TIME__)));
  }
}

void loop() {
  DateTime now = rtc.now();

  Serial.print("Поточна дата: ");
  Serial.print(now.year(), DEC);
  Serial.print('/');
  Serial.print(now.month(), DEC);
  Serial.print('/');
  Serial.println(now.day(), DEC);

  delay(3000); // Затримка на 3 секунди для зменшення кількості повідомлень
}
```

Прикладний вивід (за умови, що ваш RTC було попередньо налаштовано):

```
Поточна дата: 2023/4/15
```

Цей код ініціалізує модуль RTC, а потім, у циклі, отримує та виводить поточну дату на серійний монітор кожні 3 секунди. Пам'ятайте, що рядок `rtc.adjust(...)` можна розкоментувати та змінити для початкового налаштування дати та часу RTC або після того, як він втратив живлення.