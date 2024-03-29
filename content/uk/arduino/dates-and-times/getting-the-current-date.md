---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:09:08.865725-07:00
description: "\u041E\u0442\u0440\u0438\u043C\u0430\u043D\u043D\u044F \u043F\u043E\u0442\
  \u043E\u0447\u043D\u043E\u0457 \u0434\u0430\u0442\u0438 \u0432 \u043F\u0440\u043E\
  \u0435\u043A\u0442\u0430\u0445 Arduino \u043F\u0435\u0440\u0435\u0434\u0431\u0430\
  \u0447\u0430\u0454 \u043E\u0442\u0440\u0438\u043C\u0430\u043D\u043D\u044F \u0456\
  \u043D\u0444\u043E\u0440\u043C\u0430\u0446\u0456\u0457 \u0432 \u0440\u0435\u0430\
  \u043B\u044C\u043D\u043E\u043C\u0443 \u0447\u0430\u0441\u0456, \u044F\u043A\u0430\
  \ \u043C\u043E\u0436\u0435 \u0431\u0443\u0442\u0438 \u0432\u0438\u0440\u0456\u0448\
  \u0430\u043B\u044C\u043D\u043E\u044E \u0434\u043B\u044F \u0432\u0435\u0434\u0435\
  \u043D\u043D\u044F \u0436\u0443\u0440\u043D\u0430\u043B\u0456\u0432, \u043C\u0430\
  \u0440\u043A\u0443\u0432\u0430\u043D\u043D\u044F\u2026"
lastmod: '2024-03-13T22:44:49.735948-06:00'
model: gpt-4-0125-preview
summary: "\u041E\u0442\u0440\u0438\u043C\u0430\u043D\u043D\u044F \u043F\u043E\u0442\
  \u043E\u0447\u043D\u043E\u0457 \u0434\u0430\u0442\u0438 \u0432 \u043F\u0440\u043E\
  \u0435\u043A\u0442\u0430\u0445 Arduino \u043F\u0435\u0440\u0435\u0434\u0431\u0430\
  \u0447\u0430\u0454 \u043E\u0442\u0440\u0438\u043C\u0430\u043D\u043D\u044F \u0456\
  \u043D\u0444\u043E\u0440\u043C\u0430\u0446\u0456\u0457 \u0432 \u0440\u0435\u0430\
  \u043B\u044C\u043D\u043E\u043C\u0443 \u0447\u0430\u0441\u0456, \u044F\u043A\u0430\
  \ \u043C\u043E\u0436\u0435 \u0431\u0443\u0442\u0438 \u0432\u0438\u0440\u0456\u0448\
  \u0430\u043B\u044C\u043D\u043E\u044E \u0434\u043B\u044F \u0432\u0435\u0434\u0435\
  \u043D\u043D\u044F \u0436\u0443\u0440\u043D\u0430\u043B\u0456\u0432, \u043C\u0430\
  \u0440\u043A\u0443\u0432\u0430\u043D\u043D\u044F\u2026"
title: "\u041E\u0442\u0440\u0438\u043C\u0430\u043D\u043D\u044F \u043F\u043E\u0442\u043E\
  \u0447\u043D\u043E\u0457 \u0434\u0430\u0442\u0438"
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
