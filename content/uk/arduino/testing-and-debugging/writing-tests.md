---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:29:59.386568-07:00
description: "\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\u0438\
  : Arduino \u043D\u0435 \u043C\u0430\u0454 \u0432\u0431\u0443\u0434\u043E\u0432\u0430\
  \u043D\u043E\u0433\u043E \u0444\u0440\u0435\u0439\u043C\u0432\u043E\u0440\u043A\u0443\
  \ \u0434\u043B\u044F \u0442\u0435\u0441\u0442\u0443\u0432\u0430\u043D\u043D\u044F\
  , \u044F\u043A \u0434\u0435\u044F\u043A\u0456 \u0456\u043D\u0448\u0456 \u043F\u0440\
  \u043E\u0433\u0440\u0430\u043C\u043D\u0456 \u0441\u0435\u0440\u0435\u0434\u043E\u0432\
  \u0438\u0449\u0430. \u041E\u0434\u043D\u0430\u043A, \u0432\u0438 \u043C\u043E\u0436\
  \u0435\u0442\u0435 \u0432\u0438\u043A\u043E\u0440\u0438\u0441\u0442\u043E\u0432\u0443\
  \u0432\u0430\u0442\u0438 \u0441\u0442\u043E\u0440\u043E\u043D\u043D\u0456\u2026"
lastmod: '2024-03-13T22:44:49.723974-06:00'
model: gpt-4-0125-preview
summary: "Arduino \u043D\u0435 \u043C\u0430\u0454 \u0432\u0431\u0443\u0434\u043E\u0432\
  \u0430\u043D\u043E\u0433\u043E \u0444\u0440\u0435\u0439\u043C\u0432\u043E\u0440\u043A\
  \u0443 \u0434\u043B\u044F \u0442\u0435\u0441\u0442\u0443\u0432\u0430\u043D\u043D\
  \u044F, \u044F\u043A \u0434\u0435\u044F\u043A\u0456 \u0456\u043D\u0448\u0456 \u043F\
  \u0440\u043E\u0433\u0440\u0430\u043C\u043D\u0456 \u0441\u0435\u0440\u0435\u0434\u043E\
  \u0432\u0438\u0449\u0430."
title: "\u041F\u0438\u0441\u044C\u043C\u043E \u0442\u0435\u0441\u0442\u0456\u0432"
weight: 36
---

## Як це зробити:
Arduino не має вбудованого фреймворку для тестування, як деякі інші програмні середовища. Однак, ви можете використовувати сторонні бібліотеки, такі як `AUnit` для модульного тестування коду Arduino. AUnit надихнутий вбудованою бібліотекою Arduino, `ArduinoUnit`, та тестувальним фреймворком Google, `Google Test`.

### Приклад з AUnit:
Спершу встановіть AUnit через менеджер бібліотек в Arduino IDE: перейдіть в Sketch > Include Library > Manage Libraries... > шукайте AUnit і встановіть її.

Потім ви можете написати тести так:

```cpp
#include <AUnit.h>

test(ledPinHigh) {
  const int ledPin = 13;
  pinMode(ledPin, OUTPUT);
  digitalWrite(ledPin, HIGH);
  assertTrue(digitalRead(ledPin));
}

test(ledPinLow) {
  const int ledPin = 13;
  pinMode(ledPin, OUTPUT);
  digitalWrite(ledPin, LOW);
  assertFalse(digitalRead(ledPin));
}

void setup() {
  Serial.begin(9600);
  aunit::TestRunner::run();
}

void loop() {
  // Пусто
}
```
Після завантаження цього тесту на вашу плату Arduino, відкрийте монітор серійного порту, щоб переглянути результати тестування. Ви повинні побачити вивід, який вказує, чи пройшов кожен тест чи ні:

```
TestRunner started on 2 test(s).
Test ledPinHigh passed.
Test ledPinLow passed.
TestRunner duration: 0.002 seconds.
TestRunner summary: 2 passed, 0 failed, 0 skipped, 0 timed out, out of 2 test(s).
```

Цей простий приклад демонструє використання AUnit для тестування стану світлодіодного виводу. Створюючи тести, ви підтверджуєте, що ваш Arduino поводить себе як очікується в різних умовах. З AUnit ви можете писати більш складні тести, набори тестів та користуватися функціями, такими як таймаути тестів і процедури налаштування/знесення для більш складних сценаріїв.
