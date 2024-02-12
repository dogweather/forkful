---
title:                "Письмо тестів"
aliases: - /uk/arduino/writing-tests.md
date:                  2024-02-03T19:29:59.386568-07:00
model:                 gpt-4-0125-preview
simple_title:         "Письмо тестів"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/arduino/writing-tests.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Що та Чому?

Написання тестів в середовищі Arduino означає процес створення автоматизованих тестів, які перевіряють функціональність вашого коду на пристроях Arduino. Програмісти роблять це, щоб переконатися, що їх код працює як очікується, зменшує кількість помилок та покращує якість їх проєктів, що особливо важливо в системах вбудованих, де пошук помилок може бути складнішим.

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
