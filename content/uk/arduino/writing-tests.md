---
title:                "Написання тестів"
date:                  2024-01-19
html_title:           "Arduino: Написання тестів"
simple_title:         "Написання тестів"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/arduino/writing-tests.md"
---

{{< edit_this_page >}}

## Що це таке & Навіщо?
Тестування коду - перевірка правильності роботи програми шляхом автоматичного запуску заздалегідь написаних тестів. Програмісти тестують код, щоб забезпечити його надійність та виявити помилки до того, як програма потрапить до кінцевого користувача.

## Як це робити:
```Arduino
#include <ArduinoUnitTests.h>

void setup() {
  Serial.begin(9600);
}

void testFunction() {
  int a = 5;
  int b = a + 5;
  assertEqual(10, b);
}

void loop() {
  test(testFunction);
  if (testRunner.complete()) {
    Serial.println(testRunner.counts());
    while(true);
  }
}
```
Семпл виводу:
```
TestRunner started on 1 test(s).
Test testFunction passed.
TestRunner duration: 0.002 seconds.
TestRunner summary: 1 passed, 0 failed, 0 skipped, 0 timed out, out of 1 test(s).
```

## Підводне каміння:
Тестування коду для Arduino - це відносно нова практика, особливо у порівнянні з тестуванням для загальноприйнятих мов програмування. Замість стандартних бібліотек для тестування, що використовуються в інших мовах, для Arduino розроблено спеціальні бібліотеки, такі як "ArduinoUnitTests". Основні виклики полягають у відсутності підтримки великої кількості тестових фреймворків та обмеженому обладнанні.

## Зверніть також увагу:
- [Arduino Mock Library](https://github.com/FabioBatSilva/ArduinoFake) - бібліотека для мокінга (створення імітаційних об’єктів).
