---
title:                "Arduino: Написання тестів"
programming_language: "Arduino"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/arduino/writing-tests.md"
---

{{< edit_this_page >}}

## Чому

Надання надійного та ефективного програмного коду є критично важливим завданням для більшості проектів на Arduino платформі. Завдяки написанню тестів ви можете впевнитися, що ваш код працює як очікувалося, та запобігти появі помилок, які можуть виникнути в майбутньому.

## Як це зробити

Написання тестів на Arduino - це простий процес, який забезпечить надійність та ефективність вашого коду. Основною метою тестування є перевірка правильності роботи коду. В цьому прикладі ми напишемо тест для перевірки роботи події натискання кнопки:

```Arduino
const int buttonPin = 2;  // написання сигнального піна
const int ledPin = 13; // написання приймаючого піна

void setup() {
  pinMode(ledPin, OUTPUT); // встановлюємо приймаючий пін як вихідний
  pinMode(buttonPin, INPUT);  // встановлюємо сигнальний пін як вхідний
}

void loop() {
  if (digitalRead(buttonPin) == HIGH) { // перевіряємо, чи натиснута кнопка
    digitalWrite(ledPin, HIGH); // вмикаємо LED
  } else {
    digitalWrite(ledPin, LOW); // вимикаємо LED
  }
}
```

В даному прикладі ми використовуємо функцію `digitalRead` для перевірки стану кнопки та функцію `digitalWrite` для керування світлодіодом на платі. Тепер давайте напишемо тест для перевірки цього коду:

```Arduino
#include "Arduino.h"
#include "unity.h"

#define main loop // переозначення функції loop

void test_button_press() {
  digitalWrite(buttonPin, HIGH); // встановлюємо високий рівень сигнального піна
  main(); // викликаємо головну функцію
  TEST_ASSERT_EQUAL(HIGH, digitalRead(ledPin)); // перевірка, чи LED увімкнений
}

void test_button_not_pressed() {
  digitalWrite(buttonPin, LOW); // встановлюємо низький рівень сигнального піна
  main(); // викликаємо головну функцію
  TEST_ASSERT_EQUAL(LOW, digitalRead(ledPin)); // перевірка, чи LED вимкнений
}

void setup() {
  pinMode(buttonPin, OUTPUT);  // встановлюємо сигнальний пін як вхідний
  pinMode(ledPin, OUTPUT); // встановлюємо приймаючий пін як вихідний
  UNITY_BEGIN(); // ініціалізація механізму тестування
  RUN_TEST(test_button_press); // запуск першого тесту
  RUN_TEST(test_button_not_pressed); // запуск другого тесту
}

void loop() {
  UNITY_END(); // завершення механізму тестування
}
```

Тепер ми можемо запустити цей тест та переконатися, що наш код прац