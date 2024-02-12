---
title:                "Написание тестов"
aliases:
- /ru/arduino/writing-tests.md
date:                  2024-01-29T00:05:50.583700-07:00
model:                 gpt-4-0125-preview
simple_title:         "Написание тестов"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/arduino/writing-tests.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?
Написание тестов означает создание набора условий для проверки соответствия поведения вашего кода ожиданиям. Программисты делают это для того, чтобы заранее выявлять ошибки, обеспечивать качество программного обеспечения и избегать головной боли при добавлении новых функций.

## Как это сделать:
```Arduino
#include <Arduino.h>
#include <unity.h>

void setUp(void) {
// подготовка перед тестом
}

void tearDown(void) {
// очистка после теста
}

void test_led_builtin_pin_number(void) {
    TEST_ASSERT_EQUAL(13, LED_BUILTIN);
}

void test_led_state_high(void) {
    digitalWrite(LED_BUILTIN, HIGH);
    TEST_ASSERT_EQUAL(digitalRead(LED_BUILTIN), HIGH);
}

void setup() {
    UNITY_BEGIN();
    RUN_TEST(test_led_builtin_pin_number);
    RUN_TEST(test_led_state_high);
    UNITY_END();
}

void loop() {
    // Обычно пусто в режиме тестирования
}
```
Вывод:
```
.
.
OK
```

## Глубокое погружение
Исторический контекст: Тестирование на Arduino появилось позже, чем в разработке программного обеспечения, и было менее распространённым из-за взаимодействия с аппаратурой. Альтернативы: Ручное тестирование или более сложные фреймворки для тестирования, такие как Google Test. Детали реализации: Обычно мы используем библиотеку вроде ArduinoUnit или AUnit. Размещаем тесты в `setup()` и оставляем `loop()` пустым, так как тесты запускаются один раз.

## Смотрите также
- Библиотека ArduinoUnit: https://github.com/mmurdoch/arduinounit
- Библиотека AUnit: https://github.com/bxparks/AUnit
- Введение в модульное тестирование: https://www.arduino.cc/en/Guide/UnitTesting
