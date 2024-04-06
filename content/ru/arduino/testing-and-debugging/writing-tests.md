---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:05:50.583700-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: \u0418\u0441\u0442\u043E\u0440\u0438\u0447\u0435\u0441\u043A\u0438\
  \u0439 \u043A\u043E\u043D\u0442\u0435\u043A\u0441\u0442: \u0422\u0435\u0441\u0442\
  \u0438\u0440\u043E\u0432\u0430\u043D\u0438\u0435 \u043D\u0430 Arduino \u043F\u043E\
  \u044F\u0432\u0438\u043B\u043E\u0441\u044C \u043F\u043E\u0437\u0436\u0435, \u0447\
  \u0435\u043C \u0432 \u0440\u0430\u0437\u0440\u0430\u0431\u043E\u0442\u043A\u0435\
  \ \u043F\u0440\u043E\u0433\u0440\u0430\u043C\u043C\u043D\u043E\u0433\u043E \u043E\
  \u0431\u0435\u0441\u043F\u0435\u0447\u0435\u043D\u0438\u044F, \u0438 \u0431\u044B\
  \u043B\u043E \u043C\u0435\u043D\u0435\u0435 \u0440\u0430\u0441\u043F\u0440\u043E\
  \u0441\u0442\u0440\u0430\u043D\u0451\u043D\u043D\u044B\u043C\u2026"
lastmod: '2024-04-05T22:50:58.904633-06:00'
model: gpt-4-0125-preview
summary: "\u0418\u0441\u0442\u043E\u0440\u0438\u0447\u0435\u0441\u043A\u0438\u0439\
  \ \u043A\u043E\u043D\u0442\u0435\u043A\u0441\u0442."
title: "\u041D\u0430\u043F\u0438\u0441\u0430\u043D\u0438\u0435 \u0442\u0435\u0441\u0442\
  \u043E\u0432"
weight: 36
---

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
