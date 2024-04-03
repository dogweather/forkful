---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:06:29.591462-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: Arduino \u0438\u0437\u043D\u0430\u0447\u0430\u043B\u044C\u043D\u043E\
  \ \u043D\u0435 \u043F\u043E\u0434\u0434\u0435\u0440\u0436\u0438\u0432\u0430\u0435\
  \u0442 stderr, \u043D\u043E \u043C\u044B \u043C\u043E\u0436\u0435\u043C \u0438\u043C\
  \u0438\u0442\u0438\u0440\u043E\u0432\u0430\u0442\u044C \u0435\u0433\u043E, \u0437\
  \u0430\u043F\u0438\u0441\u044B\u0432\u0430\u044F \u0432 Serial. \u041F\u0440\u0435\
  \u0434\u0441\u0442\u0430\u0432\u0438\u043C \u043F\u0440\u043E\u0433\u0440\u0430\u043C\
  \u043C\u0443 \u043C\u0438\u0433\u0430\u043D\u0438\u044F \u0441\u0432\u0435\u0442\
  \u043E\u0434\u0438\u043E\u0434\u043E\u043C \u0441\u2026"
lastmod: '2024-03-13T22:44:45.561589-06:00'
model: gpt-4-0125-preview
summary: "Arduino \u0438\u0437\u043D\u0430\u0447\u0430\u043B\u044C\u043D\u043E \u043D\
  \u0435 \u043F\u043E\u0434\u0434\u0435\u0440\u0436\u0438\u0432\u0430\u0435\u0442\
  \ stderr, \u043D\u043E \u043C\u044B \u043C\u043E\u0436\u0435\u043C \u0438\u043C\u0438\
  \u0442\u0438\u0440\u043E\u0432\u0430\u0442\u044C \u0435\u0433\u043E, \u0437\u0430\
  \u043F\u0438\u0441\u044B\u0432\u0430\u044F \u0432 Serial."
title: "\u0417\u0430\u043F\u0438\u0441\u044C \u0432 \u0441\u0442\u0430\u043D\u0434\
  \u0430\u0440\u0442\u043D\u044B\u0439 \u043F\u043E\u0442\u043E\u043A \u043E\u0448\
  \u0438\u0431\u043E\u043A"
weight: 25
---

## Как это сделать:
Arduino изначально не поддерживает stderr, но мы можем имитировать его, записывая в Serial. Представим программу мигания светодиодом с проверкой на ошибки:

```Arduino
void setup() {
  Serial.begin(9600);
  pinMode(LED_BUILTIN, OUTPUT);
}

void loop() {
  if(!digitalWriteCheck(LED_BUILTIN, HIGH)) {
    Serial.println("Ошибка: Невозможно установить LED в высокое состояние"); // Это наш "stderr"
  }
  delay(1000); // Подождать секунду
  if(!digitalWriteCheck(LED_BUILTIN, LOW)) {
    Serial.println("Ошибка: Невозможно установить LED в низкое состояние"); // Это наш "stderr"
  }
  delay(1000); // Подождать секунду
}

bool digitalWriteCheck(int pin, int value) {
  // Представим, что эта функция проверяет, была ли успешной функция digitalWrite
  digitalWrite(pin, value);
  // Если успех - возвращаем true, давайте для примера всегда возвращать false
  return false;
}
```

Пример вывода:
```
Ошибка: Невозможно установить LED в высокое состояние
Ошибка: Невозможно установить LED в низкое состояние
```

## Подробнее
Исторически, stderr является стандартным потоком во многих операционных системах, введенным Unix. В Arduino, которая не имеет операционной системы, мы вручную выводим ошибки, используя Serial.print или похожие команды. Если вы ведете журнал на компьютере, логи могут быть перенаправлены из Serial в файл, эффективно отделяя их от stdout. Продвинутые пользователи могут использовать SoftwareSerial для эмуляции stderr на разных аппаратных последовательных портах.

## Смотрите также
- Официальную документацию Arduino по Serial: https://www.arduino.cc/reference/en/language/functions/communication/serial/
- Стандартные потоки Unix: https://en.wikipedia.org/wiki/Standard_streams
- Библиотеку SoftwareSerial: https://www.arduino.cc/en/Reference/SoftwareSerial
