---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 23:58:48.548260-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: \u0414\u043E\u043F\u0443\u0441\u0442\u0438\u043C, \u0432\u0430\u0448\
  \ Arduino \u0441\u0447\u0438\u0442\u044B\u0432\u0430\u0435\u0442 \u0434\u0430\u043D\
  \u043D\u044B\u0435 \u0441 \u0434\u0430\u0442\u0447\u0438\u043A\u0430, \u043A\u043E\
  \u0442\u043E\u0440\u044B\u0439 \u0438\u043D\u043E\u0433\u0434\u0430 \u043C\u043E\
  \u0436\u0435\u0442 \u0432\u044B\u0434\u0430\u0432\u0430\u0442\u044C \u0437\u043D\
  \u0430\u0447\u0435\u043D\u0438\u044F \u0432\u043D\u0435 \u0434\u043E\u043F\u0443\
  \u0441\u0442\u0438\u043C\u043E\u0433\u043E \u0434\u0438\u0430\u043F\u0430\u0437\u043E\
  \u043D\u0430. \u0412\u043E\u0442 \u043A\u0430\u043A \u0432\u044B \u043C\u043E\u0436\
  \u0435\u0442\u0435\u2026"
lastmod: '2024-03-13T22:44:45.544623-06:00'
model: gpt-4-0125-preview
summary: "\u0414\u043E\u043F\u0443\u0441\u0442\u0438\u043C, \u0432\u0430\u0448 Arduino\
  \ \u0441\u0447\u0438\u0442\u044B\u0432\u0430\u0435\u0442 \u0434\u0430\u043D\u043D\
  \u044B\u0435 \u0441 \u0434\u0430\u0442\u0447\u0438\u043A\u0430, \u043A\u043E\u0442\
  \u043E\u0440\u044B\u0439 \u0438\u043D\u043E\u0433\u0434\u0430 \u043C\u043E\u0436\
  \u0435\u0442 \u0432\u044B\u0434\u0430\u0432\u0430\u0442\u044C \u0437\u043D\u0430\
  \u0447\u0435\u043D\u0438\u044F \u0432\u043D\u0435 \u0434\u043E\u043F\u0443\u0441\
  \u0442\u0438\u043C\u043E\u0433\u043E \u0434\u0438\u0430\u043F\u0430\u0437\u043E\u043D\
  \u0430."
title: "\u041E\u0431\u0440\u0430\u0431\u043E\u0442\u043A\u0430 \u043E\u0448\u0438\u0431\
  \u043E\u043A"
weight: 16
---

## Как это сделать:
Допустим, ваш Arduino считывает данные с датчика, который иногда может выдавать значения вне допустимого диапазона. Вот как вы можете обработать это:

```Arduino
int sensorValue = analogRead(A0);

if (sensorValue >= 0 && sensorValue <= 1023) {
  // Значение в пределах допустимого диапазона, продолжаем обработку
  Serial.println(sensorValue);
} else {
  // Значение вне допустимого диапазона, обрабатываем ошибку
  Serial.println("Ошибка: Значение датчика вне допустимого диапазона.");
}
```
Пример вывода:
```
523
Ошибка: Значение датчика вне допустимого диапазона.
761
```

## Подробнее
Обработка ошибок не всегда была такой простой. В начале разработки программ, разработчики часто игнорировали ошибки, что приводило к опасному "неопределенному поведению". С развитием программирования эволюционировали и инструменты — теперь во многих языках есть исключения, но в мире Arduino до сих пор сохраняется старая школа проверки "проверь сначала" из-за ограничений аппаратного обеспечения и корней C++.

В программировании для Arduino часто используются операторы `if-else` для обработки ошибок. Но есть альтернативы: использование функции `assert` для остановки выполнения, если условие не выполняется, или проектирование аварийной защиты непосредственно в вашей аппаратной конфигурации.

При реализации обработки ошибок учитывайте последствия остановки программы по сравнению с возможностью ее продолжения в стандартном или безопасном состоянии. Существует компромисс, и правильный выбор зависит от потенциального вреда от прерываний по сравнению с неправильной работой.

## Смотрите также
Изучите обнаружение и обработку ошибок с помощью следующих ресурсов:

- Справочник по языку Arduino: https://www.arduino.cc/reference/en/
- Более глубокий взгляд на обработку ошибок от Embedded Artistry: https://embeddedartistry.com/blog/2017/05/17/creating-a-circular-buffer-in-c-and-c/
- Обработка ошибок в C++: https://en.cppreference.com/w/cpp/error/exception

Это должно дать вам знания и уверенность, чтобы избежать подводных камней ошибок в ваших Arduino приключениях.
