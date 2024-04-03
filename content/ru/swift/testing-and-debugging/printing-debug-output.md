---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:01:06.931134-07:00
description: "\u041A\u0430\u043A: \u0412 Swift \u0443 \u0432\u0430\u0441 \u0435\u0441\
  \u0442\u044C \u0434\u0440\u0443\u0433 \u0432 \u043B\u0438\u0446\u0435 \u0444\u0443\
  \u043D\u043A\u0446\u0438\u0438 `print()`. \u041B\u0435\u0433\u043A\u043E \u0438\u0441\
  \u043F\u043E\u043B\u044C\u0437\u043E\u0432\u0430\u0442\u044C, \u043E\u043D\u0430\
  \ \u043F\u043E\u0437\u0432\u043E\u043B\u044F\u0435\u0442 \u0432\u0430\u043C \u0432\
  \u0438\u0434\u0435\u0442\u044C, \u0447\u0442\u043E \u043F\u0440\u043E\u0438\u0441\
  \u0445\u043E\u0434\u0438\u0442 \u0432 \u0432\u0430\u0448\u0435\u043C \u043A\u043E\
  \u0434\u0435."
lastmod: '2024-03-13T22:44:45.688464-06:00'
model: gpt-4-0125-preview
summary: "\u0412 Swift \u0443 \u0432\u0430\u0441 \u0435\u0441\u0442\u044C \u0434\u0440\
  \u0443\u0433 \u0432 \u043B\u0438\u0446\u0435 \u0444\u0443\u043D\u043A\u0446\u0438\
  \u0438 `print()`."
title: "\u0412\u044B\u0432\u043E\u0434 \u043E\u0442\u043B\u0430\u0434\u043E\u0447\u043D\
  \u043E\u0439 \u0438\u043D\u0444\u043E\u0440\u043C\u0430\u0446\u0438\u0438"
weight: 33
---

## Как:
В Swift у вас есть друг в лице функции `print()`. Легко использовать, она позволяет вам видеть, что происходит в вашем коде.

```Swift
var greeting = "Hello, playground"
print(greeting)
// Вывод: Hello, playground

let numbers = [1, 2, 3, 4, 5]
for number in numbers {
    print(number)
}
// Вывод:
// 1
// 2
// 3
// 4
// 5
```

Но это ещё не всё! Нужна подробная отладочная информация? `debugPrint()` к вашим услугам:

```Swift
debugPrint(greeting)
// Вывод: "Hello, playground"
```

Заметили эти кавычки? `debugPrint()` выдаёт подробности с дополнительной информацией о типах данных и структуре.

## Погружение
В старые добрые времена Objective-C мы использовали `NSLog` для вывода информации. Swift сделал всё проще — `print()` это ваш хлеб насущный для стандартного вывода, в то время как `debugPrint()` это ароматизированное масло для подробных представлений.

Интересный факт: стандартный вывод в Swift — это не только текст, это может быть любой тип, соответствующий `CustomStringConvertible` или `CustomDebugStringConvertible`. Эти протоколы позволяют настраивать внешний вид ваших объектов при их выводе на печать.

Под капотом `print()` и `debugPrint()` используют `String(describing:)` и `String(reflecting:)` для преобразования ваших объектов в строки. По сути эти функции используют зеркало, чтобы делать селфи ваших данных.

Альтернативы? У вас есть `os_log` и `NSLog`, но они больше подходят для логирования уровня продакшн, а не для быстрой и грязной отладки, с которой мы тут занимаемся.

## Смотрите также
- Справочник API Swift для функций печати: [Библиотека стандартов Swift: print(_:separator:terminator:)](https://developer.apple.com/documentation/swift/1541053-print)
- Более глубокий взгляд на логирование в Swift, рассмотрение GDPR и конфиденциальности: [Единое логирование и отслеживание активности](https://developer.apple.com/documentation/os/logging)
- Интерполяция строк в Swift и настраиваемость для отладочных описаний: [CustomStringConvertible](https://developer.apple.com/documentation/swift/customstringconvertible) и [CustomDebugStringConvertible](https://developer.apple.com/documentation/swift/customdebugstringconvertible)
