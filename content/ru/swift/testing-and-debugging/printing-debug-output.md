---
title:                "Вывод отладочной информации"
aliases: - /ru/swift/printing-debug-output.md
date:                  2024-01-29T00:01:06.931134-07:00
model:                 gpt-4-0125-preview
simple_title:         "Вывод отладочной информации"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/swift/printing-debug-output.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?

В Swift вывод отладочной информации означает отображение данных в области отладки, обычно внутри IDE или консоли, для мониторинга того, что происходит в реальном времени в вашем коде. Это основное действие для быстрого диагностирования проблем или понимания потока кода — думайте об этом как о заглядывании в мозг вашего кода.

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
