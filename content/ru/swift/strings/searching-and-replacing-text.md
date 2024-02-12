---
title:                "Поиск и замена текста"
date:                  2024-01-29T00:03:13.130484-07:00
model:                 gpt-4-0125-preview
simple_title:         "Поиск и замена текста"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/swift/searching-and-replacing-text.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Зачем?

Поиск и замена текста в программировании - это именно то, что звучит: сканирование строк на наличие определенных шаблонов и их замена на что-то другое. Программисты делают это часто - для очистки данных, обновления пользовательского интерфейса или подготовки строк к обработке.

## Как это делается:

```Swift
var приветствие = "Привет, старый друг!"

// Простая замена
приветствие = приветствие.replacingOccurrences(of: "старый", with: "новый")
print(приветствие) // "Привет, новый друг!"

// Использование опций для регистронезависимой замены
let результатБезУчетаРегистра = приветствие.replacingOccurrences(
    of: "привет",
    with: "Здравствуй",
    options: .caseInsensitive
)
print(результатБезУчетаРегистра) // "Здравствуй, новый друг!"

// Замена с использованием регулярных выражений
let результатСРегулярнымВыражением = приветствие.replacingOccurrences(
    of: "\\bновый\\b",
    with: "лучший",
    options: .regularExpression
)
print(результатСРегулярнымВыражением) // "Привет, лучший друг!"
```

## Подробнее

Мы занимаемся заменой текста в строках с самых ранних дней компьютерной эры. Изначально это было с простыми инструментами командной строки, как `sed`. В Swift `replacingOccurrences(of:with:)` выполняет основную работу, и вы получаете больше контроля с такими опциями, как `.caseInsensitive` или `.regularExpression`.

Альтернативы в Swift включают использование `NSRegularExpression` для сложных шаблонов и `NSMutableString` для операций с изменяемыми строками. Под капотом методы замены строк Swift связаны с мощными аналогами в Objective-C, обеспечивая скорость и универсальность.

## Смотрите также

- [Документация по строкам Swift](https://developer.apple.com/documentation/swift/string/)
- [Регулярные выражения в Swift](https://nshipster.com/swift-regular-expressions/)
- [Swift.org - Работа со строками](https://swift.org/documentation/api-design-guidelines/#strive-for-fluent-usage)
