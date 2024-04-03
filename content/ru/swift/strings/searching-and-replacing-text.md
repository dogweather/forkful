---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:03:13.130484-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0434\u0435\u043B\u0430\u0435\
  \u0442\u0441\u044F: ."
lastmod: '2024-03-13T22:44:45.656639-06:00'
model: gpt-4-0125-preview
summary: .
title: "\u041F\u043E\u0438\u0441\u043A \u0438 \u0437\u0430\u043C\u0435\u043D\u0430\
  \ \u0442\u0435\u043A\u0441\u0442\u0430"
weight: 10
---

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
