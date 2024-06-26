---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:03:13.130484-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0434\u0435\u043B\u0430\u0435\
  \u0442\u0441\u044F: \u041C\u044B \u0437\u0430\u043D\u0438\u043C\u0430\u0435\u043C\
  \u0441\u044F \u0437\u0430\u043C\u0435\u043D\u043E\u0439 \u0442\u0435\u043A\u0441\
  \u0442\u0430 \u0432 \u0441\u0442\u0440\u043E\u043A\u0430\u0445 \u0441 \u0441\u0430\
  \u043C\u044B\u0445 \u0440\u0430\u043D\u043D\u0438\u0445 \u0434\u043D\u0435\u0439\
  \ \u043A\u043E\u043C\u043F\u044C\u044E\u0442\u0435\u0440\u043D\u043E\u0439 \u044D\
  \u0440\u044B. \u0418\u0437\u043D\u0430\u0447\u0430\u043B\u044C\u043D\u043E \u044D\
  \u0442\u043E \u0431\u044B\u043B\u043E \u0441 \u043F\u0440\u043E\u0441\u0442\u044B\
  \u043C\u0438 \u0438\u043D\u0441\u0442\u0440\u0443\u043C\u0435\u043D\u0442\u0430\u043C\
  \u0438 \u043A\u043E\u043C\u0430\u043D\u0434\u043D\u043E\u0439\u2026"
lastmod: '2024-04-05T21:53:46.033878-06:00'
model: gpt-4-0125-preview
summary: "\u041C\u044B \u0437\u0430\u043D\u0438\u043C\u0430\u0435\u043C\u0441\u044F\
  \ \u0437\u0430\u043C\u0435\u043D\u043E\u0439 \u0442\u0435\u043A\u0441\u0442\u0430\
  \ \u0432 \u0441\u0442\u0440\u043E\u043A\u0430\u0445 \u0441 \u0441\u0430\u043C\u044B\
  \u0445 \u0440\u0430\u043D\u043D\u0438\u0445 \u0434\u043D\u0435\u0439 \u043A\u043E\
  \u043C\u043F\u044C\u044E\u0442\u0435\u0440\u043D\u043E\u0439 \u044D\u0440\u044B."
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
