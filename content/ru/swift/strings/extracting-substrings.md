---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 23:58:29.743377-07:00
description: "\u041A\u0430\u043A: Swift \u0434\u0435\u043B\u0430\u0435\u0442 \u0440\
  \u0430\u0431\u043E\u0442\u0443 \u0441 \u043F\u043E\u0434\u0441\u0442\u0440\u043E\
  \u043A\u0430\u043C\u0438 \u0434\u043E\u0432\u043E\u043B\u044C\u043D\u043E \u043F\
  \u0440\u043E\u0441\u0442\u043E\u0439. \u0414\u0430\u0432\u0430\u0439\u0442\u0435\
  \ \u0441\u0440\u0430\u0437\u0443 \u0436\u0435 \u043F\u0435\u0440\u0435\u0439\u0434\
  \u0435\u043C \u043A \u043D\u0435\u043A\u043E\u0442\u043E\u0440\u044B\u043C \u043F\
  \u0440\u0438\u043C\u0435\u0440\u0430\u043C."
lastmod: '2024-03-13T22:44:45.663755-06:00'
model: gpt-4-0125-preview
summary: "Swift \u0434\u0435\u043B\u0430\u0435\u0442 \u0440\u0430\u0431\u043E\u0442\
  \u0443 \u0441 \u043F\u043E\u0434\u0441\u0442\u0440\u043E\u043A\u0430\u043C\u0438\
  \ \u0434\u043E\u0432\u043E\u043B\u044C\u043D\u043E \u043F\u0440\u043E\u0441\u0442\
  \u043E\u0439."
title: "\u0418\u0437\u0432\u043B\u0435\u0447\u0435\u043D\u0438\u0435 \u043F\u043E\u0434\
  \u0441\u0442\u0440\u043E\u043A"
weight: 6
---

## Как:
Swift делает работу с подстроками довольно простой. Давайте сразу же перейдем к некоторым примерам.

```swift
let fullString = "Hello, Swift Programmer!"
let startIndex = fullString.index(fullString.startIndex, offsetBy: 7)
let endIndex = fullString.index(fullString.startIndex, offsetBy: 12)

// Извлечение подстроки с использованием String.Index
let substring = fullString[startIndex...endIndex]

print(substring) // "Swift"

// Другой способ, используя NSRange и NSString
import Foundation

let nsRange = NSRange(location: 7, length: 6)
if let range = Range(nsRange, in: fullString) {
    let substring = fullString[range]
    print(substring) // "Swift"
}

// Краткий способ, если вы знаете точные индексы
let quickSubstring = fullString[7...12]

print(quickSubstring) // Это вызовет ошибку, потому что строки в Swift не поддерживают индексацию целыми числами
```

Вывод:
```
Swift
Swift
// Ошибка: 'subscript(_:)' недоступен: нельзя индексировать String с помощью Int, смотрите документацию по String для получения дополнительной информации
```

## Погружение
Извлечение подстрок в Swift включает в себя понимание того, как Swift управляет строками, что немного отличается от языков, таких как Python или C#. В Swift строки являются коллекциями символов, которые не используют целочисленные индексы. Это проистекает из поддержки Swift символов, совместимых с Unicode, делая строки не фиксированной длины, а скорее коллекцией графемных кластеров (то, что пользователь воспринимает как отдельный символ).

Этот дизайн означает, что прямая индексация целыми числами не работает со строками Swift; вам нужно работать с `String.Index`. Хотя это и не так интуитивно понятно, как использование целых чисел, это обеспечивает согласованную работу с различными текстовыми скриптами и эмодзи.

Альтернативы включают использование `NSString` из Objective-C, как показано в примерах, что позволяет использовать NSRange, но это довольно старомодно и не по-Swifty. Начиная с Swift 4, сама строка получила много улучшений, с богатым, более интуитивным набором API для работы с подстроками, оставив `NSString` в прошлом для большинства задач.

Детали реализации критически важны — наивное извлечение подстрок может привести к снижению производительности, поскольку каждый вызов `index(_: offsetBy:)` может быть O(n) при работе со строками, совместимыми с Unicode. Кроме того, когда вы создаете подстроку в Swift, она разделяет память исходной строки, что делает это эффективным, но это необходимо учитывать, если вы позже измените исходную строку.

## Смотрите также
Для получения дополнительной информации по этой теме ознакомьтесь с официальной документацией:

- Swift String и Characters: [https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- Руководство по программированию строк: [https://developer.apple.com/library/archive/documentation/Cocoa/Conceptual/Strings/](https://developer.apple.com/library/archive/documentation/Cocoa/Conceptual/Strings/)

Попрактикуйтесь и поэкспериментируйте в Swift playground, чтобы действительно разобраться в этом.
