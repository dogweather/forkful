---
title:                "Извлечение подстрок"
date:                  2024-01-28T23:58:29.743377-07:00
model:                 gpt-4-0125-preview
simple_title:         "Извлечение подстрок"

category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/swift/extracting-substrings.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и почему?

Извлечение подстрок означает получение лишь части строки — подобно тому, как если бы вы отрезали ленту нужной длины. Программисты делают это, чтобы изолировать, анализировать или манипулировать определенными фрагментами текстовых данных, такими как пользовательский ввод, имена файлов или обработка текста.

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
