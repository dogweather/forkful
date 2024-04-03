---
date: 2024-01-20 17:43:47.229610-07:00
description: "\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\u0438\
  : \u0412 Swift, \u0449\u043E\u0431 \u0432\u0438\u0434\u0430\u043B\u0438\u0442\u0438\
  \ \u0441\u0438\u043C\u0432\u043E\u043B\u0438 \u0437\u0430 \u0448\u0430\u0431\u043B\
  \u043E\u043D\u043E\u043C, \u0432\u0438\u043A\u043E\u0440\u0438\u0441\u0442\u043E\
  \u0432\u0443\u0439\u0442\u0435 `NSRegularExpression` \u0456 \u043C\u0435\u0442\u043E\
  \u0434 `stringByReplacingMatches`. \u041E\u0441\u044C \u043F\u0440\u043E\u0441\u0442\
  \u0438\u0439 \u043F\u0440\u0438\u043A\u043B\u0430\u0434."
lastmod: '2024-03-13T22:44:49.892849-06:00'
model: gpt-4-1106-preview
summary: "\u0412 Swift, \u0449\u043E\u0431 \u0432\u0438\u0434\u0430\u043B\u0438\u0442\
  \u0438 \u0441\u0438\u043C\u0432\u043E\u043B\u0438 \u0437\u0430 \u0448\u0430\u0431\
  \u043B\u043E\u043D\u043E\u043C, \u0432\u0438\u043A\u043E\u0440\u0438\u0441\u0442\
  \u043E\u0432\u0443\u0439\u0442\u0435 `NSRegularExpression` \u0456 \u043C\u0435\u0442\
  \u043E\u0434 `stringByReplacingMatches`."
title: "\u0412\u0438\u0434\u0430\u043B\u0435\u043D\u043D\u044F \u0441\u0438\u043C\u0432\
  \u043E\u043B\u0456\u0432 \u0437\u0430 \u0432\u0456\u0437\u0435\u0440\u0443\u043D\
  \u043A\u043E\u043C"
weight: 5
---

## Як це зробити:
В Swift, щоб видалити символи за шаблоном, використовуйте `NSRegularExpression` і метод `stringByReplacingMatches`. Ось простий приклад:

```Swift
import Foundation

func deleteCharacters(matching pattern: String, from input: String) -> String {
    let regex = try! NSRegularExpression(pattern: pattern)
    let range = NSRange(input.startIndex..<input.endIndex, in: input)
    return regex.stringByReplacingMatches(in: input, options: [], range: range, withTemplate: "")
}

let originalString = "Hello, World! 123."
let pattern = "[^A-Za-z ]" // Видалення усіх символів, крім літер алфавіту і пробілів

let cleanedString = deleteCharacters(matching: pattern, from: originalString)
print(cleanedString) // Виведе: "Hello World"
```

Sample output:
```
Hello World
```

## Детальніше:
Історично, регулярні вирази, як і `NSRegularExpression`, використовуються для таких операцій з текстами. Альтернатива – `String` методи в Swift, але регулярні вирази кращі для складних шаблонів. Важливо контролювати виключення `try!` при створенні `NSRegularExpression` – помилки можуть з'явитись, якщо шаблон неправильний.

## Також подивіться:
Для більш детального розуміння регулярних виразів:
- [NSRegularExpression](https://developer.apple.com/documentation/foundation/nsregularexpression)

Ознайомтеся з документацією Swift щодо рядків і символів:
- [Swift String and Characters](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)

Поглибити знання з роботою з текстовими даними в Swift допоможе:
