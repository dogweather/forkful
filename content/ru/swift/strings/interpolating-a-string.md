---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 23:59:02.258044-07:00
description: "\u041A\u0430\u043A: Swift \u0434\u0435\u043B\u0430\u0435\u0442 \u0438\
  \u043D\u0442\u0435\u0440\u043F\u043E\u043B\u044F\u0446\u0438\u044E \u0441\u0442\u0440\
  \u043E\u043A \u043E\u0447\u0435\u043D\u044C \u043F\u0440\u043E\u0441\u0442\u043E\
  \u0439 \u0441 \u043F\u043E\u043C\u043E\u0449\u044C\u044E \u0441\u0438\u043D\u0442\
  \u0430\u043A\u0441\u0438\u0441\u0430 `\\(\u0438\u043C\u044F\u041F\u0435\u0440\u0435\
  \u043C\u0435\u043D\u043D\u043E\u0439)`."
lastmod: '2024-03-13T22:44:45.658422-06:00'
model: gpt-4-0125-preview
summary: "Swift \u0434\u0435\u043B\u0430\u0435\u0442 \u0438\u043D\u0442\u0435\u0440\
  \u043F\u043E\u043B\u044F\u0446\u0438\u044E \u0441\u0442\u0440\u043E\u043A \u043E\
  \u0447\u0435\u043D\u044C \u043F\u0440\u043E\u0441\u0442\u043E\u0439 \u0441 \u043F\
  \u043E\u043C\u043E\u0449\u044C\u044E \u0441\u0438\u043D\u0442\u0430\u043A\u0441\u0438\
  \u0441\u0430 `\\(\u0438\u043C\u044F\u041F\u0435\u0440\u0435\u043C\u0435\u043D\u043D\
  \u043E\u0439)`."
title: "\u0418\u043D\u0442\u0435\u0440\u043F\u043E\u043B\u044F\u0446\u0438\u044F \u0441\
  \u0442\u0440\u043E\u043A\u0438"
weight: 8
---

## Как:
Swift делает интерполяцию строк очень простой с помощью синтаксиса `\(имяПеременной)`.

```Swift
let name = "Джейн"
let age = 28
let greeting = "Привет, \(name), тебе \(age) лет."
print(greeting)  // Вывод: Привет, Джейн, тебе 28 лет.
```

Вы даже можете выполнять операции внутри интерполяции:

```Swift
let apples = 3
let oranges = 5
let fruitSummary = "У меня есть \(apples + oranges) кусочков фруктов."
print(fruitSummary)  // Вывод: У меня есть 8 кусочков фруктов.
```

## Подробнее
Хорошо, давайте немного о истории. Интерполяция строк не уникальна для Swift. Она существует во многих языках (например, JavaScript, Python и т.д.), но версия Swift является типобезопасной, что означает, что компилятор проверяет типы за вас, сокращая количество ошибок.

До Swift 5 интерполяция строк была менее мощной и более громоздкой. Но Swift 5 ввёл расширенную интерполяцию строк, что позволяет настраивать интерполяцию строк, предоставляя впечатляющую гибкость.

Альтернативы интерполяции строк в Swift включают конкатенацию с использованием `+` и более старый метод `String(format:)`. Однако эти методы менее удобны и, для строк формата, труднее для чтения.

Детали реализации? С интерполяцией строк Swift вы можете настраивать, как типы представлены в строках, расширяя протокол `StringInterpolation`. Это означает, что вы можете определить, как пользовательские типы отображаются во время интерполяции, что очень удобно.

```Swift
extension String.StringInterpolation {
    mutating func appendInterpolation(_ value: Date) {
        let formatter = DateFormatter()
        formatter.dateStyle = .medium
        appendLiteral(formatter.string(from: value))
    }
}

let today = Date()
let dateString = "Сегодняшняя дата \(today)."
print(dateString) // Вывод будет сегодняшняя дата в формате средней длины.
```

## Смотрите также
Чтобы узнать все детали об интерполяции строк, документация Swift - кладезь знаний:
- [Интерполяция Строк](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html#ID292)
- [Предложение по улучшению интерполяции строк в Swift Evolution](https://github.com/apple/swift-evolution/blob/main/proposals/0228-fix-expressiblebystringinterpolation.md)

Для более глубокого погружения в форматирование пользовательских типов:
- [Настройка Интерполяции Строк в Swift](https://www.hackingwithswift.com/articles/178/super-powered-string-interpolation-in-swift-5)
