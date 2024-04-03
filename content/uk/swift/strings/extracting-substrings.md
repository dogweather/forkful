---
date: 2024-01-20 17:46:44.089808-07:00
description: "\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\u0438\
  : Swift \u0432\u0438\u043A\u043E\u0440\u0438\u0441\u0442\u043E\u0432\u0443\u0454\
  \ `String.Index` \u0434\u043B\u044F \u0440\u043E\u0431\u043E\u0442\u0438 \u0437\
  \ \u043F\u0456\u0434\u0440\u044F\u0434\u043A\u0430\u043C\u0438. \u041E\u0441\u044C\
  \ \u0431\u0430\u0437\u043E\u0432\u0438\u0439 \u043F\u0440\u0438\u043A\u043B\u0430\
  \u0434."
lastmod: '2024-03-13T22:44:49.900687-06:00'
model: gpt-4-1106-preview
summary: "Swift \u0432\u0438\u043A\u043E\u0440\u0438\u0441\u0442\u043E\u0432\u0443\
  \u0454 `String.Index` \u0434\u043B\u044F \u0440\u043E\u0431\u043E\u0442\u0438 \u0437\
  \ \u043F\u0456\u0434\u0440\u044F\u0434\u043A\u0430\u043C\u0438."
title: "\u0412\u0438\u0434\u0456\u043B\u0435\u043D\u043D\u044F \u043F\u0456\u0434\u0440\
  \u044F\u0434\u043A\u0456\u0432"
weight: 6
---

## Як це зробити:
Swift використовує `String.Index` для роботи з підрядками. Ось базовий приклад:

```Swift
let text = "Привіт, світ!"
let start = text.index(text.startIndex, offsetBy: 8)
let end = text.index(text.startIndex, offsetBy: 11)
let substring = text[start...end]

print(substring) // Виведе: світ
```

Альтернативний спосіб через `range(of:)`:

```Swift
let text = "Привіт, світ!"
if let range = text.range(of: "світ") {
    let substring = text[range]
    print(substring) // Виведе: світ
}
```

## Глибше занурення:
В Swift, з моменту створення мови, робота з рядками була предметом оптимізацій та змін. Починаючи з Swift 4, рядки отримали більш зручні і ефективні API для роботи з індексами.

Важливо розуміти, що в Swift, коли ви отримуєте підрядок, ви отримуєте `Substring` тип, який спільно користується пам'яттю з оригінальним `String`. Це значить, що вам необхідно конвертувати `Substring` у новий `String`, якщо потрібно довготривале зберігання.

Іншим варіантом є використання `NSString` методів, але вони менш типові для Swift та можуть бути менш ефективними через зв'язки з Objective-C.

У реалізації строк Swift використовує комплексні алгоритми для зберігання та доступу до символів, що підтримують Unicode та запобігають надмірному споживанню пам'яті при роботі з підрядками.

## Дивіться також:
- Офіційний документ Swift про роботу зі строками: [Strings and Characters — The Swift Programming Language (Swift 5.7)](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- Подальше читання про оптимізацію роботи з рядками в Swift: [Optimizing Swift Performance](https://swift.org/blog/utf8-string/)
