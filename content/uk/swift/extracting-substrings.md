---
title:                "Виділення підрядків"
date:                  2024-01-20T17:46:44.089808-07:00
model:                 gpt-4-1106-preview
simple_title:         "Виділення підрядків"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/swift/extracting-substrings.md"
---

{{< edit_this_page >}}

## Що це та навіщо?
Витягування підрядків – це процес отримання частини рядка. Програмісти роблять це, щоб маніпулювати або аналізувати конкретні дані в рамках більших рядків.

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