---
title:    "Swift: Об'єднання рядків"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/uk/swift/concatenating-strings.md"
---

{{< edit_this_page >}}

## Чому

Конкатенація рядків є важливою частиною програмування Swift, оскільки вона дозволяє об'єднувати рядкові значення для більш зручної обробки даних.

## Як

```Swift
let firstName = "Анна"
let lastName = "Коваль"
let fullName = firstName + " " + lastName // виводиться "Анна Коваль"
```

```Swift
let temperature = "20"
let unit = "градусів Цельсія"
let weatherInfo = "Сьогодні температура становить \(temperature) \(unit)" // виводиться "Сьогодні температура становить 20 градусів Цельсія"
```

## Глибше дослідження

Конкатенація рядків може бути корисною для об'єднання даних з різних джерел або для створення динамічних рядків. Також, в програмуванні Swift є багато інших способів об'єднання рядків, таких як використання оператора `+` або методу `append`.

## Подивіться також

- [Офіційна документація Swift для конкатенації рядків](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- [Туторіал на YouTube з прикладами конкатенації рядків в Swift](https://www.youtube.com/watch?v=GBb2p6BgOto)
- [Стаття з корисними порадами щодо конкатенації рядків у Swift](https://www.infragistics.com/community/blogs/b/news/posts/swift-and-string-concatenation)