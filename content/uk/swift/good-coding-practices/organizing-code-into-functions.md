---
date: 2024-01-26 01:16:34.535969-07:00
description: "\u042F\u043A \u0446\u0435 \u0440\u043E\u0431\u0438\u0442\u0438: \u0423\
  \u044F\u0432\u0456\u0442\u044C \u0437\u0430\u0432\u0434\u0430\u043D\u043D\u044F\
  : \u0440\u043E\u0437\u0440\u0430\u0445\u0443\u0432\u0430\u0442\u0438 \u0441\u0435\
  \u0440\u0435\u0434\u043D\u0454 \u0437\u043D\u0430\u0447\u0435\u043D\u043D\u044F\
  \ \u043C\u0430\u0441\u0438\u0432\u0443. \u0411\u0435\u0437 \u0444\u0443\u043D\u043A\
  \u0446\u0456\u0439 \u0432\u0438 \u0431 \u0443\u0441\u0435 \u0446\u0435 \u0432\u0441\
  \u0442\u0430\u0432\u0438\u043B\u0438 \u0431 \u043F\u0440\u044F\u043C\u043E \u0443\
  \ main. \u0417 \u0444\u0443\u043D\u043A\u0446\u0456\u044F\u043C\u0438 \u0432\u0438\
  \ \u0437\u0440\u043E\u0431\u0438\u043B\u0438 \u0431 \u0442\u0430\u043A."
lastmod: '2024-04-05T22:38:48.828952-06:00'
model: gpt-4-0125-preview
summary: "\u0423\u044F\u0432\u0456\u0442\u044C \u0437\u0430\u0432\u0434\u0430\u043D\
  \u043D\u044F: \u0440\u043E\u0437\u0440\u0430\u0445\u0443\u0432\u0430\u0442\u0438\
  \ \u0441\u0435\u0440\u0435\u0434\u043D\u0454 \u0437\u043D\u0430\u0447\u0435\u043D\
  \u043D\u044F \u043C\u0430\u0441\u0438\u0432\u0443. \u0411\u0435\u0437 \u0444\u0443\
  \u043D\u043A\u0446\u0456\u0439 \u0432\u0438 \u0431 \u0443\u0441\u0435 \u0446\u0435\
  \ \u0432\u0441\u0442\u0430\u0432\u0438\u043B\u0438 \u0431 \u043F\u0440\u044F\u043C\
  \u043E \u0443 main. \u0417 \u0444\u0443\u043D\u043A\u0446\u0456\u044F\u043C\u0438\
  \ \u0432\u0438 \u0437\u0440\u043E\u0431\u0438\u043B\u0438 \u0431 \u0442\u0430\u043A\
  ."
title: "\u041E\u0440\u0433\u0430\u043D\u0456\u0437\u0430\u0446\u0456\u044F \u043A\u043E\
  \u0434\u0443 \u0432 \u0444\u0443\u043D\u043A\u0446\u0456\u0457"
weight: 18
---

## Як це робити:
Уявіть завдання: розрахувати середнє значення масиву. Без функцій ви б усе це вставили б прямо у main. З функціями ви зробили б так:

```swift
func calculateAverage(of numbers: [Double]) -> Double {
    let sum = numbers.reduce(0, +)
    return numbers.isEmpty ? 0 : sum / Double(numbers.count)
}

// Використання
let scores = [92.5, 88.75, 99.0, 70.5]
let averageScore = calculateAverage(of: scores)
print("Середній бал \(averageScore)")
```

Приклад виводу буде: 
```
Середній бал 87.6875
```

## Поглиблено
Історично, з ростом складності програмування, функції стали каменем основи для управління складністю. Альтернативи включають вбудоване кодування та копіювання коду (код-спагеті) - зараз це вважається поганою практикою. У Swift функції є громадянами першого рангу; їх можна призначати змінним, передавати як аргументи та повертати з інших функцій, роблячи код більш модульним і гнучким.

У виконавському плані, проектуйте свої функції так, щоб вони добре виконували одну річ. Прагніть функцій з чіткою метою та назвою, яка це відображає. Слідкуйте за кількістю параметрів — якщо їх забагато, то ви, ймовірно, робите занадто багато. Обробка помилок? Розгляньте можливість створення функцій з викиданням і граціозною обробкою проблем. Пам’ятайте: Swift все про читабельність та легкість обслуговування.

## Дивіться також
- [Путівник мовою програмування Swift - Функції](https://docs.swift.org/swift-book/LanguageGuide/Functions.html)
- [Swift Style Guide від Ray Wenderlich](https://github.com/raywenderlich/swift-style-guide)
- [Рефакторинг: вдосконалення дизайну існуючого коду від Мартіна Фаулера](https://martinfowler.com/books/refactoring.html)
