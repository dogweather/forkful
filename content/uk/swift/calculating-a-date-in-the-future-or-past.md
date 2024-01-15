---
title:                "Обчислення дати в майбутньому або минулому"
html_title:           "Swift: Обчислення дати в майбутньому або минулому"
simple_title:         "Обчислення дати в майбутньому або минулому"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/swift/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Чому

Пересування у часі починається з можливості обрахувати дати у майбутньому або минулому. Це може бути корисно для планування подій або отримання інформації з минулих подій.

## Як

Існує кілька способів обрахувати дату у майбутньому або минулому в Swift. Один із способів це використовувати клас `Date` та його метод `addingTimeInterval`, який дозволяє додавати або віднімати часовий інтервал до поточної дати. 

```Swift
let currentDate = Date() // поточна дата
let futureDate = currentDate.addingTimeInterval(86400) // добавляємо один день (в секундах) до поточної дати
let pastDate = currentDate.addingTimeInterval(-604800) // віднімаємо один тиждень (в секундах) від поточної дати
print(futureDate) // Виводить: 2021-10-07 07:02:25 +0000
print(pastDate) // Виводить: 2021-09-29 07:02:25 +0000
```

Ще один спосіб - використовувати `Calendar` та його метод `date(byAdding:to:wrappingComponents:)`. Цей метод дозволяє додавати або віднімати часові компоненти, такі як дні, тижні, місяці або роки, до поточної дати.

```Swift
let currentDate = Date()
let calendar = Calendar.current
let futureDate = calendar.date(byAdding: .day, value: 7, to: currentDate)
let pastDate = calendar.date(byAdding: .weekOfMonth, value: -1, to: currentDate)
print(futureDate) // Виводить: 2021-10-07 07:02:25 +0000
print(pastDate) // Виводить: 2021-09-29 07:02:25 +0000
```

Обидва цих методи дозволяють обчитати дати у різних часових зонах, якщо передати до них відповідні параметри.

## Глибокий занурення
Всі дати в Swift вимірюються від початку світу 1 січня 2001 року, існує багато інших деталей і особливостей, пов'язаних з роботою з датами. Для детальнішого розуміння рекомендуємо ознайомитися з [офіційною документацією Swift Date](https://developer.apple.com/documentation/foundation/date).

## Також

- [Working with Dates and Times in Swift](https://www.hackingwithswift.com/read/11/overview)
- [Formatting Dates and Times in Swift](https://www.appcoda.com/swift-format-dates/)
- [Common Pitfalls when Working with Dates in Swift](https://medium.com/@jigar2805/common-pitfalls-when-working-with-dates-and-time-in-swift-85bb2c503d1)