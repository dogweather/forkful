---
title:                "Отримання поточної дати"
html_title:           "Swift: Отримання поточної дати"
simple_title:         "Отримання поточної дати"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/swift/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Чому
Чому люди потребують отримати поточну дату? На це питання можна дати кілька відповідей. Деякі можуть потребувати поточну дату для реєстрації важливої події, інші можуть використовувати її для створення звіту про свою роботу. У цілому, отримання поточної дати є важливою задачею для багатьох програмістів.

## Як отримати поточну дату
Існує кілька способів отримання поточної дати в Swift. Перший, та найпростіший, це використання вбудованої функції `Date()`, яка повертає поточну дату та час. Наприклад:
```Swift
let currentDate = Date()
print(currentDate)
```
Ви зможете побачити результат подібний до цього: `2019-11-25 15:30:00 +0000`

Другий спосіб, це використати форматування строк для вказання того, як саме потрібно представити дату. Наприклад, для отримання дати у форматі `дд/мм/рррр`, можна використати такий код:
```Swift
let dateFormatter = DateFormatter()
dateFormatter.dateFormat = "dd/MM/yyyy"
let currentDate = Date()
let formattedDate = dateFormatter.string(from: currentDate)
print(formattedDate)
```
Результат буде виглядати так: `25/11/2019`.

## Deep Dive
Якщо ви хочете ще більшу гнучкість при роботі з датою, ви можете використовувати клас `Calendar`. Він дозволяє здійснювати різні операції з датами, такі як отримання конкретного дня або місяця. Наприклад, щоб отримати номер поточного місяця, можна використати наступний код:
```Swift
let calendar = Calendar.current
let month = calendar.component(.month, from: Date())
print(month)
```
Результат буде числом від 1 до 12, у залежності від поточного місяця.

## See Also 
- [Робота з датами в Swift](https://www.hackingwithswift.com/articles/154/how-to-work-with-dates-in-swift)
- [Офіційна документація Swift](https://developer.apple.com/documentation/swift)
- [Туторіал із роботи з датами в Swift](https://www.raywenderlich.com/792-nsdate-tutorial)