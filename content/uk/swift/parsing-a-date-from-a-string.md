---
title:                "Розбір дати з рядка"
html_title:           "Swift: Розбір дати з рядка"
simple_title:         "Розбір дати з рядка"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/swift/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Що & Чому?

Розбір дати з рядка - це процес перетворення рядка з датою на об'єкт дати, який можна обробляти у програмі. Програмісти роблять це, щоб привести дату з формату, який необхідно використовувати в програмі.

## Як:

```Swift
// приклад рядка з датою
let dateStr = "2021-05-25"
// створення об'єкта дати з рядка
guard let date = DateFormatter().date(from: dateStr) else { return }
// друк дати в різних форматах
print(DateFormatter().string(from: date)) // "May 25, 2021"
print(DateFormatter().string(from: date, with: "dd/MM/yyyy")) // "25/05/2021"
```

## Глибше:
Розбір дати з рядка - це процес, який став особливо важливим з появою різних форматів дат у світі інтернету та ПЗ. Цю задачу можна вирішити іншими способами, наприклад, розбивши рядок на окремі складові та перетворивши їх в числові значення. Однак, використання вбудованої у Swift класу DateFormatter дозволяє це зробити швидше та надійніше.

## Дивись Також:
- [Документація Swift для класу DateFormatter](https://developer.apple.com/documentation/foundation/dateformatter)
- [Стаття про роботу з датами в Swift](https://www.hackingwithswift.com/articles/148/how-to-work-with-dates-and-times-in-swift)