---
title:                "Розрахунок дати в майбутньому або минулому"
html_title:           "Swift: Розрахунок дати в майбутньому або минулому"
simple_title:         "Розрахунок дати в майбутньому або минулому"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/swift/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Що і чому?
Обчислення дати в майбутньому або минулому є дією випередження або відкату календарного часу. Програмісти виконують це для планування подій, відслідковування термінів і управління даними.

## Як це робиться:
Розглянемо, як можна зробити це в Swift за допомогою `Calendar` і `DateComponents`. 

```Swift
import Foundation

let calendar = Calendar.current
let currentDate = Date()

// Обчислення дати на 7 днів вперед
let dateInFuture = calendar.date(byAdding: .day, value: 7, to: currentDate)

// Обчислення дати на 7 днів назад
let dateInPast = calendar.date(byAdding: .day, value: -7, to: currentDate)

print("Зараз: \(currentDate)")
print("Через 7 днів: \(dateInFuture ?? currentDate)")
print("7 днів тому: \(dateInPast ?? currentDate)")
```

## Поглиблений розбір:
Система управління датою і часом в Swift розвивалась із версії 1980 року, починаючи з ANSI C. Swift впровадив сучаснішу систему `Date`, `Time` і `Calendar` для точних календарних обчислень.

Альтернативою може бути використання типу `TimeInterval` для обчислення дати в майбутньому або минулому. Однак, це може бути менш точним, особливо для довгих проміжків часу.

Важливо знати, що Swift використовує систему часових поясів. Метод `date(byAdding:value:to:)` враховує поточний часовий пояс.

## Дивись також:
- [Date & Time Programming Guide](https://developer.apple.com/library/archive/documentation/Cocoa/Conceptual/DatesAndTimes/DatesAndTimes.html)
- [Swift Date Documentation](https://developer.apple.com/documentation/foundation/date)
- [Swift Calendar Documentation](https://developer.apple.com/documentation/foundation/calendar)