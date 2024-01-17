---
title:                "Порівняння двох дат"
html_title:           "Swift: Порівняння двох дат"
simple_title:         "Порівняння двох дат"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/swift/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Що & Чому?
Порівняння двох дат означає визначення порядку між ними - чи одна дата відбулася до, після або в той самий час, що й інша дата. Програмісти використовують таке порівняння для сортування даних, визначення часових інтервалів та для багатьох інших завдань.

## Як:
```
let firstDate = Date()
let secondDate = Date(timeIntervalSinceNow: 3600)

// Порівняння за допомогою `compare`
if firstDate.compare(secondDate) == .orderedAscending {
  print("\(firstDate) відбулася до \(secondDate).")
} else if firstDate.compare(secondDate) == .orderedDescending {
  print("\(firstDate) відбулася після \(secondDate).")
} else {
  print("\(firstDate) та \(secondDate) відбулися в один час.")
}

// Використання операторів `>` та `<`
if firstDate > secondDate {
  print("\(firstDate) відбулася після \(secondDate).")
} else if firstDate < secondDate {
  print("\(firstDate) відбулася до \(secondDate).")
} else {
  print("\(firstDate) та \(secondDate) відбулися в один час.")
}

// Використання оператора `==` для порівняння на той самий час
if firstDate == secondDate {
  print("\(firstDate) та \(secondDate) відбулися в один час.")
}

// Порівняння за допомогою `timeIntervalSince`
if firstDate.timeIntervalSince(secondDate) > 0 {
  print("\(firstDate) відбулася після \(secondDate).")
} else if firstDate.timeIntervalSince(secondDate) < 0 {
  print("\(firstDate) відбулася до \(secondDate).")
} else {
  print("\(firstDate) та \(secondDate) відбулися в один час.")
}
```

Вихідні дані:
```
2019-08-21 17:10:56 +0000 відбулася до 2019-08-21 18:10:56 +0000. 
2019-08-21 17:10:56 +0000 відбулася до 2019-08-21 18:10:56 +0000. 
2019-08-21 17:10:56 +0000 та 2019-08-21 17:10:56 +0000 відбулися в один час.
2019-08-21 17:10:56 +0000 відбулася до 2019-08-21 18:10:56 +0000.
```

## Глибша розмова:
В порівнянні дат використовується логіка "менше", "більше" або "рівне". Історично, для порівняння дат використовувалися різні формати, наприклад Unix Timestamp або Julian Date. Але зараз, з появою класу `Date`, Swift надає простіший і більш зручний спосіб порівняння дат.

Існує кілька альтернативних способів порівняння дат, таких як використання `Calendar` або `DateComponents`. Вони дозволяють врахувати інший часовий пояс або календар при порівнянні дат.

У Swift порівняння дат здійснюється з використанням UTC (Універсального координованого часу), тому необхідно враховувати різницю в часових зонах при порівнянні дат.

## Дивіться також:
- [Визначення часу та дати в Swift](https://www.raywenderlich.com/160413/learn-swift-3-collections-date-enum)
- [Документація про `Date`](https://developer.apple.com/documentation/foundation/date)
- [Порівняння часу і дати в Swift](https://www.hackingwithswift.com/articles/175/how-to-compare-date-and-time-in-swift)