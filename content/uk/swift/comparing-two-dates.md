---
title:                "Swift: Порівняння двох дат"
programming_language: "Swift"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/swift/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Чому

У програмуванні часто виникає потреба порівняти дві дати. Це може бути необхідно для сортування, фільтрації або визначення часу між подіями. У цій статті ми подивимося, як легко та ефективно порівняти дати з допомогою мови Swift.

## Як це зробити

Коли ми порівнюємо дати у Swift, ми фактично порівнюємо об'єкти типу `Date`. Для цього у нас є декілька методів та операторів, які допоможуть нам виконати цю задачу. Давайте подивимося на приклади: 

```Swift
let today = Date()
let tomorrow = Calendar.current.date(byAdding: .day, value: 1, to: today)

if today < tomorrow {
    print("Сьогодні раніше за завтра")
}

// Виведе "Сьогодні раніше за завтра"
```

Також ми можемо використовувати метод `compare`, щоб отримати результат порівняння у вигляді об'єкта типу `ComparisonResult`:

```Swift
let firstDate = Date()
let secondDate = Calendar.current.date(byAdding: .hour, value: 3, to: firstDate)

let result = firstDate.compare(secondDate!)

switch result {
case .orderedAscending:
    print("Перша дата раніше за другу")
case .orderedDescending:
    print("Перша дата пізніше за другу")
case .orderedSame:
    print("Дати рівні")
}
// Виведе "Перша дата пізніше за другу"
```

## Глибоке занурення

У випадку, коли нам не потрібно точно порівнювати дати і досягнута точність часу нам не важлива, ми можемо скористатися оператором `==` для порівняння дат у Swift. Це спрацює, якщо об'єкти містять однакові значення, але можуть бути різними об'єктами типу `Date`:

```Swift
let date1 = Date()
let date2 = Date(timeIntervalSince1970: date1.timeIntervalSince1970)

if date1 == date2 {
    print("Дати однакові")
}

// Виведе "Дати однакові"
```

Крім того, у Swift ми можемо порівнювати дати за допомогою деяких інших параметрів, таких як часовий пояс або календар. Для цього ми можемо використовувати метод `isEquals(to:)` з об'єктом типу `Calendar`.

## Дивіться також

- [Oficialna dokumentatsiia z porivniannia dat u Swift](https://developer.apple.com/documentation/foundation/date/1781605-compare)
- [Stack Overflow postopro porivniannia dat u Swift](https://stackoverflow.com/questions/42314272/comparing-two-dates-in-swift)
- [Codecademy tutorial na temu porivniannia dat u Swift](https://www.codecademy.com/learn/learn-swift/modules/learn-swift-conditions/cheatsheet)