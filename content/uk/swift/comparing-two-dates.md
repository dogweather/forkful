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

## Чому

Порівняння дат є важливою задачею в програмуванні, оскільки воно дозволяє перевірити, чи дві дати співпадають або яка з них більша чи менша. Це необхідно для роботи з часовими данними та для виконання певних дій в залежності від дат.

## Як

Для порівняння двох дат використовується функція `compare()` з класу `Date`. Ця функція повертає об'єкт типу `ComparisonResult`, який може мати три значення: `.orderedAscending`, `.orderedDescending` або `.orderedSame`. Перші два вказують на те, що одна дата більша або менша за іншу, а третє - що вони рівні.

Давайте розглянемо приклад коду, щоб зрозуміти як це працює:

```Swift
let date1 = Date()
let date2 = Date(timeIntervalSinceNow: 3600)

let comparisonResult = date1.compare(date2)

switch comparisonResult {
    case .orderedAscending:
        print("\(date1) є більшою за \(date2)")
    case .orderedDescending:
        print("\(date1) є меншою за \(date2)")
    case .orderedSame:
        print("\(date1) та \(date2) рівні")
}
```

В цьому прикладі ми створюємо два об'єкти `Date`: `date1` - поточна дата і `date2` - дата, яка наступає через 1 годину від поточної. За допомогою функції `compare()` ми отримуємо результат порівняння цих дат і за допомогою оператора `switch` виводимо в консоль відповідне повідомлення.

## Глибоке дослідження

Також існує можливість порівнювати дати за допомогою операторів `>`, `<`, `==` тощо. Також варто зазначити, що за замовчуванням дати порівнюються з точністю до секунди, але варто пам'ятати, що це може призводити до неточного результату, оскільки деякі дати можуть мати різний час до секунди.

## Дивись також

Для отримання додаткової інформації про роботу з датами в Swift, дивіться наступні ресурси:

- [Working with Dates in Swift](https://www.raywenderlich.com/67483099-working-with-dates-in-swift)
- [How to Use the Date Class in Swift](https://www.hackingwithswift.com/example-code/system/how-to-use-the-date-class-in-swift)
- [Introduction to Date and Time in Swift](https://medium.com/@abhimuralidharan/introduction-to-date-and-time-in-swift-c10b5eae99c1)