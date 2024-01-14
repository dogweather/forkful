---
title:                "Swift: Порівняння двох дат"
simple_title:         "Порівняння двох дат"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/swift/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Чому 

В програмуванні часто необхідно порівнювати дати, щоб визначити, чи одна дата відбулася до іншої, чи після неї. Це може бути корисно при створенні функцій для розрахунку віку, перевірки термінів дії документів та багатьох інших сценаріїв. У цій статті ми розглянемо, як порівнювати дві дати за допомогою мови програмування Swift.

## Як 

Для порівняння дат у Swift використовується структура `Date`. Давайте розглянемо два приклади:

```Swift
// Приклад 1
let date1 = Date() // Поточна дата
let date2 = Date(timeIntervalSinceNow: -86400) // Дата, представляюча понеділок цього тижня
print(date1 > date2) // Виводить "true"
```

```Swift
// Приклад 2
let dateFormatter = DateFormatter()
dateFormatter.dateFormat = "dd/MM/yyyy"
let date1 = dateFormatter.date(from: "01/01/2020") // 1 січня 2020 року
let date2 = dateFormatter.date(from: "31/12/2020") // 31 грудня 2020 року
print(date1! < date2!) // Виводить "true"
```

У першому прикладі ми використали конструктор `Date(timeIntervalSinceNow:)`, щоб створити дату, яка розташовується за 24 години від поточної. Далі ми порівняли її з поточною датою за допомогою оператора "більше" (`>`). У другому прикладі ми використовуємо `DateFormatter` для перетворення рядка у дату та порівняли дві дати за допомогою оператора "менше" (`<`).

## Огляд 

Існує декілька способів глибше дослідити порівняння дат у Swift. Наприклад, можна вивчити додаткові методи структури `Date`, такі як `timeIntervalSince(_: Date)`, який повертає час, що пройшов з вказаної дати до поточної. Також можна дізнатися більше про те, як працює `DateFormatter` та як налаштувати його для різних форматів дат.

## Дивіться також 

- [Документація Swift для структури `Date`](https://developer.apple.com/documentation/foundation/date)
- [Офіційний сайт мови Swift](https://swift.org/)
- [Спільнота Swift в Україні](https://www.facebook.com/swiftukraine/)