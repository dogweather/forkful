---
title:                "Swift: Видалення символів, що відповідають взору."
programming_language: "Swift"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/swift/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Чому

Часто, коли ми працюємо зі строками в Swift, нам потрібно виконати певні маніпуляції зі знаками. Іноді це означає видаляти певний символ або групу символів зі строки, щоб отримати бажаний результат. У цьому пості ми розглянемо, як видаляти символи, які відповідають певному шаблону, використовуючи Swift.

## Як

Існує декілька способів видаляти символи з строки, але в даному випадку ми будемо використовувати метод `replacingOccurrences(of:with:)`. Цей метод дозволяє замінювати певний шаблон у строці іншими символами або навіть пустою строкою.

```Swift 
let string = "Привіт, світ!"
let newString = string.replacingOccurrences(of: "!", with: "")
// newString тепер має значення "Привіт, світ"
```

Для більш складних випадків можна використовувати регулярні вирази, щоб видаляти символи з конкретними властивостями. Наприклад, якщо нам потрібно видалити всі головні літери зі строки, ми можемо використати наступний код:

```Swift 
let string = "Hello, world!"
let newString = string.replacingOccurrences(of: "[A-Z]", with: "", options: .regularExpression)
// newString тепер має значення ", !"
```

## Deep Dive

Метод `replacingOccurrences(of:with:)` є дуже потужним і може використовуватися для видалення різних символів, будь яких шаблонів, а також для заміни символів на інші. Він підтримує використання регулярних виразів, що дозволяє здійснювати більш глибокі та складні маніпуляції зі строками.

## Дивись також

- [Офіційна документація Apple про метод `replacingOccurrences(of:with:)`](https://developer.apple.com/documentation/foundation/nsstring/1417518-replacingoccurrences)
- [Підручник з регулярних виразів для Swift](https://www.raywenderlich.com/915-regular-expressions-in-swift-tutorial-getting-started)