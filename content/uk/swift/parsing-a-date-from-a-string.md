---
title:                "Аналіз дати з рядка"
html_title:           "C++: Аналіз дати з рядка"
simple_title:         "Аналіз дати з рядка"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/swift/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Що та навіщо?

Парсинг дати з рядка це процес конвертації текстового представлення дати в спеціальний об'єкт дати. Це корисно тоді, коли працюємо з датами у форматах, котрі не відразу можна використати в програмі, наприклад, при зчитуванні дати з текстового файлу або від користувача.

## Як це робиться

Приклади коду та результати їх виконання в Swift:

```Swift
import Foundation

let dateFormatter = DateFormatter()
dateFormatter.dateFormat = "dd-MM-yyyy"
let dateFromString = dateFormatter.date(from: "15-03-2021")

print("Дата в об'єкт: \(String(describing: dateFromString))")
```

У консолі ви побачите щось на зразок: 

```Swift
Дата в об'єкт: Optional(2021-03-14 22:00:00 +0000)
```

## Більше деталей

1. Історія: Перші способи парсингу дати були простіші, але мали обмеження у форматах. До приходу Swift ця задача покладалась на `NSDateFormatter` в Objective-C.

2. Альтернативи: Є інші бібліотеки, такі як `DateTools` або `SwiftDate`, які надають розширені можливості для роботи з датами.

3. Деталі реалізації: При парсингу дати важливо врахувати часовий пояс та локаль, бо це впливає на точне представлення дати. 

## Дивіться також

1. [Робота з Датами та Часом в Swift](https://www.hackingwithswift.com/articles/141/how-to-work-with-dates-and-times-in-swift)
   
2. [Парсинг дати на StackOverflow](https://stackoverflow.com/questions/35700281/date-format-in-swift)

3. [Докладно про DateFormatter](https://developer.apple.com/documentation/foundation/dateformatter)