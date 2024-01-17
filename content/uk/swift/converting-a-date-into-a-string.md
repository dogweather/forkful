---
title:                "Перетворення дати у рядок"
html_title:           "Swift: Перетворення дати у рядок"
simple_title:         "Перетворення дати у рядок"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/swift/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Що і чому?

Перетворення дати у рядок є важливим етапом у програмуванні. Це дає змогу змінювати формат дати та використовувати її у різноманітних форматах, наприклад, для виведення її на екран користувачу. Програмісти використовують цю функцію, щоб полегшити роботу з датами та збільшити флексибільність своїх програм.

## Як це зробити:

Програмування цієї функції може бути досить простим. Ось приклад коду на Swift, що перетворює дату у рядок з форматом "день-місяць-рік":

```Swift
let date = Date()
let dateFormatter = DateFormatter()
dateFormatter.dateFormat = "dd-MM-yyyy"
let dateString = dateFormatter.string(from: date)
print(dateString)
```

Результат виводу: "08-10-2021"

## Глибока зануреність:

Перетворення дати у рядок – це процес, який триває не один рік. Його історія сягає далеко назад, до початку комп'ютерної ери. У світі програмування існує безліч альтернатив методу перетворення дати, але перетворення у рядок – це найпростіший та найпоширеніший з них. Цей процес вимагає використання різних форматів та дат, швидкість та ефективність його роботи залежать від правильного вибору цих параметрів.

## Дивіться також:

Щоб дізнатись більше про перетворення дат у рядок на Swift, пропонуємо переглянути такі ресурси:

- Документація Apple: https://developer.apple.com/documentation/foundation/dateformatter
- Стаття на блозі Hacking with Swift: https://www.hackingwithswift.com/example-code/system/how-to-convert-a-date-to-a-string
- Відеоуроки на Youtube каналі Code with Chris: https://www.youtube.com/watch?v=rZRlaFgQw8s