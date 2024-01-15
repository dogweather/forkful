---
title:                "Робота з csv."
html_title:           "Swift: Робота з csv."
simple_title:         "Робота з csv."
programming_language: "Swift"
category:             "Swift"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/swift/working-with-csv.md"
---

{{< edit_this_page >}}

## Чому

Робота з CSV форматом є необхідною для всіх, хто працює з данними, зокрема для аналітиків, програмістів, бізнесменів та інших професій. Цей формат дозволяє зберігати дані у вигляді таблиць, що робить їх легко зрозумілими та зручними для обробки.

## Як користуватися CSV у Swift

Для початку роботи з CSV у Swift, потрібно завантажити бібліотеку `SwiftCSV`, яка дозволяє зчитувати та записувати дані у форматі CSV. Далі необхідно імпортувати бібліотеку у свій проект та використовувати методи `read` та `write` для роботи зі зчитуванням та записом даних відповідно.

```Swift
import SwiftCSV

// Зчитуємо дані з CSV файлу
let csv = try! CSV(url: "example.csv")

// Доступ до конкретної клітинки
let cell = csv[1, 0]
print(cell)

// Доступ до рядка даних
let row = csv[1]
print(row)

// Записуємо дані у CSV
let data = [["Name", "Age"], ["John", "25"], ["Sara", "30"]]
let newCSV = try! CSV(data: data)
try! newCSV.write(to: "newFile.csv")
```

## Поглиблене вивчення

Робота з CSV форматом є корисним навичкою для всіх, хто працює з обробкою та аналізом даних. Для досягнення більшої ефективності у роботі з CSV, слід вивчити додаткові можливості бібліотеки `SwiftCSV`, такі як фільтрація даних, створення графіків та інші.

## Дивіться також

- [Презентація бібліотеки SwiftCSV](https://github.com/swiftcsv/SwiftCSV)
- [Підручник зі зчитування та запису даних у Swift](https://www.raywenderlich.com/4526-swift-csv-tutorial-for-reading-and-writing)
- [Стаття про роботу з CSV форматом у Swift](https://www.appcoda.com/csv-import/)
- [Реалізація роботи з CSV форматом у Swift](https://medium.com/@mahaboudroza/using-a-csv-file-with-swift-3-e9d9c6d00ef9)