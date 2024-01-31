---
title:                "Робота з CSV файлами"
date:                  2024-01-19
html_title:           "Arduino: Робота з CSV файлами"
simple_title:         "Робота з CSV файлами"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/swift/working-with-csv.md"
---

{{< edit_this_page >}}

## Що це та навіщо?
Робота з CSV в Swift - це парсинг і запис кома-сепарейтед велью файлів. Дозволяє обмінюватися даними з таблицями й базами даних, бо формат універсальний і легко читається людьми.

## Як це зробити:
```Swift
import Foundation

// Читання CSV файлу
let csvString = """
name,age,city
Alice,30,New York
Bob,25,Los Angeles
Charlie,35,Chicago
"""

// Розбивка на рядки і колонки
var rows = csvString.components(separatedBy: .newlines)
rows.removeFirst() // Видалити заголовки

let columns = rows.map { $0.components(separatedBy: ",") }

// Працюємо з даними
for row in columns {
    if !row.isEmpty {
        print("Name: \(row[0]), Age: \(row[1]), City: \(row[2])")
    }
}
```

Вивід:
```
Name: Alice, Age: 30, City: New York
Name: Bob, Age: 25, City: Los Angeles
Name: Charlie, Age: 35, City: Chicago
```

## Глибоке занурення:
CSV (Comma-Separated Values) зародився у ранніх 1970-х, коли потрібен був простий формат обміну табличними даними. Його альтернативи зараз — JSON і XML, кожен із своїми перевагами. В CSV, немає типу даних, тільки рядки, тому під час роботи часто треба конвертувати типи вручну. Основна задача Swift коду, який працює з CSV, — правильно обробити краєві випадки, такі як коми і переноси рядків в значеннях.

## Дивіться також:
- [Swift CSV](https://github.com/swiftcsv/SwiftCSV), бібліотека для роботи з CSV в Swift.
- [RFC 4180](https://tools.ietf.org/html/rfc4180), офіційний стандарт CSV.
- [CodableCSV](https://github.com/dehesa/CodableCSV), ще одна бібліотека, яка надає кодувальні й декодувальні можливості для CSV.
