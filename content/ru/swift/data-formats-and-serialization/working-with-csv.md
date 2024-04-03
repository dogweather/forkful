---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:04:22.511397-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: \u0414\u0430\u0432\u0430\u0439\u0442\u0435 \u043F\u0440\u043E\u0447\
  \u0438\u0442\u0430\u0435\u043C \u0444\u0430\u0439\u043B CSV \u0438 \u0440\u0430\u0437\
  \u0431\u0435\u0440\u0435\u043C \u0435\u0433\u043E \u0441\u043E\u0434\u0435\u0440\
  \u0436\u0438\u043C\u043E\u0435 \u043D\u0430 Swift. \u0421\u043D\u0430\u0447\u0430\
  \u043B\u0430 \u043F\u0440\u0435\u0434\u043F\u043E\u043B\u043E\u0436\u0438\u043C\
  , \u0447\u0442\u043E \u0443 \u043D\u0430\u0441 \u0435\u0441\u0442\u044C \u0444\u0430\
  \u0439\u043B `data.csv` \u0441 \u0442\u0430\u043A\u0438\u043C \u0441\u043E\u0434\
  \u0435\u0440\u0436\u0430\u043D\u0438\u0435\u043C."
lastmod: '2024-03-13T22:44:45.723358-06:00'
model: gpt-4-0125-preview
summary: "\u0414\u0430\u0432\u0430\u0439\u0442\u0435 \u043F\u0440\u043E\u0447\u0438\
  \u0442\u0430\u0435\u043C \u0444\u0430\u0439\u043B CSV \u0438 \u0440\u0430\u0437\u0431\
  \u0435\u0440\u0435\u043C \u0435\u0433\u043E \u0441\u043E\u0434\u0435\u0440\u0436\
  \u0438\u043C\u043E\u0435 \u043D\u0430 Swift."
title: "\u0420\u0430\u0431\u043E\u0442\u0430 \u0441 CSV"
weight: 37
---

## Как это сделать:
Давайте прочитаем файл CSV и разберем его содержимое на Swift.

Сначала предположим, что у нас есть файл `data.csv` с таким содержанием:

```обычный текст
name,age,city
Alice,30,New York
Bob,25,Los Angeles
```

Вот базовый скрипт на Swift для чтения и разбора:

```swift
import Foundation

let csvContent = """
name,age,city
Alice,30,New York
Bob,25,Los Angeles
"""

var rows = csvContent.components(separatedBy: "\n")
let headers = rows.removeFirst().components(separatedBy: ",")

var data = [[String: String]]()

for row in rows {
    let columns = row.components(separatedBy: ",")
    var rowData = [String: String]()
    for (header, column) in zip(headers, columns) {
        rowData[header] = column
    }
    data.append(rowData)
}

print(data)
```

Пример вывода:

```обычный текст
[["name": "Alice", "age": "30", "city": "New York"], ["name": "Bob", "age": "25", "city": "Los Angeles"]]
```

## Подробнее
CSV существует с ранних дней компьютеров — используется для перемещения данных между программами, базами данных и системами. Существуют альтернативы, такие как JSON и XML, но CSV остается популярным из-за своей простоты. С точки зрения эффективности, методы `String` в Swift хорошо справляются с CSV для небольших наборов данных, но для обработки данных в больших масштабах может потребоваться специализированная библиотека, такая как SwiftCSV или CodableCSV, для повышения производительности и удобства.

## Смотрите также
- Документация Apple по манипуляциям со строками в Swift: [https://developer.apple.com/documentation/swift/string](https://developer.apple.com/documentation/swift/string)
- SwiftCSV, специализированная библиотека CSV для Swift: [https://github.com/swiftcsv/SwiftCSV](https://github.com/swiftcsv/SwiftCSV)
- CodableCSV, кодировщик/декодировщик CSV для Swift: [https://github.com/dehesa/CodableCSV](https://github.com/dehesa/CodableCSV)
