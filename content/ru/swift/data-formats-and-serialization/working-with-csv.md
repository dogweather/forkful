---
aliases:
- /ru/swift/working-with-csv/
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:04:22.511397-07:00
description: "CSV (\u0437\u043D\u0430\u0447\u0435\u043D\u0438\u044F, \u0440\u0430\u0437\
  \u0434\u0435\u043B\u0435\u043D\u043D\u044B\u0435 \u0437\u0430\u043F\u044F\u0442\u044B\
  \u043C\u0438) \u2014 \u044D\u0442\u043E \u043F\u0440\u043E\u0441\u0442\u044B\u0435\
  \ \u0442\u0435\u043A\u0441\u0442\u043E\u0432\u044B\u0435 \u0444\u0430\u0439\u043B\
  \u044B \u0434\u043B\u044F \u0445\u0440\u0430\u043D\u0435\u043D\u0438\u044F \u0442\
  \u0430\u0431\u043B\u0438\u0447\u043D\u044B\u0445 \u0434\u0430\u043D\u043D\u044B\u0445\
  . \u041F\u0440\u043E\u0433\u0440\u0430\u043C\u043C\u0438\u0441\u0442\u044B \u0438\
  \u0441\u043F\u043E\u043B\u044C\u0437\u0443\u044E\u0442 \u0438\u0445, \u043F\u043E\
  \u0442\u043E\u043C\u0443 \u0447\u0442\u043E \u043E\u043D\u0438 \u043B\u0435\u0433\
  \u043A\u043E\u2026"
lastmod: 2024-02-18 23:08:57.435891
model: gpt-4-0125-preview
summary: "CSV (\u0437\u043D\u0430\u0447\u0435\u043D\u0438\u044F, \u0440\u0430\u0437\
  \u0434\u0435\u043B\u0435\u043D\u043D\u044B\u0435 \u0437\u0430\u043F\u044F\u0442\u044B\
  \u043C\u0438) \u2014 \u044D\u0442\u043E \u043F\u0440\u043E\u0441\u0442\u044B\u0435\
  \ \u0442\u0435\u043A\u0441\u0442\u043E\u0432\u044B\u0435 \u0444\u0430\u0439\u043B\
  \u044B \u0434\u043B\u044F \u0445\u0440\u0430\u043D\u0435\u043D\u0438\u044F \u0442\
  \u0430\u0431\u043B\u0438\u0447\u043D\u044B\u0445 \u0434\u0430\u043D\u043D\u044B\u0445\
  . \u041F\u0440\u043E\u0433\u0440\u0430\u043C\u043C\u0438\u0441\u0442\u044B \u0438\
  \u0441\u043F\u043E\u043B\u044C\u0437\u0443\u044E\u0442 \u0438\u0445, \u043F\u043E\
  \u0442\u043E\u043C\u0443 \u0447\u0442\u043E \u043E\u043D\u0438 \u043B\u0435\u0433\
  \u043A\u043E\u2026"
title: "\u0420\u0430\u0431\u043E\u0442\u0430 \u0441 CSV"
---

{{< edit_this_page >}}

## Что и Почему?

CSV (значения, разделенные запятыми) — это простые текстовые файлы для хранения табличных данных. Программисты используют их, потому что они легко читаются и записываются, а также широко поддерживаются на различных системах и языках.

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
