---
title:                "Swift: Робота з csv"
simple_title:         "Робота з csv"
programming_language: "Swift"
category:             "Swift"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/swift/working-with-csv.md"
---

{{< edit_this_page >}}

## Чому

Програмування мовою Swift є дуже корисним навичком для будь-якої людини, яка працює з даними. Робота з CSV - це одна з важливих задач у сфері обробки даних. Наприклад, ви можете використовувати CSV, щоб експортувати дані зі своєї програми в електронну таблицю, таку як Excel або Google Sheets.

## Як

```Swift
import Foundation

// Create CSV string with headers
var csvString = "Name, Age, Occupation\n"

// Add data to CSV string
csvString += "Anna, 28, Lawyer\n"
csvString += "Petro, 35, Engineer\n"
csvString += "Oksana, 42, Teacher\n"

// Write CSV string to a file
let fileURL = URL(fileURLWithPath: "users.csv")
do {
    try csvString.write(to: fileURL, atomically: true, encoding: .utf8)
    print("CSV successfully saved!")
} catch {
    print(error.localizedDescription)
}

// Read data from CSV file
do {
    let csvData = try String(contentsOf: fileURL, encoding: .utf8)
    print(csvData)
} catch {
    print(error.localizedDescription)
}
```

В результаті ви отримаєте файл "users.csv" з таким вмістом:

```
Name, Age, Occupation
Anna, 28, Lawyer
Petro, 35, Engineer
Oksana, 42, Teacher
```

## Глибинний занурення

CSV - це текстовий файл, що містить табличні дані, розділені комами або іншими роздільниками. Робота з CSV вимагає деякої обробки даних, наприклад, розбиття рядків на колонки або обробки числових значень. Також важливо враховувати можливі помилки, які можуть виникнути при роботі зі збереженими даними.

## Дивись також

- [Using the CSV Swift Package](https://www.swiftbysundell.com/articles/the-power-of-the-swift-package-manager/)
- [Working with CSV files using Swift](https://medium.com/developerinsider/working-with-csv-files-in-swift-6c62779931cc)
- [CSVParser library for Swift](https://github.com/yaslab/CSV.swift)