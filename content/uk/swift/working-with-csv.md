---
title:                "Робота з csv"
html_title:           "Swift: Робота з csv"
simple_title:         "Робота з csv"
programming_language: "Swift"
category:             "Swift"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/swift/working-with-csv.md"
---

{{< edit_this_page >}}

## Що & Чому?
Робота з CSV - це зчитування та запис даних у файл формату CSV у програмах. Це корисний інструмент для програмістів, який дозволяє легко і швидко обробляти та керувати великими обсягами даних.

## Як це зробити:
```Swift
// Читання даних з файлу CSV
let csvURL = Bundle.main.url(forResource: "file", withExtension: "csv")
if let csvString = try? String(contentsOf: csvURL!) {
    // розділення даних на рядки
    let csvData = csvString.components(separatedBy: "\n")
    for row in csvData {
        // розділення рядків на окремі поля
        let fields = row.components(separatedBy: ",")
        // обробка даних
        // ...
    }
}

// Запис даних до файлу CSV
let csvData = "field1,field2,field3\nvalue1,value2,value3"
// створення файлу
let fileURL = FileManager.default.urls(for: .documentDirectory, in: .userDomainMask).first?.appendingPathComponent("file.csv")
// запис даних у файл
try? csvData.write(to: fileURL!, atomically: true, encoding: .utf8) 
```

## Глибинний аналіз:
CSV був створений у 1972 році і був широко популярним у 1980-х роках для обміну даними між різними програмними продуктами. Хоча зараз існує багато альтернатив, CSV все ще залишається популярним форматом для зберігання та обробки табличних даних.

При роботі з CSV важливо враховувати, що дані можуть містити коми та інші спеціальні символи, тому їх потрібно правильно обробляти при читанні та записі.

## Дивись також:
- [Офіційна документація Swift для роботи з CSV](https://developer.apple.com/documentation/foundation/archives_and_serialization/reading_and_writing_csv_data)
- [Стаття про CSV на Вікіпедії](https://uk.wikipedia.org/wiki/CSV)
- [Бібліотека для легкого роботи з CSV у Swift](https://github.com/yaslab/CSV.swift)