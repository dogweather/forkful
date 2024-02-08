---
title:                "Робота з CSV"
aliases:
- uk/swift/working-with-csv.md
date:                  2024-02-03T19:21:59.625998-07:00
model:                 gpt-4-0125-preview
simple_title:         "Робота з CSV"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/swift/working-with-csv.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Що і чому?

Робота з файлами CSV (Comma-Separated Values, значення, розділені комами) передбачає аналіз та генерацію структурованих даних з текстових файлів, де кожен рядок представляє запис, а кожен запис складається з полів, розділених комами. Програмісти часто займаються цією діяльністю, щоб легко імпортувати, експортувати та маніпулювати табличними даними за допомогою формату, який широко підтримується на різних платформах і мовами програмування завдяки його простоті та формату, зрозумілому для людини.

## Як:

У Swift немає вбудованої підтримки для безпосереднього аналізу файлів CSV, але ви можете обробляти дані CSV, використовуючи методи `String` для розбиття вмісту, або використовуючи сторонні бібліотеки, такі як SwiftCSV, для більш зручного підходу. Ось обидва методи:

### Ручний аналіз без зовнішніх бібліотек
```swift
// Розглянемо простий рядок CSV
let csvString = """
name,age,city
John Doe,29,New York
Jane Smith,34,Los Angeles
"""

// Розділімо рядок CSV на рядки
let rows = csvString.components(separatedBy: "\n")

// Витягнемо ключі з першого рядка
let keys = rows.first?.components(separatedBy: ",")

// Проходимося по рядках, починаючи з другого
var result: [[String: String]] = []
for row in rows.dropFirst() {
    let values = row.components(separatedBy: ",")
    let dict = Dictionary(uniqueKeysWithValues: zip(keys!, values))
    result.append(dict)
}

// Приклад виводу
print(result)
// Виводить: [{"city": "New York", "age": "29", "name": "John Doe"}, {"city": "Los Angeles", "age": "34", "name": "Jane Smith"}]
```
Цей підхід простий, але не дуже надійний, особливо у випадках із спеціальними умовами, такими як коми у значеннях, розриви рядків у полях тощо.

### Використання бібліотеки SwiftCSV
Спочатку додайте SwiftCSV до вашого проєкту, включивши її в залежності вашого `Package.swift`:
```swift
.package(url: "https://github.com/swiftcsv/SwiftCSV.git", from: "0.5.6")
```
Потім імпортуйте та використовуйте її наступним чином:
```swift
import SwiftCSV

// Припустимо, `csvString` визначено як вище

// Створимо об'єкт CSV
if let csv = try? CSV(string: csvString) {
    // Доступ до рядків як до словників
    let rows = csv.namedRows
    
    // Приклад виводу
    print(rows)
    // Виводить: [{"city": "New York", "age": "29", "name": "John Doe"}, {"city": "Los Angeles", "age": "34", "name": "Jane Smith"}]
}
```
SwiftCSV спрощує аналіз, автоматично обробляючи нюанси, такі як коми всередині тексту, розриви рядків у полях та кодування символів. Однак, пам'ятайте про необхідність обробки можливих помилок у реальних додатках, особливо при роботі з зовнішніми джерелами даних.
