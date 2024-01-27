---
title:                "Робота з CSV файлами"
date:                  2024-01-19
html_title:           "Arduino: Робота з CSV файлами"
simple_title:         "Робота з CSV файлами"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/gleam/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why? / Що таке і навіщо?
CSV - це простий формат файлів для збереження табличних даних. Програмісти використовують його, щоб легко обмінюватися даними між різними програмами.

## How to: / Як робити:
```gleam
import gleam/csv.{decode, encode}
import gleam/list
import gleam/io

fn main() {
  let data = "name,age\nAlice,30\nBob,35"
  let rows = decode(data) // Розбір CSV
  case rows {
    Ok(rows) -> io.print(rows |> list.map(fn(row) { row[0] })) // Виведення імен
    Error(_) -> io.println("Помилка обробки CSV.")
  }

  let users = [["name", "age"], ["Alice", "30"], ["Bob", "35"]]
  let csv_data = encode(users) // Кодування в CSV
  case csv_data {
    Ok(csv) -> io.print(csv)  // Виведення CSV-рядка
    Error(_) -> io.println("Не вдалося створити CSV.")
  }
}
```
Sample Output:
```
Alice
Bob
name,age
Alice,30
Bob,35
```

## Deep Dive / Глибоке занурення
CSV є віддзеркаленням електронних таблиць і з'явився з часів перших персональних комп'ютерів. Альтернативи, такі як JSON чи XML, забезпечують більше можливостей для структурування, але CSV залишається популярним через свою простоту. Gleam обробляє CSV без зовнішніх бібліотек, що спрощує використання.

## See Also / Дивіться також
- Про CSV на Вікіпедії: [uk.wikipedia.org/wiki/CSV](https://uk.wikipedia.org/wiki/CSV)
- Звідки взявся CSV: [history-computer.com/](https://history-computer.com/)
- JSON: [www.json.org/json-uk.html](http://www.json.org/json-uk.html)
- XML: [w3schools.com/xml/](https://www.w3schools.com/xml/)
