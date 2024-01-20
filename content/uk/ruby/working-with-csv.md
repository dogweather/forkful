---
title:                "Робота з CSV файлами"
html_title:           "Arduino: Робота з CSV файлами"
simple_title:         "Робота з CSV файлами"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/ruby/working-with-csv.md"
---

{{< edit_this_page >}}

## Що й Навіщо?
CSV (Comma-Separated Values) - це формат зберігання даних, в якому кожен рядок файлу - це один запис, розділений комами. Програмісти використовують CSV через його простоту і зручність для обміну та маніпуляції великими об'ємами табличних даних.

## Як це робити:
```Ruby
require 'csv'

# Запис у CSV файл
CSV.open("example.csv", "wb") do |csv|
  csv << ["Ім'я", "Прізвище", "Email"]
  csv << ["Василь", "Пупкін", "vasyl@example.com"]
  csv << ["Оксана", "Вишивана", "oksana@example.ua"]
end

# Читання з CSV файлу
CSV.foreach("example.csv", headers: true) do |row|
  puts "#{row['Ім'я']} - #{row['Email']}"
end
```
Результат в консолі:
```
Василь - vasyl@example.com
Оксана - oksana@example.ua
```

## Поглиблено:
CSV був розроблений у 1970-х для простого обміну даними між різними програмами. Альтернативою CSV може бути JSON, XML, або бази даних, але CSV залишається популярним через свою універсальність і легкість інтеграції. У Ruby обробка CSV реалізована через вбудовану бібліотеку 'csv', яка дозволяє легко читати і писати CSV файли без необхідності встановлення додаткових гемів.

## Більше Інформації:
- Рубі Документація CSV: https://ruby-doc.org/stdlib-3.1.0/libdoc/csv/rdoc/CSV.html
- Туторіал з роботою CSV у Ruby: https://www.kaggle.com/general/19254
- RFC 4180, офіційний стандарт CSV: https://tools.ietf.org/html/rfc4180