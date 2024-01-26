---
title:                "Робота з CSV файлами"
html_title:           "Arduino: Робота з CSV файлами"
simple_title:         "Робота з CSV файлами"
programming_language: "Python"
category:             "Python"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/python/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why? (Що та Чому?)
Робота з CSV полягає у читанні, записі та обробці даних у форматі Comma-Separated Values. Програмісти використовують CSV через простоту і сумісність з багатьма програмами.

## How to: (Як робити:)
```Python
import csv

# Читання CSV файлу
with open('sample.csv', mode='r', encoding='utf-8') as file:
    reader = csv.reader(file)
    for row in reader:
        print(row)

# Запис у CSV файл
with open('output.csv', mode='w', encoding='utf-8', newline='') as file:
    writer = csv.writer(file)
    writer.writerow(['name', 'age'])
    writer.writerow(['Oleg', 30])
    writer.writerow(['Kateryna', 25])

# Приклад виводу після читання
# ['name', 'age']
# ['Oleg', '30']
# ['Kateryna', '25']
```

## Deep Dive (Поглиблений аналіз):
CSV - це старий, але добре втілений формат. Він існує десятиліттями і став стандартним способом обміну даними між системами. Існують альтернативи, як JSON або XML, але CSV виграє простотою для людського сприйняття. При роботі з CSV важливо враховувати деталі реалізації, такі як кодування файлу та обробку спеціальних символів.

## See Also (Дивіться також):
- Офіційна [документація модуля csv](https://docs.python.org/3/library/csv.html)
- Уроки для більш поглибленого вивчення [Pandas](https://pandas.pydata.org/) для обробки даних у форматі CSV.
- [RFC 4180](https://tools.ietf.org/html/rfc4180), стандарт, який описує формат CSV.
