---
title:                "Python: Робота з csv"
simple_title:         "Робота з csv"
programming_language: "Python"
category:             "Python"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/python/working-with-csv.md"
---

{{< edit_this_page >}}

## Чому
Робота з CSV (Comma-Separated Values) є необхідною для багатьох завдань в програмуванні. CSV є одним з найпоширеніших форматів для збереження й обміну даними. Найчастіше його використовують для імпорту й експорту баз даних, звітів та іншої інформації.

## Як
```Python
import csv

# Відкриття CSV файлу для читання
with open('data.csv', 'r') as file:
    reader = csv.reader(file)

    # Прочитати кожен рядок у файлі
    for row in reader:
        # Вивести значення з кожної колонки
        print(row[0], row[1], row[2])

# Запис у CSV файл
with open('data.csv', 'w') as file:
    writer = csv.writer(file)

    # Додати новий рядок зі значеннями
    writer.writerow(['John', 'Doe', '30'])
```

Приклад виводу:
```
John, Doe, 30
Jane, Smith, 25
```

## Глибокий Занурення
CSV є простим форматом, але має деякі нюанси, які варто знати. Наприклад, значення можуть містити коми, у цьому випадку вони повинні бути обгорнуті подвійними лапками. Також важливо дотримуватися стандарту кодування UTF-8 при роботі з CSV файлами, щоб уникнути проблем з кирилицею.

## Дивіться також
- [Документація Python з модулем CSV](https://docs.python.org/3/library/csv.html)
- [Стаття про роботу з CSV файлами в Python](https://realpython.com/python-csv/)