---
title:                "Робота з CSV"
html_title:           "Python: Робота з CSV"
simple_title:         "Робота з CSV"
programming_language: "Python"
category:             "Python"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/python/working-with-csv.md"
---

{{< edit_this_page >}}

## Чому

З доступністю та простотою обробки даних, CSV є одним з найпопулярніших форматів для зберігання таблиць та інших структурованих даних. Робота з CSV дозволяє легко читати та записувати дані з простими та зрозумілими інструкціями, що робить його важливим для багатьох програмістів.

## Як це зробити

```Python
# Імпортуємо модуль CSV
import csv

# Відкриваємо файл для читання та створюємо об'єкт для читання
with open('file.csv', 'r') as csv_file:
    csv_reader = csv.reader(csv_file)

    # Читаємо дані рядок за рядком та виводимо їх
    for row in csv_reader:
        print(row)
```

```Python
# Імпортуємо модуль CSV
import csv

# Відкриваємо файл для запису та створюємо об'єкт для запису
with open('file.csv', 'w') as csv_file:
    csv_writer = csv.writer(csv_file)

    # Записуємо дані у файл рядок за рядком
    csv_writer.writerow(['Назва', 'Ціна', 'Кількість'])
    csv_writer.writerow(['Яблука', '20 грн', '10'])
    csv_writer.writerow(['Банани', '30 грн', '5'])
    csv_writer.writerow(['Апельсини', '25 грн', '8'])
```

## Глибинний аналіз

CSV файл - це простий текстовий файл, в якому дані поділяються комами або іншими роздільниками. Цей формат не передбачає вбудованої підтримки для типів даних, тому всі дані є рядками. Більш глибокий аналіз CSV дозволяє працювати з даними більш ефективно, наприклад, використовуючи модуль Pandas для створення DataFrame з CSV даних та виконання різних операцій над ними.

## Дивись також

- [Документація Python для модуля CSV](https://docs.python.org/3/library/csv.html)
- [Основи роботи з Pandas](https://realpython.com/pandas-python-explore-dataset/)
- [Приклади використання CSV формату](https://data36.com/data-wrangling-tutorial-python-pandas/)